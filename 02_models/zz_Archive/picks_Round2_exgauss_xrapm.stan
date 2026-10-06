// -----------------------------------------------------------------------------
// 2nd Round Pick NBA Draft Valuation Model: ex-Gaussian played outcomes (CV comparison;
// its exponential tail is too light for played second-rounders and understates EV)
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Hurdle / structural-zero layer
//        Players drafted in the second round don't always log NBA minutes.
//        played[n] ~ Bernoulli(pi_p[p]); a player who never plays is exactly 0.
//
//   2. Expected pick value (strictly decreasing in every posterior draw)
//        ev[p] = pi_p[p] * mean_play[p] is the expected value of the pick. It is
//        built from positive gaps between adjacent picks, so pick 31 is always
//        worth more than pick 32, and so on; the posterior uncertainty is in
//        HOW MUCH more:
//
//        ev[31]  = ev_31
//        ev[p+1] = ev[p] - gap[p]
//        gap[p]  = exp(log_gap + eps[p])
//
//      eps is a centered random walk, so adjacent gaps share information
//      (nearer picks more strongly) and tau_eps, learned from the data, sets how
//      far individual gaps can depart from the typical gap.
//
//   3. Played outcomes
//        If the player plays, four-season xRAPM wins above replacement (war4)
//        are right-skewed: most played second-rounders sit near replacement, a
//        few become rotation players. They are ex-Gaussian (Normal +
//        Exponential) with the conditional MEAN as a parameter:
//
//        war4[n] | played = Normal(mean_play[p] - tau_x[p], sigma_g[p])
//                           + Exponential(mean tau_x[p])
//        mean_play[p] = ev[p] / pi_p[p]
//
//      Total played SD (sd_play) and skew share r_skew = tau_x / sd_play each
//      follow an adjacent-pick random walk (SD with a downward drift):
//
//        tau_x[p]   = sd_play[p] * r_skew[p]
//        sigma_g[p] = sd_play[p] * sqrt(1 - r_skew[p]^2)
//
//   4. Play probability
//        eta[p] = logit(pi_p[p]) evolves by an adjacent-pick random walk with
//        drift. It is not forced to decrease; only expected value is.
//
//   Stan uses the local index j = pick - 30. Fit with init = 0.5: wide random
//   inits can strand a chain with huge random-walk scales.
//
// Priors are on the xRAPM wins-above-replacement scale. Empirically (2015-2022
// drafts): ~95% of picks 31-35 log minutes vs ~66% at 56-60; expected value
// falls from ~1.8 wins at picks 31-35 to ~0 at 56-60; played SD falls from ~5.7
// to ~1.
// -----------------------------------------------------------------------------


data {
  int<lower=1> N;                        // Number of players
  array[N] int<lower=31, upper=60> pick; // Pick each player was drafted at (31-60)
  array[N] int<lower=0, upper=1> played; // Play indicator (0 = no NBA minutes in the window)
  vector[N] war4;                        // Four-season xRAPM wins above replacement (0 if never played)
}



parameters {
  // ---------------------------------------------------------------------------
  // Play probability curve
  // ---------------------------------------------------------------------------
  real eta_31;          // play probability for pick 31 (logit scale)
  real d_pi;            // average change in logit play probability per pick
  real<lower=0> tau_pi; // pick-to-pick curve flexibility
  vector[29] z_pi;      // pick-by-pick curve adjustments


  // ---------------------------------------------------------------------------
  // Expected value curve
  // ---------------------------------------------------------------------------
  real ev_31;            // expected value of pick 31
  real log_gap;          // typical gap between adjacent picks (log scale)
  vector[29] z_eps;      // pick-by-pick adjustments to the gaps
  real<lower=0> tau_eps; // how far gaps can depart from the typical gap


  // ---------------------------------------------------------------------------
  // Played-outcome spread and skew
  // ---------------------------------------------------------------------------
  real log_sd_31;        // played-outcome SD at pick 31 (log scale)
  real d_sd;             // average change in log SD per pick
  vector[29] z_sd;
  real<lower=0> tau_sd;

  real logit_r_31;       // skew share at pick 31 (logit scale)
  vector[29] z_r;
  real<lower=0> tau_r;
}



transformed parameters {
  vector[30] eta;
  vector<lower=0, upper=1>[30] pi_p;  // Play probabilities
  vector[29] eps;                     // Log departure of each gap from the typical gap
  vector<lower=0>[29] gap;            // ev[j] - ev[j + 1]
  vector[30] ev;                      // Expected value by pick
  vector[30] mean_play;               // Mean four-season value if the player plays
  vector<lower=0>[30] sd_play;        // Played-outcome SD
  vector<lower=0, upper=1>[30] r_skew;
  vector<lower=0>[30] tau_x;          // Exponential (upside) mean
  vector<lower=0>[30] sigma_g;        // Gaussian SD

  eta[1] = eta_31;
  for (j in 2:30) {
    eta[j] = eta[j - 1] + d_pi + tau_pi * z_pi[j - 1];
  }
  pi_p = inv_logit(eta);

  {
    vector[29] eps_raw = cumulative_sum(tau_eps * z_eps);
    eps = eps_raw - mean(eps_raw);
  }
  gap = exp(log_gap + eps);

  ev[1] = ev_31;
  for (j in 2:30) {
    ev[j] = ev[j - 1] - gap[j - 1];
  }
  mean_play = ev ./ pi_p;

  sd_play = exp(log_sd_31 + append_row(0, cumulative_sum(d_sd + tau_sd * z_sd)));
  r_skew  = 0.99 * inv_logit(logit_r_31 + append_row(0, cumulative_sum(tau_r * z_r)));
  tau_x   = sd_play .* r_skew;
  sigma_g = sd_play .* sqrt(1 - square(r_skew));
}



model {
  // Priors
  // Play probability
  eta_31 ~ normal(logit(0.88), 0.60);  // Pick 31 very likely to appear in the NBA
  d_pi   ~ normal(-0.05, 0.05);        // Declines gradually from pick 31-60
  tau_pi ~ normal(0, 0.10);
  z_pi   ~ std_normal();

  // Expected value
  ev_31   ~ normal(1.5, 1);            // Pick 31 worth ~1.5 wins
  log_gap ~ normal(log(0.06), 0.75);   // ~0.06 wins per pick (~1.8 wins over the round)
  tau_eps ~ normal(0, 0.30);
  z_eps   ~ std_normal();

  // Played-outcome spread and skew
  log_sd_31  ~ normal(log(5), 0.50);   // Played pick-31 outcomes spread ~5 wins
  d_sd       ~ normal(-0.04, 0.04);    // Spread narrows later in the round
  tau_sd     ~ normal(0, 0.10);
  z_sd       ~ std_normal();
  logit_r_31 ~ normal(1, 1);           // Strong right skew (most near zero, a few rotation players)
  tau_r      ~ normal(0, 0.20);
  z_r        ~ std_normal();


  // Likelihood
  for (n in 1:N) {
    int j = pick[n] - 30;              // Convert NBA pick number 31-60 into index 1-30

    played[n] ~ bernoulli(pi_p[j]);

    if (played[n] == 1) {
      war4[n] ~ exp_mod_normal(mean_play[j] - tau_x[j], sigma_g[j], inv(tau_x[j]));
    }
  }
}



generated quantities {
  vector[30] ev_sd;       // Total pick SD including zero outcomes and played variance
  array[N] int play_rep;  // Simulated played / did-not-play result for each historical row
  vector[N] log_lik;      // Per-player log likelihood for PSIS-LOO and validation


  for (j in 1:30) {
    real m2 = pi_p[j] * (square(sd_play[j]) + square(mean_play[j]));

    ev_sd[j] = sqrt(fmax(0, m2 - square(ev[j])));
  }


  for (n in 1:N) {
    int j = pick[n] - 30;

    play_rep[n] = bernoulli_rng(pi_p[j]);

    log_lik[n] = bernoulli_lpmf(played[n] | pi_p[j]);
    if (played[n] == 1) {
      log_lik[n] += exp_mod_normal_lpdf(war4[n] | mean_play[j] - tau_x[j], sigma_g[j], inv(tau_x[j]));
    }
  }
}
