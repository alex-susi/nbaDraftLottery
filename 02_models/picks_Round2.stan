// -----------------------------------------------------------------------------
// 2nd Round Pick NBA Draft Valuation Model
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
//   3. Played outcomes: fringe players and contributors
//        Played second-rounders bunch tightly just below replacement, and a few
//        become rotation players worth 10-25 wins. One exponential tail cannot
//        fit both the sharp peak and the long tail (an ex-Gaussian shrank the
//        tail and understated EV), so played outcomes are a two-group mixture
//        that shares the fringe level:
//
//        fringe       (prob 1 - p_contrib): Normal(fringe_mean, sd_fringe)
//        contributor  (prob p_contrib):     Normal(fringe_mean, sd_fringe)
//                                           + Exponential(mean upside_mean)
//
//        mean_play[p]   = ev[p] / pi_p[p]
//        fringe_mean[p] = mean_play[p] - p_contrib[p] * upside_mean[p]
//
//      so the played mean stays a parameter and EV stays exact. Contributor
//      share, upside size and fringe spread each follow an adjacent-pick
//      random walk with drift (logit / log scale), so neighboring picks share
//      information and the late-round collapse in contributors can emerge
//      from the data. In draft-class CV this beat log-linear trends
//      (zz_Archive/picks_Round2_mixture_linear_xrapm.stan) and quadratic
//      trends (zz_Archive/picks_Round2_mixture_curved_xrapm.stan) on held-out
//      log density and late-pick band calibration.
//
//   4. Play probability
//        eta[p] = logit(pi_p[p]) evolves by an adjacent-pick random walk with
//        drift. It is not forced to decrease; only expected value is.
//
//   Stan uses the local index j = pick - 30. Fit with adapt_delta = 0.999 (the
//   derived fringe level makes late-pick geometry sharp; 0.99 left a few
//   divergences) and starting points near the prior means (r2_mixture_init in
//   03_models.R): generic random inits can start with steep EV gaps that put
//   the fringe level far from every played outcome, stranding the chain.
//
// Priors are on the xRAPM wins-above-replacement scale. Empirically (2015-2022
// drafts): ~95% of picks 31-35 log minutes vs ~66% at 56-60; expected value
// falls from ~1.8 wins at picks 31-35 to ~0 at 56-60; played outcomes have a
// median of -0.2 wins and a mean of 1.0.
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
  // Played-outcome mixture: adjacent-pick random walks with drift
  // ---------------------------------------------------------------------------
  real logit_q_31;       // contributor share at pick 31 (logit scale)
  real d_q;              // average change in logit contributor share per pick
  vector[29] z_q;        // pick-by-pick departures
  real<lower=0> tau_q;   // how far adjacent picks can differ
  real log_T_31;         // contributor upside mean at pick 31 (log scale)
  real d_T;
  vector[29] z_T;
  real<lower=0> tau_T;
  real log_s0_31;        // fringe spread at pick 31 (log scale)
  real d_s0;
  vector[29] z_s0;
  real<lower=0> tau_s0;
}



transformed parameters {
  vector[30] eta;
  vector<lower=0, upper=1>[30] pi_p;       // Play probabilities
  vector[29] eps;                          // Log departure of each gap from the typical gap
  vector<lower=0>[29] gap;                 // ev[j] - ev[j + 1]
  vector[30] ev;                           // Expected value by pick
  vector[30] mean_play;                    // Mean four-season value if the player plays
  vector<lower=0, upper=1>[30] p_contrib;  // Share of played players who are contributors
  vector<lower=0>[30] upside_mean;         // Contributors' exponential upside mean
  vector<lower=0>[30] sd_fringe;           // Fringe (and shared Gaussian) spread
  vector[30] fringe_mean;                  // Fringe level

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

  p_contrib   = inv_logit(logit_q_31 + append_row(0, cumulative_sum(d_q + tau_q * z_q)));
  upside_mean = exp(log_T_31 + append_row(0, cumulative_sum(d_T + tau_T * z_T)));
  sd_fringe   = exp(log_s0_31 + append_row(0, cumulative_sum(d_s0 + tau_s0 * z_s0)));
  fringe_mean = mean_play - p_contrib .* upside_mean;
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

  // Played-outcome mixture
  logit_q_31 ~ normal(logit(0.30), 1); // ~30% of played pick-31 players contribute
  d_q        ~ normal(-0.02, 0.03);    // Fewer contributors later in the round
  log_T_31   ~ normal(log(5), 0.75);   // Contributors add ~5 wins on average
  d_T        ~ normal(-0.02, 0.03);
  log_s0_31  ~ normal(log(1), 0.75);   // Fringe outcomes spread ~1 win
  d_s0       ~ normal(0, 0.03);
  z_q        ~ std_normal();
  z_T        ~ std_normal();
  z_s0       ~ std_normal();
  tau_q      ~ normal(0, 0.10);
  tau_T      ~ normal(0, 0.10);
  tau_s0     ~ normal(0, 0.10);


  // Likelihood
  for (n in 1:N) {
    int j = pick[n] - 30;              // Convert NBA pick number 31-60 into index 1-30

    played[n] ~ bernoulli(pi_p[j]);

    if (played[n] == 1) {
      target += log_mix(p_contrib[j],
                        exp_mod_normal_lpdf(war4[n] | fringe_mean[j], sd_fringe[j],
                                            inv(upside_mean[j])),
                        normal_lpdf(war4[n] | fringe_mean[j], sd_fringe[j]));
    }
  }
}



generated quantities {
  vector[30] sd_play;     // SD of played outcomes
  vector[30] ev_sd;       // Total pick SD including zero outcomes and played variance
  array[N] int play_rep;  // Simulated played / did-not-play result for each historical row
  vector[N] log_lik;      // Per-player log likelihood for PSIS-LOO and validation


  for (j in 1:30) {
    // Var = sd_fringe^2 + Var(B * E), B ~ Bernoulli(q), E ~ Exponential(mean T)
    real q = p_contrib[j];
    real T = upside_mean[j];
    real m2;

    sd_play[j] = sqrt(square(sd_fringe[j]) + q * (2 - q) * square(T));
    m2 = pi_p[j] * (square(sd_play[j]) + square(mean_play[j]));
    ev_sd[j] = sqrt(fmax(0, m2 - square(ev[j])));
  }


  for (n in 1:N) {
    int j = pick[n] - 30;

    play_rep[n] = bernoulli_rng(pi_p[j]);

    log_lik[n] = bernoulli_lpmf(played[n] | pi_p[j]);
    if (played[n] == 1) {
      log_lik[n] += log_mix(p_contrib[j],
                            exp_mod_normal_lpdf(war4[n] | fringe_mean[j], sd_fringe[j],
                                                inv(upside_mean[j])),
                            normal_lpdf(war4[n] | fringe_mean[j], sd_fringe[j]));
    }
  }
}
