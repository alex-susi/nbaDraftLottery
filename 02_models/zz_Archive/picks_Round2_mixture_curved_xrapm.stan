// -----------------------------------------------------------------------------
// 2nd Round Pick NBA Draft Valuation Model: quadratic (curved) mixture trends
// (CV comparison; worse held-out log density than per-pick random walks)
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
//      share, upside size and fringe spread trend log-linearly across the
//      round; pick-by-pick random walks on them were not supported by the data
//      (learned scales ~0.05-0.07) and caused funnel divergences.
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
  // Played-outcome mixture: curved (quadratic) trends across the round on a
  // centered pick scale x = (j - 15.5) / 14.5, so x = -1 at pick 31, 0 at
  // 45.5 and 1 at pick 60
  // ---------------------------------------------------------------------------
  real a_q;              // logit contributor share at mid-round
  real b_q;              // linear trend
  real c_q;              // curvature (negative = decline accelerates late)
  real a_T;              // log contributor upside mean at mid-round
  real b_T;
  real c_T;
  real a_s0;             // log fringe spread at mid-round
  real b_s0;
  real c_s0;
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

  for (j in 1:30) {
    real x = (j - 15.5) / 14.5;

    p_contrib[j]   = inv_logit(a_q + b_q * x + c_q * square(x));
    upside_mean[j] = exp(a_T + b_T * x + c_T * square(x));
    sd_fringe[j]   = exp(a_s0 + b_s0 * x + c_s0 * square(x));
  }
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
  // Same centers and slopes as the log-linear version (per-pick slopes x 14.5),
  // plus curvature terms centered on zero
  a_q  ~ normal(logit(0.30) - 0.29, 1); // ~25% of played mid-round players contribute
  b_q  ~ normal(-0.29, 0.45);           // Fewer contributors later in the round
  c_q  ~ normal(0, 0.50);
  a_T  ~ normal(log(5) - 0.29, 0.75);   // Contributors add ~4 wins on average mid-round
  b_T  ~ normal(-0.29, 0.45);
  c_T  ~ normal(0, 0.50);
  a_s0 ~ normal(0, 0.75);               // Fringe outcomes spread ~1 win
  b_s0 ~ normal(0, 0.45);
  c_s0 ~ normal(0, 0.50);


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
