// -----------------------------------------------------------------------------
// 1st Round Pick NBA Draft Valuation Model: ex-Gaussian, power-law mean (CV comparison;
// production is 02_models/picks_Round1.stan, which adds pooled gaps)
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Player outcome
//        Each drafted player has one row. The outcome is xRAPM wins above
//        replacement (-2.0 baseline) over the four rookie-contract seasons after
//        the draft (war4). Seasons a player misses count as zero.
//
//        Outcomes are right-skewed: most picks land near replacement and a few
//        become stars. The ex-Gaussian (Normal + Exponential) captures that
//        skew while keeping the MEAN as a parameter:
//
//        war4[n] = Normal(mu[p] - tau_x[p], sigma_g[p]) + Exponential(mean tau_x[p])
//        E[war4 | pick p] = mu[p]
//
//      so mu[p] stays an unbiased expected pick value (the Student-t centered
//      on the typical outcome instead, and sat ~2 wins below slot means).
//
//   2. Pick value curve (strictly decreasing in every posterior draw)
//        mu[p] = alpha / p^beta + gamma,   alpha > 0, beta > 0
//
//      Pick 1 is always worth more than pick 2, and so on; the posterior
//      uncertainty is in HOW MUCH more.
//
//   3. Outcome spread and skew by pick
//        Each pick has a total outcome SD (sd_pick) and a skew share
//        r_skew = tau_x / sd_pick (0 = symmetric Gaussian, 1 = pure exponential
//        upside). Both evolve across picks through random walks, so each pick
//        borrows from its neighbors, nearer picks more strongly, and the data
//        set how much (tau_sd, tau_r):
//
//        log_sd[p]  = log_sd[p - 1]  + tau_sd * z_sd[p - 1]
//        logit_r[p] = logit_r[p - 1] + tau_r  * z_r[p - 1]
//
//        tau_x[p]   = sd_pick[p] * r_skew[p]
//        sigma_g[p] = sd_pick[p] * sqrt(1 - r_skew[p]^2)
//
//      Skewness = 2 * r_skew^3, so r_skew is capped at 0.99 (skewness <= 1.94)
//      to keep a Gaussian component. Late first-round picks press against a
//      0.95 cap, and 0.99 improved held-out tail calibration.
//
// Priors are on the xRAPM wins-above-replacement scale. Empirically (2015-2022
// drafts): picks 1-3 average ~12 wins with a spread ~13; picks 21-30 average
// ~2.8 with a spread ~6; skewness ~0.2 at the top and ~1.3 late in the round.
// -----------------------------------------------------------------------------


data {
  int<lower=1> N;                         // Number of players
  array[N] int<lower=1, upper=30> pick;   // Pick each player was drafted at (1-30)
  vector[N] war4;                         // Four-season xRAPM wins above replacement
}



parameters {
  // ---------------------------------------------------------------------------
  // Pick value curve: mu[p] = alpha / p^beta + gamma
  // ---------------------------------------------------------------------------
  real log_alpha; // Height of the first-round value curve (log scale)
  real log_beta;  // Decline rate from early to late first round (log scale)
  real gamma;     // Late-first-round baseline value the curve approaches


  // ---------------------------------------------------------------------------
  // Outcome spread and skew by pick
  // ---------------------------------------------------------------------------
  real log_sd_1;          // Outcome SD at pick 1 (log scale)
  vector[29] z_sd;        // Pick-by-pick adjustments to the SD curve
  real<lower=0> tau_sd;   // How much the SD curve can vary by pick

  real logit_r_1;         // Skew share at pick 1 (logit scale)
  vector[29] z_r;         // Pick-by-pick adjustments to the skew curve
  real<lower=0> tau_r;    // How much the skew curve can vary by pick
}



transformed parameters {
  real<lower=0> alpha = exp(log_alpha);
  real<lower=0> beta  = exp(log_beta);

  vector[30] mu;                   // Expected value by pick
  vector<lower=0>[30] sd_pick;     // Total outcome SD by pick
  vector<lower=0, upper=1>[30] r_skew;  // Skew share of the SD by pick
  vector<lower=0>[30] tau_x;       // Exponential (upside) mean by pick
  vector<lower=0>[30] sigma_g;     // Gaussian SD by pick

  for (p in 1:30) {
    mu[p] = alpha / pow(p, beta) + gamma;
  }

  sd_pick = exp(log_sd_1 + append_row(0, cumulative_sum(tau_sd * z_sd)));
  r_skew  = 0.99 * inv_logit(logit_r_1 + append_row(0, cumulative_sum(tau_r * z_r)));
  tau_x   = sd_pick .* r_skew;
  sigma_g = sd_pick .* sqrt(1 - square(r_skew));
}



model {
  // Priors
  log_alpha ~ normal(log(15), 0.60);    // Pick 1 sits ~15 wins above the late-first baseline
  log_beta  ~ normal(log(0.55), 0.50);  // Smooth decline in value from pick 1-30 (scale-free)
  gamma     ~ normal(1.5, 2);           // Late-first mean value modestly above replacement

  log_sd_1  ~ normal(log(13), 0.50);    // #1 pick outcome SD centered around 13 wins
  z_sd      ~ std_normal();
  tau_sd    ~ normal(0, 0.15);

  logit_r_1 ~ normal(0, 1);             // Skew share near 0.5 at the top (skewness ~0.25)
  z_r       ~ std_normal();
  tau_r     ~ normal(0, 0.20);


  // Likelihood
  war4 ~ exp_mod_normal(mu[pick] - tau_x[pick], sigma_g[pick], inv(tau_x[pick]));
}



generated quantities {
  vector[30] war4_pred;      // Expected four-season wins above replacement by pick
  vector[30] war4_pred_sd;   // Total outcome SD by pick
  vector[30] war4_outcome;   // One simulated player outcome per pick
  vector[N] log_lik;         // Per-player log likelihood for PSIS-LOO and validation

  war4_pred    = mu;
  war4_pred_sd = sd_pick;

  for (p in 1:30) {
    war4_outcome[p] = exp_mod_normal_rng(mu[p] - tau_x[p], sigma_g[p], inv(tau_x[p]));
  }

  for (n in 1:N) {
    int p = pick[n];
    log_lik[n] = exp_mod_normal_lpdf(war4[n] | mu[p] - tau_x[p], sigma_g[p], inv(tau_x[p]));
  }
}
