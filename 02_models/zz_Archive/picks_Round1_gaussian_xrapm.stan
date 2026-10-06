// -----------------------------------------------------------------------------
// 1st Round Pick NBA Draft Valuation Model: Gaussian mean curve (production until
// 2026-10-04; kept as a CV / LOO benchmark)
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Player outcome
//        Each drafted player has one row. The outcome is xRAPM wins above
//        replacement (-2.0 baseline) over the four rookie-contract seasons after
//        the draft (war4). Seasons a player misses count as zero.
//
//        war4[n] ~ Normal(mu[pick[n]], sigma_pick[pick[n]])
//
//   2. Pick value curve
//        Expected value declines smoothly from the top of the draft to the end
//        of the first round.
//
//        mu[p] = alpha / p^beta + gamma
//
//      where:
//        alpha controls the overall height of the curve.
//        beta controls how quickly value falls from early to late first.
//        gamma is the lower baseline the curve approaches later in the round.
//
//   3. Pick-specific outcome spread
//        Each pick has its own outcome spread, but nearby picks are tied
//        together so the model does not overreact to one unusually good or bad
//        historical pick slot.
//
//        log_sigma_pick[1] = log_sigma_1
//        log_sigma_pick[p] = log_sigma_pick[p - 1]
//                            + tau_log_sigma_rw * z_sigma_step[p - 1]
//
//   4. Why a Gaussian likelihood
//        mu[p] is used downstream as expected pick value, so the curve must
//        track the MEAN outcome at each slot. Four-season xRAPM value is
//        right-skewed (most picks land near replacement, a few become stars).
//        A Student-t centers on the typical outcome and sat ~50% below slot
//        means; a shifted-lognormal mixture extrapolated its tail and
//        overstated top picks. A heteroskedastic Gaussian likelihood fits the
//        curve to slot means, and won a draft-class cross-validation on
//        held-out mean error.
//
//        The skewed player-outcome distribution is NOT taken from this
//        likelihood: 03_models.R draws outcomes as mu[p] plus empirical
//        residuals from nearby picks.
//
// Priors are on the xRAPM wins-above-replacement scale. Empirically (1996-2022
// drafts): pick 1 averages ~16.6 wins with a spread ~15; picks 21-30 average
// ~2.6 with a spread ~6.
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
  real gamma;     // Late-first-round baseline value the curve approaches.


  // ---------------------------------------------------------------------------
  // Player outcome spread by pick
  // ---------------------------------------------------------------------------
  real log_sigma_1;                 // Outcome spread at pick 1 (log scale)
  vector[29] z_sigma_step;          // Pick-by-pick adjustments to the spread curve
  real<lower=0> tau_log_sigma_rw;   // How much the spread curve can vary by pick
}



transformed parameters {
  real<lower=0> alpha = exp(log_alpha); // Height of value curve
  real<lower=0> beta  = exp(log_beta);  // Decline rate

  vector[30] log_sigma_pick;            // Outcome spread by pick, log scale
  vector<lower=0>[30] sigma_pick;       // Positive outcome spread by pick

  log_sigma_pick[1] = log_sigma_1;      // Start spread curve at pick 1


  // Build spread curve for picks 2-30, nearby picks similar
  for (p in 2:30) {
    log_sigma_pick[p] = log_sigma_pick[p - 1] +
                        tau_log_sigma_rw * z_sigma_step[p - 1];
  }

  sigma_pick = exp(log_sigma_pick);     // Outcome spread from log scale to positive scale
}



model {
  // Priors
  log_alpha ~ normal(log(15), 0.60);    // Pick 1 sits ~15 wins above the late-first baseline
  log_beta  ~ normal(log(0.55), 0.50);  // Smooth decline in value from pick 1-30 (scale-free)
  gamma     ~ normal(1.5, 2);           // Late-first mean value modestly above replacement

  log_sigma_1       ~ normal(log(13), 0.50); // #1 pick outcome SD centered around 13 wins
  z_sigma_step      ~ std_normal();          // Pick-by-pick outcome-spread adjustments
  tau_log_sigma_rw  ~ normal(0, 0.15);       // Pick-by-pick outcome-spread curve bend


  // Likelihood
  for (n in 1:N) {
    real mu = alpha / pow(pick[n], beta) + gamma;

    war4[n] ~ normal(mu, sigma_pick[pick[n]]);
  }
}



generated quantities {
  vector[30] war4_pred;     // Expected four-season wins above replacement by pick
  vector[30] war4_pred_sd;  // Outcome SD by pick
  vector[N] log_lik;        // Per-player log likelihood for PSIS-LOO and validation


  for (p in 1:30) {
    war4_pred[p]    = alpha / pow(p, beta) + gamma;
    war4_pred_sd[p] = sigma_pick[p];
  }


  for (n in 1:N) {
    real mu = alpha / pow(pick[n], beta) + gamma;

    log_lik[n] = normal_lpdf(war4[n] | mu, sigma_pick[pick[n]]);
  }
}
