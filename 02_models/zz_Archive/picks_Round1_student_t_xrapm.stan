// -----------------------------------------------------------------------------
// 1st Round Pick NBA Draft Valuation Model: Student-t comparison (xRAPM scale)
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Player outcome
//        Each drafted player has one row. The outcome is xRAPM wins above
//        replacement (-2.0 baseline) over the four rookie-contract seasons after
//        the draft (war4). Seasons a player misses count as zero.
//
//        war4[n] ~ Student_t(nu, mu[pick[n]], sigma_pick[pick[n]])
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
//        Each pick has its own player outcome spread, but nearby picks are tied
//        together so the model does not overreact to one unusually good or bad
//        historical pick slot.
//
//        log_sigma_pick[1] = log_sigma_1
//        log_sigma_pick[p] = log_sigma_pick[p - 1]
//                            + tau_log_sigma_rw * z_sigma_step[p - 1]
//
//   4. Heavy-tailed player outcomes
//        The Student-t likelihood allows for extreme draft outcomes:
//        stars, busts, and other unusual player paths.
//        nu controls how much tail risk the model allows.
//
// Priors are on the xRAPM wins-above-replacement scale. Empirically (1996-2022
// drafts): pick 1 median ~16 wins with a robust spread ~13; picks 21-30 have a
// median near 0 and a mean near 2.6.
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


  // ---------------------------------------------------------------------------
  // Star / bust tail risk
  // ---------------------------------------------------------------------------
  real<lower=2> nu; // Student-t DoF; lower values allow more extreme draft outcomes
}



transformed parameters {
  real<lower=0> alpha = exp(log_alpha); // Height of value curve
  real<lower=0> beta  = exp(log_beta);  // Decline rate

  vector[30] log_sigma_pick;            // Outcome spread by pick, log scale
  vector<lower=0>[30] sigma_pick;       // Positive outcome spread by pick

  log_sigma_pick[1] = log_sigma_1;      // Start spread curve at pick 1


  // Build spread curve for picks 2-30, nearby picks similar
  for (p in 2:30) {
    // Update outcome spread from previous pick.
    log_sigma_pick[p] = log_sigma_pick[p - 1] +
                        tau_log_sigma_rw * z_sigma_step[p - 1];
  }

  sigma_pick = exp(log_sigma_pick);     // Outcome spread from log scale to positive scale
}



model {
  // Priors
  log_alpha ~ normal(log(15), 0.60);    // Pick 1 sits ~15 wins above the late-first baseline
  log_beta  ~ normal(log(0.55), 0.50);  // Smooth decline in value from pick 1-30 (scale-free)
  gamma     ~ normal(1, 2);             // Late-first-round baseline near replacement

  log_sigma_1       ~ normal(log(9), 0.50); // #1 pick outcome spread centered around 9 wins
  z_sigma_step      ~ std_normal();         // Pick-by-pick outcome-spread adjustments
  tau_log_sigma_rw  ~ normal(0, 0.15);      // Pick-by-pick outcome-spread curve bend

  nu - 2 ~ exponential(0.20); // Finite variance while allowing stars and busts


  // Likelihood
  // Loop over historical first-round picks
  for (n in 1:N) {
    // Expected four-season wins above replacement for player's draft slot
    real mu = alpha / pow(pick[n], beta) + gamma;

    war4[n] ~ student_t(nu, mu, sigma_pick[pick[n]]);
  }
}



generated quantities {
  vector[30] war4_pred;        // Expected four-season wins above replacement by pick
  vector[30] war4_pred_scale;  // Student-t scale by pick (SD = scale * sqrt(nu / (nu - 2)))
  vector[N] log_lik;           // Per-player log likelihood for PSIS-LOO and validation
  vector[N] war4_rep;          // Simulated four-season value for each historical player row


  // Loop over first-round picks
  for (p in 1:30) {
    real mu = alpha / pow(p, beta) + gamma; // Expected value for this pick

    war4_pred[p]       = mu;             // Expected value for this pick
    war4_pred_scale[p] = sigma_pick[p];  // Player outcome scale for this pick
  }


  // Loop over historical first-round picks
  for (n in 1:N) {
    // Expected four-season value for player's draft slot
    real mu = alpha / pow(pick[n], beta) + gamma;

    log_lik[n] = student_t_lpdf(war4[n] | nu, mu, sigma_pick[pick[n]]);

    war4_rep[n] = student_t_rng(nu, mu, sigma_pick[pick[n]]);
  }
}
