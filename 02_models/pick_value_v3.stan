// pick_value.stan
// Bayesian draft pick value curve — player-level version with adjacent-pick
// smoothing for pick-specific residual variance
//
// Models first-4-year Win Shares using one row per drafted player:
//
//   ws4[n] ~ Student_t(nu, mu[pick[n]], sigma_pick[pick[n]])
//
// Mean curve:
//
//   mu[p] = alpha / p^beta + gamma
//
// Variance model:
//
//   log_sigma_pick[1] = log_sigma_1
//   log_sigma_pick[p] = log_sigma_pick[p - 1]
//                       + tau_log_sigma_rw * z_sigma_step[p - 1]
//
// This version directly declares:
//
//   gamma ~ normal(2, 3)
//   nu - 2 ~ exponential(0.20)
//
// instead of using:
//
//   gamma_raw
//   nu_minus_two

data {
  int<lower=1> N;                         // number of drafted-player rows
  array[N] int<lower=1, upper=30> pick;   // draft slot, 1..30
  vector[N] ws4;                          // first-4-year Win Shares
}



parameters {
  // --------------------------------------------------------------------------
  // Mean-curve parameters
  // --------------------------------------------------------------------------

  // log_alpha is sampled on the unconstrained real line, then exponentiated
  // into alpha > 0 in transformed parameters.
  //
  // alpha controls the height of the pick-value curve.
  real log_alpha;

  // log_beta is sampled on the unconstrained real line, then exponentiated
  // into beta > 0 in transformed parameters.
  //
  // beta controls how quickly expected value declines by draft slot.
  real log_beta;

  // gamma is now sampled directly.
  //
  // In the mean curve:
  //
  //   mu[p] = alpha / p^beta + gamma
  //
  // gamma acts like the baseline / lower asymptote of the curve. As pick number
  // gets larger, alpha / p^beta shrinks and the curve approaches gamma.
  //
  // We give this a direct prior in the model block:
  //
  //   gamma ~ normal(2, 3)
  //
  // This is equivalent to the old setup:
  //
  //   gamma_raw ~ normal(0, 1)
  //   gamma = 2 + 3 * gamma_raw
  //
  // but it is easier to read.
  real gamma;



  // --------------------------------------------------------------------------
  // Adjacent-pick-smoothed residual scale parameters
  // --------------------------------------------------------------------------

  // log residual scale at pick 1.
  //
  // The random walk for log sigma starts here.
  real log_sigma_1;

  // Standardized random-walk innovations.
  //
  // z_sigma_step[1] is the step from pick 1 to pick 2,
  // z_sigma_step[2] is the step from pick 2 to pick 3,
  // ...
  // z_sigma_step[29] is the step from pick 29 to pick 30.
  vector[29] z_sigma_step;

  // Adjacent-pick smoothing scale.
  //
  // Smaller values force sigma_pick[p] to change smoothly across picks.
  // Larger values allow more local volatility jumps between neighboring picks.
  real<lower=0> tau_log_sigma_rw;



  // --------------------------------------------------------------------------
  // Student-t tail thickness
  // --------------------------------------------------------------------------

  // Student-t degrees of freedom.
  //
  // nu controls tail thickness:
  //
  //   lower nu  = heavier tails, more tolerance for extreme stars / busts
  //   higher nu = closer to normal residuals
  //
  // We constrain nu > 2 so the Student-t variance is finite. In the model block
  // we place the prior on the excess above 2:
  //
  //   nu - 2 ~ exponential(0.20)
  //
  // This is equivalent to the old setup:
  //
  //   nu_minus_two ~ exponential(0.20)
  //   nu = 2 + nu_minus_two
  //
  // but again it is easier to read.
  real<lower=2> nu;
}



transformed parameters {
  // Convert log-scale alpha back to the positive draft-value curve scale.
  real<lower=0> alpha = exp(log_alpha);

  // Convert log-scale beta back to the positive curve-decay scale.
  real<lower=0> beta = exp(log_beta);

  // Store the log residual standard deviation for each pick.
  //
  // We model sigma on the log scale because sigma must be positive, while
  // log_sigma_pick can move freely on the real line.
  vector[30] log_sigma_pick;

  // Positive player-level residual standard deviation for each pick.
  //
  // sigma_pick[p] is the outcome volatility around the mean curve for pick p.
  vector<lower=0>[30] sigma_pick;

  // Anchor the log-sigma random walk at pick 1.
  log_sigma_pick[1] = log_sigma_1;

  // Build the adjacent-pick random walk.
  //
  // Each pick's log residual scale equals the prior pick's log residual scale
  // plus a scaled standardized innovation.
  //
  // This gives nearby picks similar residual volatility unless the data strongly
  // support a local change.
  for (p in 2:30) {
    log_sigma_pick[p] = log_sigma_pick[p - 1] +
                        tau_log_sigma_rw * z_sigma_step[p - 1];
  }

  // Convert log residual scales back to positive residual scales.
  sigma_pick = exp(log_sigma_pick);
}



model {
  // --------------------------------------------------------------------------
  // Mean-curve priors
  // --------------------------------------------------------------------------

  // alpha is positive because alpha = exp(log_alpha).
  //
  // This prior centers alpha around 20 on the original scale, while allowing
  // substantial uncertainty.
  log_alpha ~ normal(log(20), 0.60);

  // beta is positive because beta = exp(log_beta).
  //
  // This prior centers beta around 0.55, which implies a smooth declining curve
  // from pick 1 to pick 30.
  log_beta ~ normal(log(0.55), 0.50);

  // Direct prior on gamma.
  //
  // This replaces:
  //
  //   gamma_raw ~ normal(0, 1)
  //   gamma = 2 + 3 * gamma_raw
  //
  // with the equivalent, more readable:
  //
  //   gamma ~ normal(2, 3)
  //
  // Interpretation: before seeing the data, the late-first-round baseline value
  // is centered near 2 first-four-year Win Shares, but the prior is broad.
  gamma ~ normal(2, 3);



  // --------------------------------------------------------------------------
  // Variance-smoothing priors
  // --------------------------------------------------------------------------

  // Starting residual scale at pick 1.
  //
  // Since this is on the log scale, log(8) means the prior center for
  // sigma_pick[1] is roughly 8 Win Shares.
  log_sigma_1 ~ normal(log(8), 0.50);

  // Standard normal innovations for the random-walk steps.
  //
  // These are non-centered innovations. The actual step size is:
  //
  //   tau_log_sigma_rw * z_sigma_step[p - 1]
  z_sigma_step ~ std_normal();

  // Smoothing prior.
  //
  // Since tau_log_sigma_rw has lower bound 0, this is a half-normal prior.
  // Most prior mass is near small values, which favors a smooth sigma curve.
  tau_log_sigma_rw ~ normal(0, 0.15);



  // --------------------------------------------------------------------------
  // Student-t tail prior
  // --------------------------------------------------------------------------

  // Direct prior on the excess degrees of freedom above 2.
  //
  // Because nu is constrained as real<lower=2>, nu - 2 is positive.
  //
  // exponential(0.20) has mean 5, so this prior roughly centers nu around:
  //
  //   2 + 5 = 7
  //
  // while still allowing very heavy tails if the data want nu close to 2, or
  // nearly normal residuals if the data push nu much higher.
  nu - 2 ~ exponential(0.20);



  // --------------------------------------------------------------------------
  // Likelihood
  // --------------------------------------------------------------------------

  for (n in 1:N) {
    // Expected first-four-year Win Shares for this player's draft slot.
    real mu = alpha / pow(pick[n], beta) + gamma;

    // Player-level outcome model.
    //
    // The Student-t likelihood is robust to extreme draft outcomes.
    // sigma_pick[pick[n]] lets each draft slot have its own residual volatility.
    ws4[n] ~ student_t(nu, mu, sigma_pick[pick[n]]);
  }
}



generated quantities {
  vector[30] war_pred;        // posterior mean curve by pick
  vector[30] war_pred_sd;     // player-level residual scale by pick
  vector[N] log_lik;          // pointwise log-likelihood for player-level LOO
  vector[N] ws4_rep;          // replicated player-level outcomes for PPC

  // Store the posterior mean curve and residual scale curve by pick.
  for (p in 1:30) {
    real mu = alpha / pow(p, beta) + gamma;

    war_pred[p] = mu;
    war_pred_sd[p] = sigma_pick[p];
  }

  // Store pointwise log likelihoods and posterior predictive replicated
  // outcomes for validation.
  for (n in 1:N) {
    real mu = alpha / pow(pick[n], beta) + gamma;

    log_lik[n] = student_t_lpdf(ws4[n] | nu, mu, sigma_pick[pick[n]]);
    ws4_rep[n] = student_t_rng(nu, mu, sigma_pick[pick[n]]);
  }
}
