// pick_value.stan
// Bayesian draft pick value curve — player-level version with adjacent-pick
// smoothing for pick-specific residual variance
//
// Models first-4-year Win Shares (rookie-contract window) by draft slot using
// one row per drafted player:
//
//   ws4[n] ~ Student_t(nu, mu[pick[n]], sigma_pick[pick[n]])
//
// Mean curve:
//
//   mu[p] = alpha / p^beta + gamma
//
// Variance model:
//   Each draft slot gets its own player-level residual scale sigma_pick[p], but
//   log sigma values are smoothed across adjacent picks using a non-centered
//   first-order random-walk prior:
//
//   log_sigma_pick[1] = log_sigma_1
//   log_sigma_pick[p] = log_sigma_pick[p - 1] + tau_log_sigma_rw * z_sigma_step[p - 1]
//
// This is a structured hierarchical prior: the 30 pick-specific scales are
// partially pooled through the shared smoothing hyperparameter tau_log_sigma_rw,
// and nearby picks borrow information from one another. It is not exchangeable
// pooling, because pick order matters.
//
// NOTE: This version intentionally does NOT add slot-specific random effects in
// the mean. It only makes the residual variance pick-specific and smoothed.

data {
  int<lower=1> N;                         // number of drafted-player rows
  array[N] int<lower=1, upper=30> pick;   // draft slot, 1..30
  vector[N] ws4;                          // first-4-year Win Shares
}



parameters {
  // Mean-curve parameters, reparameterized on unconstrained scales.
  real log_alpha;
  real log_beta;
  real gamma_raw;

  // Adjacent-pick-smoothed residual scale parameters.
  real log_sigma_1;                       // log residual scale at pick 1
  vector[29] z_sigma_step;                // non-centered random-walk innovations
  real<lower=0> tau_log_sigma_rw;         // adjacent-pick smoothing scale

  // Student-t tail thickness.
  real<lower=0> nu_minus_two;             // Student-t df = 2 + nu_minus_two
}



transformed parameters {
  // Convert the unconstrained log-scale alpha parameter back to the positive
  // draft-value curve scale.
  //
  // alpha controls the overall height of the pick-value curve. Larger alpha
  // means the top of the draft is worth more relative to later picks.
  real<lower=0> alpha = exp(log_alpha);

  // Convert the unconstrained log-scale beta parameter back to a positive
  // curvature/decay parameter.
  //
  // beta controls how quickly expected value declines as pick number increases.
  // Larger beta = steeper drop-off from pick 1 to pick 30.
  real<lower=0> beta = exp(log_beta);
  
  
  // Transform a standardized raw parameter into the gamma scale used in the
  // mean curve.
  //
  // Since gamma_raw ~ normal(0, 1), this implies gamma is centered around 2
  // with prior scale about 3. In the mean curve, gamma acts like a baseline or
  // lower asymptote: the expected value that the curve approaches at later picks.
  real gamma = 2.0 + 3.0 * gamma_raw;

  // Convert the positive nu_minus_two parameter into the Student-t degrees of
  // freedom.
  //
  // nu controls tail thickness. Smaller nu means more tolerance for extreme
  // draft outcomes, like stars or busts. Adding 2 keeps the degrees of freedom
  // near or above the range where the Student-t variance is well behaved.
  real<lower=2> nu = 2.0 + nu_minus_two;

  // This vector stores the log residual standard deviation for each pick slot.
  //
  // Working on the log scale is useful because sigma must be positive, but
  // log_sigma_pick can move freely on the real line.
  vector[30] log_sigma_pick;

  // This is the actual positive residual standard deviation for each pick slot.
  //
  // sigma_pick[p] is the player-level volatility around the mean curve for
  // pick p. For example, pick 1 can have a larger residual scale than pick 20.
  vector<lower=0>[30] sigma_pick;

  // Anchor the random walk at pick 1.
  //
  // log_sigma_1 is the starting value for the log residual scale curve.
  // Every later pick's log sigma is built by stepping away from this value.
  log_sigma_pick[1] = log_sigma_1;

  // Build the adjacent-pick random walk for log residual volatility.
  //
  // For each pick p from 2 to 30:
  //   log_sigma_pick[p] =
  //     previous pick's log sigma
  //     + smoothing scale * standardized random innovation
  //
  // z_sigma_step[p - 1] is the random step from pick p - 1 to pick p.
  // tau_log_sigma_rw controls how large those steps are allowed to be.
  //
  // Small tau_log_sigma_rw:
  //   sigma changes slowly and smoothly across adjacent picks.
  //
  // Large tau_log_sigma_rw:
  //   neighboring picks can have more different residual volatility.
  for (p in 2:30) {
    log_sigma_pick[p] = log_sigma_pick[p - 1] +
                        tau_log_sigma_rw * z_sigma_step[p - 1];
  }

  // Convert the full log-sigma curve back to the positive sigma scale.
  //
  // After this line, sigma_pick[p] is the residual standard deviation used in:
  //
  //   ws4[n] ~ student_t(nu, mu[pick[n]], sigma_pick[pick[n]])
  //
  // So this is the final pick-specific player-outcome volatility curve.
  sigma_pick = exp(log_sigma_pick);
}



model {
  // Mean-curve priors. These are intentionally broad but centered near the
  // previous player-level fit.
  log_alpha ~ normal(log(20), 0.60);
  log_beta  ~ normal(log(0.55), 0.50);
  gamma_raw ~ normal(0, 1);

  // Variance-smoothing priors.
  // log_sigma_1 is centered slightly above the previous constant-scale estimate,
  // because diagnostics suggested undercoverage at the very top of the draft.
  // tau_log_sigma_rw controls how quickly residual volatility can change across
  // adjacent picks. Smaller values imply a smoother sigma curve.
  log_sigma_1       ~ normal(log(8), 0.50);
  z_sigma_step      ~ std_normal();
  tau_log_sigma_rw  ~ normal(0, 0.15);

  // Student-t degrees of freedom. Smaller values mean heavier tails; df > 2
  // keeps the variance finite.
  nu_minus_two ~ exponential(0.20);

  for (n in 1:N) {
    real mu = alpha / pow(pick[n], beta) + gamma;
    ws4[n] ~ student_t(nu, mu, sigma_pick[pick[n]]);
  }
}



generated quantities {
  vector[30] war_pred;        // posterior mean curve by pick
  vector[30] war_pred_sd;     // player-level residual scale by pick
  vector[N] log_lik;          // pointwise log-likelihood for player-level LOO
  vector[N] ws4_rep;          // replicated player-level outcomes for PPC

  for (p in 1:30) {
    real mu = alpha / pow(p, beta) + gamma;
    war_pred[p] = mu;
    war_pred_sd[p] = sigma_pick[p];
  }

  for (n in 1:N) {
    real mu = alpha / pow(pick[n], beta) + gamma;
    log_lik[n] = student_t_lpdf(ws4[n] | nu, mu, sigma_pick[pick[n]]);
    ws4_rep[n] = student_t_rng(nu, mu, sigma_pick[pick[n]]);
  }
}
