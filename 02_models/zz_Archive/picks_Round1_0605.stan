// pick_value.stan
// Bayesian draft pick value curve — player-level version
//
// Models first-4-year Win Shares (rookie-contract window) by draft slot using
// one row per drafted player:
//
//   ws4[n] ~ Student_t(nu, mu[pick[n]], sigma[pick[n]])
//
// The mean curve keeps the inverse-power shape used downstream in the lottery
// simulator:
//
//   mu[p] = alpha / p^beta + gamma
//
// The model is reparameterized on unconstrained scales for the positive
// parameters. This usually gives NUTS an easier posterior geometry than direct
// lower-bounded declarations for alpha, beta, sigma_base, and sigma_slope.
//
// Heteroscedastic player-level outcome noise:
//
//   sigma[p] = sigma_base + sigma_slope * (p - 1)
//
// NOTE: This version intentionally does NOT add slot-specific random effects.
// Add those only after this simpler robust player-level model has passed basic
// geometry, PPC, LOO, and SBC checks.

data {
  int<lower=1> N;                         // number of drafted-player rows
  array[N] int<lower=1, upper=30> pick;   // draft slot, 1..30
  vector[N] ws4;                          // first-4-year Win Shares
}

parameters {
  real log_alpha;                         // unconstrained alpha parameter
  real log_beta;                          // unconstrained beta parameter
  real gamma_raw;                         // centered floor parameter
  real log_sigma_base;                    // unconstrained sigma_base parameter
  real log_sigma_slope;                   // unconstrained sigma_slope parameter
  real<lower=0> nu_minus_two;             // Student-t df = 2 + nu_minus_two
}

transformed parameters {
  real<lower=0> alpha = exp(log_alpha);
  real<lower=0> beta = exp(log_beta);
  real gamma = 2.0 + 3.0 * gamma_raw;
  real<lower=0> sigma_base = exp(log_sigma_base);
  real<lower=0> sigma_slope = exp(log_sigma_slope);
  real<lower=2> nu = 2.0 + nu_minus_two;
}

model {
  // Priors are centered on the scale implied by the previous fit diagnostics,
  // but deliberately leave room for the player-level likelihood to learn much
  // larger residual variation than the old slot-mean model.
  log_alpha       ~ normal(log(20), 0.60);
  log_beta        ~ normal(log(0.55), 0.50);
  gamma_raw       ~ normal(0, 1);
  log_sigma_base  ~ normal(log(8), 0.60);
  log_sigma_slope ~ normal(log(0.03), 1.00);
  nu_minus_two    ~ exponential(0.20);

  for (n in 1:N) {
    real mu = alpha / pow(pick[n], beta) + gamma;
    real sigma_pick = sigma_base + sigma_slope * (pick[n] - 1);
    ws4[n] ~ student_t(nu, mu, sigma_pick);
  }
}

generated quantities {
  vector[30] war_pred;        // posterior mean curve by pick
  vector[30] war_pred_sd;     // player-level residual scale by pick
  vector[N] log_lik;          // pointwise log-likelihood for player-level LOO
  vector[N] ws4_rep;          // replicated player-level outcomes for PPC

  for (p in 1:30) {
    real mu = alpha / pow(p, beta) + gamma;
    real sigma_pick = sigma_base + sigma_slope * (p - 1);
    war_pred[p] = mu;
    war_pred_sd[p] = sigma_pick;
  }

  for (n in 1:N) {
    real mu = alpha / pow(pick[n], beta) + gamma;
    real sigma_pick = sigma_base + sigma_slope * (pick[n] - 1);
    log_lik[n] = student_t_lpdf(ws4[n] | nu, mu, sigma_pick);
    ws4_rep[n] = student_t_rng(nu, mu, sigma_pick);
  }
}
