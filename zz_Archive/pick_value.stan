// pick_value_model.stan
// Bayesian nonlinear fit: WAR = alpha * exp(-beta * (pick - 1)) + gamma
// Heteroscedastic noise: sigma_pick = sigma_base + sigma_slope * (pick - 1)
// Fit to historical draft pick production data (rookie-contract WAR by pick)

data {
  int<lower=1> N;
  array[N] int<lower=1> pick;
  vector[N] war_obs;
  vector<lower=0>[N] war_se;
}

parameters {
  real<lower=0> alpha;
  real<lower=0.01> beta;
  real<lower=0> gamma;
  real<lower=0> sigma_base;
  real<lower=0> sigma_slope;
}

model {
  // Priors — informed by prior draft analyses but weakly regularizing
  alpha      ~ normal(22, 5);
  beta       ~ normal(0.12, 0.05);
  gamma      ~ normal(1.5, 1.0);
  sigma_base ~ exponential(0.5);
  sigma_slope ~ exponential(5);

  // Likelihood — measurement error + model error
  for (i in 1:N) {
    real mu       = alpha * exp(-beta * (pick[i] - 1)) + gamma;
    real sigma_pick = sigma_base + sigma_slope * (pick[i] - 1);
    real total_sd = sqrt(square(war_se[i]) + square(sigma_pick));
    war_obs[i] ~ normal(mu, total_sd);
  }
}

generated quantities {
  vector[30] war_pred;
  vector[30] war_pred_sd;
  for (p in 1:30) {
    war_pred[p]    = alpha * exp(-beta * (p - 1)) + gamma;
    war_pred_sd[p] = sigma_base + sigma_slope * (p - 1);
  }
}
