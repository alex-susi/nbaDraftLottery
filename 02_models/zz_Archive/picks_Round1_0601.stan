// pick_value.stan
// Bayesian draft pick value curve
//
// Models expected 4-year Win Shares (rookie-contract window) by draft slot.
// We use a log-decay-plus-floor parameterization that matches the empirical
// shape documented across the public literature (Pelton, McCartney, the
// Wharton "NBA Draft Curves" paper): a steep drop across picks 1-5, a long
// flattening tail, and a small positive floor for late-first / second round.
//
//   E[WS | pick p] = alpha * exp(-beta * (p - 1)) + gamma
//
// Heteroscedastic observation noise grows with pick number, reflecting the
// empirical fact that the *relative* spread of outcomes widens deeper in the
// draft (a late pick is mostly busts plus the occasional star):
//
//   sigma(p) = sigma_base + sigma_slope * (p - 1)
//
// Each observed data point is a per-slot MEAN of 4-year WS with a known
// standard error (war_se = sd / sqrt(n_obs)). The likelihood combines that
// sampling error with the model-level noise so the curve is not overconfident
// where slot samples are small.
//
// NOTE ON BAYES vs. BOOTSTRAP:
// The R pipeline can EITHER use this Stan fit OR a nonparametric bootstrap of
// per-slot 4-year WS to produce the pick-value posterior. Both yield credible
// /confidence intervals; the bootstrap makes fewer shape assumptions while
// this Stan model borrows strength across slots through the smooth curve and
// is better when some slots have few observations. The toggle lives in
// nba_lottery.R (USE_BAYESIAN_PICK_CURVE).

data {
  int<lower=1> N;                  // number of draft slots with data (<= 30)
  array[N] int<lower=1> pick;      // slot number (1..30)
  vector[N] war_obs;               // mean 4-year WS at that slot
  vector<lower=0>[N] war_se;       // standard error of that mean
}


parameters {
  real<lower=0> alpha;             // height above floor at pick 1
  real<lower=0.3> beta;            // decay rate across slots
  real<lower=0> gamma;             // asymptotic floor value
  real<lower=0> sigma_base;        // model noise at pick 1
  real<lower=0> sigma_slope;       // growth of model noise per slot
}


model {
  alpha       ~ normal(60, 20);   // pick-1 height ~ alpha + gamma; let it be large
  beta        ~ normal(0.8, 0.3); // ~0.7–0.9 reproduces published curves
  gamma       ~ normal(8, 4);     // late-1st-round floor
  sigma_base  ~ exponential(0.3);
  sigma_slope ~ exponential(5);

  for (i in 1:N) {
    real mu         = alpha / pow(pick[i], beta) + gamma;
    real sigma_pick = sigma_base + sigma_slope * (pick[i] - 1);
    real total_sd   = sqrt(square(war_se[i]) + square(sigma_pick));
    war_obs[i] ~ normal(mu, total_sd);
  }
}


generated quantities {
  vector[30] war_pred;
  vector[30] war_pred_sd;
  vector[N]  log_lik;
  for (p in 1:30) {
    war_pred[p]    = alpha / pow(p, beta) + gamma;
    war_pred_sd[p] = sigma_base + sigma_slope * (p - 1);
  }
  for (i in 1:N) {
    real mu         = alpha / pow(pick[i], beta) + gamma;
    real sigma_pick = sigma_base + sigma_slope * (pick[i] - 1);
    real total_sd   = sqrt(square(war_se[i]) + square(sigma_pick));
    log_lik[i] = normal_lpdf(war_obs[i] | mu, total_sd);
  }
}
