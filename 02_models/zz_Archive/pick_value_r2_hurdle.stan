// pick_value_r2_hurdle.stan
// Bayesian second-round draft pick value curve with a structural-zero hurdle.
//
// One row per drafted second-round player. Players who never log NBA minutes
// in their first four seasons remain in the data with played = 0 and ws4 = 0.
// Conditional on playing, first-four-year Win Shares follow a Student-t curve.
// The probability of playing and the residual scale are both smoothed across
// adjacent second-round slots.

data {
  int<lower=1> N;
  array[N] int<lower=31, upper=60> pick;
  array[N] int<lower=0, upper=1> played;
  vector[N] ws4;
}

parameters {
  // Conditional production curve, reusing the round-1 inverse-power form.
  real log_alpha;
  real log_beta;
  real gamma;

  // Conditional residual scale random walk, slots 31..60.
  real log_sigma_31;
  vector[29] z_sigma_step;
  real<lower=0> tau_log_sigma_rw;

  // Structural play-probability random walk, slots 31..60.
  real logit_play_31;
  vector[29] z_logit_play_step;
  real<lower=0> tau_logit_play_rw;

  // Student-t tail thickness for players who appear.
  real<lower=2> nu;
}

transformed parameters {
  real<lower=0> alpha = exp(log_alpha);
  real<lower=0> beta = exp(log_beta);

  vector[30] log_sigma_pick;
  vector<lower=0>[30] sigma_pick;
  vector[30] logit_play_pick;
  vector<lower=0, upper=1>[30] p_play_pick;

  log_sigma_pick[1] = log_sigma_31;
  logit_play_pick[1] = logit_play_31;

  for (p in 2:30) {
    log_sigma_pick[p] = log_sigma_pick[p - 1] +
                        tau_log_sigma_rw * z_sigma_step[p - 1];
    logit_play_pick[p] = logit_play_pick[p - 1] +
                         tau_logit_play_rw * z_logit_play_step[p - 1];
  }

  sigma_pick = exp(log_sigma_pick);
  p_play_pick = inv_logit(logit_play_pick);
}

model {
  // Conditional production priors. These are lower than the first-round priors
  // but still allow rare second-round stars through the Student-t tail.
  log_alpha ~ normal(log(6), 0.80);
  log_beta  ~ normal(log(0.75), 0.55);
  gamma     ~ normal(0, 2.0);

  // Adjacent-pick residual-scale smoothing.
  log_sigma_31 ~ normal(log(3), 0.75);
  z_sigma_step ~ std_normal();
  tau_log_sigma_rw ~ normal(0, 0.15);

  // Adjacent-pick play-probability smoothing.
  logit_play_31 ~ normal(logit(0.65), 0.90);
  z_logit_play_step ~ std_normal();
  tau_logit_play_rw ~ normal(0, 0.25);

  nu - 2 ~ exponential(0.20);

  for (n in 1:N) {
    int s = pick[n] - 30;
    real mu = alpha / pow(pick[n], beta) + gamma;

    played[n] ~ bernoulli_logit(logit_play_pick[s]);
    if (played[n] == 1) {
      ws4[n] ~ student_t(nu, mu, sigma_pick[s]);
    }
  }
}

generated quantities {
  vector[30] war_pred;              // unconditional E[WS4] = p_play * conditional mean
  vector[30] war_pred_conditional;  // conditional mean among players who appear
  vector[30] war_pred_sd;           // conditional residual scale
  vector[30] p_play;                // structural play probability
  vector[N] log_lik;
  vector[N] ws4_rep;
  array[N] int played_rep;

  for (p in 1:30) {
    int slot = p + 30;
    real mu = alpha / pow(slot, beta) + gamma;
    war_pred_conditional[p] = mu;
    war_pred[p] = p_play_pick[p] * mu;
    war_pred_sd[p] = sigma_pick[p];
    p_play[p] = p_play_pick[p];
  }

  for (n in 1:N) {
    int s = pick[n] - 30;
    real mu = alpha / pow(pick[n], beta) + gamma;

    log_lik[n] = bernoulli_logit_lpmf(played[n] | logit_play_pick[s]);
    if (played[n] == 1) {
      log_lik[n] += student_t_lpdf(ws4[n] | nu, mu, sigma_pick[s]);
    }

    played_rep[n] = bernoulli_rng(p_play_pick[s]);
    if (played_rep[n] == 1) {
      ws4_rep[n] = student_t_rng(nu, mu, sigma_pick[s]);
    } else {
      ws4_rep[n] = 0;
    }
  }
}
