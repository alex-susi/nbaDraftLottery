// Round 2 smoothed hurdle model for NBA draft pick valuation
// ---------------------------------------------------------------
// This model replaces the old broad-band empirical WS layer with a fully
// smoothed second-round hurdle model:
//
//   played[n] ~ Bernoulli(P(play | pick))
//   ws4[n] | played[n] = 1 ~ Student_t(nu_ws,
//                                      E[WS4 | play, pick],
//                                      sigma_ws[pick])
//
// The three pick-specific curves borrow strength from nearby second-round slots
// through adjacent-pick random walks, matching the smoothing idea used in
// pick_value_v3.stan:
//
//   logit P(play)[p]      = logit P(play)[p-1]      + drift + tau * z[p-1]
//   log E[WS4 | play][p] = log E[WS4 | play][p-1] + drift + tau * z[p-1]
//   log sigma_ws[p]      = log sigma_ws[p-1]       + tau * z[p-1]
//
// Expected asset value for picks 31-60 is generated directly as:
//
//   EV[p] = P(play)[p] * E[WS4 | play, p]
//
// This removes the visible 5-pick-band cliffs around 51-55 / 56-60 while still
// preserving a structural-zero hurdle and fat-tailed played-player outcomes.

data {
  int<lower=1> N;
  array[N] int<lower=31, upper=60> pick;
  array[N] int<lower=0, upper=1> played;
  vector[N] ws4;
}

parameters {
  // P(play) curve, pick 31 anchor + smooth adjacent-pick drift/random walk.
  real logit_play_31;
  real delta_logit_play;
  real<lower=0> tau_logit_play_rw;
  vector[29] z_logit_play_step;

  // Conditional played-player mean curve on log scale.
  // The mean is constrained positive, while the Student-t likelihood still
  // allows individual played outcomes to be negative.
  real log_cond_mean_ws_31;
  real delta_log_cond_mean_ws;
  real<lower=0> tau_log_cond_mean_ws_rw;
  vector[29] z_log_cond_mean_ws_step;

  // Conditional played-player residual scale curve on log scale.
  real log_sigma_ws_31;
  real<lower=0> tau_log_sigma_ws_rw;
  vector[29] z_log_sigma_ws_step;

  // Conditional played-player Student-t tail thickness.
  real<lower=2> nu_ws;
}

transformed parameters {
  vector[30] logit_play_pick;
  vector<lower=0, upper=1>[30] p_play_pick;

  vector[30] log_cond_mean_ws_pick;
  vector<lower=0>[30] cond_mean_ws_pick;

  vector[30] log_sigma_ws_pick;
  vector<lower=0>[30] sigma_ws_pick;

  logit_play_pick[1] = logit_play_31;
  log_cond_mean_ws_pick[1] = log_cond_mean_ws_31;
  log_sigma_ws_pick[1] = log_sigma_ws_31;

  for (p in 2:30) {
    logit_play_pick[p] = logit_play_pick[p - 1] +
                         delta_logit_play +
                         tau_logit_play_rw * z_logit_play_step[p - 1];

    log_cond_mean_ws_pick[p] = log_cond_mean_ws_pick[p - 1] +
                               delta_log_cond_mean_ws +
                               tau_log_cond_mean_ws_rw * z_log_cond_mean_ws_step[p - 1];

    log_sigma_ws_pick[p] = log_sigma_ws_pick[p - 1] +
                           tau_log_sigma_ws_rw * z_log_sigma_ws_step[p - 1];
  }

  p_play_pick = inv_logit(logit_play_pick);
  cond_mean_ws_pick = exp(log_cond_mean_ws_pick);
  sigma_ws_pick = exp(log_sigma_ws_pick);
}

model {
  // P(play) priors. Pick 31 is usually high, and the average step is mildly
  // downward across the round. tau controls local slot wiggle.
  logit_play_31 ~ normal(logit(0.88), 0.60);
  delta_logit_play ~ normal(-0.05, 0.05);
  tau_logit_play_rw ~ normal(0, 0.10);
  z_logit_play_step ~ std_normal();

  // Conditional played-player mean priors. The curve starts around 5-6 first-
  // four-year WS for early seconds and drifts mildly downward.
  log_cond_mean_ws_31 ~ normal(log(5.5), 0.55);
  delta_log_cond_mean_ws ~ normal(-0.015, 0.04);
  tau_log_cond_mean_ws_rw ~ normal(0, 0.06);
  z_log_cond_mean_ws_step ~ std_normal();

  // Conditional played-player residual scale priors.
  log_sigma_ws_31 ~ normal(log(7.0), 0.55);
  tau_log_sigma_ws_rw ~ normal(0, 0.10);
  z_log_sigma_ws_step ~ std_normal();

  // Fat-tailed played-player outcome distribution.
  nu_ws - 2 ~ exponential(0.20);

  for (n in 1:N) {
    int s = pick[n] - 30;

    played[n] ~ bernoulli(p_play_pick[s]);

    if (played[n] == 1) {
      ws4[n] ~ student_t(nu_ws, cond_mean_ws_pick[s], sigma_ws_pick[s]);
    }
  }
}

generated quantities {
  vector[30] p_play;
  vector[30] cond_mean_ws;
  vector[30] cond_scale_ws;
  vector[30] cond_sd_ws;
  vector[30] ev;
  vector[30] ev_sd;

  array[N] int played_rep;
  vector[N] ws4_rep;
  vector[N] log_lik;

  for (p in 1:30) {
    real cond_var;

    p_play[p] = p_play_pick[p];
    cond_mean_ws[p] = cond_mean_ws_pick[p];
    cond_scale_ws[p] = sigma_ws_pick[p];

    // Student-t variance exists because nu_ws is constrained above 2.
    cond_var = square(sigma_ws_pick[p]) * nu_ws / (nu_ws - 2);
    cond_sd_ws[p] = sqrt(cond_var);

    ev[p] = p_play_pick[p] * cond_mean_ws_pick[p];
    ev_sd[p] = sqrt(fmax(0,
                         p_play_pick[p] * (cond_var + square(cond_mean_ws_pick[p])) -
                         square(ev[p])));
  }

  for (n in 1:N) {
    int s = pick[n] - 30;

    played_rep[n] = bernoulli_rng(p_play_pick[s]);
    if (played_rep[n] == 1) {
      ws4_rep[n] = student_t_rng(nu_ws, cond_mean_ws_pick[s], sigma_ws_pick[s]);
    } else {
      ws4_rep[n] = 0;
    }

    log_lik[n] = bernoulli_lpmf(played[n] | p_play_pick[s]);
    if (played[n] == 1) {
      log_lik[n] += student_t_lpdf(ws4[n] | nu_ws, cond_mean_ws_pick[s], sigma_ws_pick[s]);
    }
  }
}
