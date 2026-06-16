// VERSION: pick_play_r2_v4_right_skew_checked.stan
// Checked export: right-skew shifted-lognormal mixture with global rare-upside component.
// Round 2 smoothed hurdle model for NBA draft pick valuation
// ---------------------------------------------------------------
// This version keeps the adjacent-pick borrowing structure from the v3 round-2
// model, but replaces the symmetric Student-t played-player outcome layer with
// a shifted, right-skewed lognormal contamination model:
//
//   played[n] ~ Bernoulli(P(play | pick))
//   WS4[n] | played[n] = 1 = ws_floor + LogNormal(typical component)
//                         or ws_floor + LogNormal(rare upside component)
//
// The floor prevents unrealistic extreme negative outcomes. The rare upside
// component lets the model acknowledge that second-round picks occasionally
// produce star-level outcomes, while its strongly regularized, global mixture
// probability prevents exact-pick curves from overfitting players like Jokic,
// Draymond, Ginobili, etc.
//
// The three pick-specific curves still borrow strength from nearby second-round
// slots through adjacent-pick random walks, matching the smoothing idea used in
// pick_value_v3.stan:
//
//   logit P(play)[p]              = logit P(play)[p-1] + drift + tau * z[p-1]
//   log typical shifted median[p] = log median[p-1]    + drift + tau * z[p-1]
//   log lognormal sigma[p]        = log sigma[p-1]              + tau * z[p-1]
//
// Expected asset value for picks 31-60 is generated directly as:
//
//   EV[p] = P(play)[p] * E[WS4 | play, p]

data {
  int<lower=1> N;
  array[N] int<lower=31, upper=60> pick;
  array[N] int<lower=0, upper=1> played;
  vector[N] ws4;

  // Practical lower bound for first-four-year WS among players who appear in
  // the NBA. 03_models.R sets this to a conservative value, then moves it down
  // only if an observed played-player outcome is lower.
  real<upper=0> ws_floor;
}

parameters {
  // P(play) curve, pick 31 anchor + smooth adjacent-pick drift/random walk.
  real logit_play_31;
  real delta_logit_play;
  real<lower=0> tau_logit_play_rw;
  vector[29] z_logit_play_step;

  // Typical played-player shifted-outcome median curve on log scale.
  // This is log(WS4 - ws_floor), not log(E[WS4 | play]).
  real log_cond_mean_ws_31;
  real delta_log_cond_mean_ws;
  real<lower=0> tau_log_cond_mean_ws_rw;
  vector[29] z_log_cond_mean_ws_step;

  // Typical played-player lognormal scale curve.
  real log_sigma_ws_31;
  real<lower=0> tau_log_sigma_ws_rw;
  vector[29] z_log_sigma_ws_step;

  // Global rare-upside component. Keeping this global rather than pick-specific
  // prevents exact slots from chasing individual historical outliers.
  real logit_upside_prob;
  real<lower=0> upside_log_shift;
  real<lower=1> upside_sigma_mult;
}

transformed parameters {
  vector[30] logit_play_pick;
  vector<lower=0, upper=1>[30] p_play_pick;

  vector[30] log_cond_mean_ws_pick;
  vector[30] log_sigma_ws_pick;
  vector<lower=0>[30] sigma_ws_pick;

  real<lower=0, upper=1> upside_prob;

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
  sigma_ws_pick = exp(log_sigma_ws_pick);
  upside_prob = inv_logit(logit_upside_prob);
}

model {
  // P(play) priors. Pick 31 is usually high, and the average step is mildly
  // downward across the round. tau controls local slot wiggle.
  logit_play_31 ~ normal(logit(0.88), 0.60);
  delta_logit_play ~ normal(-0.05, 0.05);
  tau_logit_play_rw ~ normal(0, 0.10);
  z_logit_play_step ~ std_normal();

  // Typical played-player shifted median priors. With ws_floor around -8, this
  // centers early-second played outcomes around 5-6 WS before the rare-upside
  // component, then drifts mildly downward.
  log_cond_mean_ws_31 ~ normal(log(13.0), 0.35);
  delta_log_cond_mean_ws ~ normal(-0.015, 0.035);
  tau_log_cond_mean_ws_rw ~ normal(0, 0.05);
  z_log_cond_mean_ws_step ~ std_normal();

  // Lognormal scale priors. These are intentionally much tighter than the old
  // WS-scale Student-t sigma prior; upside is handled by the rare component,
  // not by inflating a symmetric residual scale.
  log_sigma_ws_31 ~ normal(log(0.45), 0.25);
  tau_log_sigma_ws_rw ~ normal(0, 0.05);
  z_log_sigma_ws_step ~ std_normal();

  // Rare upside. Strong prior says star outcomes exist, but are uncommon and
  // should not dominate the exact-pick mean curve.
  logit_upside_prob ~ normal(logit(0.025), 0.75);
  upside_log_shift ~ normal(log(3.0), 0.35);
  upside_sigma_mult ~ lognormal(log(1.35), 0.20);

  for (n in 1:N) {
    int s = pick[n] - 30;

    played[n] ~ bernoulli(p_play_pick[s]);

    if (played[n] == 1) {
      real y_shift = ws4[n] - ws_floor;
      if (y_shift <= 0) {
        reject("Played-player ws4 must be greater than ws_floor. ws4=", ws4[n],
               ", ws_floor=", ws_floor);
      }
      target += log_mix(
        upside_prob,
        lognormal_lpdf(y_shift |
                       log_cond_mean_ws_pick[s] + upside_log_shift,
                       sigma_ws_pick[s] * upside_sigma_mult),
        lognormal_lpdf(y_shift |
                       log_cond_mean_ws_pick[s],
                       sigma_ws_pick[s])
      );
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

  // One posterior predictive player-outcome draw per exact second-round pick.
  // 03_models.R uses this for simulations and curve outcome quantiles.
  vector[30] ws4_pick_rep;

  array[N] int played_rep;
  vector[N] ws4_rep;
  vector[N] log_lik;

  for (p in 1:30) {
    real mu_typ = log_cond_mean_ws_pick[p];
    real sig_typ = sigma_ws_pick[p];
    real mu_up = log_cond_mean_ws_pick[p] + upside_log_shift;
    real sig_up = sigma_ws_pick[p] * upside_sigma_mult;

    real typ_m1 = exp(mu_typ + 0.5 * square(sig_typ));
    real typ_m2 = exp(2 * mu_typ + 2 * square(sig_typ));
    real up_m1 = exp(mu_up + 0.5 * square(sig_up));
    real up_m2 = exp(2 * mu_up + 2 * square(sig_up));
    real shift_m1 = (1 - upside_prob) * typ_m1 + upside_prob * up_m1;
    real shift_m2 = (1 - upside_prob) * typ_m2 + upside_prob * up_m2;
    real cond_second;
    real cond_var;

    p_play[p] = p_play_pick[p];
    cond_mean_ws[p] = ws_floor + shift_m1;
    cond_second = square(ws_floor) + 2 * ws_floor * shift_m1 + shift_m2;
    cond_var = fmax(0, cond_second - square(cond_mean_ws[p]));

    // cond_scale_ws is the typical-component WS-scale SD; cond_sd_ws is the
    // full played-player mixture SD including rare upside.
    cond_scale_ws[p] = sqrt(fmax(0, typ_m2 - square(typ_m1)));
    cond_sd_ws[p] = sqrt(cond_var);

    ev[p] = p_play_pick[p] * cond_mean_ws[p];
    ev_sd[p] = sqrt(fmax(0,
                         p_play_pick[p] * (cond_var + square(cond_mean_ws[p])) -
                         square(ev[p])));

    if (bernoulli_rng(p_play_pick[p]) == 1) {
      if (bernoulli_rng(upside_prob) == 1) {
        ws4_pick_rep[p] = ws_floor + lognormal_rng(mu_up, sig_up);
      } else {
        ws4_pick_rep[p] = ws_floor + lognormal_rng(mu_typ, sig_typ);
      }
    } else {
      ws4_pick_rep[p] = 0;
    }
  }

  for (n in 1:N) {
    int s = pick[n] - 30;

    played_rep[n] = bernoulli_rng(p_play_pick[s]);
    if (played_rep[n] == 1) {
      if (bernoulli_rng(upside_prob) == 1) {
        ws4_rep[n] = ws_floor + lognormal_rng(log_cond_mean_ws_pick[s] + upside_log_shift,
                                              sigma_ws_pick[s] * upside_sigma_mult);
      } else {
        ws4_rep[n] = ws_floor + lognormal_rng(log_cond_mean_ws_pick[s],
                                              sigma_ws_pick[s]);
      }
    } else {
      ws4_rep[n] = 0;
    }

    log_lik[n] = bernoulli_lpmf(played[n] | p_play_pick[s]);
    if (played[n] == 1) {
      real y_shift = ws4[n] - ws_floor;
      if (y_shift <= 0) {
        log_lik[n] = negative_infinity();
      } else {
        log_lik[n] += log_mix(
          upside_prob,
          lognormal_lpdf(y_shift |
                         log_cond_mean_ws_pick[s] + upside_log_shift,
                         sigma_ws_pick[s] * upside_sigma_mult),
          lognormal_lpdf(y_shift |
                         log_cond_mean_ws_pick[s],
                         sigma_ws_pick[s])
        );
      }
    }
  }
}
