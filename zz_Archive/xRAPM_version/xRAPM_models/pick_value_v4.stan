// pick_value_v4.stan
// Bayesian draft pick value curve — player-level version with adjacent-pick
// smoothing for pick-specific residual variance.
//
// v4 CHANGES vs v3:
//   * Outcome is now TPI4 — first-4-season TOTAL POINTS IMPACT — instead of
//     first-4-year Win Shares:
//
//       TPI[player, season] = (xRAPM / 100) * possessions played
//       TPI4[player]        = sum over the player's first four post-draft
//                             seasons (calendar-anchored; a fully missed
//                             season contributes 0)
//
//   * Pick range extended from 1-30 to 1-P (P = 60: both rounds). P is data
//     so the same model can be re-fit on round 1 only without recompiling.
//   * Optional additive ROUND-2 LEVEL SHIFT delta_r2 (data toggle
//     use_r2_offset). Fitting the model twice on identical rows — once with
//     the toggle off (one unified power-law over 1-60) and once with it on —
//     makes loo_compare() a direct, pointwise-valid answer to the design
//     question "single unified curve vs. boundary adjustment at pick 30".
//     When the toggle is off, delta_r2 simply samples its prior and does not
//     enter the likelihood, so log_lik stays comparable across both fits.
//   * Priors recalibrated to the TPI scale (see PRIOR ELICITATION below).
//
// Likelihood (one row per drafted player):
//
//   tpi4[n] ~ Student_t(nu, mu[pick[n]], sigma_pick[pick[n]])
//
// Mean curve:
//
//   mu[p] = alpha / p^beta + gamma + delta_r2 * 1{p > 30}   (if toggled on)
//
// Variance model (unchanged from v3, now over P slots):
//
//   log_sigma_pick[1] = log_sigma_1
//   log_sigma_pick[p] = log_sigma_pick[p - 1]
//                       + tau_log_sigma_rw * z_sigma_step[p - 1]
//
// ---------------------------------------------------------------------------
// PRIOR ELICITATION (PROVISIONAL — review against the 03_eda/ plots produced
// by nba_lottery_2.R BEFORE production runs, and re-center if the empirical
// scale disagrees):
//
//   xRAPM typically spans roughly -5 to +8 points per 100 possessions, and a
//   rotation player logs ~2,000-7,000 possessions per season. A star pick's
//   four-year TPI lands on the order of +150 to +400 points; a never-plays
//   bust is exactly 0; a heavy-minutes bad player can be MEANINGFULLY
//   NEGATIVE. This last property is a real change from Win Shares, which is
//   bounded below near 0 for almost everyone: the lower tail now carries
//   information, and the Student-t must absorb genuinely negative outcomes
//   for deep-bench / G-League-shuttle picks in round 2.
//
//   * alpha  ~ height of the pick-1 premium over the baseline. Average #1
//     picks mix negative rookie years with positive years 3-4, so the prior
//     centers the premium near 150 TPI with wide uncertainty (x/÷ ~2.2 at 1sd).
//   * gamma  ~ late-second-round baseline. Centered at 0 (NOT 2 as in the WS
//     model) and ALLOWED TO BE NEGATIVE: a marginal pick who plays is often a
//     net-minus on-court.
//   * beta   ~ decay rate. Centered slightly steeper (0.80) than the WS model
//     (0.55) because the TPI curve must fall from a ~150-250 premium at pick
//     1 to near-flat by the 40s; the data will refine this.
//   * sigma_pick[1] ~ 120: realized #1-pick TPI4 ranges from ~-50 (bust who
//     plays) to ~+500 (generational), so the residual scale is large.
//   * tau_log_sigma_rw tightened slightly (0.10 vs 0.15) because the random
//     walk now has 59 steps instead of 29; the same per-step scale would
//     allow much larger cumulative drift from pick 1 to pick 60.
//   * delta_r2 ~ normal(0, 25): a level shift of ±25 TPI at the round
//     boundary is plausible a priori (two-way contracts, non-guaranteed
//     deals, stash picks change round-2 opportunity), but the prior keeps it
//     from soaking up curvature the power law should explain.
// ---------------------------------------------------------------------------

data {
  int<lower=1> N;                        // number of drafted-player rows
  int<lower=1> P;                        // max pick slot modeled (60 = both rounds)
  array[N] int<lower=1, upper=P> pick;   // overall draft slot, 1..P
  vector[N] tpi4;                        // first-4-season Total Points Impact
  int<lower=0, upper=1> use_r2_offset;   // 1 = additive level shift for picks > 30
}



transformed data {
  // Round-2 indicator per player row. The round boundary is fixed at overall
  // pick 30 for the modeled era; pre-2004 first rounds had 29 picks, but the
  // slot-level curve treats overall pick number as the running variable, so a
  // historical #30 (then round 2) and a modern #30 (round 1) share a slot.
  vector[N] is_r2;
  for (n in 1:N) {
    is_r2[n] = pick[n] > 30 ? 1.0 : 0.0;
  }
}



parameters {
  // --------------------------------------------------------------------------
  // Mean-curve parameters
  // --------------------------------------------------------------------------

  // log_alpha is sampled on the unconstrained real line, then exponentiated
  // into alpha > 0 in transformed parameters.
  //
  // alpha controls the height of the pick-value curve (the pick-1 premium
  // over the gamma baseline), now on the TPI scale.
  real log_alpha;

  // log_beta is sampled on the unconstrained real line, then exponentiated
  // into beta > 0 in transformed parameters.
  //
  // beta controls how quickly expected value declines by draft slot.
  real log_beta;

  // gamma is the baseline / lower asymptote of the curve. As pick number gets
  // large, alpha / p^beta shrinks and the curve approaches gamma (+ delta_r2
  // in round 2 when the offset is active). Unlike the WS model, gamma may be
  // negative on the TPI scale.
  real gamma;

  // Additive round-2 level shift. Only enters the likelihood when
  // use_r2_offset == 1; otherwise it samples its prior and is ignored, which
  // keeps log_lik pointwise-comparable between the two fits.
  real delta_r2;



  // --------------------------------------------------------------------------
  // Adjacent-pick-smoothed residual scale parameters
  // --------------------------------------------------------------------------

  // log residual scale at pick 1. The random walk for log sigma starts here.
  real log_sigma_1;

  // Standardized random-walk innovations: z_sigma_step[p-1] is the step from
  // pick p-1 to pick p, for p in 2..P.
  vector[P - 1] z_sigma_step;

  // Adjacent-pick smoothing scale. Smaller values force sigma_pick[p] to
  // change smoothly across picks; larger values allow local volatility jumps.
  real<lower=0> tau_log_sigma_rw;



  // --------------------------------------------------------------------------
  // Student-t tail thickness
  // --------------------------------------------------------------------------

  // nu controls tail thickness: lower nu = heavier tails (more tolerance for
  // extreme stars / busts), higher nu = closer to normal residuals. We
  // constrain nu > 2 so the Student-t variance is finite, and place the prior
  // on the excess above 2 in the model block. With a TPI outcome the LOWER
  // tail is no longer pinned near 0 the way WS was, so nu now has to
  // accommodate genuine two-sided extremes.
  real<lower=2> nu;
}



transformed parameters {
  // Convert log-scale alpha back to the positive draft-value curve scale.
  real<lower=0> alpha = exp(log_alpha);

  // Convert log-scale beta back to the positive curve-decay scale.
  real<lower=0> beta = exp(log_beta);

  // Log residual standard deviation for each pick (random walk), and its
  // positive-scale transform.
  vector[P] log_sigma_pick;
  vector<lower=0>[P] sigma_pick;

  // Anchor the log-sigma random walk at pick 1.
  log_sigma_pick[1] = log_sigma_1;

  // Build the adjacent-pick random walk: nearby picks share similar residual
  // volatility unless the data strongly support a local change.
  for (p in 2:P) {
    log_sigma_pick[p] = log_sigma_pick[p - 1]
                        + tau_log_sigma_rw * z_sigma_step[p - 1];
  }

  // Convert log residual scales back to positive residual scales.
  sigma_pick = exp(log_sigma_pick);
}



model {
  // --------------------------------------------------------------------------
  // Mean-curve priors (TPI scale — PROVISIONAL, see header)
  // --------------------------------------------------------------------------

  // Pick-1 premium centered near 150 TPI, broad.
  log_alpha ~ normal(log(150), 0.80);

  // Decay rate centered near 0.80 — steeper than the WS-scale prior because
  // the curve must flatten to near-indistinguishable values by the 40s-60s.
  log_beta ~ normal(log(0.80), 0.50);

  // Baseline centered at ZERO, explicitly allowing a negative late-round
  // asymptote (marginal picks who play are frequently net-minus on court).
  gamma ~ normal(0, 40);

  // Round-2 level shift: modest a priori, two-sided.
  delta_r2 ~ normal(0, 25);



  // --------------------------------------------------------------------------
  // Variance-smoothing priors
  // --------------------------------------------------------------------------

  // Starting residual scale at pick 1: prior center ~120 TPI.
  log_sigma_1 ~ normal(log(120), 0.70);

  // Standard normal innovations for the (non-centered) random-walk steps.
  z_sigma_step ~ std_normal();

  // Half-normal smoothing prior, tightened relative to v3 (0.10 vs 0.15)
  // because the walk now spans 59 steps instead of 29.
  tau_log_sigma_rw ~ normal(0, 0.10);



  // --------------------------------------------------------------------------
  // Student-t tail prior
  // --------------------------------------------------------------------------

  // exponential(0.20) has mean 5, so nu is centered near 7 while permitting
  // very heavy tails (nu -> 2) or near-normal residuals (large nu).
  nu - 2 ~ exponential(0.20);



  // --------------------------------------------------------------------------
  // Likelihood
  // --------------------------------------------------------------------------

  for (n in 1:N) {
    // Expected first-4-season TPI for this player's draft slot.
    real mu = alpha / pow(pick[n], beta) + gamma
              + (use_r2_offset == 1 ? delta_r2 * is_r2[n] : 0.0);

    // Player-level outcome model. The Student-t likelihood is robust to
    // extreme draft outcomes in BOTH tails — superstars above and
    // heavy-minutes negative players below — and sigma_pick[pick[n]] lets
    // each draft slot carry its own residual volatility.
    tpi4[n] ~ student_t(nu, mu, sigma_pick[pick[n]]);
  }
}



generated quantities {
  vector[P] tpi_pred;        // posterior mean curve by pick (1..P)
  vector[P] tpi_pred_sd;     // player-level residual scale by pick
  vector[N] log_lik;         // pointwise log-likelihood for player-level LOO
  vector[N] tpi4_rep;        // replicated player-level outcomes for PPC

  // Posterior mean curve and residual-scale curve by pick.
  for (p in 1:P) {
    real mu = alpha / pow(p, beta) + gamma
              + (use_r2_offset == 1 && p > 30 ? delta_r2 : 0.0);

    tpi_pred[p] = mu;
    tpi_pred_sd[p] = sigma_pick[p];
  }

  // Pointwise log likelihoods and posterior predictive replicates.
  for (n in 1:N) {
    real mu = alpha / pow(pick[n], beta) + gamma
              + (use_r2_offset == 1 ? delta_r2 * is_r2[n] : 0.0);

    log_lik[n] = student_t_lpdf(tpi4[n] | nu, mu, sigma_pick[pick[n]]);
    tpi4_rep[n] = student_t_rng(nu, mu, sigma_pick[pick[n]]);
  }
}
