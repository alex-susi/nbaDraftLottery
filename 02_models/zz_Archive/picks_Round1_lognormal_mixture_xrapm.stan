// -----------------------------------------------------------------------------
// 1st Round Pick NBA Draft Valuation Model: lognormal-mixture comparison (xRAPM scale)
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Player outcome
//        Each drafted player has one row. The outcome is xRAPM wins above
//        replacement (-2.0 baseline) over the four rookie-contract seasons after
//        the draft (war4). Seasons a player misses count as zero.
//
//   2. Pick value curve (the MEAN of the outcome distribution)
//
//        mu[p] = alpha / p^beta + gamma
//
//      where:
//        alpha controls the overall height of the curve.
//        beta controls how quickly value falls from early to late first.
//        gamma is the lower baseline the curve approaches later in the round.
//
//      mu[p] is the expected four-season value at pick p, so it is the
//      expected pick value used downstream.
//
//   3. Right-skewed player outcomes
//        Four-season value is right-skewed: most picks land near replacement
//        and a minority become stars. Outcomes are a shifted lognormal mixture
//        of a typical component and an upside component:
//
//        war4[n] - war_floor ~
//          LogNormal(m[p],          s[p])           with prob. 1 - u[p]
//          LogNormal(m[p] + delta,  kappa * s[p])   with prob. u[p]
//
//      m[p] is solved from mu[p] so the mixture mean equals mu[p] exactly.
//      war_floor < min(war4) is fixed data supplied by R.
//
//   4. Adjacent-pick smoothing
//        The typical-component spread s[p] follows a random walk with drift on
//        the log scale. The upside share u[p] declines across the round through
//        a non-positive logit drift d_u.
//
// Priors are on the xRAPM wins-above-replacement scale. Empirically (1996-2022
// drafts): pick 1 averages ~16.6 wins; picks 21-30 average ~2.6 with a median
// near 0.
// -----------------------------------------------------------------------------


data {
  int<lower=1> N;                         // Number of players
  array[N] int<lower=1, upper=30> pick;   // Pick each player was drafted at (1-30)
  vector[N] war4;                         // Four-season xRAPM wins above replacement

  // Fixed negative shift for the shifted-lognormal model. Must sit below every
  // observed war4 so that war4[n] - war_floor > 0.
  real<upper=0> war_floor;
}



parameters {
  // ---------------------------------------------------------------------------
  // Pick value curve: mu[p] = alpha / p^beta + gamma
  // ---------------------------------------------------------------------------
  real log_alpha;                  // Height of the first-round value curve (log scale)
  real log_beta;                   // Decline rate from early to late first round (log scale)
  real<lower=war_floor> gamma;     // Late-first-round baseline the mean curve approaches


  // ---------------------------------------------------------------------------
  // Typical-component spread by pick: s[p]
  // ---------------------------------------------------------------------------
  real log_s_1;                    // Spread at pick 1 (log scale)
  real d_s;                        // Average change in log spread per pick
  vector[29] z_s_step;             // Pick-by-pick adjustments to the spread curve
  real<lower=0> tau_log_s_rw;      // How much the spread curve can bend by pick


  // ---------------------------------------------------------------------------
  // Upside component
  // ---------------------------------------------------------------------------
  real u_eta_1;                    // Upside share at pick 1 (logit scale)
  real<upper=0> d_u;               // Logit change per pick, <= 0 -> declines with later picks
  real<lower=0> delta;             // Upside shift on the log scale
  real<lower=1, upper=4> kappa;    // Extra dispersion of upside outcomes
}



transformed parameters {
  real<lower=0> alpha = exp(log_alpha);  // Height of value curve
  real<lower=0> beta  = exp(log_beta);   // Decline rate

  vector[30] mu;                   // Mean four-season value by pick
  vector[30] log_s;                // Typical-component spread, log scale
  vector<lower=0>[30] s;           // Typical-component spread
  vector<lower=0, upper=1>[30] u;  // Upside share by pick
  vector[30] m;                    // Typical-component log-median (shifted scale)

  log_s[1] = log_s_1;
  for (p in 2:30) {
    log_s[p] = log_s[p - 1] + d_s + tau_log_s_rw * z_s_step[p - 1];
  }
  s = exp(log_s);

  for (p in 1:30) {
    mu[p] = alpha / pow(p, beta) + gamma;
    u[p]  = inv_logit(u_eta_1 + d_u * (p - 1));

    // Mixture mean on the shifted scale is
    //   exp(m) * [(1 - u) exp(s^2 / 2) + u exp(delta + kappa^2 s^2 / 2)]
    // so solve for m that makes it equal mu - war_floor.
    m[p] = log(mu[p] - war_floor) -
           log_mix(u[p],
                   delta + 0.5 * square(kappa * s[p]),
                   0.5 * square(s[p]));
  }
}



model {
  // Priors
  log_alpha ~ normal(log(15), 0.60);    // Pick 1 averages ~15 wins above the late-first baseline
  log_beta  ~ normal(log(0.55), 0.50);  // Smooth decline in value from pick 1-30 (scale-free)
  gamma     ~ normal(1.5, 2);           // Late-first mean value modestly above replacement

  log_s_1      ~ normal(log(0.45), 0.40); // Top picks: wide typical outcomes on the shifted scale
  d_s          ~ normal(-0.03, 0.03);     // Typical outcomes tighten in later picks
  z_s_step     ~ std_normal();
  tau_log_s_rw ~ normal(0, 0.10);

  u_eta_1 ~ normal(logit(0.25), 0.60);  // About a quarter of top picks come from the upside bucket
  d_u     ~ normal(-0.03, 0.03);        // Upside share declines across the round
  delta   ~ normal(log(2), 0.40);       // Upside outcomes ~2x the typical shifted value
  kappa   ~ lognormal(log(1.5), 0.30);  // Upside outcomes are more dispersed


  // Likelihood
  for (n in 1:N) {
    int p = pick[n];
    real y_shift = war4[n] - war_floor;

    target += log_mix(u[p],
                      lognormal_lpdf(y_shift | m[p] + delta, kappa * s[p]),
                      lognormal_lpdf(y_shift | m[p], s[p]));
  }
}



generated quantities {
  vector[30] war4_pred;       // Expected four-season value by pick (= mu)
  vector[30] war4_pred_sd;    // Standard deviation of four-season value by pick
  vector[30] war4_pick_rep;   // One simulated outcome per pick
  vector[N] log_lik;          // Per-player log likelihood for PSIS-LOO and validation
  vector[N] war4_rep;         // Simulated four-season value for each historical player row


  for (p in 1:30) {
    real m2 = (1 - u[p]) * exp(2 * m[p] + 2 * square(s[p])) +
              u[p] * exp(2 * (m[p] + delta) + 2 * square(kappa * s[p]));

    war4_pred[p]    = mu[p];
    war4_pred_sd[p] = sqrt(fmax(0, m2 - square(mu[p] - war_floor)));

    if (bernoulli_rng(u[p]) == 1) {
      war4_pick_rep[p] = war_floor + lognormal_rng(m[p] + delta, kappa * s[p]);
    } else {
      war4_pick_rep[p] = war_floor + lognormal_rng(m[p], s[p]);
    }
  }


  for (n in 1:N) {
    int p = pick[n];
    real y_shift = war4[n] - war_floor;

    log_lik[n] = log_mix(u[p],
                         lognormal_lpdf(y_shift | m[p] + delta, kappa * s[p]),
                         lognormal_lpdf(y_shift | m[p], s[p]));

    if (bernoulli_rng(u[p]) == 1) {
      war4_rep[n] = war_floor + lognormal_rng(m[p] + delta, kappa * s[p]);
    } else {
      war4_rep[n] = war_floor + lognormal_rng(m[p], s[p]);
    }
  }
}
