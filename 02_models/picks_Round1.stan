// -----------------------------------------------------------------------------
// 1st Round Pick NBA Draft Valuation Model
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Player outcome
//        Each drafted player has one row. The outcome is xRAPM wins above
//        replacement (-2.0 baseline) over the four rookie-contract seasons after
//        the draft (war4). Seasons a player misses count as zero.
//
//        Outcomes are right-skewed: most picks land near replacement and a few
//        become stars. The ex-Gaussian (Normal + Exponential) captures that
//        skew while keeping the MEAN as a parameter:
//
//        war4[n] = Normal(mu[p] - tau_x[p], sigma_g[p]) + Exponential(mean tau_x[p])
//        E[war4 | pick p] = mu[p]
//
//      so mu[p] stays an unbiased expected pick value. (A Student-t centers on
//      the typical outcome and sat ~1-2 wins below slot means; a
//      shifted-lognormal mixture extrapolated its tail and overstated top
//      picks. See 03_validation/r1_likelihood_cv.R.)
//
//   2. Pick value curve: strictly decreasing, with pick-specific gaps
//        The curve is built from the gaps between adjacent picks:
//
//        mu[30]    = alpha / 30^beta + gamma
//        mu[p]     = mu[p + 1] + gap[p]                       (p = 1..29)
//        gap[p]    = alpha * (p^-beta - (p + 1)^-beta) * exp(eps[p])
//
//      Every gap is positive, so pick p is always worth more than pick p + 1;
//      the posterior uncertainty is in HOW MUCH more. The power law sets the
//      typical gap, and eps lets each gap depart from it. eps is a random walk,
//      so adjacent gaps share information (nearer picks more strongly), and
//      tau_eps (learned from the data) sets how far the curve can bend away
//      from the power law; tau_eps -> 0 recovers a pure power law. eps is
//      centered so alpha keeps setting the overall height.
//
//   3. Outcome spread and skew by pick
//        Each pick has a total outcome SD (sd_pick) and a skew share
//        r_skew = tau_x / sd_pick (0 = symmetric Gaussian, 1 = pure exponential
//        upside). Both follow SECOND-order random walks: the pick-to-pick
//        slope itself drifts slowly, so the curves can steepen or level off
//        gradually but cannot zigzag around a handful of players:
//
//        slope_sd[p] = slope_sd[p - 1] + tau_sd * z_sd[p - 1]   (slope_sd[1] = d_sd)
//        log_sd[p]   = log_sd[p - 1] + slope_sd[p - 1]
//        (same for logit_r with d_r, tau_r, z_r)
//
//      A first-order walk (zz_Archive/picks_Round1_exgauss_rw1_xrapm.stan)
//      followed eight-player bumps in the 2015-2022 data that do not appear in
//      earlier classes; the second-order version ties it in draft-class CV and
//      gives a smooth outcome band.
//
//      Fit with adapt_delta = 0.99 and starting points near the prior means
//      (r1_init in 03_models.R): every slope change compounds across later
//      picks, so generic random inits can start with absurd spread curves.
//
//        tau_x[p]   = sd_pick[p] * r_skew[p]
//        sigma_g[p] = sd_pick[p] * sqrt(1 - r_skew[p]^2)
//
//      Skewness = 2 * r_skew^3, so r_skew is capped at 0.99 (skewness <= 1.94)
//      to keep a Gaussian component. Late first-round picks press against a
//      0.95 cap, and 0.99 improved held-out tail calibration.
//
// Priors are on the xRAPM wins-above-replacement scale. Empirically (2015-2022
// drafts): picks 1-3 average ~12 wins with a spread ~13; picks 21-30 average
// ~2.8 with a spread ~6; skewness ~0.2 at the top and ~1.3 late in the round.
// -----------------------------------------------------------------------------


data {
  int<lower=1> N;                         // Number of players
  array[N] int<lower=1, upper=30> pick;   // Pick each player was drafted at (1-30)
  vector[N] war4;                         // Four-season xRAPM wins above replacement
}



parameters {
  // ---------------------------------------------------------------------------
  // Pick value curve
  // ---------------------------------------------------------------------------
  real log_alpha;         // Height of the first-round value curve (log scale)
  real log_beta;          // Decline rate from early to late first round (log scale)
  real gamma;             // Late-first-round baseline value the curve approaches
  vector[29] z_eps;       // Pick-by-pick adjustments to the gap curve
  real<lower=0> tau_eps;  // How far gaps can depart from the power law


  // ---------------------------------------------------------------------------
  // Outcome spread and skew by pick
  // ---------------------------------------------------------------------------
  real log_sd_1;          // Outcome SD at pick 1 (log scale)
  real d_sd;              // Initial pick-to-pick change in log SD
  vector[28] z_sd;        // Pick-by-pick changes in that slope
  real<lower=0> tau_sd;   // How fast the slope can change

  real logit_r_1;         // Skew share at pick 1 (logit scale)
  real d_r;               // Initial pick-to-pick change in logit skew share
  vector[28] z_r;
  real<lower=0> tau_r;
}



transformed parameters {
  real<lower=0> alpha = exp(log_alpha);
  real<lower=0> beta  = exp(log_beta);

  vector[29] eps;                  // Log departure of each gap from the power law
  vector<lower=0>[29] gap;         // mu[p] - mu[p + 1]
  vector[30] mu;                   // Expected value by pick
  vector<lower=0>[30] sd_pick;
  vector<lower=0, upper=1>[30] r_skew;
  vector<lower=0>[30] tau_x;
  vector<lower=0>[30] sigma_g;

  {
    vector[29] eps_raw = cumulative_sum(tau_eps * z_eps);
    eps = eps_raw - mean(eps_raw);
  }

  for (p in 1:29) {
    gap[p] = alpha * (pow(p, -beta) - pow(p + 1, -beta)) * exp(eps[p]);
  }

  mu[30] = alpha / pow(30, beta) + gamma;
  for (i in 1:29) {
    int p = 30 - i;
    mu[p] = mu[p + 1] + gap[p];
  }

  // Second-order random walks: the slope (pick-to-pick change) itself follows
  // a random walk, so the curves bend gradually instead of zigzagging
  {
    vector[29] slope_sd = d_sd + append_row(0, cumulative_sum(tau_sd * z_sd));
    vector[29] slope_r  = d_r  + append_row(0, cumulative_sum(tau_r * z_r));

    sd_pick = exp(log_sd_1 + append_row(0, cumulative_sum(slope_sd)));
    r_skew  = 0.99 * inv_logit(logit_r_1 + append_row(0, cumulative_sum(slope_r)));
  }
  tau_x   = sd_pick .* r_skew;
  sigma_g = sd_pick .* sqrt(1 - square(r_skew));
}



model {
  // Priors
  log_alpha ~ normal(log(15), 0.60);
  log_beta  ~ normal(log(0.55), 0.50);
  gamma     ~ normal(1.5, 2);
  z_eps     ~ std_normal();
  tau_eps   ~ normal(0, 0.30);          // Adjacent gaps typically within ~30% of each other

  log_sd_1  ~ normal(log(13), 0.50);
  d_sd      ~ normal(-0.03, 0.05);      // Spread narrows ~3% per pick early on
  z_sd      ~ std_normal();
  tau_sd    ~ normal(0, 0.02);          // Slope changes slowly

  logit_r_1 ~ normal(0, 1);
  d_r       ~ normal(0, 0.10);
  z_r       ~ std_normal();
  tau_r     ~ normal(0, 0.03);


  // Likelihood
  war4 ~ exp_mod_normal(mu[pick] - tau_x[pick], sigma_g[pick], inv(tau_x[pick]));
}



generated quantities {
  vector[30] war4_pred;
  vector[30] war4_pred_sd;
  vector[30] war4_outcome;
  vector[N] log_lik;

  war4_pred    = mu;
  war4_pred_sd = sd_pick;

  for (p in 1:30) {
    war4_outcome[p] = exp_mod_normal_rng(mu[p] - tau_x[p], sigma_g[p], inv(tau_x[p]));
  }

  for (n in 1:N) {
    int p = pick[n];
    log_lik[n] = exp_mod_normal_lpdf(war4[n] | mu[p] - tau_x[p], sigma_g[p], inv(tau_x[p]));
  }
}
