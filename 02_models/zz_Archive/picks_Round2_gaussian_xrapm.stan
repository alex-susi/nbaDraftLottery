// -----------------------------------------------------------------------------
// 2nd Round Pick NBA Draft Valuation Model: Gaussian conditional mean (production
// until 2026-10-04; kept as a CV benchmark)
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Hurdle / structural-zero layer
//        Players drafted in the second round don't always log NBA minutes.
//        played[n] ~ Bernoulli(pi_p[p])
//
//   2. Conditional mean layer
//        If the player plays, four-season xRAPM wins above replacement (war4)
//        are fit with a Gaussian working likelihood around a smoothed
//        conditional-mean curve:
//
//        war4[n] | played[n] = 1 ~ Normal(mean_play[p], sd_play[p])
//
//      mean_play[p] feeds expected pick value, so it must track the MEAN of
//      played outcomes. Played second-round value is right-skewed (median just
//      below replacement, a few starters). A shifted-lognormal mixture fit to
//      this scale centered on the typical player and understated the mean by
//      ~35%. The skewed outcome distribution is NOT taken from this
//      likelihood: 03_models.R draws outcomes as mean_play[p] plus empirical
//      residuals from played players at nearby picks.
//
//   3. Smoothing across picks
//        The play curve and conditional-mean curve evolve across picks 31-60
//        through adjacent-pick random walks with drift; the spread follows a
//        log-linear trend (a random-walk spread was unstable under the
//        Gaussian working likelihood).
//
//        eta[p]      = logit(pi_p[p])
//        eta[p]      = eta[p-1] + d_pi + tau_pi * z_pi[p-1]
//        mean_play[p] = mean_play[p-1] + d_mp + tau_mp * z_mp[p-1]
//        log_sd[p]   = log_sd_31 + d_sd * (p - 31)   (log-linear trend)
//
//   4. Expected asset value
//        ev[p] = pi_p[p] * mean_play[p]
//
// Priors are on the xRAPM wins-above-replacement scale. Empirically (1996-2022
// drafts): ~89% of pick-31 players log minutes vs ~48% at pick 60; played
// players average ~1.4 wins at picks 31-40 and ~0.2 at picks 51-60.
// -----------------------------------------------------------------------------


data {
  int<lower=1> N;                        // Number of players
  array[N] int<lower=31, upper=60> pick; // Pick each player was drafted at (31-60)
  array[N] int<lower=0, upper=1> played; // Play indicator (0 = no NBA minutes in the window)
  vector[N] war4;                        // Four-season xRAPM wins above replacement (0 if never played)
}



parameters {
  // ---------------------------------------------------------------------------
  // Play probability curve: pi_p[p] = P(player at pick p logs NBA minutes)
  // ---------------------------------------------------------------------------
  real eta_31;          // baseline play probability for pick 31 (logit scale)
  real d_pi;            // average change in logit play probability per pick
  real<lower=0> tau_pi; // pick-to-pick curve flexibility
  vector[29] z_pi;      // pick-by-pick curve adjustments


  // ---------------------------------------------------------------------------
  // Conditional mean of played outcomes: mean_play[p]
  // ---------------------------------------------------------------------------
  real mp_31;           // mean played value at pick 31
  real d_mp;            // average change in mean played value per pick
  real<lower=0> tau_mp; // pick-to-pick curve flexibility
  vector[29] z_mp;      // pick-by-pick curve adjustments


  // ---------------------------------------------------------------------------
  // Spread of played outcomes: sd_play[p]
  // ---------------------------------------------------------------------------
  real log_sd_31;       // played-outcome SD at pick 31 (log scale)
  real d_sd;            // linear change in log SD per pick
}



transformed parameters {
  vector[30] eta;                    // Play probabilities (logit scale)
  vector<lower=0, upper=1>[30] pi_p; // Play probabilities
  vector[30] mean_play;              // Mean four-season value if the player plays
  vector[30] log_sd;                 // Played-outcome SD (log scale)
  vector<lower=0>[30] sd_play;       // Played-outcome SD

  eta[1]       = eta_31;
  mean_play[1] = mp_31;

  for (j in 2:30) {
    eta[j]       = eta[j - 1] + d_pi + tau_pi * z_pi[j - 1];
    mean_play[j] = mean_play[j - 1] + d_mp + tau_mp * z_mp[j - 1];
  }

  for (j in 1:30) {
    log_sd[j] = log_sd_31 + d_sd * (j - 1);  // spread trends log-linearly across the round
  }

  pi_p    = inv_logit(eta);
  sd_play = exp(log_sd);
}



model {
  // Priors
  // Play probability
  eta_31 ~ normal(logit(0.88), 0.60);  // Pick 31 very likely to appear in the NBA
  d_pi   ~ normal(-0.05, 0.05);        // Declines gradually from pick 31-60
  tau_pi ~ normal(0, 0.10);
  z_pi   ~ std_normal();

  // Conditional mean of played outcomes
  mp_31  ~ normal(1.3, 1.5);           // Played pick-31 players average ~1.3 wins
  d_mp   ~ normal(-0.04, 0.05);        // Declines gradually from pick 31-60
  tau_mp ~ normal(0, 0.05);
  z_mp   ~ std_normal();

  // Played-outcome spread
  log_sd_31 ~ normal(log(4), 0.50);    // Played pick-31 outcomes spread ~4 wins
  d_sd      ~ normal(-0.01, 0.02);     // Spread narrows slightly later in the round


  // Likelihood
  for (n in 1:N) {
    int j = pick[n] - 30;              // Convert NBA pick number 31-60 into index 1-30

    played[n] ~ bernoulli(pi_p[j]);

    if (played[n] == 1) {
      war4[n] ~ normal(mean_play[j], sd_play[j]);
    }
  }
}



generated quantities {
  vector[30] ev;          // Expected asset value: play chance times mean value if played
  vector[30] ev_sd;       // Total pick SD including zero outcomes and played variance
  array[N] int play_rep;  // Simulated played / did-not-play result for each historical row
  vector[N] log_lik;      // Per-player log likelihood for PSIS-LOO and validation


  for (j in 1:30) {
    real m2 = pi_p[j] * (square(sd_play[j]) + square(mean_play[j]));

    ev[j]    = pi_p[j] * mean_play[j];
    ev_sd[j] = sqrt(fmax(0, m2 - square(ev[j])));
  }


  for (n in 1:N) {
    int j = pick[n] - 30;

    play_rep[n] = bernoulli_rng(pi_p[j]);

    log_lik[n] = bernoulli_lpmf(played[n] | pi_p[j]);
    if (played[n] == 1) {
      log_lik[n] += normal_lpdf(war4[n] | mean_play[j], sd_play[j]);
    }
  }
}
