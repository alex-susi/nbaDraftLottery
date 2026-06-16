// -----------------------------------------------------------------------------
// 2nd Round Pick NBA Draft Valuation Model
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Hurdle / structural-zero layer
//        Players drafted in the second round don't always log NBA minutes.
//        played[n] ~ Bernoulli(pi_p[p])
//
//   2. Conditional player outcome layer
//        If the player plays, first-4-year Win Shares are modeled with a
//        shifted lognormal mixture:
//
//        ws4[n] - ws_floor | played[n] = 1 ~
//          LogNormal(m[p],          s[p])           with prob. 1 - u[p]
//          LogNormal(m[p] + delta,  kappa * s[p])   with prob. u[p]
//
//      where:
//        ws_floor < 0 is fixed data supplied by R.
//        m[p] is the log-median of the typical shifted outcome.
//        s[p] is the typical lognormal dispersion.
//        u[p] is the upside mixture probability.
//        delta > 0 shifts the upside component upward on the log scale.
//        kappa >= 1 inflates the upside component's dispersion.
//
//   3. Adjacent-pick smoothing
//        The play curve pi_p, typical-outcome curve m, and dispersion curve s
//        each evolve across picks 31-60 through adjacent-pick random walks.
//        This lets nearby picks borrow strength without forcing the curve to be
//        perfectly smooth or estimating each slot independently.
//
//        eta[p] = logit(pi_p[p])
//        eta[p] = eta[p-1] + d_pi + tau_pi * z_pi[p-1]
//
//        m[p] = m[p-1] + d_m + tau_m * z_m[p-1]
//
//        s_log[p] = log(s[p])
//        s_log[p] = s_log[p-1] + tau_s * z_s[p-1]
//
//   4. Pick-declining upside
//        u[p] declines across the round through a negative logit drift d_u.
//        This preserves the possibility of rare second-round stars while
//        preventing late picks from inheriting implausibly large star odds.
//
//   5. Expected asset value
//        ev[p] = pi_p[p] * E[ws4 | played = 1, pick = p]
//
// -----------------------------------------------------------------------------


data {
  int<lower=1> N;                        // Number of players
  array[N] int<lower=31, upper=60> pick; // Pick each player was drafted at (31-60)
  array[N] int<lower=0, upper=1> played; // Play indicator (0 = never logged an NBA minute)
  vector[N] ws4;                         // First-4-year Win Shares (0 if never played)


  // Fixed negative shift for the shifted-lognormal model.
  //   This must sit below all observed player ws4 outcomes so that
  //   ws4[n] - ws_floor is always strictly positive when played[n] = 1.
  real<upper=0> ws_floor;
}



parameters {
  // ---------------------------------------------------------------------------
  // Play probability curve: pi_p[p] = P(player at pick p logs NBA minutes)
  // ---------------------------------------------------------------------------
  real eta_31;          // baseline play probability for pick 31
  real d_pi;            // average delta in pi_p[p] from one pick to the next
  real<lower=0> tau_pi; // pick-to-pick curve flexibility
  vector[29] z_pi;      // pick-by-pick curve adjustments


  // ---------------------------------------------------------------------------
  // Typical player outcome curve given player logs NBA minutes: m[p]
  // ---------------------------------------------------------------------------
  //
  // m[p] controls the typical 4-year Win Shares outcome for a player who plays. 
  //    It is stored on the log scale because player outcomes are right-skewed. 
  //    Most second-rounders who play produce modest value, while a few become stars.
  real m_31;           // median player outcome for pick 31
  real d_m;            // average change in outcome as picks move from 31 to 60
  real<lower=0> tau_m; // pick-to-pick curve flexibility
  vector[29] z_m;      // pick-by-pick curve adjustments


  // ---------------------------------------------------------------------------
  // Typical player dispersion curve given player logs NBA minutes: s[p]
  // ---------------------------------------------------------------------------
  //
  // s[p] controls how wide the range of typical player outcomes is. 
  //    Larger values mean more uncertainty around what a non-star NBA contributor 
  //    from that pick might becomw
  real s_log_31;       // baseline outcome spread for pick 31 (log scale)
  real<lower=0> tau_s; // pick-to-pick curve flexibility
  vector[29] z_s;      // pick-by-pick cureve adjustments


  // ---------------------------------------------------------------------------
  // upside mixture component
  // ---------------------------------------------------------------------------
  //
  // Most second-rounders who play are modeled as typical contributors. 
  //    A small share are allowed to come from a separate "rare upside" bucket, 
  //    which captures players like unexpected rotation hits, high-end starters, 
  //    or stars without letting every pick get too much credit for that tail.
  //
  // u[p]:
  //   probability that a played player at pick p comes from the upside
  //   component rather than the typical component.
  //
  real u_eta_31;       // upside probability at pick 31
  real<upper=0> d_u;   // Avg change in upside probability, constrained <= 0 -> decreases with later picks
  real<lower=0> delta; // Upside impact
  real<lower=1, upper=2.25> kappa;  // extra uncertainty for upside outcomes
}



transformed parameters {
  vector[30] eta;                    // Play prbabilities (log scale)
  vector<lower=0, upper=1>[30] pi_p; // Play prbabilities (0-100%)
  vector[30] m;                      // Typical 4-year WS outcomes (log scale)
  vector[30] s_log;                  // Typical 4-year WS outcome spread (log scale)
  vector<lower=0>[30] s;             // Positive outcome spread used in the lognormal likelihood
  vector[30] u_eta;                  // upside probability (log scale)
  vector<lower=0, upper=1>[30] u;    // upside probability

  real<lower=0, upper=1> u_31;       // upside probability at pick 31
  real<lower=0, upper=1> u_45;       // upside probability at pick 45
  real<lower=0, upper=1> u_60;       // upside probability at pick 60

  eta[1]   = eta_31;                 // Play probability curve at pick 31
  m[1]     = m_31;                   // Typical-outcome curve at pick 31
  s_log[1] = s_log_31;               // Outcome-spread curve at pick 31


  // Build curves for picks 32-60, updating from previous pick
  for (j in 2:30) {
    eta[j] = eta[j - 1] + d_pi + tau_pi * z_pi[j - 1];  // Play probability
    m[j] = m[j - 1] + d_m + tau_m * z_m[j - 1];         // Typical player outcome
    s_log[j] = s_log[j - 1] + tau_s * z_s[j - 1];       // Player outcome spread
  }

  pi_p = inv_logit(eta);    // Convert from logit scale to probability scale
  s    = exp(s_log);        // Convert outcome spread from log scale to positive scale


  // Upside probabilities for picks 31-60
  for (j in 1:30) {
    u_eta[j] = u_eta_31 + d_u * (j - 1);  // Upside probability declines with later picks
  }
  u = inv_logit(u_eta);      // Convert from logit scale to probability scale

  u_31 = u[1];               // Save pick 31 upside probability for diagnostics
  u_45 = u[15];              // Save pick 45 upside probability for diagnostics
  u_60 = u[30];              // Save pick 60 upside probability for diagnostics
}



model {
  // Priors
  // Play probability
  eta_31  ~ normal(logit(0.88), 0.60); // Pick 31 very likely to appear in NBA
  d_pi    ~ normal(-0.05, 0.05);       // Expected to decline gradually from pick 31-60
  tau_pi  ~ normal(0, 0.10);           // How much the curve can bend by exact pick
  z_pi    ~ std_normal();              // Pick-by-pick adjustments

  // Expected Outcome
  m_31   ~ normal(log(13.0), 0.35);    // Pick 31 centered around ~5 WS after adding ws_floor
  d_m    ~ normal(-0.015, 0.035);      // Expected to decline gradually from pick 31-60
  tau_m  ~ normal(0, 0.05);            // How much the curve can bend by exact pick
  z_m    ~ std_normal();               // Pick-by-pick adjustments

  // Outcome Spread
  s_log_31 ~ normal(log(0.45), 0.25);  // Pick 31 typical player outcome spread (log scale)
  tau_s    ~ normal(0, 0.05);          // How much the curve can bend by exact pick
  z_s      ~ std_normal();             // Pick-by-pick adjustments

  // Upside Probability
  u_eta_31 ~ normal(logit(0.10), 0.35);  // Pick 31 centered around 10% among players who play
  d_u      ~ normal(-0.04, 0.015);       // Expected to decline gradually from pick 31-60
  delta    ~ normal(log(3.0), 0.35);     // upside outcomes meaningfully better than typical outcomes
  kappa    ~ lognormal(log(1.25), 0.15); // upside outcomes wider and more boom/bust than typical outcomes


  // Likelihood
  // Loop over historical second-round picks
  for (n in 1:N) {                      
    int j = pick[n] - 30;               // Convert NBA pick number 31-60 into index 1-30

    played[n] ~ bernoulli(pi_p[j]);     // Model whether this pick ever played in the NBA.

    if (played[n] == 1) {               // Only players who reached the NBA enter outcome model
      real y_shift = ws4[n] - ws_floor; // Shift WS4 so lognormal model is positive

      if (y_shift <= 0) {               // player shifted WS4 must be positive
        reject("Played-player ws4 must be greater than ws_floor. ws4=", ws4[n],
               ", ws_floor=", ws_floor);
      }
      
      // Played players are a mix of typical contributors and upside hits
      target += log_mix(
        u[j],                          // Probability this player is from the upside bucket
        lognormal_lpdf(y_shift | m[j] + delta, s[j] * kappa), // upside player outcome
        lognormal_lpdf(y_shift | m[j],         s[j])          // Typical player outcome
      );
    }
  }
}



generated quantities {
  vector[30] mean_play;   // Average 4-year WS if player actually plays
  vector[30] sd_typ;      // Outcome spread for typical player bucket
  vector[30] sd_play;     // Outcome spread for all played players, including upside hits
  vector[30] ev;          // Expected asset value: play chance times value if the player plays
  vector[30] ev_sd;       // Total pick uncertainty, including zero outcomes and player variance
  vector[30] y_pick_rep;  // One simulated full player outcome for each pick 31-60
  vector[30] u_prob;      // upside probability by exact pick
  array[N] int play_rep;  // Simulated played / did-not-play result for each historical row
  vector[N] y_rep;        // Simulated 4-year WS for each historical row
  vector[N] log_lik;      // Per-player log likelihood for PSIS-LOO and validation


  // Loop over second-round picks
  for (j in 1:30) {
    real mu_typ  = m[j];          // Typical player log outcome level
    real sig_typ = s[j];          // Typical player outcome spread
    real mu_up   = m[j] + delta;  // upside log outcome level
    real sig_up  = s[j] * kappa;  // upside outcome spread

    real typ_m1 = exp(mu_typ + 0.5 * square(sig_typ));   // Average shifted value for the typical bucket
    real typ_m2 = exp(2 * mu_typ + 2 * square(sig_typ)); // Second moment for the typical bucket
    real up_m1 = exp(mu_up + 0.5 * square(sig_up));      // Average shifted value for the upside bucket
    real up_m2 = exp(2 * mu_up + 2 * square(sig_up));    // Second moment for the upside bucket

    real x_m1 = (1 - u[j]) * typ_m1 + u[j] * up_m1;      // Average shifted value after mixing typical and upside outcomes.
    real x_m2 = (1 - u[j]) * typ_m2 + u[j] * up_m2;      // Second moment after mixing typical and upside outcomes.

    real y2_play;                       // Second moment of WS4 among players who play
    real var_play;                      // Variance of WS4 among players who play

    u_prob[j] = u[j];                   // Store upside probability for this pick
    mean_play[j] = ws_floor + x_m1;     // Average 4-year WS conditional on the player playing

    y2_play = square(ws_floor) + 2 * ws_floor * x_m1 + x_m2; // Second moment after adding ws_floor back in
    var_play = fmax(0, y2_play - square(mean_play[j]));      // Played-player variance

    sd_typ[j]  = sqrt(fmax(0, typ_m2 - square(typ_m1)));     // Typical-bucket outcome spread
    sd_play[j] = sqrt(var_play);                             // Full player outcome spread

    ev[j] = pi_p[j] * mean_play[j];                          // Expected asset value for this pick
    ev_sd[j] = sqrt(fmax(0, pi_p[j] * y2_play - square(ev[j]))); // Total pick uncertainty including non-play outcomes

    if (bernoulli_rng(pi_p[j]) == 1) {     // Simulate whether a player picked here reaches the NBA
      if (bernoulli_rng(u[j]) == 1) {      // Simulate whether that NBA player is a upside hit
        y_pick_rep[j] = ws_floor + lognormal_rng(mu_up, sig_up); // Simulated upside outcome
      } else {
        y_pick_rep[j] = ws_floor + lognormal_rng(mu_typ, sig_typ); // Simulated typical player outcome
      }
    } else {
      y_pick_rep[j] = 0;                   // Simulated non-play outcome
    }
  }


  // Loop over historical second-round picks
  for (n in 1:N) {                         
    int j = pick[n] - 30;                  // Convert NBA pick number 31-60 to 1-30

    play_rep[n] = bernoulli_rng(pi_p[j]);  // Simulate if this historical pick would play

    if (play_rep[n] == 1) {                // Simulate WS4 only if the replicated player reaches the NBA
      if (bernoulli_rng(u[j]) == 1) {      // Simulate whether the replicated player is a upside hit
        y_rep[n] = ws_floor + lognormal_rng(m[j] + delta, s[j] * kappa); // Simulated upside historical row
      } else {
        y_rep[n] = ws_floor + lognormal_rng(m[j], s[j]);                 // Simulated typical historical row
      }
    } else {
      y_rep[n] = 0;                        // Simulated historical row for a player who never appears
    }

    log_lik[n] = bernoulli_lpmf(played[n] | pi_p[j]); // Log probability of the actual play / no-play result

    if (played[n] == 1) {                  // Add player WS4 likelihood only for players who reached the NBA
      real y_shift = ws4[n] - ws_floor;    // Shift actual WS4 onto the positive lognormal scale

      if (y_shift <= 0) {                  // Guardrail for invalid shifted player outcomes
        log_lik[n] = negative_infinity();
      } else {
        log_lik[n] += log_mix(             // Log probability of the actual player outcome
          u[j],                            // Probability of upside bucket
          lognormal_lpdf(y_shift | m[j] + delta, s[j] * kappa), // upside likelihood
          lognormal_lpdf(y_shift | m[j],         s[j])          // Typical-outcome likelihood
        );
      }
    }
  }
}

