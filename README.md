# Probabilistic NBA Draft Pick Valuations Under the New 3-2-1 Lottery

[![Dashboard](https://img.shields.io/badge/Interactive%20Dashboard-Shiny-blue)](https://alexsusi2298.shinyapps.io/nbaDraftLottery/)
[![R](https://img.shields.io/badge/R-Shiny%20%7C%20Stan-276DC3)](https://www.r-project.org/)
[![Stan](https://img.shields.io/badge/Stan-Bayesian%20Modeling-B2011D)](https://mc-stan.org/)
[![License](https://img.shields.io/badge/License-ADD%20LICENSE-lightgrey)](LICENSE)


## 01 - Overview

This project estimates how the **[NBA's new 3-2-1 Draft Lottery](https://www.nba.com/news/nba-board-governors-approve-new-draft-lottery-system)** will impact the value, risk, and trade utility of every tradeable draft pick from 2026–2032. Rather than assigning each pick a single value, the model simulates future team strength, lottery outcomes, pick protections, swaps, conveyance rules, and player-outcome uncertainty. The result is a distribution of possible values for every pick, team portfolio, and hypothetical trade.

The **[Interactive Dashboard](https://alexsusi2298.shinyapps.io/nbaDraftLottery/)** includes team-level portfolio views, individual pick distributions, lottery odds, pick movers, and a trade machine to evaluate hypothetical pick deals.

> This is an independent research / portfolio project and is not an official NBA, team, or league valuation model. Future pick obligations, protections, swaps, and conveyance rules are encoded based on public sources and were last updated as of **2026-06-22**.

<br>

## 02 - Dashboard Preview

| Tab            | Use case                                                                     |
| -------------- | ---------------------------------------------------------------------------- |
| Pick Landscape | A high-level view of each team's pick quantity, pick quality, and total portfolio value |
| Team Summaries | Impact of the 3-2-1 Lottery on each team's pick portfolio, including a summary of the most affected picks |
| Single Pick    | Distribution of outcomes for one pick                    |
| Trade Machine  | Evaluate a real or hypothetical transaction          |
| Methodology    | Model validation, transition matrix, and Pick Value Curve                   |


<details>
<summary><h3>Dashboard Screenshots</h3></summary>

Total EPV Leaderboard

![Total EPV leaderboard with dumbbell plots](https://github.com/alex-susi/nbaDraftLottery/blob/master/01_data/epv_leaderboard.png)

Pick Landscape

![Pick landscape scatterplot](https://github.com/alex-susi/nbaDraftLottery/blob/master/01_data/pick_landscape.png)

Team Portfolio View

![Team portfolio summary](https://github.com/alex-susi/nbaDraftLottery/blob/master/01_data/nets.png)

Single Pick Distribution

![Single pick distribution](https://github.com/alex-susi/nbaDraftLottery/blob/master/01_data/single_pick.png)

Trade Machine

![Trade machine pick valuation](https://github.com/alex-susi/nbaDraftLottery/blob/master/01_data/trademachine1.png)
![Trade machine pick valuation](https://github.com/alex-susi/nbaDraftLottery/blob/master/01_data/trademachine2.png)

</details>

<br>

## 03 - Key Findings

1. There's a lot of uncertainty in projecting future expected pick value. Between the difficulty of predicting team performances and the increased randomness of the draft lottery, many indiviudal picks and total team portfolios show small changes in expected value.

2. In the long run (7 years in this case, since that's the furthest out draft picks can be traded), team performance expectations converge towards league-average (15th). As such, there is also a convergence of expected value for distant draft picks. For first-round picks, the EPV converges to around 9.2, and around 2.5 for second-round picks.

3. The largest decreases in single-pick EPV are Memphis' 2027 first round most-favorable selection of Utah, Cleveland, and Minnesota (-1.8 EPV due to Utah's pick being ineligible to land in the top 5) and Washington's own 2027 first round pick (-1.6 EPV as it cannot land #1 overall in consecutive years).

4. The largest increases in single-pick EPV are Miami's top-14 protected 2027 first round pick (+1.1 EPV) and Brooklyn's less favorable 2027 first round pick between their own and Houston's (+0.7 EPV). 


<br>

## 04 - Methodology

1. **Draft Pick Value Curves**

   * First-round picks are modeled with a Bayesian Student-t regression on draft slot.
   * Second-round picks are modeled separately using a hurdle model because many second-rounders never appear in an NBA game.

2. **Projecting Future Team Performance**

   * Markov chain model used to simulate future standings, where a team's standing in Year `t+1` is dependent on the team's standing in year `t`.

3. **Lottery Simulation**

   * For 2027-2032, a Monte Carlo simulation projects future team standings, and runs both the current lottery and the new 3-2-1 system.
   * It applies the appropriate number of lottery balls based on tier, the 12th-pick floor, no consecutive No. 1 picks, no three straight top-5 picks, and the ban on newly traded top-12 through top-15 protections.
   * The simulation applies future pick obligations, protections, swaps, conveyance rules, return legs, and conditional structures.
   * The trade machine evaluates hypothetical pick packages using correlated simulation draws, so team trajectories and pick outcomes remain internally consistent.


<br>

### Draft Pick Value Curve Outcome Variable

The core player outcome is first-four-year [Win Shares](https://www.basketball-reference.com/about/ws.html), a player statistic which attempts to divvy up credit for team success to the individuals on the team. First four years were chosen to cover the length of the cost-controlled rookie-scale contracts. Win Share data scraped from Basketball-Reference draft classes from 1985–2021.

$$\text{4-YR WS}_n = \text{Win Shares accumulated by player } n \text{ over his first four NBA seasons}$$

The Draft Pick Value Curve distinguishes between:

* **Expected Pick Value (EPV):** the posterior mean value of a draft pick before knowing the drafted player.
* **4-Yr WS Outcomes:** simulated player-level outcomes around the pick curve.

While higher draft picks have greater EPV, that does not guarantee that players picked higher will perform better than players drafted after them. For instance, the 1st pick in a draft is always a more valuable asset than the 3rd pick in the same draft, but that does not guarantee that the player selected with the 1st pick will be better than the player selected 3rd.

<br>

### First Round Pick Model

A regression of 4-YR WS on draft slot. The mean follows a power-law decay in pick number, and per-slot residual variance is smoothed across adjacent picks by a Gaussian random walk on the log scale. The Student-t likelihood accommodates the heavy tails of draft outcomes (superstars and busts) without the mean curve being distorted by outliers.

| Parameter  | Description                                                                                                                                              |
| ---------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| $\mu_p$    | Expected first-four-year Win Shares at pick $p$                                                                                                          |
| $\alpha$   | Curve height of the top of the lottery                                                                                                                   |
| $\beta$    | Decay exponent controlling how steeply value falls with pick number                                                                                      |
| $\gamma$   | Baseline value the curve approaches in the late first round                                                                                              |
| $\sigma_p$ | Slot-specific residual scale, anchored at $\sigma_1$ and propagated by the random walk                                                                   |
| $\tau$     | Random-walk innovation scale; small $\tau$ forces $\sigma_p$ to gradually evolve across neighboring slots, large $\tau$ permits abrupt volatility shifts |
| $z_p$      | Pick-specific movement in outcome risk relative to adjacent picks                                                                                        |
| $\nu$      | Student-t degrees of freedom; lower $\nu$ yields heavier tails, with $\nu>2$ keeping variance finite                                                     |


<details>
<summary><h3><code>picks_Round1.stan</code></h3></summary>

<br>

```stan
// -----------------------------------------------------------------------------
// 1st Round Pick NBA Draft Valuation Model
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Player outcome
//        Each drafted player has one row. The outcome is first-4-year Win Shares.
//
//        ws4[n] ~ Student_t(nu, mu[pick[n]], sigma_pick[pick[n]])
//
//   2. Pick value curve
//        Expected value declines smoothly from the top of the draft to the end
//        of the first round.
//
//        mu[p] = alpha / p^beta + gamma
//
//      where:
//        alpha controls the overall height of the curve.
//        beta controls how quickly value falls from early to late first.
//        gamma is the lower baseline the curve approaches later in the round.
//
//   3. Pick-specific outcome spread
//        Each pick has its own player outcome spread, but nearby picks are tied
//        together so the model does not overreact to one unusually good or bad
//        historical pick slot.
//
//        log_sigma_pick[1] = log_sigma_1
//        log_sigma_pick[p] = log_sigma_pick[p - 1]
//                            + tau_log_sigma_rw * z_sigma_step[p - 1]
//
//   4. Heavy-tailed player outcomes
//        The Student-t likelihood allows for extreme draft outcomes: 
//        stars, busts, and other unusual player paths. 
//        nu controls how much tail risk the model allows.
//
// -----------------------------------------------------------------------------


data {
  int<lower=1> N;                         // Number of players
  array[N] int<lower=1, upper=30> pick;   // Pick each player was drafted at (1-30)
  vector[N] ws4;                          // First-4-year Win Shares
}



parameters {
  // ---------------------------------------------------------------------------
  // Pick value curve: mu[p] = alpha / p^beta + gamma
  // ---------------------------------------------------------------------------
  real log_alpha; // Height of the first-round value curve (log scale)
  real log_beta;  // Decline rate from early to late first round (log scale)
  real gamma;     // Late-first-round baseline value the curve approaches.


  // ---------------------------------------------------------------------------
  // Player outcome spread by pick
  // ---------------------------------------------------------------------------
  real log_sigma_1;                 // Outcome spread at pick 1 (log scale)
  vector[29] z_sigma_step;          // Pick-by-pick adjustments to the spread curve
  real<lower=0> tau_log_sigma_rw;   // How much the spread curve can vary by pick


  // ---------------------------------------------------------------------------
  // Star / bust tail risk
  // ---------------------------------------------------------------------------
  real<lower=2> nu; // Student-t DoF; lower values allow more extreme draft outcomes
}



transformed parameters {
  real<lower=0> alpha = exp(log_alpha); // Height of value curve
  real<lower=0> beta  = exp(log_beta);  // Decline rate

  vector[30] log_sigma_pick;            // Outcome spread by pick, log scale
  vector<lower=0>[30] sigma_pick;       // Positive outcome spread by pick

  log_sigma_pick[1] = log_sigma_1;      // Start spread curve at pick 1


  // Build spread curve for picks 2-30, nearby picks similar
  for (p in 2:30) {
    // Update outcome spread from previous pick.
    log_sigma_pick[p] = log_sigma_pick[p - 1] +
                        tau_log_sigma_rw * z_sigma_step[p - 1];
  }

  sigma_pick = exp(log_sigma_pick);     // Outcome spread from log scale to positive scale
}



model {
  // Priors
  log_alpha ~ normal(log(20), 0.60);    // High expected value for #1 overall pick
  log_beta  ~ normal(log(0.55), 0.50);  // Smooth decline in value from pick 1-30
  gamma     ~ normal(2, 3);             // Late-first round pick expected value

  log_sigma_1       ~ normal(log(8), 0.50); // #1 pick outcome spread centered around 8 WS
  z_sigma_step      ~ std_normal();         // Pick-by-pick outcome-spread adjustments
  tau_log_sigma_rw  ~ normal(0, 0.15);      // Pick-by-pick outcome-spread curve bend

  nu - 2 ~ exponential(0.20); // Finite variance while allowing stars and busts
  
  
  // Likelihood
  // Loop over historical first-round picks
  for (n in 1:N) {
    // Expected 4-year WS for player's draft slot
    real mu = alpha / pow(pick[n], beta) + gamma;

    ws4[n] ~ student_t(nu, mu, sigma_pick[pick[n]]);
  }
}



generated quantities {
  vector[30] ws4_pred;     // Expected 4-year WS by each first-round pick
  vector[30] ws4_pred_sd;  // Player outcome spread by each first-round pick
  vector[N] log_lik;       // Per-player log likelihood for PSIS-LOO and validation
  vector[N] ws4_rep;       // Simulated 4-year WS for each historical player row


  // Loop over first-round picks
  for (p in 1:30) {
    real mu = alpha / pow(p, beta) + gamma; // Expected 4-year WS for this pick

    ws4_pred[p]    = mu;             // Expected value for this pick
    ws4_pred_sd[p] = sigma_pick[p];  // Player outcome spread for this pick
  }
  
  
  // Loop over historical first-round picks
  for (n in 1:N) {
    // Expected 4-year WS for player's draft slot
    real mu = alpha / pow(pick[n], beta) + gamma;

    log_lik[n] = student_t_lpdf(ws4[n] | nu, mu, sigma_pick[pick[n]]);

    ws4_rep[n] = student_t_rng(nu, mu, sigma_pick[pick[n]]);
  }
}

```


</details>

<br>

### Second Round Pick Model

Unlike first-round picks, second-round picks frequently sign non-guaranteed contracts, spend significant time in the G League, or return to play overseas before ever appearing in an NBA game.

The second round model is therefore a two-part **hurdle**:

1. A Bernoulli model for whether the player drafted in a given slot logs any NBA minutes.
2. A shifted right-skewed lognormal mixture for player outcomes conditional on playing.

A pick-declining mixture weight allows for the rare second-round star without inflating the typical-pick curve. The play probability $\pi_p$, typical median $m_p$, and dispersion $s_p$ each follow their own adjacent-pick random walk with drift, borrowing strength across neighboring slots in a similar manner to the first-round pick model.


| Parameter                       | Description                                                                                                                             |
| ------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| $\pi_p$                         | Probability a player at pick $p$ appears in at least one NBA game                                                                       |
| $\text{floor}$                  | Fixed negative shift placing the lognormal support below the worst observed played-player outcome, so $\text{ws4}_n - \text{floor} > 0$ |
| $m_p$                           | Log-median of the typical played-player outcome at pick $p$                                                                             |
| $s_p$                           | Lognormal dispersion of the typical component                                                                                           |
| $u_p$                           | Rare-upside mixture weight, regularized to decline across the round                                                                     |
| $\delta$                        | Additive log-location shift of the upside component                                                                                     |
| $\kappa$                        | Multiplicative inflation of the upside component's dispersion                                                                           |
| $d_\pi$                         | Drift in play probability across picks                                                                                                  |
| $d_m$                           | Drift in typical-outcome median across picks                                                                                            |
| $\tau_\pi,\ \tau_m,\ \tau_s$    | Random-walk innovation scales for the play, median, and dispersion curves                                                               |
| $z^{\pi}_p,\ z^{m}_p,\ z^{s}_p$ | Pick-specific departures of each curve from its neighbor                                                                                |

<details>
<summary><h3><code>picks_Round2.stan</code></h3></summary>

<br>

```stan
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

```

</details>

<br>

### Draft Pick Value Curve

![Draft Pick Value Curve](https://github.com/alex-susi/nbaDraftLottery/blob/master/01_data/pick_curve.png)

<br>

### Team-Strength Model

A Bayesian Markov chain over all 30 league-wide standing positions. The five 3-2-1 lottery tiers are then derived from the simulated rank:

| Rank range | 3-2-1 tier |
|---:|---|
| 1–3 | Relegation |
| 4–10 | Non-play-in |
| 11–14 | Play-in 9/10 seed |
| 15–16 | Play-in 7/8 loser |
| 17–30 | Playoff |

Note that this is an imperfect mapping to the lottery tiers, as it does not account for within-conference seeding, nor does it simulate play-in game outcomes. 

The model estimates a $30 \times 30$ transition matrix:

$$
\theta_{i\cdot} = P(\text{rank}_{t+1} = j \mid \text{rank}_t = i)
$$

where each row gives the probability distribution over next-season ranks for a team currently in rank state $i$.

Rather than using a Dirichlet prior for each row independently, `team_strength_v3.stan` uses a **smoothed softmax transition surface**. The transition logits combine:

1. A global destination-rank baseline, capturing which future ranks are generally more or less common.
2. A distance penalty, favoring movement to nearby ranks over large jumps.
3. A local deviation surface, smoothed across adjacent current ranks and adjacent future ranks.


| Parameter / Quantity | Description |
|---|---|
| $K$ | Number of rank states ($K = 30$). |
| $\text{counts}_{ij}$ | Observed historical count of season-over-season transitions from rank $i$ to rank $j$. |
| $\theta_{i\cdot}$ | The $i$-th row of the $30 \times 30$ transition matrix; a simplex giving next-season rank probabilities. |
| $\eta_{ij}$ | Transition logit for moving from current rank $i$ to future rank $j$, centered within each row for softmax identification. |
| $b_j$ | Centered global destination-rank baseline. This captures ranks that are generally more or less common as future destinations. |
| $\epsilon_{ij}$ | Local transition-logit deviation for the specific current-rank / future-rank pair. |
| $\lambda$ | Distance slope. This is constrained to be non-positive, so larger absolute rank jumps are penalized unless the data strongly support them. |
| `eta_scale` | Fixed scale controlling the marginal size of local transition-logit deviations. Supplied by the R pipeline. |
| `row_smooth_scale` | Fixed smoothing scale across adjacent current-rank rows. Supplied by the R pipeline. |
| `col_smooth_scale` | Fixed smoothing scale across adjacent future-rank columns. Supplied by the R pipeline. |
| `dest_scale` | Fixed scale controlling the size of the global destination-rank baseline. Supplied by the R pipeline. |
| `counts_rep` | Posterior predictive replicated transition counts used for model checking. |
| `row_entropy` | Entropy of each transition row, used to summarize how concentrated or diffuse each current-rank transition distribution is. |


<details>
<summary><h3><code>team_strength_v3.stan</code></h3></summary>

<br>

```stan
// team_strength_v3.stan
// Bayesian Markov chain over all 30 league-wide standings / draft-order ranks.
//
// Rank convention used by the R pipeline:
//   rank_worst = 1  -> worst regular-season record / old lottery seed 1
//   rank_worst = 30 -> best regular-season record / pick 30 under inverse record
//
// This version keeps the 30x30 smoothed-softmax transition surface from v2, but
// removes learned smoothing scales. In v2, sigma_eta / sigma_row / sigma_col
// collapsed toward zero and created poor geometry. Here those scales are fixed
// data inputs chosen in R, so the model remains Bayesian but avoids the funnel /
// boundary behavior from estimating weakly identified smoothness hyperparameters.
//
// The transition logits use:
//   1. a global destination-rank baseline;
//   2. a distance penalty favoring nearby future ranks;
//   3. a small local deviation surface smoothed across rows and columns.


data {
  int<lower=2> K;                       // number of rank states; production K = 30
  array[K, K] int<lower=0> counts;      // observed transition counts: rank t -> rank t+1

  // Fixed smoothing constants supplied by the R pipeline. These are deliberately
  // data, not parameters, to avoid the v2 near-zero scale collapse.
  real<lower=0> eta_scale;              // marginal size of local logit deviations
  real<lower=0> row_smooth_scale;       // adjacent starting-rank smoothness
  real<lower=0> col_smooth_scale;       // adjacent ending-rank smoothness
  real<lower=0> dest_scale;             // size of global destination-rank baseline
}



parameters {
  matrix[K, K] eta_raw;                 // local transition-logit deviations
  vector[K] dest_raw;                   // global destination-rank baseline
  real<upper=0> distance_slope;         // penalty per absolute rank jump
}



transformed parameters {
  vector[K] dest_baseline;              // centered destination effect
  matrix[K, K] eta;                     // row-centered logits
  array[K] simplex[K] theta;            // transition probabilities by current rank

  dest_baseline = dest_scale * (dest_raw - mean(dest_raw));

  for (i in 1:K) {
    vector[K] row_eta;
    for (j in 1:K) {
      row_eta[j] = dest_baseline[j] + eta_raw[i, j] + distance_slope * abs(i - j);
    }
    row_eta = row_eta - mean(row_eta);  // softmax location identification
    for (j in 1:K) eta[i, j] = row_eta[j];
    theta[i] = softmax(row_eta);
  }
}



model {
  // Distance is the main interpretable persistence term. The prior allows broad
  // transition rows, but keeps the sign negative unless the data strongly object.
  distance_slope ~ normal(-0.18, 0.08);

  // Global destination-rank baseline and local deviations.
  dest_raw ~ normal(0, 1);
  to_vector(eta_raw) ~ normal(0, eta_scale);

  // Fixed-scale smoothing across adjacent starting ranks.
  for (i in 2:K) {
    eta_raw[i, ] - eta_raw[i - 1, ] ~ normal(0, row_smooth_scale);
  }

  // Fixed-scale smoothing across adjacent ending ranks.
  for (j in 2:K) {
    eta_raw[, j] - eta_raw[, j - 1] ~ normal(0, col_smooth_scale);
  }

  for (i in 1:K) {
    counts[i] ~ multinomial(theta[i]);
  }
}



generated quantities {
  vector[K] row_log_lik;
  vector[K] row_entropy;
  array[K, K] int counts_rep;

  for (i in 1:K) {
    if (sum(counts[i]) > 0)
      row_log_lik[i] = multinomial_lpmf(counts[i] | theta[i]);
    else
      row_log_lik[i] = 0;

    counts_rep[i] = multinomial_rng(theta[i], sum(counts[i]));

    row_entropy[i] = 0;
    for (j in 1:K)
      row_entropy[i] += -theta[i][j] * log(theta[i][j] + 1e-12);
  }
}

```

</details>

<br>

### Monte Carlo Simulator

To project future pick values, the simulation:

1. Seeds each team in its actual 2025–26 lottery tier.
2. Evolves team tiers year by year using posterior draws from the Markov transition model.
3. Orders teams within tiers to construct future lottery seeds.
4. Runs both the current lottery and the new 3-2-1 lottery.
5. Applies 3-2-1 restrictions:

   * Relegation-tier ball counts
   * 12th-pick floor for relegated teams
   * No consecutive No. 1 overall picks
   * No three straight top-5 picks
   
6. Applies public pick obligations:

   * Outright traded picks
   * Protections
   * Conveyance conditions
   * Swap rights
   * Return legs
   
7. Values every owned pick under each simulated outcome.
8. Aggregates pick-level draws into team portfolios, single-pick summaries, pick-mover tables, and trade-machine outputs.

<br>

## 05 - Model Validation Summary

The production models were evaluated with sampler diagnostics, posterior predictive checks, PSIS-LOO, Markov transition-code checks, lottery simulator validation, and simulation-based calibration where computationally feasible.

<details>
<summary><strong>Model validation and diagnostics table</strong></summary>

<br>

| Model                | Check                            |                                                                                                                                                                              Metric | Use Case                                                                                                                                        | Status |
| -------------------- | -------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: | ----------------------------------------------------------------------------------------------------------------------------------------------- | ------ |
| Round 1 pick value   | R-hat / ESS                      |                                                                                                                                                 max R-hat 1.002; min bulk-ESS 3,566 | Confirms the posterior draws mixed reliably, so the reported means and intervals are not chain artifacts.                                       | PASS   |
| Round 1 pick value   | Posterior predictive coverage    |                                                                                                                                                        90% player rows coverage 91% | Compares simulated draft outcomes to historical outcomes; good coverage means the model's uncertainty is realistic, not just the average curve. | PASS   |
| Round 1 pick value   | PSIS-LOO / Pareto-k              |                                                                                                                                      elpd_loo -3889.5; p_loo 5.0; max Pareto-k 0.25 | Tests out-of-sample reliability and whether a few extreme players dominate the fit; acceptable Pareto-k makes the comparison credible.          | PASS   |
| Round 2 hurdle       | R-hat / ESS                      |                                                                                                                                                 max R-hat 1.002; min bulk-ESS 2,656 | Confirms the posterior draws mixed reliably, so the reported means and intervals are not chain artifacts.                                       | PASS   |
| Round 2 hurdle       | Posterior predictive / play rate |                                                                                                        90% PPC 93%; empirical play 73.3%; P(play) #31/#45/#60 91.6% / 74.9% / 45.1% | Compares simulated draft outcomes to historical outcomes; good coverage means the model's uncertainty is realistic, not just the average curve. | PASS   |
| Round 2 hurdle       | PSIS-LOO / Pareto-k              |                                                                                                                                     elpd_loo -2495.9; p_loo 20.9; max Pareto-k 0.57 | Tests out-of-sample reliability and whether a few extreme players dominate the fit; acceptable Pareto-k makes the comparison credible.          | PASS   |
| Team-strength Markov | Transition-matrix code check     |                                                                                       630 transitions over 22 seasons; Stan vs closed-form max diff 0.0011; mixing time 1.5 seasons | Verifies the tier-transition engine against the conjugate closed-form check, which supports the future team-path simulations.                   | PASS   |
| Validation script    | NUTS geometry                    | Displayed from `03_validation/validation_decision_table_latest_models.csv` when `05_model_validation.R` has been run; target is 0 divergences, 0 max-treedepth hits, E-BFMI > 0.30. | Checks whether Stan explored the posterior without pathological geometry; divergences or treedepth hits would undermine trust in the draws.     | INFO   |
| Validation script    | SBC rank uniformity              |                   Displayed from `03_validation/validation_decision_table_latest_models.csv` when SBC is run; approximately uniform ranks indicate calibrated Bayesian uncertainty. | Uses simulated data where the truth is known to verify that Bayesian credible intervals are calibrated.                                         | INFO   |

</details>

<br>

Additional validation checks include:

* **SBC** (simulation-based calibration) for rank uniformity.
* **PSIS-LOO** with Pareto-k diagnostics, comparing the random-walk variance model against constant- and linear-sigma baselines.
* **Posterior predictive checks** on both player outcomes and transition-count statistics.
* **NUTS diagnostics** and a closed-form Dirichlet-Multinomial cross-check.
* **Lottery simulator validation** against the official published 3-2-1 odds table.

<br>

## 06 - Example Trade Case Study

### Memphis Trades Down

During the first round of the 2026 NBA Draft, [Memphis traded down twice](https://www.nba.com/news/2026-offseason-trade-tracker):

1. **Memphis → Oklahoma City:** Memphis moved from **No. 16** to **No. 17** and received **two future second-round picks** from OKC.
2. **Memphis → Detroit:** Memphis then moved from **No. 17** to **No. 21** and received **three additional future second-round picks** from Detroit.

For purposes of this case study, we will analyze the initial Memphis/OKC deal.

The Trade Machine can be used to analyze 3 different questions for this transaction:

> How much value does a team give up by moving down a few slots in the first round?

> Which team is more likely to receive more total production from players drafted with the traded picks?

> Which team is more likely to receive the single best player from the traded picks?

### Using the Trade Machine

![Memphis-OKC Trade](https://github.com/alex-susi/nbaDraftLottery/blob/master/01_data/mem_okc1.png)
![Memphis-OKC Trade](https://github.com/alex-susi/nbaDraftLottery/blob/master/01_data/mem_okc2.png)


### Trade Analysis

On paper, this is good business for Memphis. The expected value lost from moving down only 1 draft slot is more than compensated for by picking up 2 future second round picks, noted by the 100% higher EPV to Memphis. However, Memphis is not guaranteed to get more productivity out of the players selected with these picks, but the model favors them with about a 64% chance. The trade machine also likes Memphis' chances to get the single-most productive player with the picks involved in this deal, at about a 59% rate. 

Important caveat - trades like this during the draft are often done when a team is targeting a specific player. Teams may be more willing to trade back if a player they like is still expected to be on the board, or may be willing to "overpay" with future draft picks if they really want a specific player and don't want to risk another team drafting him. Additionally, this does not specifically model player projections on the incoming 2026 rookies, so the above player outcome percentages may be impacted by that too. 

<br>

## 07 - Glossary

| Term                        | Meaning                                                                                                                                                                   |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **EPV**                     | Expected Pick Value. The posterior expected value of a draft pick before knowing the drafted player.                                                            |
| **4-YR WS**                 | First-four-year Win Shares. The player metric used to value draft outcomes during the rookie-scale contract.                                                     |
| **Credible interval**       | Bayesian uncertainty interval. A 90% credible interval means 90% of posterior draws fall inside that range.                                                               |
| **Conveyance**              | Whether a traded pick is actually delivered to the receiving team after applying protections and conditions.                                                              |
| **Protection**              | A restriction on a traded pick, usually allowing the original team to keep the pick if it lands in a specified range. Example: top-4 protected.                           |
| **Swap right**              | The right for one team to exchange its pick with another team's pick, usually taking the more favorable slot.                                                             |
| **Upside component**        | In the round-two model, the mixture component that captures rare second-round star outcomes without inflating the typical expected value of every second-round pick. |
| **Relegation tier**         | The bottom three teams under the 3-2-1 structure. They receive two lottery balls each and cannot pick worse than 12th.                                                    |


<br>

## 08 - Repo Structure

| File                    | Purpose                                                                     |
| ----------------------- | --------------------------------------------------------------------------- |
| `01_data.R`             | Scrape standings, rosters, draft production; build Markov transition counts |
| `02_picks.R`            | Pick ownership, protections, swaps for 2026–2032                            |
| `03_models.R`           | Fit all three Stan models and run LOO comparison                            |
| `04_lotterySims.R`      | Monte Carlo lottery simulation                                              |
| `05_model_validation.R` | SBC / LOO / PPC model validation                                            |
| `picks_Round1.stan`     | First-round pick model                                 |
| `picks_Round2.stan`     | Second-round pick model                                                        |
| `team_strength.stan`    | Markov chain model                                                |
| `app.R`                 | R Shiny dashboard                                                           |

<br>

## 09 - Running the Project

There are two ways to run the project.

### Quickstart: Run the Dashboard from Precomputed Data

Use this path if you only want to launch the Shiny app without re-scraping data or re-fitting Stan models.

```r
install.packages(c("tidyverse", "shiny", "plotly", "DT", "bslib", "igraph"))

shiny::runApp("app.R")
```

This assumes the repo includes a precomputed `dashboard_data.rds` file in the expected location. Note that `dashboard_data.rds` is available for download in the `01_data` folder.

Depending on the local file structure, the app looks for:

```text
01_data/dashboard_data.rds
dashboard_data.rds
```

### Full Rebuild: Scrape Data, Fit Models, Run Simulations

Use this path to fully rebuild the data, refit Stan models, rerun lottery simulations, and regenerate `dashboard_data.rds`.

```r
install.packages(c("tidyverse", "hoopR", "rvest", "httr", "cmdstanr",
                   "janitor", "posterior", "expm", "loo",
                   "shiny", "plotly", "DT", "bslib", "igraph"))

cmdstanr::install_cmdstan()

source("01_data.R")
source("02_picks.R")
source("03_models.R")
source("04_lotterySims.R")   # writes dashboard_data.rds
shiny::runApp("app.R")
```

Run the validation suite separately:

```r
source("05_model_validation.R")
```

Validation outputs are written to:

```text
03_validation/
```

<br>

## 10 - Limitations and Future Work

* **No time discounting yet.** A 2031 pick and a 2027 pick of equal EPV are treated equally.
* **No surplus value yet.** Production is not netted against rookie-scale salary.
* **Team projections are intentionally simple.** The Markov model does not yet account for age, roster continuity, salary cap space, draft capital, injuries, market size, player development, or front-office strategy. Future versions might explore this, as well as higher-order Markov chain models.
* **Historical team behavior may not generalize.** Team projections are constructed using historical seasons that operated under the old lottery format and CBA rules. The new 3-2-1 lottery may incentivize team behavior changes in ways that historical data cannot fully identify.
* **Deeply nested swap chains are approximated.** Publicly reported multi-team pick obligations can be ambiguous or conditional in ways that require close approximations.
* **Win Shares is imperfect.** Because of how the metric is calculated, it might over- or under-value certain players, and is biased towards players who recieve more playing time. Future versions may explore EPM, RAPTOR, xRAPM, DARKO, BPM, or salary-adjusted surplus value.
* **No player-specific projections.** The current model values picks based on historical performances of players drafted in those slots. It does not account for incoming player projections. For example, 2026 draft picks were valued using model outcomes trained on historical data. Projection models specifically for the incoming 2026 rookies were not built for this project. 


<br>

## 11 - References

This project builds on the draft-value-curve lineage from:

* Roland Beech / Barzilai-style draft value curves at 82games
* Kevin Pelton's ESPN WARP draft charts
* Jacob Goldstein's PIPM-based draft value work
* Nick Thoreson and Luke McCartney's 3-2-1-specific re-pricing work
* Foster & Binns, *Valuing Protections on NBA Draft Picks* (MIT Sloan, 2019)

Additional public data sources and references include:

* Basketball-Reference draft history and player advanced statistics
* Public NBA standings history
* RealGM future draft pick summaries
* NBA official draft and trade trackers
* Public reporting on pick protections, swaps, and conveyance terms
