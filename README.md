# Probabilistic NBA Draft Pick Valuations Under the New 3-2-1 Lottery

[![Dashboard](https://img.shields.io/badge/Interactive%20Dashboard-Shiny-blue)](https://alexsusi2298.shinyapps.io/nbaDraftLottery/)
[![R](https://img.shields.io/badge/R-Shiny%20%7C%20Stan-276DC3)](https://www.r-project.org/)
[![Stan](https://img.shields.io/badge/Stan-Bayesian%20Modeling-B2011D)](https://mc-stan.org/)
[![License](https://img.shields.io/badge/License-ADD%20LICENSE-lightgrey)](LICENSE)


## Overview

This project estimates how the **[NBA's new 3-2-1 Draft Lottery](https://www.nba.com/news/nba-board-governors-approve-new-draft-lottery-system)** will impact the value, risk, and trade utility of every future draft pick from 2026–2032. Rather than assigning each pick a single value, the model simulates future team strength, lottery outcomes, pick protections, swaps, conveyance rules, and player-outcome uncertainty. The result is a distribution of possible values for every pick, team portfolio, and hypothetical trade.

The **[Interactive Dashboard](https://alexsusi2298.shinyapps.io/nbaDraftLottery/)** includes team-level portfolio views, individual pick distributions, lottery odds, pick movers, and a trade machine to evaluate hypothetical pick deals.

> This is an independent research / portfolio project and is not an official NBA, team, or league valuation model. Future pick obligations, protections, swaps, and conveyance rules are encoded based on public sources and were last updated as of **2026-06-22**.

<br>

## Dashboard Preview

| Tab            | Use case                                                                     |
| -------------- | ---------------------------------------------------------------------------- |
| Pick Landscape | A high-level view of each team's pick quantity, pick quality, and total portfolio value |
| Team Summaries | Impact of the 3-2-1 Lottery on each team's pick portfolio, including a summary of the most affected picks |
| Single Pick    | Distribution of outcomes for one pick                    |
| Trade Machine  | Evaluate a real or hypothetical transaction          |
| Methodology    | Model validation, transition matrix, and pick-value curves                   |

> Replace the placeholder image paths below with screenshots from the dashboard. Recommended location: `docs/images/`.

### Total EPV Leaderboard / Dumbbell Plot

![Total EPV leaderboard with dumbbell plots](docs/images/total-epv-leaderboard-dumbbell.png)

### Pick Landscape

![Pick landscape scatterplot](docs/images/pick-landscape-scatterplot.png)

### Team Portfolio View

![Team portfolio summary](docs/images/team-portfolio-summary.png)

### Single Pick Distribution

![Single pick distribution](docs/images/single-pick-distribution.png)

### Trade Machine

![Trade machine pick valuation](docs/images/trade-machine.png)

### Methodology / Validation View

![Methodology and validation dashboard](docs/images/methodology-validation.png)

<br>

## Key Findings

1. **[Finding 1]**

2. **[Finding 2]**

3. **[Finding 3]**

4. **[Finding 4]**

5. **[Finding 5]**

<br>

## Example Decision Case Study

### Memphis Trades Down Twice in the 2026 First Round

During the first round of the 2026 NBA Draft, Memphis traded down twice:

1. **Memphis → Oklahoma City:** Memphis moved from **No. 16** to **No. 17** and received **two future second-round picks**.
2. **Memphis → Detroit:** Memphis then moved from **No. 17** to **No. 21** and received **three additional future second-round picks**.

The combined result: Memphis moved from **No. 16 to No. 21** and accumulated **five future second-round picks**.

Sources: [NBA 2026 Offseason Trade Tracker](https://www.nba.com/news/2026-offseason-trade-tracker), [Reuters recap](https://www.reuters.com/sports/grizzlies-trade-back-twice-first-round-draft-karim-lopez--flm-2026-06-24/)

This is a useful case study because it mirrors the exact type of question the dashboard is designed to answer:

> How much value does a team give up by moving down a few slots in the first round, and how many future second-round picks are needed to make the trade fair?

### How the Trade Machine Can Analyze It

The trade can be evaluated three ways:

| Scenario           | Memphis gives up |          Memphis receives | Question                                                                      |
| ------------------ | ---------------: | ------------------------: | ----------------------------------------------------------------------------- |
| OKC trade only     |           No. 16 | No. 17 + 2 future seconds | Did two seconds compensate Memphis for moving down one slot?                  |
| Detroit trade only |           No. 17 | No. 21 + 3 future seconds | Did three seconds compensate Memphis for moving down four slots?              |
| Combined sequence  |           No. 16 | No. 21 + 5 future seconds | Did Memphis gain enough future draft value to justify moving down five slots? |

### Model Output Placeholder

Replace this table with dashboard results once the exact future second-round picks are encoded.

| Trade                       | Memphis EPV Sent | Memphis EPV Received | Net EPV | 90% Credible Interval | Probability Memphis Gains Value |
| --------------------------- | ---------------: | -------------------: | ------: | --------------------: | ------------------------------: |
| No. 16 → No. 17 + 2 seconds |            `[x]` |                `[x]` |   `[x]` |              `[x, x]` |                          `[x%]` |
| No. 17 → No. 21 + 3 seconds |            `[x]` |                `[x]` |   `[x]` |              `[x, x]` |                          `[x%]` |
| No. 16 → No. 21 + 5 seconds |            `[x]` |                `[x]` |   `[x]` |              `[x, x]` |                          `[x%]` |

### Basketball Interpretation Placeholder

`[Add interpretation after running the trade machine. Example: If the combined sequence is close to fair by EPV but has wider uncertainty, Memphis effectively exchanged one more certain first-round outcome for a diversified basket of lower-probability future outcomes. If the model shows Memphis gained value, the deal can be framed as monetizing a relatively flat section of the first-round pick curve. If the model shows Memphis lost value, the future seconds may not fully compensate for the drop from mid-first to late-first expected value.]`

<br>



## Methodology

1. **Draft pick value curves**

   * Round 1 picks are modeled with a Bayesian Student-t regression on draft slot.
   * Round 2 picks are modeled separately using a hurdle model because many second-rounders never appear in an NBA game.

2. **Team Strength simulation**

   * Teams move through five 3-2-1 lottery tiers using a Bayesian Markov transition model.

3. **Lottery simulation**

   * For 2027-2032, a Monte Carlo simulation projects future team tiers, and runs both the current lottery and the new 3-2-1 system.
   * It applies relegation-tier ball counts, the 12th-pick floor, no consecutive No. 1 picks, no three straight top-5 picks, and the ban on newly traded top-12 through top-15 protections.
   * The simulation applies public future pick obligations, protections, swaps, conveyance rules, return legs, and conditional structures.
   * The trade machine evaluates hypothetical pick packages using correlated simulation draws, so team trajectories and pick outcomes remain internally consistent.

**Pick value target:** first-four-year Win Shares, abbreviated **4-YR WS**. This approximates the rookie-scale, cost-controlled window and is scraped from Basketball-Reference draft classes from 1985–2021.

The project uses three Stan models:

1. `picks_Round1.stan`
2. `picks_Round2.stan`
3. `team_strength.stan`

A Monte Carlo simulation then projects future team tiers, runs both lottery systems, applies protections and swaps, and values every owned pick under each system.

<br>

### Pick Value Target

The core player outcome is:

$$\text{4-YR WS}_n = \text{Win Shares accumulated by player } n \text{ over his first four NBA seasons}$$

The model distinguishes between:

* **Expected Pick Value (EPV):** the posterior mean value of a pick slot before knowing the drafted player.
* **Realized player outcomes:** simulated player-level outcomes around the pick curve, including busts, rotation players, and rare star outcomes.

This distinction matters because a pick can have lower expected value but still have meaningful upside in realized player-outcome simulations.

<br>

### First Round Pick Model

A regression of first-four-year Win Shares on draft slot. The mean follows a power-law decay in pick number, and per-slot residual variance is smoothed across adjacent picks by a Gaussian random walk on the log scale. The Student-t likelihood accommodates the heavy tails of draft outcomes (superstars and busts) without the mean curve being distorted by outliers.

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

</details>

<br>

### Round 2 Pick Model — `picks_Round2.stan`

Second-round picks violate the first-round model because the modal career outcome is *zero* NBA value. Unlike first-round picks, second-round picks frequently sign non-guaranteed contracts, spend significant time in the G League, or return to play overseas before ever appearing in an NBA game.

The valuation is therefore a two-part **hurdle**:

1. A Bernoulli model for whether the player logs any NBA minutes.
2. A shifted right-skewed lognormal mixture for outcomes conditional on playing.

A pick-declining mixture weight allows for the rare second-round star without inflating the typical-pick curve.

<details>
<summary><strong>Round 2 model equations and priors</strong></summary>

<br>

$$\text{played}*n \sim \text{Bernoulli}(\pi*{p[n]})$$

$$
\text{ws4}_n - \text{floor} \mid \big(\text{played}_n = 1\big) \sim
\begin{cases}
\text{LogNormal}(m_p + \delta,\ \kappa, s_p) & \text{with prob. } u_p \
\text{LogNormal}(m_p,\ s_p) & \text{with prob. } 1 - u_p
\end{cases}
$$

The play probability $\pi_p$, typical median $m_p$, and dispersion $s_p$ each follow their own adjacent-pick random walk with drift, borrowing strength across neighboring second-round slots exactly as in the round-one variance model.

Priors:

$$\text{logit},\pi_p = \text{logit},\pi_{p-1} + d_\pi + \tau_\pi z^{\pi}_p, \qquad \pi_p \in (0,1)$$

$$m_p = m_{p-1} + d_m + \tau_m z^{m}_p, \qquad m_p \in \mathbb{R}$$

$$\log s_p = \log s_{p-1} + \tau_s z^{s}_p, \qquad s_p > 0$$

$$z^{\pi}_p,\ z^{m}_p,\ z^{s}_p \sim \mathcal{N}(0,1), \qquad z^{\pi}_p,\ z^{m}_p,\ z^{s}_p \in \mathbb{R}$$

$$d_\pi,\ d_m \sim \mathcal{N}(0,\ \cdot), \qquad d_\pi,\ d_m \in \mathbb{R}$$

$$\tau_\pi,\ \tau_m,\ \tau_s \sim \text{Half-}\mathcal{N}(0,\ \cdot), \qquad \tau_\pi,\ \tau_m,\ \tau_s \ge 0$$

$$\text{floor} \ \text{fixed constant}, \qquad \text{floor} < 0$$

$$u_p \ \text{declining in } p, \qquad u_p \in (0,1)$$

$$\delta \sim \text{Half-}\mathcal{N}(0,\ \cdot), \qquad \delta > 0$$

$$\kappa - 1 \sim \text{Half-}\mathcal{N}(0,\ \cdot), \qquad \kappa \ge 1$$

where, for player $n$ at slot $p[n] \in {31,\dots,60}$:

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

Expected asset value is generated directly as:

$$\text{EV}_p = \pi_p \cdot \mathbb{E}[\text{ws4} \mid \text{play},\ p]$$

</details>

<br>

### Team-Strength Model — `team_strength.stan`

A first-order Markov chain over the five 3-2-1 lottery tiers. The states are chosen to match the lottery's seeding mechanism: the tier a team occupies in season $t$ determines its ball count, so modeling tier-to-tier movement maps directly onto how each team's pick is seeded in future drafts.

Each row of the transition matrix receives a Dirichlet prior whose concentrations encode tier adjacency. Staying put or moving one tier is a priori more likely than leaping across the standings. Dirichlet-Multinomial conjugacy yields the closed-form posterior $\text{Dirichlet}(\alpha_{i\cdot} + \text{counts}_{i\cdot})$, computed in R as a cross-check, while Stan provides MCMC diagnostics and posterior draws for the downstream simulation.

<details>
<summary><strong>Team-strength model equations and priors</strong></summary>

<br>

$$\theta_{i\cdot} \sim \text{Dirichlet}(\alpha_{i\cdot}), \qquad \text{counts}*{i\cdot} \sim \text{Multinomial}(\theta*{i\cdot})$$

Priors:

$$\theta_{i\cdot} \sim \text{Dirichlet}(\alpha_{i\cdot}), \qquad \theta_{i\cdot} \in \Delta^{K-1} \ \ (\textstyle\sum_j \theta_{ij} = 1)$$

$$
\alpha_{ij} =
\begin{cases}
3.0 & i = j \
1.5 & |i - j| = 1 \
\max!\big(0.4,\ 1.5 \cdot 0.6^{,|i-j|-1}\big) & \text{otherwise,}
\end{cases}
\qquad \alpha_{ij} > 0
$$

with the observed transition counts entering as data:

$$\mathrm{counts}*{ij} \in \mathbb{Z}*{\geq 0}$$

where, for tiers:

$$i, j \in \lbrace \mathrm{relegation},\ \mathrm{non\text{-}play\text{-}in},\ \mathrm{play\text{-}in\ seed},\ \mathrm{play\text{-}in\ loser},\ \mathrm{playoff} \rbrace$$

| Parameter            | Description                                                                                                         |
| -------------------- | ------------------------------------------------------------------------------------------------------------------- |
| $\theta_{i\cdot}$    | The $i$-th row of the $5\times5$ transition matrix; a simplex giving $P(\text{tier}_{t+1}=j \mid \text{tier}_t=i)$  |
| $\text{counts}_{ij}$ | Observed historical count of season-over-season transitions $i \to j$ from 2005–2026 standings                      |
| $\alpha_{ij}$        | Dirichlet prior concentration: largest on the diagonal, smaller for adjacent tiers, and decaying with tier distance |

</details>

<br>

### Monte Carlo Simulator

To project future pick values, the simulation:

1. Seeds each team in its actual 2025–26 tier.
2. Evolves team tiers year by year using posterior draws from the Markov transition model.
3. Orders teams within tiers to construct future lottery seeds.
4. Runs both the current lottery and the new 3-2-1 lottery.
5. Applies 3-2-1 restrictions:

   * Relegation-tier ball counts
   * 12th-pick floor for relegated teams
   * No consecutive No. 1 overall picks
   * No three straight top-5 picks
   * No newly traded top-12 through top-15 protections
6. Applies public pick obligations:

   * Outright traded picks
   * Protections
   * Conveyance conditions
   * Swap rights
   * Return legs
   * Multi-team ranked pools
7. Values every owned pick under each simulated outcome.
8. Aggregates pick-level draws into team portfolios, single-pick summaries, pick-mover tables, and trade-machine outputs.

<br>

## Model Validation Summary

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

## Glossary

| Term                        | Meaning                                                                                                                                                                   |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **EPV**                     | Expected Pick Value. The posterior expected value of a pick or pick package before knowing the drafted player.                                                            |
| **4-YR WS**                 | First-four-year Win Shares. The player-production target used to value draft outcomes during the rookie-scale window.                                                     |
| **Credible interval**       | Bayesian uncertainty interval. A 90% credible interval means 90% of posterior draws fall inside that range.                                                               |
| **Conveyance**              | Whether a traded pick is actually delivered to the receiving team after applying protections and conditions.                                                              |
| **Protection**              | A restriction on a traded pick, usually allowing the original team to keep the pick if it lands in a specified range. Example: top-4 protected.                           |
| **Swap right**              | The right for one team to exchange its pick with another team's pick, usually taking the more favorable slot.                                                             |
| **Upside component**        | In the round-two model, the rare high-outcome mixture component that captures second-round stars without inflating the typical expected value of every second-round pick. |
| **Relegation tier**         | The bottom three teams under the 3-2-1 structure. They receive two lottery balls each and cannot pick worse than 12th.                                                    |
| **Expected asset value**    | Pick value using the posterior mean value of the simulated draft slot. This is the cleaner trade-value metric.                                                            |
| **Realized player outcome** | Simulated player-level outcome around the pick curve. This captures the uncertainty that a lower pick can still become a star or a higher pick can bust.                  |

<br>

## Repo Structure

| File                    | Purpose                                                                     |
| ----------------------- | --------------------------------------------------------------------------- |
| `01_data.R`             | Scrape standings, rosters, draft production; build Markov transition counts |
| `02_picks.R`            | Pick ownership, protections, swaps for 2026–2032                            |
| `03_models.R`           | Fit all three Stan models and run LOO comparison                            |
| `04_lotterySims.R`      | Monte Carlo lottery + valuation engine                                      |
| `05_model_validation.R` | Standalone SBC / LOO / PPC validation                                       |
| `picks_Round1.stan`     | Round 1 Student-t / random-walk sigma curve                                 |
| `picks_Round2.stan`     | Round 2 hurdle model                                                        |
| `team_strength.stan`    | Tier-transition Markov chain                                                |
| `app.R`                 | Shiny dashboard + trade machine                                             |

<br>

## Running the Project

There are two ways to run the project.

### Quickstart: Run the Dashboard from Precomputed Data

Use this path if you only want to launch the Shiny app without re-scraping data or re-fitting Stan models.

```r
install.packages(c(
  "tidyverse", "shiny", "plotly", "DT", "bslib", "igraph"
))

shiny::runApp("app.R")
```

This assumes the repo includes a precomputed `dashboard_data.rds` file in the expected location.

Depending on the local file structure, the app looks for:

```text
01_data/dashboard_data.rds
dashboard_data.rds
```

### Full Rebuild: Scrape Data, Fit Models, Run Simulations

Use this path to fully rebuild the data, refit Stan models, rerun lottery simulations, and regenerate `dashboard_data.rds`.

```r
install.packages(c(
  "tidyverse", "hoopR", "rvest", "httr", "cmdstanr",
  "janitor", "posterior", "expm", "loo",
  "shiny", "plotly", "DT", "bslib", "igraph"
))

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

## Limitations and Future Work

* **No time discounting yet.** A 2031 pick and a 2027 pick of equal EPV are treated equally.
* **No surplus value yet.** Production is not netted against rookie-scale salary.
* **Team projections are intentionally simple.** The Markov model does not yet account for age, roster continuity, salary cap space, draft capital, injuries, market size, player development, or front-office strategy.
* **Historical team behavior may not fully generalize.** Team projections are constructed using historical seasons that operated under the old lottery rules. The new 3-2-1 incentives may change behavior in ways that historical data cannot fully identify.
* **Deeply nested swap chains are approximated.** Publicly reported multi-team pick obligations can be ambiguous or conditional in ways that require close approximations.
* **Win Shares is imperfect.** Future versions may explore EPM, RAPTOR, xRAPM, DARKO, BPM, or salary-adjusted surplus value.
* **No player/prospect covariates yet.** The current model values picks by slot and team/pick context, not by prospect-specific information.
* **Future second-round pick details can be incomplete.** Some reported trades do not immediately disclose the exact years or conditions of second-round picks.

<br>

## References

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
