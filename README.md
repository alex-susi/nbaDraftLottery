# Probabilistic Valuations of NBA Draft Picks Under the New 3-2-1 Lottery

[![Dashboard](https://img.shields.io/badge/Interactive%20Dashboard-Shiny-blue)](https://alexsusi2298.shinyapps.io/nbaDraftLottery/)
[![R](https://img.shields.io/badge/R-Shiny%20%7C%20Stan-276DC3)](https://www.r-project.org/)
[![Stan](https://img.shields.io/badge/Stan-Bayesian%20Modeling-B2011D)](https://mc-stan.org/)
[![License](https://img.shields.io/badge/License-ADD%20LICENSE-lightgrey)](LICENSE)


## 01 - Overview

This project estimates how the **[NBA's new 3-2-1 Draft Lottery](https://www.nba.com/news/nba-board-governors-approve-new-draft-lottery-system)** will impact the value, risk, and trade utility of every tradeable draft pick from 2026–2032. Rather than assigning each pick a single value, the model simulates future team strength, lottery outcomes, pick protections, swaps, conveyance rules, and player-outcome uncertainty. The result is a distribution of possible values for every pick, team portfolio, and hypothetical trade.

The **[Interactive Dashboard](https://alex-susi.github.io/nbaDraftLottery/)** includes team-level portfolio views, individual pick distributions, lottery odds, pick movers, and a trade machine to evaluate hypothetical pick deals.

> This is an independent research / portfolio project and is not an official NBA, team, or league valuation model. Future pick obligations, protections, swaps, and conveyance rules are encoded based on public sources and were last updated as of **2026-06-22**.

<br>

## 02 - Dashboard Preview

| Tab            | Use case                                                                     |
| -------------- | ---------------------------------------------------------------------------- |
| Summary        | Which teams gain and lose the most pick value under 3-2-1, by how much, and which picks move most |
| Team           | Team portfolios, the pick landscape (quality vs quantity), and single-pick value distributions |
| Trade Machine  | Evaluate a real or hypothetical transaction          |
| Methodology    | Valuation overview, pick-value curve, team-strength model, lottery odds, validation, and glossary |


<details>
<summary><h3>Dashboard Screenshots</h3></summary>

*These screenshots show the earlier eight-tab, Win Shares version of the dashboard and need to be re-captured.*

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

2. In the long run (7 years in this case, since that's the furthest out draft picks can be traded), team performance expectations converge towards league-average (15th). As such, there is also a convergence of expected value for distant draft picks. For first-round picks, the EPV converges to around 4.4 xRAPM wins above replacement over the rookie contract, and around 0.7 for second-round picks.

3. The largest decreases in single-pick EPV are Memphis' 2027 first round most-favorable selection of Utah, Cleveland, and Minnesota (-1.0 EPV, -16%, due to Utah's pick being ineligible to land in the top 5) and Washington's own 2027 first round pick (-1.0 EPV, -16%, as it cannot land #1 overall in consecutive years).

4. The largest increases in single-pick EPV are Miami's top-14 protected 2027 first round pick (+0.5 EPV, +23%), Brooklyn's less favorable 2027 first round pick between their own and Houston's (+0.3, +31%), and Orlando's own 2027 first (+0.3).


<br>

## 04 - Methodology

1. **Draft Pick Value Curves**

   * First-round picks are modeled with a Bayesian curve on draft slot that is strictly decreasing. An earlier pick is always worth more, but the uncertainty is in how much more. Player outcomes follow a right-skewed (ex-Gaussian) distribution whose spread and skew are smoothed across neighboring picks.
   * Second-round picks are modeled separately using a hurdle model because many second-rounders never appear in an NBA game.

2. **Projecting Future Team Performance**

   * Markov chain model used to simulate future standings, where a team's standing in Year `t+1` is dependent on the team's standing in year `t`.

3. **Lottery Simulation**

   * For 2027-2032, a Monte Carlo simulation projects future team standings, and runs both the current lottery and the new 3-2-1 system.
   * It applies the appropriate number of lottery balls based on tier, the 12th-pick floor, no consecutive No. 1 picks, no three straight top-5 picks, and the ban on newly traded top-12 through top-15 protections.
   * The simulation applies future pick obligations, protections, and swap rights.
   * The trade machine evaluates hypothetical pick trades using correlated simulation draws, so team trajectories and pick outcomes remain internally consistent.


<br>

### Draft Pick Value Curve Outcome Variable

The core player outcome is [xRAPM](https://xrapm.com/) wins above replacement over the four rookie-contract seasons after the draft. xRAPM is a plus-minus rating that combines lineup data with a box-score and play-by-play prior. It is a rate (points per 100 possessions relative to league average), so each season is converted to wins above a replacement level of −2.0:

$$\text{WAR}_{n,t} = \frac{(\text{xRAPM}_{n,t} + 2.0) \times \text{possessions}_{n,t}}{100 \times 30.4}$$

$$\text{4-YR xRAPM WAR}_n = \sum_{t = d_n + 1}^{d_n + 4} \text{WAR}_{n,t}$$

where $d_n$ is the player's draft year.

* **Replacement level.** −2.0 is B-Ref's BPM convention for a minimum-salary or end-of-rotation player. A −2.0 player adds nothing, and minutes below it subtract value.
* **Points per win.** 30.4 points of scoring margin per win matches B-Ref's 2.7 VORP-to-wins factor (82 ÷ 2.7).
* **Window.** The window is the four seasons after the draft, matching the cost-controlled rookie-scale contract. A season a player misses (injury, stash, never signed) counts as zero.
* **Season length.** Lockout and COVID seasons (1998–99, 2011–12, 2019–20, 2020–21) are scaled to 82 games.
* **Data and joins.** xRAPM (1996–97 to 2025–26) and possessions come from the cached files in `01_data/`. They are joined to Basketball-Reference player-seasons by season and normalized name; 99.3% of player-seasons (99.5% of minutes) match. The few unmatched seasons fall back to 2.7 × VORP, which uses the same baseline and wins scale.
* **Draft classes.** The fit covers the 2015–2022 classes (2022 is the last class with four completed seasons). Earlier classes are left out for two reasons. The game has changed since the early 2010s. And xRAPM's inputs (tracking-based shot defense, deflections) only exist from the mid-2010s, so older seasons are rated differently. That leaves about eight players per draft slot; the Bayesian models carry the resulting uncertainty into every curve and simulation.

The Draft Pick Value Curve distinguishes between:

* **Expected Pick Value (EPV):** the posterior mean value of a draft pick before knowing the drafted player.
* **4-Yr xRAPM WAR Outcomes:** simulated player-level outcomes around the pick curve.

While higher draft picks have greater EPV, that does not guarantee that players picked higher will perform better than players drafted after them. For instance, the 1st pick in a draft is always a more valuable asset than the 3rd pick in the same draft, but that does not guarantee that the player selected with the 1st pick will be better than the player selected 3rd.

<br>

### First Round Pick Model

A regression of 4-YR xRAPM WAR on draft slot with three smoothed pieces: the expected value at each pick, the spread of player outcomes, and their skew.

**Expected value is strictly decreasing.** Pick 1 is always worth more than pick 2, which is always worth more than pick 3, in every posterior draw. What is uncertain is *how much* more. The curve is built from positive gaps between adjacent picks. A power law sets the typical gap, and a random walk lets each gap depart from it, so neighboring picks share information and the data decide how far the curve bends:

$$\mu_{30} = \alpha \, 30^{-\beta} + \gamma, \qquad \mu_p = \mu_{p+1} + \alpha \left(p^{-\beta} - (p+1)^{-\beta}\right) e^{\epsilon_p}$$

**Player outcomes are right-skewed.** Most picks land near replacement and a few become stars. Outcomes are ex-Gaussian (a Normal plus an Exponential upside), parameterized so that $\mu_p$ is exactly the mean:

$$\text{WAR}_n \sim \text{Normal}(\mu_p - \tau_p,\ \sigma_p) + \text{Exponential}(\text{mean } \tau_p)$$

The total outcome SD and the skew share $r_p = \tau_p / \text{SD}_p$ each follow **second-order** random walks: the pick-to-pick *slope* drifts slowly, so risk can keep shrinking down the draft or gradually level off, but it can't zigzag around a handful of careers. A first-order walk (each pick merely close to its neighbors) followed bumps in the 2015–2022 data, such as four large negatives at picks 20–24 from players handed heavy minutes on rebuilding teams. Those bumps don't appear in the 1996–2014 classes. The reasons risk shrinks with draft position (less playing time on better teams, lower ceilings) change gradually, so the second-order version treats such bumps as luck.

The curve $\mu_p$ is used directly as expected pick value, so it has to track the **mean** outcome at each slot. Likelihoods were compared in a leave-one-draft-class-out cross-validation over the 2015–2022 classes, scored on held-out mean error and on how well they describe held-out players ([`03_validation/r1_likelihood_cv_xrapm.csv`](03_validation/r1_likelihood_cv_xrapm.csv)):

| Likelihood | Held-out RMSE | Mean bias | Bias, picks 1–3 | Held-out log density | Inside 10th–90th (target 80%): picks 1–10 / 11–20 / 21–30 |
| ---------- | ------------- | --------- | --------------- | -------------------- | ---------------------------------------------------------- |
| Ex-Gaussian, pooled gaps, 2nd-order spread (production) | 7.78 | **−0.02** | −1.95 | −3.31 | 79% / 81% / 80% |
| Ex-Gaussian, pooled gaps, 1st-order spread | 7.78 | −0.07 | −1.98 | **−3.30** | 79% / 83% / 79% |
| Ex-Gaussian, pure power law | 7.78 | −0.07 | −1.95 | −3.30 | 79% / 83% / 79% |
| Gaussian | 7.78 | −0.15 | −1.97 | −3.45 | 81% / 85% / 84% |
| Student-t | 7.86 | −1.26 | −2.94 | – | – |
| Shifted-lognormal mixture | 7.91 | +0.93 | +2.45 | – | – |
| Raw slot means | 8.45 | 0 | 0 | – | – |

* **Ex-Gaussian:** ties the Gaussian on expected value and describes held-out players better, so outcomes come straight from the model. The pooled-gap curve scores the same as a pure power law with eight classes of data, but lets individual gaps move as more classes are added.
* **Second- vs first-order spread:** the two are equivalent on held-out players (a 0.01 log-density difference per player, same band coverage); the second-order version gives a smooth band that follows the broad decline instead of 2015–2022 bumps.
* **Gaussian:** targets the mean, but its symmetric shape puts too few held-out players below the 10th percentile (5%). The previous version drew outcomes from empirical residuals within two picks instead, which made the outcome band jagged.
* **Student-t:** it centers on the typical outcome, so it sits below slot means.
* **Lognormal mixture:** extrapolating its tail overstates top picks.
* **Picks 1–3:** every smooth curve under-predicts them by about two wins. In 2015–2022 the average No. 3 pick (14.6) out-produced the average No. 2 (6.7); a decreasing curve treats that as noise.

| Parameter  | Description |
| ---------- | ----------- |
| $\mu_p$    | Expected four-season xRAPM wins above replacement at pick $p$ (expected pick value) |
| $\alpha$   | Curve height of the top of the lottery |
| $\beta$    | Decay exponent of the typical gap between picks |
| $\gamma$   | Baseline value the curve approaches in the late first round |
| $\epsilon_p$, $\tau_\epsilon$ | Departure of gap $p$ from the power law (a centered random walk), and its learned scale |
| $\text{SD}_p$, $r_p$ | Total outcome SD and skew share at pick $p$, each following a second-order random walk (slowly drifting slope) |
| $\sigma_p$, $\tau_p$ | Gaussian SD and exponential-upside mean of the ex-Gaussian, $\tau_p = r_p \text{SD}_p$, $\sigma_p = \text{SD}_p\sqrt{1 - r_p^2}$ |


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
//      (r1_inits in 03_models.R): every slope change compounds across later
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
```


</details>

<br>

### Second Round Pick Model

Unlike first-round picks, second-round picks frequently sign non-guaranteed contracts, spend significant time in the G League, or return to play overseas before ever appearing in an NBA game.

The second round model is therefore a two-part **hurdle**:

1. A Bernoulli model for whether the player drafted in a given slot logs any NBA minutes.
2. A fringe/contributor model for the value of players who do play.

* **Expected value is strictly decreasing.** As in the first round, pick 31 is always worth more than pick 32, and so on. The expected-value curve $\text{EV}_p = \pi_p \, \mu^{\text{play}}_p$ is built from positive gaps between adjacent picks, with a centered random walk letting each gap depart from the typical gap. The played mean is then $\mu^{\text{play}}_p = \text{EV}_p / \pi_p$.
* **Play probability.** $\pi_p$ follows an adjacent-pick random walk with drift on the logit scale. It is not forced to decrease; only expected value is.
* **Played outcomes.** Played second-rounders bunch tightly just below replacement (median −0.2 wins), and a few become rotation players worth 10–25 wins. A single exponential tail cannot fit both. An ex-Gaussian shrank the tail to fit the peak and understated expected value. Played outcomes are therefore a two-group mixture that shares a fringe level:

  $$\text{WAR}_n \mid \text{played} \sim (1 - q_p)\,\text{Normal}(c_p, s_p) + q_p\left[\text{Normal}(c_p, s_p) + \text{Exponential}(\text{mean } T_p)\right]$$

  The contributor share $q_p$, upside $T_p$ and fringe spread $s_p$ each follow an adjacent-pick random walk with drift: each pick resembles its neighbors, and the data decide how much the round's profile can bend. That lets the late-round collapse emerge, where teams shift to draft-and-stash and two-way prospects and almost nobody drafted after about pick 50 becomes a contributor. The fringe level is derived, $c_p = \mu^{\text{play}}_p - q_p T_p$, so the played mean stays exact.
* **Outcomes.** A simulated outcome is exactly zero when the player does not play, and a fringe or contributor draw otherwise.
* **Validation.** A leave-one-draft-class-out cross-validation compares three ways of letting the mixture change across the round, plus the ex-Gaussian and Gaussian hurdles. All are scored on every drafted player, including non-players at zero ([`03_validation/r2_likelihood_cv_xrapm.csv`](03_validation/r2_likelihood_cv_xrapm.csv)):

| Model | Held-out RMSE | Mean bias | Bias 31–40 | Bias 51–60 | Held-out log density | Inside 10th–90th at 51–60 (target 80%) |
| ----- | ------------- | --------- | ---------- | ---------- | -------------------- | -------------------------------------- |
| Mixture, per-pick random walks (production) | 3.64 | −0.07 | −0.46 | +0.25 | **−2.30** | 86% |
| Mixture, per-pick random walks, 2nd-order fringe spread | 3.64 | −0.10 | −0.48 | +0.20 | −2.40 | 89% |
| Mixture, log-linear trends | 3.64 | **−0.05** | −0.45 | +0.29 | −2.35 | 90% |
| Mixture, curved (quadratic) trends | 3.63 | −0.10 | −0.46 | **+0.17** | −2.40 | 89% |
| Ex-Gaussian | 3.64 | −0.19 | −0.52 | +0.07 | −2.42 | 90% |
| Gaussian | 3.63 | −0.10 | −0.32 | +0.02 | −2.65 | 92% |
| Raw slot means | 3.95 | 0 | 0 | 0 | – | – |

  The random-walk mixture describes held-out players best (about 12 log-density points better in total than log-linear trends) and has the best-calibrated late-round band. The curved trend lowers the late-pick mean the most but describes players worse. A second-order walk on the fringe spread smooths the lower band, but it loses about 23 log-density points on held-out players. The fringe spread genuinely differs from slot to slot (mostly through how many minutes that slot's players got), and those differences carry over to unseen draft classes, so the production model keeps the first-order walk. Because expected value must decrease smoothly, every mixture under-predicts picks 31–40 (the 2015–2022 No. 35 and No. 36 picks averaged 3.4 and 5.6 wins) and over-predicts picks 51–60, where observed value is slightly negative.


| Parameter                       | Description                                                                                 |
| ------------------------------- | ------------------------------------------------------------------------------------------- |
| $\pi_p$                         | Probability a player at pick $p$ logs NBA minutes in the four-season window                 |
| $\text{EV}_p$                   | Expected pick value, $\pi_p \, \mu^{\text{play}}_p$; strictly decreasing                    |
| $\mu^{\text{play}}_p$           | Mean four-season xRAPM WAR of players at pick $p$ who play, $\text{EV}_p / \pi_p$           |
| $q_p$, $T_p$                    | Share of played players who are contributors, and contributors' mean upside                 |
| $c_p$, $s_p$                    | Fringe level (derived) and spread                                                           |
| $d_\pi,\ \tau_\pi$              | Drift and random-walk scale of the logit play probability                                   |
| $\tau_\epsilon$                 | Learned scale of the gap departures in the expected-value curve                             |

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
//        played[n] ~ Bernoulli(pi_p[p]); a player who never plays is exactly 0.
//
//   2. Expected pick value (strictly decreasing in every posterior draw)
//        ev[p] = pi_p[p] * mean_play[p] is the expected value of the pick. It is
//        built from positive gaps between adjacent picks, so pick 31 is always
//        worth more than pick 32, and so on; the posterior uncertainty is in
//        HOW MUCH more:
//
//        ev[31]  = ev_31
//        ev[p+1] = ev[p] - gap[p]
//        gap[p]  = exp(log_gap + eps[p])
//
//      eps is a centered random walk, so adjacent gaps share information
//      (nearer picks more strongly) and tau_eps, learned from the data, sets how
//      far individual gaps can depart from the typical gap.
//
//   3. Played outcomes: fringe players and contributors
//        Played second-rounders bunch tightly just below replacement, and a few
//        become rotation players worth 10-25 wins. One exponential tail cannot
//        fit both the sharp peak and the long tail (an ex-Gaussian shrank the
//        tail and understated EV), so played outcomes are a two-group mixture
//        that shares the fringe level:
//
//        fringe       (prob 1 - p_contrib): Normal(fringe_mean, sd_fringe)
//        contributor  (prob p_contrib):     Normal(fringe_mean, sd_fringe)
//                                           + Exponential(mean upside_mean)
//
//        mean_play[p]   = ev[p] / pi_p[p]
//        fringe_mean[p] = mean_play[p] - p_contrib[p] * upside_mean[p]
//
//      so the played mean stays a parameter and EV stays exact. Contributor
//      share, upside size and fringe spread each follow an adjacent-pick
//      random walk with drift (logit / log scale), so neighboring picks share
//      information and the late-round collapse in contributors can emerge
//      from the data. In draft-class CV this beat log-linear trends
//      (zz_Archive/picks_Round2_mixture_linear_xrapm.stan) and quadratic
//      trends (zz_Archive/picks_Round2_mixture_curved_xrapm.stan) on held-out
//      log density and late-pick band calibration.
//
//   4. Play probability
//        eta[p] = logit(pi_p[p]) evolves by an adjacent-pick random walk with
//        drift. It is not forced to decrease; only expected value is.
//
//   Stan uses the local index j = pick - 30. Fit with adapt_delta = 0.999 (the
//   derived fringe level makes late-pick geometry sharp; 0.99 left a few
//   divergences) and starting points near the prior means (r2_mixture_inits in
//   03_models.R): generic random inits can start with steep EV gaps that put
//   the fringe level far from every played outcome, stranding the chain.
//
// Priors are on the xRAPM wins-above-replacement scale. Empirically (2015-2022
// drafts): ~95% of picks 31-35 log minutes vs ~66% at 56-60; expected value
// falls from ~1.8 wins at picks 31-35 to ~0 at 56-60; played outcomes have a
// median of -0.2 wins and a mean of 1.0.
// -----------------------------------------------------------------------------


data {
  int<lower=1> N;                        // Number of players
  array[N] int<lower=31, upper=60> pick; // Pick each player was drafted at (31-60)
  array[N] int<lower=0, upper=1> played; // Play indicator (0 = no NBA minutes in the window)
  vector[N] war4;                        // Four-season xRAPM wins above replacement (0 if never played)
}



parameters {
  // ---------------------------------------------------------------------------
  // Play probability curve
  // ---------------------------------------------------------------------------
  real eta_31;          // play probability for pick 31 (logit scale)
  real d_pi;            // average change in logit play probability per pick
  real<lower=0> tau_pi; // pick-to-pick curve flexibility
  vector[29] z_pi;      // pick-by-pick curve adjustments


  // ---------------------------------------------------------------------------
  // Expected value curve
  // ---------------------------------------------------------------------------
  real ev_31;            // expected value of pick 31
  real log_gap;          // typical gap between adjacent picks (log scale)
  vector[29] z_eps;      // pick-by-pick adjustments to the gaps
  real<lower=0> tau_eps; // how far gaps can depart from the typical gap


  // ---------------------------------------------------------------------------
  // Played-outcome mixture: adjacent-pick random walks with drift
  // ---------------------------------------------------------------------------
  real logit_q_31;       // contributor share at pick 31 (logit scale)
  real d_q;              // average change in logit contributor share per pick
  vector[29] z_q;        // pick-by-pick departures
  real<lower=0> tau_q;   // how far adjacent picks can differ
  real log_T_31;         // contributor upside mean at pick 31 (log scale)
  real d_T;
  vector[29] z_T;
  real<lower=0> tau_T;
  real log_s0_31;        // fringe spread at pick 31 (log scale)
  real d_s0;
  vector[29] z_s0;
  real<lower=0> tau_s0;
}



transformed parameters {
  vector[30] eta;
  vector<lower=0, upper=1>[30] pi_p;       // Play probabilities
  vector[29] eps;                          // Log departure of each gap from the typical gap
  vector<lower=0>[29] gap;                 // ev[j] - ev[j + 1]
  vector[30] ev;                           // Expected value by pick
  vector[30] mean_play;                    // Mean four-season value if the player plays
  vector<lower=0, upper=1>[30] p_contrib;  // Share of played players who are contributors
  vector<lower=0>[30] upside_mean;         // Contributors' exponential upside mean
  vector<lower=0>[30] sd_fringe;           // Fringe (and shared Gaussian) spread
  vector[30] fringe_mean;                  // Fringe level

  eta[1] = eta_31;
  for (j in 2:30) {
    eta[j] = eta[j - 1] + d_pi + tau_pi * z_pi[j - 1];
  }
  pi_p = inv_logit(eta);

  {
    vector[29] eps_raw = cumulative_sum(tau_eps * z_eps);
    eps = eps_raw - mean(eps_raw);
  }
  gap = exp(log_gap + eps);

  ev[1] = ev_31;
  for (j in 2:30) {
    ev[j] = ev[j - 1] - gap[j - 1];
  }
  mean_play = ev ./ pi_p;

  p_contrib   = inv_logit(logit_q_31 + append_row(0, cumulative_sum(d_q + tau_q * z_q)));
  upside_mean = exp(log_T_31 + append_row(0, cumulative_sum(d_T + tau_T * z_T)));
  sd_fringe   = exp(log_s0_31 + append_row(0, cumulative_sum(d_s0 + tau_s0 * z_s0)));
  fringe_mean = mean_play - p_contrib .* upside_mean;
}



model {
  // Priors
  // Play probability
  eta_31 ~ normal(logit(0.88), 0.60);  // Pick 31 very likely to appear in the NBA
  d_pi   ~ normal(-0.05, 0.05);        // Declines gradually from pick 31-60
  tau_pi ~ normal(0, 0.10);
  z_pi   ~ std_normal();

  // Expected value
  ev_31   ~ normal(1.5, 1);            // Pick 31 worth ~1.5 wins
  log_gap ~ normal(log(0.06), 0.75);   // ~0.06 wins per pick (~1.8 wins over the round)
  tau_eps ~ normal(0, 0.30);
  z_eps   ~ std_normal();

  // Played-outcome mixture
  logit_q_31 ~ normal(logit(0.30), 1); // ~30% of played pick-31 players contribute
  d_q        ~ normal(-0.02, 0.03);    // Fewer contributors later in the round
  log_T_31   ~ normal(log(5), 0.75);   // Contributors add ~5 wins on average
  d_T        ~ normal(-0.02, 0.03);
  log_s0_31  ~ normal(log(1), 0.75);   // Fringe outcomes spread ~1 win
  d_s0       ~ normal(0, 0.03);
  z_q        ~ std_normal();
  z_T        ~ std_normal();
  z_s0       ~ std_normal();
  tau_q      ~ normal(0, 0.10);
  tau_T      ~ normal(0, 0.10);
  tau_s0     ~ normal(0, 0.10);


  // Likelihood
  for (n in 1:N) {
    int j = pick[n] - 30;              // Convert NBA pick number 31-60 into index 1-30

    played[n] ~ bernoulli(pi_p[j]);

    if (played[n] == 1) {
      target += log_mix(p_contrib[j],
                        exp_mod_normal_lpdf(war4[n] | fringe_mean[j], sd_fringe[j],
                                            inv(upside_mean[j])),
                        normal_lpdf(war4[n] | fringe_mean[j], sd_fringe[j]));
    }
  }
}



generated quantities {
  vector[30] sd_play;     // SD of played outcomes
  vector[30] ev_sd;       // Total pick SD including zero outcomes and played variance
  array[N] int play_rep;  // Simulated played / did-not-play result for each historical row
  vector[N] log_lik;      // Per-player log likelihood for PSIS-LOO and validation


  for (j in 1:30) {
    // Var = sd_fringe^2 + Var(B * E), B ~ Bernoulli(q), E ~ Exponential(mean T)
    real q = p_contrib[j];
    real T = upside_mean[j];
    real m2;

    sd_play[j] = sqrt(square(sd_fringe[j]) + q * (2 - q) * square(T));
    m2 = pi_p[j] * (square(sd_play[j]) + square(mean_play[j]));
    ev_sd[j] = sqrt(fmax(0, m2 - square(ev[j])));
  }


  for (n in 1:N) {
    int j = pick[n] - 30;

    play_rep[n] = bernoulli_rng(pi_p[j]);

    log_lik[n] = bernoulli_lpmf(played[n] | pi_p[j]);
    if (played[n] == 1) {
      log_lik[n] += log_mix(p_contrib[j],
                            exp_mod_normal_lpdf(war4[n] | fringe_mean[j], sd_fringe[j],
                                                inv(upside_mean[j])),
                            normal_lpdf(war4[n] | fringe_mean[j], sd_fringe[j]));
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
| Round 1 pick value   | R-hat / ESS                      | max R-hat 1.001; min bulk-ESS 3,051; 0 divergences | Confirms the posterior draws mixed reliably, so the reported means and intervals are not chain artifacts. | PASS   |
| Round 1 pick value   | Posterior predictive coverage    | 90% player rows coverage 90.8% (ex-Gaussian outcomes, second-order spread) | Compares simulated draft outcomes to historical outcomes; good coverage means the model's uncertainty is realistic, not just the average curve. | PASS   |
| Round 1 pick value   | PSIS-LOO / Pareto-k + mean CV    | elpd_loo -793.6 (best of 6 by 18+); max Pareto-k 0.70; draft-class CV RMSE 7.78, bias -0.02 | LOO checks influential players; the draft-class cross-validation checks the mean curve that drives EPV. | PASS   |
| Round 2 hurdle       | R-hat / ESS                      | max R-hat 1.001; min bulk-ESS 5,208; 0 divergences | Confirms the posterior draws mixed reliably, so the reported means and intervals are not chain artifacts. | PASS   |
| Round 2 hurdle       | Posterior predictive / play rate | 90% PPC 89.5%; empirical play 82.4%; P(play) #31/#45/#60 92.7% / 83.7% / 65.1%; EV 0.74 vs 0.80 observed | Compares simulated draft outcomes to historical outcomes; good coverage means the model's uncertainty is realistic, not just the average curve. | PASS   |
| Round 2 hurdle       | PSIS-LOO / Pareto-k + mean CV    | elpd_loo -533.5; p_loo 32.4; max Pareto-k 1.23; draft-class CV RMSE 3.64, bias -0.07 | A couple of second-round outliers (24.6 and 18.1 wins) are highly influential, so PSIS-LOO is unreliable for them; the mean is checked by draft-class cross-validation instead. | WARN   |
| Team-strength Markov | Transition-matrix code check     | 630 transitions over 22 seasons; mixing time 1.8 seasons; 0 divergences | Verifies the rank-transition engine, which supports the future team-path simulations. | PASS   |
| Validation script    | NUTS geometry                    | Displayed from `03_validation/validation_decision_table_latest_models.csv` when `05_model_validation.R` has been run; target is 0 divergences, 0 max-treedepth hits, E-BFMI > 0.30. | Checks whether Stan explored the posterior without pathological geometry; divergences or treedepth hits would undermine trust in the draws.     | INFO   |
| Validation script    | SBC rank uniformity              |                   Displayed from `03_validation/validation_decision_table_latest_models.csv` when SBC is run; approximately uniform ranks indicate calibrated Bayesian uncertainty. | Uses simulated data where the truth is known to verify that Bayesian credible intervals are calibrated.                                         | INFO   |

</details>

<br>

Additional validation checks include:

* **SBC** (simulation-based calibration) for rank uniformity.
* **PSIS-LOO** with Pareto-k diagnostics, plus leave-one-draft-class-out cross-validations of both rounds (`03_validation/r1_likelihood_cv.R`, `03_validation/r2_likelihood_cv.R`) scoring held-out mean error, log density, and outcome-band calibration.
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

> **Note (2026-10-03):** the screenshots and percentages in this case study come from the earlier Win Shares version of the model and have not been re-captured since the xRAPM rebuild.

On paper, this good business for Memphis. The expected value lost from moving down only 1 draft slot is more than compensated for by picking up 2 future second round picks, noted by the 100% higher EPV to Memphis. However, Memphis is not guaranteed to get more productivity out of the players selected with these picks, but the model favors them with about a 64% chance. The trade machine also likes Memphis' chances to get the single-most productive player with the picks involved in this deal, at about a 59% rate. 

An important caveat here is that deals like this one that occur live during the draft are almost always done when a team is targeting a specific player. Teams may be more willing to trade back if a player they like is stil expected to be on the board, or may be willing to "overpay" with future draft picks if they really want a specific player and don't want to risk another team drafting him. Additionally, this does not specifically model player projections on the incoming 2026 rookies, so the above player outcome percentages may be impacted by that too. 

<br>

## 07 - Glossary

| Term                        | Meaning                                                                                                                                                                   |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **EPV**                     | Expected Pick Value. The posterior expected value of a draft pick before knowing the drafted player.                                                            |
| **4-YR xRAPM WAR**         | xRAPM wins above replacement (-2.0 baseline) over the four rookie-contract seasons after the draft. The player metric used to value draft outcomes.                  |
| **Replacement level**       | -2.0 points per 100 possessions relative to league average: roughly a minimum-salary or end-of-rotation player. A replacement-level player adds zero value.          |
| **Credible interval**       | Bayesian uncertainty interval. A 90% credible interval means 90% of posterior draws fall inside that range.                                                               |
| **Conveyance**              | Whether a traded pick is actually delivered to the receiving team after applying protections and conditions.                                                              |
| **Protection**              | A restriction on a traded pick, usually allowing the original team to keep the pick if it lands in a specified range. Example: top-4 protected.                           |
| **Swap right**              | The right for one team to exchange its pick with another team's pick, usually taking the more favorable slot.                                                             |
| **Ex-Gaussian**             | A Normal plus an Exponential upside. Simulated player outcomes use it so the right skew of real careers (many near replacement, a few stars) carries into the simulations while the slot's expected value stays exact. |
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
* **xRAPM is one rating among several.** It blends lineup data with a box-score prior, and its inputs (shot defense, deflections) only exist from the mid-2010s, so its construction changes over the fit window. The replacement level (−2.0) is a convention; moving it changes how much credit playing time earns and how top-heavy the pick curve is. VORP- and other metric-based curves are planned robustness checks (see `ROADMAP.md`).
* **Rookie contract only.** Value stops after four seasons, so it excludes extension and restricted-free-agency value, which matters most for top picks.
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
