# Probabilistic NBA Draft Pick Valuations Under the New 3-2-1 Lottery

A fully Bayesian pipeline for valuing NBA draft picks under the league's new **3-2-1 lottery format** (approved by the NBA Board of Governors in May 2026, effective for the 2027–2029 drafts). Every pick is valued with full posterior uncertainty, picks are re-priced under both the old and new lottery systems, and a trade machine lets you compare any deal with correlated within-simulation draws.

To my knowledge this is the first public implementation of the official 37-ball, 16-team, relegation-floor lottery structure, paired with a rigorous Bayesian treatment rather than point-estimate value curves.

<br>

## Overview
 
- **Values every first and second round draft pick, 2026–2032**, under both the current and 3-2-1 lottery systems
- **Propagates full posterior uncertainty** from model parameters through credible intervals on every pick and every trade, including pick conveyance and swap exercise probabilities.
- **Models the new anti-tank rules** — relegation tier ball counts, the 12th pick floor, no consecutive #1 overall picks, no three straight top-5 picks, and the 12–15 protection ban.
- **Runs a trade machine** that values multi-pick deals (with hypothetical protections/swaps) using correlated draws, so realized player outcomes can be compared to expected pick value.

<br>

## Methodology
 
**Pick value = first-4-year Win Shares** (the rookie-scale cost-controlled window), scraped from Basketball-Reference draft classes 1985–2021.
 
Three Stan models:
 
### First Round Picks — `pick_value_v3.stan`
 
A robust regression of first-four-year Win Shares on draft slot. The mean follows a power-law decay in pick number, and per-slot residual variance is smoothed across adjacent picks by a Gaussian random walk on the log scale. The Student-t likelihood accommodates the heavy tails of draft outcomes (occasional superstars and total busts) without the mean curve being distorted by outliers.
 
$$\text{ws4}_n \sim \text{Student-}t\big(\nu,\ \mu_{p[n]},\ \sigma_{p[n]}\big), \qquad \mu_p = \frac{\alpha}{p^{\beta}} + \gamma$$
 
$$\log \sigma_p = \log \sigma_{p-1} + \tau\, z_p$$
 
Priors:
 
$$\log\alpha \sim \mathcal{N}(\log 20,\ 0.6), \qquad \alpha > 0$$
 
$$\log\beta \sim \mathcal{N}(\log 0.55,\ 0.5), \qquad \beta > 0$$
 
$$\gamma \sim \mathcal{N}(2,\ 3), \qquad \gamma \in \mathbb{R}$$
 
$$\log\sigma_1 \sim \mathcal{N}(\log 8,\ 0.5), \qquad \sigma_1 > 0$$
 
$$\tau \sim \text{Half-}\mathcal{N}(0,\ 0.15), \qquad \tau \ge 0$$
 
$$z_p \sim \mathcal{N}(0,1), \qquad z_p \in \mathbb{R}$$
 
$$\nu - 2 \sim \text{Exponential}(0.20), \qquad \nu > 2$$
 
where, for player $n$ drafted at pick $p[n] \in \{1,\dots,30\}$:
 
| Parameter | Description |
|-----------|------|
| $\mu_p$ | expected first-four-year Win Shares at pick $p$. |
| $\alpha$ | curve amplitude, governing the height of the top of the lottery. |
| $\beta$ | decay exponent controlling how steeply value falls with pick number. |
| $\gamma$ | asymptotic floor, the baseline value the curve approaches in the late first round. |
| $\sigma_p$ | slot-specific residual scale (outcome volatility around $\mu_p$), anchored at $\sigma_1$ and propagated by the random walk. |
| $\tau$ | random-walk innovation scale; small $\tau$ forces $\sigma_p$ to gradually evolve across neighboring slots, large $\tau$ permits abrupt volatility shifts. |
| $z_p$ | how much pick $p$'s outcome risk moves from those of the adjacent picks. |
| $\nu$ | Student-t degrees of freedom; lower $\nu$ yields heavier tails, with the constraint $\nu>2$ keeping the variance finite. The $\text{Exponential}(0.20)$ prior centers $\nu \approx 7$ (moderately heavy tails) while permitting near-Gaussian or much heavier tails as the data dictate. |

<br>

### Second Round Picks — `pick_play_r2_v6_declining_upside.stan`
 
Second-round picks violate the first-round model because the modal career outcome is *zero* NBA value. The valuation is therefore a two-part **hurdle**: a Bernoulli model for whether a player logs any NBA minutes at all, and, conditional on playing, a shifted right-skewed lognormal mixture. A pick-declining mixture weight allows for the rare second-round star without inflating the typical-pick curve.
 
$$\text{played}_n \sim \text{Bernoulli}(\pi_{p[n]})$$
 
$$
\text{ws4}_n - \text{floor} \mid \big(\text{played}_n = 1\big) \sim
\begin{cases}
\text{LogNormal}(m_p + \delta,\ \kappa\, s_p) & \text{with prob. } u_p \\
\text{LogNormal}(m_p,\ s_p) & \text{with prob. } 1 - u_p
\end{cases}
$$
 
The play probability $\pi_p$, typical median $m_p$, and dispersion $s_p$ each follow their own adjacent-pick random walk (with drift), borrowing strength across neighboring second-round slots exactly as in the round-one variance model.
 
Priors:
 
$$\text{logit}\,\pi_p = \text{logit}\,\pi_{p-1} + d_\pi + \tau_\pi z^{\pi}_p, \qquad \pi_p \in (0,1)$$
 
$$m_p = m_{p-1} + d_m + \tau_m z^{m}_p, \qquad m_p \in \mathbb{R}$$
 
$$\log s_p = \log s_{p-1} + \tau_s z^{s}_p, \qquad s_p > 0$$
 
$$z^{\pi}_p\, z^{m}_p\, z^{s}_p \sim \mathcal{N}(0,1), \qquad z^{\pi}_p\, z^{m}_p\, z^{s}_p \in \mathbb{R}$$
 
$$d_\pi\, d_m \sim \mathcal{N}(0,\ \cdot), \qquad d_\pi\, d_m \in \mathbb{R}$$
 
$$\tau_\pi\, \tau_m\, \tau_s \sim \text{Half-}\mathcal{N}(0,\ \cdot), \qquad \tau_\pi\, \tau_m\, \tau_s \ge 0$$
 
$$\text{floor} \ \text{fixed constant}, \qquad \text{floor} < 0$$
 
$$u_p \ \text{declining in } p, \qquad u_p \in (0,1)$$
 
$$\delta \sim \text{Half-}\mathcal{N}(0,\ \cdot), \qquad \delta > 0$$
 
$$\kappa - 1 \sim \text{Half-}\mathcal{N}(0,\ \cdot), \qquad \kappa \ge 1$$
 
where, for player $n$ at slot $p[n] \in \{31,\dots,60\}$:
 
| Parameter | Description |
|-----------|------|
| $\pi_p$ | Probability a player at pick $p$ records any NBA production. |
| $\text{floor}$ | Fixed negative shift placing the lognormal support a few Win Shares below the worst observed played-player outcome, so $\text{ws4}_n - \text{floor} > 0$ and the log scale is always valid. |
| $m_p$ | log-median of the *typical* played-player outcome at pick $p$. |
| $s_p$ | lognormal dispersion of the typical component; how spread out the rotation player outcomes are at pick $p$. |
| $u_p$ | Rare-upside mixture weight, regularized to *decline* across the round so later picks are not credited with implausible star probability. |
| $\delta$ | Additive log-location shift of the upside component; the gap between a rare star hit and a typical contributor. |
| $\kappa$ | Multiplicative inflation of the upside component's dispersion, capped to prevent an explosive far-right tail; how much rarer the boom-or-bust outcomes are than the typical outcomes. |
| $d_\pi$ | Drift in play probability across picks, capturing the steady decline in odds that a later pick records at least 1 NBA minute. |
| $d_m$ | Drift in typical-outcome median across picks, capturing how the value of a contributing pick decreases deeper into the round. |
| $\tau_\pi\, \tau_m\, \tau_s$ | Random-walk innovation scales for the play, median, and dispersion curves; how smoothly each evolves from one pick to the next. |
| $z^{\pi}_p\, z^{m}_p\, z^{s}_p$ | Pick-specific departures of each curve from its neighbor. |

Expected asset value is generated directly as $\text{EV}_p = \pi_p \cdot \mathbb{E}[\text{ws4} \mid \text{play},\ p]$.

<br>

### Team strength — `team_strength.stan`
 
A first-order Markov chain over the five 3-2-1 lottery tiers, chosen so the states *are* the lottery's seeding mechanism: the tier a team occupies in season $t$ determines its ball count, so modeling tier-to-tier movement maps directly onto how each team's pick is seeded in future drafts. Each row of the transition matrix receives a Dirichlet prior whose concentrations encode tier adjacency (staying put or moving one tier is a priori far likelier than leaping across the standings). Dirichlet-Multinomial conjugacy yields the closed-form posterior $\text{Dirichlet}(\alpha_{i\cdot} + \text{counts}_{i\cdot})$, computed in R as a cross-check, while Stan provides MCMC diagnostics and posterior draws for the downstream simulation.
 
$$\theta_{i\cdot} \sim \text{Dirichlet}(\alpha_{i\cdot}), \qquad \text{counts}_{i\cdot} \sim \text{Multinomial}(\theta_{i\cdot})$$
 
Priors:
 
$$\theta_{i\cdot} \sim \text{Dirichlet}(\alpha_{i\cdot}), \qquad \theta_{i\cdot} \in \Delta^{K-1} \ \ (\textstyle\sum_j \theta_{ij} = 1)$$
 
$$
\alpha_{ij} =
\begin{cases}
3.0 & i = j \\
1.5 & |i - j| = 1 \\
\max\!\big(0.4,\ 1.5 \cdot 0.6^{\,|i-j|-1}\big) & \text{otherwise,}
\end{cases}
\qquad \alpha_{ij} > 0
$$
 
with the observed transition counts entering as data:

$$\mathrm{counts}_{ij} \in \mathbb{Z}_{\geq 0}$$

where, for tiers:

$$i, j \in \lbrace \mathrm{relegation},\ \mathrm{non\text{-}play\text{-}in},\ \mathrm{play\text{-}in\ seed},\ \mathrm{play\text{-}in\ loser},\ \mathrm{playoff} \rbrace$$
 
| Parameter | Description |
|-----------|------|
| $\theta_{i\cdot}$ | The $i$-th row of the $5\times5$ transition matrix; a simplex giving $P(\text{tier}_{t+1}=j \mid \text{tier}_t=i)$. |
| $\text{counts}_{ij}$ | Observed historical count of season-over-season transitions $i \to j$ (2005–2026 standings). |
| $\alpha_{ij}$ | Dirichlet prior concentration: largest on the diagonal (stay), smaller for adjacent tiers, and decaying with tier distance, with a strictly nonzero floor so rare jumps (e.g. relegation → playoff) are not assigned probability exactly zero and the model is not overconfident that bad teams stay bad. |

To project forward, each team is seeded in its actual 2025–26 tier and the chain is evolved year by year; the stationary distribution and mixing time (from the second eigenvalue $\lambda_2$) confirm that current standing washes out toward the league baseline within a few seasons, which is reflected in the widening uncertainty on far-future picks.
 
A Monte Carlo simulation then projects each team's tier forward, runs both lotteries, applies all protections/swaps/restrictions, and values every pick under each system.

<br>
 
## Validation
 
- **SBC** (simulation-based calibration) for rank uniformity
- **PSIS-LOO** with Pareto-k diagnostics, comparing the random-walk variance model against constant- and linear-sigma baselines
- **Posterior predictive checks** on transition-count statistics
- **NUTS diagnostics** and a closed-form Dirichlet-Multinomial cross-check
- Simulated lottery odds validated against the official published 3-2-1 table
## Repo structure
 
| File | Purpose |
|------|---------|
| `01_data.R` | Scrape standings, rosters, draft production; build Markov transition counts |
| `02_picks.R` | Pick ownership, protections, swaps for 2026–2032 |
| `03_models.R` | Fit all three Stan models and run LOO comparison |
| `04_lotterySims.R` | Monte Carlo lottery + valuation engine |
| `05_model_validation.R` | Standalone SBC / LOO / PPC validation |
| `pick_value_v3.stan` | Round 1 Student-t / RW-sigma curve |
| `pick_play_r2_v6_declining_upside.stan` | Round 2 hurdle model |
| `team_strength.stan` | Tier-transition Markov chain |
| `app.R` | Shiny dashboard + trade machine |

<br>

## Running it
 
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
 
Run `05_model_validation.R` separately for the full validation suite (outputs to `./03_validation/`).

<br>

## Limitations
 
- No time discounting — a 2031 pick and a 2027 pick of equal EV are treated equally.
- No surplus value yet (production is not netted against rookie-scale salary).
- Team projections in the Markov Chain do not account for age, roster continuity, salary cap space, draft picks, injuries, etc.
- Deeply nested multi-team swap chains are encoded as close approximations.
- Win Shares is hardly a perfect metric, and other more advanced all-in-one statistics (EPM, RAPTOR, xRAPM, etc.) may be explored in the future.
## Acknowledgments
 
Builds on the draft-value-curve lineage from Barzilai (82games), Pelton (ESPN WARP charts), and Goldstein (PIPM); the 3-2-1-specific re-pricing work of Nick Thoreson and Luke McCartney; and the option-pricing template of Foster & Binns, *Valuing Protections on NBA Draft Picks* (MIT Sloan, 2019).
