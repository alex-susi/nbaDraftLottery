# NBA 3-2-1 Draft Lottery: Bayesian Pick Valuation

A fully Bayesian pipeline for valuing NBA draft picks under the league's new **3-2-1 lottery format** (approved by the NBA Board of Governors in May 2026, effective for the 2027–2029 drafts). Every pick is valued with full posterior uncertainty, picks are re-priced under both the old and new lottery systems, and a Shiny trade machine lets you compare any deal with correlated within-simulation draws.

To my knowledge this is the first public implementation — in any language — of the official 37-ball, 16-team, relegation-floor lottery structure, paired with a rigorous Bayesian treatment rather than point-estimate value curves.

## What it does

- **Values every owned pick, 2026–2032**, under both the current and 3-2-1 lottery systems
- **Propagates full posterior uncertainty** from model parameters through to credible intervals on every pick and every trade
- **Models the new anti-tank rules** — relegation tier ball counts, the pick-12 floor, no consecutive #1s, no three-straight top-5 picks, and the 12–15 protection ban
- **Runs a trade machine** that values multi-pick deals (with hypothetical protections/swaps) using correlated draws, so realized outcomes can disagree with expected value ex post

## Methodology

**Pick value = first-4-year Win Shares** (the rookie-scale cost-controlled window), scraped from Basketball-Reference draft classes 1985–2021.

Three Stan models do the work:

- **Round 1 pick curve** (`pick_value_v3.stan`) — Student-t likelihood for fat draft-outcome tails, with a power-law mean curve `mu[p] = alpha / p^beta + gamma`. Per-pick residual variance follows a **log-scale random walk across adjacent slots**, a principled replacement for the ad-hoc neighbor-averaging in most public curves.
- **Round 2 pick curve** (`pick_play_r2_v6_declining_upside.stan`) — a **hurdle model** (most second-rounders never produce), with a shifted right-skewed lognormal mixture and a pick-declining rare-upside probability.
- **Team strength** (`team_strength.stan`) — a first-order **Dirichlet-Multinomial Markov chain** over the five 3-2-1 tiers (relegation → playoff). Tiers are chosen to *be* the lottery's seeding mechanism, so transitions map directly onto how picks get seeded.

A Monte Carlo engine then projects each team's tier forward, runs both lotteries, applies all protections/swaps/restrictions, and values every pick under each system.

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

## Running it

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

Run `05_model_validation.R` separately for the full validation suite (outputs to `./03_validation/`).

## Known limitations

- No time discounting — a 2031 pick and a 2027 pick of equal EV are treated equally
- No surplus value yet (production is not netted against rookie-scale salary)
- Far-future picks wash out to the league baseline by construction; read 2031–2032 valuations as base rates, not team-specific forecasts
- Deeply nested multi-team swap chains are encoded as close approximations
- Single-metric (Win Shares) caveats apply: era effects, injury luck, availability

## Acknowledgments

Builds on the draft-value-curve lineage from Barzilai (82games), Pelton (ESPN WARP charts), and Goldstein (PIPM); the 3-2-1-specific re-pricing work of Nick Thoreson and Luke McCartney; and the option-pricing template of Foster & Binns, *Valuing Protections on NBA Draft Picks* (MIT Sloan, 2019).
