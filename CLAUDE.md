# CLAUDE.md

Guidance for working in [alex-susi/nbaDraftLottery](https://github.com/alex-susi/nbaDraftLottery).

Based on `master` commit `fa3a39fc35cc34921fe37af7c7a99711cd7a5c5f`, inspected on 2026-09-28. Recheck the source when the repository changes. This is a source-based guide, not evidence that the pipeline or app passed a fresh runtime test.

## Project purpose

This R / Stan project values NBA draft picks in xRAPM wins above replacement over the four rookie-contract seasons after the draft, and compares the legacy lottery with the repository's 3-2-1 scenario. It combines Bayesian pick-value curves, future team-rank simulations, lottery outcomes, ownership obligations, and an interactive Shiny dashboard.

The checked-in configuration locks 2026 draft slots and projects 2027–2032. The README dates its ownership data to 2026-06-22. Treat these as snapshot assumptions; verify public sources before updating dates, lottery rules, or trade obligations.

Two value modes must remain distinct:

- **Expected Pick Value (EPV):** conditional expected player value for each simulated slot and ownership outcome, using that simulation's posterior curve draw. It retains model, standings, lottery, and conveyance uncertainty.
- **4-Yr xRAPM WAR Outcomes:** predictive player outcomes, including player-level variation and second-round structural zeroes.

Since the 2026-10-03 rebuild, names containing `war` (`war4`, `war_mean`, `expected_war`) hold xRAPM wins above replacement. Caches built before that hold Win Shares; the app detects this through `dd$metadata$value_metric` and labels accordingly. The WS-era Stan files are archived in `02_models/zz_Archive/20260928/`.

## Where to work

| Path | Role |
| --- | --- |
| `app.R` | Shiny entry point; cache loading, compatibility handling, display assets, UI, server logic, and Trade Machine. Four top-level tabs (`main_nav`): Summary, Team (sub-tabs `team_view`: portfolios / landscape / single pick), Trade Machine, Methodology (accordion). Pick links call `Shiny.setInputValue('goto_single_pick', id)` to open Single pick. Unit labels come from `VALUE_UNIT` / `VALUE_OUTCOME`, set from `dd$metadata$value_metric`. |
| `00_helpers.R` | Every reusable function (called from two or more places), sourced by `01_data.R`. Large headers group functions by the script that first calls them; section headers match that script's sections. |
| `01_data.R` | Packages, sources `00_helpers.R`, configuration, standings scraping, team names, historical draft-slot cache (fills missing classes only), and the xRAPM wins-above-replacement outcome built from `01_data/xrapm_cache.rds`, `01_data/possessions_cache.rds`, and `01_data/Advanced.csv`. |
| `02_picks.R` | Locked 2026 orders; explicit future obligations; internal and display asset registries. |
| `03_models.R` | Rank transitions, kernel baseline, Stan fits, inline diagnostics/PPC/LOO, and posterior extraction. `sample_pick_value()`, `draw_r1_outcomes()`, and `draw_r2_outcomes()` are in `00_helpers.R`. |
| `04_lotterySims.R` | Monte Carlo (rank resolution, anti-tank restrictions, and second-round slots are inline in the loop; lottery draws and ownership allocation call `00_helpers.R`), output checks, summaries, and dashboard export. |
| `05_model_validation.R` | Extended MCMC/PPC/LOO/SBC and rank-calibration checks. Has compatibility issues described below. |
| `06_web_export.R` | Loads `app.R`'s prepared objects (without launching it) and writes the static site's data to `web/public/data/`. |
| `web/` | Static Vite + Svelte + TypeScript rebuild of all four dashboard tabs, deployed to GitHub Pages by `.github/workflows/deploy-web.yml`. `src/lib/trade.ts` ports the Trade Machine valuation; `npm test` checks it against `app.R` results saved by `web/scripts/parity_reference.R`. See `web/README.md`. `app.R` stays the reference; do not edit it for the web build. |
| `02_models/picks_Round1.stan` | Active first-round model: ex-Gaussian with a strictly decreasing pooled-gap mean curve. |
| `02_models/picks_Round2.stan` | Active second-round hurdle: Bernoulli play probability + strictly decreasing pooled-gap EV curve + fringe/contributor played outcomes. |
| `02_models/team_strength_v3.stan` | Active 30-rank smoothed-softmax transition model. |
| `01_data/dashboard_data.rds` | Cached data used by the app; produced by `04_lotterySims.R`. |
| `03_validation/` | Saved diagnostic tables, figures, and results. The app separately reads `validation_decision_table_latest_models.csv` here. |
| `README.md` | Public methodology, assumptions, results, and dashboard links. |
| `nbaDraftLottery.Rproj` | RStudio project; two-space indentation and UTF-8. |
| `rsconnect/` | Existing shinyapps.io deployment metadata. |

Top-level files are the working implementation. `zz_Archive/` contains older work; the WS-era Stan files are in `02_models/zz_Archive/20260928/`. `03_models.R` always fits `02_models/zz_Archive/picks_Round1_gaussian_xrapm.stan` and `picks_Round1_student_t_xrapm.stan` as LOO benchmarks, and, when `FIT_R1_COMPARISON_MODELS = TRUE`, `picks_Round1_lognormal_mixture_xrapm.stan` plus the WS-prior `picks_Round1_0601.stan` / `picks_Round1_0605.stan`. Keep those dependencies when changing the fitting script.

There is no checked-in R package manifest, `renv.lock`, or R test suite. Do not invent package-style build/test commands for the R code. The only tests and CI are for `web/` (`npm test`, `.github/workflows/deploy-web.yml`).

## Running the project

Run from the repository root.

### Launch the cached dashboard

App packages: `shiny`, `plotly`, `DT`, `bslib`, and `tidyverse`; `igraph` is optional. The app also calls `htmlwidgets`, normally installed through its dependencies.

```bash
Rscript -e 'shiny::runApp(".")'
```

The app first looks for `01_data/dashboard_data.rds`, then `dashboard_data.rds` at the root. It does not fit Stan models or run the data pipeline on startup. Use this route for UI work.

### Rebuild data and simulations

The scripts share objects in one R session. Their intended order is:

```r
source("01_data.R")
source("02_picks.R")
source("03_models.R")
source("04_lotterySims.R")
```

This is the dependency order, not a verified clean-build recipe: resolve the existing export issue below before a full run. Separate `Rscript 01_data.R`, `Rscript 02_picks.R`, etc. processes will lose the shared objects.

The pipeline loads `tidyverse`, `rvest`, `httr`, `cmdstanr`, `janitor`, `posterior`, `expm`, `loo`, `hoopR`, and `dplyr`. It needs a working CmdStan installation and compiler. Although it checks `hoopR_available`, it first calls `library(hoopR)`, so that package is required as written. Extended validation also requires `bayesplot` and `jsonlite`.

`01_data.R` scrapes standings, reads `Advanced.csv`, and uses `draft_slots_cache.rds` when available. `02_picks.R` uses explicit ownership tables; its comments about a live ownership scrape do not describe the active implementation.

Defaults in `01_data.R`: seed 2026, `N_SIMS = 10000`, `N_LOT = 50000`, standings history 2005–2026, draft classes 2015–2022 (tracking era; xRAPM itself starts in 1996–97), `XRAPM_REPLACEMENT = -2.0`, `POINTS_PER_WIN = 82 / 2.7`. Avoid the expensive fitting path for changes that only affect presentation.

### Extended validation

After reconciling its inputs and Stan names, run `source("05_model_validation.R")` in the same session as the fits. It can otherwise restart scraping and fitting because `RUN_PIPELINE_IF_NEEDED = TRUE`.

Its defaults enable 50 first-round SBC refits; second-round and Markov SBC are off. Review those controls before a diagnostic run. The actual output folder is `03_validation/`, despite an older folder name in its header.

## Model contracts

### Historical outcomes and pick curves

`01_data.R` builds one B-Ref row per player-season (preferring `2TM` / `3TM` aggregate rows), joins xRAPM and possessions by season and normalized name (~99.3% of player-seasons; ambiguous names and unmatched seasons fall back to 2.7 x VORP), and converts each season to wins: `(xRAPM - XRAPM_REPLACEMENT) * poss / 100 / POINTS_PER_WIN`, scaled to 82 games for shortened seasons. It sums seasons draft+1..draft+4 per drafted player (missed seasons = 0; first-rounders who never played are kept as zeros) and marks second-round `played` using positive minutes inside that window. Every draft row since 1996 has a B-Ref id; the name fallback only accepts a player whose debut falls inside the window.

Preserve negative values and true no-play zeroes. Do not replace failed joins with assumed no-play observations without checking data coverage. Changing the observation window, replacement level, or points-per-win changes the target and requires a model/prior review.

First round, picks 1–30:

- `war4 ~ exp_mod_normal(mu - tau_x, sigma_g, 1 / tau_x)`: ex-Gaussian parameterized so `mu[p]` is exactly the slot mean (EPV). It ties the previous Gaussian on held-out mean error and beats it on held-out log density in leave-one-draft-class-out CV (`03_validation/r1_likelihood_cv.R`). The Gaussian (`zz_Archive/picks_Round1_gaussian_xrapm.stan`), Student-t (below slot means) and lognormal mixture (overstates top picks) are kept as comparisons.
- `mu` is strictly decreasing in every draw by construction (user requirement: pick p always beats p + 1; uncertainty is only in the gaps). `mu[30] = alpha / 30^beta + gamma`, and each gap is the power-law gap times `exp(eps[p])`, with `eps` a centered random walk scaled by `tau_eps`. Keep this monotone construction when changing the curve.
- Total SD (`sd_pick`) and skew share (`r_skew`, capped at 0.99) follow second-order random walks (initial slopes `d_sd`, `d_r`; slope changes scaled by `tau_sd`, `tau_r`), so the outcome band follows the broad decline rather than small-sample bumps; `tau_x = sd_pick * r_skew`, `sigma_g = sd_pick * sqrt(1 - r_skew^2)`. Fit with `adapt_delta = 0.99` and `init = r1_inits` (one prior-mean starting list per chain): random inits gave a chain stuck at the treedepth limit, and 0.95 left divergences. The first-order version is `zz_Archive/picks_Round1_exgauss_rw1_xrapm.stan`.
- `war4_pred` holds means and `war4_pred_sd` the total SD. `draw_r1_outcomes(draw_idx, pk)` draws ex-Gaussian outcomes from the same posterior draws, for `sample_pick_value()`, `pick_outcome_draws` (`OUTCOME_REPS_PER_DRAW` outcomes per draw, display only), and the R1 PPC.
- LOO alone does not qualify a model for EPV; check held-out mean bias too.

Second round, picks 31–60:

- `played ~ Bernoulli(pi_p)`; non-playing outcomes are exactly zero.
- `ev` is strictly decreasing in every draw: `ev[1] = ev_31`, then positive gaps `exp(log_gap + eps)` with `eps` a centered random walk. `mean_play = ev / pi_p`.
- Played outcomes are a fringe/contributor mixture: `Normal(fringe_mean, sd_fringe)`, plus `Exponential(mean upside_mean)` with probability `p_contrib`. `fringe_mean = mean_play - p_contrib * upside_mean` is derived so the played mean stays exact. `p_contrib`, `upside_mean`, and `sd_fringe` follow adjacent-pick random walks with drift (`tau_q`, `tau_T`, `tau_s0`). In draft-class CV this beat log-linear trends (`zz_Archive/picks_Round2_mixture_linear_xrapm.stan`) and quadratic trends (`zz_Archive/picks_Round2_mixture_curved_xrapm.stan`) on held-out log density and late-pick calibration; the band is less smooth than the trend versions. A second-order walk on `sd_fringe` only (`zz_Archive/picks_Round2_mixture_rw_s0rw2_xrapm.stan`) smoothed the band but lost ~23 held-out log-density points (2026-10-05), so keep the first-order fringe walk unless new data change that. The ex-Gaussian (`zz_Archive/picks_Round2_exgauss_xrapm.stan`) understated EV because its exponential tail is too light; the Gaussian hurdle is `zz_Archive/picks_Round2_gaussian_xrapm.stan`. All are compared in `03_validation/r2_likelihood_cv.R`.
- `pi_p` uses a logit random walk with drift and is not forced to decrease; only `ev` is.
- Stan uses local index `pick - 30`. Fit with `init = r2_mixture_inits` (one prior-mean starting list per chain, with small jitter; even `init = 0.5` stranded a CV chain with a collapsed step size because steep EV gaps put the derived fringe level far from the data) and `adapt_delta = 0.999` (0.99 left a few divergences at the sharp late-pick geometry). The CV script carries its own copy of the init function.
- `sd_play` and `ev_sd` are generated quantities. `draw_r2_outcomes(draw_idx, pk)` returns zero if not played, else a fringe/contributor draw; `pick2_outcome_draws` and the R2 PPC use the same process.

### Team strength

Production uses `team_strength_v3.stan`: a 30 × 30 multinomial transition surface with softmax probabilities, a destination effect, a non-positive distance slope, and local deviations smoothed across rows and columns. Smoothing scales are fixed data inputs in this version.

`rank_worst = 1` means worst record and `30` means best. The Methodology UI reverses this convention for display with `rank_display_matrix()` and `rank_display_to_model()`. Never reverse the stored matrices to fix a plot label.

The five tiers are derived display/validation groups. The old five-tier Dirichlet model and `rank_kernel_transition_matrix` are benchmarks, not the transition matrix drawn in the main Monte Carlo loop.

Inside the `04_lotterySims.R` Monte Carlo loop, each team draws a desired rank, then a sort with a random tiebreaker resolves ties into a 1:30 permutation. Future tier/play-in labels use league-rank bands; the simulator does not model conference play-in games explicitly. Matrix-power rank projections and the joint standings simulator therefore need not have identical marginal distributions.

## Lottery and ownership rules

These describe the checked-in implementation, not a fresh verification of league rules.

| Component | Contract |
| --- | --- |
| Legacy lottery | 14 teams; draw the first four picks, then assign the remaining slots in seed order. |
| 3-2-1 lottery | 16 teams, with balls per seed `2,2,2,3,3,3,3,3,3,3,2,2,2,2,1,1`; draw all 16. Seeds 1–3 cannot fall below pick 12. |
| History restrictions | No consecutive No. 1 picks or three consecutive top-five picks. Apply to the **original team**, before ownership allocation, and preserve a slot permutation. |
| Second round | Legacy: inverse record, slots 31–60. New scenario: lottery teams get `47 - final_first_round_slot`; remaining teams get 47–60 by inverse record. |
| 2026 | Slots are fixed in both systems, but pick-curve and player-outcome uncertainty remain. Current/new draws for these assets must match. |

Keep original-team slots separate from owners. Resolve each original pick to one owner per simulation/year/round. Preserve retained-own legs, swap-return legs, ranked pools, and obligation state across years.

Start ownership changes in `02_picks.R`, then trace `resolve_pick_owners()` and `resolve_second_pick_owners()` in `00_helpers.R`, which hold the simple and complex obligation logic. Update display grouping and labels in `app.R` when needed. Editing a label alone does not change allocation.

`pick_assets` contains internal conditional legs. `pick_display_assets` and `pick_display_members` represent economic pick entitlements shown to users. Counting internal rows as picks double-counts swap/pool structures. Preserve the duplicate-own-row checks for cases such as HOU/BKN 2027 and BKN's 2028 ranked pool.

Use explicit conveyance indicators for pick counts and probabilities. A pick that conveys may produce zero or negative value.

## Export and Trade Machine contracts

`04_lotterySims.R` is the producer and `app.R` is the consumer of `dashboard_data.rds`. Check both when changing an export field.

| Export | Shape / meaning |
| --- | --- |
| `asset_*_draws` | Rows are retained simulations; columns identify internal `asset_id` values. Distinguish outcome, EV, slot, raw-value, own-slot, and conveyance stores. |
| `display_asset_*_draws`, `display_convey_*_draws` | Same simulation rows, grouped by `display_asset_id`. |
| `team_slot*_draws`, `team_rank_worst_draws` | Simulation × team × projected year, with dimnames. First- and second-round arrays are distinct. |
| `sim_curve_par_draws` | Same simulation rows; includes `mu_1`–`mu_60`, `sigma_1`–`sigma_60`, and second-round play/conditional-mean fields. |
| `summary`, `summary_ev`, `pick_curve` | Outcome and EV summaries; pick curve must cover 1–60. `transition_matrix` is the five-tier view; `rank_transition_matrix` is 30 × 30. |

The export retains up to 2,000 draws using one shared `keep_idx`. Preserve row alignment across every matrix and array; never shuffle or resample trade components separately. Retain `drop = FALSE` when subsetting matrices.

`slot_value_vec()` uses each simulation's `mu_*` fields. A hypothetical swap's EPV is the eligible positive gain over the receiver's own pick, not the full value of the better pick. Preserve fixed-slot valuation for 2026, distinguish swap exercise from guaranteed pick receipt, and check both trade sides' signs.

App compatibility fallbacks support older caches. Keep them deliberate; do not let an old fallback silently replace current second-round values or conveyance flags.

## Existing issues to check before a rebuild

These findings come from the inspected source. Recheck them before acting; do not treat saved diagnostics as proof that the current scripts run cleanly.

1. **Resolved: undefined export object.** `roster_info` (an unused placeholder) is no longer exported, and `03_models.R` now builds `pick_loo_compare_tbl` / `pick_loo_summary`.
2. **Validation names have drifted.** `05_model_validation.R` expects `pick_fit` and `pick_fit_r2_hurdle`, while `03_models.R` creates `fit_r1_v3` and `fit_r2`. It also asks for old Stan variables: R1 `war_pred_sd` is now `war4_pred_sd` (the ex-Gaussian total SD), the outcome is `war4` not `ws4`; R2 `played_rep`, `p_play`, and `cond_mean_ws` correspond to `play_rep`, `pi_p`, and `mean_play`; there is no longer an upside probability, and R2 `y_rep` is built in R (`war4_rep_mat_r2`). Reconcile consumers and SBC assumptions; adding fit aliases alone is insufficient.
3. **Rolling-origin input is missing.** Validation reads `rank_transitions$season_next`, but the final `transmute()` in `03_models.R` drops that column. Confirm that any reported validation has nonempty held-out rows.
4. **Protection coercion needs review.** `02_picks.R` converts protections with floors 12–15 to `top10`. This includes `lottery` (14) on encoded obligations. Check historical terms, allocation helpers, and labels together before changing this behavior.
5. **Comments and outputs can be stale.** App messages mention nonexistent root `nba_lottery.R`; validation comments mention `*_rank_v3.R` filenames. Some labels say 3-2-1 applies in 2027–2029, but the loop applies the new scenario through 2032. Saved validation tables include older model labels and an `Inf` SBC summary; inspect underlying results before reporting a pass.

## Validation after changes

1. **Parse edited R files without executing them.** For the active scripts:
   ```bash
   Rscript -e 'invisible(lapply(c("00_helpers.R", "01_data.R", "02_picks.R", "03_models.R", "04_lotterySims.R", "05_model_validation.R", "app.R"), parse))'
   ```
2. **For simulation/export changes**, run the relevant pipeline after resolving dependencies. Keep `validate_round_aware_outputs()` enabled. Check slot ranges, rank permutations, second-round inversion, one owner per original pick, fixed 2026 equality, and draw alignment.
3. **For model changes**, inspect Rhat, bulk/tail ESS, divergences, treedepth, E-BFMI, PPCs, and Pareto-k. The extended script sets Rhat ≤ 1.01, ESS ≥ 400, and E-BFMI ≥ 0.30. Require finite results and meaningful sample counts. Row-level Markov LOO is not the same as held-out season prediction.
4. **For UI/trade changes**, launch the cached app and exercise affected tabs, both value modes, both rounds, and a fixed 2026 pick. Check a protected pick, a swap, and a ranked-pool asset when changing trade logic; verify counts, labels, intervals, and signs.
5. **Report what ran.** Separate syntax checks, cached-app checks, model refits, and full exports. Name blockers and untested behavior; do not claim a fresh fit or validation from checked-in CSVs.

## R style and scope

Use two spaces, `<-`, existing `%>%` pipelines, and explicit namespaces where ambiguity matters. Follow the surrounding organization and keep changes focused; do not reformat the entire large app for a small fix.

Keep the function name, opening parenthesis, and first argument together. Align later arguments with the first argument, and close the call on the final argument line. No line should end with an opening parenthesis or begin with a closing parenthesis.

```r
pick_summary <- pick_assets %>%
  group_by(owner, year, round) %>%
  summarise(n_assets = n(),
            .groups = "drop")
```

This example counts internal asset rows, not economic picks.

Functions called from two or more places go in `00_helpers.R`, under the large header for the script that first calls them and the matching section header, each with short bullet comments. Code used once runs inline in its script rather than as a single-use function. Use dplyr for data-frame wrangling. Keep model parameters, array names, and public labels consistent across producers and consumers.

Preserve reproducible seeds and distinguish smoke-run settings from final exports. Keep generated diagnostics/cache changes visible in the diff, and compile Stan from source rather than relying on tracked platform-specific `.exe` files. For methodology changes, update the README and app explanation alongside the implementation.

