# Static web dashboard

A static rebuild of the Shiny dashboard (`../app.R`) that runs entirely in the
browser. It is a Vite + Svelte + TypeScript site deployed to GitHub Pages, with
all four tabs of the Shiny app: Summary (`#/summary`), Team
(`#/team/portfolios`, `#/team/landscape`, `#/team/single/<pick id>`), Trade
Machine (`#/trade`), and Methodology (`#/methodology`). `app.R` remains the
reference implementation.

The Trade Machine draws its charts as plain SVG. The other tabs use Plotly
(cartesian bundle), which is split into its own file and downloaded only when a
Plotly tab is opened.

## How it fits together

1. `04_lotterySims.R` writes `01_data/dashboard_data.rds` (unchanged).
2. `06_web_export.R` (repo root) loads `app.R`'s prepared objects without
   launching the app and writes compact files to `web/public/data/`:
   - `meta.json`: picks, teams, labels, block offsets
   - `core.bin`: per-simulation pick-value curves and projected team slots
   - `teams/<ABB>.bin`: draws for the picks each team owns, fetched on demand
   - `methodology.json`: the tables behind the Methodology charts, plus the
     value-metric text and validation table read from `app.R`'s rendered output
   - `summary.json`: Summary headline numbers, team portfolios for every
     year / round filter (with the scatterplot's logo positions), per-pick EPV
     changes, and the team table
   - `single/<ABB>.json`: Single pick details, conveyance / swap odds, and R's
     density curves for each pick a team owns, fetched on demand
3. `src/lib/trade.ts` ports the Trade Machine valuation from `app.R`'s server
   functions, one simulation at a time and in the same row order.
4. `scripts/parity_reference.R` runs reference trades through `app.R`'s own
   server code (via `shiny::testServer`) and saves `tests/parity_reference.json`;
   `npm test` recomputes them in TypeScript and requires a match.

## Commands

Run R scripts from the repository root and npm commands from `web/`.

```bash
Rscript 06_web_export.R
```

```bash
Rscript web/scripts/parity_reference.R
```

```bash
npm install
```

```bash
npm run dev
```

```bash
npm test
```

```bash
npm run build
```

After any change to the data or the valuation logic, rerun the export, then the
parity reference, then `npm test`. The test fails if the reference was built
from a different export.

## Deployment

`.github/workflows/deploy-web.yml` runs the checks, tests, and build on every
push to `master` that touches `web/`, then publishes `web/dist` to GitHub
Pages. One-time setup: in the repository's Settings → Pages, set the source to
"GitHub Actions".
