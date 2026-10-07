<script lang="ts">
  // The Methodology tab of app.R: six collapsible sections, the first two open.
  import PlotlyChart from "../components/PlotlyChart.svelte";
  import { loadMethodology } from "../lib/data";
  import {
    lotteryBarFigure, lotteryLineFigure, pickCurveFigure, rankHeatmapFigure, rankHorizonFigure,
    tierHeatmapFigure, type IntervalChoice,
  } from "../lib/methodologyCharts";

  const load = loadMethodology();

  let matrixType = $state<"tier" | "rank">("tier");
  let startRank = $state<string>("all");
  let interval = $state<IntervalChoice>("10_90");

  const rankChoices = Array.from({ length: 30 }, (_, i) => i + 1);

  // One decimal, as in the model export (e.g. -2.0, 30.4).
  const f1 = (x: number) => x.toFixed(1);
</script>

{#await load}
  <p class="status">Loading methodology…</p>
{:then m}
  {@const curve = pickCurveFigure(m)}
  {@const tier = tierHeatmapFigure(m)}
  {@const rankMat = rankHeatmapFigure(m)}
  {@const lotLine = lotteryLineFigure(m)}
  {@const lotBar = lotteryBarFigure(m)}
  {@const horizon = rankHorizonFigure(m, startRank === "all" ? "all" : Number(startRank), interval)}
  {@const vm = m.value_metric}
  {@const glossary = [
    ["EPV", `Expected Pick Value: the average ${m.value_outcome} for a draft asset.`],
    [m.value_outcome, vm.is_xrapm
      ? `xRAPM wins above replacement (${f1(vm.replacement)} baseline) over the four rookie-contract seasons after the draft.`
      : "Basketball-Reference Win Shares over a player's first four NBA seasons."],
    ["Replacement level", "The level of a freely available player (minimum salary or outside a normal rotation): -2.0 points per 100 possessions relative to league average. Playing time below it subtracts value."],
    ["Conveyance", "Whether a traded pick actually transfers to the receiving team after protections and conditions are applied."],
    ["Protection", "A condition that lets the original team keep the pick in certain ranges, such as top-4 or lottery protected."],
    ["Swap right", "The right to exchange picks with another team when the swap holder's outcome is better."],
    ["Relegation", "The three worst teams overall. Under 3-2-1 they receive two lottery balls and cannot fall past pick 12."],
    ["Non-Play-In", "Non-relegated teams that miss the play-in. Under 3-2-1 they receive three balls."],
    ["9/10 Seeds", "The four conference 9- and 10-seeds. Under 3-2-1 they receive two balls."],
    ["7v8 Losers", "The two teams that lose the 7-vs-8 play-in games. Under 3-2-1 they receive one ball."],
    ["Playoff", "The 14 playoff teams, ordered after the lottery teams for draft-position purposes."],
  ]}

  <div class="sections">
    <details open>
      <summary>Methodology Overview</summary>
      <div class="body grid3">
        <section class="card method-card">
          <h3 class="card-header">Expected Pick Value</h3>
          <div class="card-body">
            <p>Expected Pick Value (EPV) is the expected value of a draft asset before the player is known. Every future pick is run through simulated team trajectories, lottery draws, protections, swaps and conveyance rules, and the resulting draft slot is valued with a Bayesian pick-value curve. EPV separates the value of the asset from the outcome of any one player's career.</p>
          </div>
        </section>
        <section class="card method-card">
          <h3 class="card-header">Value Metric</h3>
          <div class="card-body">
            <!-- Edit the wording freely. Values in {braces} come from the model export. -->
            {#if vm.is_xrapm}
              <p>Picks are valued by what players drafted in each slot produced during their rookie contracts: xRAPM wins above replacement over the four seasons after the draft ({vm.draft_years[0]}-{vm.draft_years[1]} draft classes). Seasons a player misses count as zero.</p>
              <p>xRAPM (xrapm.com) is a plus-minus rating that combines lineup data with a box-score and play-by-play prior. Each season's value is (xRAPM + {f1(-vm.replacement)}) × possessions ÷ 100 ÷ {f1(vm.points_per_win)} points per win, so a {f1(vm.replacement)} player (about the level of a minimum-salary or end-of-rotation player) adds nothing. Lockout and COVID seasons are scaled to 82 games.</p>
            {:else}
              <p>Picks are valued by Basketball-Reference Win Shares over each player's first four NBA seasons.</p>
            {/if}
          </div>
        </section>
        <section class="card method-card">
          <h3 class="card-header">The 3-2-1 Rule</h3>
          <div class="card-body">
            <p>The 3-2-1 system gives 16 teams lottery balls by competitive tier: three balls for non-play-in teams, two for the three relegation teams and the 9/10 play-in seeds, and one for the 7v8 play-in losers. The model also applies the anti-tank rules and the relegation floor, then compares each team's portfolio against simulated outcomes from the old lottery rules on the same projected seasons.</p>
          </div>
        </section>
      </div>
    </details>

    <details open>
      <summary>Draft Pick Value Curve</summary>
      <div class="body">
        <section class="card chart-card">
          <PlotlyChart data={curve.data} layout={curve.layout} height={(w) => (w < 640 ? 520 : 440)}
                       label="Draft pick value curve for picks 1 to 60" />
        </section>
      </div>
    </details>

    <details>
      <summary>Team-strength Model</summary>
      <div class="body grid2">
        <section class="card">
          <div class="card-header with-controls">
            <h3>Transition Matrix</h3>
            <select class="select compact" bind:value={matrixType} aria-label="Matrix type">
              <option value="tier">Tier Matrix</option>
              <option value="rank">30 Rank</option>
            </select>
          </div>
          {#if matrixType === "rank" && rankMat}
            <PlotlyChart data={rankMat.data} layout={rankMat.layout} height={(w) => (w < 640 ? Math.min(w + 40, 460) : 390)}
                         label="30-rank transition matrix" />
          {:else if matrixType === "rank"}
            <p class="note">A valid 30 x 30 rank transition matrix was not found in dashboard_data.rds.</p>
          {:else}
            <PlotlyChart data={tier.data} layout={tier.layout} height={390} label="Tier transition matrix" />
          {/if}
        </section>
        <section class="card">
          <div class="card-header with-controls">
            <h3>Seven-Year Rank Trajectory</h3>
            <div class="controls">
              <select class="select compact" bind:value={startRank} aria-label="Starting rank">
                <option value="all">All ranks</option>
                {#each rankChoices as r}<option value={String(r)}>Rank {String(r).padStart(2, "0")}</option>{/each}
              </select>
              <select class="select compact" bind:value={interval} aria-label="Interval">
                <option value="40_60">[40%, 60%]</option>
                <option value="25_75">[25%, 75%]</option>
                <option value="10_90">[10%, 90%]</option>
              </select>
            </div>
          </div>
          <PlotlyChart data={horizon.data} layout={horizon.layout} height={(w) => (w < 640 ? 460 : 390)}
                       label="Seven-year rank trajectory" />
        </section>
      </div>
    </details>

    <details>
      <summary>Lottery Odds</summary>
      <div class="body grid2">
        <section class="card">
          <h3 class="card-header">Expected Pick Position by Lottery Seed</h3>
          <PlotlyChart data={lotLine.data} layout={lotLine.layout} height={(w) => (w < 640 ? 440 : 640)}
                       label="Expected pick position by lottery seed" />
        </section>
        <section class="card">
          <h3 class="card-header">Probability of #1 Pick by Seed (%)</h3>
          <PlotlyChart data={lotBar.data} layout={lotBar.layout} height={(w) => (w < 640 ? 440 : 640)}
                       label="Probability of the first pick by seed" />
        </section>
      </div>
    </details>

    <details>
      <summary>Model Validation &amp; Diagnostics</summary>
      <div class="body">
        <div class="validation" role="table" aria-label="Model validation checks">
          <div class="v-row v-head" role="row">
            <span role="columnheader">Model</span>
            <span role="columnheader">Check</span>
            <span role="columnheader">Metric</span>
            <span role="columnheader">Use Case</span>
            <span role="columnheader" class="c">Status</span>
          </div>
          {#each m.validation.rows as r}
            <div class="v-row" role="row">
              <span class="v-model" role="cell">{r.model}</span>
              <span class="v-check" role="cell">{r.check}</span>
              <span class="v-metric" role="cell">{r.metric}</span>
              <span class="v-why" role="cell">{r.why}</span>
              <span class="c" role="cell">
                <span class="badge" class:pass={r.status === "PASS"} class:check={r.status === "CHECK"}>{r.status}</span>
              </span>
            </div>
          {/each}
        </div>
        <p class="note">{m.validation.note}</p>
      </div>
    </details>

    <details>
      <summary>Glossary</summary>
      <div class="body">
        <dl class="glossary">
          {#each glossary as [term, def]}
            <div><dt>{term}</dt><dd>{def}</dd></div>
          {/each}
        </dl>
      </div>
    </details>
  </div>
{:catch err}
  <p class="status error" role="alert">Could not load the methodology data: {err.message}</p>
{/await}

<style>
  .status { padding: 60px 0; text-align: center; color: var(--muted); font-size: 16px; }
  .error { color: #fca5a5; }
  .sections { display: flex; flex-direction: column; gap: 10px; }

  details {
    background: var(--surface);
    border: 1px solid var(--border);
    border-radius: var(--radius);
  }
  summary {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 14px 18px;
    font-weight: 800;
    font-size: 1.05rem;
    color: var(--text-strong);
    cursor: pointer;
    list-style: none;
    border-radius: var(--radius);
  }
  summary::-webkit-details-marker { display: none; }
  summary::after {
    content: "";
    margin-left: auto;
    width: 9px;
    height: 9px;
    border-right: 2px solid #c9ccd6;
    border-bottom: 2px solid #c9ccd6;
    transform: rotate(45deg);
    transition: transform 0.15s;
  }
  details[open] > summary::after { transform: rotate(-135deg); }
  summary:hover { background: rgba(255, 255, 255, 0.02); }
  summary:focus-visible { outline: 2px solid var(--link); outline-offset: -2px; }
  .body { padding: 4px 16px 16px; }

  .grid3 { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 14px; }
  .grid2 { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 14px; }
  @media (max-width: 1000px) {
    .grid3, .grid2 { grid-template-columns: minmax(0, 1fr); }
  }

  h3 { margin: 0; font-size: 14px; }
  h3.card-header { font-size: 14px; }
  .method-card p { margin: 0 0 10px; color: #cfd2dc; line-height: 1.65; font-size: 14px; }
  .method-card p:last-child { margin-bottom: 0; }
  .chart-card { padding: 6px 4px; }
  .with-controls {
    display: flex;
    align-items: center;
    justify-content: space-between;
    flex-wrap: wrap;
    gap: 8px 12px;
  }
  .controls { display: flex; gap: 8px; flex-wrap: wrap; }
  .select.compact { width: auto; min-width: 140px; min-height: 34px; padding-top: 4px; padding-bottom: 4px; font-size: 13px; }
  .note { margin: 10px 2px 0; font-size: 12px; color: #888; }

  .validation { font-size: 12px; line-height: 1.6; }
  .v-row {
    display: grid;
    grid-template-columns: 16% 15% 35% 26% 8%;
    border-bottom: 1px solid rgba(255, 255, 255, 0.035);
  }
  .v-row > * { padding: 7px; min-width: 0; overflow-wrap: anywhere; }
  .v-head { color: #cfd2dc; font-weight: 700; border-bottom-color: #1a1a2a; }
  .v-model { color: #e6e8ef; font-family: var(--mono); }
  .v-check { color: #d7d8e2; }
  .v-metric, .v-why { color: #b8bcc9; }
  .v-metric { font-family: var(--mono); }
  .c { text-align: center; }
  .badge { font-weight: 800; color: #9ca3af; }
  .badge.pass { color: #10b981; font-weight: 900; }
  .badge.check { color: #ef4444; font-weight: 900; }
  @media (max-width: 760px) {
    .v-head { display: none; }
    .v-row { grid-template-columns: 1fr auto; padding: 8px 0; }
    .v-row > * { padding: 2px 4px; }
    .v-model { grid-column: 1; font-weight: 700; }
    .v-row .c { grid-column: 2; grid-row: 1; text-align: right; }
    .v-check, .v-metric, .v-why { grid-column: 1 / -1; }
    .v-check { color: var(--text-strong); }
  }

  .glossary {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 9px 18px;
    margin: 6px 0 0;
  }
  .glossary dt { color: #f4f4f8; font-weight: 850; }
  .glossary dd { margin: 0; color: #b8bcc9; }
  @media (max-width: 900px) { .glossary { grid-template-columns: 1fr; } }
  @media (max-width: 560px) {
    summary { padding: 12px 14px; }
    .body { padding: 4px 10px 12px; }
  }
</style>
