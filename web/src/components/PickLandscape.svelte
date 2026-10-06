<script lang="ts">
  // Team > Pick landscape (output$impact_chart in app.R): the pick scatterplot
  // (click a logo to isolate a team) or the EPV leaderboard.
  import { leaderboardFigure, scatterFigure } from "../lib/teamCharts";
  import type { Summary, SummaryTeam } from "../lib/types";
  import PlotlyChart from "./PlotlyChart.svelte";

  let { s, teams }: { s: Summary; teams: Map<string, SummaryTeam> } = $props();

  let year = $state("All");
  let round = $state("All");
  let view = $state<"scatter" | "leaderboard">("scatter");
  let sort = $state<"delta" | "total">("total");
  let selectedTeam = $state<string | null>(null);

  // Any filter change clears an isolated team, as in app.R.
  $effect(() => {
    void year; void round; void view;
    selectedTeam = null;
  });

  const rows = $derived(s.portfolio[`${year}|${round}`] ?? []);
  const fig = $derived(view === "leaderboard" ? leaderboardFigure(rows, sort, teams) : scatterFigure(rows, selectedTeam, teams));
  const title = $derived(
    `${view === "leaderboard" ? "EPV Leaderboard" : "Pick Scatterplot"} — ` +
    `${year === "All" ? "All Years" : year}, ${round === "All" ? "All Rounds" : `Round ${round}`}`
  );
</script>

<div class="layout">
  <aside class="card sidebar">
    <label>Draft year
      <select class="select" bind:value={year}>
        <option value="All">All</option>
        {#each s.years as y}<option value={String(y)}>{y}</option>{/each}
      </select>
    </label>
    <label>Round
      <select class="select" bind:value={round}>
        <option value="All">All</option>
        <option value="1">Round 1</option>
        <option value="2">Round 2</option>
      </select>
    </label>
    <label>View
      <select class="select" bind:value={view}>
        <option value="scatter">Pick Scatterplot</option>
        <option value="leaderboard">EPV Leaderboard</option>
      </select>
    </label>
    {#if view === "leaderboard"}
      <label>Sort teams by
        <select class="select" bind:value={sort}>
          <option value="delta">Δ EPV (biggest movers)</option>
          <option value="total">Total 3-2-1 EPV</option>
        </select>
      </label>
    {/if}
    <button type="button" class="btn small" onclick={() => { year = "All"; round = "All"; selectedTeam = null; }}>Clear filters</button>
    <p class="explain">
      Pick Scatterplot shows 3-2-1 average EPV per pick against expected pick count. EPV Leaderboard compares each
      team's current EPV to new 3-2-1 EPV, with the logo placed at the 3-2-1 midpoint.
      {#if view === "scatter"}Click a logo to show that team alone; click it again to show all teams.{/if}
    </p>
  </aside>

  <section class="card">
    <h3 class="card-header">{title}</h3>
    <PlotlyChart data={fig.data} layout={fig.layout} logos={fig.logos} label={title}
                 height={(w) => (w < 640 ? (view === "leaderboard" ? 820 : 560) : 820)}
                 onPointClick={(team) => {
                   if (view !== "scatter") return;
                   const t = String(team);
                   selectedTeam = selectedTeam === t ? null : t;
                 }} />
  </section>
</div>

<style>
  .layout { display: grid; grid-template-columns: 285px minmax(0, 1fr); gap: 14px; align-items: start; }
  .sidebar { display: flex; flex-direction: column; gap: 14px; padding: 16px; }
  .sidebar label { display: flex; flex-direction: column; gap: 6px; font-weight: 700; color: #cfd2dc; }
  .btn.small { min-height: 36px; padding: 6px 14px; font-size: 13px; align-self: flex-start; }
  .explain { margin: 0; font-family: var(--mono); font-size: 11px; color: #888; line-height: 1.6; }
  h3 { margin: 0; font-size: 14px; }
  @media (max-width: 900px) {
    .layout { grid-template-columns: minmax(0, 1fr); }
    .sidebar { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; padding: 12px; }
    .explain { grid-column: 1 / -1; }
  }
</style>
