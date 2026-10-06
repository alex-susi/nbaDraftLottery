<script lang="ts">
  // The Team tab of app.R: Team portfolios, Pick landscape, and Single pick.
  // Each view stays mounted after its first visit so its settings survive.
  import PickLandscape from "../components/PickLandscape.svelte";
  import SinglePick from "../components/SinglePick.svelte";
  import TeamPortfolios from "../components/TeamPortfolios.svelte";
  import { loadSummary } from "../lib/data";
  import { route, TEAM_VIEWS, type TeamView } from "../lib/router.svelte";
  import type { SummaryTeam } from "../lib/types";

  const load = loadSummary();
  const LABELS: Record<TeamView, string> = {
    portfolios: "Team portfolios",
    landscape: "Pick landscape",
    single: "Single pick",
  };

  let visited = $state<TeamView[]>([]);
  $effect(() => {
    if (route.page === "team" && !visited.includes(route.teamView)) visited = [...visited, route.teamView];
  });
</script>

<nav class="subnav" aria-label="Team views">
  {#each TEAM_VIEWS as v (v)}
    <a href={`#/team/${v}`} class:active={route.teamView === v} aria-current={route.teamView === v ? "page" : undefined}>{LABELS[v]}</a>
  {/each}
</nav>

{#await load}
  <p class="status">Loading teams…</p>
{:then s}
  {@const teams = new Map<string, SummaryTeam>(s.teams.map((t) => [t.abbr, t]))}
  {#if visited.includes("portfolios")}
    <div hidden={route.teamView !== "portfolios"}><TeamPortfolios {s} {teams} /></div>
  {/if}
  {#if visited.includes("landscape")}
    <div hidden={route.teamView !== "landscape"}><PickLandscape {s} {teams} /></div>
  {/if}
  {#if visited.includes("single")}
    <div hidden={route.teamView !== "single"}><SinglePick {s} {teams} /></div>
  {/if}
{:catch err}
  <p class="status error" role="alert">Could not load the team data: {err.message}</p>
{/await}

<style>
  .subnav {
    display: flex;
    gap: 4px;
    margin: -4px 0 14px;
    border-bottom: 1px solid var(--border);
    overflow-x: auto;
    scrollbar-width: none;
  }
  .subnav a {
    padding: 10px 12px;
    color: var(--muted);
    text-decoration: none;
    font-weight: 600;
    white-space: nowrap;
    border-bottom: 2px solid transparent;
    margin-bottom: -1px;
  }
  .subnav a:hover { color: var(--text-strong); }
  .subnav a.active { color: var(--text-strong); border-bottom-color: var(--primary); }
  .status { padding: 60px 0; text-align: center; color: var(--muted); font-size: 16px; }
  .error { color: #fca5a5; }
</style>
