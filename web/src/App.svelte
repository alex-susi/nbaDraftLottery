<script lang="ts">
  import TradeMachine from "./components/TradeMachine.svelte";
  import { loadMetaAndCore } from "./lib/data";
  import { route, type Page, type TeamView } from "./lib/router.svelte";
  import { makeContext, type TradeContext } from "./lib/trade";
  import Methodology from "./pages/Methodology.svelte";
  import Summary from "./pages/Summary.svelte";
  import Team from "./pages/Team.svelte";

  const SHINY_URL = "https://alexsusi2298.shinyapps.io/nbaDraftLottery/";
  const NAV: { page: Page; label: string }[] = [
    { page: "summary", label: "Summary" },
    { page: "team", label: "Team" },
    { page: "trade", label: "Trade Machine" },
    { page: "methodology", label: "Methodology" },
  ];

  // Pages stay mounted after their first visit so their inputs survive tab switches.
  let visited = $state<Page[]>([]);
  // The Team tab reopens the sub-view used last.
  let lastTeamView = $state<TeamView>("portfolios");
  $effect(() => {
    if (!visited.includes(route.page)) visited = [...visited, route.page];
    if (route.page === "team") lastTeamView = route.teamView;
  });

  let tradeLoad: Promise<TradeContext> | null = $state(null);
  $effect(() => {
    if (visited.includes("trade") && !tradeLoad) {
      tradeLoad = loadMetaAndCore().then(({ meta, core }) => makeContext(meta, core));
    }
  });

  const href = (p: Page) => (p === "team" ? `#/team/${lastTeamView}` : `#/${p}`);
</script>

<header class="nav">
  <div class="nav-inner">
    <a class="brand" href="#/summary">NBA 3-2-1 Lottery Reform</a>
    <nav aria-label="Sections">
      {#each NAV as n (n.page)}
        <a href={href(n.page)} class:active={route.page === n.page} aria-current={route.page === n.page ? "page" : undefined}>{n.label}</a>
      {/each}
    </nav>
  </div>
</header>

<main>
  {#if visited.includes("summary")}
    <div hidden={route.page !== "summary"}><Summary /></div>
  {/if}
  {#if visited.includes("team")}
    <div hidden={route.page !== "team"}><Team /></div>
  {/if}
  {#if visited.includes("trade")}
    <div hidden={route.page !== "trade"}>
      {#if tradeLoad}
        {#await tradeLoad}
          <p class="status">Loading simulations…</p>
        {:then ctx}
          <TradeMachine {ctx} />
        {:catch err}
          <p class="status error" role="alert">Could not load the simulation data: {err.message}</p>
        {/await}
      {/if}
    </div>
  {/if}
  {#if visited.includes("methodology")}
    <div hidden={route.page !== "methodology"}><Methodology /></div>
  {/if}
  <footer>
    Draft-pick values under the current and 3-2-1 lotteries.
    Code and methodology: <a href="https://github.com/alex-susi/nbaDraftLottery" rel="noopener">GitHub repo</a>.
    Original <a href={SHINY_URL} rel="noopener">Shiny dashboard</a>.
  </footer>
</main>

<style>
  .nav {
    position: sticky;
    top: 0;
    z-index: 30;
    background: #0f0f1a;
    border-bottom: 1px solid var(--border);
  }
  .nav-inner {
    max-width: 1600px;
    margin: 0 auto;
    padding: 0 16px;
    min-height: 54px;
    display: flex;
    align-items: center;
    justify-content: space-between;
    flex-wrap: wrap;
    gap: 4px 18px;
  }
  .brand { font-weight: 800; font-size: 17px; color: var(--text-strong); text-decoration: none; }
  nav { display: flex; gap: 2px; overflow-x: auto; scrollbar-width: none; max-width: 100%; }
  nav a {
    padding: 15px 10px;
    color: var(--muted);
    text-decoration: none;
    font-weight: 600;
    white-space: nowrap;
    border-bottom: 2px solid transparent;
  }
  nav a:hover { color: var(--text-strong); }
  nav a.active { color: var(--text-strong); border-bottom-color: var(--text-strong); }
  main { max-width: 1600px; margin: 0 auto; padding: 14px 16px 28px; }
  .status { padding: 60px 0; text-align: center; color: var(--muted); font-size: 16px; }
  .error { color: #fca5a5; }
  footer { margin-top: 22px; font-size: 12px; color: var(--muted); text-align: center; }
  @media (max-width: 560px) {
    .nav-inner { padding: 0 12px; }
    nav a { padding: 10px 6px; font-size: 13.5px; }
    main { padding: 12px 12px 24px; }
  }
</style>
