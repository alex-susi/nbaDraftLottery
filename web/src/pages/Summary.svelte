<script lang="ts">
  // The Summary tab of app.R: headline, KPI tiles, EPV leaderboard, biggest
  // pick movers, and the full pick table.
  import AllPicksTable from "../components/AllPicksTable.svelte";
  import PlotlyChart from "../components/PlotlyChart.svelte";
  import TeamTag from "../components/TeamTag.svelte";
  import { loadSummary } from "../lib/data";
  import { deltaColor, fmt1, fmtDelta1, fmtSigned1, pctChange } from "../lib/format";
  import { pickHref } from "../lib/router.svelte";
  import { leaderboardFigure } from "../lib/teamCharts";
  import type { Summary, SummaryTeam } from "../lib/types";

  const load = loadSummary();
  let year = $state("All");
  let round = $state("All");
  let sort = $state<"delta" | "total">("delta");

  const teamMap = (s: Summary) => new Map<string, SummaryTeam>(s.teams.map((t) => [t.abbr, t]));
  const pct0 = (p: number) => `${(100 * p).toFixed(0)}%`;
</script>

{#await load}
  <p class="status">Loading summary…</p>
{:then s}
  {@const teams = teamMap(s)}
  {@const picksById = new Map(s.picks.map((p) => [p.id, p]))}
  {@const gain = teams.get(s.gain.team)}
  {@const loss = teams.get(s.loss.team)}
  {@const lb = leaderboardFigure(s.portfolio[`${year}|${round}`] ?? [], sort, teams)}

  <div class="summary">
    <section class="hero">
      <h2>What the 3-2-1 lottery does to draft-pick value</h2>
      <p class="headline">
        Under the 3-2-1 lottery, the <b>{gain?.name}</b> gain the most pick value
        (<b>{fmtSigned1(s.gain.delta)} EPV</b>, ahead in {pct0(s.gain.p_positive)} of simulations)
        and the <b>{loss?.name}</b> lose the most (<b>{fmtSigned1(s.loss.delta)} EPV</b>, behind in
        {pct0(1 - s.loss.p_positive)} of simulations). About <b>{fmt1(s.shifted)} EPV</b> moves between teams in
        total, and <b>{s.n_moved} of {s.n_future}</b> future picks change in value by 5% or more.
      </p>
      <p class="note">
        Expected pick value (EPV) is the expected {s.value_metric_desc}. Current = the legacy 14-team lottery;
        3-2-1 = the approved format, compared on the same simulated seasons. 2026 picks are locked to the actual
        draft order. See <a href="#/methodology">Methodology</a> or the
        <a href="https://github.com/alex-susi/nbaDraftLottery" target="_blank" rel="noopener noreferrer">GitHub repo</a>
        for details.
      </p>
    </section>

    <div class="kpis">
      <div class="kpi" style="--kpi-accent:#10b981">
        {#if gain?.logo}<img src={gain.logo} alt="" width="64" height="64" />{/if}
        <div>
          <div class="kpi-label">Biggest gain</div>
          <div class="kpi-value">{fmtSigned1(s.gain.delta)} EPV</div>
          <div class="kpi-sub">{gain?.name} · ahead in {pct0(s.gain.p_positive)} of simulations</div>
        </div>
      </div>
      <div class="kpi" style="--kpi-accent:#ef4444">
        {#if loss?.logo}<img src={loss.logo} alt="" width="64" height="64" />{/if}
        <div>
          <div class="kpi-label">Biggest loss</div>
          <div class="kpi-value">{fmtSigned1(s.loss.delta)} EPV</div>
          <div class="kpi-sub">{loss?.name} · behind in {pct0(1 - s.loss.p_positive)} of simulations</div>
        </div>
      </div>
      <div class="kpi" style="--kpi-accent:#8b5cf6">
        <div>
          <div class="kpi-label">Picks that move</div>
          <div class="kpi-value">{s.n_moved} of {s.n_future}</div>
          <div class="kpi-sub">
            future picks change by 5%+ and 0.1+ EPV{#if s.top}. Largest: {s.top.owner} {s.top.year} {s.top.short_label}
              ({fmtSigned1(s.top.delta)} EPV){/if}
          </div>
        </div>
      </div>
    </div>

    <div class="cols">
      <section class="card">
        <div class="card-header with-controls">
          <h3>Who wins and loses: total EPV by team</h3>
          <div class="controls">
            <select class="select compact" bind:value={year} aria-label="Draft year">
              <option value="All">All years</option>
              {#each s.years as y}<option value={String(y)}>{y}</option>{/each}
            </select>
            <select class="select compact" bind:value={round} aria-label="Round">
              <option value="All">Both rounds</option>
              <option value="1">Round 1</option>
              <option value="2">Round 2</option>
            </select>
            <select class="select compact wide" bind:value={sort} aria-label="Sort">
              <option value="delta">Sort by change</option>
              <option value="total">Sort by 3-2-1 total</option>
            </select>
          </div>
        </div>
        <PlotlyChart data={lb.data} layout={lb.layout} logos={lb.logos} height={820}
                     label="Total EPV by team, current lottery versus 3-2-1" />
        <p class="hint">Line = change from the current lottery (grey tick) to 3-2-1 (logo). Shaded bar = 80% interval of the change.</p>
      </section>

      <section class="card">
        <h3 class="card-header">Biggest pick movers</h3>
        <div class="card-body movers">
          <table>
            <thead>
              <tr><th class="txt">Owner</th><th class="txt">Pick</th><th>Current</th><th>3-2-1</th><th>Δ EPV</th><th>Δ %</th></tr>
            </thead>
            <tbody>
              {#each s.mover_ids as id (id)}
                {@const p = picksById.get(id)}
                {#if p}
                  {@const pc = pctChange(p.cur_mean, p.new_mean)}
                  <tr>
                    <td class="txt"><TeamTag team={teams.get(p.owner)} /></td>
                    <td class="txt"><a href={pickHref(p.id)}>{p.year} {p.short_label}</a></td>
                    <td>{fmt1(p.cur_mean)}</td>
                    <td>{fmt1(p.new_mean)}</td>
                    <td style:color={deltaColor(p.delta)}>{fmtSigned1(p.delta)}</td>
                    <td style:color={deltaColor(pc)}>{fmtDelta1(pc, "%")}</td>
                  </tr>
                {/if}
              {/each}
            </tbody>
          </table>
          <p class="hint">Click a pick to open its full distribution.</p>
        </div>
      </section>
    </div>

    <details class="more">
      <summary>All picks, sorted by change in EPV</summary>
      <div class="more-body">
        <AllPicksTable picks={s.picks} {teams} years={s.years} />
      </div>
    </details>
  </div>
{:catch err}
  <p class="status error" role="alert">Could not load the summary data: {err.message}</p>
{/await}

<style>
  .status { padding: 60px 0; text-align: center; color: var(--muted); font-size: 16px; }
  .error { color: #fca5a5; }
  .summary { max-width: 1500px; margin: 0 auto; display: flex; flex-direction: column; gap: 14px; }
  .hero {
    border: 1px solid rgba(255, 255, 255, 0.08);
    background: linear-gradient(135deg, rgba(109, 40, 217, 0.18), rgba(15, 15, 26, 0.96));
    border-radius: 16px;
    padding: 18px 22px;
  }
  .hero h2 { margin: 0 0 10px; font-weight: 900; letter-spacing: -0.03em; color: #f4f4f8; font-size: 1.55rem; }
  .headline { color: #e6e8ef; font-size: 17px; line-height: 1.6; margin: 0; }
  .headline b { color: #fff; }
  .note { color: #9ca0b0; font-size: 12.5px; line-height: 1.55; margin: 10px 0 0; }
  .kpis { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 12px; }
  .kpi {
    display: flex;
    gap: 14px;
    align-items: center;
    min-height: 112px;
    padding: 14px 16px;
    border: 1px solid var(--border);
    border-left: 4px solid var(--kpi-accent);
    border-radius: 10px;
    background: rgba(15, 15, 26, 0.72);
  }
  .kpi img { width: 64px; height: 64px; object-fit: contain; flex: 0 0 auto; }
  .kpi-label { font-size: 11.5px; color: #9ca0b0; text-transform: uppercase; letter-spacing: 0.07em; font-weight: 750; }
  .kpi-value { font-size: 28px; font-weight: 850; line-height: 1.12; color: var(--kpi-accent); white-space: nowrap; }
  .kpi-sub { font-size: 13px; color: #aab0c0; margin-top: 4px; line-height: 1.4; }
  .cols { display: grid; grid-template-columns: minmax(0, 7fr) minmax(0, 5fr); gap: 14px; align-items: start; }
  h3 { margin: 0; font-size: 14px; }
  .with-controls { display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px 12px; }
  .controls { display: flex; gap: 8px; flex-wrap: wrap; }
  .select.compact { width: 135px; min-height: 34px; padding-top: 4px; padding-bottom: 4px; font-size: 13px; }
  .select.compact.wide { width: 185px; }
  .hint { font-size: 12px; color: #8b8fa3; margin: 8px 12px 12px; }
  .movers { padding-bottom: 4px; }
  .movers .hint { margin: 10px 0 4px; }
  .movers table { width: 100%; border-collapse: collapse; font-size: 12.5px; table-layout: fixed; font-family: var(--mono); }
  .movers th { color: #aab0c0; text-align: right; padding: 6px; border-bottom: 1px solid #1a1a2a; font-weight: 750; }
  .movers td { padding: 6px; border-bottom: 1px solid rgba(255, 255, 255, 0.045); text-align: right; color: #d7d8e2; }
  .movers .txt { text-align: left; }
  .movers td.txt { overflow-wrap: anywhere; font-family: var(--font); }
  .movers th:nth-child(1) { width: 19%; }
  .movers th:nth-child(2) { width: 33%; }
  .movers a { color: var(--link); text-decoration: none; }
  .movers a:hover { text-decoration: underline; }
  .more { background: var(--surface); border: 1px solid var(--border); border-radius: var(--radius); }
  .more summary {
    display: flex; align-items: center; padding: 14px 18px; font-weight: 800; font-size: 1.05rem;
    color: var(--text-strong); cursor: pointer; list-style: none;
  }
  .more summary::-webkit-details-marker { display: none; }
  .more summary::after {
    content: ""; margin-left: auto; width: 9px; height: 9px;
    border-right: 2px solid #c9ccd6; border-bottom: 2px solid #c9ccd6; transform: rotate(45deg); transition: transform 0.15s;
  }
  .more[open] summary::after { transform: rotate(-135deg); }
  .more summary:focus-visible { outline: 2px solid var(--link); outline-offset: -2px; }
  .more-body { padding: 4px 16px 16px; }
  @media (max-width: 1100px) {
    .kpis, .cols { grid-template-columns: minmax(0, 1fr); }
  }
  @media (max-width: 560px) {
    .hero { padding: 14px 14px; }
    .hero h2 { font-size: 1.25rem; }
    .headline { font-size: 15px; }
    .kpi { min-height: 0; }
    .kpi img { width: 48px; height: 48px; }
    .kpi-value { font-size: 24px; }
    .movers table { font-size: 11.5px; }
    .movers th:nth-child(1) { width: 22%; }
    .movers th:nth-child(2) { width: 34%; }
    .more summary { padding: 12px 14px; }
    .more-body { padding: 4px 10px 12px; }
  }
</style>
