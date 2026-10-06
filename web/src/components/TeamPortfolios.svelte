<script lang="ts">
  // Team > Team portfolios (output$full_table and output$team_detail in app.R):
  // all 30 teams, worst record first; selecting a team opens its pick impacts
  // and a breakdown by pick type and round.
  import { deltaColor, fmt1, fmtDelta1, pctChange } from "../lib/format";
  import { pickHref } from "../lib/router.svelte";
  import type { PickRow, Summary, SummaryTeam } from "../lib/types";
  import TeamTag from "./TeamTag.svelte";

  let { s, teams }: { s: Summary; teams: Map<string, SummaryTeam> } = $props();

  let selected = $state<string | null>(null);
  type Key = "team" | "tier" | "wins" | "n_picks" | "cur" | "new" | "delta" | "pct";
  let sortKey = $state<Key | null>(null);
  let sortDesc = $state(false);
  let detailEl = $state<HTMLElement | null>(null);

  const base = $derived.by(() => {
    const pf = new Map(s.portfolio["All|All"].map((r) => [r.team, r]));
    return s.team_table
      .map((t) => {
        const p = pf.get(t.team)!;
        return {
          ...t,
          n_picks: p.new_expected_picks,
          cur: p.cur_total_value,
          new: p.new_total_value,
          delta: p.delta_total_value,
          pct: (p.new_total_value / Math.max(p.cur_total_value, 0.01) - 1) * 100,
        };
      })
      .sort((a, b) => a.wins - b.wins);
  });

  const rows = $derived.by(() => {
    if (!sortKey) return base;
    const k = sortKey;
    return [...base].sort((a, b) => {
      const va = a[k], vb = b[k];
      const c = typeof va === "number" && typeof vb === "number" ? va - vb : String(va).localeCompare(String(vb));
      return sortDesc ? -c : c;
    });
  });

  function sortBy(k: Key) {
    if (sortKey === k) sortDesc = !sortDesc;
    else { sortKey = k; sortDesc = !["team", "tier", "wins"].includes(k); }
  }

  function choose(team: string) {
    selected = selected === team ? null : team;
    // On narrow screens the detail sits above the table; bring it into view.
    if (selected && window.matchMedia("(max-width: 1000px)").matches) {
      setTimeout(() => detailEl?.scrollIntoView({ behavior: "smooth", block: "start" }), 0);
    }
  }

  const columns: { key: Key; label: string; cls?: string }[] = [
    { key: "team", label: "Team" },
    { key: "tier", label: "Tier" },
    { key: "wins", label: "Record", cls: "opt" },
    { key: "n_picks", label: "#Pk", cls: "r" },
    { key: "cur", label: "Current EPV", cls: "r opt" },
    { key: "new", label: "3-2-1 EPV", cls: "r" },
    { key: "delta", label: "Δ EPV", cls: "r" },
    { key: "pct", label: "Δ %", cls: "r" },
  ];

  // ---- Detail pane ----
  const BUCKETS = ["Own picks", "Incoming outright picks", "Swaps / protections"] as const;
  const teamPicks = $derived(selected ? s.picks.filter((p) => p.owner === selected) : []);
  const positive = $derived(teamPicks.filter((p) => p.delta > 0).sort((a, b) => b.delta - a.delta).slice(0, 3));
  const negative = $derived(teamPicks.filter((p) => p.delta < 0).sort((a, b) => a.delta - b.delta).slice(0, 3));

  function breakout(round: number) {
    const out = BUCKETS.map((bucket) => {
      const ps = teamPicks.filter((p) => p.impact_bucket === bucket && p.round === round);
      const sum = (f: (p: PickRow) => number) => ps.reduce((acc, p) => acc + (Number.isFinite(f(p)) ? f(p) : 0), 0);
      return {
        bucket,
        n: sum((p) => Math.max(p.cur_expected_pick_count ?? 0, p.new_expected_pick_count ?? 0)),
        cur: sum((p) => p.cur_mean),
        new: sum((p) => p.new_mean),
        delta: sum((p) => p.delta),
        total: false,
      };
    });
    const tot = (k: "n" | "cur" | "new" | "delta") => out.reduce((a, r) => a + r[k], 0);
    out.push({ bucket: "Total" as never, n: tot("n"), cur: tot("cur"), new: tot("new"), delta: tot("delta"), total: true });
    return out.map((r) => ({ ...r, pct: Math.abs(r.cur) > 1e-9 ? (r.delta / r.cur) * 100 : NaN, zero: Math.abs(r.n) < 1e-9 }));
  }
  const fmtCount = (n: number) => (Math.abs(n - Math.round(n)) < 0.05 ? String(Math.round(n)) : n.toFixed(1));
</script>

<div class="split" class:with-detail={selected}>
  <section class="card table-card">
    <h3 class="card-header">All 30 Teams — Click a row for detail</h3>
    <div class="scroll">
      <table>
        <thead>
          <tr>
            {#each columns as c (c.key)}
              <th class={c.cls} aria-sort={sortKey === c.key ? (sortDesc ? "descending" : "ascending") : "none"}>
                <button type="button" onclick={() => sortBy(c.key)}>
                  {c.label}<span class="arrow" aria-hidden="true">{sortKey === c.key ? (sortDesc ? "▼" : "▲") : ""}</span>
                </button>
              </th>
            {/each}
          </tr>
        </thead>
        <tbody>
          {#each rows as r (r.team)}
            <tr class:selected={selected === r.team} onclick={() => choose(r.team)}
                tabindex="0" onkeydown={(e) => (e.key === "Enter" || e.key === " ") && (e.preventDefault(), choose(r.team))}
                aria-selected={selected === r.team}>
              <td><TeamTag team={teams.get(r.team)} size={22} /></td>
              <td>{r.tier}</td>
              <td class="opt">{r.wins}-{r.losses}</td>
              <td class="r">{fmt1(r.n_picks)}</td>
              <td class="r opt">{fmt1(r.cur)}</td>
              <td class="r">{fmt1(r.new)}</td>
              <td class="r" style:color={deltaColor(r.delta)}>{fmtDelta1(r.delta)}</td>
              <td class="r" style:color={deltaColor(r.pct)}>{fmtDelta1(r.pct, "%")}</td>
            </tr>
          {/each}
        </tbody>
      </table>
    </div>
  </section>

  {#if selected}
    {@const t = teams.get(selected)}
    <section class="card detail" bind:this={detailEl} aria-label={`${t?.name} detail`}>
      <div class="detail-head" style:border-color={t?.primary} style:border-left-color={t?.secondary}>
        <TeamTag team={t} size={34} full />
        <button type="button" class="close" aria-label="Close team detail" onclick={() => (selected = null)}>×</button>
      </div>
      <div class="detail-body">
        {#each [["Most positive pick impacts", positive, "#10b981", "No positive pick impacts"],
                ["Most negative pick impacts", negative, "#ef4444", "No negative pick impacts"]] as const as [title, list, col, empty]}
          {#if list.length === 0}
            <p class="none">{empty}</p>
          {:else}
            <table class="mini">
              <thead>
                <tr>
                  <th class="txt" style:color={col} style="width:48%">{title}</th>
                  <th>Cur</th><th>3-2-1</th><th>Δ EPV</th><th>Δ EPV %</th>
                </tr>
              </thead>
              <tbody>
                {#each list as p (p.id)}
                  {@const pc = pctChange(p.cur_mean, p.new_mean)}
                  <tr>
                    <td class="txt"><a href={pickHref(p.id)}>{p.year} {p.short_label}</a></td>
                    <td>{fmt1(p.cur_mean)}</td>
                    <td>{fmt1(p.new_mean)}</td>
                    <td style:color={deltaColor(p.delta)}>{fmtDelta1(p.delta)}</td>
                    <td style:color={deltaColor(pc)}>{fmtDelta1(pc, "%")}</td>
                  </tr>
                {/each}
              </tbody>
            </table>
          {/if}
        {/each}

        <hr />
        <div class="sub">Impact by pick type and round</div>
        {#each [1, 2] as rnd}
          <table class="mini breakout">
            <thead>
              <tr>
                <th class="txt" style="width:36%">Round {rnd}</th>
                <th># Picks</th><th>Cur</th><th>3-2-1</th><th>Δ EPV</th><th>Δ EPV %</th>
              </tr>
            </thead>
            <tbody>
              {#each breakout(rnd) as r (r.bucket)}
                <tr class:total={r.total}>
                  <td class="txt">{r.bucket}</td>
                  <td>{fmtCount(r.n)}</td>
                  <td>{fmt1(r.cur)}</td>
                  <td>{fmt1(r.new)}</td>
                  <td style:color={r.zero ? "#d0d0d0" : deltaColor(r.delta)}>{r.zero ? "—" : fmtDelta1(r.delta)}</td>
                  <td style:color={r.zero ? "#d0d0d0" : deltaColor(r.pct)}>{r.zero ? "—" : fmtDelta1(r.pct, "%")}</td>
                </tr>
              {/each}
            </tbody>
          </table>
        {/each}
      </div>
    </section>
  {/if}
</div>

<style>
  .split { display: grid; grid-template-columns: minmax(0, 1fr); gap: 12px; }
  .split.with-detail { grid-template-columns: minmax(0, 1.05fr) minmax(0, 0.95fr); }
  .table-card, .detail { display: flex; flex-direction: column; min-height: 0; }
  @media (min-width: 1001px) {
    .split { height: calc(100vh - 150px); min-height: 620px; }
    .table-card, .detail { height: 100%; overflow: hidden; }
    .scroll { flex: 1 1 auto; min-height: 0; overflow-y: auto; }
    .detail { overflow-y: auto; }
  }
  @media (max-width: 1000px) {
    .split.with-detail { grid-template-columns: minmax(0, 1fr); }
    .detail { order: -1; scroll-margin-top: 70px; }
  }
  h3 { margin: 0; font-size: 14px; }
  .scroll { container-type: inline-size; }
  table { width: 100%; border-collapse: collapse; font-family: var(--mono); font-size: 13px; }
  th { position: sticky; top: 0; z-index: 2; background: var(--surface); border-bottom: 1px solid var(--border); padding: 0; text-align: left; }
  th button {
    width: 100%; border: 0; background: none; padding: 9px 8px; font: inherit; font-weight: 600;
    color: #cfd2dc; text-align: inherit; cursor: pointer; white-space: nowrap;
  }
  th button:focus-visible { outline: 2px solid var(--link); outline-offset: -2px; }
  .arrow { display: inline-block; width: 1em; font-size: 9px; margin-left: 3px; }
  td { padding: 7px 8px; border-bottom: 1px solid rgba(255, 255, 255, 0.045); }
  tbody tr { cursor: pointer; }
  tbody tr:hover td { background: rgba(255, 255, 255, 0.03); }
  tbody tr.selected td { background: rgba(109, 40, 217, 0.22); }
  tbody tr:focus-visible { outline: 2px solid var(--link); outline-offset: -2px; }
  .r { text-align: right; }
  @container (max-width: 560px) {
    .opt { display: none; }
    table { font-size: 12px; }
    td, th button { padding-left: 5px; padding-right: 5px; }
  }

  .detail-head {
    display: flex; align-items: center; justify-content: space-between; gap: 10px;
    padding: 6px 12px 8px; border: 1px solid; border-left-width: 3px; border-radius: var(--radius) var(--radius) 0 0;
    background: rgba(15, 15, 26, 0.72); font-weight: 850; font-size: 16px; color: var(--text-strong);
  }
  .close { border: 0; background: none; font-size: 24px; line-height: 1; cursor: pointer; color: #c9ccd6; padding: 4px 8px; }
  .detail-body { padding: 12px 14px 14px; font-size: 12px; color: #bbb; }
  .mini { table-layout: fixed; font-size: 12px; margin-bottom: 14px; }
  .mini th { position: static; padding: 4px; text-align: right; color: #aaa; font-weight: 800; background: none; border-bottom: 1px solid #1a1a2a; }
  .mini td { padding: 4px; text-align: right; overflow-wrap: anywhere; }
  .mini .txt { text-align: left; }
  .mini td.txt { font-family: var(--font); }
  .mini tbody tr { cursor: default; }
  .mini tbody tr:hover td { background: none; }
  .mini a { color: var(--link); text-decoration: none; }
  .mini a:hover { text-decoration: underline; }
  .breakout tr.total td { font-weight: 800; border-top: 1px solid #303044; }
  .mini td:not(.txt) { white-space: nowrap; }
  @media (max-width: 560px) {
    /* Numbers keep their width; the label column takes what is left and wraps. */
    .mini { table-layout: auto; font-size: 11px; }
    .mini th:first-child { width: auto !important; }
    .mini th, .mini td { padding: 4px 3px; }
    .detail-body { padding: 10px; }
  }
  .none { color: #777; margin: 0 0 14px; }
  hr { border: 0; border-top: 1px solid #1a1a2a; margin: 4px 0 8px; }
  .sub { font-weight: 700; color: #d0d0d0; margin-bottom: 6px; font-size: 13px; }
</style>
