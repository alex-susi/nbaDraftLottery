<script lang="ts">
  // "All picks, sorted by change in EPV" (pick_movers_table in app.R): every
  // pick with year / round / team filters and sortable columns. Pick names link
  // to Team > Single pick.
  import { deltaColor, fmt1, fmtDelta1, pctChange } from "../lib/format";
  import { pickHref } from "../lib/router.svelte";
  import type { PickRow, SummaryTeam } from "../lib/types";
  import TeamSelect from "./TeamSelect.svelte";
  import TeamTag from "./TeamTag.svelte";

  let { picks, teams, years }: { picks: PickRow[]; teams: Map<string, SummaryTeam>; years: number[] } = $props();

  let year = $state("All");
  let round = $state("All");
  let team = $state("All");
  type Key = "owner" | "year" | "round" | "short_label" | "cur_mean" | "new_mean" | "delta" | "pct";
  let sortKey = $state<Key | null>(null);
  let sortDesc = $state(true);

  const owners = $derived([...new Set(picks.map((p) => p.owner))].sort());

  // Default order matches app.R: biggest gain first, then year, round, owner, label.
  const base = $derived(
    picks
      .filter((p) => (year === "All" || p.year === Number(year)) &&
                     (round === "All" || p.round === Number(round)) &&
                     (team === "All" || p.owner === team))
      .map((p) => ({ ...p, pct: pctChange(p.cur_mean, p.new_mean) }))
      .sort((a, b) => b.delta - a.delta || a.year - b.year || a.round - b.round ||
                      a.owner.localeCompare(b.owner) || a.short_label.localeCompare(b.short_label))
  );

  const rows = $derived.by(() => {
    if (!sortKey) return base;
    const k = sortKey;
    const val = (r: (typeof base)[number]) => {
      const v = r[k];
      return typeof v === "number" ? (Number.isFinite(v) ? v : 0) : String(v);
    };
    return [...base].sort((a, b) => {
      const va = val(a), vb = val(b);
      const c = typeof va === "number" && typeof vb === "number" ? va - vb : String(va).localeCompare(String(vb));
      return sortDesc ? -c : c;
    });
  });

  function sortBy(k: Key) {
    if (sortKey === k) sortDesc = !sortDesc;
    else { sortKey = k; sortDesc = k !== "owner" && k !== "short_label"; }
  }
  const ariaSort = (k: Key) => (sortKey === k ? (sortDesc ? "descending" : "ascending") : "none");

  const columns: { key: Key; label: string; cls?: string }[] = [
    { key: "owner", label: "Owner" },
    { key: "year", label: "Year", cls: "r opt" },
    { key: "round", label: "Round", cls: "r opt" },
    { key: "short_label", label: "Pick" },
    { key: "cur_mean", label: "Current", cls: "r opt" },
    { key: "new_mean", label: "3-2-1", cls: "r" },
    { key: "delta", label: "Delta EPV", cls: "r" },
    { key: "pct", label: "Delta EPV %", cls: "r" },
  ];
</script>

<div class="filters">
  <label>Draft year
    <select class="select" bind:value={year}>
      <option value="All">All</option>
      {#each years as y}<option value={String(y)}>{y}</option>{/each}
    </select>
  </label>
  <label>Round
    <select class="select" bind:value={round}>
      <option value="All">All</option>
      <option value="1">Round 1</option>
      <option value="2">Round 2</option>
    </select>
  </label>
  <div class="field">
    <span id="pm-team-label">Team</span>
    <TeamSelect id="pm-team" labelledby="pm-team-label" allLabel="All teams"
                teams={owners.map((o) => teams.get(o) ?? { abbr: o, name: o, logo: null })}
                value={team} onchange={(abbr) => (team = abbr)} />
  </div>
  <button type="button" class="btn small" onclick={() => { year = "All"; round = "All"; team = "All"; sortKey = null; }}>
    Clear filters
  </button>
</div>

<div class="wrap">
  <table>
    <thead>
      <tr>
        {#each columns as c (c.key)}
          <th class={c.cls} aria-sort={ariaSort(c.key)}>
            <button type="button" onclick={() => sortBy(c.key)}>
              {c.label}<span class="arrow" aria-hidden="true">{sortKey === c.key ? (sortDesc ? "▼" : "▲") : ""}</span>
            </button>
          </th>
        {/each}
      </tr>
    </thead>
    <tbody>
      {#each rows as p (p.id)}
        <tr>
          <td><TeamTag team={teams.get(p.owner)} /></td>
          <td class="r opt num">{p.year}</td>
          <td class="r opt num">{p.round}</td>
          <td class="pick"><a href={pickHref(p.id)}><span class="m-year">{p.year} </span>{p.short_label}</a></td>
          <td class="r opt num">{fmt1(p.cur_mean)}</td>
          <td class="r num">{fmt1(p.new_mean)}</td>
          <td class="r num" style:color={deltaColor(p.delta)}>{fmtDelta1(p.delta)}</td>
          <td class="r num" style:color={deltaColor(p.pct)}>{fmtDelta1(p.pct, "%")}</td>
        </tr>
      {:else}
        <tr><td colspan="8" class="empty">No picks match the selected filters.</td></tr>
      {/each}
    </tbody>
  </table>
</div>

<style>
  .filters { display: flex; gap: 12px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 12px; }
  .filters label, .filters .field { display: flex; flex-direction: column; gap: 6px; width: 190px; font-weight: 700; color: #cfd2dc; }
  .filters .field { width: 240px; }
  .field :global(.team-select) { font-weight: 400; color: var(--text); }
  .btn.small { min-height: 38px; padding: 6px 14px; font-size: 13px; }
  .wrap { max-height: 560px; overflow: auto; container-type: inline-size; border-top: 1px solid var(--border); }
  table { width: 100%; border-collapse: collapse; font-family: var(--mono); font-size: 13px; }
  th {
    position: sticky;
    top: 0;
    z-index: 2;
    background: var(--surface);
    border-bottom: 1px solid var(--border);
    text-align: left;
    padding: 0;
  }
  th button {
    width: 100%;
    border: 0;
    background: none;
    padding: 9px 8px;
    font: inherit;
    font-weight: 600;
    color: #cfd2dc;
    text-align: inherit;
    cursor: pointer;
    white-space: nowrap;
  }
  th button:focus-visible { outline: 2px solid var(--link); outline-offset: -2px; }
  .arrow { display: inline-block; width: 1em; font-size: 9px; margin-left: 3px; }
  td { padding: 7px 8px; border-bottom: 1px solid rgba(255, 255, 255, 0.045); vertical-align: middle; }
  tr:hover td { background: rgba(255, 255, 255, 0.025); }
  .r { text-align: right; }
  .pick { width: 42%; font-family: var(--font); overflow-wrap: anywhere; }
  .pick a { color: var(--link); text-decoration: none; }
  .pick a:hover { text-decoration: underline; }
  .m-year { display: none; }
  .empty { text-align: center; color: var(--muted); padding: 24px; }
  @container (max-width: 620px) {
    .opt { display: none; }
    .m-year { display: inline; }
    table { font-size: 12px; }
    td, th button { padding-left: 5px; padding-right: 5px; }
  }
  @media (max-width: 560px) {
    .filters label, .filters .field { width: calc(50% - 6px); }
  }
</style>
