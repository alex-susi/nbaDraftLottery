<script lang="ts">
  // Team > Single pick (sp_obligation, sp_headline, sp_dist_* in app.R): one
  // pick's ownership details, conveyance / swap odds, and its value
  // distribution under both lotteries. The address tracks the chosen pick so it
  // can be shared.
  import { untrack } from "svelte";
  import { loadSingle } from "../lib/data";
  import { deltaColor, fmt1, fmtSigned1, pctChange } from "../lib/format";
  import { pickHref, replaceRoute, route } from "../lib/router.svelte";
  import { singlePickFigure } from "../lib/teamCharts";
  import type { ProbSummary, SinglePick, Summary, SummaryTeam } from "../lib/types";
  import PlotlyChart from "./PlotlyChart.svelte";
  import TeamSelect from "./TeamSelect.svelte";
  import TeamTag from "./TeamTag.svelte";

  let { s, teams }: { s: Summary; teams: Map<string, SummaryTeam> } = $props();

  const picksById = $derived(new Map(s.picks.map((p) => [p.id, p])));
  const teamList = $derived([...teams.keys()].sort());

  let year = $state(2026);
  let team = $state("ATL");
  let pickId = $state<string | null>(null);
  let mode = $state<"ev" | "outcome">("ev");

  // A pick link (#/team/single/<id>) selects that pick's year, team, and pick.
  // Only the address is tracked, so menu choices are not pulled back.
  $effect(() => {
    const id = route.pickId;
    const p = id ? picksById.get(id) : undefined;
    untrack(() => {
      if (p && p.id !== pickId) {
        year = p.year;
        team = p.owner;
        pickId = p.id;
      }
    });
  });

  const options = $derived(s.picks.filter((p) => p.year === year && p.owner === team));

  // Keep the chosen pick when it is still available, otherwise take the first.
  $effect(() => {
    if (!options.some((o) => o.id === pickId)) pickId = options[0]?.id ?? null;
  });

  // Mirror the selection in the address while this view is showing.
  $effect(() => {
    if (pickId && route.page === "team" && route.teamView === "single" && route.pickId !== pickId) {
      replaceRoute(pickHref(pickId));
    }
  });

  let detail = $state<SinglePick | null>(null);
  let loadError = $state<string | null>(null);
  $effect(() => {
    const id = pickId;
    const owner = team;
    if (!id) { detail = null; return; }
    loadSingle(owner)
      .then((all) => { if (pickId === id) { detail = all[id] ?? null; loadError = null; } })
      .catch((e) => (loadError = String(e?.message ?? e)));
  });

  const row = $derived(pickId ? picksById.get(pickId) : undefined);
  const dist = $derived(detail ? (mode === "ev" ? detail.ev : detail.outcome) : null);
  const fig = $derived(dist && row ? singlePickFigure(
    dist.cur, dist.new, dist.stats,
    mode === "ev" ? `Expected Pick Value (${s.value_outcome} scale)` : `Realized ${s.value_outcome} outcome`,
    mode === "ev" ? "EV" : "outcome", s.value_unit) : null);

  const showProb = (p: ProbSummary | null) => p != null && p.prob != null && Number.isFinite(p.prob);
  const pct1 = (x: number | null) => (x == null ? "—" : `${(100 * x).toFixed(1)}%`);
</script>

<div class="layout">
  <aside class="card sidebar">
    <label>Draft year
      <select class="select" bind:value={year}>
        {#each s.years as y}<option value={y}>{y}</option>{/each}
      </select>
    </label>
    <div class="field">
      <span id="sp-team-label">Team</span>
      <TeamSelect id="sp-team" labelledby="sp-team-label" teams={teamList.map((t) => teams.get(t)!)}
                  value={team} onchange={(abbr) => (team = abbr)} />
    </div>
    <label>Pick
      <select class="select" bind:value={pickId} disabled={options.length === 0}>
        {#each options as o (o.id)}<option value={o.id}>{o.short_label}</option>{/each}
      </select>
    </label>
    {#if options.length === 0}<p class="muted">{team} has no {year} picks.</p>{/if}

    {#if detail && row}
      <hr />
      <div class="details">
        <strong class="details-title">Pick Details</strong>
        <div class="drow"><span class="dlabel">Current Team:</span><TeamTag team={teams.get(row.owner)} size={24} /></div>
        <div class="drow">
          <span class="dlabel">Original Team{detail.original_teams.length > 1 ? "s" : ""}:</span>
          <span class="tags">{#each detail.original_teams as o}<TeamTag team={teams.get(o)} size={24} />{/each}</span>
        </div>
        {#if detail.fixed_slot_display}<p class="green">Actual Pick: {detail.fixed_slot_display}</p>{/if}
        <div class="drow"><span class="dlabel">Obligation:</span><span>{detail.obligation}</span></div>

        {#each [["Conveyance Probability", detail.convey, detail.show_convey],
                ["Swap Exercise Probability", detail.swap, detail.swap.cur != null || detail.swap.new != null]] as const as [label, pair, show]}
          {#if show}
            <div class="probs">
              <div class="probs-label">{label}</div>
              <div class="prob-row">
                {#each [["Current", pair.cur, "#3b82f6"], ["3-2-1", pair.new, "#f59e0b"]] as const as [name, st, col]}
                  {#if st && showProb(st)}
                    <div class="prob" style:border-left-color={col}>
                      <div class="prob-name">{name}</div>
                      <div class="prob-val" style:color={col}>{pct1(st.prob)}</div>
                      <div class="prob-ci">90% CI [{pct1(st.q05)}, {pct1(st.q95)}]</div>
                    </div>
                  {/if}
                {/each}
              </div>
            </div>
          {/if}
        {/each}
        {#if row.year === 2026}<p class="green">Locked to the actual 2026 draft result</p>{/if}
      </div>
    {/if}
  </aside>

  <div class="main">
    <section class="card">
      <h3 class="card-header">Expected Pick Value Impact</h3>
      <div class="card-body">
        {#if detail}
          {@const st = detail.ev.stats}
          {@const delta = st.new_mean - st.cur_mean}
          {@const pc = pctChange(st.cur_mean, st.new_mean)}
          <div class="boxes">
            <div class="box" style:border-left-color="#3b82f6">
              <div class="box-title">Current system</div>
              <div class="box-val" style:color="#3b82f6">{fmt1(st.cur_mean)}</div>
              <div class="box-sub">90% interval: [{fmt1(st.cur_q05)}, {fmt1(st.cur_q95)}]</div>
            </div>
            <div class="box" style:border-left-color="#f59e0b">
              <div class="box-title">3-2-1 system</div>
              <div class="box-val" style:color="#f59e0b">{fmt1(st.new_mean)}</div>
              <div class="box-sub">90% interval: [{fmt1(st.new_q05)}, {fmt1(st.new_q95)}]</div>
            </div>
            <div class="box" style:border-left-color={deltaColor(delta)}>
              <div class="box-title">Δ EPV</div>
              <div class="box-val" style:color={deltaColor(delta)}>{fmtSigned1(delta)}</div>
              <div class="box-sub">3-2-1 minus current</div>
            </div>
            <div class="box" style:border-left-color={deltaColor(pc)}>
              <div class="box-title">Δ EPV %</div>
              <div class="box-val" style:color={deltaColor(pc)}>{Number.isFinite(pc) ? `${fmtSigned1(pc)}%` : "—"}</div>
              <div class="box-sub">relative to current</div>
            </div>
          </div>
        {:else if loadError}
          <p class="error" role="alert">Could not load this pick: {loadError}</p>
        {:else}
          <p class="muted">Loading pick…</p>
        {/if}
      </div>
    </section>

    <section class="card">
      <div class="card-header with-controls">
        <h3>{mode === "ev" ? "Expected Pick Value" : `${s.value_outcome} Outcome Distribution`}</h3>
        <div class="radios" role="radiogroup" aria-label="Value shown">
          <label><input type="radio" bind:group={mode} value="ev" /> Expected pick value</label>
          <label><input type="radio" bind:group={mode} value="outcome" /> {s.value_outcome} outcomes</label>
        </div>
      </div>
      {#if fig}
        <PlotlyChart data={fig.data} layout={fig.layout} height={(w) => (w < 640 ? 320 : 380)}
                     label="Value distribution under the current and 3-2-1 lotteries" />
      {/if}
    </section>
  </div>
</div>

<style>
  .layout { display: grid; grid-template-columns: minmax(280px, 500px) minmax(0, 1fr); gap: 14px; align-items: start; }
  .sidebar { display: flex; flex-direction: column; gap: 14px; padding: 16px; min-width: 0; }
  .sidebar label, .field { display: flex; flex-direction: column; gap: 6px; font-weight: 700; color: #cfd2dc; }
  .field :global(.team-select) { font-weight: 400; color: var(--text); }
  .main { display: flex; flex-direction: column; gap: 14px; min-width: 0; }
  h3 { margin: 0; font-size: 14px; }
  hr { border: 0; border-top: 1px solid var(--border); margin: 2px 0; width: 100%; }
  .muted { color: var(--muted); margin: 0; }
  .error { color: #fca5a5; margin: 0; }
  .details { font-size: 14px; color: #c5c8d2; line-height: 1.65; }
  .details-title { display: block; margin-bottom: 14px; color: #fff; font-size: 21px; }
  .drow { display: grid; grid-template-columns: 128px minmax(0, 1fr); gap: 8px; align-items: start; margin-bottom: 10px; }
  .dlabel { color: #9ca0b0; font-weight: 700; }
  .tags { display: flex; flex-wrap: wrap; gap: 6px 14px; }
  .green { color: #10b981; margin: 0 0 10px; }
  .probs { margin-top: 10px; }
  .probs-label { font-size: 14px; font-weight: 700; color: #d0d0d0; margin-bottom: 6px; }
  .prob-row { display: flex; gap: 10px; flex-wrap: wrap; }
  .prob { flex: 0 0 182px; padding: 10px; border: 1px solid #1a1a2a; border-left: 3px solid; border-radius: 8px; }
  .prob-name { font-size: 14px; color: #aaa; }
  .prob-val { font-size: 25px; font-weight: 700; line-height: 1.1; margin-top: 3px; }
  .prob-ci { font-size: 13px; color: #c5c8d2; margin-top: 5px; white-space: nowrap; }
  .boxes { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; }
  .box { padding: 12px; border: 1px solid #1a1a2a; border-left: 3px solid; border-radius: 8px; min-width: 0; }
  .box-title { font-size: 11px; color: #888; }
  .box-val { font-size: 26px; font-weight: 700; }
  .box-sub { font-size: 11px; color: #aaa; }
  .with-controls { display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px 12px; }
  .radios { display: flex; gap: 16px; flex-wrap: wrap; font-weight: 600; font-size: 13px; }
  .radios label { display: inline-flex; align-items: center; gap: 6px; cursor: pointer; }
  .radios input { accent-color: var(--primary); width: 16px; height: 16px; }
  @media (max-width: 1000px) {
    .layout { grid-template-columns: minmax(0, 1fr); }
  }
  @media (max-width: 700px) {
    .boxes { grid-template-columns: 1fr 1fr; }
  }
  @media (max-width: 420px) {
    .prob { flex: 1 1 140px; }
    .drow { grid-template-columns: 1fr; gap: 2px; }
  }
</style>
