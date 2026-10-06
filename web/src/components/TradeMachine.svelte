<script lang="ts">
  import { loadTeamBundle } from "../lib/data";
  import { fmt1, hexToRgba } from "../lib/format";
  import { mean } from "../lib/stats";
  import { controlKey, valueTrade, type TradeContext } from "../lib/trade";
  import type { PickControl, Side, Team } from "../lib/types";
  import Assessment from "./Assessment.svelte";
  import DensityChart from "./DensityChart.svelte";
  import PickSelect from "./PickSelect.svelte";
  import PickTable from "./PickTable.svelte";

  // The context is created once at load and never replaced.
  let { ctx }: { ctx: TradeContext } = $props();
  // svelte-ignore state_referenced_locally
  const meta = ctx.meta;
  const teamsByAbbr = new Map(meta.teams.map((t) => [t.abbr, t]));
  const outcomeLabel = meta.labels.value_outcome;

  let teamA = $state(meta.teams[0].abbr);
  let teamB = $state(meta.teams[1].abbr);
  let picksA = $state<string[]>([]);
  let picksB = $state<string[]>([]);
  // Like Shiny inputs, a pick's protection / swap settings persist if it is
  // removed and added again.
  let controls = $state<Record<string, PickControl>>({});
  let showOutcomes = $state(false);
  let loaded = $state<string[]>([]);
  let loadError = $state<string | null>(null);

  $effect(() => {
    for (const team of [teamA, teamB]) {
      if (loaded.includes(team)) continue;
      loadTeamBundle(meta, team)
        .then((bundle) => {
          ctx.bundles.set(team, bundle);
          if (!loaded.includes(team)) loaded = [...loaded, team];
        })
        .catch((err) => (loadError = String(err?.message ?? err)));
    }
  });

  const ready = $derived(loaded.includes(teamA) && loaded.includes(teamB));
  const tA = $derived(teamsByAbbr.get(teamA)!);
  const tB = $derived(teamsByAbbr.get(teamB)!);
  const ownedBy = (team: string) => meta.displays.filter((d) => d.owner === team);

  const result = $derived(ready ? valueTrade(ctx, teamA, teamB, picksA, picksB, controls) : null);
  const picksOf = (side: Side) => result?.picks.filter((p) => p.side === side) ?? [];

  const verdict = $derived.by(() => {
    if (!result) return null;
    const e = mean(result.netEvToA);
    let pos = 0, neg = 0;
    for (const v of result.netEvToA) {
      if (v > 0) pos++;
      else if (v < 0) neg++;
    }
    const n = result.netEvToA.length;
    if (Math.abs(e) < 0.05) return { even: true, winner: tA, gap: Math.abs(e), p: pos / n };
    return { even: false, winner: e > 0 ? tA : tB, gap: Math.abs(e), p: (e > 0 ? pos : neg) / n };
  });

  const sideStyle = (t: Team) =>
    `--side-color:${t.secondary};--side-soft:${hexToRgba(t.secondary, 0.26)};--side-glow:${hexToRgba(t.secondary, 0.36)}`;

  function setTeam(side: Side, abbr: string) {
    if (side === "A") { teamA = abbr; picksA = []; }
    else { teamB = abbr; picksB = []; }
  }

  function setControl(side: Side, id: string, c: PickControl) {
    controls = { ...controls, [controlKey(side, id)]: c };
  }

  const negate = (x: Float64Array) => x.map((v) => -v);
</script>

<div class="tm">
  <div class="verdict" style:border-left-color={verdict && !verdict.even ? verdict.winner.secondary : undefined}
       class:accent={verdict && !verdict.even} aria-live="polite">
    {#if !verdict}
      <span class="muted">Select picks for either team below to see who wins the trade.</span>
    {:else if verdict.even}
      Roughly even: the expected value gap is under 0.05 EPV, and {verdict.winner.name} come out ahead in {(100 * verdict.p).toFixed(0)}% of simulations.
    {:else}
      The <b>{verdict.winner.name}</b> win this trade by <b>{fmt1(verdict.gap)} EPV</b> (expected {meta.labels.value_unit_long}) and come out ahead in <b>{(100 * verdict.p).toFixed(0)}%</b> of simulations.
    {/if}
  </div>

  <div class="reset">
    <button type="button" class="btn" onclick={() => { picksA = []; picksB = []; }}>Clear Trade Inputs</button>
  </div>

  {#if loadError}
    <p class="error" role="alert">Could not load pick data: {loadError}</p>
  {/if}

  <div class="two">
    {#each [["A", tA, picksA, "Team 1"], ["B", tB, picksB, "Team 2"]] as const as [side, team, picks, title] (side)}
      <section class="card team-card" class:right={side === "B"} style={sideStyle(team)} aria-label={title}>
        <div class="identity">
          {#if team.logo}<img src={team.logo} alt="" width="118" height="118" />{/if}
          <div class="team-name">{team.name}</div>
        </div>
        <div class="controls">
          <label class="label" for={`team-${side}`}>{title}</label>
          <select id={`team-${side}`} class="select" value={team.abbr}
                  onchange={(e) => setTeam(side, e.currentTarget.value)}>
            {#each meta.teams as t (t.abbr)}
              <option value={t.abbr}>{t.abbr} · {t.name}</option>
            {/each}
          </select>
          <PickSelect id={`picks-${side}`} label={`Picks ${team.abbr} sends out`}
                      options={ownedBy(team.abbr)} selected={picks}
                      onchange={(ids) => (side === "A" ? (picksA = ids) : (picksB = ids))} />
        </div>
      </section>
    {/each}
  </div>

  <div class="two out-row">
    {#each [["A", tA, tB], ["B", tB, tA]] as const as [side, team, receiver] (side)}
      <section class="card" style={sideStyle(team)}>
        <div class="card-header out-header">
          {#if team.logo}<img src={team.logo} alt="" width="28" height="28" />{/if}
          <span>{team.abbr} outgoing picks</span>
        </div>
        <div class="card-body">
          {#if ready}
            <PickTable {side} receiver={receiver.abbr} picks={picksOf(side)} displays={ctx.displays}
                       {controls} choicesFor={(r) => meta.protection_choices[r === 2 ? "2" : "1"]}
                       onControl={(id, c) => setControl(side, id, c)} />
          {:else}
            <p class="muted">Loading picks…</p>
          {/if}
        </div>
      </section>
    {/each}
  </div>

  <section class="card">
    <div class="card-header assess-header">
      <h2>Trade Assessment</h2>
      <label class="switch">
        <input type="checkbox" role="switch" bind:checked={showOutcomes} />
        <span class="track" aria-hidden="true"></span>
        Show {outcomeLabel} outcome views
      </label>
    </div>
    <div class="card-body">
      {#if result}
        <Assessment {result} teamA={tA} teamB={tB} {showOutcomes} {outcomeLabel} />
      {:else}
        <p class="muted">Select picks from one or both teams to assess the trade.</p>
      {/if}
    </div>
  </section>

  <div class="charts" class:three={showOutcomes}>
    <section class="card">
      <div class="card-header chart-header">Expected Pick Value</div>
      <div class="chart-body">
        {#if result}
          <DensityChart values={negate(result.netEvToA)} teamA={tA} teamB={tB} xTitle="Net EPV" hoverLabel="Net EPV" />
        {:else}
          <p class="waiting">Select picks from both teams to assess the trade.</p>
        {/if}
      </div>
    </section>
    {#if showOutcomes}
      <section class="card">
        <div class="card-header chart-header">{outcomeLabel} Outcome Simulation</div>
        <div class="chart-body">
          {#if result}
            <DensityChart values={negate(result.netOutcomeToA)} teamA={tA} teamB={tB}
                          xTitle={`Net ${outcomeLabel} Outcome`} hoverLabel={`Net ${outcomeLabel}`} xCap={50} xStep={10} />
          {:else}
            <p class="waiting">Select picks from both teams to assess the trade.</p>
          {/if}
        </div>
      </section>
      <section class="card">
        <div class="card-header chart-header">Best Player Outcome</div>
        <div class="chart-body">
          {#if result}
            <DensityChart values={negate(result.bestEdgeToA)} teamA={tA} teamB={tB}
                          xTitle={`Best Player ${outcomeLabel} Edge`} hoverLabel="Best-player edge" xCap={50} xStep={10} />
          {:else}
            <p class="waiting">Select picks from both teams to assess the trade.</p>
          {/if}
        </div>
      </section>
    {/if}
  </div>
</div>

<style>
  .tm { display: flex; flex-direction: column; gap: 14px; }
  .muted { color: var(--muted); margin: 0; }
  .error { color: #fca5a5; margin: 0; }
  .verdict {
    padding: 12px 16px;
    border-radius: 10px;
    border: 1px solid var(--border);
    background: rgba(15, 15, 26, 0.72);
    font-size: 17px;
    line-height: 1.5;
    color: var(--text-strong);
  }
  .verdict.accent { border-left-width: 4px; }
  .reset { display: flex; justify-content: center; }

  .two { display: grid; grid-template-columns: 1fr 1fr; gap: 14px; }
  .out-row { gap: 2.5rem; }
  .team-card {
    display: grid;
    grid-template-columns: minmax(140px, 0.62fr) minmax(0, 1.38fr);
    gap: 18px;
    padding: 14px 16px 18px;
  }
  .team-card.right { grid-template-columns: minmax(0, 1.38fr) minmax(140px, 0.62fr); }
  .team-card.right .identity { order: 2; }
  .identity {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 12px;
    min-height: 200px;
    text-align: center;
  }
  .identity img { width: 118px; height: 118px; object-fit: contain; }
  .team-name { font-size: 24px; font-weight: 900; line-height: 1.15; color: var(--text-strong); }
  .controls { display: flex; flex-direction: column; gap: 12px; padding-top: 12px; min-width: 0; }
  .label { font-weight: 800; color: #cfd2dc; margin-bottom: -6px; }

  .out-header { display: flex; align-items: center; gap: 8px; font-size: 18px; }
  .out-header img { width: 28px; height: 28px; object-fit: contain; }

  .assess-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    flex-wrap: wrap;
    gap: 8px 16px;
  }
  h2 { margin: 0; font-size: 24px; font-weight: 900; line-height: 1.15; }
  .switch { display: inline-flex; align-items: center; gap: 10px; font-weight: 600; cursor: pointer; }
  .switch input { position: absolute; opacity: 0; width: 1px; height: 1px; }
  .track {
    position: relative;
    width: 38px;
    height: 22px;
    border-radius: 11px;
    background: #3a3a4f;
    transition: background 0.15s;
    flex: 0 0 auto;
  }
  .track::after {
    content: "";
    position: absolute;
    top: 3px;
    left: 3px;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: #e6e8ef;
    transition: transform 0.15s;
  }
  .switch input:checked + .track { background: var(--primary); }
  .switch input:checked + .track::after { transform: translateX(16px); }
  .switch input:focus-visible + .track { outline: 2px solid var(--link); outline-offset: 2px; }

  .charts { display: grid; grid-template-columns: minmax(0, 1fr); gap: 14px; }
  .charts.three { grid-template-columns: repeat(3, minmax(0, 1fr)); }
  .chart-header { text-align: center; }
  .chart-body { padding: 8px 6px 4px; min-height: 120px; }
  .waiting { margin: 0; padding: 40px 12px; text-align: center; color: var(--muted); font-size: 14px; }

  @media (max-width: 1100px) {
    .charts.three { grid-template-columns: minmax(0, 1fr); }
  }
  @media (max-width: 900px) {
    .two, .out-row { grid-template-columns: minmax(0, 1fr); gap: 14px; }
  }
  @media (max-width: 560px) {
    .team-card, .team-card.right { grid-template-columns: minmax(0, 1fr); gap: 4px; padding: 12px; }
    .team-card.right .identity { order: 0; }
    .identity { flex-direction: row; min-height: 0; gap: 12px; justify-content: flex-start; text-align: left; }
    .identity img { width: 48px; height: 48px; }
    .team-name { font-size: 20px; }
    .controls { padding-top: 6px; }
    .verdict { font-size: 15px; }
    h2 { font-size: 20px; }
    .card-body { padding: 12px; }
  }
</style>
