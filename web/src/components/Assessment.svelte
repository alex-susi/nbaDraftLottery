<script lang="ts">
  // output$tm_verdict in app.R: EPV block, plus outcome and best-player blocks
  // when the outcome views are switched on.
  import { fmtPct1, fmtSigned1 } from "../lib/format";
  import { bestShare, summarizeNet, type NetSummary, type TradeResult } from "../lib/trade";
  import type { Team } from "../lib/types";

  interface Props {
    result: TradeResult;
    teamA: Team;
    teamB: Team;
    showOutcomes: boolean;
    outcomeLabel: string;
  }
  let { result, teamA, teamB, showOutcomes, outcomeLabel }: Props = $props();

  const ev = $derived(summarizeNet(result.netEvToA));
  const outcome = $derived(summarizeNet(result.netOutcomeToA));
  const best = $derived.by(() => {
    const a = bestShare(result.bestToA);
    const b = bestShare(result.bestToB);
    const toA = a.p >= b.p;
    return { team: toA ? teamA : teamB, p: Math.max(a.p, b.p), ci: toA ? a.ci : b.ci };
  });

  // signed_metric_card(): show the side that gains, with its 90% interval.
  function signed(s: NetSummary) {
    const toA = s.mean >= 0;
    return {
      team: toA ? teamA : teamB,
      shown: Math.abs(s.mean),
      lo: toA ? s.q05 : -s.q95,
      hi: toA ? s.q95 : -s.q05,
    };
  }
</script>

{#snippet metricCard(s: NetSummary, label: string)}
  {@const m = signed(s)}
  <div class="metric" style:border-color={m.team.primary} style:border-left-color={m.team.secondary}>
    {#if m.team.logo}<img src={m.team.logo} alt={m.team.name} width="64" height="64" />{/if}
    <div class="metric-text">
      <div class="big" style:color={m.team.secondary}>{m.team.abbr} {fmtSigned1(m.shown)} {label}</div>
      <div class="ci">90% CI: [{fmtSigned1(m.lo)}, {fmtSigned1(m.hi)}] {label}</div>
    </div>
  </div>
{/snippet}

{#snippet probCard(label: string, p: number, team: Team)}
  <div class="prob" style:border-color={team.primary} style:border-left-color={team.secondary}>
    <div class="prob-label">{label}</div>
    <div class="prob-value num" style:color={team.secondary}>{fmtPct1(p)}</div>
  </div>
{/snippet}

<div class="grid" class:three={showOutcomes}>
  <div class="block">
    {@render metricCard(ev, "EPV")}
    <div class="probs">
      {@render probCard(`${teamA.abbr} higher EPV`, ev.pPos, teamA)}
      {@render probCard(`${teamB.abbr} higher EPV`, ev.pNeg, teamB)}
    </div>
    <p class="blurb">Expected Pick Value compares the typical value of the draft assets each side receives, using the model's pick-value curve.</p>
  </div>

  {#if showOutcomes}
    <div class="block">
      {@render metricCard(outcome, outcomeLabel)}
      <div class="probs">
        {@render probCard(`${teamA.abbr} more ${outcomeLabel}`, outcome.pPos, teamA)}
        {@render probCard(`${teamB.abbr} more ${outcomeLabel}`, outcome.pNeg, teamB)}
      </div>
      <p class="blurb">{outcomeLabel} shows the simulated player outcomes those picks could become over the four rookie-contract seasons.</p>
    </div>

    <div class="block">
      <div class="metric tall" style:border-color={best.team.primary} style:border-left-color={best.team.secondary}>
        {#if best.team.logo}<img src={best.team.logo} alt={best.team.name} width="64" height="64" />{/if}
        <div class="metric-text">
          <div class="big" style:color={best.team.secondary}>{best.team.abbr} {fmtPct1(best.p)} Best Player</div>
          <div class="ci">90% CI: [{fmtPct1(best.ci[0])}, {fmtPct1(best.ci[1])}]</div>
        </div>
      </div>
      <p class="blurb">Best Player estimates which side is more likely to receive the single best {outcomeLabel} player outcome among the picks in the trade.</p>
    </div>
  {/if}
</div>

<style>
  .grid {
    display: grid;
    grid-template-columns: minmax(0, 620px);
    gap: 12px;
  }
  .grid.three { grid-template-columns: repeat(3, minmax(0, 1fr)); }
  @media (max-width: 1100px) {
    .grid.three { grid-template-columns: minmax(0, 1fr); }
  }
  .block { display: flex; flex-direction: column; gap: 8px; min-width: 0; }
  .metric {
    display: flex;
    align-items: center;
    gap: 14px;
    min-height: 104px;
    padding: 14px;
    border: 1px solid;
    border-left-width: 4px;
    border-radius: 10px;
    background: rgba(15, 15, 26, 0.72);
  }
  .metric.tall { flex: 1; min-height: 104px; }
  .metric img { width: 64px; height: 64px; object-fit: contain; flex: 0 0 auto; }
  .metric-text { min-width: 0; }
  .big { font-size: clamp(20px, 2.4vw, 28px); line-height: 1.1; font-weight: 850; }
  .ci { margin-top: 6px; font-size: 13px; color: #aaa; }
  .probs { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; }
  .prob {
    min-width: 0;
    padding: 10px 13px;
    border: 1px solid;
    border-left-width: 3px;
    border-radius: 8px;
  }
  .prob-label { font-size: 11px; color: #888; overflow-wrap: anywhere; }
  .prob-value { font-size: 22px; line-height: 1.15; font-weight: 600; }
  .blurb { margin: 2px 2px 0; font-size: 12px; color: var(--muted); line-height: 1.45; }
  @media (max-width: 480px) {
    .metric { min-height: 0; padding: 12px; gap: 10px; }
    .metric img { width: 48px; height: 48px; }
  }
</style>
