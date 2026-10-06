<script lang="ts">
  import { fmt1, fmtPct0 } from "../lib/format";
  import { mean } from "../lib/stats";
  import { controlKey, type PickResult } from "../lib/trade";
  import type { Choice, DisplayPick, PickControl, Side } from "../lib/types";

  interface Props {
    side: Side;
    receiver: string;
    picks: PickResult[];
    displays: Map<string, DisplayPick>;
    controls: Record<string, PickControl>;
    choicesFor: (round: number) => Choice[];
    onControl: (id: string, control: PickControl) => void;
  }
  let { side, receiver, picks, displays, controls, choicesFor, onControl }: Props = $props();

  const rows = $derived(picks.map((p) => {
    const initial = mean(p.initial);
    const final = mean(p.final);
    return { p, d: displays.get(p.id)!, initial, impact: final - initial, final };
  }));
  const totals = $derived({
    initial: rows.reduce((s, r) => s + r.initial, 0),
    impact: rows.reduce((s, r) => s + r.impact, 0),
    final: rows.reduce((s, r) => s + r.final, 0),
  });

  const control = (id: string): PickControl => controls[controlKey(side, id)] ?? { protection: "none", swap: false };
</script>

{#if rows.length === 0}
  <p class="empty">No picks selected</p>
{:else}
  <div class="table" role="table" aria-label={`Picks sent to ${receiver}`}>
    <div class="row head" role="row">
      <span role="columnheader">Picks</span>
      <span role="columnheader">Protection</span>
      <span role="columnheader" class="c">Swap<br />Right</span>
      <span role="columnheader" class="r">Convey<br />to&nbsp;{receiver}</span>
      <span role="columnheader" class="r">Initial<br />EPV</span>
      <span role="columnheader" class="r">Impact<br />EPV</span>
      <span role="columnheader" class="r">Final<br />EPV</span>
    </div>
    {#each rows as { p, d, initial, impact, final } (p.id)}
      {@const ctl = control(p.id)}
      <div class="row" role="row">
        <span class="pick" role="cell"><strong>{d.trade_label}</strong></span>
        <span class="prot" role="cell">
          <span class="m-label">Protection</span>
          {#if p.editable}
            <select class="select" aria-label={`Protection for ${d.trade_label}`}
                    value={ctl.protection}
                    onchange={(e) => onControl(p.id, { ...ctl, protection: e.currentTarget.value })}>
              {#each choicesFor(d.round) as c (c.value)}
                <option value={c.value}>{c.label}</option>
              {/each}
            </select>
          {:else}
            <span class="na">N/A</span>
          {/if}
        </span>
        <span class="swap c" role="cell">
          <span class="m-label">Swap right</span>
          <input type="checkbox" aria-label={`Swap right for ${d.trade_label}`}
                 disabled={!p.editable} checked={p.editable && ctl.swap}
                 onchange={(e) => onControl(p.id, { ...ctl, swap: e.currentTarget.checked })} />
        </span>
        <span class="r num" role="cell"><span class="m-label">Convey to {receiver}</span>{fmtPct0(p.conveyProb)}</span>
        <span class="r num" role="cell"><span class="m-label">Initial EPV</span>{fmt1(initial)}</span>
        <span class="r num" role="cell"><span class="m-label">Impact EPV</span>{fmt1(impact)}</span>
        <span class="r num final" role="cell"><span class="m-label">Final EPV</span>{fmt1(final)}</span>
      </div>
    {/each}
    <div class="row total" role="row">
      <span role="cell">Total</span>
      <span class="spacer" role="cell"></span>
      <span class="spacer" role="cell"></span>
      <span class="spacer" role="cell"></span>
      <span class="r num" role="cell"><span class="m-label">Initial EPV</span>{fmt1(totals.initial)}</span>
      <span class="r num" role="cell"><span class="m-label">Impact EPV</span>{fmt1(totals.impact)}</span>
      <span class="r num final" role="cell"><span class="m-label">Final EPV</span>{fmt1(totals.final)}</span>
    </div>
  </div>
{/if}

<style>
  .empty { margin: 4px 0; font-size: 13px; color: #666; }
  .table { container-type: inline-size; font-size: 14px; }
  .row {
    display: grid;
    grid-template-columns: 27% 20% 8% 12% 10% 13% 10%;
    align-items: center;
    padding: 8px 0;
    border-bottom: 1px solid var(--border);
  }
  .row > * { padding: 0 4px; min-width: 0; }
  .head {
    font-family: var(--mono);
    font-size: 12px;
    font-weight: 600;
    color: #cfd2dc;
    align-items: end;
  }
  .pick { overflow-wrap: anywhere; font-weight: 700; }
  .r { text-align: right; }
  .c { text-align: center; }
  .na { color: #777; }
  .total { font-weight: 800; border-bottom: 0; }
  .m-label { display: none; }
  input[type="checkbox"] { width: 18px; height: 18px; accent-color: var(--side-color); }
  .select { min-height: 36px; padding-top: 5px; padding-bottom: 5px; font-size: 13px; }

  /* Narrow cards: each pick becomes a small card with labeled values. */
  @container (max-width: 560px) {
    .head { display: none; }
    .row {
      grid-template-columns: 1fr 1fr;
      gap: 8px 12px;
      padding: 12px 0;
    }
    .pick { grid-column: 1 / -1; font-size: 15px; }
    .r, .c { text-align: left; }
    .m-label {
      display: block;
      font-family: var(--font);
      font-size: 11px;
      color: var(--muted);
      margin-bottom: 2px;
    }
    .swap { display: flex; flex-direction: column; align-items: flex-start; }
    .total { grid-template-columns: repeat(3, 1fr); }
    .total .spacer { display: none; }
    .total > :first-child { grid-column: 1 / -1; }
    .final { font-weight: 800; }
  }
</style>
