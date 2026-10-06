// Trade Machine valuation, ported from the server functions in app.R
// (sent_pick_ev_value, sent_pick_outcome_value, sent_display_*, trade_draws).
// Everything is valued under the 3-2-1 system, one value per retained
// simulation, with simulations kept in the export's row order.
//
// NA handling follows R: NaN plays the role of NA, and every place app.R
// replaces NA with 0 does the same here.

import { mean, meanBool, qbeta, quantiles } from "./stats";
import type { Core, DisplayPick, Meta, PickControl, Side, TeamBundle } from "./types";

export interface TradeContext {
  meta: Meta;
  core: Core;
  displays: Map<string, DisplayPick>;
  bundles: Map<string, TeamBundle>; // keyed by owning team
}

export interface PickResult {
  id: string;
  side: Side;
  final: Float64Array; // EPV sent to the receiver after toggles
  initial: Float64Array; // EPV of the underlying pick before toggles
  outcome: Float64Array; // realized player outcome sent
  conveyProb: number | null;
  editable: boolean;
}

export interface TradeResult {
  picks: PickResult[];
  netEvToA: Float64Array;
  netOutcomeToA: Float64Array;
  bestToA: Uint8Array;
  bestToB: Uint8Array;
  bestEdgeToA: Float64Array;
}

export function makeContext(meta: Meta, core: Core): TradeContext {
  return { meta, core, displays: new Map(meta.displays.map((d) => [d.id, d])), bundles: new Map() };
}

export function controlKey(side: Side, id: string): string {
  return `${side}:${id}`;
}

// normalize_protection_for_round(): anything not offered for the round is "none".
export function normalizeProtection(meta: Meta, protection: string | undefined, round: number): string {
  const choices = meta.protection_choices[round === 2 ? "2" : "1"];
  return protection && choices.some((c) => c.value === protection) ? protection : "none";
}

// pick_conveys_app(): does a pick at slot `pos` convey under `protection`?
// Returns null for an NA slot, as R would.
function makeConveys(protection: string): (pos: number) => boolean | null {
  const top: Record<string, number> = {
    top1: 1, top2: 2, top3: 3, top4: 4, top5: 5, top6: 6, top8: 8,
    top10: 10, lottery: 14, top16: 16, top20: 20,
  };
  if (protection in top) {
    const k = top[protection];
    return (pos) => (Number.isNaN(pos) ? null : pos > k);
  }
  let m = /^protected(\d+)_(\d+)$/.exec(protection);
  if (m) {
    const lo = +m[1], hi = +m[2];
    return (pos) => (Number.isNaN(pos) ? null : !(pos >= lo && pos <= hi));
  }
  m = /^convey(\d+)_(\d+)$/.exec(protection);
  if (m) {
    const lo = +m[1], hi = +m[2];
    return (pos) => (Number.isNaN(pos) ? null : pos >= lo && pos <= hi);
  }
  return () => true;
}

function slotAt(u8: Uint8Array | null, i: number): number {
  if (!u8) return NaN;
  const v = u8[i];
  return v === 0 ? NaN : v;
}

// slot_value_vec(): value a slot under simulation i's mean pick-value curve.
function slotValue(core: Core, i: number, slot: number): number {
  if (Number.isNaN(slot)) return NaN;
  const s = Math.min(Math.max(Math.trunc(slot), 1), 60);
  return core.mu[i * 60 + s - 1];
}

const nz = (v: number) => (Number.isNaN(v) ? 0 : v);

// receiver_slot_vec(): the receiver's own projected slot in that year and round.
function receiverSlots(ctx: TradeContext, receiver: string, year: number, round: number): Uint8Array | null {
  const { meta, core } = ctx;
  const t = meta.slot_teams.indexOf(receiver);
  const y = meta.proj_years.indexOf(year);
  if (t < 0 || y < 0) return null;
  const n = meta.n_draws;
  const arr = round === 2 ? core.teamSlot2 : core.teamSlot1;
  const start = (y * meta.slot_teams.length + t) * n;
  return arr.subarray(start, start + n);
}

// receiver_raw_outcome_vec(): the receiver's own pick's raw outcome draws, or
// the slot-curve value of the receiver's slot when that pick has no draws.
function receiverRaw(ctx: TradeContext, receiver: string, year: number, round: number,
                     rcSlots: Uint8Array | null): Float64Array {
  const n = ctx.meta.n_draws;
  const out = new Float64Array(n);
  const aid = ctx.meta.receiver_raw[receiver]?.[`${year}-${round}`];
  const raw = aid ? ctx.bundles.get(receiver)?.f32.get(`a:${aid}:raw`) : undefined;
  if (aid && !raw) throw new Error(`Draws for ${receiver} are not loaded`);
  for (let i = 0; i < n; i++) {
    out[i] = raw ? nz(raw[i]) : nz(slotValue(ctx.core, i, slotAt(rcSlots, i)));
  }
  return out;
}

function bundleFor(ctx: TradeContext, d: DisplayPick): TeamBundle {
  const b = ctx.bundles.get(d.owner);
  if (!b) throw new Error(`Draws for ${d.owner} are not loaded`);
  return b;
}

export function isEditable(d: DisplayPick): boolean {
  return d.single_leg && d.year !== 2026;
}

export function valuePick(ctx: TradeContext, id: string, side: Side, receiver: string,
                          control: PickControl | undefined): PickResult {
  const d = ctx.displays.get(id);
  if (!d) throw new Error(`Unknown pick ${id}`);
  const n = ctx.meta.n_draws;
  const final = new Float64Array(n);
  const initial = new Float64Array(n);
  const outcome = new Float64Array(n);
  const editable = isEditable(d);

  // No member asset has draws: app.R returns zeros and an NA convey probability.
  if (d.n_legs === 0) {
    return { id, side, final, initial, outcome, conveyProb: null, editable };
  }

  const bundle = bundleFor(ctx, d);

  // Grouped entitlements: display-level draws, no toggles, initial = final.
  if (!d.single_leg) {
    const ev = bundle.f32.get(`d:${id}:ev`)!;
    const out = bundle.f32.get(`d:${id}:out`)!;
    for (let i = 0; i < n; i++) {
      final[i] = nz(ev[i]);
      initial[i] = final[i];
      outcome[i] = nz(out[i]);
    }
    return { id, side, final, initial, outcome, conveyProb: d.convey_prob_static, editable };
  }

  const aid = d.asset_id!;
  const slot = bundle.u8.get(`a:${aid}:slot`)!;
  const convey = bundle.u8.get(`a:${aid}:convey`)!;
  const out = bundle.f32.get(`a:${aid}:out`)!;
  const raw = bundle.f32.get(`a:${aid}:raw`)!;

  // 2026 slots are locked: fixed-slot value, outcome draws as exported.
  if (d.year === 2026 && d.fixed_slot != null) {
    for (let i = 0; i < n; i++) {
      final[i] = nz(slotValue(ctx.core, i, d.fixed_slot));
      initial[i] = final[i];
      outcome[i] = nz(out[i]);
    }
    return { id, side, final, initial, outcome, conveyProb: d.guaranteed ? d.convey_prob_static : 1, editable };
  }

  const prot = normalizeProtection(ctx.meta, control?.protection, d.round);
  const swap = !!control?.swap;
  const conveys = makeConveys(prot);

  for (let i = 0; i < n; i++) initial[i] = nz(slotValue(ctx.core, i, slotAt(slot, i)));

  let conveyCount = 0;
  if (swap) {
    // A swap right sends only the gain over the receiver's own pick, and only
    // when the sent pick is eligible and lands ahead of it.
    const rc = receiverSlots(ctx, receiver, d.year, d.round);
    const rcRaw = receiverRaw(ctx, receiver, d.year, d.round, rc);
    for (let i = 0; i < n; i++) {
      const og = slotAt(slot, i);
      const r = slotAt(rc, i);
      const exercised = conveys(og) === true && !Number.isNaN(og) && !Number.isNaN(r) && og < r;
      if (exercised) {
        conveyCount++;
        final[i] = nz(Math.max(slotValue(ctx.core, i, og) - slotValue(ctx.core, i, r), 0));
        outcome[i] = nz(raw[i] - rcRaw[i]);
      }
    }
  } else if (prot !== "none") {
    // A user-added protection decides conveyance from the slot alone.
    for (let i = 0; i < n; i++) {
      const og = slotAt(slot, i);
      if (!Number.isNaN(og) && conveys(og) === true) {
        conveyCount++;
        final[i] = nz(slotValue(ctx.core, i, og));
        outcome[i] = nz(raw[i]);
      }
    }
  } else {
    for (let i = 0; i < n; i++) {
      const og = slotAt(slot, i);
      if (convey[i] > 0) conveyCount++;
      final[i] = !Number.isNaN(og) && convey[i] > 0 ? nz(slotValue(ctx.core, i, og)) : 0;
      outcome[i] = nz(out[i]);
    }
  }

  const conveyProb = d.guaranteed ? d.convey_prob_static : conveyCount / n;
  return { id, side, final, initial, outcome, conveyProb, editable };
}

export function valueTrade(ctx: TradeContext, teamA: string, teamB: string,
                           picksA: string[], picksB: string[],
                           controls: Record<string, PickControl>): TradeResult | null {
  if (picksA.length === 0 && picksB.length === 0) return null;
  const n = ctx.meta.n_draws;
  const picks = [
    ...picksA.map((id) => valuePick(ctx, id, "A", teamB, controls[controlKey("A", id)])),
    ...picksB.map((id) => valuePick(ctx, id, "B", teamA, controls[controlKey("B", id)])),
  ];

  const netEvToA = new Float64Array(n);
  const netOutcomeToA = new Float64Array(n);
  const bestA = new Float64Array(n).fill(-Infinity); // best outcome Team 1 receives
  const bestB = new Float64Array(n).fill(-Infinity);
  for (const p of picks) {
    const sign = p.side === "B" ? 1 : -1;
    const best = p.side === "B" ? bestA : bestB;
    for (let i = 0; i < n; i++) {
      netEvToA[i] += sign * p.final[i];
      netOutcomeToA[i] += sign * p.outcome[i];
      if (p.outcome[i] > best[i]) best[i] = p.outcome[i];
    }
  }

  const hasA = picksB.length > 0;
  const hasB = picksA.length > 0;
  const bestToA = new Uint8Array(n);
  const bestToB = new Uint8Array(n);
  const bestEdgeToA = new Float64Array(n);
  for (let i = 0; i < n; i++) {
    bestToA[i] = hasA && (!hasB || bestA[i] > bestB[i]) ? 1 : 0;
    bestToB[i] = hasB && (!hasA || bestB[i] > bestA[i]) ? 1 : 0;
    bestEdgeToA[i] = (hasA ? bestA[i] : 0) - (hasB ? bestB[i] : 0);
  }
  return { picks, netEvToA, netOutcomeToA, bestToA, bestToB, bestEdgeToA };
}

export interface NetSummary {
  mean: number;
  q05: number;
  q95: number;
  pPos: number;
  pNeg: number;
}

export function summarizeNet(x: Float64Array): NetSummary {
  const [q05, q95] = quantiles(x, [0.05, 0.95]);
  let pos = 0, neg = 0;
  for (const v of x) {
    if (v > 0) pos++;
    else if (v < 0) neg++;
  }
  return { mean: mean(x), q05, q95, pPos: pos / x.length, pNeg: neg / x.length };
}

// best_prob_ci() in app.R: 90% Jeffreys-style beta interval for a share.
export function bestShare(x: Uint8Array): { p: number; ci: [number, number] } {
  const k = x.reduce((s, v) => s + v, 0);
  const nn = x.length;
  return { p: meanBool(x), ci: [qbeta(0.05, k + 1, nn - k + 1), qbeta(0.95, k + 1, nn - k + 1)] };
}
