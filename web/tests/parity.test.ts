// Recomputes the reference trades from web/scripts/parity_reference.R with the
// exported data and checks every number against app.R's own results.

import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { decodeCore, decodeTeamBundle } from "../src/lib/data";
import { mean, quantiles } from "../src/lib/stats";
import { bestShare, controlKey, makeContext, summarizeNet, valueTrade } from "../src/lib/trade";
import type { Meta, PickControl } from "../src/lib/types";

const dataDir = join(__dirname, "..", "public", "data");
const readBuffer = (path: string): ArrayBuffer => {
  const b = readFileSync(join(dataDir, path));
  return b.buffer.slice(b.byteOffset, b.byteOffset + b.byteLength) as ArrayBuffer;
};

const meta = JSON.parse(readFileSync(join(dataDir, "meta.json"), "utf8")) as Meta;
const ctx = makeContext(meta, decodeCore(meta, readBuffer("core.bin")));
for (const t of meta.teams) {
  ctx.bundles.set(t.abbr, decodeTeamBundle(meta.team_files[t.abbr], meta.n_draws, readBuffer(meta.team_files[t.abbr].file)));
}

const reference = JSON.parse(readFileSync(join(__dirname, "parity_reference.json"), "utf8"));

// Draws are stored as float32, so values agree with R to about 1e-6.
const TOL = 1e-4;
const asArray = (x: string[] | string | null | undefined): string[] => (x == null ? [] : Array.isArray(x) ? x : [x]);

describe("reference data", () => {
  it("was generated from the current export", () => {
    expect(reference.export_generated).toBe(meta.generated);
  });
});

describe.each(reference.trades as any[])("$name", (tr) => {
  const picksA = asArray(tr.picksA);
  const picksB = asArray(tr.picksB);

  // Shiny control ids are prot_<side>_<safe id> / swap_<side>_<safe id>.
  const controls: Record<string, PickControl> = {};
  for (const [side, ids] of [["A", picksA], ["B", picksB]] as const) {
    for (const id of ids) {
      const safe = id.replace(/[^A-Za-z0-9]/g, "_");
      controls[controlKey(side, id)] = {
        protection: tr.controls?.[`prot_${side}_${safe}`] ?? "none",
        swap: tr.controls?.[`swap_${side}_${safe}`] === true,
      };
    }
  }
  const result = valueTrade(ctx, tr.teamA, tr.teamB, picksA, picksB, controls)!;

  it("matches each pick row", () => {
    expect(result.picks.length).toBe(tr.picks.length);
    tr.picks.forEach((ref: any, k: number) => {
      const p = result.picks[k];
      expect(p.id).toBe(ref.id);
      if (ref.convey == null) expect(p.conveyProb).toBeNull();
      else expect(p.conveyProb!).toBeCloseTo(ref.convey, 6);
      const [q10, q90] = quantiles(p.final, [0.1, 0.9]);
      expect(Math.abs(mean(p.initial) - ref.initial)).toBeLessThan(TOL);
      expect(Math.abs(mean(p.final) - mean(p.initial) - ref.impact)).toBeLessThan(TOL);
      expect(Math.abs(mean(p.final) - ref.final)).toBeLessThan(TOL);
      expect(Math.abs(q10 - ref.final_q10)).toBeLessThan(TOL);
      expect(Math.abs(q90 - ref.final_q90)).toBeLessThan(TOL);
      expect(Math.abs(mean(p.outcome) - ref.outcome)).toBeLessThan(TOL);
    });
  });

  it("matches the trade summaries", () => {
    for (const [mine, ref] of [[summarizeNet(result.netEvToA), tr.net_ev],
                               [summarizeNet(result.netOutcomeToA), tr.net_outcome]] as const) {
      expect(Math.abs(mine.mean - ref.mean)).toBeLessThan(TOL);
      expect(Math.abs(mine.q05 - ref.q05)).toBeLessThan(TOL);
      expect(Math.abs(mine.q95 - ref.q95)).toBeLessThan(TOL);
      expect(mine.pPos).toBeCloseTo(ref.p_pos, 6);
      expect(mine.pNeg).toBeCloseTo(ref.p_neg, 6);
    }
    const a = bestShare(result.bestToA);
    const b = bestShare(result.bestToB);
    expect(a.p).toBeCloseTo(tr.best.p_A, 6);
    expect(b.p).toBeCloseTo(tr.best.p_B, 6);
    expect(a.ci[0]).toBeCloseTo(tr.best.ci_A[0], 6);
    expect(a.ci[1]).toBeCloseTo(tr.best.ci_A[1], 6);
    expect(b.ci[0]).toBeCloseTo(tr.best.ci_B[0], 6);
    expect(b.ci[1]).toBeCloseTo(tr.best.ci_B[1], 6);
    expect(Math.abs(mean(result.bestEdgeToA) - tr.best.edge_mean)).toBeLessThan(TOL);
  });
});
