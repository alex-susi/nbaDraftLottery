// sprintf-style formatting used across the Trade Machine, matching app.R output.

export const fmt1 = (x: number) => (Number.isFinite(x) ? x.toFixed(1) : "—");

export const fmtSigned1 = (x: number) => (Number.isFinite(x) ? `${x >= 0 ? "+" : "-"}${Math.abs(x).toFixed(1)}` : "—");

// format_trade_pct(): whole percent, clamped to [0, 100].
export const fmtPct0 = (p: number | null) =>
  p == null || !Number.isFinite(p) ? "—" : `${(100 * Math.min(Math.max(p, 0), 1)).toFixed(0)}%`;

export const fmtPct1 = (p: number) => (Number.isFinite(p) ? `${(100 * p).toFixed(1)}%` : "—");

// delta_color_app(): green / red / neutral after rounding to one decimal.
export function deltaColor(x: number | null | undefined): string {
  if (x == null || !Number.isFinite(x)) return "#d0d0d0";
  const y = Math.round(x * 10) / 10;
  return y === 0 ? "#d0d0d0" : y > 0 ? "#10b981" : "#ef4444";
}

// fmt_delta1_app(): signed one decimal, "—" for missing.
export const fmtDelta1 = (x: number | null | undefined, suffix = "") =>
  x == null || !Number.isFinite(x) ? "—" : `${fmtSigned1(x)}${suffix}`;

export const pctChange = (cur: number, next: number) => (Math.abs(cur) > 1e-9 ? ((next - cur) / cur) * 100 : NaN);

export function hexToRgba(hex: string, alpha: number): string {
  const h = hex.replace("#", "");
  if (h.length !== 6) return `rgba(109,40,217,${alpha})`;
  const r = parseInt(h.slice(0, 2), 16);
  const g = parseInt(h.slice(2, 4), 16);
  const b = parseInt(h.slice(4, 6), 16);
  return `rgba(${r},${g},${b},${alpha})`;
}
