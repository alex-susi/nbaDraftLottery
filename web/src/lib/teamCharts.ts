// Figure builders for the Summary and Team tabs, ported from app.R's
// epv_leaderboard_plot(), output$impact_chart (pick scatterplot), and
// single_pick_density_plot().

import { fmt1, fmtSigned1 } from "./format";
import type { Layout, Trace } from "./plotly";
import type { Curve, DistStats, LogoPoint, PortfolioRow, SummaryTeam } from "./types";

export interface TeamFigure {
  data: Trace[];
  layout: (width: number) => Layout;
  logos: (width: number) => LogoPoint[];
}

const NARROW = 640;

function median(v: number[]): number {
  const s = [...v].sort((a, b) => a - b);
  const n = s.length;
  return n % 2 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2;
}

const range = (v: number[]) => [Math.min(...v), Math.max(...v)];

// Current vs 3-2-1 total EPV per team: grey tick = current, line to the logo =
// change, wide bar = 80% interval of the change.
export function leaderboardFigure(rows: PortfolioRow[], sortBy: "delta" | "total",
                                  teams: Map<string, SummaryTeam>): TeamFigure {
  const df = rows
    .map((r) => ({
      ...r,
      lo: r.cur_total_value + (r.delta_q10 ?? 0),
      hi: r.cur_total_value + (r.delta_q90 ?? 0),
      key: sortBy === "total" ? r.new_total_value : r.delta_total_value,
    }))
    .sort((a, b) => a.key - b.key)
    .map((r, i) => ({ ...r, y: i + 1 }));

  const data: Trace[] = [];
  for (const r of df) {
    const t = teams.get(r.team)!;
    const hover = `<b>${r.team}</b><br>Total Picks: ${fmt1(r.new_expected_picks)}<br>Current: ${fmt1(r.cur_total_value)} EPV<br>` +
      `3-2-1: ${fmt1(r.new_total_value)} EPV<br>Change: ${fmtSigned1(r.delta_total_value)} EPV<br>80% interval: [${fmt1(r.lo)}, ${fmt1(r.hi)}]` +
      "<extra></extra>";
    const hoverlabel = { bgcolor: t.primary, font: { color: t.text }, align: "right" };
    const col = r.delta_total_value >= 0 ? "#10b981" : "#ef4444";
    data.push(
      { type: "scatter", mode: "lines", x: [r.lo, r.hi], y: [r.y, r.y], customdata: [r.team, r.team],
        line: { color: "rgba(255,255,255,0.16)", width: 6 }, hovertemplate: hover, hoverlabel, showlegend: false },
      { type: "scatter", mode: "lines", x: [r.cur_total_value, r.new_total_value], y: [r.y, r.y], customdata: [r.team, r.team],
        line: { color: col, width: 2 }, opacity: 0.62, hovertemplate: hover, hoverlabel, showlegend: false },
      { type: "scatter", mode: "lines", x: [r.cur_total_value, r.cur_total_value], y: [r.y - 0.16, r.y + 0.16],
        line: { color: "rgba(229,231,235,0.62)", width: 2 }, hoverinfo: "skip", showlegend: false },
      { type: "scatter", mode: "markers", x: [r.new_total_value], y: [r.y], customdata: [r.team],
        marker: { color: "rgba(255,255,255,0.01)", size: 28, line: { color: "rgba(255,255,255,0)", width: 0 } },
        hovertemplate: hover, hoverlabel, showlegend: false, opacity: 0.01 },
    );
  }

  let [lo, hi] = range(df.flatMap((r) => [r.lo, r.hi, r.cur_total_value, r.new_total_value]));
  if (!Number.isFinite(lo) || hi === lo) [lo, hi] = [0, 1];
  const pad = Math.max(1, (hi - lo) * 0.06);

  return {
    data,
    layout: (w) => ({
      xaxis: { title: { text: "Total EPV" }, range: [lo - pad, hi + pad], tickformat: ".1f" },
      yaxis: { title: { text: w < NARROW ? "" : "Team" }, tickmode: "array", tickvals: df.map((r) => r.y),
               ticktext: df.map((r) => r.team), range: [0.5, df.length + 0.5], autorange: false,
               tickfont: { size: w < NARROW ? 9 : 11 } },
      margin: w < NARROW ? { l: 44, r: 14, t: 12, b: 40 } : { l: 76, r: 30, t: 12, b: 36 },
      showlegend: false,
    }),
    logos: (w) => df.map((r) => ({ team: r.team, x: r.new_total_value, y: r.y,
                                   src: teams.get(r.team)?.logo ?? null, size: w < NARROW ? 22 : 34 })),
  };
}

// Average EPV per pick vs expected pick count, with median guides and
// quadrant labels. A clicked team is shown alone; axes stay fixed.
export function scatterFigure(rows: PortfolioRow[], selectedTeam: string | null,
                              teams: Map<string, SummaryTeam>): TeamFigure {
  const all = rows.map((r) => ({ ...r, x: r.new_avg_quality, y: r.new_expected_picks }));
  const shown = selectedTeam && all.some((r) => r.team === selectedTeam) ? all.filter((r) => r.team === selectedTeam) : all;

  let xr = range(all.flatMap((r) => [r.x, r.x_logo]));
  let yr = range(all.flatMap((r) => [r.y, r.y_logo]));
  if (!Number.isFinite(xr[0]) || xr[1] === xr[0]) xr = [0, 1];
  if (!Number.isFinite(yr[0]) || yr[1] === yr[0]) yr = [0, 1];
  const padX = Math.max(0.25, (xr[1] - xr[0]) * 0.14);
  const padY = Math.max(0.75, (yr[1] - yr[0]) * 0.14);
  const xRef = median(all.map((r) => r.x));
  const yRef = median(all.map((r) => r.y));

  const data: Trace[] = shown.map((r) => {
    const t = teams.get(r.team)!;
    return {
      type: "scatter", mode: "markers", x: [r.x_logo], y: [r.y_logo], customdata: [r.team],
      marker: { color: "rgba(255,255,255,0.01)", size: 48, line: { color: "rgba(255,255,255,0)", width: 0 } },
      hovertemplate: `<b>${r.team}</b><br>Avg EPV / pick: ${fmt1(r.new_avg_quality)}<br>Number of picks: ${fmt1(r.new_expected_picks)}<extra></extra>`,
      hoverlabel: { bgcolor: t.primary, font: { color: t.text } },
      showlegend: false, opacity: 0.01,
    };
  });

  const guide = { color: "rgba(255,255,255,0.18)", dash: "dash" };
  return {
    data,
    layout: (w) => {
      const f = w < NARROW ? 10 : 13;
      return {
        xaxis: { title: { text: "Average EPV per pick", font: { size: w < NARROW ? 12 : 15 } }, tickformat: ".1f",
                 tickfont: { size: 12 }, range: [xr[0] - padX, xr[1] + padX] },
        yaxis: { title: { text: "Number of Picks", font: { size: w < NARROW ? 12 : 15 } }, tickformat: ".0f",
                 tickfont: { size: 12 }, range: [yr[0] - padY, yr[1] + padY] },
        shapes: [
          { type: "line", x0: xRef, x1: xRef, y0: yr[0] - padY, y1: yr[1] + padY, line: guide },
          { type: "line", x0: xr[0] - padX, x1: xr[1] + padX, y0: yRef, y1: yRef, line: guide },
        ],
        annotations: [
          { x: xr[1] + padX * 0.08, y: yr[1] + padY * 0.58, text: "<b>High quality / high quantity</b>", showarrow: false, xanchor: "right", font: { size: f, color: "#10f0a5" } },
          { x: xr[1] + padX * 0.08, y: yr[0] - padY * 0.38, text: "<b>High quality / low quantity</b>", showarrow: false, xanchor: "right", font: { size: f, color: "#d1d5db" } },
          { x: xr[0] - padX * 0.08, y: yr[1] + padY * 0.58, text: "<b>Low quality / high quantity</b>", showarrow: false, xanchor: "left", font: { size: f, color: "#d1d5db" } },
          { x: xr[0] - padX * 0.08, y: yr[0] - padY * 0.38, text: "<b>Low quality / low quantity</b>", showarrow: false, xanchor: "left", font: { size: f, color: "#ff6060" } },
        ],
        margin: w < NARROW ? { l: 50, r: 14, t: 40, b: 50 } : { l: 74, r: 34, t: 72, b: 64 },
        showlegend: false,
      };
    },
    logos: (w) => shown.map((r) => ({ team: r.team, x: r.x_logo, y: r.y_logo,
                                      src: teams.get(r.team)?.logo ?? null, size: w < NARROW ? 26 : 40 })),
  };
}

// Single pick: current vs 3-2-1 densities with dashed mean lines.
export function singlePickFigure(cur: Curve, next: Curve, stats: DistStats, xTitle: string,
                                 hoverLabel: string, unit: string): { data: Trace[]; layout: (w: number) => Layout } {
  return {
    data: [
      { type: "scatter", mode: "lines", x: cur.x, y: cur.y, name: "Current", fill: "tozeroy",
        line: { color: "#3b82f6", width: 2.5 },
        hovertemplate: `Current ${hoverLabel}<br>${unit}: %{x:.1f}<br>Density: %{y:.1f}<extra></extra>` },
      { type: "scatter", mode: "lines", x: next.x, y: next.y, name: "3-2-1", fill: "tozeroy",
        line: { color: "#f59e0b", width: 2.5 },
        hovertemplate: `3-2-1 ${hoverLabel}<br>${unit}: %{x:.1f}<br>Density: %{y:.1f}<extra></extra>` },
    ],
    layout: (w) => ({
      xaxis: { title: { text: xTitle } },
      yaxis: { title: { text: "Density" } },
      legend: { x: w < NARROW ? 0.55 : 0.65, y: 0.95, font: { size: 10 } },
      shapes: [
        { type: "line", x0: stats.cur_mean, x1: stats.cur_mean, y0: 0, y1: 1, yref: "paper", line: { color: "#3b82f6", dash: "dash" } },
        { type: "line", x0: stats.new_mean, x1: stats.new_mean, y0: 0, y1: 1, yref: "paper", line: { color: "#f59e0b", dash: "dash" } },
      ],
      margin: w < NARROW ? { l: 50, r: 12, t: 20, b: 50 } : { l: 64, r: 24, t: 24, b: 56 },
    }),
  };
}
