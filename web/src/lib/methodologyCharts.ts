// Figure builders for the Methodology tab, ported from the renderPlotly()
// outputs in app.R (pick_curve_plot_combined, methodology_transition_heatmap,
// rank_horizon_plot, lottery_line, lottery_bar). Each returns traces plus a
// layout function of the container width, so phones get a legend below the
// plot instead of a wide right margin.

import { ribbon, type Layout, type Trace } from "./plotly";
import type { Methodology } from "./types";

export interface Figure {
  data: Trace[];
  layout: (width: number) => Layout;
}

const NARROW = 640;
const f1 = (v: number | null) => (v == null ? "NA" : v.toFixed(1));
const pad2 = (n: number) => String(n).padStart(2, "0");

// Legend to the right on wide screens (as in app.R), below the plot on phones.
const sideLegend = (w: number, belowY = -0.22): Layout =>
  w < NARROW
    ? { orientation: "h", x: 0, y: belowY, xanchor: "left", yanchor: "top", font: { size: 9 } }
    : { orientation: "v", x: 1.08, y: 0.5, xanchor: "left", yanchor: "middle", font: { size: 9 },
        bgcolor: "rgba(15,15,26,0.86)", bordercolor: "rgba(255,255,255,0.08)", borderwidth: 1 };

const topLegend: Layout = {
  orientation: "h", x: 0.02, y: 1.02, xanchor: "left", yanchor: "bottom", font: { size: 10 },
  bgcolor: "rgba(15,15,26,0.86)", bordercolor: "rgba(255,255,255,0.08)", borderwidth: 1,
};

export function pickCurveFigure(m: Methodology): Figure {
  const pc = m.pick_curve;
  const unit = m.value_outcome;
  const x = pc.map((r) => r.pick);
  const evLo = pc.map((r) => r.ev_q05 ?? r.expected_war);
  const evHi = pc.map((r) => r.ev_q95 ?? r.expected_war);
  const data: Trace[] = [
    ...ribbon(x, evLo, evHi, {
      name: "90% EV credible interval",
      fillcolor: "rgba(109,40,217,0.16)",
      text: pc.map((r, i) => `Pick ${r.pick}<br>EV: ${f1(r.expected_war)}<br>90% EV CI: [${f1(evLo[i])}, ${f1(evHi[i])}]`),
      hovertemplate: "%{text}<extra></extra>",
    }),
  ];

  const oc = pc.filter((r) => r.outcome_q10 != null && r.outcome_q90 != null);
  if (oc.length) {
    data.push(...ribbon(oc.map((r) => r.pick), oc.map((r) => r.outcome_q10!), oc.map((r) => r.outcome_q90!), {
      name: "Player outcomes 10th-90th",
      fillcolor: "rgba(245,158,11,0.13)",
      text: oc.map((r) => `Pick ${r.pick}<br>Player outcome 10th-90th: [${f1(r.outcome_q10)}, ${f1(r.outcome_q90)}]`),
      hovertemplate: "%{text}<extra></extra>",
    }));
  }

  if (pc.some((r) => r.emp_mean != null)) {
    data.push({
      type: "scatter", mode: "markers", x, y: pc.map((r) => r.emp_mean), name: "Empirical slot mean",
      marker: { color: "#f59e0b", size: 6 },
      hovertemplate: "Pick %{x}<br>Empirical mean: %{y:.1f}<extra></extra>",
    });
  }

  data.push({
    type: "scatter", mode: "lines", x, y: pc.map((r) => r.expected_war), name: "Posterior mean / EV",
    line: { color: "#6d28d9", width: 2.5 },
    hovertemplate: "Pick %{x}<br>EV: %{y:.1f}<extra></extra>",
  });

  // Second-round play probability on the right-hand axis.
  const r2 = pc.filter((r) => r.pick >= 31 && r.p_play != null);
  if (r2.length) {
    const band = r2.filter((r) => r.p_play_q05 != null && r.p_play_q95 != null);
    if (band.length) {
      data.push(...ribbon(band.map((r) => r.pick), band.map((r) => 100 * r.p_play_q05!), band.map((r) => 100 * r.p_play_q95!), {
        name: "90% P(play) interval",
        fillcolor: "rgba(16,185,129,0.13)",
        yaxis: "y2",
        hovertemplate: "Pick %{x}<br>90% P(play): %{y:.1f}%<extra></extra>",
      }));
    }
    data.push({
      type: "scatter", mode: "lines", x: r2.map((r) => r.pick), y: r2.map((r) => 100 * r.p_play!),
      name: "Modeled P(play) %", yaxis: "y2", line: { color: "#10b981", width: 2, dash: "dot" },
      hovertemplate: "Pick %{x}<br>P(play): %{y:.1f}%<extra></extra>",
    });
    const emp = r2.filter((r) => r.emp_p_play != null);
    if (emp.length) {
      data.push({
        type: "scatter", mode: "markers", x: emp.map((r) => r.pick), y: emp.map((r) => 100 * r.emp_p_play!),
        name: "Empirical P(play) %", yaxis: "y2", marker: { color: "#e5e7eb", size: 6, symbol: "x" },
        hovertemplate: "Pick %{x}<br>Empirical P(play): %{y:.1f}%<extra></extra>",
      });
    }
  }

  return {
    data,
    layout: (w) => ({
      shapes: [{ type: "line", x0: 30.5, x1: 30.5, xref: "x", y0: 0, y1: 1, yref: "paper",
                 line: { color: "rgba(229,231,235,0.55)", width: 1.2, dash: "dash" } }],
      annotations: [
        { x: 15.5, y: 1.04, xref: "x", yref: "paper", text: "Round 1", showarrow: false, font: { color: "#aaa", size: 11 } },
        { x: 45.5, y: 1.04, xref: "x", yref: "paper", text: "Round 2", showarrow: false, font: { color: "#aaa", size: 11 } },
      ],
      xaxis: { title: { text: "Pick" }, dtick: w < NARROW ? 10 : 5, range: [1, 60] },
      yaxis: { title: { text: unit }, tickformat: ".1f", gridcolor: "#1a1a2a", zerolinecolor: "#333" },
      // Headroom above 100% so markers at exactly 100% aren't clipped; ticks stop at 100.
      yaxis2: { title: { text: "P(play) %", standoff: 18 }, overlaying: "y", side: "right", range: [0, 106],
                tickmode: "array", tickvals: [0, 20, 40, 60, 80, 100],
                automargin: true, gridcolor: "rgba(0,0,0,0)", zerolinecolor: "#333" },
      legend: sideLegend(w, -0.2),
      margin: w < NARROW ? { l: 56, r: 50, t: 40, b: 50 } : { l: 70, r: 245, t: 58, b: 58 },
    }),
  };
}

export function tierHeatmapFigure(m: Methodology): Figure {
  const { from, to, pct } = m.tier_matrix;
  // app.R reverses the row order and then reverses the axis, so the last tier
  // (Playoff) sits on top.
  const yOrder = from.map((_, i) => from.length - 1 - i);
  const z = yOrder.map((i) => pct[i]);
  return {
    data: [{
      type: "heatmap", x: to, y: yOrder.map((i) => from[i]), z, coloraxis: "coloraxis",
      text: z.map((row) => row.map((v) => `${v.toFixed(1)}%`)),
      texttemplate: "%{text}", textfont: { size: 10, color: "#ddd" },
      hovertemplate: "From %{y} <br>To %{x} <br>Probability: %{z:.1f}%<extra></extra>",
    }],
    layout: (w) => ({
      // The cells carry their own labels, so phones drop the color bar.
      coloraxis: { colorscale: [[0, "#0f0f1a"], [1, "#6d28d9"]], showscale: w >= NARROW },
      xaxis: { title: { text: "Year t + 1", standoff: 24 }, side: "bottom", automargin: true, type: "category" },
      yaxis: { title: { text: "Year t", standoff: w < NARROW ? 12 : 42 }, autorange: "reversed", automargin: true, type: "category" },
      margin: w < NARROW ? { l: 60, r: 10, t: 20, b: 60 } : { l: 95, r: 30, t: 28, b: 75 },
    }),
  };
}

export function rankHeatmapFigure(m: Methodology): Figure | null {
  const pct = m.rank_matrix_pct;
  if (!pct) return null;
  const labels = Array.from({ length: 30 }, (_, i) => pad2(i + 1));
  return {
    data: [{
      type: "heatmap", x: labels, y: labels, z: pct,
      colorscale: [[0, "#0f0f1a"], [0.35, "#172554"], [0.7, "#4c1d95"], [1, "#a78bfa"]],
      text: pct.map((row, i) => row.map((v, j) => `From Rank ${labels[i]}<br>To Rank ${labels[j]}<br>Probability: ${v.toFixed(2)}%`)),
      hovertemplate: "%{text}<extra></extra>",
      showscale: true,
    }],
    layout: (w) => ({
      xaxis: { title: { text: "Year t + 1 Rank" }, side: "bottom", tickangle: -45, type: "category",
               tickmode: "array", tickvals: labels, ticktext: labels, automargin: true,
               tickfont: { size: w < NARROW ? 8 : 10 } },
      yaxis: { title: { text: "Year t Rank" }, type: "category", tickmode: "array", tickvals: labels, ticktext: labels,
               autorange: "reversed", automargin: true, tickfont: { size: w < NARROW ? 8 : 10 } },
      margin: w < NARROW ? { l: 50, r: 10, t: 14, b: 60 } : { l: 75, r: 45, t: 18, b: 82 },
    }),
  };
}

export type IntervalChoice = "40_60" | "25_75" | "10_90";

export function rankHorizonFigure(m: Methodology, startRank: "all" | number, interval: IntervalChoice): Figure {
  const { label, rows } = m.rank_horizon[interval];
  let data: Trace[];
  let title: string;
  let shortTitle: string;
  if (startRank === "all") {
    data = Array.from({ length: 30 }, (_, k) => {
      const d = rows.filter((r) => r.start_rank === k + 1);
      return {
        type: "scatter", mode: "lines", name: pad2(k + 1),
        x: d.map((r) => r.years_ahead), y: d.map((r) => r.mean_rank), text: d.map((r) => r.hover),
        hovertemplate: "%{text}<extra></extra>", line: { color: m.rank_colors[k], width: 1.45 }, showlegend: false,
      };
    });
    title = `All Starting Ranks: Next Seven Seasons (${label} interval in hover)`;
    shortTitle = "All Starting Ranks: Next 7 Seasons";
  } else {
    const d = rows.filter((r) => r.start_rank === startRank);
    const x = d.map((r) => r.years_ahead);
    data = [
      ...ribbon(x, d.map((r) => r.qlo), d.map((r) => r.qhi), { name: label, fillcolor: "rgba(96, 165, 250, 0.18)" }),
      { type: "scatter", mode: "lines", name: "Expected rank", x, y: d.map((r) => r.mean_rank),
        line: { color: "#60a5fa", width: 3 }, text: d.map((r) => r.hover), hovertemplate: "%{text}<extra></extra>" },
      { type: "scatter", mode: "markers", name: "Expected rank", x, y: d.map((r) => r.mean_rank),
        marker: { size: 5, color: "#60a5fa" }, text: d.map((r) => r.hover), hovertemplate: "%{text}<extra></extra>",
        showlegend: false },
    ];
    title = `Start Rank ${pad2(startRank)}: Next Seven Seasons`;
    shortTitle = `Start Rank ${pad2(startRank)}: Next 7 Seasons`;
  }
  const labels = Array.from({ length: 30 }, (_, i) => pad2(i + 1));
  return {
    data,
    layout: (w) => ({
      title: { text: w < NARROW ? shortTitle : title, font: { color: "#e5e7eb", size: w < NARROW ? 11 : 13 } },
      xaxis: { title: { text: "Years Ahead" }, tickmode: "linear", dtick: 1, range: [0, 7], automargin: true },
      yaxis: { title: { text: "League Rank (1 = best)" }, range: [30.5, 0.5], tickmode: "array",
               tickvals: labels.map(Number), ticktext: labels, automargin: true,
               tickfont: { size: w < NARROW ? 8 : 10 } },
      legend: { orientation: "h", x: 0.5, xanchor: "center", y: -0.24, yanchor: "top" },
      margin: w < NARROW ? { l: 50, r: 10, t: 38, b: 70 } : { l: 70, r: 20, t: 38, b: 70 },
    }),
  };
}

const LOTTERY_SYSTEMS = [
  { system: "Current", name: "Current", color: "#3b82f6" },
  { system: "Proposed 3-2-1", name: "3-2-1 sim", color: "#f59e0b" },
] as const;

export function lotteryLineFigure(m: Methodology): Figure {
  const data = LOTTERY_SYSTEMS.map(({ system, name, color }) => {
    const d = m.lottery.filter((r) => r.system === system);
    return {
      type: "scatter", mode: "lines+markers", name, x: d.map((r) => r.seed), y: d.map((r) => r.expected_pick),
      line: { color, width: 3 }, marker: { color, size: 7 },
      error_y: { type: "data", array: d.map((r) => 1.96 * r.expected_pick_se), visible: true, color, thickness: 0.7 },
      hovertemplate: "Seed %{x}<br>Expected Pick: %{y:.1f}<extra></extra>",
    };
  });
  return {
    data,
    layout: (w) => ({
      xaxis: { title: { text: "Lottery Seed (1 = worst record)" }, dtick: 1 },
      yaxis: { title: { text: "Expected Pick" }, range: [16.5, 0.5], tickmode: "array",
               tickvals: Array.from({ length: 16 }, (_, i) => i + 1) },
      legend: topLegend,
      margin: w < NARROW ? { l: 50, r: 12, t: 50, b: 50 } : { l: 64, r: 24, t: 78, b: 56 },
    }),
  };
}

export function lotteryBarFigure(m: Methodology): Figure {
  const data = LOTTERY_SYSTEMS.map(({ system, name, color }) => {
    const d = m.lottery.filter((r) => r.system === system);
    return {
      type: "bar", name, x: d.map((r) => r.seed), y: d.map((r) => 100 * r.prob_no1), marker: { color },
      error_y: { type: "data", array: d.map((r) => 1.96 * 100 * r.prob_no1_se), visible: true, color, thickness: 0.7 },
      hovertemplate: "Seed %{x}<br>#1 Pick Odds: %{y:.1f}%<extra></extra>",
    };
  });
  return {
    data,
    layout: (w) => ({
      barmode: "group",
      xaxis: { title: { text: "Lottery Seed" }, dtick: 1 },
      yaxis: { title: { text: "#1 Pick Odds" } },
      legend: topLegend,
      margin: w < NARROW ? { l: 50, r: 12, t: 50, b: 50 } : { l: 64, r: 24, t: 78, b: 56 },
    }),
  };
}
