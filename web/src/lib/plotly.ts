// Plotly is loaded on first use so the Trade Machine (custom SVG charts) never
// downloads it. The cartesian bundle covers scatter, bar, and heatmap traces.

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type PlotlyLib = any;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type Layout = Record<string, any>;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type Trace = Record<string, any>;

let plotlyPromise: Promise<PlotlyLib> | null = null;

export function loadPlotly(): Promise<PlotlyLib> {
  plotlyPromise ??= import("plotly.js-cartesian-dist-min").then((m) => m.default ?? m);
  return plotlyPromise;
}

const axisDefaults = { gridcolor: "#1a1a2a", zerolinecolor: "#333" };

// plotly_dark() in app.R: transparent paper, dark plot area, Plex Mono labels.
export function darkLayout(layout: Layout): Layout {
  const out: Layout = {
    paper_bgcolor: "rgba(0,0,0,0)",
    plot_bgcolor: "#0f0f1a",
    font: { family: "IBM Plex Mono, ui-monospace, monospace", color: "#999" },
    hoverlabel: { font: { family: "IBM Plex Mono, ui-monospace, monospace" } },
    ...layout,
  };
  for (const key of Object.keys(out)) {
    if (/^[xy]axis\d*$/.test(key)) out[key] = { ...axisDefaults, ...out[key] };
  }
  out.xaxis = { ...axisDefaults, ...out.xaxis };
  out.yaxis = { ...axisDefaults, ...out.yaxis };
  return out;
}

export const plotConfig = {
  displaylogo: false,
  responsive: false,
  modeBarButtonsToRemove: ["lasso2d", "select2d", "autoScale2d"],
};

// Ribbon between two series, drawn as a hidden lower bound plus a filled upper
// bound, matching plotly R's add_ribbons().
export function ribbon(x: number[], lo: number[], hi: number[], opts: Trace): Trace[] {
  const { name, fillcolor, text, hovertemplate, yaxis, showlegend } = opts;
  return [
    { type: "scatter", mode: "lines", x, y: lo, yaxis, line: { color: "transparent" },
      showlegend: false, hoverinfo: "skip" },
    { type: "scatter", mode: "lines", x, y: hi, yaxis, name, fill: "tonexty", fillcolor,
      line: { color: "transparent" }, text, hovertemplate, hoverinfo: hovertemplate ? undefined : "skip",
      showlegend: showlegend ?? true },
  ];
}
