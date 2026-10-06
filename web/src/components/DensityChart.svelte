<script lang="ts">
  // trade_density_plot() in app.R: the left side (< 0) favors Team 1 and the
  // right side (> 0) favors Team 2. Callers pass the negative of net-to-Team-1.
  import { hexToRgba } from "../lib/format";
  import { densityCurve, niceTicks } from "../lib/stats";
  import type { Team } from "../lib/types";

  interface Props {
    values: Float64Array;
    teamA: Team;
    teamB: Team;
    xTitle: string;
    hoverLabel: string;
    xCap?: number;
    xStep?: number;
    height?: number;
  }
  let { values, teamA, teamB, xTitle, hoverLabel, xCap, xStep, height = 300 }: Props = $props();

  let width = $state(0);
  let hoverIdx = $state<number | null>(null);
  const margin = { top: 12, right: 14, bottom: 46, left: 50 };
  const uid = `clip-${Math.random().toString(36).slice(2)}`;

  const curve = $derived(densityCurve(values));
  const shares = $derived.by(() => {
    let a = 0, b = 0;
    for (const v of values) {
      if (v < 0) a++;
      else if (v > 0) b++;
    }
    return { a: a / values.length, b: b / values.length };
  });

  // Like xaxis$range in app.R: the cap limits only the view, not the density.
  const xDomain = $derived.by((): [number, number] => {
    const lo = curve.x[0] ?? -1;
    const hi = curve.x[curve.x.length - 1] ?? 1;
    return xCap ? [Math.max(lo, -xCap), Math.min(hi, xCap)] : [lo, hi];
  });
  const yMax = $derived(Math.max(...curve.y, 1e-9) * 1.06);

  const innerW = $derived(Math.max(width - margin.left - margin.right, 10));
  const innerH = $derived(height - margin.top - margin.bottom);
  const sx = (v: number) => margin.left + ((v - xDomain[0]) / (xDomain[1] - xDomain[0])) * innerW;
  const sy = (v: number) => margin.top + innerH - (v / yMax) * innerH;

  function areaPath(keep: (x: number) => boolean): string {
    const pts: string[] = [];
    for (let i = 0; i < curve.x.length; i++) {
      if (keep(curve.x[i])) pts.push(`${sx(curve.x[i]).toFixed(1)},${sy(curve.y[i]).toFixed(1)}`);
    }
    if (pts.length < 2) return "";
    const first = pts[0].split(",")[0];
    const last = pts[pts.length - 1].split(",")[0];
    return `M${first},${sy(0)}L${pts.join("L")}L${last},${sy(0)}Z`;
  }
  function linePath(keep: (x: number) => boolean): string {
    const pts: string[] = [];
    for (let i = 0; i < curve.x.length; i++) {
      if (keep(curve.x[i])) pts.push(`${sx(curve.x[i]).toFixed(1)},${sy(curve.y[i]).toFixed(1)}`);
    }
    return pts.length < 2 ? "" : `M${pts.join("L")}`;
  }

  const xTicks = $derived(niceTicks(xDomain[0], xDomain[1], Math.max(3, Math.round(innerW / 55)), xStep));
  const yTicks = $derived(niceTicks(0, yMax, 5));
  const tickLabel = (v: number) => (Math.abs(v) >= 1 || v === 0 ? String(+v.toFixed(2)) : v.toFixed(2));

  function onpointermove(e: PointerEvent) {
    const rect = (e.currentTarget as SVGElement).getBoundingClientRect();
    const v = xDomain[0] + ((e.clientX - rect.left - margin.left) / innerW) * (xDomain[1] - xDomain[0]);
    if (v < xDomain[0] || v > xDomain[1] || curve.x.length === 0) {
      hoverIdx = null;
      return;
    }
    let best = 0;
    for (let i = 1; i < curve.x.length; i++) {
      if (Math.abs(curve.x[i] - v) < Math.abs(curve.x[best] - v)) best = i;
    }
    hoverIdx = best;
  }
</script>

<div class="chart" bind:clientWidth={width}>
  {#if width > 0 && curve.x.length > 0}
    <svg {width} {height} role="img" aria-label={`${xTitle} distribution`}
         {onpointermove} onpointerleave={() => (hoverIdx = null)}>
      <defs>
        <clipPath id={uid}><rect x={margin.left} y={margin.top} width={innerW} height={innerH} /></clipPath>
      </defs>
      <rect x={margin.left} y={margin.top} width={innerW} height={innerH} fill="#0f0f1a" />
      {#each yTicks as t}
        <line x1={margin.left} x2={margin.left + innerW} y1={sy(t)} y2={sy(t)} stroke="var(--grid)" />
        <text x={margin.left - 6} y={sy(t)} class="tick" text-anchor="end" dominant-baseline="middle">{tickLabel(t)}</text>
      {/each}
      {#each xTicks as t}
        <line x1={sx(t)} x2={sx(t)} y1={margin.top} y2={margin.top + innerH} stroke={t === 0 ? "#6b7280" : "var(--grid)"} />
        <text x={sx(t)} y={margin.top + innerH + 16} class="tick" text-anchor="middle">{tickLabel(t)}</text>
      {/each}
      <g clip-path={`url(#${uid})`}>
        <path d={areaPath((x) => x <= 0)} fill={hexToRgba(teamA.primary, 0.8)} />
        <path d={areaPath((x) => x >= 0)} fill={hexToRgba(teamB.primary, 0.8)} />
        <path d={linePath((x) => x <= 0)} fill="none" stroke={teamA.secondary} stroke-width="2.6" />
        <path d={linePath((x) => x >= 0)} fill="none" stroke={teamB.secondary} stroke-width="2.6" />
        {#if xDomain[0] <= 0 && xDomain[1] >= 0}
          <line x1={sx(0)} x2={sx(0)} y1={margin.top} y2={margin.top + innerH} stroke="#6b7280" stroke-width="1.5" stroke-dasharray="5 4" />
        {/if}
        {#if hoverIdx != null}
          <line x1={sx(curve.x[hoverIdx])} x2={sx(curve.x[hoverIdx])} y1={margin.top} y2={margin.top + innerH} stroke="#c9ccd6" stroke-width="1" />
          <circle cx={sx(curve.x[hoverIdx])} cy={sy(curve.y[hoverIdx])} r="4"
                  fill={curve.x[hoverIdx] <= 0 ? teamA.secondary : teamB.secondary} />
        {/if}
      </g>
      <text x={margin.left + innerW / 2} y={height - 6} class="axis-title" text-anchor="middle">{xTitle}</text>
      <text transform={`translate(13 ${margin.top + innerH / 2}) rotate(-90)`} class="axis-title" text-anchor="middle">Density</text>
    </svg>
    {#if hoverIdx != null}
      {@const left = sx(curve.x[hoverIdx])}
      <div class="tip" style:left={`${Math.min(Math.max(left, 90), width - 90)}px`}
           style:border-color={curve.x[hoverIdx] <= 0 ? teamA.secondary : teamB.secondary}>
        <div>{hoverLabel}: <b class="num">{curve.x[hoverIdx].toFixed(2)}</b></div>
        <div>Density: <span class="num">{curve.y[hoverIdx].toFixed(2)}</span></div>
        <div>{teamA.abbr} wins: <span class="num">{(100 * shares.a).toFixed(1)}%</span></div>
        <div>{teamB.abbr} wins: <span class="num">{(100 * shares.b).toFixed(1)}%</span></div>
      </div>
    {/if}
  {/if}
</div>

<style>
  .chart { position: relative; width: 100%; touch-action: pan-y; }
  svg { display: block; font-family: var(--mono); }
  .tick { fill: #d9dddc; font-size: 11px; }
  .axis-title { fill: #d9dddc; font-size: 12px; }
  .tip {
    position: absolute;
    top: 10px;
    transform: translateX(-50%);
    pointer-events: none;
    padding: 6px 9px;
    background: rgba(15, 15, 26, 0.94);
    border: 1px solid;
    border-radius: 6px;
    font-size: 12px;
    line-height: 1.45;
    white-space: nowrap;
    color: var(--text-strong);
  }
</style>
