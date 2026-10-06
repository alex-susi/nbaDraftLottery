<script lang="ts">
  // Renders one Plotly figure sized to its container. Layout and height are
  // functions of the container width so charts can rearrange on phones.
  // Optional team logos are drawn as <img> overlays at data coordinates, like
  // attach_plot_logo_overlays_app() in app.R: hovering a team dims the others.
  import { onDestroy } from "svelte";
  import { darkLayout, loadPlotly, plotConfig, type Layout, type PlotlyLib, type Trace } from "../lib/plotly";
  import type { LogoPoint } from "../lib/types";

  interface Props {
    data: Trace[];
    layout: (width: number) => Layout;
    height: number | ((width: number) => number);
    label: string;
    logos?: (width: number) => LogoPoint[];
    onPointClick?: (customdata: unknown) => void;
  }
  let { data, layout, height, label, logos, onPointClick }: Props = $props();

  let el: HTMLDivElement;
  let width = $state(0);
  let plotly = $state<PlotlyLib | null>(null);
  let failed = $state(false);
  let placed = $state<(LogoPoint & { left: number; top: number })[]>([]);
  let hoveredTeam = $state<string | null>(null);
  let listening = false;

  loadPlotly().then((p) => (plotly = p)).catch(() => (failed = true));

  const h = $derived(typeof height === "function" ? height(width) : height);

  function placeLogos() {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const fl = (el as any)?._fullLayout;
    if (!fl?.xaxis?.l2p || !fl?.yaxis?.l2p) return;
    const [x0, x1] = fl.xaxis.range;
    const [y0, y1] = fl.yaxis.range;
    const inside = (v: number, a: number, b: number) => v >= Math.min(a, b) && v <= Math.max(a, b);
    placed = (logos?.(width) ?? [])
      .filter((d) => d.src && Number.isFinite(d.x) && Number.isFinite(d.y) && inside(d.x, x0, x1) && inside(d.y, y0, y1))
      .map((d) => ({ ...d, left: fl.xaxis.l2p(d.x) + fl.xaxis._offset, top: fl.yaxis.l2p(d.y) + fl.yaxis._offset }));
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  function teamFromEvent(ev: any): string | null {
    for (const p of ev?.points ?? []) {
      let cd = p.customdata;
      if (Array.isArray(cd)) cd = cd[0];
      if (cd != null && String(cd).length) return String(cd);
    }
    return null;
  }

  $effect(() => {
    if (!plotly || width === 0) return;
    // Touch screens show the toolbar permanently, where it covers the plot.
    const config = { ...plotConfig, displayModeBar: width < 640 ? false : "hover" };
    const fig = darkLayout({ ...layout(width), width, height: h });
    void logos;
    plotly.react(el, data, fig, config).then(() => {
      placeLogos();
      if (listening) return;
      listening = true;
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const gd = el as any;
      gd.on("plotly_relayout", () => setTimeout(placeLogos, 25));
      gd.on("plotly_hover", (ev: unknown) => (hoveredTeam = teamFromEvent(ev)));
      gd.on("plotly_unhover", () => (hoveredTeam = null));
      gd.on("plotly_click", (ev: { points?: { customdata?: unknown }[] }) => {
        const cd = ev?.points?.[0]?.customdata;
        if (cd != null) onPointClick?.(Array.isArray(cd) ? cd[0] : cd);
      });
    });
  });

  onDestroy(() => {
    if (plotly && el) plotly.purge(el);
  });
</script>

<div class="plot" bind:clientWidth={width} style:height={`${h}px`} role="figure" aria-label={label}>
  {#if failed}
    <p class="msg">The chart library could not be loaded.</p>
  {:else if !plotly}
    <p class="msg">Loading chart…</p>
  {/if}
  <div bind:this={el}></div>
  {#each placed as d (d.team)}
    <img class="logo" src={d.src} alt="" title={d.team}
         style:left={`${d.left - d.size / 2}px`} style:top={`${d.top - d.size / 2}px`}
         style:width={`${d.size}px`} style:height={`${d.size}px`}
         style:opacity={hoveredTeam && hoveredTeam !== d.team ? 0.16 : 0.98}
         style:z-index={hoveredTeam === d.team ? 8 : 6} />
  {/each}
</div>

<style>
  .plot { position: relative; width: 100%; min-width: 0; }
  .msg { position: absolute; inset: 0; display: grid; place-items: center; margin: 0; color: var(--muted); }
  .logo { position: absolute; object-fit: contain; pointer-events: none; transition: opacity 90ms linear; }
  /* Plotly draws hover labels in its last SVG layer; keep it above the logos. */
  .plot :global(.svg-container > svg.main-svg:last-of-type) { z-index: 10; }
</style>
