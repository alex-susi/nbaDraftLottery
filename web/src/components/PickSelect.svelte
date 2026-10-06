<script lang="ts">
  import type { DisplayPick } from "../lib/types";

  interface Props {
    id: string;
    label: string;
    options: DisplayPick[];
    selected: string[];
    onchange: (ids: string[]) => void;
  }
  let { id, label, options, selected, onchange }: Props = $props();

  let open = $state(false);
  let query = $state("");
  let active = $state(0);
  let root: HTMLDivElement;
  let input: HTMLInputElement;

  const byId = $derived(new Map(options.map((o) => [o.id, o])));
  const available = $derived(
    options.filter((o) => !selected.includes(o.id) &&
      o.trade_label.toLowerCase().includes(query.trim().toLowerCase()))
  );

  function add(pickId: string) {
    onchange([...selected, pickId]);
    query = "";
    active = Math.min(active, Math.max(available.length - 2, 0));
    input?.focus();
  }

  function remove(pickId: string) {
    onchange(selected.filter((s) => s !== pickId));
  }

  function onkeydown(e: KeyboardEvent) {
    if (e.key === "ArrowDown") {
      open = true;
      active = Math.min(active + 1, available.length - 1);
      e.preventDefault();
    } else if (e.key === "ArrowUp") {
      active = Math.max(active - 1, 0);
      e.preventDefault();
    } else if (e.key === "Enter" && open && available[active]) {
      add(available[active].id);
      e.preventDefault();
    } else if (e.key === "Escape") {
      open = false;
    } else if (e.key === "Backspace" && query === "" && selected.length) {
      remove(selected[selected.length - 1]);
    }
  }

  function onfocusout(e: FocusEvent) {
    if (!root.contains(e.relatedTarget as Node)) open = false;
  }
</script>

<div class="pick-select" bind:this={root} {onfocusout}>
  <label class="label" for={id}>{label}</label>
  <!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_static_element_interactions -->
  <div class="control" class:open onclick={() => { open = true; input.focus(); }}>
    {#each selected as sid (sid)}
      <span class="chip">
        <span class="chip-text">{byId.get(sid)?.trade_label ?? sid}</span>
        <button type="button" class="chip-x" aria-label={`Remove ${byId.get(sid)?.trade_label ?? sid}`}
                onclick={(e) => { e.stopPropagation(); remove(sid); }}>×</button>
      </span>
    {/each}
    <input
      bind:this={input}
      {id}
      type="text"
      role="combobox"
      aria-expanded={open}
      aria-controls={`${id}-list`}
      aria-autocomplete="list"
      autocomplete="off"
      placeholder={selected.length ? "" : "select one or more picks"}
      bind:value={query}
      onfocus={() => (open = true)}
      oninput={() => { open = true; active = 0; }}
      {onkeydown}
    />
  </div>
  {#if open}
    <ul class="menu" id={`${id}-list`} role="listbox" aria-multiselectable="true">
      {#each available as o, i (o.id)}
        <li role="option" aria-selected={i === active} class:active={i === active}>
          <button type="button" tabindex="-1" onmousedown={(e) => e.preventDefault()}
                  onclick={() => add(o.id)} onmouseenter={() => (active = i)}>{o.trade_label}</button>
        </li>
      {:else}
        <li class="empty">{options.length ? "No matching picks" : "No picks to trade"}</li>
      {/each}
    </ul>
  {/if}
</div>

<style>
  .pick-select { position: relative; }
  .label { display: block; margin-bottom: 6px; font-weight: 800; color: #cfd2dc; }
  .control {
    display: flex;
    flex-wrap: wrap;
    gap: 6px;
    min-height: 44px;
    padding: 6px 8px;
    border: 1px solid var(--border);
    border-radius: 8px;
    background: var(--surface-2);
    cursor: text;
  }
  .control.open {
    border-color: var(--side-color);
    box-shadow: 0 0 0 0.22rem var(--side-glow);
  }
  .chip {
    display: inline-flex;
    align-items: center;
    gap: 2px;
    max-width: 100%;
    padding: 2px 2px 2px 8px;
    border: 1px solid var(--side-color);
    border-radius: 4px;
    background: var(--side-soft);
    color: var(--text-strong);
    font-size: 13px;
  }
  .chip-text { overflow-wrap: anywhere; }
  .chip-x {
    border: 0;
    background: none;
    padding: 2px 6px;
    font-size: 16px;
    line-height: 1;
    cursor: pointer;
    color: var(--text-strong);
    opacity: 0.75;
  }
  .chip-x:hover { opacity: 1; }
  input {
    flex: 1 1 120px;
    min-width: 80px;
    border: 0;
    outline: none;
    background: transparent;
    padding: 4px 2px;
  }
  .menu {
    position: absolute;
    z-index: 20;
    left: 0;
    right: 0;
    margin: 4px 0 0;
    padding: 4px;
    list-style: none;
    max-height: min(320px, 50vh);
    overflow-y: auto;
    background: #151525;
    border: 1px solid var(--border);
    border-radius: 8px;
    box-shadow: 0 12px 30px rgba(0, 0, 0, 0.5);
  }
  .menu button {
    display: block;
    width: 100%;
    text-align: left;
    border: 0;
    background: none;
    padding: 9px 10px;
    border-radius: 6px;
    cursor: pointer;
  }
  .menu li.active button { background: var(--side-soft); }
  .empty { padding: 9px 10px; color: var(--muted); }
</style>
