<script lang="ts">
  // Team dropdown with logos (native <select> options can only hold text).
  // Type to filter; arrow keys, Home / End, Enter, and Escape work as in a
  // native select. On phones the list fills the control's width.
  import { tick } from "svelte";
  import type { Team } from "../lib/types";

  interface Props {
    id: string;
    labelledby: string;
    teams: Pick<Team, "abbr" | "name" | "logo">[];
    value: string;
    onchange: (value: string) => void;
    allLabel?: string; // adds an "All" entry with this label (value "All")
  }
  let { id, labelledby, teams, value, onchange, allLabel }: Props = $props();

  interface Item { value: string; abbr: string; name: string; logo: string | null }

  let open = $state(false);
  let query = $state("");
  let active = $state(0);
  let root: HTMLDivElement;
  let button: HTMLButtonElement;
  let search = $state<HTMLInputElement | null>(null);
  let list = $state<HTMLUListElement | null>(null);

  const items = $derived<Item[]>([
    ...(allLabel ? [{ value: "All", abbr: "", name: allLabel, logo: null }] : []),
    ...teams.map((t) => ({ value: t.abbr, abbr: t.abbr, name: t.name, logo: t.logo })),
  ]);
  const filtered = $derived.by(() => {
    const q = query.trim().toLowerCase();
    return q ? items.filter((i) => i.abbr.toLowerCase().includes(q) || i.name.toLowerCase().includes(q)) : items;
  });
  const current = $derived(items.find((i) => i.value === value));
  const optionId = (i: number) => `${id}-opt-${i}`;

  async function openMenu() {
    query = "";
    open = true;
    active = Math.max(0, items.findIndex((i) => i.value === value));
    await tick();
    search?.focus();
    scrollActive();
  }

  function close(refocus = true) {
    open = false;
    if (refocus) button.focus();
  }

  function choose(item: Item | undefined) {
    if (!item) return;
    if (item.value !== value) onchange(item.value);
    close();
  }

  function scrollActive() {
    list?.querySelector(`#${CSS.escape(optionId(active))}`)?.scrollIntoView({ block: "nearest" });
  }

  function move(to: number) {
    active = Math.min(Math.max(to, 0), filtered.length - 1);
    tick().then(scrollActive);
  }

  function onSearchKey(e: KeyboardEvent) {
    switch (e.key) {
      case "ArrowDown": move(active + 1); break;
      case "ArrowUp": move(active - 1); break;
      case "Home": move(0); break;
      case "End": move(filtered.length - 1); break;
      case "PageDown": move(active + 8); break;
      case "PageUp": move(active - 8); break;
      case "Enter": choose(filtered[active]); break;
      case "Escape": close(); break;
      case "Tab": close(false); return;
      default: return;
    }
    e.preventDefault();
  }

  function onButtonKey(e: KeyboardEvent) {
    if (["ArrowDown", "ArrowUp", "Enter", " "].includes(e.key)) {
      e.preventDefault();
      openMenu();
    }
  }

  function onfocusout(e: FocusEvent) {
    if (open && !root.contains(e.relatedTarget as Node)) close(false);
  }
</script>

<div class="team-select" bind:this={root} {onfocusout}>
  <button
    bind:this={button}
    {id}
    type="button"
    class="control"
    class:open
    aria-haspopup="listbox"
    aria-expanded={open}
    aria-labelledby={`${labelledby} ${id}`}
    onclick={() => (open ? close() : openMenu())}
    onkeydown={onButtonKey}
  >
    {#if current?.logo}<img src={current.logo} alt="" width="22" height="22" />{/if}
    <span class="text">{current ? (current.abbr ? `${current.abbr} · ${current.name}` : current.name) : "Select a team"}</span>
  </button>

  {#if open}
    <div class="menu">
      <input
        bind:this={search}
        type="text"
        class="search"
        placeholder="Type to filter teams"
        aria-label="Filter teams"
        role="combobox"
        aria-expanded="true"
        aria-controls={`${id}-list`}
        aria-activedescendant={filtered.length ? optionId(active) : undefined}
        autocomplete="off"
        bind:value={query}
        oninput={() => (active = 0)}
        onkeydown={onSearchKey}
      />
      <ul bind:this={list} id={`${id}-list`} role="listbox" aria-labelledby={labelledby}>
        {#each filtered as item, i (item.value)}
          <!-- svelte-ignore a11y_click_events_have_key_events -->
          <li
            id={optionId(i)}
            role="option"
            aria-selected={item.value === value}
            class:active={i === active}
            class:selected={item.value === value}
            onmousedown={(e) => e.preventDefault()}
            onclick={() => choose(item)}
            onmouseenter={() => (active = i)}
          >
            {#if item.logo}
              <img src={item.logo} alt="" width="22" height="22" loading="lazy" />
            {:else}
              <span class="no-logo" aria-hidden="true"></span>
            {/if}
            {#if item.abbr}<span class="abbr">{item.abbr}</span>{/if}
            <span class="name">{item.name}</span>
          </li>
        {:else}
          <li class="empty">No matching teams</li>
        {/each}
      </ul>
    </div>
  {/if}
</div>

<style>
  .team-select { position: relative; min-width: 0; }
  .control {
    display: flex;
    align-items: center;
    gap: 8px;
    width: 100%;
    min-height: 40px;
    padding: 6px 34px 6px 10px;
    border-radius: 8px;
    border: 1px solid var(--border);
    background: var(--surface-2) url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='8' viewBox='0 0 12 8'%3E%3Cpath d='M1 1l5 5 5-5' fill='none' stroke='%23c9ccd6' stroke-width='1.8'/%3E%3C/svg%3E") no-repeat right 12px center;
    text-align: left;
    cursor: pointer;
  }
  .control:focus-visible { outline: 2px solid var(--side-color, var(--link)); outline-offset: 1px; }
  .control.open {
    border-color: var(--side-color, var(--link));
    box-shadow: 0 0 0 0.22rem var(--side-glow, rgba(196, 181, 253, 0.25));
  }
  .control img, li img { width: 22px; height: 22px; object-fit: contain; flex: 0 0 auto; }
  .text { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }

  .menu {
    position: absolute;
    z-index: 25;
    left: 0;
    right: 0;
    margin-top: 4px;
    min-width: 220px;
    background: #151525;
    border: 1px solid var(--border);
    border-radius: 8px;
    box-shadow: 0 12px 30px rgba(0, 0, 0, 0.5);
    overflow: hidden;
  }
  .search {
    width: 100%;
    padding: 9px 12px;
    border: 0;
    border-bottom: 1px solid var(--border);
    background: transparent;
    outline: none;
  }
  ul { list-style: none; margin: 0; padding: 4px; max-height: min(340px, 50vh); overflow-y: auto; }
  li {
    display: flex;
    align-items: center;
    gap: 9px;
    padding: 7px 8px;
    border-radius: 6px;
    cursor: pointer;
  }
  li.active { background: var(--side-soft, rgba(109, 40, 217, 0.28)); }
  li.selected .name, li.selected .abbr { color: #fff; font-weight: 700; }
  .abbr { font-family: var(--mono); font-size: 12px; color: #cfd2dc; width: 34px; flex: 0 0 auto; }
  .name { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
  .no-logo { width: 22px; flex: 0 0 auto; }
  .empty { color: var(--muted); cursor: default; }
  @media (max-width: 560px) {
    li { padding: 9px 8px; }
  }
</style>
