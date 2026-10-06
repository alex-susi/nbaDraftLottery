<script lang="ts">
  // team_logo_html_app(): small logo followed by the abbreviation.
  import type { Team } from "../lib/types";

  let { team, size = 20, full = false }: { team: Team | undefined; size?: number; full?: boolean } = $props();
</script>

{#if team}
  <span class="tag">
    {#if team.logo}<img src={team.logo} alt="" title={team.name} width={size} height={size} />{/if}
    <span>{full ? team.name : team.abbr}</span>
  </span>
{/if}

<style>
  .tag { display: inline-flex; align-items: center; gap: 8px; white-space: nowrap; }
  img { object-fit: contain; flex: 0 0 auto; }
</style>
