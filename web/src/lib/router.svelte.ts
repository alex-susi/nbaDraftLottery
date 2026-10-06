// Hash routes, so every view works as a static page on GitHub Pages:
//   #/summary  #/team/portfolios  #/team/landscape  #/team/single/<pick id>
//   #/trade    #/methodology

export const PAGES = ["summary", "team", "trade", "methodology"] as const;
export type Page = (typeof PAGES)[number];
export const TEAM_VIEWS = ["portfolios", "landscape", "single"] as const;
export type TeamView = (typeof TEAM_VIEWS)[number];

interface Route {
  page: Page;
  teamView: TeamView;
  pickId: string | null;
}

function parse(): Route {
  const [page, view, id] = location.hash.replace(/^#\/?/, "").split("/");
  return {
    page: (PAGES as readonly string[]).includes(page) ? (page as Page) : "summary",
    teamView: (TEAM_VIEWS as readonly string[]).includes(view) ? (view as TeamView) : "portfolios",
    pickId: id ? decodeURIComponent(id) : null,
  };
}

export const route = $state<Route>(parse());

window.addEventListener("hashchange", () => {
  const next = parse();
  const pageChanged = next.page !== route.page || next.teamView !== route.teamView;
  Object.assign(route, next);
  if (pageChanged) window.scrollTo(0, 0);
});

export const pickHref = (id: string) => `#/team/single/${encodeURIComponent(id)}`;

// Update the address without adding a history entry (e.g. picking from a menu).
export function replaceRoute(hash: string) {
  history.replaceState(null, "", hash);
  Object.assign(route, parse());
}
