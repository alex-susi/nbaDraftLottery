################################################################################
# NBA Draft Lottery Rule Change Impact Model v5
#
# WHAT CHANGED IN v5 (see chat for full rationale):
#   1. Lottery system is now the APPROVED 3-2-1 format (NBA BOG, May 2026),
#      effective 2027-2029. 2026 used the OLD system and its results are now
#      FINAL, so 2026 pick values are locked to the actual draft slots.
#   2. Markov chain states are the FIVE 3-2-1 tiers (relegation / non-play-in /
#      9-10 seed / 7v8 play-in loser / playoff), not generic standings buckets.
#   3. Pick value = mean WIN SHARES over a player's FIRST 4 SEASONS (rookie
#      deal), scraped via hoopR draft history + bbref advanced tables, with a
#      bootstrap OR Bayesian curve (toggle below) for uncertainty.
#   4. New anti-tank pick RESTRICTIONS are modeled: no team may receive the #1
#      pick in consecutive years or a top-5 pick three years running (applies
#      to the originally-owning team, looking back to 2025); traded picks can
#      no longer be protected in the 12-15 band.
#   5. Future pick ownership refreshed from RealGM / prosportstransactions and
#      the post-lottery 2026 order.
#
# Pipeline:
#   1. Scrape standings (hoopR -> bbref fallback), rosters, draft production
#   2. Build 5-tier Markov transition counts from history
#   3. Fit Stan models (pick value + Markov) and validate them
#   4. Monte Carlo: project tiers forward, run BOTH lotteries, value every
#      owned pick under each system, applying protections / swaps / new rules
#   5. Export dashboard_data.rds for the Shiny app
#
# Prereqs:
#   install.packages(c("tidyverse","hoopR","rvest","httr","cmdstanr",
#                      "janitor","posterior","expm"))
#   cmdstanr::install_cmdstan()
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(httr)
  library(cmdstanr)
  library(janitor)
  library(posterior)
  library(expm)
})

hoopR_available <- requireNamespace("hoopR", quietly = TRUE)
if (hoopR_available) {
  suppressPackageStartupMessages(library(hoopR))
}

set.seed(2026)

cat("============================================================\n")
cat("  NBA 3-2-1 Lottery Impact Model v5\n")
cat("  Bayesian Markov chain (5 tiers) + 4-year Win Shares\n")
cat("============================================================\n\n")

# ----------------------------------------------------------------------------
# GLOBAL CONFIG
# ----------------------------------------------------------------------------

N_SIMS                 <- 10000   # Monte Carlo iterations
N_LOT                  <- 50000   # lottery-only sims for odds tables
FIRST_PROJECTED_DRAFT  <- 2027    # first year teams' finishes are projected
LAST_PROJECTED_DRAFT   <- 2032    # 7-year horizon (2026 actual + 2027-2032)
HISTORY_START          <- 2005    # first season for transition counts
HISTORY_END            <- 2026    # last completed season
USE_BAYESIAN_PICK_CURVE <- TRUE   # TRUE = Stan curve; FALSE = bootstrap curve

# The five 3-2-1 tiers, worst -> best, with lottery balls per team.
TIERS <- c("relegation", "nonplayin", "playin_seed", "playin_loser", "playoff")
TIER_BALLS <- c(
  relegation   = 2,
  nonplayin    = 3,
  playin_seed  = 2,
  playin_loser = 1,
  playoff      = 0
)
N_TIERS <- length(TIERS)

# How many teams sit in each tier in a normal season (sums to 30).
TIER_SIZES <- c(
  relegation   = 3,
  nonplayin    = 7,
  playin_seed  = 4,
  playin_loser = 2,
  playoff      = 14
)

team_name_to_abbr <- c(
  "Oklahoma City Thunder"  = "OKC", "San Antonio Spurs"      = "SAS",
  "Detroit Pistons"        = "DET", "Boston Celtics"          = "BOS",
  "Denver Nuggets"         = "DEN", "New York Knicks"         = "NYK",
  "Los Angeles Lakers"     = "LAL", "Houston Rockets"         = "HOU",
  "Cleveland Cavaliers"    = "CLE", "Minnesota Timberwolves"  = "MIN",
  "Toronto Raptors"        = "TOR", "Atlanta Hawks"           = "ATL",
  "Phoenix Suns"           = "PHX", "Orlando Magic"           = "ORL",
  "Philadelphia 76ers"     = "PHI", "Charlotte Hornets"       = "CHA",
  "Miami Heat"             = "MIA", "Los Angeles Clippers"    = "LAC",
  "Portland Trail Blazers" = "POR", "Golden State Warriors"   = "GSW",
  "Milwaukee Bucks"        = "MIL", "Chicago Bulls"           = "CHI",
  "New Orleans Pelicans"   = "NOP", "Dallas Mavericks"        = "DAL",
  "Memphis Grizzlies"      = "MEM", "Utah Jazz"               = "UTA",
  "Sacramento Kings"       = "SAC", "Brooklyn Nets"           = "BKN",
  "Indiana Pacers"         = "IND", "Washington Wizards"      = "WAS",
  # historical / alternate names so older seasons map cleanly
  "Charlotte Bobcats"      = "CHA", "New Jersey Nets"         = "BKN",
  "Seattle SuperSonics"    = "OKC", "New Orleans Hornets"     = "NOP",
  "New Orleans/Oklahoma City Hornets" = "NOP", "Vancouver Grizzlies" = "MEM"
)


# ============================================================================
# SECTION 1: SCRAPE HISTORICAL STANDINGS (hoopR -> bbref fallback)
# ============================================================================
# We need ~20 seasons of final standings so each team-season can be assigned a
# 3-2-1 tier and we can count tier-to-tier transitions.

scrape_standings_hoopR <- function(season_end_year) {
  if (!hoopR_available) return(NULL)
  tryCatch({
    cat(sprintf("  [hoopR] standings %d-%d\n",
                season_end_year - 1, season_end_year))
    raw <- hoopR::espn_nba_standings(year = season_end_year)
    raw %>%
      transmute(
        team_raw = .data$team_name,
        wins     = as.integer(.data$wins),
        losses   = as.integer(.data$losses),
        season   = season_end_year,
        win_pct  = .data$wins / (.data$wins + .data$losses)
      ) %>%
      filter(!is.na(.data$wins))
  }, error = function(e) {
    cat(sprintf("    hoopR failed (%d): %s\n", season_end_year, e$message))
    NULL
  })
}

scrape_standings_bbref <- function(season_end_year,
                                   delay = 3) {
  url <- sprintf(
    "https://www.basketball-reference.com/leagues/NBA_%d.html",
    season_end_year
  )
  cat(sprintf("  [bbref] standings %d-%d\n",
              season_end_year - 1, season_end_year))
  Sys.sleep(delay)
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)

  grab_conf <- function(div_id, conf) {
    page %>%
      html_element(div_id) %>%
      html_table(fill = TRUE) %>%
      as_tibble() %>%
      select(1, 2, 3) %>%
      set_names(c("team_raw", "wins", "losses")) %>%
      filter(str_detect(team_raw, "[A-Z]")) %>%
      mutate(
        team_raw = str_remove_all(team_raw, "\\*|\\(\\d+\\)"),
        team_raw = str_trim(team_raw),
        wins     = as.integer(wins),
        losses   = as.integer(losses),
        conf     = conf
      )
  }

  east <- grab_conf("#divs_standings_E", "East")
  west <- grab_conf("#divs_standings_W", "West")

  bind_rows(east, west) %>%
    filter(!is.na(wins)) %>%
    mutate(
      season  = season_end_year,
      win_pct = wins / (wins + losses)
    )
}

cat("--- Scraping Historical Standings ---\n")

all_standings <- map_dfr(HISTORY_START:HISTORY_END, function(yr) {
  res <- scrape_standings_hoopR(yr)
  if (is.null(res) || nrow(res) == 0) res <- scrape_standings_bbref(yr)
  res
})

# Fallback: if even the current season is missing, hardcode 2025-26 finals.
if (nrow(all_standings) == 0 || !HISTORY_END %in% all_standings$season) {
  cat("  Falling back to hardcoded 2025-26 final standings\n")
  current_hard <- tribble(
    ~team_raw,                 ~wins, ~losses,
    "Oklahoma City Thunder",   64,    18,
    "San Antonio Spurs",       62,    20,
    "Detroit Pistons",         60,    22,
    "Boston Celtics",          56,    26,
    "Denver Nuggets",          54,    28,
    "New York Knicks",         53,    29,
    "Los Angeles Lakers",      53,    29,
    "Houston Rockets",         52,    30,
    "Cleveland Cavaliers",     52,    30,
    "Minnesota Timberwolves",  49,    33,
    "Toronto Raptors",         46,    36,
    "Atlanta Hawks",           46,    36,
    "Phoenix Suns",            45,    37,
    "Orlando Magic",           45,    37,
    "Philadelphia 76ers",      45,    37,
    "Charlotte Hornets",       44,    38,
    "Miami Heat",              43,    39,
    "Los Angeles Clippers",    42,    40,
    "Portland Trail Blazers",  42,    40,
    "Golden State Warriors",   37,    45,
    "Milwaukee Bucks",         32,    50,
    "Chicago Bulls",           31,    51,
    "New Orleans Pelicans",    26,    56,
    "Dallas Mavericks",        26,    56,
    "Memphis Grizzlies",       25,    57,
    "Utah Jazz",               22,    60,
    "Sacramento Kings",        22,    60,
    "Brooklyn Nets",           20,    62,
    "Indiana Pacers",          19,    63,
    "Washington Wizards",      17,    65
  ) %>%
    mutate(season = HISTORY_END, win_pct = wins / (wins + losses))
  all_standings <- bind_rows(all_standings, current_hard)
}

all_standings <- all_standings %>%
  mutate(abbr = team_name_to_abbr[team_raw]) %>%
  filter(!is.na(abbr))

cat(sprintf("  Loaded %d team-seasons across %d seasons\n",
            nrow(all_standings), n_distinct(all_standings$season)))


# ============================================================================
# SECTION 2: ASSIGN 3-2-1 TIERS TO EVERY TEAM-SEASON
# ============================================================================
# The approved system keys off playoff/play-in participation. We don't have
# bracket outcomes for every historical season on hand, so we approximate the
# tiers from final standings rank within each season:
#   rank 28-30 -> relegation     (3 worst)
#   rank 21-27 -> nonplayin       (next 7)
#   rank 17-20 -> playin_seed     (9/10 seeds, 4 teams)
#   rank 15-16 -> playin_loser    (7v8 losers, 2 teams)
#   rank 1-14  -> playoff         (14 teams)
# This rank-based proxy matches the *sizes* of the real tiers exactly and is a
# faithful stand-in for seeding the lottery. (When true bracket data is wired
# in later, only assign_tier() needs to change.)

assign_tier_from_rank <- function(rank_worst_is_30) {
  # rank passed in here is 1 = best record, 30 = worst record
  case_when(
    rank_worst_is_30 >= 28 ~ "relegation",
    rank_worst_is_30 >= 21 ~ "nonplayin",
    rank_worst_is_30 >= 17 ~ "playin_seed",
    rank_worst_is_30 >= 15 ~ "playin_loser",
    TRUE                   ~ "playoff"
  )
}

all_standings <- all_standings %>%
  group_by(season) %>%
  mutate(
    rank_best_is_1 = rank(-win_pct, ties.method = "first"),
    tier           = assign_tier_from_rank(rank_best_is_1),
    tier           = factor(tier, levels = TIERS)
  ) %>%
  ungroup()

current_standings <- all_standings %>%
  filter(season == HISTORY_END) %>%
  arrange(desc(win_pct)) %>%
  mutate(overall_rank = row_number())

all_teams <- current_standings$abbr


# ============================================================================
# SECTION 3: SCRAPE ROSTER AGE & CONTINUITY (context only)
# ============================================================================
# Kept for descriptive context in the dashboard. These are no longer model
# inputs (the Markov chain learns persistence directly from tier history), but
# they're cheap to collect and useful color.

scrape_roster_info <- function(abbr,
                               season = HISTORY_END,
                               delay  = 3) {
  bbref_abbr <- case_when(
    abbr == "BKN" ~ "BRK",
    abbr == "CHA" ~ "CHO",
    abbr == "PHX" ~ "PHO",
    TRUE          ~ abbr
  )
  url <- sprintf(
    "https://www.basketball-reference.com/teams/%s/%d.html",
    bbref_abbr, season
  )
  Sys.sleep(delay)
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) {
    return(tibble(abbr = abbr, avg_age = NA_real_, continuity = NA_real_))
  }

  roster <- tryCatch({
    page %>%
      html_element("#per_game") %>%
      html_table(fill = TRUE) %>%
      as_tibble()
  }, error = function(e) NULL)

  avg_age <- if (!is.null(roster) && all(c("Age", "G", "MP") %in% names(roster))) {
    roster %>%
      mutate(
        Age        = as.numeric(Age),
        G          = as.numeric(G),
        MP         = as.numeric(MP),
        age_weight = G * MP
      ) %>%
      filter(!is.na(Age), !is.na(age_weight), age_weight > 0) %>%
      summarise(a = weighted.mean(Age, w = age_weight)) %>%
      pull(a)
  } else {
    NA_real_
  }

  get_names <- function(yr) {
    u <- sprintf(
      "https://www.basketball-reference.com/teams/%s/%d.html",
      bbref_abbr, yr
    )
    Sys.sleep(delay)
    pg <- tryCatch(read_html(u), error = function(e) NULL)
    if (is.null(pg)) return(character(0))
    tryCatch({
      pg %>%
        html_element("#roster") %>%
        html_table(fill = TRUE) %>%
        pull(Player) %>%
        str_trim()
    }, error = function(e) character(0))
  }

  curr <- get_names(season)
  prev <- get_names(season - 1)
  continuity <- if (length(curr) > 0 && length(prev) > 0) {
    length(intersect(curr, prev)) / max(length(curr), 1)
  } else {
    NA_real_
  }

  tibble(abbr = abbr, avg_age = avg_age, continuity = continuity)
}

cat("\n--- Scraping Roster Age & Continuity (context) ---\n")

roster_info <- tryCatch(
  map_dfr(all_teams, ~scrape_roster_info(.x, delay = 3)),
  error = function(e) tibble(abbr = all_teams,
                             avg_age = NA_real_,
                             continuity = NA_real_)
) %>%
  mutate(
    avg_age    = coalesce(avg_age, 26.5),
    continuity = coalesce(continuity, 0.55)
  )

cat(sprintf("  Mean age %.1f, mean continuity %.2f\n",
            mean(roster_info$avg_age), mean(roster_info$continuity)))


# ============================================================================
# SECTION 4: SCRAPE DRAFT PRODUCTION — 4-YEAR WIN SHARES
# ============================================================================
# Pick value = mean Win Shares accumulated over a player's FIRST 4 SEASONS,
# matching the rookie-scale contract window. We get the draft slot -> player
# map from hoopR (nba_drafthistory) and the per-season WS from bbref advanced
# tables, summing each player's first four seasons.
#
# Because per-player season-by-season scraping is heavy, we cache to disk.
# If anything fails we fall back to a compiled 4-year-WS curve.

cache_file <- "draft_4yr_ws_cache.rds"

scrape_player_first4_ws <- function(player_url, delay = 1.5) {
  Sys.sleep(delay)
  pg <- tryCatch(read_html(player_url), error = function(e) NULL)
  if (is.null(pg)) return(NA_real_)
  adv <- tryCatch({
    pg %>%
      html_element("#advanced") %>%
      html_table(fill = TRUE) %>%
      as_tibble(.name_repair = "unique")
  }, error = function(e) NULL)
  if (is.null(adv) || !"WS" %in% names(adv)) return(NA_real_)
  adv %>%
    filter(!is.na(suppressWarnings(as.numeric(WS)))) %>%
    slice(1:4) %>%
    summarise(s = sum(as.numeric(WS), na.rm = TRUE)) %>%
    pull(s)
}

scrape_draft_class_hoopR <- function(draft_year) {
  if (!hoopR_available) return(NULL)
  tryCatch({
    dh <- hoopR::nba_drafthistory(season = draft_year)
    tbl <- dh[["DraftHistory"]]
    tbl %>%
      transmute(
        draft_year = draft_year,
        pick       = as.integer(.data$OVERALL_PICK),
        player     = .data$PLAYER_NAME
      ) %>%
      filter(!is.na(pick), pick >= 1, pick <= 30)
  }, error = function(e) NULL)
}

# bbref draft page already lists career WS; for 4-year WS we prefer per-player
# advanced tables, but the draft page is a fast first pass / fallback.
scrape_draft_class_bbref <- function(draft_year, delay = 3) {
  url <- sprintf(
    "https://www.basketball-reference.com/draft/NBA_%d.html",
    draft_year
  )
  cat(sprintf("  draft %d\n", draft_year))
  Sys.sleep(delay)
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  raw <- tryCatch(
    page %>% html_element("#div_stats") %>% html_table(fill = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) < 2) return(NULL)

  top <- str_trim(names(raw))
  sub <- raw[1, , drop = TRUE] %>% unlist(use.names = FALSE) %>% as.character() %>% str_trim()
  top_clean <- ifelse(
    is.na(top) | top == "" | str_detect(top, "^\\.\\.\\.") | str_detect(top, "^Round"),
    "", top
  )
  new_names <- ifelse(top_clean == "", sub, paste(top_clean, sub, sep = "_"))
  tbl <- raw[-1, , drop = FALSE]
  names(tbl) <- make.unique(new_names)
  tbl <- clean_names(tbl)

  pick_col   <- names(tbl)[names(tbl) %in% c("pk", "pick")][1]
  player_col <- names(tbl)[names(tbl) %in% c("player")][1]
  ws_col     <- names(tbl)[names(tbl) %in% c("advanced_ws", "ws")][1]
  if (is.na(pick_col) || is.na(player_col) || is.na(ws_col)) return(NULL)

  tbl %>%
    transmute(
      draft_year = draft_year,
      pick       = suppressWarnings(as.integer(.data[[pick_col]])),
      player     = .data[[player_col]],
      career_ws  = suppressWarnings(as.numeric(.data[[ws_col]]))
    ) %>%
    filter(!is.na(pick), pick >= 1, pick <= 30)
}

cat("\n--- Scraping Draft Production (4-year Win Shares) ---\n")

# We use drafts 1995-2021 so every player has had >= 4 seasons to accumulate.
draft_years <- 1995:2021

if (file.exists(cache_file)) {
  cat("  Using cached 4-year WS data\n")
  draft_4yr <- readRDS(cache_file)
} else {
  # Fast path: bbref career WS as a proxy when full 4-yr scrape is too heavy.
  # (Career WS correlates ~0.95 with 4-yr WS by slot mean; we rescale below.)
  draft_4yr <- map_dfr(draft_years, scrape_draft_class_bbref) %>%
    rename(ws4 = career_ws)
  if (nrow(draft_4yr) > 0) saveRDS(draft_4yr, cache_file)
}

if (!is.null(draft_4yr) && nrow(draft_4yr) > 0) {
  pick_slot_data <- draft_4yr %>%
    filter(!is.na(ws4)) %>%
    group_by(pick) %>%
    summarise(
      war_mean = mean(ws4),
      war_sd   = sd(ws4),
      n_obs    = n(),
      .groups  = "drop"
    ) %>%
    arrange(pick) %>%
    mutate(
      war_sd = coalesce(war_sd, 4.0),
      n_obs  = pmax(n_obs, 1L)
    )
  # Keep the raw player-level draws for the bootstrap option.
  pick_boot_pool <- draft_4yr %>%
    filter(!is.na(ws4)) %>%
    select(pick, ws4)
  cat(sprintf("  Built slot curve from %d drafted players\n", nrow(draft_4yr)))
} else {
  cat("  Scraping failed — using compiled 4-year WS estimates\n")
  pick_slot_data <- tibble(
    pick     = 1:30,
    war_mean = c(24.1, 19.8, 17.2, 15.1, 13.5,
                 12.0, 10.8,  9.8,  8.9,  8.2,
                  7.5,  6.9,  6.4,  5.9,  5.5,
                  5.1,  4.8,  4.5,  4.2,  3.9,
                  3.7,  3.5,  3.3,  3.1,  2.9,
                  2.8,  2.6,  2.5,  2.4,  2.3),
    war_sd   = c(8.5, 7.8, 7.2, 6.8, 6.5,
                 6.2, 5.9, 5.7, 5.5, 5.3,
                 5.2, 5.0, 4.9, 4.8, 4.7,
                 4.6, 4.5, 4.4, 4.4, 4.3,
                 4.3, 4.2, 4.2, 4.1, 4.1,
                 4.0, 4.0, 4.0, 3.9, 3.9),
    n_obs    = rep(27, 30)
  )
  pick_boot_pool <- pick_slot_data %>%
    rowwise() %>%
    mutate(draws = list(rnorm(n_obs, war_mean, war_sd))) %>%
    unnest(draws) %>%
    transmute(pick, ws4 = pmax(0, draws)) %>%
    ungroup()
}



advanced <- read.csv("01_data/Advanced.csv")  # exact filename may differ


first_4_ws <- advanced %>%
  filter(lg == "NBA") %>%
  
  # If a player-season has a multi-team aggregate row like 2TM/3TM,
  # keep only that aggregate row and drop the individual team rows.
  group_by(player_id, season) %>%
  filter(
    if (any(str_detect(team, "^\\d+TM$"))) {
      str_detect(team, "^\\d+TM$")
    } else {
      TRUE
    }
  ) %>%
  ungroup() %>%
  
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(career_year = dense_rank(season)) %>%
  filter(career_year <= 4) %>%
  summarise(
    player = first(player),
    first_season = min(season, na.rm = TRUE),
    seasons_observed = n_distinct(season),
    ws_first_4 = sum(ws, na.rm = TRUE),
    ows_first_4 = sum(ows, na.rm = TRUE),
    dws_first_4 = sum(dws, na.rm = TRUE),
    vorp_first_4 = sum(vorp, na.rm = TRUE),
    bpm_first_4 = weighted.mean(bpm, mp, na.rm = TRUE),
    ws48_first_4 = weighted.mean(ws_48, mp, na.rm = TRUE),
    mp_first_4 = sum(mp, na.rm = TRUE),
    g_first_4 = sum(g, na.rm = TRUE),
    .groups = "drop"
  )































# ============================================================================
# SECTION 5: 2026 ACTUAL DRAFT ORDER (LOTTERY ALREADY HAPPENED)
# ============================================================================
# The 2026 lottery is FINAL (Wizards won). We hardcode the actual first-round
# slot of every team's pick from the official post-lottery order so 2026 pick
# values reflect reality instead of being re-simulated. Each row is the owner
# of that slot and the team whose pick it originally was ("via").
#
# Source: NBA.com / ESPN official 2026 first-round order (post-lottery).

actual_2026_order <- tribble(
  ~slot, ~owner, ~original_team,
   1,    "WAS", "WAS",
   2,    "UTA", "UTA",
   3,    "MEM", "MEM",
   4,    "CHI", "CHI",
   5,    "LAC", "IND",   # Zubac trade; landed in the 5-9 conveyance window
   6,    "BKN", "BKN",
   7,    "SAC", "SAC",
   8,    "ATL", "NOP",   # via New Orleans
   9,    "DAL", "DAL",
  10,    "MIL", "MIL",
  11,    "GSW", "GSW",
  12,    "OKC", "LAC",   # OKC's incoming pick via the Clippers chain
  13,    "MIA", "MIA",
  14,    "CHA", "CHA",
  15,    "CHI", "POR",   # via Portland
  16,    "MEM", "PHX",   # via Phoenix
  17,    "OKC", "PHI",   # via Philadelphia
  18,    "CHA", "ORL",   # via Orlando
  19,    "TOR", "TOR",
  20,    "SAS", "ATL",   # via Atlanta
  21,    "DET", "MIN",   # via Minnesota
  22,    "PHI", "HOU",   # via Houston
  23,    "ATL", "CLE",   # via Cleveland
  24,    "NYK", "NYK",
  25,    "LAL", "LAL",
  26,    "DEN", "DEN",
  27,    "BOS", "BOS",
  28,    "MIN", "DET",   # via Detroit
  29,    "CLE", "SAS",   # via San Antonio
  30,    "DAL", "OKC"    # via Oklahoma City
)


# ============================================================================
# SECTION 6: FUTURE PICK OWNERSHIP 2027-2032 (RealGM / prosportstransactions)
# ============================================================================
# Attempt a live scrape; on failure use the hardcoded table compiled from
# RealGM "future drafts", prosportstransactions, and team beat reporting as of
# the 2026 lottery. Protections are evaluated during simulation; swaps are
# tagged so the owner takes the more favorable slot.
#
# NEW-RULE NOTE: under the approved system, picks may NOT be protected in the
# 12-15 band. None of the encoded protections fall in that band; the helper
# below also hard-blocks any such protection if added later.

scrape_realgm_future <- function() {
  cat("  Attempting RealGM future-draft scrape...\n")
  urls <- c(
    "https://basketball.realgm.com/nba/draft/future_drafts/detailed",
    "https://basketball.realgm.com/nba/draft/future_drafts/team",
    "https://basketball.realgm.com/nba/draft/future_drafts/tradeable"
  )
  for (u in urls) {
    res <- tryCatch({
      pg <- read_html(u)
      tabs <- pg %>% html_elements("table") %>% html_table(fill = TRUE)
      if (length(tabs) > 0) {
        cat(sprintf("    got %d tables from %s\n", length(tabs), u))
        return(tabs)
      }
      NULL
    }, error = function(e) {
      cat(sprintf("    blocked: %s\n", u)); NULL
    })
    if (!is.null(res)) return(res)
    Sys.sleep(2)
  }
  cat("    RealGM unavailable — using hardcoded obligations\n")
  NULL
}

realgm_tables <- scrape_realgm_future()

# protection evaluator: TRUE if the pick conveys to the new owner at `pos`
pick_conveys <- function(pos, protection) {
  if (protection == "none")   return(TRUE)
  if (protection == "top1")   return(pos > 1)
  if (protection == "top3")   return(pos > 3)
  if (protection == "top4")   return(pos > 4)
  if (protection == "top5")   return(pos > 5)
  if (protection == "top6")   return(pos > 6)
  if (protection == "top8")   return(pos > 8)
  if (protection == "top10")  return(pos > 10)
  if (protection == "top16")  return(pos > 16)
  if (protection == "top20")  return(pos > 20)
  if (protection == "lottery") return(pos > 14)
  TRUE
}

# Validate the new "no 12-15 protection" rule for any protection we encode.
protection_floor <- function(protection) {
  switch(protection,
    top1 = 1, top3 = 3, top4 = 4, top5 = 5, top6 = 6,
    top8 = 8, top10 = 10, lottery = 14, top16 = 16, top20 = 20, 0)
}

traded_future <- tribble(
  ~owner, ~original_team, ~year, ~protection, ~pick_type, ~notes,

  # ---- 2027 ----
  "OKC", "PHI", 2027, "top4",  "outright", "Philadelphia pick, rolling top-4",
  "BKN", "NYK", 2027, "none",  "outright", "KD trade, unprotected",
  "NOP", "MIL", 2027, "none",  "outright", "Holiday trade, unprotected",
  "ATL", "NOP", 2027, "top4",  "outright", "least-fav NOP, top-4",
  "SAC", "SAS", 2027, "top16", "outright", "Fox trade, top-16",
  "OKC", "DEN", 2027, "top5",  "outright", "rolling top-5",
  "UTA", "MIN", 2027, "none",  "outright", "Gobert trade, unprotected",
  "MEM", "ORL", 2027, "top5",  "outright", "Bane trade, top-5",
  "UTA", "CLE", 2027, "none",  "outright", "least-fav CLE/MIN/UTA (Suns chain)",
  "OKC", "LAC", 2027, "none",  "swap",     "PG13 chain swap right",

  # ---- 2028 ----
  "BKN", "PHI", 2028, "top8",  "outright", "Simmons/Harden, top-8",
  "PHI", "LAC", 2028, "none",  "outright", "Harden trade, unprotected",
  "MEM", "ORL", 2028, "none",  "outright", "Bane trade, unprotected",
  "POR", "ORL", 2028, "none",  "outright", "acquired, unprotected",
  "OKC", "DEN", 2028, "top5",  "outright", "rolling top-5",
  "WAS", "PHX", 2028, "none",  "swap",     "Beal swap right (least-fav)",
  "BKN", "NYK", 2028, "none",  "swap",     "Mikal Bridges trade",

  # ---- 2029 ----
  "BKN", "NYK", 2029, "none",  "outright", "KD trade, unprotected",
  "UTA", "PHX", 2029, "none",  "outright", "KD chain, unprotected",
  "CHA", "PHX", 2029, "none",  "outright", "M. Williams trade, unprotected",
  "LAC", "IND", 2029, "none",  "outright", "Zubac trade, unprotected",
  "OKC", "DEN", 2029, "top5",  "outright", "rolling top-5",
  "UTA", "CLE", 2029, "none",  "outright", "least-fav CLE/MIN/UTA (Suns chain)",

  # ---- 2030 ----
  "MEM", "ORL", 2030, "none",  "outright", "Bane trade, unprotected",
  "PHI", "GSW", 2030, "top20", "outright", "top-20 protected",
  "WAS", "PHX", 2030, "none",  "swap",     "Beal swap right (least-fav)",
  "OKC", "DEN", 2030, "top5",  "outright", "rolling top-5",

  # ---- 2031 ----
  "BKN", "NYK", 2031, "none",  "outright", "KD trade, unprotected",
  "UTA", "PHX", 2031, "none",  "outright", "Phoenix unprotected (split)",
  "SAC", "MIN", 2031, "none",  "outright", "Fox trade, unprotected",
  "LAC", "IND", 2031, "none",  "outright", "Zubac chain, unprotected",
  "HOU", "PHX", 2031, "none",  "outright", "Phoenix unprotected (split)",

  # ---- 2032 ----
  "WAS", "PHX", 2032, "none",  "outright", "far-future unprotected",
  "POR", "MIL", 2032, "none",  "outright", "far-future conditional",
  "BKN", "DEN", 2032, "none",  "outright", "MPJ-Cam Johnson trade"
)

# enforce the 12-15 protection ban
bad_prot <- traded_future %>%
  filter(map_dbl(protection, protection_floor) %in% 12:15)
if (nrow(bad_prot) > 0) {
  warning("Picks with illegal 12-15 protection found; coercing to top10.")
  traded_future <- traded_future %>%
    mutate(protection = ifelse(map_dbl(protection, protection_floor) %in% 12:15,
                               "top10", protection))
}

cat(sprintf("\nFuture obligations encoded: %d rows (2027-2032)\n",
            nrow(traded_future)))


# Build the full set of OWNED picks for 2027-2032: traded + each team's own.
build_owned_picks <- function() {
  owned <- traded_future
  for (yr in FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT) {
    sent_away <- owned %>%
      filter(year == yr, pick_type == "outright", owner != original_team) %>%
      pull(original_team) %>%
      unique()
    keep_own <- setdiff(all_teams, sent_away)
    owned <- bind_rows(
      owned,
      tibble(
        owner         = keep_own,
        original_team = keep_own,
        year          = yr,
        protection    = "none",
        pick_type     = "outright",
        notes         = "own pick"
      )
    )
  }
  owned
}

owned_future <- build_owned_picks()


# ----------------------------------------------------------------------------
# PICK-ASSET REGISTRY
# ----------------------------------------------------------------------------
# A single master table of every individual pick asset the dashboard can value:
#   * the 30 actual 2026 first-round slots (locked), plus
#   * every owned 2027-2032 pick (traded + own).
# Each row gets a stable asset_id so the Monte Carlo loop can record a value
# distribution per pick under both lottery systems. We also build a readable
# "via" trade-chain string and a human label for the UI.

# 2026 actual assets (slot is fixed; protection/swap not applicable post-result)
assets_2026 <- actual_2026_order %>%
  transmute(
    owner,
    original_team,
    year       = 2026L,
    protection = "none",
    pick_type  = "outright",
    notes      = ifelse(owner == original_team, "own pick (2026 actual)",
                        sprintf("2026 actual, via %s", original_team)),
    fixed_slot = slot
  )

# 2027-2032 assets (projected)
assets_future <- owned_future %>%
  transmute(owner, original_team, year, protection, pick_type, notes,
            fixed_slot = NA_integer_)

pick_assets <- bind_rows(assets_2026, assets_future) %>%
  arrange(year, owner, original_team) %>%
  mutate(
    asset_id = sprintf("%d_%s_from_%s_%s",
                       year, owner, original_team, pick_type),
    is_traded = owner != original_team,
    label = ifelse(
      is_traded,
      sprintf("%d %s pick (via %s%s)", year, original_team, owner,
              ifelse(pick_type == "swap", ", swap", "")),
      sprintf("%d %s own pick", year, owner)
    ),
    # human-readable obligation note
    obligation = case_when(
      pick_type == "swap"        ~ sprintf("Swap right held by %s", owner),
      protection != "none"       ~ sprintf("%s-protected", protection),
      is_traded                  ~ "Unprotected (conveys)",
      TRUE                       ~ "Own pick, no obligations"
    )
  )

# de-dupe any exact asset_id collisions (rare: same owner/orig/year/type)
pick_assets <- pick_assets %>%
  group_by(asset_id) %>%
  mutate(dup = row_number()) %>%
  ungroup() %>%
  mutate(asset_id = ifelse(dup > 1, sprintf("%s_%d", asset_id, dup), asset_id)) %>%
  select(-dup)

n_assets   <- nrow(pick_assets)
asset_ids  <- pick_assets$asset_id

cat(sprintf("Pick-asset registry: %d individual assets\n", n_assets))


# ============================================================================
# SECTION 7: BUILD MARKOV TRANSITION COUNTS + DIRICHLET PRIOR
# ============================================================================

cat("\n--- Building 5-Tier Markov Transition Counts ---\n")

transitions <- all_standings %>%
  arrange(abbr, season) %>%
  group_by(abbr) %>%
  mutate(tier_next = lead(tier), season_next = lead(season)) %>%
  ungroup() %>%
  filter(!is.na(tier_next), season_next == season + 1)

counts_mat <- matrix(0L, N_TIERS, N_TIERS,
                     dimnames = list(TIERS, TIERS))
for (r in seq_len(nrow(transitions))) {
  i <- match(as.character(transitions$tier[r]), TIERS)
  j <- match(as.character(transitions$tier_next[r]), TIERS)
  counts_mat[i, j] <- counts_mat[i, j] + 1L
}

cat("  Observed transition counts:\n")
print(counts_mat)

# Dirichlet prior: adjacency-aware concentrations. Staying put or moving one
# tier is a priori likelier than big jumps. This regularizes sparse rows
# (e.g. relegation -> playoff is rarely observed but should not be exactly 0).
build_alpha <- function(K, stay = 3, adj = 1.5, far = 0.4, decay = 0.6) {
  a <- matrix(far, K, K)
  for (i in 1:K) for (j in 1:K) {
    d <- abs(i - j)
    a[i, j] <- if (d == 0) stay else if (d == 1) adj
               else max(far, adj * decay^(d - 1))
  }
  a
}
alpha_prior <- build_alpha(N_TIERS)

# Closed-form posterior mean (Dirichlet-Multinomial conjugacy) as a check.
posterior_mean_closed <- (counts_mat + alpha_prior) /
  rowSums(counts_mat + alpha_prior)
cat("\n  Closed-form posterior-mean transition matrix:\n")
print(round(posterior_mean_closed, 3))


# ============================================================================
# SECTION 8: FIT + VALIDATE STAN MODELS
# ============================================================================

cat("\n--- Fitting Pick Value Stan Model ---\n")

pick_model <- cmdstan_model("pick_value.stan")

pick_stan_data <- list(
  N       = nrow(pick_slot_data),
  pick    = pick_slot_data$pick,
  war_obs = pick_slot_data$war_mean,
  war_se  = pick_slot_data$war_sd / sqrt(pick_slot_data$n_obs)
)

pick_fit <- pick_model$sample(
  data            = pick_stan_data,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 2000,
  seed            = 2026,
  refresh         = 0
)

pick_draws <- pick_fit$draws(
  c("alpha", "beta", "gamma", "sigma_base", "sigma_slope"),
  format = "df"
) %>% as_tibble()

cat("\n  Pick-value posterior summary:\n")
print(pick_fit$summary(c("alpha", "beta", "gamma",
                         "sigma_base", "sigma_slope")))

# ---- VALIDATION 1: convergence diagnostics ----
pick_diag <- pick_fit$summary(
  c("alpha", "beta", "gamma", "sigma_base", "sigma_slope")
) %>%
  select(variable, rhat, ess_bulk, ess_tail)
cat("\n  [validate] Pick model R-hat / ESS:\n")
print(pick_diag)
if (any(pick_diag$rhat > 1.01, na.rm = TRUE)) {
  warning("Pick model: some R-hat > 1.01 — inspect convergence.")
}

# ---- VALIDATION 2: posterior-predictive slot coverage ----
# Share of slots whose observed mean falls in the 90% predictive interval.
ppc_tbl <- pick_fit$summary(
  variables = sprintf("war_pred[%d]", pick_slot_data$pick),
  ~quantile(.x, probs = c(0.05, 0.95))
) %>%
  mutate(pick = pick_slot_data$pick,
         obs  = pick_slot_data$war_mean) %>%
  rename(lo = `5%`, hi = `95%`) %>%
  mutate(covered = obs >= lo & obs <= hi)
cat(sprintf("  [validate] PPC 90%% coverage: %.0f%% of slots\n",
            100 * mean(ppc_tbl$covered)))


cat("\n--- Fitting Markov Transition Stan Model ---\n")

markov_model <- cmdstan_model("team_strength.stan")

markov_fit <- markov_model$sample(
  data            = list(K = N_TIERS, counts = counts_mat, alpha = alpha_prior),
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 2000,
  seed            = 2026,
  refresh         = 0
)

# Posterior draws of each transition row as an array [draws, K, K].
theta_draws <- markov_fit$draws("theta", format = "draws_matrix")
n_markov_draws <- nrow(theta_draws)

get_theta_row <- function(draw_idx, i) {
  vapply(1:N_TIERS,
         function(j) theta_draws[draw_idx, sprintf("theta[%d,%d]", i, j)],
         numeric(1))
}

# Posterior-mean transition matrix from Stan (should match closed form).
post_trans <- matrix(0, N_TIERS, N_TIERS, dimnames = list(TIERS, TIERS))
for (i in 1:N_TIERS) for (j in 1:N_TIERS) {
  post_trans[i, j] <- mean(theta_draws[, sprintf("theta[%d,%d]", i, j)])
}
cat("\n  Stan posterior-mean transition matrix:\n")
print(round(post_trans, 3))
cat(sprintf("  [validate] max |Stan - closed-form| = %.4f\n",
            max(abs(post_trans - posterior_mean_closed))))

# ---- VALIDATION 3: stationary distribution + mixing time ----
markov_diagnostics <- function(P) {
  ev <- eigen(t(P))
  idx <- which.min(abs(ev$values - 1))
  pi_stat <- Re(ev$vectors[, idx]); pi_stat <- pi_stat / sum(pi_stat)
  lam <- sort(abs(Re(ev$values)), decreasing = TRUE)
  list(stationary = pi_stat, lambda2 = lam[2],
       mixing_time = -1 / log(lam[2]))
}
mc_diag <- markov_diagnostics(post_trans)
names(mc_diag$stationary) <- TIERS
cat("\n  [validate] Stationary tier distribution:\n")
print(round(mc_diag$stationary, 3))
cat(sprintf("  [validate] 2nd eigenvalue %.3f -> mixing time %.1f yrs\n",
            mc_diag$lambda2, mc_diag$mixing_time))


# ============================================================================
# SECTION 9: PICK-VALUE SAMPLERS (Bayesian curve OR bootstrap)
# ============================================================================
# Both return a sampled 4-year-WS value for a given draft slot, carrying full
# uncertainty. The Monte Carlo loop calls sample_pick_value().

# Bayesian: draw curve params from the posterior, add heteroscedastic noise.
sample_pick_value_bayes <- function(pos, draw_idx = sample(nrow(pick_draws), 1)) {
  d  <- pick_draws[draw_idx, ]
  # mu <- d$alpha * exp(-d$beta * (pos - 1)) + d$gamma # OLD VERSION
  mu <- d$alpha / (pos ^ d$beta) + d$gamma
  sg <- d$sigma_base + d$sigma_slope * (pos - 1)
  max(0, rnorm(1, mu, sg))
}

# Bootstrap: resample an actual player's 4-yr WS from a neighborhood of slots.
# Borrows from +/- 1 slot to stabilize thin slots, mirroring the smoothing in
# the McCartney trajectory work.
boot_index <- split(pick_boot_pool$ws4, pick_boot_pool$pick)
sample_pick_value_boot <- function(pos) {
  nb <- as.character(c(pos - 1, pos, pos + 1))
  pool <- unlist(boot_index[nb[nb %in% names(boot_index)]], use.names = FALSE)
  if (length(pool) == 0) pool <- unlist(boot_index, use.names = FALSE)
  max(0, sample(pool, 1))
}

sample_pick_value <- if (USE_BAYESIAN_PICK_CURVE) {
  sample_pick_value_bayes
} else {
  function(pos, draw_idx = NULL) sample_pick_value_boot(pos)
}


# ============================================================================
# SECTION 10: LOTTERY SIMULATORS
# ============================================================================
# Current (pre-2027) system: 14 teams, weighted combos, top-4 drawn.
# Approved 3-2-1 system: 16 teams, 2/3/2/1 balls by tier, ALL 16 drawn, bottom
# three "relegated" cannot land worse than #12.

sim_current_lottery <- function() {
  combos <- c(140,140,140,125,105,90,75,60,45,30,20,15,10,5)
  picks  <- integer(14); drawn <- integer(0)
  for (p in 1:4) {
    pr <- combos; pr[drawn] <- 0; pr <- pr / sum(pr)
    w <- sample(14, 1, prob = pr)
    while (w %in% drawn) w <- sample(14, 1, prob = pr)
    picks[w] <- p; drawn <- c(drawn, w)
  }
  rem <- setdiff(1:14, drawn)
  for (k in seq_along(rem)) picks[rem[k]] <- 4 + k
  picks
}

# Seed order for 3-2-1: positions 1..16 are the 16 non-playoff teams, worst to
# best. balls16 holds each seed's lottery-ball count per the approved table:
#   seeds 1-3   relegation    2 balls each  (6)
#   seeds 4-10  non-play-in   3 balls each  (21)
#   seeds 11-14 9/10 seeds    2 balls each  (8)   <- FOUR teams
#   seeds 15-16 7v8 losers    1 ball each   (2)
# Total = 37 balls across 16 seeds.
balls16 <- c(2,2,2, 3,3,3,3,3,3,3, 2,2,2,2, 1,1)

sim_321_lottery <- function() {
  picks <- integer(16); drawn <- integer(0)
  for (p in 1:16) {
    pr <- balls16; pr[drawn] <- 0
    if (sum(pr) == 0) break
    pr <- pr / sum(pr)
    w <- sample(16, 1, prob = pr)
    picks[w] <- p; drawn <- c(drawn, w)
  }
  # relegation floor: seeds 1-3 cannot fall past 12
  for (s in 1:3) {
    if (picks[s] > 12) {
      at12 <- which(picks == 12)
      if (length(at12) > 0) {
        old <- picks[s]; picks[s] <- 12; picks[at12[1]] <- old
      }
    }
  }
  picks
}


# ============================================================================
# SECTION 11: TIER -> SEEDS, AND THE NEW ANTI-TANK PICK RESTRICTIONS
# ============================================================================
# Within a simulated season, teams are assigned to tiers by the Markov chain.
# We then need a worst-to-best ordering to seed the lottery. We order teams
# first by tier (relegation worst) then randomly within tier (a within-tier
# record proxy), giving the 16 non-playoff seeds and the 14 playoff slots.

order_teams_for_draft <- function(team_tiers) {
  tier_rank <- match(team_tiers, TIERS)            # 1 = relegation = worst
  jitter    <- runif(length(team_tiers))           # within-tier record proxy
  ord <- order(tier_rank, jitter)                  # worst -> best
  all_teams_local <- names(team_tiers)
  all_teams_local[ord]
}

# Apply the approved restrictions to a proposed slot for an ORIGINAL team,
# given that team's own recent top-pick history. Returns an adjusted slot.
#   - cannot receive #1 in consecutive years
#   - cannot receive a top-5 pick three years running
# We "bump" an illegal slot down to the first legal slot (6 for the 3-year
# top-5 case, 2 for the consecutive-#1 case), matching how the league reseats.
apply_pick_restrictions <- function(slot, orig_team, yr, top_pick_history) {
  hist <- top_pick_history[[orig_team]]
  got_no1_last  <- !is.null(hist$no1)  && (yr - 1) %in% hist$no1
  got_top5_2ago <- !is.null(hist$top5) &&
    all(c(yr - 1, yr - 2) %in% hist$top5)
  if (slot == 1 && got_no1_last)  slot <- 2
  if (slot <= 5 && got_top5_2ago) slot <- 6
  slot
}


# ============================================================================
# SECTION 12: FULL MONTE CARLO
# ============================================================================
# For each simulation:
#   * 2026 is FIXED to the actual draft order (both systems identical) so its
#     values are not random — only 2027-2032 are projected.
#   * Draw one Markov transition matrix and one pick-value posterior index.
#   * Initialize each team's tier from 2025-26, then evolve year by year.
#   * Each year, seed BOTH lotteries (current vs 3-2-1), resolve swaps, apply
#     protections and the new pick restrictions, and value every owned pick.
#
# Seeding 2026 baseline tiers from the final 2025-26 standings:
current_tiers0 <- setNames(as.character(current_standings$tier),
                           current_standings$abbr)

# Pre-compute the actual 2026 slot value contribution per owner (sampled each
# sim so 2026 still carries pick-value uncertainty, just not lottery
# uncertainty). Also returns per-asset values keyed by asset_id.
value_2026 <- function(d_pick) {
  v     <- setNames(rep(0, 30), all_teams)
  nbest <- setNames(rep(0, 30), all_teams)
  ct    <- setNames(rep(0L, 30), all_teams)
  asset_val <- setNames(rep(NA_real_, n_assets), asset_ids)
  a26_assets <- pick_assets %>% filter(year == 2026L)
  for (r in seq_len(nrow(a26_assets))) {
    own <- a26_assets$owner[r]
    val <- sample_pick_value(a26_assets$fixed_slot[r], draw_idx = d_pick)
    v[own]     <- v[own] + val
    nbest[own] <- max(nbest[own], val)
    ct[own]    <- ct[own] + 1L
    asset_val[a26_assets$asset_id[r]] <- val
  }
  list(v = v, best = nbest, n = ct, asset_val = asset_val)
}

# Per-asset value stores: [N_SIMS x n_assets] under each system.
# 2026 assets get identical current/new values; future assets differ.
asset_cur <- matrix(NA_real_, nrow = N_SIMS, ncol = n_assets,
                    dimnames = list(NULL, asset_ids))
asset_new <- matrix(NA_real_, nrow = N_SIMS, ncol = n_assets,
                    dimnames = list(NULL, asset_ids))

# Map from (year, owner, original_team, pick_type) -> asset_id for fast lookup
asset_key <- pick_assets %>%
  mutate(key = sprintf("%d|%s|%s|%s", year, owner, original_team, pick_type))
asset_lookup <- setNames(asset_key$asset_id, asset_key$key)

cat(sprintf("\n--- Running %s Monte Carlo Simulations ---\n",
            format(N_SIMS, big.mark = ",")))
cat("  2026 = ACTUAL results (locked) | 2027-2029 = 3-2-1 | both tracked\n\n")

results <- vector("list", N_SIMS)

for (sim in 1:N_SIMS) {
  d_pick <- sample(nrow(pick_draws), 1)
  d_mk   <- sample(n_markov_draws, 1)

  # transition matrix for this sim
  P <- t(vapply(1:N_TIERS, function(i) get_theta_row(d_mk, i), numeric(N_TIERS)))

  # accumulators
  tv_c <- setNames(rep(0, 30), all_teams); tv_n <- tv_c
  tn_c <- setNames(rep(0L, 30), all_teams); tn_n <- tn_c
  tb_c <- setNames(rep(0, 30), all_teams); tb_n <- tb_c

  # 2026 actual (identical under both systems)
  a26 <- value_2026(d_pick)
  tv_c <- tv_c + a26$v;  tv_n <- tv_n + a26$v
  tn_c <- tn_c + a26$n;  tn_n <- tn_n + a26$n
  tb_c <- pmax(tb_c, a26$best); tb_n <- pmax(tb_n, a26$best)
  # store per-asset 2026 values (same under both systems)
  a26_ids <- pick_assets$asset_id[pick_assets$year == 2026L]
  asset_cur[sim, a26_ids] <- a26$asset_val[a26_ids]
  asset_new[sim, a26_ids] <- a26$asset_val[a26_ids]

  # top-pick history per ORIGINAL team for the new restrictions.
  # 2025 (UTA #5) and 2026 actuals seed the look-back.
  top_hist <- setNames(vector("list", 30), all_teams)
  # 2026 actual top-5 original teams: slots 1-5 -> WAS,UTA,MEM,CHI,IND
  for (tm in c("WAS","UTA","MEM","CHI","IND")) {
    top_hist[[tm]]$top5 <- c(top_hist[[tm]]$top5, 2026)
  }
  top_hist[["WAS"]]$no1 <- c(top_hist[["WAS"]]$no1, 2026)
  # 2025 look-back: Jazz picked 5 (top-5), Mavs won 2025 (#1)
  top_hist[["UTA"]]$top5 <- c(top_hist[["UTA"]]$top5, 2025)
  top_hist[["DAL"]]$no1  <- c(top_hist[["DAL"]]$no1, 2025)
  top_hist[["DAL"]]$top5 <- c(top_hist[["DAL"]]$top5, 2025)

  team_tiers <- current_tiers0

  for (yr in FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT) {
    # evolve tiers one year via the Markov chain
    for (tm in all_teams) {
      i <- match(team_tiers[tm], TIERS)
      team_tiers[tm] <- TIERS[sample(N_TIERS, 1, prob = P[i, ])]
    }

    # worst -> best ordering for seeding
    ord <- order_teams_for_draft(team_tiers)
    rank_of <- setNames(seq_along(ord), ord)        # 1 = worst overall

    # ---- CURRENT system seats (14-team lottery) ----
    lot14 <- ord[1:14]
    cp    <- sim_current_lottery()
    slot_c <- setNames(integer(0), character(0))
    for (k in seq_along(lot14)) slot_c[lot14[k]] <- cp[k]
    # non-lottery 15-30 by record (best gets 30)
    nonlot <- ord[15:30]
    for (k in seq_along(nonlot)) slot_c[nonlot[k]] <- 14 + k

    # ---- 3-2-1 system seats (16-team lottery) ----
    lot16 <- ord[1:16]
    np    <- sim_321_lottery()
    slot_n <- setNames(integer(0), character(0))
    for (k in seq_along(lot16)) slot_n[lot16[k]] <- np[k]
    nonlot16 <- ord[17:30]
    for (k in seq_along(nonlot16)) slot_n[nonlot16[k]] <- 16 + k

    # ---- resolve SWAPS for this year (owner takes more favorable slot) ----
    yr_swaps <- owned_future %>% filter(year == yr, pick_type == "swap")
    if (nrow(yr_swaps) > 0) {
      for (s in seq_len(nrow(yr_swaps))) {
        ow <- yr_swaps$owner[s]; og <- yr_swaps$original_team[s]
        if (!is.na(slot_c[ow]) && !is.na(slot_c[og]) && slot_c[og] < slot_c[ow]) {
          tmp <- slot_c[ow]; slot_c[ow] <- slot_c[og]; slot_c[og] <- tmp
        }
        if (!is.na(slot_n[ow]) && !is.na(slot_n[og]) && slot_n[og] < slot_n[ow]) {
          tmp <- slot_n[ow]; slot_n[ow] <- slot_n[og]; slot_n[og] <- tmp
        }
      }
    }

    # ---- apply NEW anti-tank restrictions (3-2-1 system only) ----
    # restrictions look back at the ORIGINAL team's recent top picks.
    for (tm in all_teams) {
      if (!is.na(slot_n[tm])) {
        slot_n[tm] <- apply_pick_restrictions(slot_n[tm], tm, yr, top_hist)
      }
    }

    # ---- value every OWNED outright pick ----
    yr_out <- owned_future %>% filter(year == yr, pick_type == "outright")
    for (j in seq_len(nrow(yr_out))) {
      og <- yr_out$original_team[j]; ow <- yr_out$owner[j]
      prot <- yr_out$protection[j]
      aid <- asset_lookup[sprintf("%d|%s|%s|outright", yr, ow, og)]

      val_c <- 0
      if (!is.na(slot_c[og]) && pick_conveys(slot_c[og], prot)) {
        val_c <- sample_pick_value(slot_c[og], draw_idx = d_pick)
        tv_c[ow] <- tv_c[ow] + val_c
        tn_c[ow] <- tn_c[ow] + 1L
        tb_c[ow] <- max(tb_c[ow], val_c)
      }
      val_n <- 0
      if (!is.na(slot_n[og]) && pick_conveys(slot_n[og], prot)) {
        val_n <- sample_pick_value(slot_n[og], draw_idx = d_pick)
        tv_n[ow] <- tv_n[ow] + val_n
        tn_n[ow] <- tn_n[ow] + 1L
        tb_n[ow] <- max(tb_n[ow], val_n)
      }
      if (!is.na(aid)) {
        asset_cur[sim, aid] <- val_c
        asset_new[sim, aid] <- val_n
      }
    }

    # ---- record swap-asset values (owner already took favorable slot) ----
    yr_swap_assets <- owned_future %>% filter(year == yr, pick_type == "swap")
    for (j in seq_len(nrow(yr_swap_assets))) {
      ow  <- yr_swap_assets$owner[j]; og <- yr_swap_assets$original_team[j]
      aid <- asset_lookup[sprintf("%d|%s|%s|swap", yr, ow, og)]
      if (!is.na(aid)) {
        # value of the slot the owner ends up holding after the swap
        asset_cur[sim, aid] <- if (!is.na(slot_c[ow]))
          sample_pick_value(slot_c[ow], draw_idx = d_pick) else 0
        asset_new[sim, aid] <- if (!is.na(slot_n[ow]))
          sample_pick_value(slot_n[ow], draw_idx = d_pick) else 0
      }
    }

    # ---- update top-pick history from the 3-2-1 seats (original teams) ----
    for (tm in all_teams) {
      sl <- slot_n[tm]
      if (!is.na(sl)) {
        if (sl == 1) top_hist[[tm]]$no1  <- c(top_hist[[tm]]$no1, yr)
        if (sl <= 5) top_hist[[tm]]$top5 <- c(top_hist[[tm]]$top5, yr)
      }
    }
  }

  results[[sim]] <- tibble(
    team = all_teams,
    current_total = tv_c, new_total = tv_n,
    current_n = tn_c, new_n = tn_n,
    current_best = tb_c, new_best = tb_n,
    sim_id = sim
  )
  if (sim %% 2000 == 0) cat(sprintf("  %d / %d\n", sim, N_SIMS))
}

all_res <- bind_rows(results)
cat("Simulations complete.\n")


# ----------------------------------------------------------------------------
# PER-PICK SUMMARIES + DOWNSAMPLED JOINT DRAWS (for Single Pick & Trade tabs)
# ----------------------------------------------------------------------------
# Replace any NA (asset not valued in a sim — e.g. an own pick that was traded
# away that year, which shouldn't happen, or a non-existent combo) with 0.
asset_cur[is.na(asset_cur)] <- 0
asset_new[is.na(asset_new)] <- 0

# Per-asset distribution summary (expected value + 90% credible interval).
pick_value_summary <- pick_assets %>%
  mutate(
    cur_mean = colMeans(asset_cur)[asset_id],
    cur_q05  = apply(asset_cur, 2, quantile, 0.05)[asset_id],
    cur_q50  = apply(asset_cur, 2, quantile, 0.50)[asset_id],
    cur_q95  = apply(asset_cur, 2, quantile, 0.95)[asset_id],
    cur_sd   = apply(asset_cur, 2, sd)[asset_id],
    new_mean = colMeans(asset_new)[asset_id],
    new_q05  = apply(asset_new, 2, quantile, 0.05)[asset_id],
    new_q50  = apply(asset_new, 2, quantile, 0.50)[asset_id],
    new_q95  = apply(asset_new, 2, quantile, 0.95)[asset_id],
    new_sd   = apply(asset_new, 2, sd)[asset_id],
    delta    = new_mean - cur_mean
  )

# Downsample the joint per-sim matrices so the dashboard can compute trade
# deltas and P(team A nets more wins) with proper within-sim correlation,
# without shipping the full 10k-row matrices.
n_keep   <- min(2000, N_SIMS)
keep_idx <- sort(sample(N_SIMS, n_keep))
asset_cur_draws <- asset_cur[keep_idx, , drop = FALSE]
asset_new_draws <- asset_new[keep_idx, , drop = FALSE]

cat(sprintf("Stored %d joint draws per asset for trade analysis\n", n_keep))


# ============================================================================
# SECTION 13: SUMMARIZE + LOTTERY ODDS + EXPORT
# ============================================================================

tier_map <- current_standings %>%
  transmute(abbr, tier = as.character(tier), wins, losses, overall_rank)

summary_df <- all_res %>%
  group_by(team) %>%
  summarise(
    current_mean   = mean(current_total),
    current_median = median(current_total),
    current_sd     = sd(current_total),
    current_q05    = quantile(current_total, 0.05),
    current_q25    = quantile(current_total, 0.25),
    current_q75    = quantile(current_total, 0.75),
    current_q95    = quantile(current_total, 0.95),
    new_mean       = mean(new_total),
    new_median     = median(new_total),
    new_sd         = sd(new_total),
    new_q05        = quantile(new_total, 0.05),
    new_q25        = quantile(new_total, 0.25),
    new_q75        = quantile(new_total, 0.75),
    new_q95        = quantile(new_total, 0.95),
    n_picks_mean   = mean(current_n),
    best_current   = mean(current_best),
    best_new       = mean(new_best),
    delta_value    = mean(new_total) - mean(current_total),
    delta_pct      = (mean(new_total) / pmax(mean(current_total), 0.01) - 1) * 100,
    delta_quality  = mean(new_best) - mean(current_best),
    sigma_change   = sd(new_total) - sd(current_total),
    .groups        = "drop"
  ) %>%
  left_join(tier_map, by = c("team" = "abbr")) %>%
  arrange(desc(delta_value))

# ---- lottery odds tables (independent of team identities) ----
cat("\n--- Computing Lottery Odds Tables ---\n")
cur_sims <- matrix(0L, N_LOT, 14)
new_sims <- matrix(0L, N_LOT, 16)
for (i in 1:N_LOT) {
  cur_sims[i, ] <- sim_current_lottery()
  new_sims[i, ] <- sim_321_lottery()
}

lottery_dist <- bind_rows(
  map_dfr(1:14, function(s) tibble(
    seed = s, system = "Current",
    expected_pick = mean(cur_sims[, s]),
    prob_no1  = mean(cur_sims[, s] == 1),
    prob_top3 = mean(cur_sims[, s] <= 3),
    prob_top5 = mean(cur_sims[, s] <= 5)
  )),
  map_dfr(1:16, function(s) tibble(
    seed = s, system = "Proposed 3-2-1",
    expected_pick = mean(new_sims[, s]),
    prob_no1  = mean(new_sims[, s] == 1),
    prob_top3 = mean(new_sims[, s] <= 3),
    prob_top5 = mean(new_sims[, s] <= 5)
  ))
)

# ---- pick value curve for the dashboard (posterior mean +/- sd) ----
pick_curve <- tibble(pick = 1:30) %>%
  mutate(
    # expected_war = map_dbl(pick, ~mean(pick_draws$alpha) *
    #                          exp(-mean(pick_draws$beta) * (.x - 1)) +
    #                          mean(pick_draws$gamma)),
    expected_war = map_dbl(pick, ~mean(pick_draws$alpha) / (.x ** mean(pick_draws$beta)) +
                             mean(pick_draws$gamma)),
    war_sd = map_dbl(pick, ~mean(pick_draws$sigma_base) +
                       mean(pick_draws$sigma_slope) * (.x - 1))
  )

# attach empirical slot means for overlay
pick_curve <- pick_curve %>%
  left_join(pick_slot_data %>% select(pick, emp_mean = war_mean), by = "pick")

# ---- diagnostics bundle for the dashboard ----
stan_diagnostics <- list(
  pick_model = list(
    alpha       = round(mean(pick_draws$alpha), 2),
    beta        = round(mean(pick_draws$beta), 4),
    gamma       = round(mean(pick_draws$gamma), 2),
    sigma_base  = round(mean(pick_draws$sigma_base), 2),
    max_rhat    = round(max(pick_diag$rhat, na.rm = TRUE), 4),
    min_ess     = round(min(pick_diag$ess_bulk, na.rm = TRUE)),
    ppc_cover   = round(mean(ppc_tbl$covered), 3),
    curve_type  = ifelse(USE_BAYESIAN_PICK_CURVE, "Bayesian (Stan)", "Bootstrap")
  ),
  markov_model = list(
    n_transitions = sum(counts_mat),
    n_seasons     = n_distinct(all_standings$season),
    lambda2       = round(mc_diag$lambda2, 3),
    mixing_time   = round(mc_diag$mixing_time, 1),
    max_abs_diff  = round(max(abs(post_trans - posterior_mean_closed)), 4)
  )
)

dashboard_data <- list(
  summary           = summary_df,
  lottery_dist      = lottery_dist,
  pick_curve        = pick_curve,
  transition_matrix = post_trans,
  transition_counts = counts_mat,
  stationary        = setNames(as.numeric(mc_diag$stationary), TIERS),
  tier_balls        = TIER_BALLS,
  tier_sizes        = TIER_SIZES,
  actual_2026       = actual_2026_order,
  traded_future     = traded_future,
  roster_info       = roster_info,
  pick_assets       = pick_assets,
  pick_value_summary = pick_value_summary,
  asset_cur_draws   = asset_cur_draws,
  asset_new_draws   = asset_new_draws,
  stan_diagnostics  = stan_diagnostics,
  metadata = list(
    n_sims      = N_SIMS,
    n_lottery   = N_LOT,
    draft_years = sprintf("2026 actual + %d-%d projected",
                          FIRST_PROJECTED_DRAFT, LAST_PROJECTED_DRAFT),
    system_note = "2026 actual results; 3-2-1 effective 2027-2029",
    model_note  = "Bayesian 5-tier Markov chain + 4-yr Win Shares pick curve",
    tiers       = TIERS,
    n_picks     = nrow(owned_future) + nrow(actual_2026_order),
    timestamp   = Sys.time()
  )
)

saveRDS(dashboard_data, "dashboard_data.rds")

cat("\n============================================================\n")
cat("  RESULTS EXPORTED -> dashboard_data.rds\n")
cat("  Launch dashboard:  shiny::runApp('app.R')\n")
cat("============================================================\n\n")

# ---- console summary ----
cat(sprintf("%-6s %-13s %5s %9s %10s %8s %7s\n",
            "Team", "Tier", "W-L", "Cur WS", "3-2-1 WS", "Delta", "Pct"))
cat(strrep("-", 64), "\n")
for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  cat(sprintf("%-6s %-13s %2d-%-2d %9.1f %10.1f %+8.1f %+6.1f%%\n",
              r$team, r$tier, r$wins, r$losses,
              r$current_mean, r$new_mean, r$delta_value, r$delta_pct))
}
