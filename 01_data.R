## ═════════════════════════════════════════════════════════════════════════════
# NBA Draft Lottery Rule Change Impact Model
#
#   1. Lottery system is now the APPROVED 3-2-1 format effective 2027-2029. 
#      2026 pick values are locked to the actual draft slots.
#   2. Markov chain states are the FIVE 3-2-1 tiers (relegation / non-play-in /
#      9-10 seed / 7v8 play-in loser / playoff), not generic standings buckets.
#   3. Pick value = xRAPM wins above replacement (-2.0 baseline) over the four
#      rookie-contract seasons after the draft.
#   4. New anti-tank pick restrictions are modeled: no team may receive the #1
#      pick in consecutive years or a top-5 pick three years running (applies
#      to the originally-owning team, looking back to 2025).
#   5. Future pick ownership refreshed from RealGM.
#
# Pipeline:
#   1. Scrape standings (bbref), rosters, draft production
#   2. Build Markov transition counts from history
#   3. Fit Stan models (pick value + Markov) and validate them
#   4. Monte Carlo: project tiers forward, run both old and new lotteries, value every
#      owned pick under each system, applying protections / swaps / new rules
#   5. Export dashboard_data.rds for the Shiny app
#
## ═════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(rvest)
library(httr)
library(cmdstanr)
library(janitor)
library(posterior)
library(expm)
library(loo)
library(hoopR)
library(dplyr)

hoopR_available <- requireNamespace("hoopR", quietly = TRUE)

# Reusable functions for every pipeline script (01-04)
source("00_helpers.R")

set.seed(2026)





## ═════════════════════════════════════════════════════════════════════════════
## 00 - CONFIGURE --------------------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════

N_SIMS                 <- 10000   # Monte Carlo iterations
N_LOT                  <- 50000   # lottery-only sims for odds tables
FIRST_PROJECTED_DRAFT  <- 2027    # first year teams' finishes are projected
LAST_PROJECTED_DRAFT   <- 2032    # 7-year horizon (2026 actual + 2027-2032)
HISTORY_START          <- 2005    # first season for transition counts
HISTORY_END            <- 2026    # last completed season
draft_years            <- 2015:2022  # Tracking-era classes: xRAPM's inputs change before
                                     # the mid-2010s, and the game has changed since.
                                     # 2022 is the last class with four completed
                                     # seasons (through 2025-26)

# Pick value = xRAPM wins above replacement over the four rookie-contract
# seasons after the draft (season-end years draft+1 .. draft+4).
XRAPM_REPLACEMENT      <- -2.0       # replacement level, points per 100 possessions
                                     # relative to league average (B-Ref BPM convention)
POINTS_PER_WIN         <- 82 / 2.7   # ~30.4 points of margin per win; matches the
                                     # 2.7 VORP-to-wins factor
VALUE_WINDOW_SEASONS   <- 4L         # rookie-contract window length
SHORTENED_SEASON_GAMES <- c(`1999` = 50, `2012` = 66,  # lockouts
                            `2020` = 70.6, `2021` = 72)  # COVID (2020 = league avg)
SCALE_TO_82_GAMES      <- TRUE       # rescale shortened seasons to an 82-game season
FIT_R1_COMPARISON_MODELS <- TRUE    # legacy R1 variants have Win Shares-scale priors
XRAPM_CACHE            <- "01_data/xrapm_cache.rds"
POSSESSIONS_CACHE      <- "01_data/possessions_cache.rds"

# The five 3-2-1 tiers, worst -> best, with lottery balls per team
TIERS <- c("relegation", "nonplayin", "playin_seed", "playin_loser", "playoff")
TIER_BALLS <- c(relegation   = 2,
                nonplayin    = 3,
                playin_seed  = 2,
                playin_loser = 1,
                playoff      = 0)
N_TIERS <- length(TIERS)

# Draft-slot constants. Round 1 uses slots 1-30; round 2 uses slots 31-60.
FIRST_ROUND_SLOTS  <- 1:30
SECOND_ROUND_SLOTS <- 31:60
N_DRAFT_SLOTS      <- 60

# How many teams sit in each tier in a normal season
TIER_SIZES <- c(relegation   = 3,
                nonplayin    = 7,
                playin_seed  = 4,
                playin_loser = 2,
                playoff      = 14)

team_name_to_abbr <- c("Oklahoma City Thunder"  = "OKC", 
                       "San Antonio Spurs"      = "SAS",
                       "Detroit Pistons"        = "DET", 
                       "Boston Celtics"         = "BOS",
                       "Denver Nuggets"         = "DEN", 
                       "New York Knicks"        = "NYK",
                       "Los Angeles Lakers"     = "LAL", 
                       "Houston Rockets"        = "HOU",
                       "Cleveland Cavaliers"    = "CLE", 
                       "Minnesota Timberwolves" = "MIN",
                       "Toronto Raptors"        = "TOR", 
                       "Atlanta Hawks"          = "ATL",
                       "Phoenix Suns"           = "PHX", 
                       "Orlando Magic"          = "ORL",
                       "Philadelphia 76ers"     = "PHI", 
                       "Charlotte Hornets"      = "CHA",
                       "Miami Heat"             = "MIA", 
                       "Los Angeles Clippers"   = "LAC",
                       "Portland Trail Blazers" = "POR", 
                       "Golden State Warriors"  = "GSW",
                       "Milwaukee Bucks"        = "MIL", 
                       "Chicago Bulls"          = "CHI",
                       "New Orleans Pelicans"   = "NOP", 
                       "Dallas Mavericks"       = "DAL",
                       "Memphis Grizzlies"      = "MEM", 
                       "Utah Jazz"              = "UTA",
                       "Sacramento Kings"       = "SAC", 
                       "Brooklyn Nets"          = "BKN",
                       "Indiana Pacers"         = "IND", 
                       "Washington Wizards"     = "WAS",
                       # historical / alternate names
                       "Charlotte Bobcats"      = "CHA", 
                       "New Jersey Nets"        = "BKN",
                       "Seattle SuperSonics"    = "OKC", 
                       "New Orleans Hornets"    = "NOP",
                       "New Orleans/Oklahoma City Hornets" = "NOP", 
                       "Vancouver Grizzlies"    = "MEM")





## ═════════════════════════════════════════════════════════════════════════════
## 01 - DATA SCRAPING ----------------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════

# Season standings from Basketball-Reference, one season at a time:
#   - try the league summary page first, then the standings page
#   - parse visible tables plus tables hidden inside HTML comments
#   - keep only conference standings tables (W and L columns, Eastern/Western
#     label, at least 10 team rows)
#   - stop at the first page that yields all 30 teams
standings_by_season <- list()

for (season_end_year in HISTORY_START:HISTORY_END) {
  cat(sprintf("  [bbref] standings %d-%d\n",
              season_end_year - 1, season_end_year))

  urls <- c(sprintf("https://www.basketball-reference.com/leagues/NBA_%d.html",
                    season_end_year),
            sprintf("https://www.basketball-reference.com/leagues/NBA_%d_standings.html",
                    season_end_year))
  season_standings <- tibble()

  for (url in urls) {
    # Request the page (3-4.5 second delay between requests, up to 3 retries)
    Sys.sleep(3 + runif(1, 0, 1.5))

    resp <- tryCatch(
      httr::RETRY(
        verb = "GET",
        url = url,
        times = 3,
        pause_min = 2,
        pause_cap = 8,
        httr::user_agent(paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
                                " AppleWebKit/537.36 Chrome/125 Safari/537.36")),
        httr::add_headers(
          `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          `Accept-Language` = "en-US,en;q=0.9",
          `Referer` = "https://www.basketball-reference.com/"),
        httr::timeout(30)),
      error = function(e) NULL)

    if (is.null(resp) || httr::status_code(resp) >= 400) next

    page <- tryCatch(xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8"),
                                     options = "HUGE"),
                     error = function(e) NULL)
    if (is.null(page)) next

    # All HTML tables: visible ones plus ones stored inside HTML comments
    visible_tables <- tryCatch(page %>%
                                 rvest::html_elements("table") %>%
                                 rvest::html_table(fill = TRUE),
                               error = function(e) list())

    comment_txt <- tryCatch(page %>%
                              rvest::html_elements(xpath = "//comment()") %>%
                              rvest::html_text(),
                            error = function(e) character(0))

    comment_tables <- list()
    for (txt in comment_txt[str_detect(comment_txt, "<table")]) {
      comment_tables <- c(comment_tables,
                          tryCatch(xml2::read_html(paste0("<html><body>", 
                                                          txt, 
                                                          "</body></html>"),
                                                   options = "HUGE") %>%
                                     rvest::html_elements("table") %>%
                                     rvest::html_table(fill = TRUE),
                                   error = function(e) list()))
    }

    # Standardize each table and keep the conference standings tables
    parsed <- tibble()

    for (tbl in c(visible_tables, comment_tables)) {
      tbl <- suppressMessages(tbl %>%
                                as_tibble(.name_repair = "unique") %>%
                                janitor::clean_names())

      if (nrow(tbl) == 0 || ncol(tbl) < 3) next

      nm <- names(tbl)

      wins_col <- nm[nm %in% c("w", "wins")][1]
      loss_col <- nm[nm %in% c("l", "losses")][1]

      if (is.na(wins_col) || is.na(loss_col)) next

      # Team-name column = first column that is not a known stat column
      team_col <- setdiff(nm, c(wins_col, loss_col, "w_l_percent",
                                "gb", "ps_g", "pa_g", "srs",
                                "pw", "pl", "mov", "sos", "or_tg",
                                "dr_tg", "nr_tg", "pace", "f_tr",
                                "x3p_ar", "ts_percent", "e_fg_percent",
                                "tov_percent", "orb_percent",
                                "ft_fga", "opp_e_fg_percent",
                                "opp_tov_percent", "opp_drb_percent",
                                "opp_ft_fga", "arena", "attend", "attend_g"))[1]

      if (is.na(team_col)) team_col <- nm[1]

      # Conference comes from the team column header (e.g. "eastern_conference")
      conf_val <- case_when(str_detect(team_col,
                                       regex("eastern",
                                             ignore_case = TRUE)) ~ "East",
                            str_detect(team_col,
                                       regex("western",
                                             ignore_case = TRUE)) ~ "West",
                            TRUE ~ NA_character_)

      tbl_standings <- tbl %>%
        transmute(team_raw = as.character(.data[[team_col]]),
                  wins     = suppressWarnings(as.integer(.data[[wins_col]])),
                  losses   = suppressWarnings(as.integer(.data[[loss_col]])),
                  conf     = conf_val) %>%
        mutate(team_raw = str_remove_all(team_raw, "\\*|\\(\\d+\\)"),
               team_raw = str_remove_all(team_raw, "^[0-9]+\\s+"),
               team_raw = str_squish(team_raw)) %>%
        filter(!is.na(wins),
               !is.na(losses),
               str_detect(team_raw, "[A-Za-z]"),
               !str_detect(team_raw,
                           regex("conference|division|team|overall",
                                 ignore_case = TRUE)))

      # Skip other team-level tables that also have W/L columns
      if (nrow(tbl_standings) < 10 || all(is.na(tbl_standings$conf))) next

      parsed <- bind_rows(parsed, tbl_standings)
    }

    if (nrow(parsed) == 0) next

    parsed <- parsed %>%
      distinct(team_raw, wins, losses, .keep_all = TRUE)

    if (nrow(parsed) >= 30) {
      season_standings <- parsed %>%
        slice_head(n = 30) %>%
        mutate(season  = season_end_year,
               win_pct = wins / (wins + losses))
      break
    }
  }

  if (nrow(season_standings) == 0) {
    warning(sprintf("No standings scraped for %d", season_end_year))
  }

  standings_by_season[[as.character(season_end_year)]] <- season_standings
}


# Combine all seasons and add team abbreviations
all_standings <- bind_rows(standings_by_season) %>%
  mutate(abbr = team_name_to_abbr[team_raw]) %>%
  filter(!is.na(abbr)) %>%
  relocate(abbr, .after = team_raw)

cat(sprintf("  Loaded %d team-seasons across %d seasons\n",
            nrow(all_standings), n_distinct(all_standings$season)))





## ═════════════════════════════════════════════════════════════════════════════
## 02 - ASSIGN 3-2-1 TIERS TO EVERY TEAM-SEASON --------------------------------
## ═════════════════════════════════════════════════════════════════════════════

all_standings <- all_standings %>%
  # Overall rank: 1 = best in NBA, 30 = worst in NBA
  group_by(season) %>%
  mutate(overall_rank = rank(-win_pct, ties.method = "first"),
         is_relegation = overall_rank > n() - 3) %>%
  ungroup() %>%
  
  # Conference seed: 1 = best in that conference
  group_by(season, conf) %>%
  mutate(conf_seed = rank(-win_pct, ties.method = "first")) %>%
  ungroup() %>%
  
  mutate(tier = case_when(# 3 worst teams overall, regardless of conference
    is_relegation ~ "relegation",
    
    # Non-relegated teams worse than 10th in their conference
    conf_seed > 10 ~ "nonplayin",
    
    # 9 and 10 seeds in each conference
    conf_seed %in% c(9, 10) ~ "playin_seed",
    
    # 8 seed in each conference
    conf_seed == 8 ~ "playin_loser",
    
    # Top 7 seeds in each conference
    conf_seed <= 7 ~ "playoff",
    
    TRUE ~ NA_character_),
    tier = factor(tier, levels = TIERS)) %>%
  select(-is_relegation)

current_standings <- all_standings %>%
  filter(season == HISTORY_END) %>%
  arrange(desc(win_pct)) %>%
  mutate(overall_rank = row_number())

all_teams <- current_standings$abbr





## ═════════════════════════════════════════════════════════════════════════════
## 03 - DRAFT PRODUCTION CURVE -------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════
# Pick value = xRAPM wins above replacement summed over the four seasons after
#   the draft (season-end years draft+1 .. draft+4), matching the rookie-scale
#   contract window. Seasons a player misses inside the window count as zero.
#
#   war[player, season] = (xRAPM - XRAPM_REPLACEMENT) x possessions / 100
#                         / POINTS_PER_WIN
#
# "war" names below hold these xRAPM wins above replacement.



### 03.01 Player-season xRAPM wins above replacement --------------------------
# xRAPM (xrapm.com, 1996-97 .. 2025-26) is keyed by player name only; the
# possessions cache (hoopR leaguedashplayerstats, Advanced totals) is keyed by
# NBA person id and name. Both are joined to Basketball-Reference player-seasons
# on (season, normalized name). Cached possessions count one team's possessions
# (~96 per 48 minutes), so xRAPM x possessions / 100 is points of net margin.
#
# Seasons with B-Ref minutes but no unambiguous xRAPM match fall back to
# 2.7 x VORP, which uses the same -2.0 replacement level and wins scale.

advanced <- read.csv("01_data/Advanced.csv", stringsAsFactors = FALSE) %>%
  clean_names()

# One B-Ref row per player-season. If a player-season has a multi-team
# aggregate row like 2TM/3TM, keep only that aggregate row.
bref_seasons <- advanced %>%
  filter(lg == "NBA") %>%
  group_by(player_id, season) %>%
  filter(if (any(str_detect(team, "^\\d+TM$"))) {
    str_detect(team, "^\\d+TM$")
    } else {
      TRUE
    }) %>%
  ungroup() %>%
  mutate(name_key = normalize_player_name(player))

xrapm_seasons <- readRDS(XRAPM_CACHE) %>%
  transmute(season   = as.integer(season_end),
            name_key = normalize_player_name(player_raw),
            xrapm    = as.numeric(xrapm)) %>%
  filter(!is.na(xrapm)) %>%
  group_by(season, name_key) %>%
  filter(n() == 1L) %>%                # drop names that collide after normalizing
  ungroup()

possession_seasons <- readRDS(POSSESSIONS_CACHE) %>%
  transmute(season   = as.integer(season_end),
            name_key = normalize_player_name(player_name),
            nba_id   = as.character(player_id),
            poss     = as.numeric(poss)) %>%
  group_by(season, name_key) %>%
  filter(n_distinct(nba_id) == 1L) %>%
  summarise(poss    = sum(poss, na.rm = TRUE),
            .groups = "drop")

XRAPM_SEASONS <- range(xrapm_seasons$season)

# Two different B-Ref players with the same normalized name in one season
ambiguous_bref <- bref_seasons %>%
  distinct(season, name_key, player_id) %>%
  count(season, name_key) %>%
  filter(n > 1L) %>%
  transmute(season, name_key, ambiguous = TRUE)

# Season possessions per minute, used when a matched xRAPM season has no
# possessions row
poss_per_min <- bref_seasons %>%
  inner_join(possession_seasons, by = c("season", "name_key")) %>%
  filter(mp >= 500) %>%
  group_by(season) %>%
  summarise(poss_per_min = median(poss / mp),
            .groups      = "drop")

player_season_value <- bref_seasons %>%
  filter(season >= XRAPM_SEASONS[1], season <= XRAPM_SEASONS[2]) %>%
  left_join(ambiguous_bref, by = c("season", "name_key")) %>%
  left_join(xrapm_seasons, by = c("season", "name_key")) %>%
  left_join(possession_seasons, by = c("season", "name_key")) %>%
  left_join(poss_per_min, by = "season") %>%
  mutate(ambiguous    = coalesce(ambiguous, FALSE),
         xrapm        = if_else(ambiguous, NA_real_, xrapm),
         poss         = if_else(ambiguous, NA_real_, poss),
         poss         = coalesce(poss, mp * poss_per_min),
         season_games = coalesce(unname(SHORTENED_SEASON_GAMES[as.character(season)]), 82),
         season_scale = if (SCALE_TO_82_GAMES) 82 / season_games else 1,
         value_source = case_when(!is.na(xrapm) ~ "xrapm",
                                  !is.na(vorp)  ~ "vorp_fallback",
                                  TRUE          ~ "none"),
         war_raw      = case_when(value_source == "xrapm" ~
                                    (xrapm - XRAPM_REPLACEMENT) * poss / 100 / POINTS_PER_WIN,
                                  value_source == "vorp_fallback" ~ 2.7 * vorp,
                                  TRUE ~ 0),
         war          = war_raw * season_scale) %>%
  select(player_id, player, season, team, age, g, mp, poss, xrapm, vorp,
         value_source, war)

xrapm_match_summary <- player_season_value %>%
  summarise(n_seasons        = n(),
            pct_seasons      = mean(value_source == "xrapm"),
            pct_minutes      = sum(mp[value_source == "xrapm"], na.rm = TRUE) /
                               sum(mp, na.rm = TRUE),
            n_vorp_fallback  = sum(value_source == "vorp_fallback"))

cat(sprintf(paste0("  xRAPM matched %.1f%% of %d player-seasons (%.1f%% of minutes); ",
                   "%d seasons use the 2.7 x VORP fallback\n"),
            100 * xrapm_match_summary$pct_seasons,
            xrapm_match_summary$n_seasons,
            100 * xrapm_match_summary$pct_minutes,
            xrapm_match_summary$n_vorp_fallback))



### 03.02 Draft slots ---------------------------------------------------------
# Cached map of draft slot -> B-Ref player id. Only classes missing from the
# cache are scraped.
slot_cache <- "01_data/draft_slots_cache.rds"
draft_slots <- if (file.exists(slot_cache)) readRDS(slot_cache) else NULL

missing_draft_years <- setdiff(draft_years, unique(draft_slots$draft_year))
if (length(missing_draft_years) > 0) {
  cat(sprintf("  Scraping %d draft class(es) missing from the cache: %s\n",
              length(missing_draft_years),
              paste(missing_draft_years, collapse = ", ")))

  # Basketball-Reference draft page per class: pick number, player name, and
  # B-Ref player_id (taken from the player-name hyperlinks)
  new_slots <- tibble()

  for (scrape_year in missing_draft_years) {
    cat(sprintf("  draft %d\n", scrape_year))
    Sys.sleep(3)
    page <- tryCatch(read_html(sprintf("https://www.basketball-reference.com/draft/NBA_%d.html",
                                       scrape_year)),
                     error = function(e) NULL)
    if (is.null(page)) next

    raw_table <- tryCatch(page %>%
                            html_element("#div_stats") %>%
                            html_table(fill = TRUE),
                          error = function(e) NULL)
    if (is.null(raw_table) || nrow(raw_table) < 2) next

    # The table has a two-row header: group label (e.g. "Totals") on top and
    # column label in row 1. Combine them into one column name.
    header_top <- str_trim(names(raw_table))
    header_sub <- raw_table[1, , drop = TRUE] %>%
      unlist(use.names = FALSE) %>%
      as.character() %>%
      str_trim()
    header_top <- ifelse(is.na(header_top) |
                           header_top == "" |
                           str_detect(header_top, "^\\.\\.\\.") |
                           str_detect(header_top, "^Round"),
                         "", header_top)
    header_names <- ifelse(header_top == "", header_sub,
                           paste(header_top, header_sub, sep = "_"))

    draft_table <- raw_table %>%
      setNames(make.unique(header_names)) %>%
      clean_names() %>%
      slice(-1)

    pick_col   <- names(draft_table)[names(draft_table) %in% c("pk", "pick")][1]
    player_col <- names(draft_table)[names(draft_table) %in% c("player")][1]
    if (is.na(pick_col) || is.na(player_col)) next

    draft_class_slots <- draft_table %>%
      transmute(draft_year = scrape_year,
                pick       = suppressWarnings(as.integer(.data[[pick_col]])),
                player     = .data[[player_col]]) %>%
      filter(!is.na(pick), pick >= 1, pick <= N_DRAFT_SLOTS)

    # B-Ref player_id from the player-name links in the same table
    draft_class_ids <- tryCatch({
      player_links <- page %>%
        html_element("#div_stats") %>%
        html_elements("td[data-stat='player'] a, td[data-stat='player_name'] a")
      tibble(href   = player_links %>% html_attr("href"),
             player = player_links %>%
               html_text() %>%
               str_trim()) %>%
        mutate(player_id = str_match(href, "/players/[a-z]/([a-z0-9]+)\\.html")[, 2]) %>%
        filter(!is.na(player_id)) %>%
        select(player, player_id) %>%
        distinct(player, .keep_all = TRUE)
    }, error = function(e) tibble(player = character(0), player_id = character(0)))

    new_slots <- bind_rows(new_slots,
                           draft_class_slots %>% left_join(draft_class_ids, by = "player"))
  }

  if (nrow(new_slots) > 0) {
    draft_slots <- bind_rows(draft_slots, new_slots) %>%
      distinct(draft_year, pick, .keep_all = TRUE) %>%
      arrange(draft_year, pick)
    saveRDS(draft_slots, slot_cache)
  }
} else {
  cat("  Using cached draft-slot map\n")
}

draft_slots <- draft_slots %>%
  filter(draft_year %in% draft_years, pick >= 1, pick <= N_DRAFT_SLOTS)

missing_classes <- setdiff(draft_years, unique(draft_slots$draft_year))
if (length(missing_classes) > 0) {
  warning("Draft classes unavailable and excluded: ",
          paste(missing_classes, collapse = ", "))
}

# Unlinked rows (no B-Ref id) fall back to a unique normalized-name match, but
# only to a player whose NBA debut falls inside that pick's contract window.
name_to_id <- bref_seasons %>%
  group_by(player_id, name_key) %>%
  summarise(debut_season = min(season),
            .groups      = "drop") %>%
  group_by(name_key) %>%
  filter(n() == 1L) %>%
  ungroup()

draft_slots <- draft_slots %>%
  mutate(name_key = normalize_player_name(player)) %>%
  left_join(name_to_id %>% rename(name_player_id = player_id), by = "name_key") %>%
  mutate(name_ok   = !is.na(debut_season) &
           debut_season >= draft_year + 1L &
           debut_season <= draft_year + VALUE_WINDOW_SEASONS,
         player_id = coalesce(player_id, if_else(name_ok, name_player_id, NA_character_))) %>%
  select(-name_key, -name_player_id, -debut_season, -name_ok)



### 03.03 Contract-window totals per drafted player ---------------------------
# Window = season-end years draft_year + 1 .. draft_year + VALUE_WINDOW_SEASONS.
# A drafted player with no NBA season in the window (injury, stash, never
# signed) contributes zero value. Every draft row since 1996 carries a B-Ref id,
# and Advanced.csv covers every NBA season, so a missing season is a true
# no-play season rather than a failed join.
draft_window <- draft_slots %>%
  left_join(player_season_value %>%
              select(player_id, season, mp, war, value_source),
            by = "player_id",
            relationship = "many-to-many") %>%
  mutate(in_window = !is.na(season) &
           season >= draft_year + 1L &
           season <= draft_year + VALUE_WINDOW_SEASONS) %>%
  group_by(draft_year, pick, player, player_id) %>%
  summarise(war4            = sum(war[in_window], na.rm = TRUE),
            mp_window       = sum(mp[in_window], na.rm = TRUE),
            seasons_played  = sum(in_window & mp > 0, na.rm = TRUE),
            fallback_seasons = sum(in_window & value_source == "vorp_fallback", na.rm = TRUE),
            .groups         = "drop")

draft_4yr <- draft_window %>%
  filter(pick %in% FIRST_ROUND_SLOTS) %>%
  transmute(draft_year = as.integer(draft_year),
            pick       = as.integer(pick),
            player     = as.character(player),
            war4       = as.numeric(war4),
            mp_window  = mp_window)

draft_4yr_r2 <- draft_window %>%
  filter(pick %in% SECOND_ROUND_SLOTS) %>%
  mutate(played = if_else(mp_window > 0, 1L, 0L),
         war4   = if_else(played == 1L, war4, 0)) %>%
  transmute(draft_year = as.integer(draft_year),
            pick       = as.integer(pick),
            player     = as.character(player),
            played     = as.integer(played),
            war4       = as.numeric(war4),
            mp_window  = mp_window)

cat(sprintf("  First round: %d drafted players (%d classes, %d-%d); %d never played in the window\n",
            nrow(draft_4yr), n_distinct(draft_4yr$draft_year),
            min(draft_4yr$draft_year), max(draft_4yr$draft_year),
            sum(draft_4yr$mp_window == 0)))
cat(sprintf("  Second round: %d drafted players, %.1f%% played NBA minutes in the window\n",
            nrow(draft_4yr_r2), 100 * mean(draft_4yr_r2$played == 1)))
cat(sprintf("  Draftee-seasons using the VORP fallback: %d\n",
            sum(draft_window$fallback_seasons)))



### 03.04 Build Per-slot Curve Inputs ------------------------------------------
pick_slot_data <- draft_4yr %>%
  group_by(pick) %>%
  summarise(war_mean = mean(war4),
            war_sd   = sd(war4),
            n_obs    = n(),
            .groups  = "drop") %>%
  arrange(pick) %>%
  mutate(war_sd = coalesce(war_sd, 2.0),
         n_obs  = pmax(n_obs, 1L))

# Keep the raw player-level outcomes for the bootstrap option.
pick_boot_pool <- draft_4yr %>%
  select(pick, war4)

# Empirical second-round table for dashboard overlays and fallback data.
pick_slot_data_r2 <- draft_4yr_r2 %>%
  group_by(pick) %>%
  summarise(war_mean   = mean(war4),
            war_sd     = sd(war4),
            p_play_emp = mean(played == 1),
            n_obs      = n(),
            .groups    = "drop") %>%
  arrange(pick)
