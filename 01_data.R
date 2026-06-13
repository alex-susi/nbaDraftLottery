## ═════════════════════════════════════════════════════════════════════════════
# NBA Draft Lottery Rule Change Impact Model
#
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
#                      "janitor","posterior","expm","loo"))
#   cmdstanr::install_cmdstan()
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

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

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
USE_BAYESIAN_PICK_CURVE <- TRUE   # TRUE = Stan curve; FALSE = bootstrap curve

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

# Function to scrape single season standings
scrape_standings_bbref <- function(season_end_year, delay = 3, verbose = FALSE) {
  urls <- c(sprintf("https://www.basketball-reference.com/leagues/NBA_%d.html",
                    season_end_year),
            sprintf("https://www.basketball-reference.com/leagues/NBA_%d_standings.html",
                    season_end_year))
  
  cat(sprintf("  [bbref] standings %d-%d\n",
              season_end_year - 1, season_end_year))
  
  fetch_bbref_page <- function(url) {
    Sys.sleep(delay + runif(1, 0, 1.5))
    
    resp <- tryCatch(httr::RETRY(verb = "GET",
                                 url = url,
                                 times = 3,
                                 pause_min = 2,
                                 pause_cap = 8,
                                 httr::user_agent(
                                   "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/125 Safari/537.36"),
                                 httr::add_headers(
                                   `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                                   `Accept-Language` = "en-US,en;q=0.9",
                                   `Referer` = "https://www.basketball-reference.com/"),
                                 httr::timeout(30)),
                     error = function(e) {
                       if (verbose) message("    request failed: ", e$message)
                       NULL
                     }
    )
    
    if (is.null(resp)) return(NULL)
    
    status <- httr::status_code(resp)
    if (verbose) message("    ", url, " | status = ", status)
    if (status >= 400) return(NULL)
    
    html_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    
    page <- tryCatch(xml2::read_html(html_txt, options = "HUGE"),
                     error = function(e) {
                       if (verbose) message("    read_html failed: ", e$message)
                       NULL
                     }
    )
    page
  }
  
  extract_all_tables <- function(page) {
    visible_tables <- tryCatch(page %>%
                                 rvest::html_elements("table") %>%
                                 rvest::html_table(fill = TRUE),
                               error = function(e) list())
    
    comment_txt <- tryCatch(page %>%
                              rvest::html_elements(xpath = "//comment()") %>%
                              rvest::html_text(),
                            error = function(e) character(0))
    
    comment_tables <- purrr::map(comment_txt, function(txt) {
      if (!str_detect(txt, "<table")) return(list())
      
      tryCatch({
        xml2::read_html(paste0("<html><body>", txt, "</body></html>"), 
                        options = "HUGE") %>%
          rvest::html_elements("table") %>%
          rvest::html_table(fill = TRUE)
      }, error = function(e) list())
    }) %>%
      purrr::flatten()
    
    c(visible_tables, comment_tables)
  }
  
  parse_possible_standings_table <- function(tbl) {
    tbl <- suppressMessages(tbl %>%
                              as_tibble(.name_repair = "unique") %>%
                              janitor::clean_names())
    
    if (nrow(tbl) == 0 || ncol(tbl) < 3) return(tibble())
    
    nm <- names(tbl)
    
    wins_col <- nm[nm %in% c("w", "wins")][1]
    loss_col <- nm[nm %in% c("l", "losses")][1]
    
    if (is.na(wins_col) || is.na(loss_col)) return(tibble())
    
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
    
    conf_val <- dplyr::case_when(str_detect(team_col, regex("eastern", ignore_case = TRUE)) ~ "East",
                                 str_detect(team_col, regex("western", ignore_case = TRUE)) ~ "West",
                                 TRUE ~ NA_character_)
    
    out <- tbl %>%
      dplyr::transmute(team_raw = as.character(.data[[team_col]]),
                       wins     = suppressWarnings(as.integer(.data[[wins_col]])),
                       losses   = suppressWarnings(as.integer(.data[[loss_col]])),
                       conf     = conf_val) %>%
      dplyr::mutate(team_raw = str_remove_all(team_raw, "\\*|\\(\\d+\\)"),
                    team_raw = str_remove_all(team_raw, "^[0-9]+\\s+"),
                    team_raw = str_squish(team_raw)) %>%
      dplyr::filter(!is.na(wins),
                    !is.na(losses),
                    str_detect(team_raw, "[A-Za-z]"),
                    !str_detect(team_raw,
                                regex("conference|division|team|overall", 
                                      ignore_case = TRUE)))
    
    # Keep only actual conference standings tables.
    # This avoids accidentally pulling other team-level tables with W/L columns.
    if (nrow(out) < 10 || all(is.na(out$conf))) {
      return(tibble())
    }
    
    out
  }
  
  for (url in urls) {
    page <- fetch_bbref_page(url)
    if (is.null(page)) next
    
    tables <- extract_all_tables(page)
    
    if (verbose) {
      message("    tables found = ", length(tables))
    }
    
    parsed <- tables %>%
      purrr::map(parse_possible_standings_table) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct(team_raw, wins, losses, .keep_all = TRUE)
    
    if (verbose) {
      message("    parsed rows = ", nrow(parsed))
      if (nrow(parsed) > 0) print(parsed)
    }
    
    if (nrow(parsed) >= 30) {
      return(parsed %>%
               dplyr::slice_head(n = 30) %>%
               dplyr::mutate(season  = season_end_year,
                             win_pct = wins / (wins + losses)))
    }
  }
  
  warning(sprintf("No standings scraped for %d", season_end_year))
  tibble()
}


# Scrapes all years in scope
all_standings <- suppressMessages(map_dfr(HISTORY_START:HISTORY_END,
                                          scrape_standings_bbref)) %>%
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
# Pick value = SUM of Win Shares over a player's FIRST 4 SEASONS, matching the
# rookie-scale contract window. The value comes from Basketball-Reference's
# season-level "Advanced" export (Advanced.csv); we collapse each player to
# their first-4-year WS total. The draft SLOT -> player_id map is scraped from
# bbref draft pages and joined to the CSV by player_id. If the CSV or the join
# is unavailable we fall back to a compiled first-4-year-WS curve.

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
      filter(!is.na(pick), pick >= 1, pick <= N_DRAFT_SLOTS)
  }, error = function(e) NULL)
}

# We only need the draft SLOT -> player_id map from bbref (the value itself now
# comes from Advanced.csv / ws_first_4). We extract the bbref player_id from
# each player's hyperlink (e.g. /players/j/jamesle01.html -> "jamesle01") so we
# can join cleanly to the Advanced.csv player_id column by ID rather than name.
scrape_draft_slots_bbref <- function(draft_year, delay = 3) {
  url <- sprintf("https://www.basketball-reference.com/draft/NBA_%d.html",
                 draft_year)
  cat(sprintf("  draft %d\n", draft_year))
  Sys.sleep(delay)
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  # Pull the stats table for pick numbers + player names.
  raw <- tryCatch(page %>% 
                    html_element("#div_stats") %>% 
                    html_table(fill = TRUE),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) < 2) return(NULL)
  
  top <- str_trim(names(raw))
  sub <- raw[1, , drop = TRUE] %>% 
    unlist(use.names = FALSE) %>% 
    as.character() %>% 
    str_trim()
  top_clean <- ifelse(is.na(top) | 
                        top == "" | 
                        str_detect(top, "^\\.\\.\\.") | 
                        str_detect(top, "^Round"),
                      "", top)
  new_names <- ifelse(top_clean == "", sub, paste(top_clean, sub, sep = "_"))
  tbl <- raw[-1, , drop = FALSE]
  names(tbl) <- make.unique(new_names)
  tbl <- clean_names(tbl)
  
  pick_col   <- names(tbl)[names(tbl) %in% c("pk", "pick")][1]
  player_col <- names(tbl)[names(tbl) %in% c("player")][1]
  if (is.na(pick_col) || is.na(player_col)) return(NULL)
  
  base <- tbl %>%
    transmute(draft_year = draft_year,
              pick       = suppressWarnings(as.integer(.data[[pick_col]])),
              player     = .data[[player_col]]) %>%
    filter(!is.na(pick), pick >= 1, pick <= N_DRAFT_SLOTS)
  
  # Extract player_id from the player-column hyperlinks in the same table.
  ids <- tryCatch({
    nodes <- page %>%
      html_element("#div_stats") %>%
      html_elements("td[data-stat='player'] a, td[data-stat='player_name'] a")
    tibble(href   = nodes %>% html_attr("href"),
           player = nodes %>% 
             html_text() %>% 
             str_trim()) %>%
      mutate(player_id = str_match(href, "/players/[a-z]/([a-z0-9]+)\\.html")[, 2]) %>%
      filter(!is.na(player_id)) %>%
      select(player, player_id) %>%
      distinct(player, .keep_all = TRUE)
  }, error = function(e) tibble(player = character(0), player_id = character(0)))
  
  base %>% left_join(ids, by = "player")
}

cat("\n--- Building Draft Production Curve (first-4-year Win Shares) ---\n")

# We use drafts 1985-2021 so every player has had >= 4 seasons to accumulate.
draft_years <- 1985:2021

# STEP A: load season-level Advanced stats and compute first-4-year WIN SHARES
# Advanced.csv is the Basketball-Reference "Advanced" export (one row per
# player-season). We collapse each player to the SUM of Win Shares over their
# first four NBA seasons (rookie-scale window). Multi-team seasons (2TM/3TM
# aggregate rows) are de-duplicated so a split season is counted once.

adv_path <- "01_data/Advanced.csv"

first_4_ws <- NULL
if (!is.na(adv_path)) {
  cat(sprintf("  Loading Advanced stats from %s\n", adv_path))
  advanced <- read.csv(adv_path, stringsAsFactors = FALSE) %>%
    clean_names()
  
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
      player           = first(player),
      first_season     = min(season, na.rm = TRUE),
      seasons_observed = n_distinct(season),
      ws_first_4       = sum(ws, na.rm = TRUE),
      ows_first_4      = sum(ows, na.rm = TRUE),
      dws_first_4      = sum(dws, na.rm = TRUE),
      vorp_first_4     = sum(vorp, na.rm = TRUE),
      bpm_first_4      = weighted.mean(bpm, mp, na.rm = TRUE),
      ws48_first_4     = weighted.mean(ws_48, mp, na.rm = TRUE),
      mp_first_4       = sum(mp, na.rm = TRUE),
      g_first_4        = sum(g, na.rm = TRUE),
      .groups          = "drop"
    )
  
  cat(sprintf("  Computed first-4-year WS for %d players\n", nrow(first_4_ws)))
} else {
  cat("  Advanced.csv not found in 01_data/ — will fall back to compiled curve\n")
}


# STEP B: map each drafted player (slot) to their player_id via bbref, cache it
slot_cache <- "01_data/draft_slots_cache.rds"
if (file.exists(slot_cache)) {
  cat("  Using cached draft-slot map\n")
  draft_slots <- readRDS(slot_cache)
} else {
  draft_slots <- map_dfr(draft_years, scrape_draft_slots_bbref)
  if (!is.null(draft_slots) && nrow(draft_slots) > 0) {
    saveRDS(draft_slots, slot_cache)
  }
}


# STEP C: join slot -> player_id -> ws_first_4 to get value-by-slot draws
# Round 1 keeps the original player-level production sample. Round 2 starts
# from the full drafted-player table and treats missing NBA production rows as
# structural zeroes for the hurdle model.
draft_4yr <- NULL
draft_4yr_r2 <- NULL

if (!is.null(first_4_ws) && !is.null(draft_slots) && nrow(draft_slots) > 0) {
  
  first_4_cols <- first_4_ws %>%
    select(player_id, player, ws_first_4, mp_first_4, g_first_4)
  
  joined_by_id <- draft_slots %>%
    filter(!is.na(player_id), pick >= 1, pick <= N_DRAFT_SLOTS) %>%
    left_join(first_4_cols %>% select(-player), by = "player_id")
  
  missing_id <- draft_slots %>%
    filter((is.na(player_id) | !player_id %in% joined_by_id$player_id),
           pick >= 1, pick <= N_DRAFT_SLOTS)
  
  joined_by_name <- if (nrow(missing_id) > 0) {
    missing_id %>%
      left_join(first_4_cols %>% select(-player_id), by = "player")
  } else {
    tibble()
  }
  
  draft_joined <- bind_rows(joined_by_id, joined_by_name) %>%
    distinct(draft_year, pick, player, .keep_all = TRUE)
  
  draft_4yr <- draft_joined %>%
    filter(pick %in% FIRST_ROUND_SLOTS, !is.na(ws_first_4)) %>%
    transmute(draft_year, pick, player, ws4 = ws_first_4)
  
  draft_4yr_r2 <- draft_joined %>%
    filter(pick %in% SECOND_ROUND_SLOTS) %>%
    mutate(played_nba_first4 = if_else(!is.na(mp_first_4) & mp_first_4 > 0, 1L, 0L),
           ws4 = if_else(played_nba_first4 == 1L, coalesce(ws_first_4, 0), 0)) %>%
    transmute(draft_year = as.integer(draft_year),
              pick       = as.integer(pick),
              player     = as.character(player),
              played     = as.integer(played_nba_first4),
              ws4        = as.numeric(ws4),
              mp_first_4 = coalesce(mp_first_4, 0),
              g_first_4  = coalesce(g_first_4, 0))
  
  cat(sprintf("  Joined %d first-round drafted players to first-4-year WS\n",
              nrow(draft_4yr)))
  cat(sprintf("  Built second-round hurdle sample: %d drafted players, %.1f%% played NBA minutes\n",
              nrow(draft_4yr_r2), 100 * mean(draft_4yr_r2$played == 1)))
}


# STEP D: build the per-slot curve inputs (round 1) and bootstrap pool
pick_slot_data <- draft_4yr %>%
  group_by(pick) %>%
  summarise(war_mean = mean(ws4),
            war_sd   = sd(ws4),
            n_obs    = n(),
            .groups  = "drop") %>%
  arrange(pick) %>%
  mutate(war_sd = coalesce(war_sd, 4.0),
         n_obs  = pmax(n_obs, 1L))
# Keep the raw player-level draws for the bootstrap option.
pick_boot_pool <- draft_4yr %>%
  select(pick, ws4)
cat(sprintf("  Built first-round slot curve from %d drafted players (first-4-yr WS)\n",
            nrow(draft_4yr)))



# Empirical second-round table for dashboard overlays and fallback data.
pick_slot_data_r2 <- draft_4yr_r2 %>%
  group_by(pick) %>%
  summarise(war_mean = mean(ws4),
            war_sd   = sd(ws4),
            p_play_emp = mean(played == 1),
            n_obs    = n(),
            .groups  = "drop") %>%
  arrange(pick)

