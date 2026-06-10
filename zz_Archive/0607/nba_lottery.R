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
## 01 - Data Scraping ----------------------------------------------------------
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
    visible_tables <- tryCatch(
      page %>%
        rvest::html_elements("table") %>%
        rvest::html_table(fill = TRUE),
      error = function(e) list()
    )
    
    comment_txt <- tryCatch(
      page %>%
        rvest::html_elements(xpath = "//comment()") %>%
        rvest::html_text(),
      error = function(e) character(0)
    )
    
    comment_tables <- purrr::map(comment_txt, function(txt) {
      if (!stringr::str_detect(txt, "<table")) return(list())
      
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
    tbl <- suppressMessages(
      tbl %>%
        tibble::as_tibble(.name_repair = "unique") %>%
        janitor::clean_names()
    )
    
    if (nrow(tbl) == 0 || ncol(tbl) < 3) return(tibble::tibble())
    
    nm <- names(tbl)
    
    wins_col <- nm[nm %in% c("w", "wins")][1]
    loss_col <- nm[nm %in% c("l", "losses")][1]
    
    if (is.na(wins_col) || is.na(loss_col)) return(tibble::tibble())
    
    team_col <- setdiff(nm, c(
      wins_col, loss_col, "w_l_percent", "gb", "ps_g", "pa_g", "srs",
      "pw", "pl", "mov", "sos", "or_tg", "dr_tg", "nr_tg", "pace", "f_tr",
      "x3p_ar", "ts_percent", "e_fg_percent", "tov_percent", "orb_percent",
      "ft_fga", "opp_e_fg_percent", "opp_tov_percent", "opp_drb_percent",
      "opp_ft_fga", "arena", "attend", "attend_g"
    ))[1]
    
    if (is.na(team_col)) team_col <- nm[1]
    
    conf_val <- dplyr::case_when(
      stringr::str_detect(team_col, stringr::regex("eastern", ignore_case = TRUE)) ~ "East",
      stringr::str_detect(team_col, stringr::regex("western", ignore_case = TRUE)) ~ "West",
      TRUE ~ NA_character_
    )
    
    out <- tbl %>%
      dplyr::transmute(
        team_raw = as.character(.data[[team_col]]),
        wins     = suppressWarnings(as.integer(.data[[wins_col]])),
        losses   = suppressWarnings(as.integer(.data[[loss_col]])),
        conf     = conf_val
      ) %>%
      dplyr::mutate(
        team_raw = stringr::str_remove_all(team_raw, "\\*|\\(\\d+\\)"),
        team_raw = stringr::str_remove_all(team_raw, "^[0-9]+\\s+"),
        team_raw = stringr::str_squish(team_raw)
      ) %>%
      dplyr::filter(
        !is.na(wins),
        !is.na(losses),
        stringr::str_detect(team_raw, "[A-Za-z]"),
        !stringr::str_detect(
          team_raw,
          stringr::regex("conference|division|team|overall", ignore_case = TRUE)
        )
      )
    
    # Keep only actual conference standings tables.
    # This avoids accidentally pulling other team-level tables with W/L columns.
    if (nrow(out) < 10 || all(is.na(out$conf))) {
      return(tibble::tibble())
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
      return(
        parsed %>%
          dplyr::slice_head(n = 30) %>%
          dplyr::mutate(
            season  = season_end_year,
            win_pct = wins / (wins + losses)
          )
      )
    }
  }
  
  warning(sprintf("No standings scraped for %d", season_end_year))
  tibble::tibble()
}


# Scrapes all years in scope
all_standings <- suppressMessages(map_dfr(HISTORY_START:HISTORY_END,
                                          scrape_standings_bbref)) %>%
  mutate(abbr = team_name_to_abbr[team_raw]) %>%
  filter(!is.na(abbr)) %>% 
  relocate(abbr, .after = team_raw)

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
# SECTION 4: DRAFT PRODUCTION CURVE — FIRST-4-YEAR WIN SHARES (ws_first_4)
# ============================================================================
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
      filter(!is.na(pick), pick >= 1, pick <= 30)
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
  raw <- tryCatch(
    page %>% html_element("#div_stats") %>% html_table(fill = TRUE),
    error = function(e) NULL
  )
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
    filter(!is.na(pick), pick >= 1, pick <= 30)

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

# We use drafts 1995-2021 so every player has had >= 4 seasons to accumulate.
draft_years <- 1985:2021

# ---------------------------------------------------------------------------
# STEP A: load season-level Advanced stats and compute first-4-year WIN SHARES
# ---------------------------------------------------------------------------
# Advanced.csv is the Basketball-Reference "Advanced" export (one row per
# player-season). We collapse each player to the SUM of Win Shares over their
# first four NBA seasons (rookie-scale window). Multi-team seasons (2TM/3TM
# aggregate rows) are de-duplicated so a split season is counted once.
adv_path_candidates <- c(
  "01_data/Advanced.csv",
  "Advanced.csv",
  "data/Advanced.csv"
)
adv_path <- adv_path_candidates[file.exists(adv_path_candidates)][1]

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

# ---------------------------------------------------------------------------
# STEP B: map each drafted player (slot) to their player_id via bbref, cache it
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# STEP C: join slot -> player_id -> ws_first_4 to get value-by-slot draws
# ---------------------------------------------------------------------------
# Primary join key is player_id (robust to name spellings); we fall back to a
# name join for any rows where the bbref href id was unavailable.
draft_4yr <- NULL
if (!is.null(first_4_ws) && !is.null(draft_slots) && nrow(draft_slots) > 0) {

  by_id <- draft_slots %>%
    filter(!is.na(player_id)) %>%
    inner_join(first_4_ws %>% select(player_id, ws_first_4), by = "player_id")

  # name-based recovery for slots whose player_id failed to parse
  missing_id <- draft_slots %>% filter(is.na(player_id))
  by_name <- if (nrow(missing_id) > 0) {
    missing_id %>%
      inner_join(first_4_ws %>% select(player, ws_first_4), by = "player")
  } else {
    tibble()
  }

  draft_4yr <- bind_rows(
    by_id   %>% transmute(draft_year, pick, player, ws4 = ws_first_4)#,
    #by_name %>% transmute(draft_year, pick, player, ws4 = ws_first_4)
  ) %>%
    filter(!is.na(ws4), !is.na(pick), pick >= 1, pick <= 30)

  cat(sprintf("  Joined %d drafted players to first-4-year WS\n",
              nrow(draft_4yr)))
}

# ---------------------------------------------------------------------------
# STEP D: build the per-slot curve inputs (pick_slot_data) and bootstrap pool
# ---------------------------------------------------------------------------
if (!is.null(draft_4yr) && nrow(draft_4yr) > 50) {
  pick_slot_data <- draft_4yr %>%
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
    select(pick, ws4)
  cat(sprintf("  Built slot curve from %d drafted players (first-4-yr WS)\n",
              nrow(draft_4yr)))
} else {
  cat("  Insufficient joined data — using compiled first-4-year WS estimates\n")
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
    transmute(pick, ws4 = draws) %>%
    ungroup()
}































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
  if (protection == "top2")   return(pos > 2)
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
    top1 = 1, top2 = 2, top3 = 3, top4 = 4, top5 = 5, top6 = 6,
    top8 = 8, top10 = 10, lottery = 14, top16 = 16, top20 = 20, 0)
}

traded_future <- tribble(
  ~owner, ~original_team, ~year, ~protection, ~pick_type, ~notes,

  # ---- 2027: flat / directly representable obligations ----
  "HOU", "BKN", 2027, "none",    "swap",     "HOU may swap with BKN",
  "BKN", "NYK", 2027, "none",    "outright", "NYK to BKN",
  "HOU", "PHX", 2027, "none",    "outright", "PHX to HOU via BKN",
  "MEM", "LAL", 2027, "top4",    "outright", "LAL 5-30 to MEM",
  "CHA", "DAL", 2027, "top2",    "outright", "DAL 3-30 to CHA",
  "CHA", "MIA", 2027, "lottery", "outright", "MIA 15-30 to CHA",

  # ---- 2028 ----
  "PHI", "LAC", 2028, "none",    "outright", "LAC to PHI",
  "POR", "ORL", 2028, "none",    "outright", "ORL to POR via MEM",
  "OKC", "DEN", 2028, "top5",    "outright", "DEN 6-30 to OKC if not already settled",
  "OKC", "DAL", 2028, "none",    "swap",     "OKC may swap with DAL",

  # ---- 2029 ----
  "BKN", "NYK", 2029, "none",    "outright", "NYK to BKN",
  "LAC", "IND", 2029, "none",    "outright", "IND to LAC",
  "DAL", "LAL", 2029, "none",    "outright", "LAL to DAL",
  "OKC", "DEN", 2029, "top5",    "outright", "DEN 6-30 to OKC if not already settled",

  # ---- 2030 ----
  "MEM", "ORL", 2030, "none",    "outright", "ORL to MEM",
  "DAL", "GSW", 2030, "top20",   "outright", "GSW 21-30 to DAL",
  "OKC", "DEN", 2030, "top5",    "outright", "DEN 6-30 to OKC if prior conditions satisfied",

  # ---- 2031 ----
  "BKN", "NYK", 2031, "none",    "outright", "NYK to BKN",
  "MEM", "PHX", 2031, "none",    "outright", "PHX to MEM via UTA",
  "SAC", "MIN", 2031, "none",    "outright", "MIN to SAC via SAN",
  "SAS", "SAC", 2031, "none",    "swap",     "SAS may swap with SAC",

  # ---- 2032 ----
  "BKN", "DEN", 2032, "none",    "outright", "DEN to BKN"
) %>%
  mutate(complex_group = NA_character_)

# Complex first-round obligations that require simulation-time ranked-pool
# resolution. Each row in complex_future_assets is a possible owner/original-team
# outcome. Own-pick rows are generated separately, so owner == original_team rows
# are intentionally omitted here.
complex_future_groups <- tribble(
  ~year, ~group_id, ~notes,
  2027, "MIL_NOP_ATL",              "More favorable of MIL/NOP to NOP; other to ATL if 5-30; both to NOP if both 1-4",
  2027, "CLE_MIN_UTA_MEM_UTA_PHX",  "Most favorable CLE/MIN/UTA to MEM; second to UTA; least to PHX",
  2027, "SAS_SAC_OKC",              "SAN 1-16 to SAC; SAN 17-30 to OKC",
  2027, "OKC_DEN_LAC",              "Two most/more favorable of OKC, DEN 6-30, LAC to OKC; other to LAC",
  2028, "ATL_CLE_UTA",              "ATL/CLE/UTA ranked swap pool",
  2028, "BOS_SAS",                  "SAS may swap for BOS if BOS 2-30",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL",  "Nested BKN/PHI/PHX/NYK/WAS/MIL ranked swap pool; implemented as a ranked-pool approximation",
  2029, "DAL_HOU_PHX_BKN",          "Two most favorable of DAL/HOU/PHX to HOU; other to BKN",
  2029, "BOS_MIL_POR_WAS",          "Most and least favorable of BOS/MIL/POR to POR; second to WAS",
  2029, "CLE_MIN_UTA_CHA",          "Most/two most favorable of CLE, MIN 6-30, UTA to UTA; other to CHA",
  2029, "MEM_ORL",                  "MEM may swap for ORL 3-30; ORL keeps 1-2",
  2029, "LAC_PHI",                  "PHI may swap for LAC 4-30; LAC keeps 1-3",
  2030, "WAS_PHX_MEM",              "More favorable WAS/PHX to WAS; MEM gets more favorable of MEM and less favorable WAS/PHX; least to PHX",
  2030, "DAL_SAS_MIN",              "SAS/DAL/MIN ranked swap pool; MIN keeps #1",
  2030, "MIL_POR",                  "POR may swap with MIL"
)

make_complex_assets <- function(year, group_id, original_teams, possible_owners, notes) {
  tidyr::expand_grid(
    owner = possible_owners,
    original_team = original_teams
  ) %>%
    filter(owner != original_team) %>%
    transmute(
      owner,
      original_team,
      year = as.integer(year),
      protection = "complex",
      pick_type = "complex",
      notes = notes,
      complex_group = group_id
    )
}

complex_future_assets <- bind_rows(
  make_complex_assets(2027, "MIL_NOP_ATL", c("MIL", "NOP"), c("NOP", "ATL"),
                      "MIL/NOP ranked pool: best to NOP; other to ATL unless both top-4"),
  make_complex_assets(2027, "CLE_MIN_UTA_MEM_UTA_PHX", c("CLE", "MIN", "UTA"), c("MEM", "UTA", "PHX"),
                      "CLE/MIN/UTA ranked pool: best MEM, second UTA, least PHX"),
  make_complex_assets(2027, "SAS_SAC_OKC", c("SAS"), c("SAC", "OKC"),
                      "SAN 1-16 to SAC; 17-30 to OKC"),
  make_complex_assets(2027, "OKC_DEN_LAC", c("OKC", "DEN", "LAC"), c("OKC", "LAC"),
                      "OKC/DEN/LAC ranked pool with DEN top-5 protection"),

  make_complex_assets(2028, "ATL_CLE_UTA", c("ATL", "CLE", "UTA"), c("ATL", "CLE", "UTA"),
                      "ATL/CLE/UTA ranked swap pool"),
  make_complex_assets(2028, "BOS_SAS", c("BOS", "SAS"), c("BOS", "SAS"),
                      "SAS may swap for BOS if BOS 2-30"),
  make_complex_assets(2028, "BKN_PHI_PHX_NYK_WAS_MIL", c("BKN", "PHI", "PHX", "NYK", "WAS", "MIL", "POR"),
                      c("BKN", "NYK", "WAS", "PHX", "MIL"),
                      "Nested BKN/PHI/PHX/NYK/WAS/MIL/POR ranked-pool approximation"),

  make_complex_assets(2029, "DAL_HOU_PHX_BKN", c("DAL", "HOU", "PHX"), c("HOU", "BKN"),
                      "DAL/HOU/PHX pool: two best HOU, other BKN"),
  make_complex_assets(2029, "BOS_MIL_POR_WAS", c("BOS", "MIL", "POR"), c("POR", "WAS"),
                      "BOS/MIL/POR pool: best and worst POR, middle WAS"),
  make_complex_assets(2029, "CLE_MIN_UTA_CHA", c("CLE", "MIN", "UTA"), c("UTA", "CHA"),
                      "CLE/MIN/UTA pool with MIN top-5 protection"),
  make_complex_assets(2029, "MEM_ORL", c("MEM", "ORL"), c("MEM", "ORL"),
                      "MEM may swap for ORL 3-30"),
  make_complex_assets(2029, "LAC_PHI", c("LAC", "PHI"), c("LAC", "PHI"),
                      "PHI may swap for LAC 4-30"),

  make_complex_assets(2030, "WAS_PHX_MEM", c("WAS", "PHX", "MEM"), c("WAS", "MEM", "PHX"),
                      "WAS/PHX/MEM ranked pool"),
  make_complex_assets(2030, "DAL_SAS_MIN", c("DAL", "SAS", "MIN"), c("DAL", "SAS", "MIN"),
                      "DAL/SAS/MIN ranked pool with MIN #1 protection"),
  make_complex_assets(2030, "MIL_POR", c("MIL", "POR"), c("MIL", "POR"),
                      "POR may swap with MIL")
)

# Add reciprocal contingent rows for simple two-team swaps, so the team losing
# the better pick can receive the swap-holder's original pick in simulations.
swap_return_assets <- traded_future %>%
  filter(pick_type == "swap") %>%
  transmute(
    owner = original_team,
    original_team = owner,
    year,
    protection = "none",
    pick_type = "swap_return",
    notes = sprintf("Return pick if %s exercises swap with %s", .data$original_team, .data$owner),
    complex_group = NA_character_
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

cat(sprintf("\nFlat future obligations encoded: %d rows (2027-2032)\n",
            nrow(traded_future)))
cat(sprintf("Complex future obligation assets encoded: %d rows across %d groups\n",
            nrow(complex_future_assets), n_distinct(complex_future_assets$complex_group)))


# Build the full set of possible pick assets for 2027-2032. We keep every
# team's own/retained pick row even when the pick is protected or in a swap pool;
# the simulation allocator decides which owner actually receives each original
# pick in each draw. This fixes the previous protected-pick retention bug.
build_owned_picks <- function() {
  own_future <- tidyr::expand_grid(
    year = FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT,
    original_team = all_teams
  ) %>%
    transmute(
      owner = original_team,
      original_team,
      year = as.integer(year),
      protection = "none",
      pick_type = "own",
      notes = "own / retained pick",
      complex_group = NA_character_
    )

  bind_rows(
    own_future,
    traded_future,
    swap_return_assets,
    complex_future_assets
  ) %>%
    distinct(owner, original_team, year, pick_type, complex_group, .keep_all = TRUE)
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
            fixed_slot = NA_integer_, complex_group = complex_group %||% NA_character_)

pick_assets <- bind_rows(assets_2026, assets_future) %>%
  arrange(year, owner, original_team) %>%
  mutate(
    complex_group = coalesce(complex_group, NA_character_),
    asset_id = ifelse(
      is.na(complex_group),
      sprintf("%d_%s_from_%s_%s", year, owner, original_team, pick_type),
      sprintf("%d_%s_from_%s_%s_%s", year, owner, original_team, pick_type, complex_group)
    ),
    is_traded = owner != original_team,
    label = case_when(
      pick_type == "own"         ~ sprintf("%d %s own / retained pick", year, owner),
      pick_type == "complex"     ~ sprintf("%d %s pick (contingent to %s, %s)", year, original_team, owner, complex_group),
      pick_type == "swap"        ~ sprintf("%d %s pick (swap right held by %s)", year, original_team, owner),
      pick_type == "swap_return" ~ sprintf("%d %s pick (return leg to %s)", year, original_team, owner),
      is_traded                  ~ sprintf("%d %s pick (via %s)", year, original_team, owner),
      TRUE                       ~ sprintf("%d %s own pick", year, owner)
    ),
    # human-readable obligation note
    obligation = case_when(
      pick_type == "own"         ~ "Own pick, or retained if outgoing protection/swap does not convey",
      pick_type == "complex"     ~ sprintf("Complex ranked-pool obligation: %s", notes),
      pick_type == "swap"        ~ sprintf("Swap right held by %s", owner),
      pick_type == "swap_return" ~ "Return leg if another team exercises a swap",
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


# ----------------------------------------------------------------------------
# USER-FACING PICK ENTITLEMENTS (DISPLAY ASSETS)
# ----------------------------------------------------------------------------
# The simulation keeps one internal row per possible owner/original-team outcome
# so allocations can be resolved cleanly. That is too granular for the app:
# swap-return legs and retained own-pick rows are mutually exclusive pieces of
# the SAME user-facing entitlement. For example, BKN's 2027 first should appear
# once as "BKN own or HOU (via HOU swap for BKN)", not as separate retained and
# return-leg rows. The display registry below groups internal assets into the
# RealGM-style pick entitlements users expect to select.

pick_display_assets <- tibble()
pick_display_members <- tibble()

add_display_group <- function(display_asset_id,
                              year,
                              owner,
                              original_teams,
                              label,
                              obligation,
                              notes = obligation,
                              group_type = "grouped",
                              complex_group_filter = NULL,
                              pick_types = c("own", "swap", "swap_return", "complex")) {
  members <- pick_assets %>%
    filter(
      .data$year == .env$year,
      .data$owner == .env$owner,
      .data$original_team %in% .env$original_teams,
      .data$pick_type %in% .env$pick_types
    )

  if (!is.null(complex_group_filter)) {
    members <- members %>%
      filter(
        .data$complex_group == .env$complex_group_filter |
          (.data$pick_type == "own" & .data$original_team %in% .env$original_teams)
      )
  }

  members <- members %>% distinct(asset_id, .keep_all = TRUE)
  if (nrow(members) == 0) return(invisible(NULL))

  pick_display_assets <<- bind_rows(
    pick_display_assets,
    tibble(
      display_asset_id = display_asset_id,
      owner = owner,
      year = as.integer(year),
      label = label,
      obligation = obligation,
      notes = notes,
      group_type = group_type,
      display_group = complex_group_filter %||% display_asset_id,
      member_n = nrow(members),
      member_original_teams = paste(sort(unique(members$original_team)), collapse = ", ")
    )
  )

  pick_display_members <<- bind_rows(
    pick_display_members,
    tibble(
      display_asset_id = display_asset_id,
      asset_id = members$asset_id
    )
  )

  invisible(NULL)
}

# Simple two-team swaps: collapse the holder leg, own row, and return leg into
# one selectable entitlement per team.
simple_swaps_for_display <- traded_future %>% filter(pick_type == "swap")
if (nrow(simple_swaps_for_display) > 0) {
  for (i in seq_len(nrow(simple_swaps_for_display))) {
    sw <- simple_swaps_for_display[i, ]
    yr <- sw$year
    holder <- sw$owner
    counter <- sw$original_team
    pool <- c(holder, counter)
    suffix <- sprintf("via %s swap for %s", holder, counter)

    add_display_group(
      display_asset_id = sprintf("display_%d_%s_swap_%s_%s", yr, holder, holder, counter),
      year = yr,
      owner = holder,
      original_teams = pool,
      label = sprintf("%d %s own or %s (%s)", yr, holder, counter, suffix),
      obligation = sprintf("%s receives the more favorable of %s and %s; %s receives the other.", holder, holder, counter, counter),
      notes = sw$notes,
      group_type = "simple_swap",
      pick_types = c("own", "swap")
    )

    add_display_group(
      display_asset_id = sprintf("display_%d_%s_swap_return_%s_%s", yr, counter, holder, counter),
      year = yr,
      owner = counter,
      original_teams = pool,
      label = sprintf("%d %s own or %s (%s)", yr, counter, holder, suffix),
      obligation = sprintf("%s receives the less favorable of %s and %s after %s's swap right.", counter, holder, counter, holder),
      notes = sw$notes,
      group_type = "simple_swap_return",
      pick_types = c("own", "swap_return")
    )
  }
}

# Complex ranked-pool obligations: one RealGM-style selectable entitlement per
# owner, even when the owner could receive different original teams' picks in
# different simulations. The internal member rows still preserve exact simulated
# allocation; the app sees the summed entitlement.
complex_display_specs <- tribble(
  ~year, ~complex_group, ~owner, ~original_teams, ~label, ~obligation,

  2027, "MIL_NOP_ATL", "NOP", "MIL,NOP",
  "2027 MIL or NOP (more favorable to NOP; other if both 1-4)",
  "More favorable of MIL and NOP to NOP; if both are 1-4, NOP also retains/receives the other.",
  2027, "MIL_NOP_ATL", "ATL", "MIL,NOP",
  "2027 MIL or NOP (less favorable to ATL if 5-30)",
  "Less favorable of MIL and NOP to ATL if that pick is 5-30.",

  2027, "CLE_MIN_UTA_MEM_UTA_PHX", "MEM", "CLE,MIN,UTA",
  "2027 CLE, MIN or UTA (most favorable to MEM)",
  "Most favorable of CLE, MIN and UTA to MEM.",
  2027, "CLE_MIN_UTA_MEM_UTA_PHX", "UTA", "CLE,MIN,UTA",
  "2027 CLE, MIN or UTA (second most favorable to UTA)",
  "Second most favorable of CLE, MIN and UTA to UTA.",
  2027, "CLE_MIN_UTA_MEM_UTA_PHX", "PHX", "CLE,MIN,UTA",
  "2027 CLE, MIN or UTA (least favorable to PHX)",
  "Least favorable of CLE, MIN and UTA to PHX.",

  2027, "SAS_SAC_OKC", "SAC", "SAS",
  "2027 SAS 1-16 to SAC",
  "SAS first-round pick to SAC if 1-16.",
  2027, "SAS_SAC_OKC", "OKC", "SAS",
  "2027 SAS 17-30 to OKC",
  "SAS first-round pick to OKC if 17-30.",

  2027, "OKC_DEN_LAC", "OKC", "OKC,DEN,LAC",
  "2027 OKC, DEN 6-30 or LAC (two most favorable to OKC)",
  "OKC receives the two most favorable / more favorable picks among OKC, DEN 6-30 and LAC.",
  2027, "OKC_DEN_LAC", "LAC", "OKC,DEN,LAC",
  "2027 OKC, DEN 6-30 or LAC (least / other to LAC)",
  "LAC receives the least favorable / other pick among OKC, DEN 6-30 and LAC.",

  2028, "ATL_CLE_UTA", "UTA", "ATL,CLE,UTA",
  "2028 UTA own or CLE (via UTA swap for CLE)",
  "More favorable of CLE and UTA to UTA.",
  2028, "ATL_CLE_UTA", "ATL", "ATL,CLE,UTA",
  "2028 ATL, CLE or UTA (more favorable to ATL)",
  "More favorable of ATL and the less favorable of CLE/UTA to ATL.",
  2028, "ATL_CLE_UTA", "CLE", "ATL,CLE,UTA",
  "2028 ATL, CLE or UTA (least favorable to CLE)",
  "Least favorable of ATL, CLE and UTA to CLE.",

  2028, "BOS_SAS", "SAS", "BOS,SAS",
  "2028 SAS own or BOS 2-30 (via SAS swap for BOS)",
  "SAS may swap for BOS if BOS is 2-30.",
  2028, "BOS_SAS", "BOS", "BOS,SAS",
  "2028 BOS own or SAS (via SAS swap for BOS, BOS protected #1)",
  "BOS receives the other pick if SAS exercises the BOS 2-30 swap right; BOS keeps #1.",

  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "BKN", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 BRK, PHL 9-30, PHX or NYK (most / two most favorable to BKN)",
  "Nested BKN/PHI/PHX/NYK/WAS/MIL/POR ranked-pool approximation; BKN receives the most / two most favorable eligible pick(s).",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "NYK", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 NYK, BRK, PHX or PHL 9-30 (NYK allocation)",
  "Nested BKN/PHI/PHX/NYK pool allocation to NYK.",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "WAS", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 WAS or least/less favorable BRK, PHL 9-30 and PHX (to WAS)",
  "WAS swap layer on the nested BKN/PHI/PHX pool.",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "PHX", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 PHX or least favorable BRK/PHL 9-30/WAS (to PHX)",
  "PHX receives the least favorable remaining pick in the nested pool.",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "MIL", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 MIL or WAS/PHX pool pick (via WAS/MIL swap layer)",
  "MIL swap layer on the WAS/PHX/BKN/PHI/NYK/POR pool.",

  2029, "DAL_HOU_PHX_BKN", "HOU", "DAL,HOU,PHX",
  "2029 DAL, HOU or PHX (two most favorable to HOU)",
  "Two most favorable of DAL, HOU and PHX to HOU.",
  2029, "DAL_HOU_PHX_BKN", "BKN", "DAL,HOU,PHX",
  "2029 DAL, HOU or PHX (least favorable to BKN)",
  "Least favorable of DAL, HOU and PHX to BKN.",

  2029, "BOS_MIL_POR_WAS", "POR", "BOS,MIL,POR",
  "2029 BOS, MIL or POR (most and least favorable to POR)",
  "Most and least favorable of BOS, MIL and POR to POR.",
  2029, "BOS_MIL_POR_WAS", "WAS", "BOS,MIL,POR",
  "2029 BOS, MIL or POR (second most favorable to WAS)",
  "Second most favorable of BOS, MIL and POR to WAS.",

  2029, "CLE_MIN_UTA_CHA", "UTA", "CLE,MIN,UTA",
  "2029 CLE, MIN 6-30 or UTA (most / two most favorable to UTA)",
  "Most / two most favorable of CLE, MIN 6-30 and UTA to UTA.",
  2029, "CLE_MIN_UTA_CHA", "CHA", "CLE,MIN,UTA",
  "2029 CLE, MIN 6-30 or UTA (other to CHA)",
  "Other / least favorable eligible pick from CLE, MIN 6-30 and UTA to CHA.",

  2029, "MEM_ORL", "MEM", "MEM,ORL",
  "2029 MEM own or ORL 3-30 (via MEM swap for ORL)",
  "MEM may swap for ORL if ORL is 3-30.",
  2029, "MEM_ORL", "ORL", "MEM,ORL",
  "2029 ORL own or MEM (via MEM swap for ORL, ORL protected 1-2)",
  "ORL keeps 1-2; otherwise receives the other pick if MEM exercises the swap.",

  2029, "LAC_PHI", "PHI", "LAC,PHI",
  "2029 PHI own or LAC 4-30 (via PHI swap for LAC)",
  "PHI may swap for LAC if LAC is 4-30.",
  2029, "LAC_PHI", "LAC", "LAC,PHI",
  "2029 LAC own or PHI (via PHI swap for LAC, LAC protected 1-3)",
  "LAC keeps 1-3; otherwise receives the other pick if PHI exercises the swap.",

  2030, "WAS_PHX_MEM", "WAS", "WAS,PHX,MEM",
  "2030 WAS or PHX (more favorable to WAS)",
  "More favorable of WAS and PHX to WAS.",
  2030, "WAS_PHX_MEM", "MEM", "WAS,PHX,MEM",
  "2030 MEM or less favorable WAS/PHX (more favorable to MEM)",
  "More favorable of MEM and the less favorable of WAS/PHX to MEM.",
  2030, "WAS_PHX_MEM", "PHX", "WAS,PHX,MEM",
  "2030 WAS, PHX or MEM (least favorable to PHX)",
  "Least favorable remaining pick among WAS, PHX and MEM to PHX.",

  2030, "DAL_SAS_MIN", "SAS", "DAL,SAS,MIN",
  "2030 DAL, SAS or MIN 2-30 (most favorable to SAS)",
  "Best eligible pick among DAL, SAS and MIN 2-30 to SAS.",
  2030, "DAL_SAS_MIN", "MIN", "DAL,SAS,MIN",
  "2030 DAL, SAS or MIN (second most favorable to MIN; MIN keeps #1)",
  "Second eligible pick to MIN; MIN keeps #1.",
  2030, "DAL_SAS_MIN", "DAL", "DAL,SAS,MIN",
  "2030 DAL, SAS or MIN 2-30 (least / other to DAL)",
  "Remaining eligible pick among DAL, SAS and MIN 2-30 to DAL.",

  2030, "MIL_POR", "POR", "MIL,POR",
  "2030 POR own or MIL (via POR swap for MIL)",
  "POR may swap with MIL.",
  2030, "MIL_POR", "MIL", "MIL,POR",
  "2030 MIL own or POR (via POR swap for MIL)",
  "MIL receives the other pick if POR exercises the swap."
)

if (nrow(complex_display_specs) > 0) {
  for (i in seq_len(nrow(complex_display_specs))) {
    spec <- complex_display_specs[i, ]
    orig <- stringr::str_split(spec$original_teams, "\\s*,\\s*")[[1]]
    add_display_group(
      display_asset_id = sprintf("display_%d_%s_%s", spec$year, spec$owner, spec$complex_group),
      year = spec$year,
      owner = spec$owner,
      original_teams = orig,
      label = spec$label,
      obligation = spec$obligation,
      notes = spec$obligation,
      group_type = "complex_entitlement",
      complex_group_filter = spec$complex_group,
      pick_types = c("own", "complex")
    )
  }
}

# Any asset not grouped above is already a one-to-one RealGM-style entitlement
# (locked 2026 pick, normal own pick, protected outgoing/retained row, or simple
# unprotected/protected incoming pick).
grouped_internal_asset_ids <- unique(pick_display_members$asset_id)
ungrouped_assets <- pick_assets %>%
  filter(!.data$asset_id %in% grouped_internal_asset_ids)

if (nrow(ungrouped_assets) > 0) {
  one_to_one_display <- ungrouped_assets %>%
    transmute(
      display_asset_id = paste0("display_", asset_id),
      owner,
      year,
      label,
      obligation,
      notes,
      group_type = "single_asset",
      display_group = asset_id,
      member_n = 1L,
      member_original_teams = original_team
    )

  pick_display_assets <- bind_rows(pick_display_assets, one_to_one_display)
  pick_display_members <- bind_rows(
    pick_display_members,
    ungrouped_assets %>%
      transmute(display_asset_id = paste0("display_", asset_id), asset_id)
  )
}

pick_display_members <- pick_display_members %>% distinct(display_asset_id, asset_id)
pick_display_assets <- pick_display_assets %>%
  distinct(display_asset_id, .keep_all = TRUE) %>%
  left_join(
    pick_display_members %>% count(display_asset_id, name = "member_n_actual"),
    by = "display_asset_id"
  ) %>%
  mutate(member_n = coalesce(member_n_actual, member_n)) %>%
  select(-member_n_actual) %>%
  arrange(year, owner, label)

cat(sprintf("User-facing pick entitlements: %d display rows from %d internal asset rows\n",
            nrow(pick_display_assets), nrow(pick_assets)))


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

cat("\n--- Fitting Pick Value Stan Models ---\n")

# Three pick-value model versions are fit on the same drafted-player rows so
# PSIS-LOO is pointwise-comparable across all models.
#   * 0601: oldest normal model with legacy war_obs / war_se data block
#   * 0605: player-level Student-t model with linear sigma[p]
#   * v2:   player-level Student-t model with adjacent-pick RW sigma[p]
# The v2 / random-walk model remains the production model used downstream.
PICK_MODEL_CONSTANT_SIGMA_PATH <- "zz_Archive/0605/pick_value_0601.stan"
PICK_MODEL_LINEAR_SIGMA_PATH   <- "zz_Archive/0605_1130/pick_value_0605.stan"
PICK_MODEL_RW_SIGMA_PATH       <- "pick_value_v3.stan"

resolve_required_file <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Required file not found: %s", path), call. = FALSE)
  }
  path
}

# Player-level pick-value data: one row per drafted player, outcome = first-four
# season Win Shares. All three Stan models are compared on these same rows.
pick_fit_data <- if (!is.null(draft_4yr) && nrow(draft_4yr) > 50) {
  draft_4yr %>%
    transmute(
      draft_year = as.integer(draft_year),
      pick       = as.integer(pick),
      player     = as.character(player),
      ws4        = as.numeric(ws4)
    ) %>%
    filter(!is.na(pick), pick >= 1, pick <= 30, !is.na(ws4))
} else {
  # Fallback synthetic player-level pool built from the compiled slot curve.
  pick_boot_pool %>%
    mutate(
      draft_year = NA_integer_,
      player     = NA_character_
    ) %>%
    transmute(draft_year, pick = as.integer(pick), player, ws4 = as.numeric(ws4)) %>%
    filter(!is.na(pick), pick >= 1, pick <= 30, !is.na(ws4))
}

pick_stan_data_player <- list(
  N    = nrow(pick_fit_data),
  pick = pick_fit_data$pick,
  ws4  = pick_fit_data$ws4
)

# The oldest model has the legacy slot-mean data names. Passing ws4 as war_obs
# and war_se = 0 makes its generated log_lik one row per drafted player, so
# loo_compare() is valid against the two newer player-level models.
pick_stan_data_legacy <- list(
  N       = nrow(pick_fit_data),
  pick    = pick_fit_data$pick,
  war_obs = pick_fit_data$ws4,
  war_se  = rep(0, nrow(pick_fit_data))
)

sample_pick_stan_model <- function(model_path,
                                   data,
                                   seed,
                                   label,
                                   adapt_delta = 0.95,
                                   max_treedepth = 12) {
  cat(sprintf("\n  [pick model] Compiling %s: %s\n", label, model_path))
  model <- cmdstan_model(resolve_required_file(model_path))
  cat(sprintf("  [pick model] Sampling %s\n", label))
  model$sample(
    data            = data,
    chains          = 4,
    parallel_chains = 4,
    iter_warmup     = 1000,
    iter_sampling   = 2000,
    adapt_delta     = adapt_delta,
    max_treedepth   = max_treedepth,
    seed            = seed,
    refresh         = 100
  )
}

pick_fit_constant_sigma <- sample_pick_stan_model(
  model_path = PICK_MODEL_CONSTANT_SIGMA_PATH,
  data       = pick_stan_data_legacy,
  seed       = 202601,
  label      = "constant_sigma / 0601"
)

pick_fit_linear_sigma <- sample_pick_stan_model(
  model_path = PICK_MODEL_LINEAR_SIGMA_PATH,
  data       = pick_stan_data_player,
  seed       = 202605,
  label      = "linear_sigma / 0605"
)

pick_fit_rw_sigma <- sample_pick_stan_model(
  model_path = PICK_MODEL_RW_SIGMA_PATH,
  data       = pick_stan_data_player,
  seed       = 202602,
  label      = "rw_sigma / v3"
)

# Keep the random-walk hierarchical model as the production fit used below.
pick_fit <- pick_fit_rw_sigma

# ---- VALIDATION 0: PSIS-LOO comparison across all three pick-value models ----
# These object names intentionally match the validation call you want to run.
loo_constant_sigma <- loo::loo(
  as.matrix(pick_fit_constant_sigma$draws("log_lik", format = "draws_matrix"))
)
loo_linear_sigma <- loo::loo(
  as.matrix(pick_fit_linear_sigma$draws("log_lik", format = "draws_matrix"))
)
loo_rw_sigma <- loo::loo(
  as.matrix(pick_fit_rw_sigma$draws("log_lik", format = "draws_matrix"))
)

cat("\n  [validate] PSIS-LOO comparison: constant vs linear vs RW sigma\n")
pick_loo_compare <- loo_compare(
  loo_constant_sigma,
  loo_linear_sigma,
  loo_rw_sigma
)
print(pick_loo_compare)

cat("\n  [validate] Pareto-k table: constant_sigma / 0601\n")
print(pareto_k_table(loo_constant_sigma))
cat("\n  [validate] Pareto-k table: linear_sigma / 0605\n")
print(pareto_k_table(loo_linear_sigma))
cat("\n  [validate] Pareto-k table: rw_sigma / v2\n")
print(pareto_k_table(loo_rw_sigma))

pick_loo_compare_tbl <- as.data.frame(pick_loo_compare) %>%
  rownames_to_column("model") %>%
  as_tibble()

pick_loo_summary <- tibble(
  model = c("constant_sigma_0601", "linear_sigma_0605", "rw_sigma_v2"),
  elpd_loo = c(
    loo_constant_sigma$estimates["elpd_loo", "Estimate"],
    loo_linear_sigma$estimates["elpd_loo", "Estimate"],
    loo_rw_sigma$estimates["elpd_loo", "Estimate"]
  ),
  p_loo = c(
    loo_constant_sigma$estimates["p_loo", "Estimate"],
    loo_linear_sigma$estimates["p_loo", "Estimate"],
    loo_rw_sigma$estimates["p_loo", "Estimate"]
  ),
  looic = c(
    loo_constant_sigma$estimates["looic", "Estimate"],
    loo_linear_sigma$estimates["looic", "Estimate"],
    loo_rw_sigma$estimates["looic", "Estimate"]
  ),
  max_pareto_k = c(
    max(loo::pareto_k_values(loo_constant_sigma), na.rm = TRUE),
    max(loo::pareto_k_values(loo_linear_sigma), na.rm = TRUE),
    max(loo::pareto_k_values(loo_rw_sigma), na.rm = TRUE)
  )
)

cat("\n  [validate] LOO summary by pick-value model\n")
print(pick_loo_summary)

# Core scalar variables for diagnostics. sigma_base/sigma_slope were removed
# in the adjacent-pick variance model. Residual scales now live in the
# generated vector war_pred_sd[1:30] / transformed vector sigma_pick[1:30].
pick_core_vars <- c("alpha", "beta", "gamma", "log_sigma_1", "tau_log_sigma_rw", "nu")

pick_draws <- pick_fit$draws(
  variables = c("alpha", "beta", "gamma", "tau_log_sigma_rw", "nu"),
  format = "df"
) %>% as_tibble()

# Extract Stan vector draws in numeric index order, regardless of CmdStan's
# column ordering. These matrices are draws x pick slot.
extract_stan_vector_draws <- function(fit, variable_base, K = 30L) {
  mat <- as.matrix(fit$draws(variables = variable_base, format = "draws_matrix"))
  idx <- stringr::str_match(colnames(mat), paste0("^", variable_base, "\\[(\\d+)\\]$"))[, 2]
  if (any(is.na(idx))) {
    stop("Could not parse Stan vector indices for ", variable_base, call. = FALSE)
  }
  ord <- order(as.integer(idx))
  mat <- mat[, ord, drop = FALSE]
  colnames(mat) <- paste0(variable_base, "[", seq_len(ncol(mat)), "]")
  if (ncol(mat) != K) {
    warning("Expected ", K, " columns for ", variable_base, ", found ", ncol(mat), ".")
  }
  mat
}

pick_mu_draws <- extract_stan_vector_draws(pick_fit, "war_pred", 30L)
pick_sd_draws <- extract_stan_vector_draws(pick_fit, "war_pred_sd", 30L)

cat("\n  Pick-value posterior summary: rw_sigma / v2 production model\n")
print(pick_fit$summary(pick_core_vars))

# ---- VALIDATION 1: convergence diagnostics ----
pick_diag <- pick_fit$summary(pick_core_vars) %>%
  select(variable, rhat, ess_bulk, ess_tail)
cat("\n  [validate] Pick model R-hat / ESS: rw_sigma / v2 production model\n")
print(pick_diag)
if (any(pick_diag$rhat > 1.01, na.rm = TRUE)) {
  warning("Pick model: some R-hat > 1.01 — inspect convergence.")
}

# ---- VALIDATION 2: posterior-predictive player-level coverage ----
# Share of drafted-player outcomes whose realized first-4-year WS falls in the
# 90% posterior predictive interval. PPCs are shown for the production v2 model.
ws4_rep_mat <- as.matrix(pick_fit$draws(variables = "ws4_rep", format = "draws_matrix"))

ppc_tbl <- tibble(
  row_id = seq_len(nrow(pick_fit_data)),
  draft_year = pick_fit_data$draft_year,
  pick = pick_fit_data$pick,
  player = pick_fit_data$player,
  obs = pick_fit_data$ws4,
  lo = apply(ws4_rep_mat, 2, quantile, probs = 0.05, na.rm = TRUE),
  hi = apply(ws4_rep_mat, 2, quantile, probs = 0.95, na.rm = TRUE)
) %>%
  mutate(covered = obs >= lo & obs <= hi)

cat(sprintf("  [validate] PPC 90%% coverage: %.0f%% of player rows\n",
            100 * mean(ppc_tbl$covered)))

ppc_band_tbl <- ppc_tbl %>%
  mutate(
    pick_band = case_when(
      pick <= 5  ~ "1-5",
      pick <= 10 ~ "6-10",
      pick <= 15 ~ "11-15",
      pick <= 20 ~ "16-20",
      pick <= 30 ~ "21-30"
    )
  ) %>%
  group_by(pick_band) %>%
  summarise(
    n = n(),
    coverage_90 = mean(covered),
    mean_obs = mean(obs),
    mean_pred_mid = mean((lo + hi) / 2),
    .groups = "drop"
  )

print(ppc_band_tbl)

ws4_rep_mean <- colMeans(ws4_rep_mat)

ppc_pick_resid <- ppc_tbl %>%
  mutate(pred_mean = ws4_rep_mean) %>%
  group_by(pick) %>%
  summarise(
    n = n(),
    obs_mean = mean(obs),
    pred_mean = mean(pred_mean),
    resid = obs_mean - pred_mean,
    coverage_90 = mean(covered),
    .groups = "drop"
  )

print(ppc_pick_resid, n = 30)

sigma_draws <- as.matrix(pick_fit$draws("war_pred_sd", format = "draws_matrix"))

sigma_curve <- tibble(
  pick = 1:30,
  sigma_mean = colMeans(sigma_draws),
  sigma_q05 = apply(sigma_draws, 2, quantile, 0.05),
  sigma_q50 = apply(sigma_draws, 2, quantile, 0.50),
  sigma_q95 = apply(sigma_draws, 2, quantile, 0.95)
)

print(sigma_curve, n = 30)

# Keep the original single-model LOO alias for backward compatibility, but the
# main model-selection object is pick_loo_compare / loo_compare(...) above.
loo_pick <- loo_rw_sigma
cat("\n  [validate] Production pick model LOO: rw_sigma / v2\n")
print(loo_pick)
print(pareto_k_table(loo_pick))


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

# Bayesian: draw curve params from the posterior, then draw a player-level
# first-4-year WS outcome from the Student-t predictive distribution.
sample_pick_value_bayes <- function(pos, draw_idx = sample(nrow(pick_draws), 1)) {
  pos <- as.integer(pos)
  pos <- max(1L, min(30L, pos))

  # Use generated posterior mean / residual-scale vectors so downstream code
  # does not depend on the internal variance parameterization. This works for
  # the adjacent-pick-smoothed sigma model and will also work if the mean model
  # later adds slot effects.
  mu <- pick_mu_draws[draw_idx, pos]
  sg <- pick_sd_draws[draw_idx, pos]
  nu <- pick_draws$nu[draw_idx]

  mu + sg * rt(1, df = nu)
}

# Bootstrap: resample an actual player's 4-yr WS from a neighborhood of slots.
# Borrows from +/- 1 slot to stabilize thin slots, mirroring the smoothing in
# the McCartney trajectory work.
boot_index <- split(pick_boot_pool$ws4, pick_boot_pool$pick)
sample_pick_value_boot <- function(pos) {
  nb <- as.character(c(pos - 1, pos, pos + 1))
  pool <- unlist(boot_index[nb[nb %in% names(boot_index)]], use.names = FALSE)
  if (length(pool) == 0) pool <- unlist(boot_index, use.names = FALSE)
  sample(pool, 1)
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

# ---- future-pick allocation helpers -----------------------------------------
# These functions resolve original-team pick ownership in each Monte Carlo draw.
# They leave the lottery slot attached to the ORIGINAL team, then separately
# assign that original pick to an owner. That is what allows protections, swap
# returns, retained own picks, and multi-team ranked pools to flow downstream.

rank_teams_by_slot <- function(slots, teams) {
  teams <- teams[teams %in% names(slots)]
  teams[order(as.numeric(slots[teams]), na.last = TRUE)]
}

apply_simple_future_obligations <- function(owner_by_orig, slots, yr) {
  rows <- traded_future %>% filter(year == yr)

  # Outright protected/unprotected transfers. If protection does not convey,
  # owner_by_orig stays as the original team, so the own/retained asset receives value.
  outrights <- rows %>% filter(pick_type == "outright")
  if (nrow(outrights) > 0) {
    for (j in seq_len(nrow(outrights))) {
      og <- outrights$original_team[j]
      ow <- outrights$owner[j]
      prot <- outrights$protection[j]
      if (!is.na(slots[og]) && pick_conveys(slots[og], prot)) {
        owner_by_orig[og] <- ow
      }
    }
  }

  # Simple two-team swaps. The holder receives the more favorable original pick;
  # the counterparty receives the less favorable original pick through the
  # automatically generated swap_return asset.
  swaps <- rows %>% filter(pick_type == "swap")
  if (nrow(swaps) > 0) {
    for (j in seq_len(nrow(swaps))) {
      holder <- swaps$owner[j]
      counter <- swaps$original_team[j]
      if (is.na(slots[holder]) || is.na(slots[counter])) next
      if (slots[counter] < slots[holder]) {
        owner_by_orig[counter] <- holder
        owner_by_orig[holder]  <- counter
      } else {
        owner_by_orig[counter] <- counter
        owner_by_orig[holder]  <- holder
      }
    }
  }

  owner_by_orig
}

apply_complex_future_obligations <- function(owner_by_orig, slots, yr) {
  # 2027 ----------------------------------------------------------------------
  if (yr == 2027L) {
    # MIL/NOP: best to NOP; other to ATL if 5-30; if both top-4, both to NOP.
    r <- rank_teams_by_slot(slots, c("MIL", "NOP"))
    if (length(r) == 2) {
      owner_by_orig[r[1]] <- "NOP"
      owner_by_orig[r[2]] <- if (!is.na(slots[r[2]]) && slots[r[2]] <= 4) "NOP" else "ATL"
    }

    # CLE/MIN/UTA: best MEM, second UTA, least PHX.
    r <- rank_teams_by_slot(slots, c("CLE", "MIN", "UTA"))
    if (length(r) == 3) {
      owner_by_orig[r[1]] <- "MEM"
      owner_by_orig[r[2]] <- "UTA"
      owner_by_orig[r[3]] <- "PHX"
    }

    # SAN: 1-16 SAC, 17-30 OKC.
    if (!is.na(slots["SAS"])) {
      owner_by_orig["SAS"] <- if (slots["SAS"] <= 16) "SAC" else "OKC"
    }

    # OKC/DEN/LAC: DEN participates only if 6-30. If DEN is top-5 it stays DEN.
    pool <- c("OKC", "LAC")
    if (!is.na(slots["DEN"]) && slots["DEN"] > 5) {
      pool <- c(pool, "DEN")
    } else {
      owner_by_orig["DEN"] <- "DEN"
    }
    r <- rank_teams_by_slot(slots, pool)
    if (length(r) == 2) {
      owner_by_orig[r[1]] <- "OKC"
      owner_by_orig[r[2]] <- "LAC"
    } else if (length(r) >= 3) {
      owner_by_orig[r[1:2]] <- "OKC"
      owner_by_orig[r[3]] <- "LAC"
    }
  }

  # 2028 ----------------------------------------------------------------------
  if (yr == 2028L) {
    # ATL/CLE/UTA: more favorable CLE/UTA to UTA; more favorable of ATL and
    # less favorable CLE/UTA to ATL; least of those two to CLE.
    cu <- rank_teams_by_slot(slots, c("CLE", "UTA"))
    if (length(cu) == 2) {
      owner_by_orig[cu[1]] <- "UTA"
      atl_pair <- rank_teams_by_slot(slots, c("ATL", cu[2]))
      if (length(atl_pair) == 2) {
        owner_by_orig[atl_pair[1]] <- "ATL"
        owner_by_orig[atl_pair[2]] <- "CLE"
      }
    }

    # SAS/BOS: BOS #1 protected from swap; otherwise SAS can take BOS if better.
    if (!is.na(slots["BOS"]) && !is.na(slots["SAS"]) && slots["BOS"] > 1) {
      if (slots["BOS"] < slots["SAS"]) {
        owner_by_orig["BOS"] <- "SAS"
        owner_by_orig["SAS"] <- "BOS"
      }
    }

    # BKN/PHI/PHX/NYK/WAS/MIL/POR nested pool. RealGM's text is deeply nested;
    # this is an explicit approximation: PHI keeps 1-8; among eligible BKN/PHX/
    # NYK/PHI picks, BKN gets the best two when available, NYK the next, PHX the
    # remainder; WAS can then improve by taking the better of WAS and PHX's
    # currently allocated pick; MIL can then improve by taking the better of MIL
    # and WAS's post-swap pick. POR is included only as MIL/WAS swap context.
    pool <- c("BKN", "PHX", "NYK")
    if (!is.na(slots["PHI"]) && slots["PHI"] > 8) {
      pool <- c(pool, "PHI")
    } else {
      owner_by_orig["PHI"] <- "PHI"
    }
    r <- rank_teams_by_slot(slots, pool)
    if (length(r) >= 1) owner_by_orig[r[1]] <- "BKN"
    if (length(r) >= 2) owner_by_orig[r[2]] <- "BKN"
    if (length(r) >= 3) owner_by_orig[r[3]] <- "NYK"
    if (length(r) >= 4) owner_by_orig[r[4]] <- "PHX"

    # Approximate WAS/PHX and MIL/WAS swap layers using the PHX-assigned pick.
    phx_pick <- names(owner_by_orig)[owner_by_orig == "PHX" & names(owner_by_orig) %in% pool]
    if (length(phx_pick) > 0 && !is.na(slots["WAS"])) {
      target <- rank_teams_by_slot(slots, c("WAS", phx_pick[1]))
      if (length(target) == 2 && target[1] != "WAS") {
        owner_by_orig[target[1]] <- "WAS"
        owner_by_orig["WAS"] <- "PHX"
      }
    }
    was_pick <- names(owner_by_orig)[owner_by_orig == "WAS"]
    was_pick <- was_pick[was_pick %in% c("BKN", "PHI", "PHX", "NYK", "WAS")]
    if (length(was_pick) > 0 && !is.na(slots["MIL"])) {
      target <- rank_teams_by_slot(slots, c("MIL", was_pick[1]))
      if (length(target) == 2 && target[1] != "MIL") {
        owner_by_orig[target[1]] <- "MIL"
        owner_by_orig["MIL"] <- "WAS"
      }
    }
  }

  # 2029 ----------------------------------------------------------------------
  if (yr == 2029L) {
    # DAL/HOU/PHX: two best to HOU, other to BKN.
    r <- rank_teams_by_slot(slots, c("DAL", "HOU", "PHX"))
    if (length(r) == 3) {
      owner_by_orig[r[1:2]] <- "HOU"
      owner_by_orig[r[3]] <- "BKN"
    }

    # BOS/MIL/POR: best and worst to POR; middle to WAS.
    r <- rank_teams_by_slot(slots, c("BOS", "MIL", "POR"))
    if (length(r) == 3) {
      owner_by_orig[r[c(1, 3)]] <- "POR"
      owner_by_orig[r[2]] <- "WAS"
    }

    # CLE/MIN/UTA with MIN top-5 protection.
    pool <- c("CLE", "UTA")
    if (!is.na(slots["MIN"]) && slots["MIN"] > 5) {
      pool <- c(pool, "MIN")
    } else {
      owner_by_orig["MIN"] <- "MIN"
    }
    r <- rank_teams_by_slot(slots, pool)
    if (length(r) == 2) {
      owner_by_orig[r[1]] <- "UTA"
      owner_by_orig[r[2]] <- "CHA"
    } else if (length(r) >= 3) {
      owner_by_orig[r[1:2]] <- "UTA"
      owner_by_orig[r[3]] <- "CHA"
    }

    # MEM/ORL: ORL keeps 1-2, otherwise MEM can swap for ORL if ORL is better.
    if (!is.na(slots["ORL"]) && slots["ORL"] > 2 && !is.na(slots["MEM"])) {
      if (slots["ORL"] < slots["MEM"]) {
        owner_by_orig["ORL"] <- "MEM"
        owner_by_orig["MEM"] <- "ORL"
      }
    }

    # PHI/LAC: LAC keeps 1-3, otherwise PHI can swap for LAC if LAC is better.
    if (!is.na(slots["LAC"]) && slots["LAC"] > 3 && !is.na(slots["PHI"])) {
      if (slots["LAC"] < slots["PHI"]) {
        owner_by_orig["LAC"] <- "PHI"
        owner_by_orig["PHI"] <- "LAC"
      }
    }
  }

  # 2030 ----------------------------------------------------------------------
  if (yr == 2030L) {
    # WAS/PHX/MEM: better WAS/PHX to WAS; better of MEM and worse WAS/PHX to
    # MEM; remaining to PHX.
    wp <- rank_teams_by_slot(slots, c("WAS", "PHX"))
    if (length(wp) == 2) {
      owner_by_orig[wp[1]] <- "WAS"
      rem <- rank_teams_by_slot(slots, c("MEM", wp[2]))
      if (length(rem) == 2) {
        owner_by_orig[rem[1]] <- "MEM"
        owner_by_orig[rem[2]] <- "PHX"
      }
    }

    # SAS/DAL/MIN. MIN keeps #1. Otherwise, best of DAL/SAS/MIN to SAS, second
    # to MIN, remaining to DAL. This mirrors the rank language closely enough
    # for valuation without adding another nested state machine.
    if (!is.na(slots["MIN"]) && slots["MIN"] == 1) {
      sd <- rank_teams_by_slot(slots, c("SAS", "DAL"))
      if (length(sd) == 2) {
        owner_by_orig[sd[1]] <- "SAS"
        owner_by_orig[sd[2]] <- "DAL"
      }
      owner_by_orig["MIN"] <- "MIN"
    } else {
      r <- rank_teams_by_slot(slots, c("SAS", "DAL", "MIN"))
      if (length(r) == 3) {
        owner_by_orig[r[1]] <- "SAS"
        owner_by_orig[r[2]] <- "MIN"
        owner_by_orig[r[3]] <- "DAL"
      }
    }

    # POR/MIL: POR may take MIL if MIL is better, with MIL receiving POR.
    if (!is.na(slots["MIL"]) && !is.na(slots["POR"]) && slots["MIL"] < slots["POR"]) {
      owner_by_orig["MIL"] <- "POR"
      owner_by_orig["POR"] <- "MIL"
    }
  }

  owner_by_orig
}

resolve_pick_owners <- function(slots, yr) {
  owner_by_orig <- setNames(all_teams, all_teams)
  owner_by_orig <- apply_simple_future_obligations(owner_by_orig, slots, yr)
  owner_by_orig <- apply_complex_future_obligations(owner_by_orig, slots, yr)
  owner_by_orig
}

value_allocated_future_assets <- function(sim, yr, slots, owner_by_orig, d_pick,
                                          team_value, team_n, team_best,
                                          system = c("cur", "new")) {
  system <- match.arg(system)
  yr_assets <- pick_assets %>% filter(year == yr)
  raw_by_orig <- setNames(rep(0, length(all_teams)), all_teams)
  for (tm in all_teams) {
    raw_by_orig[tm] <- if (!is.na(slots[tm])) sample_pick_value(slots[tm], draw_idx = d_pick) else 0
  }

  for (j in seq_len(nrow(yr_assets))) {
    aid <- yr_assets$asset_id[j]
    og  <- yr_assets$original_team[j]
    ow  <- yr_assets$owner[j]
    allocated <- !is.na(owner_by_orig[og]) && owner_by_orig[og] == ow
    val <- if (allocated) raw_by_orig[og] else 0

    if (system == "cur") {
      asset_cur[sim, aid] <<- val
      asset_slot_cur[sim, aid] <<- slots[og]
      asset_raw_cur[sim, aid] <<- raw_by_orig[og]
      asset_ownslot_cur[sim, aid] <<- slots[ow]
      asset_convey_cur[sim, aid] <<- as.integer(allocated)
    } else {
      asset_new[sim, aid] <<- val
      asset_slot_new[sim, aid] <<- slots[og]
      asset_raw_new[sim, aid] <<- raw_by_orig[og]
      asset_ownslot_new[sim, aid] <<- slots[ow]
      asset_convey_new[sim, aid] <<- as.integer(allocated)
    }

    if (allocated) {
      team_value[ow] <- team_value[ow] + val
      team_n[ow]     <- team_n[ow] + 1L
      team_best[ow]  <- max(team_best[ow], val)
    }
  }

  list(team_value = team_value, team_n = team_n, team_best = team_best)
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
  nbest <- setNames(rep(-Inf, 30), all_teams)
  ct    <- setNames(rep(0L, 30), all_teams)
  asset_val  <- setNames(rep(NA_real_, n_assets), asset_ids)
  asset_slot <- setNames(rep(NA_real_, n_assets), asset_ids)
  a26_assets <- pick_assets %>% filter(year == 2026L)
  for (r in seq_len(nrow(a26_assets))) {
    own <- a26_assets$owner[r]
    sl  <- a26_assets$fixed_slot[r]
    val <- sample_pick_value(sl, draw_idx = d_pick)
    v[own]     <- v[own] + val
    nbest[own] <- max(nbest[own], val)
    ct[own]    <- ct[own] + 1L
    asset_val[a26_assets$asset_id[r]]  <- val
    asset_slot[a26_assets$asset_id[r]] <- sl
  }
  list(v = v, best = nbest, n = ct, asset_val = asset_val, asset_slot = asset_slot)
}

# Per-asset value stores: [N_SIMS x n_assets] under each system.
# 2026 assets get identical current/new values; future assets differ.
asset_cur <- matrix(NA_real_, nrow = N_SIMS, ncol = n_assets,
                    dimnames = list(NULL, asset_ids))
asset_new <- matrix(NA_real_, nrow = N_SIMS, ncol = n_assets,
                    dimnames = list(NULL, asset_ids))

# --- Extra stores so the Trade Machine can apply HYPOTHETICAL protections /
#     swaps to a pick a user is sending. For every asset we record, per sim:
#       *_slot_*   = the ORIGINAL team's drafted slot (1-30)
#       *_raw_*    = the pick VALUE at that slot, BEFORE any conveyance test
#       *_ownslot_*= the OWNER team's own slot that year (needed for swaps)
#   With these the app can recompute "value if top-N protected" (= raw if
#   slot > N else 0) or "value if swap" (= value at min(own, target) slot)
#   on the fly, with correct within-sim correlation.
asset_slot_cur   <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_slot_new   <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_raw_cur    <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_raw_new    <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_ownslot_cur <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_ownslot_new <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_convey_cur <- matrix(0L, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_convey_new <- matrix(0L, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))

# Full team x year slot record (worst-to-best draft seat each projected year)
# plus the pick-value curve parameters used in each sim. Together these let the
# Trade Machine value ANY slot in ANY sim, so a user can attach a hypothetical
# protection or swap to a pick and we recompute conveyance with correct
# within-sim correlation. Years are FIRST_PROJECTED_DRAFT..LAST_PROJECTED_DRAFT.
proj_years   <- FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT
n_proj_years <- length(proj_years)
team_slot_cur <- array(NA_real_, dim = c(N_SIMS, 30, n_proj_years),
                       dimnames = list(NULL, all_teams, as.character(proj_years)))
team_slot_new <- array(NA_real_, dim = c(N_SIMS, 30, n_proj_years),
                       dimnames = list(NULL, all_teams, as.character(proj_years)))
sim_curve_par_cols <- c(
  "alpha", "beta", "gamma", "tau_log_sigma_rw", "nu",
  paste0("mu_", 1:30),
  paste0("sigma_", 1:30)
)
sim_curve_par <- matrix(NA_real_, N_SIMS, length(sim_curve_par_cols),
                        dimnames = list(NULL, sim_curve_par_cols))

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
  # record this sim's pick-value curve params (for app-side hypothetical picks)
  sim_curve_par[sim, ] <- c(
    pick_draws$alpha[d_pick],
    pick_draws$beta[d_pick],
    pick_draws$gamma[d_pick],
    pick_draws$tau_log_sigma_rw[d_pick],
    pick_draws$nu[d_pick],
    as.numeric(pick_mu_draws[d_pick, ]),
    as.numeric(pick_sd_draws[d_pick, ])
  )

  # transition matrix for this sim
  P <- t(vapply(1:N_TIERS, function(i) get_theta_row(d_mk, i), numeric(N_TIERS)))

  # accumulators
  tv_c <- setNames(rep(0, 30), all_teams); tv_n <- tv_c
  tn_c <- setNames(rep(0L, 30), all_teams); tn_n <- tn_c
  tb_c <- setNames(rep(-Inf, 30), all_teams); tb_n <- tb_c

  # 2026 actual (identical under both systems)
  a26 <- value_2026(d_pick)
  tv_c <- tv_c + a26$v;  tv_n <- tv_n + a26$v
  tn_c <- tn_c + a26$n;  tn_n <- tn_n + a26$n
  tb_c <- pmax(tb_c, a26$best); tb_n <- pmax(tb_n, a26$best)
  # store per-asset 2026 values (same under both systems). 2026 is locked, so
  # slot == fixed slot, raw value == realized value, and own-slot == slot.
  a26_ids <- pick_assets$asset_id[pick_assets$year == 2026L]
  asset_cur[sim, a26_ids] <- a26$asset_val[a26_ids]
  asset_new[sim, a26_ids] <- a26$asset_val[a26_ids]
  asset_slot_cur[sim, a26_ids]    <- a26$asset_slot[a26_ids]
  asset_slot_new[sim, a26_ids]    <- a26$asset_slot[a26_ids]
  asset_raw_cur[sim, a26_ids]     <- a26$asset_val[a26_ids]
  asset_raw_new[sim, a26_ids]     <- a26$asset_val[a26_ids]
  asset_ownslot_cur[sim, a26_ids] <- a26$asset_slot[a26_ids]
  asset_ownslot_new[sim, a26_ids] <- a26$asset_slot[a26_ids]
  asset_convey_cur[sim, a26_ids] <- 1L
  asset_convey_new[sim, a26_ids] <- 1L

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

    # ---- apply NEW anti-tank restrictions (3-2-1 system only) ----
    # Restrictions look back at the ORIGINAL team's recent top picks. They do
    # not change who owns the pick; ownership is resolved below.
    for (tm in all_teams) {
      if (!is.na(slot_n[tm])) {
        slot_n[tm] <- apply_pick_restrictions(slot_n[tm], tm, yr, top_hist)
      }
    }

    # ---- record realized ORIGINAL-team seats this year ----------------------
    yc <- as.character(yr)
    for (tm in all_teams) {
      if (!is.na(slot_c[tm])) team_slot_cur[sim, tm, yc] <- slot_c[tm]
      if (!is.na(slot_n[tm])) team_slot_new[sim, tm, yc] <- slot_n[tm]
    }

    # ---- resolve all simple + complex ownership obligations -----------------
    owner_by_orig_c <- resolve_pick_owners(slot_c, yr)
    owner_by_orig_n <- resolve_pick_owners(slot_n, yr)

    # ---- value every possible future pick asset -----------------------------
    # Each original team's pick can be allocated to exactly one owner under each
    # system. Assets whose condition is not met get zero value in that draw; the
    # retained own-pick asset gets value when protections/swaps do not convey.
    val_c <- value_allocated_future_assets(
      sim = sim, yr = yr, slots = slot_c, owner_by_orig = owner_by_orig_c,
      d_pick = d_pick,
      team_value = tv_c, team_n = tn_c, team_best = tb_c,
      system = "cur"
    )
    tv_c <- val_c$team_value; tn_c <- val_c$team_n; tb_c <- val_c$team_best

    val_n <- value_allocated_future_assets(
      sim = sim, yr = yr, slots = slot_n, owner_by_orig = owner_by_orig_n,
      d_pick = d_pick,
      team_value = tv_n, team_n = tn_n, team_best = tb_n,
      system = "new"
    )
    tv_n <- val_n$team_value; tn_n <- val_n$team_n; tb_n <- val_n$team_best

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
    current_best = ifelse(is.finite(tb_c), tb_c, NA_real_),
    new_best = ifelse(is.finite(tb_n), tb_n, NA_real_),
    sim_id = sim
  )
  if (sim %% 100 == 0) cat(sprintf("  %d / %d\n", sim, N_SIMS))
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
# raw/slot stores: leave slots as NA where a pick had no seat (e.g. own pick
# traded away that year); raw values default to 0 so protection math is safe.
asset_raw_cur[is.na(asset_raw_cur)] <- 0
asset_raw_new[is.na(asset_raw_new)] <- 0

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
    cur_convey_prob = colMeans(asset_convey_cur)[asset_id],
    new_convey_prob = colMeans(asset_convey_new)[asset_id],
    delta    = new_mean - cur_mean
  )


# Summed display-entitlement matrices. These collapse mutually exclusive or
# grouped internal assets into one user-facing pick, without changing the team
# portfolio totals already computed above.
build_display_draw_matrix <- function(draw_mat, display_members, display_assets) {
  out <- matrix(
    0,
    nrow = nrow(draw_mat),
    ncol = nrow(display_assets),
    dimnames = list(NULL, display_assets$display_asset_id)
  )

  for (did in display_assets$display_asset_id) {
    ids <- display_members %>%
      filter(.data$display_asset_id == .env$did) %>%
      pull(asset_id)
    ids <- ids[ids %in% colnames(draw_mat)]
    if (length(ids) == 1L) {
      out[, did] <- draw_mat[, ids]
    } else if (length(ids) > 1L) {
      out[, did] <- rowSums(draw_mat[, ids, drop = FALSE])
    }
  }
  out
}

build_display_convey_matrix <- function(convey_mat, display_members, display_assets) {
  out <- matrix(
    0,
    nrow = nrow(convey_mat),
    ncol = nrow(display_assets),
    dimnames = list(NULL, display_assets$display_asset_id)
  )

  for (did in display_assets$display_asset_id) {
    ids <- display_members %>%
      filter(.data$display_asset_id == .env$did) %>%
      pull(asset_id)
    ids <- ids[ids %in% colnames(convey_mat)]
    if (length(ids) == 1L) {
      out[, did] <- convey_mat[, ids]
    } else if (length(ids) > 1L) {
      # Some entitlements can produce two picks in rare protected/ranked-pool
      # scenarios, so keep the count rather than forcing a 0/1 indicator.
      out[, did] <- rowSums(convey_mat[, ids, drop = FALSE])
    }
  }
  out
}

display_asset_cur_full <- build_display_draw_matrix(asset_cur, pick_display_members, pick_display_assets)
display_asset_new_full <- build_display_draw_matrix(asset_new, pick_display_members, pick_display_assets)
display_convey_cur_full <- build_display_convey_matrix(asset_convey_cur, pick_display_members, pick_display_assets)
display_convey_new_full <- build_display_convey_matrix(asset_convey_new, pick_display_members, pick_display_assets)

pick_display_value_summary <- pick_display_assets %>%
  mutate(
    cur_mean = colMeans(display_asset_cur_full)[display_asset_id],
    cur_q05  = apply(display_asset_cur_full, 2, quantile, 0.05)[display_asset_id],
    cur_q50  = apply(display_asset_cur_full, 2, quantile, 0.50)[display_asset_id],
    cur_q95  = apply(display_asset_cur_full, 2, quantile, 0.95)[display_asset_id],
    cur_sd   = apply(display_asset_cur_full, 2, sd)[display_asset_id],
    new_mean = colMeans(display_asset_new_full)[display_asset_id],
    new_q05  = apply(display_asset_new_full, 2, quantile, 0.05)[display_asset_id],
    new_q50  = apply(display_asset_new_full, 2, quantile, 0.50)[display_asset_id],
    new_q95  = apply(display_asset_new_full, 2, quantile, 0.95)[display_asset_id],
    new_sd   = apply(display_asset_new_full, 2, sd)[display_asset_id],
    cur_convey_prob = colMeans(display_convey_cur_full > 0)[display_asset_id],
    new_convey_prob = colMeans(display_convey_new_full > 0)[display_asset_id],
    cur_expected_pick_count = colMeans(display_convey_cur_full)[display_asset_id],
    new_expected_pick_count = colMeans(display_convey_new_full)[display_asset_id],
    delta = new_mean - cur_mean
  )

# Downsample the joint per-sim matrices so the dashboard can compute trade
# deltas and P(team A nets more wins) with proper within-sim correlation,
# without shipping the full 10k-row matrices.
n_keep   <- min(2000, N_SIMS)
keep_idx <- sort(sample(N_SIMS, n_keep))
asset_cur_draws <- asset_cur[keep_idx, , drop = FALSE]
asset_new_draws <- asset_new[keep_idx, , drop = FALSE]

# Downsample the slot / raw / curve stores on the SAME kept sims so the Trade
# Machine can recompute hypothetical protections & swaps with correct
# within-sim correlation.
asset_slot_cur_draws    <- asset_slot_cur[keep_idx, , drop = FALSE]
asset_slot_new_draws    <- asset_slot_new[keep_idx, , drop = FALSE]
asset_raw_cur_draws     <- asset_raw_cur[keep_idx, , drop = FALSE]
asset_raw_new_draws     <- asset_raw_new[keep_idx, , drop = FALSE]
asset_ownslot_cur_draws <- asset_ownslot_cur[keep_idx, , drop = FALSE]
asset_ownslot_new_draws <- asset_ownslot_new[keep_idx, , drop = FALSE]
asset_convey_cur_draws  <- asset_convey_cur[keep_idx, , drop = FALSE]
asset_convey_new_draws  <- asset_convey_new[keep_idx, , drop = FALSE]
team_slot_cur_draws     <- team_slot_cur[keep_idx, , , drop = FALSE]
team_slot_new_draws     <- team_slot_new[keep_idx, , , drop = FALSE]
sim_curve_par_draws     <- sim_curve_par[keep_idx, , drop = FALSE]

display_asset_cur_draws <- build_display_draw_matrix(asset_cur_draws, pick_display_members, pick_display_assets)
display_asset_new_draws <- build_display_draw_matrix(asset_new_draws, pick_display_members, pick_display_assets)
display_convey_cur_draws <- build_display_convey_matrix(asset_convey_cur_draws, pick_display_members, pick_display_assets)
display_convey_new_draws <- build_display_convey_matrix(asset_convey_new_draws, pick_display_members, pick_display_assets)

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
    best_current   = mean(current_best, na.rm = TRUE),
    best_new       = mean(new_best, na.rm = TRUE),
    delta_value    = mean(new_total) - mean(current_total),
    delta_pct      = (mean(new_total) / pmax(mean(current_total), 0.01) - 1) * 100,
    delta_quality  = mean(new_best, na.rm = TRUE) - mean(current_best, na.rm = TRUE),
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

# ---- pick value curve for the dashboard (posterior mean +/- player-level sd) ----
# Use generated quantities from Stan rather than reconstructing the curve from
# scalar parameters. This keeps the dashboard stable if the Stan model changes
# its internal mean/variance parameterization.
pick_curve <- tibble(
  pick = 1:30,
  expected_war = colMeans(pick_mu_draws),
  war_sd = colMeans(pick_sd_draws)
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
    tau_log_sigma_rw = round(mean(pick_draws$tau_log_sigma_rw), 4),
    nu               = round(mean(pick_draws$nu), 2),
    sigma_pick_1     = round(mean(pick_sd_draws[, 1]), 2),
    sigma_pick_5     = round(mean(pick_sd_draws[, 5]), 2),
    sigma_pick_10    = round(mean(pick_sd_draws[, 10]), 2),
    sigma_pick_30    = round(mean(pick_sd_draws[, 30]), 2),
    n_players   = nrow(pick_fit_data),
    max_rhat    = round(max(pick_diag$rhat, na.rm = TRUE), 4),
    min_ess     = round(min(pick_diag$ess_bulk, na.rm = TRUE)),
    ppc_cover   = round(mean(ppc_tbl$covered), 3),
    ppc_level   = "player rows",
    loo_best_model = pick_loo_compare_tbl$model[1],
    loo_compare = pick_loo_compare_tbl,
    loo_summary = pick_loo_summary,
    curve_type  = "Bayesian Student-t player-level Stan with adjacent-pick sigma smoothing"
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
  complex_future_groups = complex_future_groups,
  complex_future_assets = complex_future_assets,
  owned_future      = owned_future,
  roster_info       = roster_info,
  pick_assets       = pick_assets,
  pick_value_summary = pick_value_summary,
  pick_display_assets = pick_display_assets,
  pick_display_members = pick_display_members,
  pick_display_value_summary = pick_display_value_summary,
  asset_cur_draws   = asset_cur_draws,
  asset_new_draws   = asset_new_draws,
  display_asset_cur_draws = display_asset_cur_draws,
  display_asset_new_draws = display_asset_new_draws,
  display_convey_cur_draws = display_convey_cur_draws,
  display_convey_new_draws = display_convey_new_draws,
  asset_slot_cur_draws    = asset_slot_cur_draws,
  asset_slot_new_draws    = asset_slot_new_draws,
  asset_raw_cur_draws     = asset_raw_cur_draws,
  asset_raw_new_draws     = asset_raw_new_draws,
  asset_ownslot_cur_draws = asset_ownslot_cur_draws,
  asset_ownslot_new_draws = asset_ownslot_new_draws,
  asset_convey_cur_draws  = asset_convey_cur_draws,
  asset_convey_new_draws  = asset_convey_new_draws,
  team_slot_cur_draws     = team_slot_cur_draws,
  team_slot_new_draws     = team_slot_new_draws,
  sim_curve_par_draws     = sim_curve_par_draws,
  proj_years              = proj_years,
  stan_diagnostics  = stan_diagnostics,
  metadata = list(
    n_sims      = N_SIMS,
    n_lottery   = N_LOT,
    draft_years = sprintf("2026 actual + %d-%d projected",
                          FIRST_PROJECTED_DRAFT, LAST_PROJECTED_DRAFT),
    system_note = "2026 actual results; 3-2-1 effective 2027-2029",
    model_note  = "Bayesian 5-tier Markov chain + player-level Student-t 4-yr Win Shares pick curve with adjacent-pick variance smoothing",
    tiers       = TIERS,
    n_picks     = nrow(owned_future) + nrow(actual_2026_order),
    timestamp   = Sys.time()
  )
)

saveRDS(dashboard_data, "01_data/dashboard_data.rds")

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


