## ═════════════════════════════════════════════════════════════════════════════
# NBA Draft Lottery Rule Change Impact Model
#
#   1. Lottery system is now the APPROVED 3-2-1 format (NBA BOG, May 2026),
#      effective 2027-2029. 2026 used the OLD system and its results are now
#      FINAL, so 2026 pick values are locked to the actual draft slots.
#   2. Markov chain states are the FIVE 3-2-1 tiers (relegation / non-play-in /
#      9-10 seed / 7v8 play-in loser / playoff), not generic standings buckets.
#   3. Pick value = TOTAL POINTS IMPACT (TPI) over a player's FIRST 4 SEASONS
#      (rookie deal):  TPI = (xRAPM / 100) x possessions, summed over the
#      four calendar seasons after the draft (a fully missed season = 0).
#      xRAPM scraped from xrapm.com per season (1996-97 .. 2025-26);
#      possessions from hoopR::nba_leaguedashplayerstats (Advanced Totals);
#      draft slots 1-60 (BOTH rounds) from hoopR draft history + bbref.
#   4. New anti-tank pick RESTRICTIONS are modeled: no team may receive the #1
#      pick in consecutive years or a top-5 pick three years running (applies
#      to the originally-owning team, looking back to 2025); traded picks can
#      no longer be protected in the 12-15 band.
#   5. SECOND ROUND is in scope. Under 3-2-1, picks 31-46 are the INVERSE of
#      the final first-round lottery order (lottery winner picks 46th; the
#      team that drew lottery slot 16 picks 31st), with 47-60 going to the 14
#      playoff teams worst-to-best. Under the old system, round 2 is pure
#      reverse record order. Second-round ownership is scraped from RealGM
#      with a hardcoded fallback ledger.
#   6. Future pick ownership refreshed from RealGM / prosportstransactions and
#      the post-lottery 2026 order.
#
# Pipeline:
#   1. Scrape standings (hoopR -> bbref fallback), rosters, xRAPM,
#      possessions, draft production (both rounds)
#   2. Build 5-tier Markov transition counts from history
#   3. EDA on TPI4 by pick slot (REVIEW BEFORE TRUSTING THE STAN PRIORS),
#      then fit Stan models (pick value v4 + Markov) and validate them
#   4. Monte Carlo: project tiers forward, run BOTH lotteries, assign BOTH
#      rounds, value every owned pick under each system, applying
#      protections / swaps / new rules
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

# ---- xRAPM / TPI pick-value settings ----
XRAPM_FIRST_SEASON_END <- 1997    # xRAPM coverage: 1996-97 .. 2025-26
XRAPM_LAST_SEASON_END  <- 2026    # current season page = xRAPM.html
DRAFT_YEARS_FIT        <- 1996:2022  # classes whose 4-season rookie window
                                     # (draft+1 .. draft+4) lies fully inside
                                     # the 1997-2026 xRAPM/possession coverage
UDFA_FLAG_DRAFTS       <- 1978:2025  # wider draft-name sweep used ONLY to flag
                                     # undrafted players active 1997-2026
N_PICKS_R1             <- 30L     # first-round slots in the modern era
N_PICKS_TOTAL          <- 60L     # both rounds; the pick-value curve, slot
                                  # stores, and Stan model all run 1..60
USE_R2_OFFSET_PRODUCTION <- TRUE  # production curve includes the round-2
                                  # level shift (delta_r2); both variants are
                                  # fit and LOO-compared either way

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
## 03 - SCRAPE ROSTER AGE & CONTINUITY -----------------------------------------
## ═════════════════════════════════════════════════════════════════════════════
# Kept for descriptive context in the dashboard. These are no longer model
# inputs (the Markov chain learns persistence directly from tier history), but
# they're cheap to collect and useful color.

scrape_roster_info <- function(abbr,
                               season = HISTORY_END,
                               delay  = 3) {
  bbref_abbr <- case_when(abbr == "BKN" ~ "BRK",
                          abbr == "CHA" ~ "CHO",
                          abbr == "PHX" ~ "PHO",
                          TRUE          ~ abbr)
  url <- sprintf("https://www.basketball-reference.com/teams/%s/%d.html",
                 bbref_abbr, season)
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
      mutate(Age        = as.numeric(Age),
             G          = as.numeric(G),
             MP         = as.numeric(MP),
             age_weight = G * MP) %>%
      filter(!is.na(Age), !is.na(age_weight), age_weight > 0) %>%
      summarise(a = weighted.mean(Age, w = age_weight)) %>%
      pull(a)
  } else {
    NA_real_
  }

  get_names <- function(yr) {
    u <- sprintf("https://www.basketball-reference.com/teams/%s/%d.html",
                 bbref_abbr, yr)
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


roster_info <- tryCatch(map_dfr(all_teams, ~scrape_roster_info(.x, delay = 3)),
                        error = function(e) tibble(abbr = all_teams,
                                                   avg_age = NA_real_,
                                                   continuity = NA_real_)) %>%
  mutate(avg_age    = coalesce(avg_age, 26.5),
         continuity = coalesce(continuity, 0.55))

cat(sprintf("  Mean age %.1f, mean continuity %.2f\n",
            mean(roster_info$avg_age), mean(roster_info$continuity)))


## ═════════════════════════════════════════════════════════════════════════════
## 04 - DRAFT PRODUCTION CURVE (xRAPM TOTAL POINTS IMPACT) ----------------------
## ═════════════════════════════════════════════════════════════════════════════
# Pick value = TOTAL POINTS IMPACT over a player's FIRST 4 SEASONS after the
# draft (rookie-scale window):
#
#   TPI[player, season] = (xRAPM / 100) * possessions played
#   TPI4[player]        = sum of TPI over seasons draft_year+1 .. draft_year+4
#
# The window is CALENDAR-ANCHORED: a player drafted in 2016 who missed one of
# 2016-17 .. 2019-20 entirely (injury, G-League stint, out of the league)
# contributes 0 possessions and 0 TPI for that season — the four-season clock
# does not pause. Players who appear in the xRAPM / possession data but in NO
# draft scrape are treated as UNDRAFTED (e.g. Alex Caruso) and are excluded
# from the slot curve.
#
# Sources:
#   * xRAPM       : xrapm.com per-season table pages, 1996-97 .. 2025-26
#                   (current season at table_pages/xRAPM.html; historical at
#                    table_pages/xRAPM_<season_end_year>.html)
#   * possessions : hoopR::nba_leaguedashplayerstats, Advanced / Totals (POSS),
#                   one call per season with the season appended to each scrape
#   * draft slots : hoopR::nba_drafthistory (gives PERSON_ID -> clean join to
#                   the possession data) for picks 1-60 — BOTH rounds — with a
#                   bbref draft-page fallback (name-based join)

season_str_from_end <- function(end_year) {
  sprintf("%d-%02d", end_year - 1L, end_year %% 100L)
}

# Name normalization for the xRAPM <-> NBA-stats join (the only name-based
# join left in the pipeline; possessions join on PERSON_ID). Handles
# diacritics (Doncic), punctuation (A.J. vs AJ, O'Neal), and suffixes.
normalize_player_name <- function(x) {
  x <- as.character(x)
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }
  x <- tolower(x)
  x <- str_replace_all(x, "[\\.\\'\\-\\u2019]", "")
  x <- str_remove(x, "\\s+(jr|sr|ii|iii|iv)\\.?$")
  str_squish(x)
}

# Manual alias map: extend this when the join diagnostics printed below flag
# high-possession player-seasons with no xRAPM match (different registered
# names across sources). Applied AFTER normalize_player_name on both sides.
manual_name_map <- tribble(
  ~from_norm,            ~to_norm,
  "nene hilario",        "nene",
  "maxi kleber",         "maximilian kleber",
  "alex sarr",           "alexandre sarr",
  "bub carrington",      "carlton carrington"
)
apply_manual_map <- function(x) {
  i <- match(x, manual_name_map$from_norm)
  ifelse(is.na(i), x, manual_name_map$to_norm[i])
}

# ---- xRAPM scraper -----------------------------------------------------------
XRAPM_BASE <- "https://xrapm.com/table_pages"


scrape_xrapm_season <- function(season_end_year, delay = 2.5, verbose = FALSE) {
  url <- if (season_end_year >= XRAPM_LAST_SEASON_END) {
    sprintf("%s/xRAPM.html", XRAPM_BASE)                      # current season
  } else {
    sprintf("%s/xRAPM_%d.html", XRAPM_BASE, season_end_year)  # historical
  }
  
  cat(sprintf("  [xrapm] season %d-%d\n", season_end_year - 1, season_end_year))
  Sys.sleep(delay + runif(1, 0, 1))
  
  empty_xrapm_tbl <- function() {
    tibble(
      season_end  = integer(),
      player_raw  = character(),
      team        = character(),
      xrapm       = numeric(),
      xrapm_pct   = integer(),
      o_xrapm     = numeric(),
      o_xrapm_pct = integer(),
      d_xrapm     = numeric(),
      d_xrapm_pct = integer()
    )
  }
  
  clean_xrapm_metric <- function(x) {
    x <- as.character(x) %>%
      stringr::str_replace_all("\u2212", "-") %>%
      stringr::str_replace_all("&minus;", "-") %>%
      stringr::str_squish()
    
    tibble(
      value = suppressWarnings(
        as.numeric(stringr::str_extract(x, "^[+-]?(?:\\d+\\.\\d+|\\d+|\\.\\d+)"))
      ),
      percentile = suppressWarnings(
        as.integer(stringr::str_match(x, "\\((\\d{1,3})\\)")[, 2])
      )
    )
  }
  
  clean_player_name_raw <- function(x) {
    x %>%
      as.character() %>%
      stringr::str_replace_all("\\[([^\\]]+)\\]\\([^\\)]+\\)", "\\1") %>%
      stringr::str_replace_all("^#+\\s*", "") %>%
      stringr::str_replace_all("^\\d+\\s+", "") %>%
      stringr::str_squish()
  }
  
  resp <- tryCatch(
    httr::RETRY(
      verb = "GET",
      url = url,
      times = 3,
      pause_min = 2,
      pause_cap = 8,
      httr::user_agent(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/125 Safari/537.36"
      ),
      httr::timeout(30)
    ),
    error = function(e) NULL
  )
  
  if (is.null(resp) || httr::status_code(resp) >= 400) {
    warning(sprintf("xRAPM scrape failed for season ending %d", season_end_year))
    return(empty_xrapm_tbl())
  }
  
  html_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  page <- tryCatch(
    xml2::read_html(html_txt, options = "HUGE"),
    error = function(e) NULL
  )
  
  if (is.null(page)) {
    warning(sprintf("xRAPM: could not parse HTML for season ending %d", season_end_year))
    return(empty_xrapm_tbl())
  }
  
  table_node <- page %>%
    rvest::html_element("table")
  
  if (length(table_node) == 0 || is.na(table_node)) {
    warning(sprintf("xRAPM: no table found for season ending %d (%s)", season_end_year, url))
    return(empty_xrapm_tbl())
  }
  
  headers <- table_node %>%
    rvest::html_elements("th") %>%
    rvest::html_text2() %>%
    stringr::str_squish()
  
  cells <- table_node %>%
    rvest::html_elements("tbody td") %>%
    rvest::html_text2() %>%
    stringr::str_squish()
  
  n_cols <- length(headers)
  
  if (n_cols == 0) {
    warning(sprintf("xRAPM: no table headers found for season ending %d (%s)", season_end_year, url))
    return(empty_xrapm_tbl())
  }
  
  if (length(cells) == 0) {
    warning(sprintf("xRAPM: no table body cells found for season ending %d (%s)", season_end_year, url))
    return(empty_xrapm_tbl())
  }
  
  if (length(cells) %% n_cols != 0) {
    warning(sprintf(
      "xRAPM: cell count not divisible by header count for season ending %d (%s): headers=%d, cells=%d",
      season_end_year, url, n_cols, length(cells)
    ))
    return(empty_xrapm_tbl())
  }
  
  raw_tbl <- matrix(cells, ncol = n_cols, byrow = TRUE) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(headers)
  
  # Current season has:
  #   Player | Team | Offense | Defense(*) | Total
  #
  # Historical seasons have:
  #   Player | Offense | Defense(*) | Total
  nm <- names(raw_tbl)
  
  player_col <- nm[nm == "Player"][1]
  team_col   <- nm[nm == "Team"][1]
  off_col    <- nm[nm == "Offense"][1]
  def_col    <- nm[nm %in% c("Defense(*)", "Defense", "Defense *")][1]
  total_col  <- nm[nm == "Total"][1]
  
  if (is.na(player_col) || is.na(off_col) || is.na(def_col) || is.na(total_col)) {
    warning(sprintf(
      "xRAPM: required columns not found for season ending %d (%s). Headers found: %s",
      season_end_year, url, paste(nm, collapse = ", ")
    ))
    return(empty_xrapm_tbl())
  }
  
  off_metric   <- clean_xrapm_metric(raw_tbl[[off_col]])
  def_metric   <- clean_xrapm_metric(raw_tbl[[def_col]])
  total_metric <- clean_xrapm_metric(raw_tbl[[total_col]])
  
  out <- raw_tbl %>%
    transmute(
      season_end = as.integer(season_end_year),
      player_raw = clean_player_name_raw(.data[[player_col]]),
      team = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_
    ) %>%
    bind_cols(
      total_metric %>%
        rename(xrapm = value, xrapm_pct = percentile),
      off_metric %>%
        rename(o_xrapm = value, o_xrapm_pct = percentile),
      def_metric %>%
        rename(d_xrapm = value, d_xrapm_pct = percentile)
    ) %>%
    filter(
      !is.na(player_raw),
      !is.na(xrapm),
      stringr::str_detect(player_raw, "[A-Za-z]")
    ) %>%
    distinct(season_end, player_raw, team, .keep_all = TRUE)
  
  if (nrow(out) < 50) {
    warning(sprintf(
      "xRAPM: parsed fewer than 50 rows for season ending %d (%s)",
      season_end_year, url
    ))
    return(empty_xrapm_tbl())
  }
  
  if (verbose) {
    message("    parsed ", nrow(out), " players")
    print(head(out, 5))
  }
  
  out
}

test_2025 <- scrape_xrapm_season(2025, delay = 0, verbose = TRUE)
test_2026 <- scrape_xrapm_season(2026, delay = 0, verbose = TRUE)

print(test_2025 %>% select(season_end, player_raw, team, o_xrapm, d_xrapm, xrapm) %>% head(10))
print(test_2026 %>% select(season_end, player_raw, team, o_xrapm, d_xrapm, xrapm) %>% head(10))



xrapm_cache <- "01_data/xrapm_cache.rds"
if (file.exists(xrapm_cache)) {
  cat("  Using cached xRAPM tables\n")
  xrapm_all <- readRDS(xrapm_cache)
} else {
  cat("\n--- Scraping xRAPM tables (xrapm.com) ---\n")
  xrapm_all <- map_dfr(XRAPM_FIRST_SEASON_END:XRAPM_LAST_SEASON_END,
                       scrape_xrapm_season, verbose = TRUE)
  if (nrow(xrapm_all) > 0) saveRDS(xrapm_all, xrapm_cache)
}

xrapm_all <- xrapm_all %>%
  mutate(player_norm = apply_manual_map(normalize_player_name(player_raw)))

xr_dupes <- xrapm_all %>%
  count(season_end, player_norm) %>%
  filter(n > 1)

if (nrow(xr_dupes) > 0) {
  warning(sprintf("xRAPM: %d duplicated player-season rows averaged", nrow(xr_dupes)))
}

xrapm_all <- xrapm_all %>%
  group_by(season_end, player_norm) %>%
  summarise(
    player_raw  = first(player_raw),
    team        = first(na.omit(team)) %||% NA_character_,
    
    xrapm       = mean(xrapm, na.rm = TRUE),
    o_xrapm     = mean(o_xrapm, na.rm = TRUE),
    d_xrapm     = mean(d_xrapm, na.rm = TRUE),
    
    xrapm_pct   = round(mean(xrapm_pct, na.rm = TRUE)),
    o_xrapm_pct = round(mean(o_xrapm_pct, na.rm = TRUE)),
    d_xrapm_pct = round(mean(d_xrapm_pct, na.rm = TRUE)),
    
    .groups = "drop"
  )

cat(sprintf("  xRAPM: %d player-seasons across %d seasons\n",
            nrow(xrapm_all), n_distinct(xrapm_all$season_end)))

# ---- possessions scraper (hoopR / stats.nba.com) ------------------------------
# One Advanced-Totals call per season; the season is appended to every scrape.
get_possessions_season <- function(season_end_year, delay = 1.5) {
  if (!hoopR_available) return(tibble())
  ss <- season_str_from_end(season_end_year)
  cat(sprintf("  [poss] %s\n", ss))
  Sys.sleep(delay)
  tryCatch({
    res <- hoopR::nba_leaguedashplayerstats(
      season      = ss,
      season_type = "Regular Season",
      measure_type = "Advanced",
      per_mode    = "Totals"
    )
    res$LeagueDashPlayerStats %>%
      as.data.frame() %>%
      select(PLAYER_ID, PLAYER_NAME, POSS) %>%
      transmute(
        season_end  = season_end_year,
        player_id   = as.character(PLAYER_ID),
        player_name = as.character(PLAYER_NAME),
        poss        = suppressWarnings(as.numeric(POSS))
      ) %>%
      filter(!is.na(poss))
  }, error = function(e) {
    warning(sprintf("possessions scrape failed for %s: %s", ss, e$message))
    tibble()
  })
}

poss_cache <- "01_data/possessions_cache.rds"
if (file.exists(poss_cache)) {
  cat("  Using cached possession totals\n")
  poss_all <- readRDS(poss_cache)
} else {
  cat("\n--- Scraping per-season possession totals (hoopR Advanced Totals) ---\n")
  poss_all <- map_dfr(XRAPM_FIRST_SEASON_END:XRAPM_LAST_SEASON_END,
                      get_possessions_season)
  if (nrow(poss_all) > 0) saveRDS(poss_all, poss_cache)
}

poss_all <- poss_all %>%
  mutate(player_norm = apply_manual_map(normalize_player_name(player_name)))

cat(sprintf("  Possessions: %d player-seasons across %d seasons\n",
            nrow(poss_all), n_distinct(poss_all$season_end)))

# ---- player-season TPI ---------------------------------------------------------
# TPI = (xRAPM / 100) * possessions. Possession rows are the spine (PERSON_ID
# joins downstream); xRAPM attaches by normalized name within season.
player_season_tpi <- poss_all %>%
  left_join(xrapm_all %>% select(season_end, player_norm, xrapm),
            by = c("season_end", "player_norm")) %>%
  mutate(tpi = (xrapm / 100) * poss)

xr_match_rate <- mean(!is.na(player_season_tpi$xrapm))
cat(sprintf("  xRAPM <-> possessions name-match rate: %.1f%% of player-seasons\n",
            100 * xr_match_rate))
unmatched_heavy <- player_season_tpi %>%
  filter(is.na(xrapm), poss > 1000) %>%
  arrange(desc(poss)) %>%
  select(season_end, player_name, poss)
if (nrow(unmatched_heavy) > 0) {
  cat(sprintf("  WARNING: %d player-seasons with >1000 poss lack an xRAPM match.\n",
              nrow(unmatched_heavy)))
  cat("  Extend manual_name_map for the worst offenders below (they currently\n")
  cat("  contribute 0 TPI for those seasons):\n")
  print(head(unmatched_heavy, 12))
}

# ---- draft slots, BOTH ROUNDS (picks 1-60) ------------------------------------
# Primary: hoopR draft history (PERSON_ID enables an ID join to possessions).
scrape_draft_class_hoopR <- function(draft_year) {
  if (!hoopR_available) return(NULL)
  tryCatch({
    dh <- hoopR::nba_drafthistory(season = draft_year)
    tbl <- dh[["DraftHistory"]]
    tbl %>%
      transmute(
        draft_year = draft_year,
        pick       = suppressWarnings(as.integer(.data$OVERALL_PICK)),
        round      = {
          rn <- suppressWarnings(as.integer(.data$ROUND_NUMBER))
          ifelse(!is.na(rn), ifelse(rn >= 2L, "R2", "R1"),
                 ifelse(suppressWarnings(as.integer(.data$OVERALL_PICK)) > 30L,
                        "R2", "R1"))
        },
        player     = as.character(.data$PLAYER_NAME),
        person_id  = as.character(.data$PERSON_ID)
      ) %>%
      filter(!is.na(pick), pick >= 1, pick <= N_PICKS_TOTAL)
  }, error = function(e) NULL)
}

# bbref FALLBACK (used only when hoopR draft history fails): slot -> player
# name map for BOTH rounds. bbref has no NBA-stats PERSON_ID, so rows recovered
# this way join to the possession spine by normalized name instead.
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
              round      = ifelse(pick > 30L, "R2", "R1"),
              player     = .data[[player_col]]) %>%
    filter(!is.na(pick), pick >= 1, pick <= N_PICKS_TOTAL)

  # bbref href ids are bbref-specific (not NBA-stats PERSON_IDs); keep them for
  # reference but force the downstream join for these rows onto names.
  ids <- tryCatch({
    nodes <- page %>%
      html_element("#div_stats") %>%
      html_elements("td[data-stat='player'] a, td[data-stat='player_name'] a")
    tibble(href   = nodes %>% html_attr("href"),
           player = nodes %>% 
             html_text() %>% 
             str_trim()) %>%
      mutate(bbref_id = str_match(href, "/players/[a-z]/([a-z0-9]+)\\.html")[, 2]) %>%
      filter(!is.na(bbref_id)) %>%
      select(player, bbref_id) %>%
      distinct(player, .keep_all = TRUE)
  }, error = function(e) tibble(player = character(0), bbref_id = character(0)))

  base %>%
    left_join(ids, by = "player") %>%
    mutate(person_id = NA_character_)
}

cat("\n--- Building Draft Production Curve (first-4-year Total Points Impact) ---\n")

# Fit window: drafts 1996-2022. xRAPM coverage starts 1996-97, and 2022 is the
# last class with four complete calendar-anchored seasons by 2025-26.
draft_years_fit <- DRAFT_YEARS_FIT

# STEP A: draft slots for the fit classes, picks 1-60 (BOTH rounds), cached.
# hoopR draft history is primary (PERSON_ID -> clean ID join to possessions);
# bbref is the fallback (name join only).
slot_cache <- "01_data/draft_slots_cache_r1r2.rds"
if (file.exists(slot_cache)) {
  cat("  Using cached draft-slot map (rounds 1-2)\n")
  draft_slots <- readRDS(slot_cache)
} else {
  cat("  Scraping draft classes (hoopR draft history, bbref fallback)\n")
  draft_slots <- map_dfr(draft_years_fit, function(yr) {
    out <- scrape_draft_class_hoopR(yr)
    if (is.null(out) || nrow(out) == 0) {
      cat(sprintf("  [fallback->bbref] draft %d\n", yr))
      out <- scrape_draft_slots_bbref(yr)
      if (!is.null(out)) {
        out <- out %>% select(draft_year, pick, round, player, person_id)
      }
    }
    out
  })
  if (!is.null(draft_slots) && nrow(draft_slots) > 0) {
    saveRDS(draft_slots, slot_cache)
  }
}

draft_slots <- draft_slots %>%
  mutate(player_norm = apply_manual_map(normalize_player_name(player))) %>%
  distinct(draft_year, pick, .keep_all = TRUE)

cat(sprintf("  Draft slots: %d picks across %d classes (R1: %d, R2: %d)\n",
            nrow(draft_slots), n_distinct(draft_slots$draft_year),
            sum(draft_slots$round == "R1"), sum(draft_slots$round == "R2")))

# STEP B: undrafted-player flag. Any player who appears NOWHERE in the
# historical draft scrape is treated as undrafted (e.g. Alex Caruso) and is
# EXCLUDED from the pick-value curve — UDFAs carry no draft slot, so their
# production must not contaminate slot 31-60 estimates. We sweep a much wider
# set of draft classes than the fit window so veterans drafted before 1996
# (still active in early xRAPM seasons) are not misflagged as UDFAs.
udfa_cache <- "01_data/draft_names_all_cache.rds"
if (file.exists(udfa_cache)) {
  cat("  Using cached all-draft name sweep (UDFA flagging)\n")
  drafted_names_all <- readRDS(udfa_cache)
} else {
  cat("  Sweeping all draft classes for UDFA flagging (this is cached)\n")
  drafted_names_all <- map_dfr(UDFA_FLAG_DRAFTS, function(yr) {
    out <- scrape_draft_class_hoopR(yr)
    if (is.null(out) || nrow(out) == 0) out <- scrape_draft_slots_bbref(yr)
    if (is.null(out)) return(tibble())
    out %>% transmute(draft_year,
                      person_id = if ("person_id" %in% names(.)) {
                        as.character(person_id)
                      } else NA_character_,
                      player_norm = apply_manual_map(
                        normalize_player_name(player)))
  })
  if (nrow(drafted_names_all) > 0) saveRDS(drafted_names_all, udfa_cache)
}

undrafted_flags <- poss_all %>%
  distinct(player_id, player_norm) %>%
  mutate(undrafted = !(player_id %in% drafted_names_all$person_id |
                         player_norm %in% drafted_names_all$player_norm))
cat(sprintf("  Flagged %d of %d NBA players as undrafted (excluded from curve)\n",
            sum(undrafted_flags$undrafted), nrow(undrafted_flags)))

# STEP C: first-4-year TPI per drafted player, CALENDAR-ANCHORED.
# The window is the four league seasons ending draft_year+1 .. draft_year+4.
# A player drafted in 2016 who misses 2018-19 entirely (injury, G-League,
# out of the league) contributes 0 possessions x 0 xRAPM = 0 TPI for that
# season — the join below leaves the row unmatched and coalesce() zeroes it.
draft_tpi4 <- NULL
if (nrow(player_season_tpi) > 0 && nrow(draft_slots) > 0) {

  window_grid <- draft_slots %>%
    tidyr::expand_grid(season_idx = 1:4) %>%
    mutate(season_end = draft_year + season_idx) %>%
    filter(season_end <= XRAPM_LAST_SEASON_END)

  tpi_by_id <- player_season_tpi %>%
    filter(!is.na(player_id)) %>%
    select(season_end, player_id, tpi_id = tpi, poss_id = poss)
  tpi_by_name <- player_season_tpi %>%
    select(season_end, player_norm, tpi_nm = tpi, poss_nm = poss) %>%
    group_by(season_end, player_norm) %>%
    summarise(tpi_nm = sum(tpi_nm, na.rm = TRUE),
              poss_nm = sum(poss_nm, na.rm = TRUE), .groups = "drop")

  draft_tpi4 <- window_grid %>%
    left_join(tpi_by_id,   by = c("person_id" = "player_id", "season_end")) %>%
    left_join(tpi_by_name, by = c("player_norm", "season_end")) %>%
    mutate(
      tpi_season  = coalesce(tpi_id, tpi_nm, 0),
      poss_season = coalesce(poss_id, poss_nm, 0)
    ) %>%
    group_by(draft_year, pick, round, player, person_id) %>%
    summarise(
      tpi4           = sum(tpi_season),
      poss4          = sum(poss_season),
      seasons_played = sum(poss_season > 0),
      .groups        = "drop"
    ) %>%
    filter(!is.na(pick), pick >= 1, pick <= N_PICKS_TOTAL)
  # NOTE: never-played picks stay in the fit at tpi4 = 0 — a drafted player
  # who never logs an NBA possession IS the realized value of that slot.

  cat(sprintf("  Computed first-4-year TPI for %d drafted players\n",
              nrow(draft_tpi4)))
  cat(sprintf("    never played: %d | negative TPI4: %d | TPI4 > +150: %d\n",
              sum(draft_tpi4$seasons_played == 0),
              sum(draft_tpi4$tpi4 < 0),
              sum(draft_tpi4$tpi4 > 150)))
}

# STEP D: EDA — RUN AND REVIEW BEFORE TRUSTING THE STAN PRIORS.
# TPI lives on a different scale than WS (stars ~ +150..+400, busts ~0 or
# meaningfully NEGATIVE for heavy-minutes bad players). The v4 priors below
# were elicited from this picture; re-elicit if these plots look different.
if (!is.null(draft_tpi4) && nrow(draft_tpi4) > 50) {
  dir.create("03_eda", showWarnings = FALSE)

  p_scatter <- ggplot(draft_tpi4, aes(pick, tpi4)) +
    geom_point(alpha = 0.25, size = 0.9) +
    geom_smooth(method = "loess", span = 0.4, se = TRUE,
                color = "#c0392b", linewidth = 0.8) +
    geom_vline(xintercept = 30.5, linetype = "dashed", color = "grey40") +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    annotate("text", x = 31.5, y = max(draft_tpi4$tpi4) * 0.95,
             label = "round 2", hjust = 0, size = 3, color = "grey40") +
    labs(title    = "First-4-year Total Points Impact by draft slot",
         subtitle = sprintf("Drafts %d-%d | TPI = (xRAPM/100) x possessions, summed over 4 seasons",
                            min(draft_years_fit), max(draft_years_fit)),
         x = "Pick", y = "TPI (first 4 seasons)") +
    theme_minimal()
  ggsave("03_eda/tpi4_vs_pick_scatter.png", p_scatter,
         width = 10, height = 6, dpi = 150)

  band_levels <- c("1-5", "6-10", "11-15", "16-20", "21-30", "31-45", "46-60")
  eda_bands <- draft_tpi4 %>%
    mutate(band = factor(case_when(
      pick <=  5 ~ "1-5",   pick <= 10 ~ "6-10",  pick <= 15 ~ "11-15",
      pick <= 20 ~ "16-20", pick <= 30 ~ "21-30", pick <= 45 ~ "31-45",
      TRUE       ~ "46-60"), levels = band_levels))

  p_hist <- ggplot(eda_bands, aes(tpi4)) +
    geom_histogram(bins = 40, fill = "#2c3e50") +
    geom_vline(xintercept = 0, color = "#c0392b", linewidth = 0.3) +
    facet_wrap(~band, scales = "free_y", ncol = 2) +
    labs(title = "TPI4 distribution by pick band",
         subtitle = "Note the negative mass — unlike WS, TPI is not floored near 0",
         x = "TPI (first 4 seasons)", y = "Players") +
    theme_minimal()
  ggsave("03_eda/tpi4_hist_by_band.png", p_hist,
         width = 9, height = 9, dpi = 150)

  eda_band_table <- eda_bands %>%
    group_by(band) %>%
    summarise(n = n(), mean = mean(tpi4), median = median(tpi4),
              sd = sd(tpi4), p10 = quantile(tpi4, 0.10),
              p90 = quantile(tpi4, 0.90), pct_neg = mean(tpi4 < 0),
              .groups = "drop")
  write.csv(eda_band_table, "03_eda/tpi4_band_summary.csv", row.names = FALSE)

  cat("\n  ── TPI4 by pick band ───────────────────────────────────────────\n")
  print(as.data.frame(eda_band_table), digits = 3)
  cat("  ────────────────────────────────────────────────────────────────\n")
  cat("  >>> EDA saved to 03_eda/. REVIEW the scatter + band histograms\n")
  cat("  >>> before trusting the v4 Stan priors (elicited from this view).\n")
  cat("  >>> Check esp. the pick-30 boundary: the r2-offset model vs the\n")
  cat("  >>> unified curve is adjudicated by LOO in section 08.\n\n")
}

# STEP E: per-slot curve inputs (pick 1-60) and the bootstrap pool.
if (!is.null(draft_tpi4) && nrow(draft_tpi4) > 50) {
  pick_slot_data <- draft_tpi4 %>%
    group_by(pick) %>%
    summarise(
      tpi_mean = mean(tpi4),
      tpi_sd   = sd(tpi4),
      n_obs    = n(),
      .groups  = "drop"
    ) %>%
    arrange(pick) %>%
    mutate(
      tpi_sd = coalesce(tpi_sd, 60),
      n_obs  = pmax(n_obs, 1L)
    )
  pick_boot_pool <- draft_tpi4 %>%
    select(pick, tpi4)
  cat(sprintf("  Built slot curve from %d drafted players (first-4-yr TPI)\n",
              nrow(draft_tpi4)))
} else {
  cat("  Insufficient joined data — using compiled first-4-year TPI curve\n")
  # Compiled fallback on the TPI scale: ~ +290 at pick 1, ~ +35 by pick 14,
  # near 0 by pick 30, slightly negative-to-flat through round 2.
  pick_slot_data <- tibble(pick = 1:60) %>%
    mutate(
      tpi_mean = 320 / pick^0.85 - 15,
      tpi_sd   = 150 * exp(-pick / 40) + 40,
      n_obs    = rep(25L, 60)
    )
  pick_boot_pool <- pick_slot_data %>%
    rowwise() %>%
    mutate(draws = list(rnorm(n_obs, tpi_mean, tpi_sd))) %>%
    unnest(draws) %>%
    transmute(pick, tpi4 = draws) %>%
    ungroup()
}





## ═════════════════════════════════════════════════════════════════════════════
## 05 - 2026 ACTUAL DRAFT ORDER ------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════
# The 2026 lottery is FINAL (Wizards won). We hardcode the actual first-round
# slot of every team's pick from the official post-lottery order so 2026 pick
# values reflect reality instead of being re-simulated. Each row is the owner
# of that slot and the team whose pick it originally was ("via").
#
# Source: NBA.com / ESPN official 2026 first-round order (post-lottery).

actual_2026_order <- tribble(~slot, ~owner, ~original_team,
                             1,    "WAS", "WAS",
                             2,    "UTA", "UTA",
                             3,    "MEM", "MEM",
                             4,    "CHI", "CHI",
                             5,    "LAC", "IND",   # Zubac trade
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
                            30,    "DAL", "OKC")    # via Oklahoma City

# ── 2026 SECOND-ROUND ORDER (slots 31-60) ────────────────────────────────────
# The 2026 draft PRE-DATES the 3-2-1 inversion (first applies with the new
# lottery), so round 2 still follows the legacy rule: reverse regular-season
# record, worst team picks 31. We derive the base order from final standings.
#
# NOTE: the official R2 order also applies coin-flip/random tiebreaks that flip
# the ORDER relative to round 1 for tied teams. Those flips are not recoverable
# from W-L records alone — VERIFY slots against the official release and patch
# `actual_2026_r2_base` if any tied pair is transposed.
actual_2026_r2_base <- current_standings %>%
  arrange(win_pct) %>%                       # worst record first
  transmute(slot = 30L + row_number(),
            original_team = abbr)

# Traded 2026 second-rounders: owner != original team. The base order above
# assigns every R2 slot to its original team; this ledger reassigns the slots
# that have changed hands. PLACEHOLDER — populate/VERIFY from RealGM's 2026
# draft page; entries here follow the same shape as actual_2026_order.
traded_2026_r2 <- tribble(
  ~original_team, ~owner,
  # "PHX",        "WAS",   # example shape — VERIFY before relying on 2026 R2
)

actual_2026_r2_order <- actual_2026_r2_base %>%
  left_join(traded_2026_r2, by = "original_team") %>%
  mutate(owner = coalesce(owner, original_team)) %>%
  select(slot, owner, original_team)

stopifnot(nrow(actual_2026_r2_order) == 30L,
          !anyDuplicated(actual_2026_r2_order$slot))





## ═════════════════════════════════════════════════════════════════════════════
## 06 - FUTURE PICK OWNERSHIP 2027-2032 (RealGM / prosportstransactions) --------
## ═════════════════════════════════════════════════════════════════════════════
# Attempt a live scrape; on failure use the hardcoded table compiled from
# RealGM "future drafts", prosportstransactions, and team beat reporting as of
# the 2026 lottery. Protections are evaluated during simulation; swaps are
# tagged so the owner takes the more favorable slot.
#
# NEW-RULE NOTE: under the approved system, picks may NOT be protected in the
# 12-15 band. None of the encoded protections fall in that band; the helper
# below also hard-blocks any such protection if added later.



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
  # second-round protection levels (overall pick number, 31-60 scale)
  if (protection == "top45")  return(pos > 45)
  if (protection == "top55")  return(pos > 55)
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
complex_future_groups <- tribble(~year, ~group_id, ~notes,
  2027, "MIL_NOP_ATL",              "More favorable of MIL/NOP to NOP; other to ATL if 5-30; both to NOP if both 1-4",
  2027, "CLE_MIN_UTA_MEM_UTA_PHX",  "Most favorable CLE/MIN/UTA to MEM; second to UTA; least to PHX",
  2027, "SAS_SAC_OKC",              "SAN 1-16 to SAC; SAN 17-30 to OKC",
  2027, "OKC_DEN_LAC",              "Two most/more favorable of OKC, DEN 6-30, LAC to OKC; other to LAC",
  2028, "ATL_CLE_UTA",              "ATL/CLE/UTA ranked swap pool",
  2028, "BOS_SAS",                  "SAS may swap for BOS if BOS 2-30",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL",  "Nested BKN/PHI/PHX/NYK/WAS/MIL ranked swap pool",
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
  tidyr::expand_grid(owner = possible_owners,
                     original_team = original_teams) %>%
    filter(owner != original_team) %>%
    transmute(owner,
              original_team,
              year = as.integer(year),
              protection = "complex",
              pick_type = "complex",
              notes = notes,
              complex_group = group_id)
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
  transmute(owner = original_team,
            original_team = owner,
            year,
            protection = "none",
            pick_type = "swap_return",
            notes = sprintf("Return pick if %s exercises swap with %s", 
                            .data$original_team, .data$owner),
            complex_group = NA_character_)

# ── FUTURE SECOND-ROUND OBLIGATIONS (2027-2032) ──────────────────────────────
# Primary: scrape RealGM future-drafts (yearly view lists each team's First
# Round / Second Round entries; outgoing picks read "To XXX; ..."). RealGM
# rate-limits aggressively, so the scrape is cached and falls back to the
# hardcoded ledger below on failure.
#
# SIMPLIFICATION (documented): future R2 obligations are modeled as simple
# OUTRIGHT transfers from original_team -> owner. Real R2 ledgers contain
# more-/less-favorable pools and conditional routes; collapsing them to the
# most likely single route keeps the allocator simple at small cost — R2 slot
# values are nearly flat (see EDA), so pool-vs-outright resolution moves
# expected value by very little.

realgm_abbr_map <- c(
  "ATL"="ATL","BOS"="BOS","BRK"="BKN","CHA"="CHA","CHI"="CHI","CLE"="CLE",
  "DAL"="DAL","DEN"="DEN","DET"="DET","GOS"="GSW","HOU"="HOU","IND"="IND",
  "LAC"="LAC","LAL"="LAL","MEM"="MEM","MIA"="MIA","MIL"="MIL","MIN"="MIN",
  "NOR"="NOP","NOP"="NOP","NYK"="NYK","OKC"="OKC","ORL"="ORL","PHL"="PHI",
  "PHI"="PHI","PHO"="PHX","PHX"="PHX","POR"="POR","SAC"="SAC","SAN"="SAS",
  "SAS"="SAS","TOR"="TOR","UTH"="UTA","UTA"="UTA","WAS"="WAS"
)

scrape_realgm_future_r2 <- function(delay = 4) {
  url <- "https://basketball.realgm.com/nba/draft/future_drafts/yearly"
  cat("  [realgm] future second-round obligations (yearly view)\n")
  Sys.sleep(delay)
  page <- tryCatch({
    resp <- httr::RETRY("GET", url, times = 3, pause_min = 3, pause_cap = 10,
                        httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"),
                        httr::timeout(30))
    if (httr::status_code(resp) >= 400) return(NULL)
    xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  }, error = function(e) NULL)
  if (is.null(page)) return(NULL)

  out <- tryCatch({
    # Yearly view: one section per draft year, tables with columns
    # Team | First Round | Second Round.
    headers <- page %>% rvest::html_elements("h2") %>% rvest::html_text()
    tables  <- page %>% rvest::html_elements("table") %>%
      rvest::html_table(fill = TRUE)
    years <- suppressWarnings(as.integer(str_extract(headers, "20\\d{2}")))

    purrr::map2_dfr(tables, years[seq_along(tables)], function(tbl, yr) {
      if (is.na(yr) || !yr %in% FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT) {
        return(tibble())
      }
      tbl <- janitor::clean_names(tbl)
      team_col <- names(tbl)[str_detect(names(tbl), "team")][1]
      r2_col   <- names(tbl)[str_detect(names(tbl), "second")][1]
      if (is.na(team_col) || is.na(r2_col)) return(tibble())
      tbl %>%
        transmute(original_team_raw = as.character(.data[[team_col]]),
                  entry = as.character(.data[[r2_col]])) %>%
        filter(str_detect(entry, "To ")) %>%
        mutate(to_abbrs = str_extract_all(entry, "To\\s+([A-Z]{3})") %>%
                 purrr::map(~str_remove(.x, "To\\s+"))) %>%
        unnest(to_abbrs) %>%
        transmute(
          owner = realgm_abbr_map[to_abbrs],
          original_team = realgm_abbr_map[
            str_extract(toupper(original_team_raw), "[A-Z]{3}")],
          year = yr,
          protection = "none",
          pick_type = "outright",
          notes = str_trunc(entry, 110)
        ) %>%
        filter(!is.na(owner), !is.na(original_team), owner != original_team)
    }) %>%
      distinct(owner, original_team, year, .keep_all = TRUE)
  }, error = function(e) NULL)

  if (is.null(out) || nrow(out) < 5) return(NULL)
  out
}

# Hardcoded fallback ledger. *** EVERY ROW BELOW IS A PLACEHOLDER — VERIFY ***
# against RealGM future-drafts before trusting any R2-specific output. These
# encode plausible R2 routings as of the 2026 lottery but were NOT confirmed
# line-by-line. Same shape as traded_future (round column added at build time).
traded_future_r2_fallback <- tribble(
  ~owner, ~original_team, ~year, ~protection, ~pick_type, ~notes,
  "BOS", "MEM", 2027, "none", "outright", "VERIFY — MEM 2027 2nd to BOS",
  "OKC", "ATL", 2027, "none", "outright", "VERIFY — ATL 2027 2nd to OKC",
  "SAS", "TOR", 2027, "none", "outright", "VERIFY — TOR 2027 2nd to SAS",
  "UTA", "DAL", 2027, "none", "outright", "VERIFY — DAL 2027 2nd to UTA",
  "WAS", "CHI", 2027, "none", "outright", "VERIFY — CHI 2027 2nd to WAS",
  "OKC", "HOU", 2028, "none", "outright", "VERIFY — HOU 2028 2nd to OKC",
  "ORL", "BOS", 2028, "none", "outright", "VERIFY — BOS 2028 2nd to ORL",
  "NYK", "GSW", 2028, "none", "outright", "VERIFY — GSW 2028 2nd to NYK",
  "DET", "NOP", 2028, "none", "outright", "VERIFY — NOP 2028 2nd to DET",
  "BKN", "PHI", 2029, "none", "outright", "VERIFY — PHI 2029 2nd to BKN",
  "MEM", "POR", 2029, "none", "outright", "VERIFY — POR 2029 2nd to MEM",
  "SAC", "LAL", 2029, "none", "outright", "VERIFY — LAL 2029 2nd to SAC",
  "CHA", "DEN", 2030, "none", "outright", "VERIFY — DEN 2030 2nd to CHA",
  "IND", "MIA", 2030, "none", "outright", "VERIFY — MIA 2030 2nd to IND",
  "TOR", "MIL", 2031, "none", "outright", "VERIFY — MIL 2031 2nd to TOR"
)

r2_cache <- "01_data/realgm_future_r2_cache.rds"
traded_future_r2 <- NULL
if (file.exists(r2_cache)) {
  cat("  Using cached RealGM future second-round obligations\n")
  traded_future_r2 <- readRDS(r2_cache)
} else {
  traded_future_r2 <- scrape_realgm_future_r2()
  if (!is.null(traded_future_r2)) saveRDS(traded_future_r2, r2_cache)
}
if (is.null(traded_future_r2)) {
  cat("  RealGM R2 scrape unavailable — using HARDCODED fallback ledger.\n")
  cat("  *** All fallback R2 obligations are flagged VERIFY — confirm against\n")
  cat("  *** RealGM before quoting any second-round-specific result.\n")
  traded_future_r2 <- traded_future_r2_fallback
}
traded_future_r2 <- traded_future_r2 %>%
  mutate(complex_group = NA_character_)
cat(sprintf("  Future R2 obligations encoded: %d rows (2027-2032)\n",
            nrow(traded_future_r2)))

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
  own_future_r1 <- tidyr::expand_grid(year = FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT,
                                      original_team = all_teams) %>%
    transmute(owner = original_team,
              original_team,
              year = as.integer(year),
              round = "R1",
              protection = "none",
              pick_type = "own",
              notes = "own / retained pick",
              complex_group = NA_character_)

  # every team also originates a second-round pick each projected year
  own_future_r2 <- own_future_r1 %>%
    mutate(round = "R2",
           notes = "own / retained pick (2nd round)")

  bind_rows(own_future_r1,
            own_future_r2,
            traded_future        %>% mutate(round = "R1"),
            swap_return_assets   %>% mutate(round = "R1"),
            complex_future_assets %>% mutate(round = "R1"),
            traded_future_r2     %>% mutate(round = "R2")) %>%
    distinct(owner, original_team, year, round, pick_type, complex_group,
             .keep_all = TRUE)
}

owned_future <- build_owned_picks()


# PICK-ASSET REGISTRY
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
    round      = "R1",
    protection = "none",
    pick_type  = "outright",
    notes      = ifelse(owner == original_team, "own pick (2026 actual)",
                        sprintf("2026 actual, via %s", original_team)),
    fixed_slot = slot
  )

# 2026 actual SECOND-ROUND assets (slots 31-60; legacy reverse-record order)
assets_2026_r2 <- actual_2026_r2_order %>%
  transmute(
    owner,
    original_team,
    year       = 2026L,
    round      = "R2",
    protection = "none",
    pick_type  = "outright",
    notes      = ifelse(owner == original_team, "own 2nd (2026 actual)",
                        sprintf("2026 actual 2nd, via %s", original_team)),
    fixed_slot = slot
  )

# 2027-2032 assets (projected)
assets_future <- owned_future %>%
  transmute(owner, original_team, year, round, protection, pick_type, notes,
            fixed_slot = NA_integer_, complex_group = complex_group %||% NA_character_)

pick_assets <- bind_rows(assets_2026, assets_2026_r2, assets_future) %>%
  arrange(year, round, owner, original_team) %>%
  mutate(
    complex_group = coalesce(complex_group, NA_character_),
    asset_id = ifelse(
      is.na(complex_group),
      sprintf("%d_%s_%s_from_%s_%s", year, round, owner, original_team, pick_type),
      sprintf("%d_%s_%s_from_%s_%s_%s", year, round, owner, original_team, pick_type, complex_group)
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
    label = ifelse(round == "R2", paste0(label, " [2nd round]"), label),
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


# USER-FACING PICK ENTITLEMENTS (DISPLAY ASSETS)
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
  # Display grouping applies to FIRST-ROUND entitlements only; R2 assets are
  # simple outrights and fall through to the one-to-one path below.
  members <- pick_assets %>%
    filter(.data$round == "R1",
           .data$year == .env$year,
           .data$owner == .env$owner,
           .data$original_team %in% .env$original_teams,
           .data$pick_type %in% .env$pick_types)

  if (!is.null(complex_group_filter)) {
    members <- members %>%
      filter(.data$complex_group == .env$complex_group_filter |
               (.data$pick_type == "own" & .data$original_team %in% .env$original_teams))
  }

  members <- members %>% distinct(asset_id, .keep_all = TRUE)
  if (nrow(members) == 0) return(invisible(NULL))

  pick_display_assets <<- bind_rows(pick_display_assets,
                                    tibble(display_asset_id = display_asset_id,
                                           owner = owner,
                                           year = as.integer(year),
                                           label = label,
                                           obligation = obligation,
                                           notes = notes,
                                           group_type = group_type,
                                           display_group = complex_group_filter %||% display_asset_id,
                                           member_n = nrow(members),
                                           member_original_teams = paste(sort(unique(members$original_team)),
                                                                         collapse = ", ")))

  pick_display_members <<- bind_rows(pick_display_members,
                                     tibble(display_asset_id = display_asset_id,
                                            asset_id = members$asset_id))

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

    add_display_group(display_asset_id = sprintf("display_%d_%s_swap_%s_%s", 
                                                 yr, holder, holder, counter),
                      year = yr,
                      owner = holder,
                      original_teams = pool,
                      label = sprintf("%d %s own or %s (%s)", yr, holder, counter, suffix),
                      obligation = sprintf("%s receives the more favorable of %s and %s; %s receives the other.",
                                           holder, holder, counter, counter),
                      notes = sw$notes,
                      group_type = "simple_swap",
                      pick_types = c("own", "swap"))

    add_display_group(display_asset_id = sprintf("display_%d_%s_swap_return_%s_%s", 
                                                 yr, counter, holder, counter),
                      year = yr,
                      owner = counter,
                      original_teams = pool,
                      label = sprintf("%d %s own or %s (%s)", yr, counter, holder, suffix),
                      obligation = sprintf("%s receives the less favorable of %s and %s after %s's swap right.", 
                                           counter, holder, counter, holder),
                      notes = sw$notes,
                      group_type = "simple_swap_return",
                      pick_types = c("own", "swap_return"))
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
    orig <- str_split(spec$original_teams, "\\s*,\\s*")[[1]]
    add_display_group(display_asset_id = sprintf("display_%d_%s_%s", 
                                                 spec$year, spec$owner, spec$complex_group),
                      year = spec$year,
                      owner = spec$owner,
                      original_teams = orig,
                      label = spec$label,
                      obligation = spec$obligation,
                      notes = spec$obligation,
                      group_type = "complex_entitlement",
                      complex_group_filter = spec$complex_group,
                      pick_types = c("own", "complex"))               
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





## ═════════════════════════════════════════════════════════════════════════════
## 07 - BUILD MARKOV TRANSITION COUNTS + DIRICHLET PRIOR -----------------------
## ═════════════════════════════════════════════════════════════════════════════

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


## ═════════════════════════════════════════════════════════════════════════════
## 08 - FIT & VALIDATE STAN MODELS ---------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════


# Two fits of the SAME v4 model (player-level Student-t, adjacent-pick RW
# sigma, picks 1-60) on IDENTICAL rows, differing only in the use_r2_offset
# data toggle:
#   * v4 unified : one power-law mu = alpha/p^beta + gamma over picks 1-60
#   * v4 r2off   : adds an additive level shift delta_r2 for picks 31-60
# Because the rows and likelihood family are identical, loo_compare() is a
# direct, pointwise-valid answer to the design question "single unified curve
# vs boundary adjustment at pick 30".
#
# RETIRED: the legacy 0601/0605 archive models (constant / linear sigma) were
# dimensioned for 30 picks and a bounded-below WS outcome; they are no longer
# fit. The v3 -> v4 lineage keeps the RW-sigma structure.
PICK_MODEL_V4_PATH <- "02_models/pick_value_v4.stan"

resolve_required_file <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Required file not found: %s", path), call. = FALSE)
  }
  path
}

# Player-level pick-value data: one row per drafted player, outcome = first-4
# season Total Points Impact, picks 1-60. Both v4 fits use these same rows.
pick_fit_data <- if (!is.null(draft_tpi4) && nrow(draft_tpi4) > 50) {
  draft_tpi4 %>%
    transmute(
      draft_year = as.integer(draft_year),
      pick       = as.integer(pick),
      round      = as.character(round),
      player     = as.character(player),
      tpi4       = as.numeric(tpi4)
    ) %>%
    filter(!is.na(pick), pick >= 1, pick <= N_PICKS_TOTAL, !is.na(tpi4))
} else {
  # Fallback synthetic player-level pool built from the compiled slot curve.
  pick_boot_pool %>%
    mutate(
      draft_year = NA_integer_,
      player     = NA_character_
    ) %>%
    transmute(draft_year, pick = as.integer(pick),
              round = ifelse(pick > 30L, "R2", "R1"),
              player, tpi4 = as.numeric(tpi4)) %>%
    filter(!is.na(pick), pick >= 1, pick <= N_PICKS_TOTAL, !is.na(tpi4))
}

pick_stan_data_unified <- list(
  N             = nrow(pick_fit_data),
  P             = N_PICKS_TOTAL,
  pick          = pick_fit_data$pick,
  tpi4          = pick_fit_data$tpi4,
  use_r2_offset = 0L
)
pick_stan_data_r2off <- modifyList(pick_stan_data_unified,
                                   list(use_r2_offset = 1L))

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

pick_fit_unified <- sample_pick_stan_model(
  model_path = PICK_MODEL_V4_PATH,
  data       = pick_stan_data_unified,
  seed       = 202604,
  label      = "v4 unified (no round-2 offset)"
)

pick_fit_r2off <- sample_pick_stan_model(
  model_path = PICK_MODEL_V4_PATH,
  data       = pick_stan_data_r2off,
  seed       = 202606,
  label      = "v4 r2off (additive round-2 offset)"
)

# Production model selection (set in config; flip after reviewing the LOO
# comparison and the pick-30 boundary residuals).
pick_fit <- if (USE_R2_OFFSET_PRODUCTION) pick_fit_r2off else pick_fit_unified

# ---- VALIDATION 0: PSIS-LOO — unified curve vs round-2 boundary offset ----
loo_unified <- loo::loo(
  as.matrix(pick_fit_unified$draws("log_lik", format = "draws_matrix"))
)
loo_r2off <- loo::loo(
  as.matrix(pick_fit_r2off$draws("log_lik", format = "draws_matrix"))
)

cat("\n  [validate] PSIS-LOO comparison: v4 unified vs v4 round-2 offset\n")
pick_loo_compare <- loo_compare(list(v4_unified  = loo_unified,
                                     v4_r2_offset = loo_r2off))
print(pick_loo_compare)

# pareto_k_table is provided by the user's validation toolkit; define a guarded
# local fallback so a standalone run still prints the diagnostic.
if (!exists("pareto_k_table")) {
  pareto_k_table <- function(loo_obj) {
    k <- loo::pareto_k_values(loo_obj)
    tibble(
      bucket = c("good (k <= 0.5)", "ok (0.5 < k <= 0.7)", "bad (k > 0.7)"),
      n      = c(sum(k <= 0.5, na.rm = TRUE),
                 sum(k > 0.5 & k <= 0.7, na.rm = TRUE),
                 sum(k > 0.7, na.rm = TRUE)),
      pct    = n / length(k)
    )
  }
}

cat("\n  [validate] Pareto-k table: v4 unified\n")
print(pareto_k_table(loo_unified))
cat("\n  [validate] Pareto-k table: v4 r2off\n")
print(pareto_k_table(loo_r2off))

pick_loo_compare_tbl <- as.data.frame(pick_loo_compare) %>%
  rownames_to_column("model") %>%
  as_tibble()

pick_loo_summary <- tibble(
  model = c("v4_unified", "v4_r2_offset"),
  elpd_loo = c(
    loo_unified$estimates["elpd_loo", "Estimate"],
    loo_r2off$estimates["elpd_loo", "Estimate"]
  ),
  p_loo = c(
    loo_unified$estimates["p_loo", "Estimate"],
    loo_r2off$estimates["p_loo", "Estimate"]
  ),
  looic = c(
    loo_unified$estimates["looic", "Estimate"],
    loo_r2off$estimates["looic", "Estimate"]
  ),
  max_pareto_k = c(
    max(loo::pareto_k_values(loo_unified), na.rm = TRUE),
    max(loo::pareto_k_values(loo_r2off), na.rm = TRUE)
  )
)

cat("\n  [validate] LOO summary by pick-value model\n")
print(pick_loo_summary)

# Core scalar variables for diagnostics. delta_r2 only enters the likelihood
# in the r2off fit; in the unified fit it samples its prior (sanity check:
# its posterior there should match normal(0, 25)).
pick_core_vars <- c("alpha", "beta", "gamma", "delta_r2",
                    "log_sigma_1", "tau_log_sigma_rw", "nu")

pick_draws <- pick_fit$draws(
  variables = c("alpha", "beta", "gamma", "delta_r2", "tau_log_sigma_rw", "nu"),
  format = "df"
) %>% as_tibble()

# Extract Stan vector draws in numeric index order, regardless of CmdStan's
# column ordering. These matrices are draws x pick slot.
extract_stan_vector_draws <- function(fit, variable_base, K = N_PICKS_TOTAL) {
  mat <- as.matrix(fit$draws(variables = variable_base, format = "draws_matrix"))
  idx <- str_match(colnames(mat), paste0("^", variable_base, "\\[(\\d+)\\]$"))[, 2]
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

pick_mu_draws <- extract_stan_vector_draws(pick_fit, "tpi_pred", N_PICKS_TOTAL)
pick_sd_draws <- extract_stan_vector_draws(pick_fit, "tpi_pred_sd", N_PICKS_TOTAL)

cat("\n  Pick-value posterior summary: v4 production model\n")
print(pick_fit$summary(pick_core_vars))

# ---- VALIDATION 1: convergence diagnostics ----
pick_diag <- pick_fit$summary(pick_core_vars) %>%
  select(variable, rhat, ess_bulk, ess_tail)
cat("\n  [validate] Pick model R-hat / ESS: v4 production model\n")
print(pick_diag)
if (any(pick_diag$rhat > 1.01, na.rm = TRUE)) {
  warning("Pick model: some R-hat > 1.01 — inspect convergence.")
}

# ---- VALIDATION 2: posterior-predictive player-level coverage ----
# Share of drafted-player outcomes whose realized first-4-year TPI falls in
# the 90% posterior predictive interval, by pick band (both rounds).
tpi4_rep_mat <- as.matrix(pick_fit$draws(variables = "tpi4_rep", format = "draws_matrix"))

ppc_tbl <- tibble(
  row_id = seq_len(nrow(pick_fit_data)),
  draft_year = pick_fit_data$draft_year,
  pick = pick_fit_data$pick,
  player = pick_fit_data$player,
  obs = pick_fit_data$tpi4,
  lo = apply(tpi4_rep_mat, 2, quantile, probs = 0.05, na.rm = TRUE),
  hi = apply(tpi4_rep_mat, 2, quantile, probs = 0.95, na.rm = TRUE)
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
      pick <= 30 ~ "21-30",
      pick <= 45 ~ "31-45",
      TRUE       ~ "46-60"
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

tpi4_rep_mean <- colMeans(tpi4_rep_mat)

ppc_pick_resid <- ppc_tbl %>%
  mutate(pred_mean = tpi4_rep_mean) %>%
  group_by(pick) %>%
  summarise(
    n = n(),
    obs_mean = mean(obs),
    pred_mean = mean(pred_mean),
    resid = obs_mean - pred_mean,
    coverage_90 = mean(covered),
    .groups = "drop"
  )

print(ppc_pick_resid, n = N_PICKS_TOTAL)

# Pay particular attention to resid around picks 28-33: a systematic sign
# flip there is the empirical signature that the unified curve distorts the
# round boundary and the r2off model should be production.

sigma_draws <- as.matrix(pick_fit$draws("tpi_pred_sd", format = "draws_matrix"))

sigma_curve <- tibble(
  pick = 1:N_PICKS_TOTAL,
  sigma_mean = colMeans(sigma_draws),
  sigma_q05 = apply(sigma_draws, 2, quantile, 0.05),
  sigma_q50 = apply(sigma_draws, 2, quantile, 0.50),
  sigma_q95 = apply(sigma_draws, 2, quantile, 0.95)
)

print(sigma_curve, n = N_PICKS_TOTAL)

# Keep the original single-model LOO alias for backward compatibility, but the
# main model-selection object is pick_loo_compare / loo_compare(...) above.
loo_pick <- if (USE_R2_OFFSET_PRODUCTION) loo_r2off else loo_unified
cat("\n  [validate] Production pick model LOO: v4\n")
print(loo_pick)
print(pareto_k_table(loo_pick))


cat("\n--- Fitting Markov Transition Stan Model ---\n")

markov_model <- cmdstan_model("02_models/team_strength.stan")

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
# Both return a sampled first-4-year TPI value for a given draft slot (1-60),
# carrying full uncertainty. The Monte Carlo loop calls sample_pick_value().

# Bayesian: draw curve params from the posterior, then draw a player-level
# first-4-year TPI outcome from the Student-t predictive distribution.
sample_pick_value_bayes <- function(pos, draw_idx = sample(nrow(pick_draws), 1)) {
  pos <- as.integer(pos)
  pos <- max(1L, min(N_PICKS_TOTAL, pos))

  # Use generated posterior mean / residual-scale vectors so downstream code
  # does not depend on the internal variance parameterization. This works for
  # the adjacent-pick-smoothed sigma model and will also work if the mean model
  # later adds slot effects.
  mu <- pick_mu_draws[draw_idx, pos]
  sg <- pick_sd_draws[draw_idx, pos]
  nu <- pick_draws$nu[draw_idx]

  mu + sg * rt(1, df = nu)
}

# Bootstrap: resample an actual player's 4-yr TPI from a neighborhood of slots.
# Borrows from +/- 1 slot to stabilize thin slots, mirroring the smoothing in
# the McCartney trajectory work.
boot_index <- split(pick_boot_pool$tpi4, pick_boot_pool$pick)
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
  picks <- integer(16)
  drawn <- integer(0)

  for (p in 1:16) {
    undrawn <- setdiff(1:16, drawn)
    releg_remaining <- setdiff(1:3, drawn)

    # Relegated seeds cannot fall past pick 12. Enforce that constraint while
    # drawing, rather than repairing with a post-hoc swap. If the number of
    # undrawn relegated teams equals the number of remaining floor-safe slots,
    # the next pick must come from the remaining relegated seeds so all can be
    # seated by #12. Everyone else shifts down naturally.
    if (p <= 12) {
      slots_to_floor <- 12 - p + 1
      eligible <- if (length(releg_remaining) >= slots_to_floor) {
        releg_remaining
      } else {
        undrawn
      }
    } else {
      eligible <- setdiff(undrawn, 1:3)
      if (length(eligible) == 0) eligible <- undrawn
    }

    pr <- balls16
    pr[setdiff(1:16, eligible)] <- 0
    pr <- pr / sum(pr)

    w <- sample(16, 1, prob = pr)
    picks[w] <- p
    drawn <- c(drawn, w)
  }

  picks
}

# ---- SECOND-ROUND SLOT ASSIGNMENT --------------------------------------------
# Maps each original team's ROUND-1 slot (1-30) to its ROUND-2 overall slot
# (31-60) under each system.
#
# CURRENT system: round 2 is its own reverse-record ordering, independent of
# the lottery draw. We proxy "record order" with the pre-lottery worst-to-best
# ordering (ord = position 1 is the worst record), so the worst team picks 31.
second_round_current <- function(ord) {
  # ord: character vector of teams, worst record first (length 30)
  setNames(30L + seq_along(ord), ord)
}

# 3-2-1 system (per the approved rules / Yahoo explainer): "the first 16 picks
# of the second round will be the INVERSE of the (post-draw) lottery order".
# The team that wins the #1 lottery slot picks 46; the team at lottery slot 16
# picks 31. Picks 47-60 then go to the 14 playoff teams worst-to-best, i.e.
# round-1 slots 17-30 map to +30 as before.
#
#   R1 slot s in 1..16  ->  R2 slot 47 - s
#   R1 slot s in 17..30 ->  R2 slot s + 30
#
# ASSUMPTION (documented): the inversion is applied to the POST-RESTRICTION
# round-1 order — i.e. after the anti-tank reseating (no repeat #1, no 3-yr
# top-5) has produced the final first-round slots. The league memo does not
# spell out whether the inversion uses the raw draw or the adjusted order;
# using the final order keeps round 2 consistent with the round-1 board shown
# to teams. Flip the input (raw draw slots) if guidance emerges.
second_round_321 <- function(slot_n) {
  # slot_n: named integer vector, original team -> final R1 slot (1-30)
  s <- as.integer(slot_n)
  r2 <- ifelse(s <= 16L, 47L - s, s + 30L)
  setNames(as.integer(r2), names(slot_n))
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

# Apply the approved restrictions to the full ORIGINAL-team slot permutation,
# given each team's recent top-pick history.
#   - cannot receive #1 in consecutive years
#   - cannot receive a top-5 pick three years running
# When a restriction binds, the illegal team is bumped down to the first legal
# slot and every intervening team shifts up one slot. This preserves a valid
# one-team-per-slot draft order.
restricted_slot_target <- function(slot, orig_team, yr, top_pick_history) {
  hist <- top_pick_history[[orig_team]]
  got_no1_last  <- !is.null(hist$no1)  && (yr - 1) %in% hist$no1
  got_top5_2ago <- !is.null(hist$top5) &&
    all(c(yr - 1, yr - 2) %in% hist$top5)

  target <- slot
  if (target == 1 && got_no1_last) target <- 2
  if (target <= 5 && got_top5_2ago) target <- 6
  target
}

apply_pick_restrictions <- function(slots, yr, top_pick_history) {
  slots <- as.integer(slots) %>% setNames(names(slots))

  # Restart after each bump because shifting a team up can move that team into a
  # newly illegal slot (for example, a prior #1 winner shifted from #2 to #1).
  for (iter in seq_len(length(slots) + 5L)) {
    changed <- FALSE
    for (orig_team in names(slots)[order(slots)]) {
      slot <- slots[[orig_team]]
      target <- restricted_slot_target(slot, orig_team, yr, top_pick_history)

      if (!is.na(target) && target > slot) {
        affected <- names(slots)[slots > slot & slots <= target]
        slots[affected] <- slots[affected] - 1L
        slots[[orig_team]] <- target
        changed <- TRUE
        break
      }
    }

    if (!changed) break
    if (iter == length(slots) + 5L) {
      warning("Pick restriction reseating did not stabilize; inspect slot history.")
    }
  }

  if (anyDuplicated(slots) || !identical(sort(as.integer(slots)), seq_along(slots))) {
    warning("Pick restriction reseating produced a non-permutation draft order.")
  }

  slots
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

# ---- ROUND-2 ownership resolution --------------------------------------------
# Future R2 obligations are encoded as simple outright transfers (see the
# section-06 SIMPLIFICATION note), so resolution is a single conveyance pass
# over traded_future_r2. Protections (top45/top55) are evaluated against the
# OVERALL slot number (31-60).
apply_simple_future_obligations_r2 <- function(owner_by_orig2, slots2, yr) {
  rows <- traded_future_r2 %>% filter(year == yr)
  if (nrow(rows) > 0) {
    for (j in seq_len(nrow(rows))) {
      og <- rows$original_team[j]
      ow <- rows$owner[j]
      prot <- rows$protection[j]
      if (!is.na(slots2[og]) && pick_conveys(slots2[og], prot)) {
        owner_by_orig2[og] <- ow
      }
    }
  }
  owner_by_orig2
}

resolve_pick_owners_r2 <- function(slots2, yr) {
  owner_by_orig2 <- setNames(all_teams, all_teams)
  apply_simple_future_obligations_r2(owner_by_orig2, slots2, yr)
}

# Values every future pick asset (BOTH rounds) for one sim-year-system cell.
# R1 assets read the first-round slot/owner state; R2 assets read the
# second-round slot/owner state. Raw values for both rounds are drawn from the
# same posterior curve (slots 31-60 land on the flat tail).
value_allocated_future_assets <- function(sim, yr, slots, owner_by_orig,
                                          slots2, owner_by_orig2, d_pick,
                                          team_value, team_n, team_best,
                                          system = c("cur", "new")) {
  system <- match.arg(system)
  yr_assets <- pick_assets %>% filter(year == yr)

  raw_by_orig <- setNames(rep(0, length(all_teams)), all_teams)
  raw_by_orig2 <- raw_by_orig
  for (tm in all_teams) {
    raw_by_orig[tm]  <- if (!is.na(slots[tm]))  sample_pick_value(slots[tm],  draw_idx = d_pick) else 0
    raw_by_orig2[tm] <- if (!is.na(slots2[tm])) sample_pick_value(slots2[tm], draw_idx = d_pick) else 0
  }

  for (j in seq_len(nrow(yr_assets))) {
    aid <- yr_assets$asset_id[j]
    og  <- yr_assets$original_team[j]
    ow  <- yr_assets$owner[j]
    is_r2 <- identical(yr_assets$round[j], "R2")

    slot_og <- if (is_r2) slots2[og] else slots[og]
    slot_ow <- if (is_r2) slots2[ow] else slots[ow]
    raw_og  <- if (is_r2) raw_by_orig2[og] else raw_by_orig[og]
    owner_of_og <- if (is_r2) owner_by_orig2[og] else owner_by_orig[og]

    allocated <- !is.na(owner_of_og) && owner_of_og == ow
    val <- if (allocated) raw_og else 0

    if (system == "cur") {
      asset_cur[sim, aid] <<- val
      asset_slot_cur[sim, aid] <<- slot_og
      asset_raw_cur[sim, aid] <<- raw_og
      asset_ownslot_cur[sim, aid] <<- slot_ow
      asset_convey_cur[sim, aid] <<- as.integer(allocated)
    } else {
      asset_new[sim, aid] <<- val
      asset_slot_new[sim, aid] <<- slot_og
      asset_raw_new[sim, aid] <<- raw_og
      asset_ownslot_new[sim, aid] <<- slot_ow
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





## ═════════════════════════════════════════════════════════════════════════════
## SECTION 12: FULL MONTE CARLO ------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════
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
# Second-round seats (overall slots 31-60) per original team / year / system.
team_slot2_cur <- array(NA_real_, dim = c(N_SIMS, 30, n_proj_years),
                        dimnames = list(NULL, all_teams, as.character(proj_years)))
team_slot2_new <- array(NA_real_, dim = c(N_SIMS, 30, n_proj_years),
                        dimnames = list(NULL, all_teams, as.character(proj_years)))
sim_curve_par_cols <- c(
  "alpha", "beta", "gamma", "delta_r2", "tau_log_sigma_rw", "nu",
  paste0("mu_", 1:N_PICKS_TOTAL),
  paste0("sigma_", 1:N_PICKS_TOTAL)
)
sim_curve_par <- matrix(NA_real_, N_SIMS, length(sim_curve_par_cols),
                        dimnames = list(NULL, sim_curve_par_cols))

# Map from (year, round, owner, original_team, pick_type) -> asset_id
asset_key <- pick_assets %>%
  mutate(key = sprintf("%d|%s|%s|%s|%s", year, round, owner, original_team, pick_type))
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
    pick_draws$delta_r2[d_pick],
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
    # not change who owns the pick; ownership is resolved below. Reseating is
    # applied to the full permutation so no two teams can occupy the same slot.
    slot_n <- apply_pick_restrictions(slot_n, yr, top_hist)

    # ---- SECOND-ROUND seats (overall slots 31-60) ---------------------------
    # CURRENT system: reverse record, independent of the lottery draw.
    slot2_c <- second_round_current(ord)
    # 3-2-1 system: invert the FINAL (post-restriction) first-round lottery
    # order for picks 31-46; playoff teams map +30 as before. See the
    # second_round_321() ASSUMPTION note on raw-draw vs adjusted order.
    slot2_n <- second_round_321(slot_n)

    # ---- record realized ORIGINAL-team seats this year ----------------------
    yc <- as.character(yr)
    for (tm in all_teams) {
      if (!is.na(slot_c[tm])) team_slot_cur[sim, tm, yc] <- slot_c[tm]
      if (!is.na(slot_n[tm])) team_slot_new[sim, tm, yc] <- slot_n[tm]
      if (!is.na(slot2_c[tm])) team_slot2_cur[sim, tm, yc] <- slot2_c[tm]
      if (!is.na(slot2_n[tm])) team_slot2_new[sim, tm, yc] <- slot2_n[tm]
    }

    # ---- resolve all simple + complex ownership obligations -----------------
    owner_by_orig_c <- resolve_pick_owners(slot_c, yr)
    owner_by_orig_n <- resolve_pick_owners(slot_n, yr)
    owner_by_orig2_c <- resolve_pick_owners_r2(slot2_c, yr)
    owner_by_orig2_n <- resolve_pick_owners_r2(slot2_n, yr)

    # ---- value every possible future pick asset -----------------------------
    # Each original team's pick can be allocated to exactly one owner under each
    # system. Assets whose condition is not met get zero value in that draw; the
    # retained own-pick asset gets value when protections/swaps do not convey.
    val_c <- value_allocated_future_assets(
      sim = sim, yr = yr, slots = slot_c, owner_by_orig = owner_by_orig_c,
      slots2 = slot2_c, owner_by_orig2 = owner_by_orig2_c,
      d_pick = d_pick,
      team_value = tv_c, team_n = tn_c, team_best = tb_c,
      system = "cur"
    )
    tv_c <- val_c$team_value; tn_c <- val_c$team_n; tb_c <- val_c$team_best

    val_n <- value_allocated_future_assets(
      sim = sim, yr = yr, slots = slot_n, owner_by_orig = owner_by_orig_n,
      slots2 = slot2_n, owner_by_orig2 = owner_by_orig2_n,
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


# PER-PICK SUMMARIES + DOWNSAMPLED JOINT DRAWS (for Single Pick & Trade tabs)
# Replace any NA (asset not valued in a sim — e.g. an own pick that was traded
# away that year, which shouldn't happen, or a non-existent combo) with 0.
asset_cur[is.na(asset_cur)] <- 0
asset_new[is.na(asset_new)] <- 0
# raw/slot stores: leave slots as NA where a pick had no seat (e.g. own pick
# traded away that year); raw values default to 0 so protection math is safe.
asset_raw_cur[is.na(asset_raw_cur)] <- 0
asset_raw_new[is.na(asset_raw_new)] <- 0

# Expected Asset Value (EV) matrices: same simulated pick slots / conveyance
# events, but valued with the posterior mean slot curve instead of a sampled
# player-level Student-t outcome. These are the team / pick values shown when
# the app toggles to "Expected Asset Value" and match the left-side Trade
# Machine interpretation.
asset_value_mean_from_slots <- function(slot_mat, convey_mat) {
  out <- matrix(
    0,
    nrow = nrow(slot_mat),
    ncol = ncol(slot_mat),
    dimnames = dimnames(slot_mat)
  )

  mu_mat <- as.matrix(sim_curve_par[, paste0("mu_", 1:N_PICKS_TOTAL), drop = FALSE])

  for (aid in colnames(slot_mat)) {
    slots <- as.integer(slot_mat[, aid])
    active <- !is.na(slots)
    if (!is.null(convey_mat) && aid %in% colnames(convey_mat)) {
      active <- active & convey_mat[, aid] > 0
    }
    if (any(active)) {
      idx <- which(active)
      slot_idx <- pmin(pmax(slots[idx], 1L), N_PICKS_TOTAL)
      out[idx, aid] <- mu_mat[cbind(idx, slot_idx)]
    }
  }

  out[is.na(out)] <- 0
  out
}

asset_cur_ev <- asset_value_mean_from_slots(asset_slot_cur, asset_convey_cur)
asset_new_ev <- asset_value_mean_from_slots(asset_slot_new, asset_convey_new)

# Per-asset distribution summary (sampled player-outcome value + 90% credible interval).
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

# Per-asset Expected Asset Value summary. Same columns as pick_value_summary,
# but based on posterior mean slot values instead of sampled player outcomes.
pick_value_ev_summary <- pick_assets %>%
  mutate(
    cur_mean = colMeans(asset_cur_ev)[asset_id],
    cur_q05  = apply(asset_cur_ev, 2, quantile, 0.05)[asset_id],
    cur_q50  = apply(asset_cur_ev, 2, quantile, 0.50)[asset_id],
    cur_q95  = apply(asset_cur_ev, 2, quantile, 0.95)[asset_id],
    cur_sd   = apply(asset_cur_ev, 2, sd)[asset_id],
    new_mean = colMeans(asset_new_ev)[asset_id],
    new_q05  = apply(asset_new_ev, 2, quantile, 0.05)[asset_id],
    new_q50  = apply(asset_new_ev, 2, quantile, 0.50)[asset_id],
    new_q95  = apply(asset_new_ev, 2, quantile, 0.95)[asset_id],
    new_sd   = apply(asset_new_ev, 2, sd)[asset_id],
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
display_asset_cur_ev_full <- build_display_draw_matrix(asset_cur_ev, pick_display_members, pick_display_assets)
display_asset_new_ev_full <- build_display_draw_matrix(asset_new_ev, pick_display_members, pick_display_assets)
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

pick_display_value_ev_summary <- pick_display_assets %>%
  mutate(
    cur_mean = colMeans(display_asset_cur_ev_full)[display_asset_id],
    cur_q05  = apply(display_asset_cur_ev_full, 2, quantile, 0.05)[display_asset_id],
    cur_q50  = apply(display_asset_cur_ev_full, 2, quantile, 0.50)[display_asset_id],
    cur_q95  = apply(display_asset_cur_ev_full, 2, quantile, 0.95)[display_asset_id],
    cur_sd   = apply(display_asset_cur_ev_full, 2, sd)[display_asset_id],
    new_mean = colMeans(display_asset_new_ev_full)[display_asset_id],
    new_q05  = apply(display_asset_new_ev_full, 2, quantile, 0.05)[display_asset_id],
    new_q50  = apply(display_asset_new_ev_full, 2, quantile, 0.50)[display_asset_id],
    new_q95  = apply(display_asset_new_ev_full, 2, quantile, 0.95)[display_asset_id],
    new_sd   = apply(display_asset_new_ev_full, 2, sd)[display_asset_id],
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
asset_cur_ev_draws <- asset_cur_ev[keep_idx, , drop = FALSE]
asset_new_ev_draws <- asset_new_ev[keep_idx, , drop = FALSE]

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
team_slot2_cur_draws    <- team_slot2_cur[keep_idx, , , drop = FALSE]
team_slot2_new_draws    <- team_slot2_new[keep_idx, , , drop = FALSE]
sim_curve_par_draws     <- sim_curve_par[keep_idx, , drop = FALSE]

display_asset_cur_draws <- build_display_draw_matrix(asset_cur_draws, pick_display_members, pick_display_assets)
display_asset_new_draws <- build_display_draw_matrix(asset_new_draws, pick_display_members, pick_display_assets)
display_asset_cur_ev_draws <- build_display_draw_matrix(asset_cur_ev_draws, pick_display_members, pick_display_assets)
display_asset_new_ev_draws <- build_display_draw_matrix(asset_new_ev_draws, pick_display_members, pick_display_assets)
display_convey_cur_draws <- build_display_convey_matrix(asset_convey_cur_draws, pick_display_members, pick_display_assets)
display_convey_new_draws <- build_display_convey_matrix(asset_convey_new_draws, pick_display_members, pick_display_assets)

cat(sprintf("Stored %d joint draws per asset for trade analysis\n", n_keep))


## ═════════════════════════════════════════════════════════════════════════════
## 13 - SUMMARIZE + LOTTERY ODDS + EXPORT --------------------------------------
## ═════════════════════════════════════════════════════════════════════════════

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

build_team_summary_from_asset_draws <- function(cur_mat, new_mat, base_summary) {
  purrr::map_dfr(all_teams, function(tm) {
    cols <- which(pick_assets$owner == tm)
    if (length(cols) == 0) {
      cur_total <- rep(0, nrow(cur_mat))
      new_total <- rep(0, nrow(new_mat))
      cur_best <- rep(NA_real_, nrow(cur_mat))
      new_best <- rep(NA_real_, nrow(new_mat))
    } else {
      cur_sub <- cur_mat[, cols, drop = FALSE]
      new_sub <- new_mat[, cols, drop = FALSE]
      cur_total <- rowSums(cur_sub)
      new_total <- rowSums(new_sub)
      cur_best <- apply(cur_sub, 1, max, na.rm = TRUE)
      new_best <- apply(new_sub, 1, max, na.rm = TRUE)
    }

    base_row <- base_summary %>% filter(team == tm)
    tibble(
      team = tm,
      current_mean   = mean(cur_total),
      current_median = median(cur_total),
      current_sd     = sd(cur_total),
      current_q05    = quantile(cur_total, 0.05),
      current_q25    = quantile(cur_total, 0.25),
      current_q75    = quantile(cur_total, 0.75),
      current_q95    = quantile(cur_total, 0.95),
      new_mean       = mean(new_total),
      new_median     = median(new_total),
      new_sd         = sd(new_total),
      new_q05        = quantile(new_total, 0.05),
      new_q25        = quantile(new_total, 0.25),
      new_q75        = quantile(new_total, 0.75),
      new_q95        = quantile(new_total, 0.95),
      n_picks_mean   = base_row$n_picks_mean[1],
      best_current   = mean(cur_best, na.rm = TRUE),
      best_new       = mean(new_best, na.rm = TRUE),
      delta_value    = mean(new_total) - mean(cur_total),
      delta_pct      = (mean(new_total) / pmax(mean(cur_total), 0.01) - 1) * 100,
      delta_quality  = mean(new_best, na.rm = TRUE) - mean(cur_best, na.rm = TRUE),
      sigma_change   = sd(new_total) - sd(cur_total)
    )
  }) %>%
    left_join(tier_map, by = c("team" = "abbr")) %>%
    arrange(desc(delta_value))
}

summary_ev <- build_team_summary_from_asset_draws(asset_cur_ev, asset_new_ev, summary_df)

# ---- lottery odds tables (independent of team identities) ----
cat("\n--- Computing Lottery Odds Tables ---\n")
cur_sims <- matrix(0L, N_LOT, 14)
new_sims <- matrix(0L, N_LOT, 16)
for (i in 1:N_LOT) {
  cur_sims[i, ] <- sim_current_lottery()
  new_sims[i, ] <- sim_321_lottery()
}

lottery_seed_tiers <- tibble(
  seed = 1:16,
  lottery_tier = case_when(
    seed <= 3  ~ "relegation",
    seed <= 10 ~ "nonplayin",
    seed <= 14 ~ "playin_seed",
    TRUE       ~ "playin_loser"
  ),
  lottery_tier_label = case_when(
    lottery_tier == "relegation"   ~ "Three worst",
    lottery_tier == "nonplayin"    ~ "4th-10th worst",
    lottery_tier == "playin_seed"  ~ "9/10 Play-In seeds",
    lottery_tier == "playin_loser" ~ "7v8 Play-In losers"
  )
)

summarise_lottery_seed <- function(x) {
  tibble(
    expected_pick = mean(x),
    expected_pick_se = sd(x) / sqrt(length(x)),
    prob_no1  = mean(x == 1),
    prob_top3 = mean(x <= 3),
    prob_top5 = mean(x <= 5),
    prob_top10 = mean(x <= 10),
    prob_no1_se  = sqrt(prob_no1 * (1 - prob_no1) / length(x)),
    prob_top3_se = sqrt(prob_top3 * (1 - prob_top3) / length(x)),
    prob_top5_se = sqrt(prob_top5 * (1 - prob_top5) / length(x)),
    prob_top10_se = sqrt(prob_top10 * (1 - prob_top10) / length(x))
  )
}

lottery_dist <- bind_rows(
  map_dfr(1:14, function(s) {
    summarise_lottery_seed(cur_sims[, s]) %>%
      mutate(seed = s, system = "Current", .before = 1)
  }),
  map_dfr(1:16, function(s) {
    summarise_lottery_seed(new_sims[, s]) %>%
      mutate(seed = s, system = "Proposed 3-2-1", .before = 1)
  })
) %>%
  left_join(lottery_seed_tiers, by = "seed")

# Published aggregate 3-2-1 odds table. These are tier-level values because
# teams with the same ball count / floor treatment are symmetric within tier.
official_321_tier_odds <- tribble(
  ~lottery_tier,  ~lottery_tier_label,   ~seed_midpoint, ~official_prob_no1, ~official_prob_top3, ~official_prob_top5, ~official_prob_top10, ~official_expected_pick,
  "relegation",   "Three worst",              2.0,              0.054,              0.16,               0.28,                0.61,                 8.1,
  "nonplayin",    "4th-10th worst",           7.0,              0.081,              0.24,               0.39,                0.73,                 7.4,
  "playin_seed",  "9/10 Play-In seeds",      12.5,              0.054,              0.16,               0.28,                0.59,                 9.1,
  "playin_loser", "7v8 Play-In losers",      15.5,              0.027,              0.08,               0.15,                0.35,                11.7
)

lottery_tier_validation <- lottery_dist %>%
  filter(system == "Proposed 3-2-1") %>%
  group_by(lottery_tier, lottery_tier_label) %>%
  summarise(
    seed_min = min(seed),
    seed_max = max(seed),
    sim_expected_pick = mean(expected_pick),
    sim_prob_no1 = mean(prob_no1),
    sim_prob_top3 = mean(prob_top3),
    sim_prob_top5 = mean(prob_top5),
    sim_prob_top10 = mean(prob_top10),
    sim_expected_pick_mc_se = sqrt(sum(expected_pick_se^2)) / n(),
    sim_prob_no1_mc_se = sqrt(sum(prob_no1_se^2)) / n(),
    sim_prob_top3_mc_se = sqrt(sum(prob_top3_se^2)) / n(),
    sim_prob_top5_mc_se = sqrt(sum(prob_top5_se^2)) / n(),
    sim_prob_top10_mc_se = sqrt(sum(prob_top10_se^2)) / n(),
    .groups = "drop"
  ) %>%
  left_join(official_321_tier_odds, by = c("lottery_tier", "lottery_tier_label")) %>%
  mutate(
    diff_expected_pick = sim_expected_pick - official_expected_pick,
    diff_prob_no1 = sim_prob_no1 - official_prob_no1,
    diff_prob_top3 = sim_prob_top3 - official_prob_top3,
    diff_prob_top5 = sim_prob_top5 - official_prob_top5,
    diff_prob_top10 = sim_prob_top10 - official_prob_top10
  ) %>%
  arrange(seed_min)

cat("\n  [validate] 3-2-1 lottery odds vs published tier table\n")
print(lottery_tier_validation %>%
        transmute(
          tier = lottery_tier_label,
          sim_no1 = round(100 * sim_prob_no1, 2),
          official_no1 = round(100 * official_prob_no1, 1),
          sim_top3 = round(100 * sim_prob_top3, 2),
          official_top3 = round(100 * official_prob_top3, 1),
          sim_top5 = round(100 * sim_prob_top5, 2),
          official_top5 = round(100 * official_prob_top5, 1),
          sim_top10 = round(100 * sim_prob_top10, 2),
          official_top10 = round(100 * official_prob_top10, 1),
          sim_avg_pick = round(sim_expected_pick, 2),
          official_avg_pick = official_expected_pick
        ))

# ---- pick value curve for the dashboard (posterior mean +/- player-level sd) ----
# Use generated quantities from Stan rather than reconstructing the curve from
# scalar parameters. This keeps the dashboard stable if the Stan model changes
# its internal mean/variance parameterization.
pick_curve <- tibble(
  pick = 1:N_PICKS_TOTAL,
  expected_tpi = colMeans(pick_mu_draws),
  tpi_sd = colMeans(pick_sd_draws)
)

# attach empirical slot means for overlay
pick_curve <- pick_curve %>%
  left_join(pick_slot_data %>% select(pick, emp_mean = tpi_mean), by = "pick")

# ---- diagnostics bundle for the dashboard ----
stan_diagnostics <- list(
  pick_model = list(
    alpha       = round(mean(pick_draws$alpha), 2),
    beta        = round(mean(pick_draws$beta), 4),
    gamma       = round(mean(pick_draws$gamma), 2),
    delta_r2    = round(mean(pick_draws$delta_r2), 2),
    use_r2_offset_production = USE_R2_OFFSET_PRODUCTION,
    tau_log_sigma_rw = round(mean(pick_draws$tau_log_sigma_rw), 4),
    nu               = round(mean(pick_draws$nu), 2),
    sigma_pick_1     = round(mean(pick_sd_draws[, 1]), 2),
    sigma_pick_10    = round(mean(pick_sd_draws[, 10]), 2),
    sigma_pick_30    = round(mean(pick_sd_draws[, 30]), 2),
    sigma_pick_45    = round(mean(pick_sd_draws[, 45]), 2),
    sigma_pick_60    = round(mean(pick_sd_draws[, 60]), 2),
    n_players   = nrow(pick_fit_data),
    max_rhat    = round(max(pick_diag$rhat, na.rm = TRUE), 4),
    min_ess     = round(min(pick_diag$ess_bulk, na.rm = TRUE)),
    ppc_cover   = round(mean(ppc_tbl$covered), 3),
    ppc_level   = "player rows",
    loo_best_model = pick_loo_compare_tbl$model[1],
    loo_compare = pick_loo_compare_tbl,
    loo_summary = pick_loo_summary,
    curve_type  = "Bayesian Student-t player-level Stan (picks 1-60, TPI outcome) with adjacent-pick sigma smoothing and optional round-2 offset"
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
  summary_ev        = summary_ev,
  lottery_dist      = lottery_dist,
  lottery_tier_validation = lottery_tier_validation,
  official_321_tier_odds = official_321_tier_odds,
  pick_curve        = pick_curve,
  transition_matrix = post_trans,
  transition_counts = counts_mat,
  stationary        = setNames(as.numeric(mc_diag$stationary), TIERS),
  tier_balls        = TIER_BALLS,
  tier_sizes        = TIER_SIZES,
  actual_2026       = actual_2026_order,
  actual_2026_r2    = actual_2026_r2_order,
  traded_future     = traded_future,
  traded_future_r2  = traded_future_r2,
  complex_future_groups = complex_future_groups,
  complex_future_assets = complex_future_assets,
  owned_future      = owned_future,
  roster_info       = roster_info,
  pick_assets       = pick_assets,
  pick_value_summary = pick_value_summary,
  pick_value_ev_summary = pick_value_ev_summary,
  pick_display_assets = pick_display_assets,
  pick_display_members = pick_display_members,
  pick_display_value_summary = pick_display_value_summary,
  pick_display_value_ev_summary = pick_display_value_ev_summary,
  asset_cur_draws   = asset_cur_draws,
  asset_new_draws   = asset_new_draws,
  asset_cur_ev_draws = asset_cur_ev_draws,
  asset_new_ev_draws = asset_new_ev_draws,
  display_asset_cur_draws = display_asset_cur_draws,
  display_asset_new_draws = display_asset_new_draws,
  display_asset_cur_ev_draws = display_asset_cur_ev_draws,
  display_asset_new_ev_draws = display_asset_new_ev_draws,
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
  team_slot2_cur_draws    = team_slot2_cur_draws,
  team_slot2_new_draws    = team_slot2_new_draws,
  sim_curve_par_draws     = sim_curve_par_draws,
  proj_years              = proj_years,
  stan_diagnostics  = stan_diagnostics,
  metadata = list(
    n_sims      = N_SIMS,
    n_lottery   = N_LOT,
    draft_years = sprintf("2026 actual + %d-%d projected",
                          FIRST_PROJECTED_DRAFT, LAST_PROJECTED_DRAFT),
    system_note = "2026 actual results; 3-2-1 effective 2027-2029 (round-2 inversion applied under 3-2-1)",
    model_note  = "Bayesian 5-tier Markov chain + player-level Student-t 4-yr TPI pick curve (picks 1-60, xRAPM x possessions) with adjacent-pick variance smoothing",
    value_unit  = "TPI (first-4-year Total Points Impact = xRAPM/100 x possessions)",
    tiers       = TIERS,
    n_picks     = nrow(owned_future) + nrow(actual_2026_order) + nrow(actual_2026_r2_order),
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
            "Team", "Tier", "W-L", "Cur TPI", "3-2-1 TPI", "Delta", "Pct"))
cat(strrep("-", 64), "\n")
for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  cat(sprintf("%-6s %-13s %2d-%-2d %9.1f %10.1f %+8.1f %+6.1f%%\n",
              r$team, r$tier, r$wins, r$losses,
              r$current_mean, r$new_mean, r$delta_value, r$delta_pct))
}


