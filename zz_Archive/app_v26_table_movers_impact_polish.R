################################################################################
# NBA 3-2-1 Lottery Reform — Shiny Dashboard (v26)
# VERSION NOTE (v26 — pick movers + impact/full-table polish):
#   - Pick Movers rebuilt as simple HTML tables to avoid DT/flex rendering blanks.
#   - Full Table brighter cells, colored Δ%/Conf, stickier header styling, and
#     tighter detail spacing.
#   - Impact dumbbell color now uses a gentle red/green confidence gradient
#     instead of hard gray thresholds; renamed tabs per user request.
#
# VERSION NOTE (v25 — bug fixes + dumbbell rework):
#   - Fixed Full Table and Pick Movers rendering blank: both DT tables that use
#     escape=FALSE / custom row styling now render client-side (server = FALSE),
#     which is the reliable mode for HTML-cell tables of this size. Full Table
#     row-dimming now uses formatStyle(target="row") instead of a rowCallback.
#   - Winners & Losers dumbbell: color now encodes CONFIDENCE in the direction
#     of the change (vivid = P(direction) >= 85%, muted = >= 65%, gray = within
#     noise) instead of the all-or-nothing "does the 80% band clear zero" rule,
#     which made every team gray. Added a Y-axis sort control (Δ value vs total
#     3-2-1 value).
#   - Full Table: replaced the z-score "Signal" column with a clearer directional
#     "Conf" column (▲/▼ + P of that direction); dropped the now-redundant
#     P(+ Impact) column.
#
# VERSION NOTE (v24):
#   - Inter for prose/headers, IBM Plex Mono for tables/numbers; bigger headers.
#   - New Winners & Losers dumbbell tab; Full Table inline interval-bar + signal.
#   - Full Table single-scroll wrapper; Trade Machine column widening + gap.
#
# Reads dashboard_data.rds produced by nba_lottery.R
#
# Tabs:
#   Impact Δ        — change in expected draft-pick value by team
#   Lottery Odds    — seed-level expected pick and P(#1)
#   Full Table      — all 30 teams with credible intervals
#   Markov + Curve  — transition heatmap, state diagram, pick-value curve,
#                     and draft pick value curves
#
# Prereqs:
#   install.packages(c("shiny","plotly","DT","bslib","tidyverse","igraph"))
################################################################################

library(shiny)
library(plotly)
library(DT)
library(bslib)
library(tidyverse)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

has_igraph <- requireNamespace("igraph", quietly = TRUE)

dashboard_path <- c("01_data/dashboard_data.rds", "dashboard_data.rds")
dashboard_path <- dashboard_path[file.exists(dashboard_path)][1]
if (is.na(dashboard_path)) {
  stop("dashboard_data.rds not found. Run nba_lottery.R first.")
}

dd <- readRDS(dashboard_path)

summary_outcome_df <- dd$summary
summary_df         <- summary_outcome_df  # backward-compatible alias for outcome mode
lottery_dist <- dd$lottery_dist
lottery_tier_validation <- dd$lottery_tier_validation %||% tibble()
official_321_tier_odds <- dd$official_321_tier_odds %||% tibble()
for (nm in c("prob_top10", "expected_pick_se", "prob_no1_se", "prob_top3_se", "prob_top5_se", "prob_top10_se")) {
  if (!nm %in% names(lottery_dist)) lottery_dist[[nm]] <- NA_real_
}
pick_curve   <- dd$pick_curve

# Normalize the exported pick-value curve for display. New dashboard_data.rds
# files produced by nba_lottery_second_round.R export picks 1:60 directly. For
# older / intermediate RDS files, rebuild any missing 31:60 rows from the
# per-simulation posterior mean curve columns when available so the app never
# silently omits the second-round curve.
if (is.null(pick_curve) || nrow(pick_curve) == 0) {
  pick_curve <- tibble(pick = integer(0))
}
if (!"round" %in% names(pick_curve) && "pick" %in% names(pick_curve)) {
  pick_curve <- pick_curve %>%
    mutate(round = if_else(.data$pick >= 31L, 2L, 1L))
}
if ("pick" %in% names(pick_curve) && !all(31:60 %in% pick_curve$pick)) {
  missing_r2 <- setdiff(31:60, pick_curve$pick)
  mu2_cols <- paste0("mu_", missing_r2)
  if (!is.null(dd$sim_curve_par_draws) && all(mu2_cols %in% colnames(dd$sim_curve_par_draws))) {
    sigma2_cols <- paste0("sigma_", missing_r2)
    pplay2_cols <- paste0("p_play_", missing_r2)
    rebuilt_r2 <- tibble(
      pick = missing_r2,
      round = 2L,
      expected_war = colMeans(as.matrix(dd$sim_curve_par_draws[, mu2_cols, drop = FALSE]), na.rm = TRUE),
      ev_q05 = apply(as.matrix(dd$sim_curve_par_draws[, mu2_cols, drop = FALSE]), 2, quantile, 0.05, na.rm = TRUE),
      ev_q50 = apply(as.matrix(dd$sim_curve_par_draws[, mu2_cols, drop = FALSE]), 2, quantile, 0.50, na.rm = TRUE),
      ev_q95 = apply(as.matrix(dd$sim_curve_par_draws[, mu2_cols, drop = FALSE]), 2, quantile, 0.95, na.rm = TRUE),
      outcome_q10 = NA_real_,
      outcome_q90 = NA_real_,
      war_sd = if (all(sigma2_cols %in% colnames(dd$sim_curve_par_draws))) {
        colMeans(as.matrix(dd$sim_curve_par_draws[, sigma2_cols, drop = FALSE]), na.rm = TRUE)
      } else {
        NA_real_
      },
      p_play = if (all(pplay2_cols %in% colnames(dd$sim_curve_par_draws))) {
        colMeans(as.matrix(dd$sim_curve_par_draws[, pplay2_cols, drop = FALSE]), na.rm = TRUE)
      } else {
        NA_real_
      },
      p_play_q05 = if (all(pplay2_cols %in% colnames(dd$sim_curve_par_draws))) {
        apply(as.matrix(dd$sim_curve_par_draws[, pplay2_cols, drop = FALSE]), 2, quantile, 0.05, na.rm = TRUE)
      } else {
        NA_real_
      },
      p_play_q50 = if (all(pplay2_cols %in% colnames(dd$sim_curve_par_draws))) {
        apply(as.matrix(dd$sim_curve_par_draws[, pplay2_cols, drop = FALSE]), 2, quantile, 0.50, na.rm = TRUE)
      } else {
        NA_real_
      },
      p_play_q95 = if (all(pplay2_cols %in% colnames(dd$sim_curve_par_draws))) {
        apply(as.matrix(dd$sim_curve_par_draws[, pplay2_cols, drop = FALSE]), 2, quantile, 0.95, na.rm = TRUE)
      } else {
        NA_real_
      }
    )
    pick_curve <- bind_rows(pick_curve, rebuilt_r2)
  }
}
# Backfill fields used by the plotters for compatibility with old RDS files.
if (!"p_play" %in% names(pick_curve)) {
  pick_curve <- pick_curve %>% mutate(p_play = if_else(.data$pick <= 30L, 1, NA_real_))
}
if (!"emp_p_play" %in% names(pick_curve)) {
  pick_curve <- pick_curve %>% mutate(emp_p_play = NA_real_)
}
for (nm in c("p_play_q05", "p_play_q50", "p_play_q95")) {
  if (!nm %in% names(pick_curve)) pick_curve[[nm]] <- NA_real_
}
# Newer 04_lotterySims exports these directly. Keep backward compatibility so
# older dashboard_data.rds files do not break, but do not use war_sd as the
# curve ribbon anymore.
for (nm in c("ev_q05", "ev_q50", "ev_q95", "outcome_q10", "outcome_q90")) {
  if (!nm %in% names(pick_curve)) pick_curve[[nm]] <- NA_real_
}
pick_curve <- pick_curve %>% arrange(.data$pick)

trans_mat    <- dd$transition_matrix
trans_counts <- dd$transition_counts
stationary   <- dd$stationary
tier_balls   <- dd$tier_balls
meta         <- dd$metadata
stan_diag    <- dd$stan_diagnostics

# Per-pick objects (Single Pick & Trade Machine tabs)
pick_assets        <- dd$pick_assets
if (!"round" %in% names(pick_assets)) {
  pick_assets <- pick_assets %>%
    mutate(round = if_else(!is.na(fixed_slot) & fixed_slot >= 31L, 2L, 1L))
}
pick_value_summary <- dd$pick_value_summary
asset_cur_draws    <- dd$asset_cur_draws
asset_new_draws    <- dd$asset_new_draws

# Slot / raw-value / curve stores that let the Trade Machine apply HYPOTHETICAL
# protections & swaps to a pick a user is sending (recomputed per simulation).
asset_slot_cur_draws    <- dd$asset_slot_cur_draws
asset_slot_new_draws    <- dd$asset_slot_new_draws
asset_raw_cur_draws     <- dd$asset_raw_cur_draws
asset_raw_new_draws     <- dd$asset_raw_new_draws
asset_ownslot_cur_draws <- dd$asset_ownslot_cur_draws
asset_ownslot_new_draws <- dd$asset_ownslot_new_draws
if (!is.null(dd$asset_convey_cur_draws) && length(dd$asset_convey_cur_draws) > 0) {
  asset_convey_cur_draws <- dd$asset_convey_cur_draws
} else {
  asset_convey_cur_draws <- 1L * (!is.na(asset_slot_cur_draws) & asset_cur_draws != 0)
}
if (!is.null(dd$asset_convey_new_draws) && length(dd$asset_convey_new_draws) > 0) {
  asset_convey_new_draws <- dd$asset_convey_new_draws
} else {
  asset_convey_new_draws <- 1L * (!is.na(asset_slot_new_draws) & asset_new_draws != 0)
}
team_slot_cur_draws     <- dd$team_slot_cur_draws
team_slot_new_draws     <- dd$team_slot_new_draws
team_slot2_cur_draws    <- dd$team_slot2_cur_draws %||% NULL
team_slot2_new_draws    <- dd$team_slot2_new_draws %||% NULL
sim_curve_par_draws     <- dd$sim_curve_par_draws
proj_years              <- dd$proj_years

# value a draft slot under each kept sim's pick-value mean curve. The
# player-level Monte Carlo uses Student-t noise in nba_lottery.R; the app uses
# the deterministic mean here so hypothetical-protection deltas are stable.
# Prefer the per-slot posterior mean columns written by nba_lottery.R because
# they are robust to Stan-side changes in the mean / variance parameterization.
slot_value_vec <- function(slot_vec) {
  n_draws <- nrow(sim_curve_par_draws)
  slot_vec <- as.integer(slot_vec)

  # Match the old vectorized behavior: a scalar slot is valued under every kept
  # simulation draw; a vector slot should usually already be length n_draws.
  if (length(slot_vec) == 1L) {
    slot_vec <- rep(slot_vec, n_draws)
  } else if (length(slot_vec) != n_draws) {
    slot_vec <- rep(slot_vec, length.out = n_draws)
  }

  slot_clamped <- pmin(pmax(slot_vec, 1L), 60L)
  slot_index <- ifelse(is.na(slot_clamped), 1L, slot_clamped)

  mu_cols <- paste0("mu_", 1:60)
  if (all(mu_cols %in% colnames(sim_curve_par_draws))) {
    mu_mat <- as.matrix(sim_curve_par_draws[, mu_cols, drop = FALSE])
    out <- mu_mat[cbind(seq_len(n_draws), slot_index)]
    out[is.na(slot_clamped)] <- NA_real_
    return(out)
  }

  # Backward-compatible fallback for older dashboard_data.rds files.
  a  <- sim_curve_par_draws[, "alpha"]
  b  <- sim_curve_par_draws[, "beta"]
  g  <- sim_curve_par_draws[, "gamma"]
  out <- a / (pmin(slot_clamped, 30L) ^ b) + g
  if (any(slot_clamped > 30, na.rm = TRUE)) {
    # Old dashboard_data.rds files do not have a second-round curve. Use a
    # conservative decaying tail instead of incorrectly valuing all seconds as
    # pick 30.
    out[slot_clamped > 30] <- pmax(0, out[slot_clamped > 30] * exp(-0.11 * (slot_clamped[slot_clamped > 30] - 30)))
  }
  out[is.na(slot_clamped)] <- NA_real_
  out
}


# Smooth one-dimensional density helper for Plotly distribution displays.
# Keeps negative Win Shares intact and only removes missing / non-finite values.
density_curve_df <- function(x, n = 512, adjust = 1.05) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(tibble(x = numeric(0), density = numeric(0)))
  }

  ux <- unique(x)
  if (length(ux) == 1L) {
    center <- ux[1]
    bw <- max(0.05, stats::sd(x, na.rm = TRUE), abs(center) * 0.01)
    grid <- seq(center - 4 * bw, center + 4 * bw, length.out = n)
    return(tibble(x = grid, density = stats::dnorm(grid, mean = center, sd = bw)))
  }

  d <- stats::density(x, n = n, adjust = adjust, na.rm = TRUE)
  tibble(x = d$x, density = d$y)
}

# Expected-asset-value draw matrices. Outcome draws include player-level Student-t
# noise; EV draws value the same simulated draft slots under each posterior mean
# slot curve, matching the left-side Trade Machine logic.
build_asset_ev_draw_matrix_app <- function(slot_mat, convey_mat) {
  out <- matrix(
    0,
    nrow = nrow(slot_mat),
    ncol = ncol(slot_mat),
    dimnames = dimnames(slot_mat)
  )

  for (aid in colnames(slot_mat)) {
    slots <- as.integer(slot_mat[, aid])
    active <- !is.na(slots)
    if (!is.null(convey_mat) && aid %in% colnames(convey_mat)) {
      active <- active & convey_mat[, aid] > 0
    }
    if (any(active)) {
      vals <- slot_value_vec(slots)
      vals[is.na(vals)] <- 0
      out[active, aid] <- vals[active]
    }
  }

  out[is.na(out)] <- 0
  out
}

asset_cur_ev_draws <- if (!is.null(dd$asset_cur_ev_draws) && length(dd$asset_cur_ev_draws) > 0) {
  dd$asset_cur_ev_draws
} else {
  build_asset_ev_draw_matrix_app(asset_slot_cur_draws, asset_convey_cur_draws)
}

asset_new_ev_draws <- if (!is.null(dd$asset_new_ev_draws) && length(dd$asset_new_ev_draws) > 0) {
  dd$asset_new_ev_draws
} else {
  build_asset_ev_draw_matrix_app(asset_slot_new_draws, asset_convey_new_draws)
}

summarise_pick_draws_app <- function(base_tbl, cur_mat, new_mat, id_col) {
  base_tbl %>%
    mutate(
      cur_mean = colMeans(cur_mat)[.data[[id_col]]],
      cur_q05  = apply(cur_mat, 2, quantile, 0.05)[.data[[id_col]]],
      cur_q50  = apply(cur_mat, 2, quantile, 0.50)[.data[[id_col]]],
      cur_q95  = apply(cur_mat, 2, quantile, 0.95)[.data[[id_col]]],
      cur_sd   = apply(cur_mat, 2, sd)[.data[[id_col]]],
      new_mean = colMeans(new_mat)[.data[[id_col]]],
      new_q05  = apply(new_mat, 2, quantile, 0.05)[.data[[id_col]]],
      new_q50  = apply(new_mat, 2, quantile, 0.50)[.data[[id_col]]],
      new_q95  = apply(new_mat, 2, quantile, 0.95)[.data[[id_col]]],
      new_sd   = apply(new_mat, 2, sd)[.data[[id_col]]],
      delta    = new_mean - cur_mean
    )
}

pick_value_ev_summary <- if (!is.null(dd$pick_value_ev_summary) && nrow(dd$pick_value_ev_summary) > 0) {
  dd$pick_value_ev_summary
} else {
  summarise_pick_draws_app(pick_assets, asset_cur_ev_draws, asset_new_ev_draws, "asset_id") %>%
    mutate(
      cur_convey_prob = colMeans(asset_convey_cur_draws)[asset_id],
      new_convey_prob = colMeans(asset_convey_new_draws)[asset_id]
    )
}


# User-facing display assets collapse mutually exclusive internal simulation legs
# into one RealGM-style pick entitlement for the Single Pick and Trade Machine tabs.
build_display_matrix_app <- function(draw_mat, display_members, display_assets, count_mode = FALSE) {
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

if (!is.null(dd$pick_display_members) && nrow(dd$pick_display_members) > 0) {
  pick_display_members <- dd$pick_display_members
} else {
  if (!"asset_id" %in% names(pick_assets)) {
    stop("dashboard_data.rds is missing both pick_display_members and pick_assets$asset_id. Re-run nba_lottery.R with the display-assets export enabled.", call. = FALSE)
  }
  pick_display_members <- tibble(
    display_asset_id = paste0("display_", pick_assets$asset_id),
    asset_id = pick_assets$asset_id
  )
}

if (!is.null(dd$pick_display_assets) && nrow(dd$pick_display_assets) > 0) {
  pick_display_assets <- dd$pick_display_assets
} else {
  if (!"asset_id" %in% names(pick_assets)) {
    stop("dashboard_data.rds is missing both pick_display_assets and pick_assets$asset_id. Re-run nba_lottery.R with the display-assets export enabled.", call. = FALSE)
  }
  pick_display_assets <- pick_assets %>%
    transmute(
      display_asset_id = paste0("display_", asset_id),
      owner, year, round, label, obligation, notes,
      group_type = "single_asset",
      display_group = asset_id,
      member_n = 1L,
      member_original_teams = original_team
    )
}

if (!"round" %in% names(pick_display_assets)) {
  pick_display_assets <- pick_display_assets %>%
    left_join(pick_display_members %>% left_join(pick_assets %>% select(asset_id, round), by = "asset_id") %>%
                group_by(display_asset_id) %>% summarise(round = min(round, na.rm = TRUE), .groups = "drop"),
              by = "display_asset_id") %>%
    mutate(round = if_else(is.infinite(round), 1L, as.integer(round)))
}

# Defensive team-name normalization for older dashboard_data.rds exports that
# accidentally mixed full team names into owner/original-team fields. The app UI
# should only expose NBA abbreviations in team dropdowns and pick labels.
team_name_to_abbr_app <- c(
  "Atlanta Hawks" = "ATL", "Boston Celtics" = "BOS", "Brooklyn Nets" = "BKN",
  "New Jersey Nets" = "BKN", "Charlotte Hornets" = "CHA", "Charlotte Bobcats" = "CHA",
  "Chicago Bulls" = "CHI", "Cleveland Cavaliers" = "CLE", "Dallas Mavericks" = "DAL",
  "Denver Nuggets" = "DEN", "Detroit Pistons" = "DET", "Golden State Warriors" = "GSW",
  "Houston Rockets" = "HOU", "Indiana Pacers" = "IND", "Los Angeles Clippers" = "LAC",
  "Los Angeles Lakers" = "LAL", "Memphis Grizzlies" = "MEM", "Vancouver Grizzlies" = "MEM",
  "Miami Heat" = "MIA", "Milwaukee Bucks" = "MIL", "Minnesota Timberwolves" = "MIN",
  "New Orleans Pelicans" = "NOP", "New Orleans Hornets" = "NOP",
  "New Orleans/Oklahoma City Hornets" = "NOP", "New York Knicks" = "NYK",
  "Oklahoma City Thunder" = "OKC", "Seattle SuperSonics" = "OKC", "Orlando Magic" = "ORL",
  "Philadelphia 76ers" = "PHI", "Phoenix Suns" = "PHX", "Portland Trail Blazers" = "POR",
  "Sacramento Kings" = "SAC", "San Antonio Spurs" = "SAS", "Toronto Raptors" = "TOR",
  "Utah Jazz" = "UTA", "Washington Wizards" = "WAS",
  "BRK" = "BKN", "NJN" = "BKN", "CHO" = "CHA", "PHO" = "PHX", "NOH" = "NOP", "NOK" = "NOP", "PHL" = "PHI", "SAN" = "SAS", "SEA" = "OKC", "VAN" = "MEM"
)

canonical_team_abbr_app <- function(x) {
  x <- as.character(x)
  out <- stringr::str_squish(x)
  idx <- !is.na(out) & out %in% names(team_name_to_abbr_app)
  out[idx] <- unname(team_name_to_abbr_app[out[idx]])
  out
}

canonical_team_list_app <- function(x) {
  vapply(as.character(x), function(one) {
    if (is.na(one) || !nzchar(one)) return(NA_character_)
    vals <- unlist(stringr::str_split(one, "\\s*,\\s*"), use.names = FALSE)
    vals <- canonical_team_abbr_app(vals)
    paste(vals[!is.na(vals) & nzchar(vals)], collapse = ", ")
  }, character(1))
}

if (all(c("owner", "original_team") %in% names(pick_assets))) {
  pick_assets <- pick_assets %>%
    mutate(
      owner = canonical_team_abbr_app(.data$owner),
      original_team = canonical_team_abbr_app(.data$original_team)
    )
}
if (!"protection" %in% names(pick_assets)) pick_assets$protection <- NA_character_
if (!"pick_type" %in% names(pick_assets)) pick_assets$pick_type <- NA_character_

if ("owner" %in% names(pick_display_assets)) {
  pick_display_assets <- pick_display_assets %>%
    mutate(
      owner = canonical_team_abbr_app(.data$owner),
      member_original_teams = if ("member_original_teams" %in% names(.)) {
        canonical_team_list_app(.data$member_original_teams)
      } else {
        .data$member_original_teams
      }
    )
}

if (!is.null(dd$display_asset_cur_draws) && length(dd$display_asset_cur_draws) > 0) {
  display_asset_cur_draws <- dd$display_asset_cur_draws
} else {
  display_asset_cur_draws <- build_display_matrix_app(asset_cur_draws, pick_display_members, pick_display_assets)
}

if (!is.null(dd$display_asset_new_draws) && length(dd$display_asset_new_draws) > 0) {
  display_asset_new_draws <- dd$display_asset_new_draws
} else {
  display_asset_new_draws <- build_display_matrix_app(asset_new_draws, pick_display_members, pick_display_assets)
}

if (!is.null(dd$display_asset_cur_ev_draws) && length(dd$display_asset_cur_ev_draws) > 0) {
  display_asset_cur_ev_draws <- dd$display_asset_cur_ev_draws
} else {
  display_asset_cur_ev_draws <- build_display_matrix_app(asset_cur_ev_draws, pick_display_members, pick_display_assets)
}

if (!is.null(dd$display_asset_new_ev_draws) && length(dd$display_asset_new_ev_draws) > 0) {
  display_asset_new_ev_draws <- dd$display_asset_new_ev_draws
} else {
  display_asset_new_ev_draws <- build_display_matrix_app(asset_new_ev_draws, pick_display_members, pick_display_assets)
}

if (!is.null(dd$display_convey_cur_draws) && length(dd$display_convey_cur_draws) > 0) {
  display_convey_cur_draws <- dd$display_convey_cur_draws
} else {
  display_convey_cur_draws <- build_display_matrix_app(asset_convey_cur_draws, pick_display_members, pick_display_assets)
}

if (!is.null(dd$display_convey_new_draws) && length(dd$display_convey_new_draws) > 0) {
  display_convey_new_draws <- dd$display_convey_new_draws
} else {
  display_convey_new_draws <- build_display_matrix_app(asset_convey_new_draws, pick_display_members, pick_display_assets)
}

pick_display_value_summary <- if (!is.null(dd$pick_display_value_summary) && nrow(dd$pick_display_value_summary) > 0) {
  dd$pick_display_value_summary
} else {
  summarise_pick_draws_app(pick_display_assets, display_asset_cur_draws, display_asset_new_draws, "display_asset_id") %>%
    mutate(
      cur_convey_prob = colMeans(display_convey_cur_draws > 0)[display_asset_id],
      new_convey_prob = colMeans(display_convey_new_draws > 0)[display_asset_id],
      cur_expected_pick_count = colMeans(display_convey_cur_draws)[display_asset_id],
      new_expected_pick_count = colMeans(display_convey_new_draws)[display_asset_id]
    )
}

pick_display_value_ev_summary <- if (!is.null(dd$pick_display_value_ev_summary) && nrow(dd$pick_display_value_ev_summary) > 0) {
  dd$pick_display_value_ev_summary
} else {
  summarise_pick_draws_app(pick_display_assets, display_asset_cur_ev_draws, display_asset_new_ev_draws, "display_asset_id") %>%
    mutate(
      cur_convey_prob = colMeans(display_convey_cur_draws > 0)[display_asset_id],
      new_convey_prob = colMeans(display_convey_new_draws > 0)[display_asset_id],
      cur_expected_pick_count = colMeans(display_convey_cur_draws)[display_asset_id],
      new_expected_pick_count = colMeans(display_convey_new_draws)[display_asset_id]
    )
}

# Backward-compatible display cleanup: older dashboard_data.rds exports can
# contain internal own/retained rows that never convey under either system.
# Those rows are allocator artifacts, not real user-facing pick entitlements,
# so remove them from Single Pick / Trade Machine selectors and summaries.
expected_display_count_app <- function(mat, ids) {
  vals <- setNames(rep(0, length(ids)), ids)
  if (!is.null(mat) && length(mat) > 0 && ncol(mat) > 0) {
    cm <- colMeans(mat, na.rm = TRUE)
    hit <- intersect(names(cm), ids)
    vals[hit] <- cm[hit]
  }
  vals
}

filter_display_matrix_app <- function(mat, ids) {
  if (is.null(mat) || length(mat) == 0) return(mat)
  ids <- ids[ids %in% colnames(mat)]
  mat[, ids, drop = FALSE]
}

display_convey_audit_app <- tibble(
  display_asset_id = pick_display_assets$display_asset_id,
  cur_expected_pick_count = expected_display_count_app(display_convey_cur_draws, pick_display_assets$display_asset_id),
  new_expected_pick_count = expected_display_count_app(display_convey_new_draws, pick_display_assets$display_asset_id)
) %>%
  mutate(active_display_asset = .data$cur_expected_pick_count > 0 | .data$new_expected_pick_count > 0)

hidden_zero_convey_display_assets_app <- pick_display_assets %>%
  left_join(display_convey_audit_app, by = "display_asset_id") %>%
  filter(!.data$active_display_asset)

active_display_ids_app <- display_convey_audit_app %>%
  filter(.data$active_display_asset) %>%
  pull(display_asset_id)

if (nrow(hidden_zero_convey_display_assets_app) > 0) {
  message(sprintf(
    "Hiding %d zero-conveyance display-only pick rows from user-facing selectors",
    nrow(hidden_zero_convey_display_assets_app)
  ))
}

pick_display_assets <- pick_display_assets %>%
  filter(.data$display_asset_id %in% active_display_ids_app)
pick_display_members <- pick_display_members %>%
  semi_join(pick_display_assets %>% select(display_asset_id), by = "display_asset_id")
pick_display_value_summary <- pick_display_value_summary %>%
  filter(.data$display_asset_id %in% active_display_ids_app)
pick_display_value_ev_summary <- pick_display_value_ev_summary %>%
  filter(.data$display_asset_id %in% active_display_ids_app)

display_asset_cur_draws <- filter_display_matrix_app(display_asset_cur_draws, active_display_ids_app)
display_asset_new_draws <- filter_display_matrix_app(display_asset_new_draws, active_display_ids_app)
display_asset_cur_ev_draws <- filter_display_matrix_app(display_asset_cur_ev_draws, active_display_ids_app)
display_asset_new_ev_draws <- filter_display_matrix_app(display_asset_new_ev_draws, active_display_ids_app)
display_convey_cur_draws <- filter_display_matrix_app(display_convey_cur_draws, active_display_ids_app)
display_convey_new_draws <- filter_display_matrix_app(display_convey_new_draws, active_display_ids_app)

build_team_summary_from_asset_draws_app <- function(cur_mat, new_mat, base_summary) {
  teams <- base_summary$team
  out <- purrr::map_dfr(teams, function(tm) {
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
      n_picks_mean   = base_summary$n_picks_mean[match(tm, base_summary$team)],
      best_current   = mean(cur_best, na.rm = TRUE),
      best_new       = mean(new_best, na.rm = TRUE),
      delta_value    = mean(new_total) - mean(cur_total),
      delta_pct      = (mean(new_total) / pmax(mean(cur_total), 0.01) - 1) * 100,
      delta_quality  = mean(new_best, na.rm = TRUE) - mean(cur_best, na.rm = TRUE),
      sigma_change   = sd(new_total) - sd(cur_total)
    )
  })

  out %>%
    left_join(
      base_summary %>% select(team, tier, wins, losses, overall_rank),
      by = "team"
    ) %>%
    arrange(desc(delta_value))
}

summary_ev_df <- if (!is.null(dd$summary_ev) && nrow(dd$summary_ev) > 0) {
  dd$summary_ev
} else {
  build_team_summary_from_asset_draws_app(
    asset_cur_ev_draws,
    asset_new_ev_draws,
    summary_outcome_df
  )
}

value_mode_choices <- c(
  "4-Yr Win Share Outcomes" = "outcome",
  "Expected Pick Value" = "ev"
)

value_mode_label <- function(mode) {
  if (identical(mode, "ev")) "Expected Pick Value" else "4-Yr Win Share Outcomes"
}

value_mode_short_label <- function(mode) {
  if (identical(mode, "ev")) "expected pick value" else "realized player outcomes"
}

value_mode_unit <- function(mode) {
  if (identical(mode, "ev")) "EPV" else "4-Yr WS"
}

summary_for_value_mode <- function(mode) {
  if (identical(mode, "ev")) summary_ev_df else summary_outcome_df
}


# User-facing short pick labels and actual-slot tags used by Single Pick and Trade Machine.
# The display registry can group multiple internal conditional legs; these helpers keep
# dropdown labels concise while preserving the underlying RealGM-style entitlement.
display_fixed_slot_tbl <- pick_display_members %>%
  left_join(pick_assets %>% select(asset_id, fixed_slot), by = "asset_id") %>%
  group_by(display_asset_id) %>%
  summarise(
    fixed_slot_display = {
      slots <- sort(unique(fixed_slot[is.finite(fixed_slot)]))
      if (length(slots) == 0L) NA_character_ else paste0("#", paste(slots, collapse = "/#"))
    },
    .groups = "drop"
  )

pick_display_assets <- pick_display_assets %>%
  select(-any_of("fixed_slot_display")) %>%
  left_join(display_fixed_slot_tbl, by = "display_asset_id") %>%
  mutate(fixed_slot_display = coalesce(.data$fixed_slot_display, NA_character_))

split_abbrs <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  out <- unlist(str_split(x, "\\s*,\\s*"), use.names = FALSE)
  out[!is.na(out) & nzchar(out)]
}

protection_ranges_app <- function(protection, round = 1L) {
  protection <- stringr::str_trim(as.character(protection %||% NA_character_))
  draft_round <- suppressWarnings(as.integer(round %||% NA_integer_))
  if (is.na(draft_round)) draft_round <- 1L
  invalid <- list(keep = NA_character_, convey = NA_character_)

  if (is.na(protection) || protection %in% c("", "none", "complex")) {
    return(invalid)
  }

  # Round 1 picks can only have first-round protection language like top-2,
  # top-4, lottery, etc. Range protections such as 31-45 / 51-60 are second-
  # round conveyance clauses and should never be projected onto a first-round
  # own-pick label.
  top_map <- c(top1 = 1L, top2 = 2L, top3 = 3L, top4 = 4L, top5 = 5L,
               top6 = 6L, top8 = 8L, top10 = 10L, lottery = 14L,
               top16 = 16L, top20 = 20L)
  if (protection %in% names(top_map)) {
    if (draft_round != 1L) return(invalid)
    n <- unname(top_map[protection])
    keep <- sprintf("1-%d", n)
    convey <- if (n < 30L) sprintf("%d-30", n + 1L) else NA_character_
    return(list(keep = keep, convey = convey))
  }

  m <- stringr::str_match(protection, "^protected(\\d+)_(\\d+)$")
  if (!is.na(m[1, 1])) {
    lo <- as.integer(m[1, 2]); hi <- as.integer(m[1, 3])
    if (draft_round != 2L || lo < 31L || hi > 60L) return(invalid)
    keep <- sprintf("%d-%d", lo, hi)
    convey <- if (hi < 60L) sprintf("%d-60", hi + 1L) else NA_character_
    return(list(keep = keep, convey = convey))
  }

  m <- stringr::str_match(protection, "^convey(\\d+)_(\\d+)$")
  if (!is.na(m[1, 1])) {
    lo <- as.integer(m[1, 2]); hi <- as.integer(m[1, 3])
    if (draft_round != 2L || lo < 31L || hi > 60L) return(invalid)
    convey <- sprintf("%d-%d", lo, hi)
    keep <- if (lo <= 31L && hi < 60L) sprintf("%d-60", hi + 1L)
    else if (lo > 31L) sprintf("31-%d", lo - 1L)
    else NA_character_
    return(list(keep = keep, convey = convey))
  }

  invalid
}

protected_outgoing_lookup_app <- pick_assets %>%
  filter(!is.na(.data$protection), !.data$protection %in% c("", "none", "complex"),
         !is.na(.data$owner), !is.na(.data$original_team), .data$owner != .data$original_team) %>%
  mutate(
    keep_range = purrr::map2_chr(.data$protection, .data$round, ~ protection_ranges_app(.x, .y)$keep),
    convey_range = purrr::map2_chr(.data$protection, .data$round, ~ protection_ranges_app(.x, .y)$convey),
    valid_round_protection = !is.na(.data$keep_range) | !is.na(.data$convey_range)
  ) %>%
  filter(.data$valid_round_protection) %>%
  group_by(.data$year, .data$round, .data$original_team) %>%
  summarise(
    recipient = dplyr::first(.data$owner),
    protection = dplyr::first(.data$protection),
    keep_range = dplyr::first(.data$keep_range),
    convey_range = dplyr::first(.data$convey_range),
    .groups = "drop"
  )


range_valid_for_round_app <- function(range_txt, draft_round) {
  range_txt <- stringr::str_trim(as.character(range_txt %||% NA_character_))
  if (is.na(range_txt) || !nzchar(range_txt)) return(TRUE)
  m <- stringr::str_match(range_txt, "^(\\d+)\\s*-\\s*(\\d+)$")
  if (is.na(m[1, 1])) return(TRUE)
  lo <- suppressWarnings(as.integer(m[1, 2]))
  hi <- suppressWarnings(as.integer(m[1, 3]))
  draft_round <- suppressWarnings(as.integer(draft_round %||% NA_integer_))
  if (is.na(lo) || is.na(hi) || is.na(draft_round)) return(FALSE)
  if (draft_round == 1L) return(lo >= 1L && hi <= 30L)
  if (draft_round == 2L) return(lo >= 31L && hi <= 60L)
  FALSE
}

protected_pick_label_app <- function(year, round, owner, original_team, slot_txt = "") {
  if (is.na(year) || is.na(round) || is.na(owner) || is.na(original_team)) return(NA_character_)
  row <- protected_outgoing_lookup_app %>%
    filter(.data$year == as.integer(year),
           .data$round == as.integer(round),
           .data$original_team == .env$original_team)
  if (nrow(row) == 0L) return(NA_character_)
  row <- row[1, ]
  # Defensive guard: bad upstream display rows can occasionally attach a
  # second-round range protection (31-45, 31-50, etc.) to the original team's
  # first-round own pick label. Never display a protection range that is outside
  # the selected draft round's slot universe.
  if (!range_valid_for_round_app(row$keep_range, round) ||
      !range_valid_for_round_app(row$convey_range, round)) {
    return(NA_character_)
  }
  rnd <- paste0("R", as.integer(round))

  if (identical(owner, original_team) && !is.na(row$keep_range) && nzchar(row$keep_range)) {
    return(stringr::str_squish(sprintf("%s%s own if %s, otherwise to %s", rnd, slot_txt, row$keep_range, row$recipient)))
  }
  if (identical(owner, row$recipient) && !is.na(row$convey_range) && nzchar(row$convey_range)) {
    return(stringr::str_squish(sprintf("%s%s via %s if %s", rnd, slot_txt, original_team, row$convey_range)))
  }
  NA_character_
}

pick_obligation_display_app <- function(r) {
  if (is.null(r) || nrow(r) == 0L) return(NA_character_)
  origs <- split_abbrs(r$member_original_teams)
  if (length(origs) == 1L) {
    special <- protected_pick_label_app(r$year, r$round, r$owner, origs[1], r$fixed_slot_display %||% "")
    if (!is.na(special) && nzchar(special)) return(special)
  }
  as.character(r$obligation %||% NA_character_)
}

extract_swap_terms_from_text <- function(text, original_teams = character(0), owner = NA_character_) {
  text <- as.character(text %||% "")
  text[is.na(text)] <- ""
  holder <- str_match(text, "via\\s+([A-Z]{2,3})\\s+swap")[, 2]
  if (is.na(holder)) holder <- str_match(text, "([A-Z]{2,3})\\s+may\\s+swap")[, 2]
  if (is.na(holder)) holder <- str_match(text, "after\\s+([A-Z]{2,3})['’]?s\\s+swap")[, 2]

  target <- str_match(text, "swap\\s+for\\s+([A-Z]{2,3})")[, 2]
  if (is.na(target)) target <- str_match(text, "swap\\s+with\\s+([A-Z]{2,3})")[, 2]
  if (is.na(target) && !is.na(holder)) {
    other <- setdiff(original_teams, holder)
    if (length(other) > 0) target <- other[1]
  }
  if (is.na(target) && length(original_teams) > 0 && !is.na(owner)) {
    other <- setdiff(original_teams, owner)
    if (length(other) > 0) target <- other[1]
  }

  min_pick <- 1L
  max_pick <- 30L
  if (!is.na(target)) {
    rg <- str_match(text, paste0(target, "\\s+(\\d+)-(\\d+)"))
    if (!is.na(rg[1, 1])) {
      min_pick <- as.integer(rg[1, 2])
      max_pick <- as.integer(rg[1, 3])
    }
  }
  if (str_detect(text, regex("keeps #1|protected #1", ignore_case = TRUE)) && min_pick == 1L) {
    min_pick <- 2L
  }
  if (str_detect(text, regex("keeps 1-2|protected 1-2", ignore_case = TRUE)) && min_pick == 1L) {
    min_pick <- 3L
  }
  if (str_detect(text, regex("keeps 1-3|protected 1-3", ignore_case = TRUE)) && min_pick == 1L) {
    min_pick <- 4L
  }

  list(holder = holder, target = target, min_pick = min_pick, max_pick = max_pick)
}

display_pick_short_label <- function(year, round, owner, member_original_teams,
                                     group_type, label, obligation, notes,
                                     fixed_slot_display = NA_character_) {
  rnd <- paste0("R", as.integer(round))
  slot_txt <- if (!is.na(fixed_slot_display) && nzchar(fixed_slot_display)) {
    paste0(" ", fixed_slot_display)
  } else {
    ""
  }
  origs <- split_abbrs(member_original_teams)
  if (length(origs) == 0L) origs <- owner

  text <- paste(label %||% "", obligation %||% "", notes %||% "")

  if (length(origs) == 1L) {
    protected_label <- protected_pick_label_app(year, round, owner, origs[1], slot_txt)
    if (!is.na(protected_label) && nzchar(protected_label)) return(protected_label)
  }

  terms <- extract_swap_terms_from_text(text, origs, owner)
  if (!is.na(terms$holder) && !is.na(terms$target)) {
    # Label swap entitlements from the selected owner's perspective. The swap
    # holder sees "own or counterparty"; the non-holder still gets a guaranteed
    # pick and should see "own or holder" after the holder's swap right.
    counterparty <- if (!is.na(owner) && identical(owner, terms$holder)) {
      candidates <- setdiff(origs, terms$holder)
      if (length(candidates) > 0L) candidates[1] else terms$target
    } else if (!is.na(owner) && !identical(owner, terms$holder)) {
      terms$holder
    } else {
      candidates <- setdiff(origs, owner)
      if (length(candidates) > 0L) candidates[1] else terms$target
    }
    return(str_squish(sprintf("%s%s own or %s via %s swap", rnd, slot_txt, counterparty, terms$holder)))
  }

  simple_group <- is.na(group_type) || group_type %in% c("single_asset", "own", "outright")
  if (length(origs) == 1L && simple_group) {
    via <- if (identical(origs[1], owner)) "own" else paste("via", origs[1])
    return(str_squish(sprintf("%s%s %s", rnd, slot_txt, via)))
  }

  if (length(origs) == 1L) {
    via <- if (identical(origs[1], owner)) "own" else paste("via", origs[1])
    return(str_squish(sprintf("%s%s %s", rnd, slot_txt, via)))
  }

  label_clean <- str_remove(as.character(label %||% ""), paste0("^", year, "\\s+"))
  str_squish(sprintf("%s%s %s", rnd, slot_txt, label_clean))
}

fallback_simple_pick_label_app <- function(round, owner, member_original_teams, fixed_slot_display = NA_character_) {
  rnd <- paste0("R", as.integer(round))
  slot_txt <- if (!is.na(fixed_slot_display) && nzchar(fixed_slot_display)) {
    paste0(" ", fixed_slot_display)
  } else {
    ""
  }
  origs <- split_abbrs(member_original_teams)
  if (length(origs) == 0L) origs <- owner
  if (length(origs) == 1L) {
    via <- if (identical(origs[1], owner)) "own" else paste("via", origs[1])
    return(stringr::str_squish(sprintf("%s%s %s", rnd, slot_txt, via)))
  }
  stringr::str_squish(sprintf("%s%s %s", rnd, slot_txt, paste(origs, collapse = ", ")))
}

sanitize_round_protection_label_app <- function(short_label, round, owner, member_original_teams, fixed_slot_display = NA_character_) {
  short_label <- as.character(short_label %||% "")
  draft_round <- suppressWarnings(as.integer(round %||% NA_integer_))
  if (is.na(draft_round)) return(short_label)

  # The label generator should be round-aware, but this final display pass keeps
  # impossible labels out of every dropdown/table even if an older RDS export has
  # stale or malformed protection metadata. Examples removed:
  #   R1 own if 31-45, otherwise to NYK
  #   2026 R1 #3 own if 31-50, otherwise to MIN
  has_second_round_range <- stringr::str_detect(short_label, "\\b(?:3[1-9]|[4-6][0-9])\\s*-\\s*(?:3[1-9]|[4-6][0-9])\\b")
  has_first_round_range  <- stringr::str_detect(short_label, "\\b(?:[1-9]|[12][0-9]|30)\\s*-\\s*(?:[1-9]|[12][0-9]|30)\\b")
  protected_phrase <- stringr::str_detect(short_label, regex("\\bif\\b|otherwise to", ignore_case = TRUE))

  if (draft_round == 1L && protected_phrase && has_second_round_range) {
    return(fallback_simple_pick_label_app(round, owner, member_original_teams, fixed_slot_display))
  }
  if (draft_round == 2L && protected_phrase && has_first_round_range) {
    return(fallback_simple_pick_label_app(round, owner, member_original_teams, fixed_slot_display))
  }
  short_label
}

pick_display_assets <- pick_display_assets %>%
  rowwise() %>%
  mutate(
    short_label = display_pick_short_label(
      year, round, owner, member_original_teams,
      group_type, label, obligation, notes, fixed_slot_display
    ),
    short_label = sanitize_round_protection_label_app(
      short_label, round, owner, member_original_teams, fixed_slot_display
    ),
    trade_label = str_squish(sprintf("%d %s", year, short_label))
  ) %>%
  ungroup()

team_delta_draw_summary_app <- function(cur_mat, new_mat, asset_owner_tbl, teams, mode = c("total", "quality")) {
  mode <- match.arg(mode)
  purrr::map_dfr(teams, function(tm) {
    ids <- asset_owner_tbl %>% filter(.data$owner == .env$tm) %>% pull(asset_id)
    ids <- ids[ids %in% colnames(cur_mat) & ids %in% colnames(new_mat)]
    if (length(ids) == 0L) {
      delta <- rep(0, nrow(new_mat))
    } else if (mode == "quality") {
      cur_sub <- cur_mat[, ids, drop = FALSE]
      new_sub <- new_mat[, ids, drop = FALSE]
      cur_val <- if (ncol(cur_sub) == 1L) as.numeric(cur_sub[, 1]) else apply(cur_sub, 1, max, na.rm = TRUE)
      new_val <- if (ncol(new_sub) == 1L) as.numeric(new_sub[, 1]) else apply(new_sub, 1, max, na.rm = TRUE)
      cur_val[!is.finite(cur_val)] <- 0
      new_val[!is.finite(new_val)] <- 0
      delta <- new_val - cur_val
    } else {
      delta <- rowSums(new_mat[, ids, drop = FALSE], na.rm = TRUE) -
        rowSums(cur_mat[, ids, drop = FALSE], na.rm = TRUE)
    }
    tibble(
      team = tm,
      delta_mean = mean(delta, na.rm = TRUE),
      delta_q05  = as.numeric(quantile(delta, 0.05, na.rm = TRUE)),
      delta_q95  = as.numeric(quantile(delta, 0.95, na.rm = TRUE))
    )
  })
}

team_quantity_delta_summary_app <- function(cur_convey_mat, new_convey_mat, display_assets, teams) {
  purrr::map_dfr(teams, function(tm) {
    ids <- display_assets %>% filter(.data$owner == .env$tm) %>% pull(display_asset_id)
    ids <- ids[ids %in% colnames(cur_convey_mat) & ids %in% colnames(new_convey_mat)]
    if (length(ids) == 0L) {
      delta <- rep(0, nrow(new_convey_mat))
    } else {
      delta <- rowSums(new_convey_mat[, ids, drop = FALSE], na.rm = TRUE) -
        rowSums(cur_convey_mat[, ids, drop = FALSE], na.rm = TRUE)
    }
    tibble(
      team = tm,
      delta_mean = mean(delta, na.rm = TRUE),
      delta_q05  = as.numeric(quantile(delta, 0.05, na.rm = TRUE)),
      delta_q95  = as.numeric(quantile(delta, 0.95, na.rm = TRUE))
    )
  })
}

all_summary_teams <- sort(unique(summary_outcome_df$team))
team_delta_outcome_total <- team_delta_draw_summary_app(asset_cur_draws, asset_new_draws, pick_assets, all_summary_teams, "total")
team_delta_ev_total <- team_delta_draw_summary_app(asset_cur_ev_draws, asset_new_ev_draws, pick_assets, all_summary_teams, "total")
team_delta_outcome_quality <- team_delta_draw_summary_app(asset_cur_draws, asset_new_draws, pick_assets, all_summary_teams, "quality")
team_delta_ev_quality <- team_delta_draw_summary_app(asset_cur_ev_draws, asset_new_ev_draws, pick_assets, all_summary_teams, "quality")
team_delta_quantity <- team_quantity_delta_summary_app(display_convey_cur_draws, display_convey_new_draws, pick_display_assets, all_summary_teams)

team_delta_for_mode <- function(value_mode, metric = "total") {
  if (identical(metric, "quantity")) return(team_delta_quantity)
  if (identical(value_mode, "ev") && identical(metric, "quality")) return(team_delta_ev_quality)
  if (identical(value_mode, "ev")) return(team_delta_ev_total)
  if (identical(metric, "quality")) return(team_delta_outcome_quality)
  team_delta_outcome_total
}


# Display-level portfolio summaries used by the redesigned Impact tab and
# pick-mover tables. These intentionally use display assets so mutually
# exclusive RealGM-style entitlements are not double-counted in user-facing views.
value_mats_for_mode_app <- function(value_mode, display = FALSE) {
  if (isTRUE(display)) {
    if (identical(value_mode, "ev")) {
      return(list(cur = display_asset_cur_ev_draws, new = display_asset_new_ev_draws))
    }
    return(list(cur = display_asset_cur_draws, new = display_asset_new_draws))
  }
  if (identical(value_mode, "ev")) {
    return(list(cur = asset_cur_ev_draws, new = asset_new_ev_draws))
  }
  list(cur = asset_cur_draws, new = asset_new_draws)
}

team_draw_summary_for_table_app <- function(value_mode, teams = all_summary_teams) {
  mats <- value_mats_for_mode_app(value_mode, display = FALSE)
  purrr::map_dfr(teams, function(tm) {
    ids <- pick_assets %>% filter(.data$owner == .env$tm) %>% pull(asset_id)
    ids <- ids[ids %in% colnames(mats$cur) & ids %in% colnames(mats$new)]
    if (length(ids) == 0L) {
      cur_total <- rep(0, nrow(mats$new))
      new_total <- rep(0, nrow(mats$new))
    } else {
      cur_total <- rowSums(mats$cur[, ids, drop = FALSE], na.rm = TRUE)
      new_total <- rowSums(mats$new[, ids, drop = FALSE], na.rm = TRUE)
    }
    delta <- new_total - cur_total
    tibble(
      team = tm,
      p_positive = mean(delta > 0, na.rm = TRUE),
      new_q10 = as.numeric(quantile(new_total, 0.10, na.rm = TRUE)),
      new_q90 = as.numeric(quantile(new_total, 0.90, na.rm = TRUE)),
      delta_q10 = as.numeric(quantile(delta, 0.10, na.rm = TRUE)),
      delta_q90 = as.numeric(quantile(delta, 0.90, na.rm = TRUE))
    )
  })
}

portfolio_quality_quantity_summary_app <- function(value_mode, year_filter = "All", round_filter = "All") {
  mats <- value_mats_for_mode_app(value_mode, display = TRUE)
  assets <- pick_display_assets
  if (!is.null(year_filter) && !identical(year_filter, "All")) {
    assets <- assets %>% filter(.data$year == as.integer(.env$year_filter))
  }
  if (!is.null(round_filter) && !identical(round_filter, "All")) {
    assets <- assets %>% filter(.data$round == as.integer(.env$round_filter))
  }

  purrr::map_dfr(all_summary_teams, function(tm) {
    ids <- assets %>% filter(.data$owner == .env$tm) %>% pull(display_asset_id)
    value_ids <- ids[ids %in% colnames(mats$cur) & ids %in% colnames(mats$new)]
    count_ids <- ids[ids %in% colnames(display_convey_cur_draws) & ids %in% colnames(display_convey_new_draws)]

    if (length(value_ids) == 0L) {
      cur_total <- rep(0, nrow(mats$new))
      new_total <- rep(0, nrow(mats$new))
    } else {
      cur_total <- rowSums(mats$cur[, value_ids, drop = FALSE], na.rm = TRUE)
      new_total <- rowSums(mats$new[, value_ids, drop = FALSE], na.rm = TRUE)
    }

    if (length(count_ids) == 0L) {
      cur_count_draw <- rep(0, nrow(mats$new))
      new_count_draw <- rep(0, nrow(mats$new))
    } else {
      # Count each user-facing display entitlement at most once per simulation.
      # Grouped swap/conditional legs can have multiple internal rows, but the
      # user-facing pick count should still be one pick when any leg conveys.
      cur_count_draw <- rowSums((display_convey_cur_draws[, count_ids, drop = FALSE] > 0) * 1, na.rm = TRUE)
      new_count_draw <- rowSums((display_convey_new_draws[, count_ids, drop = FALSE] > 0) * 1, na.rm = TRUE)
    }

    cur_total_mean <- mean(cur_total, na.rm = TRUE)
    new_total_mean <- mean(new_total, na.rm = TRUE)
    cur_count <- mean(cur_count_draw, na.rm = TRUE)
    new_count <- mean(new_count_draw, na.rm = TRUE)

    # The x-axis is average value per user-facing pick entitlement. Expected
    # pick count remains the y-axis.
    display_pick_count <- length(unique(ids))
    cur_avg_quality <- ifelse(display_pick_count > 0L, cur_total_mean / display_pick_count, 0)
    new_avg_quality <- ifelse(display_pick_count > 0L, new_total_mean / display_pick_count, 0)
    delta_total <- new_total_mean - cur_total_mean
    delta_count <- new_count - cur_count
    delta_quality <- new_avg_quality - cur_avg_quality

    tibble(
      team = tm,
      display_pick_count = display_pick_count,
      cur_total_value = cur_total_mean,
      new_total_value = new_total_mean,
      delta_total_value = delta_total,
      cur_expected_picks = cur_count,
      new_expected_picks = new_count,
      delta_expected_picks = delta_count,
      cur_avg_quality = cur_avg_quality,
      new_avg_quality = new_avg_quality,
      delta_avg_quality = delta_quality,
      quantity_effect = cur_avg_quality * delta_count,
      quality_effect = cur_count * delta_quality,
      interaction_effect = delta_count * delta_quality,
      p_positive = mean((new_total - cur_total) > 0, na.rm = TRUE),
      delta_q05 = as.numeric(quantile(new_total - cur_total, 0.05, na.rm = TRUE)),
      delta_q95 = as.numeric(quantile(new_total - cur_total, 0.95, na.rm = TRUE)),
      bubble_size = pmax(new_total_mean, cur_total_mean, 0.05)
    )
  })
}


classify_pick_bucket_app <- function(owner, member_original_teams, short_label, group_type, obligation, notes, label) {
  owner <- as.character(owner %||% "")
  origs_txt <- as.character(member_original_teams %||% "")
  short_txt_raw <- as.character(short_label %||% "")
  group_txt <- as.character(group_type %||% "")
  obligation_txt <- as.character(obligation %||% "")
  notes_txt <- as.character(notes %||% "")
  label_txt <- as.character(label %||% "")

  owner[is.na(owner)] <- ""
  origs_txt[is.na(origs_txt)] <- ""
  short_txt_raw[is.na(short_txt_raw)] <- ""
  group_txt[is.na(group_txt)] <- ""
  obligation_txt[is.na(obligation_txt)] <- ""
  notes_txt[is.na(notes_txt)] <- ""
  label_txt[is.na(label_txt)] <- ""

  origs <- split_abbrs(origs_txt)
  own_team <- isTRUE(nzchar(owner)) && owner %in% origs
  short_txt <- stringr::str_to_lower(short_txt_raw)
  txt <- stringr::str_to_lower(paste(group_txt, obligation_txt, notes_txt, label_txt, short_txt_raw))

  # Plain incoming picks should stay in the outright bucket. This catches labels
  # like "R1 via NYK" / "R2 via DEN" before broader obligation text can push
  # them into swaps/protections.
  if (stringr::str_detect(short_txt, "^r[12](\\s+#[0-9/]+)?\\s+via\\s+[a-z]{2,3}(\\s+if\\s+[0-9]+-[0-9]+)?$") &&
      !stringr::str_detect(short_txt, "swap|own or|otherwise|protect|conditional|complex|favorable|ranked|pool")) {
    return("Incoming outright picks")
  }

  # Labels that explicitly describe retained own protection, swap rights, ranked
  # pools, or conditional branches are not plain incoming outright picks.
  if (stringr::str_detect(short_txt, "own or|swap|otherwise|protect|conditional|complex|favorable|ranked|pool")) {
    return("Swaps / protections")
  }
  if (stringr::str_detect(txt, "swap|protect|protected|conditional|complex|favorable|ranked|pool")) {
    # If the displayed asset is simply another team's pick coming in and does
    # not itself mention a condition, keep it as incoming outright despite noisy
    # underlying notes/group labels.
    if (!own_team && stringr::str_detect(short_txt, "^r[12].*\\bvia\\b") &&
        !stringr::str_detect(short_txt, "if|swap|own or|otherwise")) {
      return("Incoming outright picks")
    }
    return("Swaps / protections")
  }
  if (own_team) return("Own picks")
  "Incoming outright picks"
}

pick_impact_rows_app <- function(value_mode) {
  smry <- if (identical(value_mode, "ev")) pick_display_value_ev_summary else pick_display_value_summary

  asset_meta <- pick_display_assets %>%
    transmute(
      display_asset_id,
      owner = .data$owner,
      year = .data$year,
      round = .data$round,
      short_label = .data$short_label,
      trade_label = .data$trade_label,
      label = .data$label,
      obligation = .data$obligation,
      notes = .data$notes,
      group_type = .data$group_type,
      member_original_teams = .data$member_original_teams
    )

  out <- smry %>%
    select(-any_of(c("owner", "year", "round", "short_label", "trade_label",
                     "label", "obligation", "notes", "group_type",
                     "member_original_teams"))) %>%
    left_join(asset_meta, by = "display_asset_id")

  if (!"delta" %in% names(out)) {
    out <- out %>% mutate(delta = .data$new_mean - .data$cur_mean)
  } else {
    out <- out %>% mutate(delta = coalesce(.data$delta, .data$new_mean - .data$cur_mean))
  }

  out %>%
    rowwise() %>%
    mutate(
      impact_bucket = classify_pick_bucket_app(
        owner = .data$owner,
        member_original_teams = .data$member_original_teams,
        short_label = .data$short_label,
        group_type = .data$group_type,
        obligation = .data$obligation,
        notes = .data$notes,
        label = .data$label
      )
    ) %>%
    ungroup()
}



# Internal asset-level breakout for Full Table details. The user-facing display
# registry groups swap/protection entitlements, which is appropriate for menus,
# but it can hide retained own picks inside complex groups. For the bucket
# breakout, use the underlying asset registry so own picks, plain incoming
# outrights, and swap/protection legs are counted in the correct buckets.
pick_internal_breakout_rows_app <- function(value_mode) {
  smry <- if (identical(value_mode, "ev")) pick_value_ev_summary else pick_value_summary

  asset_meta <- pick_assets %>%
    select(any_of(c("asset_id", "owner", "original_team", "year", "round",
                    "pick_type", "protection", "complex_group", "notes", "label")))

  out <- smry %>%
    select(-any_of(c("owner", "original_team", "year", "round", "pick_type",
                     "protection", "complex_group", "notes", "label"))) %>%
    left_join(asset_meta, by = "asset_id")

  for (nm in c("owner", "original_team", "pick_type", "protection", "complex_group", "notes", "label")) {
    if (!nm %in% names(out)) out[[nm]] <- NA_character_
  }
  if (!"round" %in% names(out)) out$round <- NA_integer_
  if (!"year" %in% names(out)) out$year <- NA_integer_

  if (!"delta" %in% names(out)) {
    out <- out %>% mutate(delta = .data$new_mean - .data$cur_mean)
  } else {
    out <- out %>% mutate(delta = coalesce(.data$delta, .data$new_mean - .data$cur_mean))
  }

  out %>%
    mutate(
      owner = as.character(.data$owner),
      original_team = as.character(.data$original_team),
      pick_type = as.character(.data$pick_type),
      protection = as.character(.data$protection),
      complex_group = as.character(.data$complex_group),
      impact_bucket = case_when(
        !is.na(.data$owner) & !is.na(.data$original_team) & .data$owner == .data$original_team ~ "Own picks",
        !is.na(.data$pick_type) & .data$pick_type == "outright" &
          (is.na(.data$protection) | .data$protection == "none" | .data$protection == "") &
          (is.na(.data$complex_group) | .data$complex_group == "") ~ "Incoming outright picks",
        TRUE ~ "Swaps / protections"
      )
    )
}

team_slot_array_for_round_app <- function(round, system = c("new", "cur")) {
  system <- match.arg(system)
  if (as.integer(round) == 2L) {
    arr <- if (system == "new") team_slot2_new_draws else team_slot2_cur_draws
    if (!is.null(arr)) return(arr)
  }
  if (system == "new") team_slot_new_draws else team_slot_cur_draws
}

probability_summary_from_indicator <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(tibble(prob = NA_real_, q05 = NA_real_, q95 = NA_real_))
  }
  p <- mean(x > 0, na.rm = TRUE)
  se <- sqrt(pmax(p * (1 - p) / length(x), 0))
  tibble(
    prob = p,
    q05 = pmax(0, p - 1.645 * se),
    q95 = pmin(1, p + 1.645 * se)
  )
}

# Team abbreviations present in the asset registry, sorted
all_team_abbr <- sort(unique(c(pick_display_assets$owner, pick_assets$owner, pick_assets$original_team)))

# protection options offered in the Trade Machine (top-N bands; 12-15 illegal)
protection_choices <- c(
  "None" = "none",
  "Top-1"  = "top1",  "Top-2"  = "top2",  "Top-3"  = "top3",  "Top-4"  = "top4",
  "Top-5"  = "top5",  "Top-6"  = "top6",  "Top-8"  = "top8",
  "Top-10" = "top10", "Lottery (top-14)" = "lottery",
  "Top-16" = "top16", "Top-20" = "top20",
  "2nd protected 31-45" = "protected31_45",
  "2nd protected 31-50" = "protected31_50",
  "2nd protected 31-55" = "protected31_55"
)
protection_floor_app <- function(p) {
  switch(p, top1 = 1, top2 = 2, top3 = 3, top4 = 4, top5 = 5, top6 = 6,
         top8 = 8, top10 = 10, lottery = 14, top16 = 16, top20 = 20,
         protected31_45 = 45, protected31_50 = 50, protected31_55 = 55, 0)
}

# Five 3-2-1 tiers, worst -> best
TIERS <- meta$tiers
tier_colors <- c(
  relegation   = "#dc2626",
  nonplayin    = "#ca8a04",
  playin_seed  = "#7c3aed",
  playin_loser = "#2563eb",
  playoff      = "#059669"
)
tier_labels <- c(
  relegation   = "Relegation (3 worst)",
  nonplayin    = "Non-Play-In",
  playin_seed  = "9/10 Seeds",
  playin_loser = "7v8 Losers",
  playoff      = "Playoff"
)
tier_short <- c(
  relegation   = "Releg.",
  nonplayin    = "Non-PI",
  playin_seed  = "9/10",
  playin_loser = "7v8 L",
  playoff      = "Playoff"
)


# NBA team display helpers ----------------------------------------------------
team_full_names_app <- c(
  ATL = "Atlanta Hawks", BOS = "Boston Celtics", BKN = "Brooklyn Nets",
  CHA = "Charlotte Hornets", CHI = "Chicago Bulls", CLE = "Cleveland Cavaliers",
  DAL = "Dallas Mavericks", DEN = "Denver Nuggets", DET = "Detroit Pistons",
  GSW = "Golden State Warriors", HOU = "Houston Rockets", IND = "Indiana Pacers",
  LAC = "LA Clippers", LAL = "Los Angeles Lakers", MEM = "Memphis Grizzlies",
  MIA = "Miami Heat", MIL = "Milwaukee Bucks", MIN = "Minnesota Timberwolves",
  NOP = "New Orleans Pelicans", NYK = "New York Knicks", OKC = "Oklahoma City Thunder",
  ORL = "Orlando Magic", PHI = "Philadelphia 76ers", PHX = "Phoenix Suns",
  POR = "Portland Trail Blazers", SAC = "Sacramento Kings", SAS = "San Antonio Spurs",
  TOR = "Toronto Raptors", UTA = "Utah Jazz", WAS = "Washington Wizards"
)

team_nba_ids_app <- c(
  ATL = "1610612737", BOS = "1610612738", BKN = "1610612751",
  CHA = "1610612766", CHI = "1610612741", CLE = "1610612739",
  DAL = "1610612742", DEN = "1610612743", DET = "1610612765",
  GSW = "1610612744", HOU = "1610612745", IND = "1610612754",
  LAC = "1610612746", LAL = "1610612747", MEM = "1610612763",
  MIA = "1610612748", MIL = "1610612749", MIN = "1610612750",
  NOP = "1610612740", NYK = "1610612752", OKC = "1610612760",
  ORL = "1610612753", PHI = "1610612755", PHX = "1610612756",
  POR = "1610612757", SAC = "1610612758", SAS = "1610612759",
  TOR = "1610612761", UTA = "1610612762", WAS = "1610612764"
)

team_full_name_app <- function(team) {
  team <- as.character(team)
  out <- team_full_names_app[team]
  out[is.na(out)] <- team[is.na(out)]
  unname(out)
}

team_logo_url_app <- function(team) {
  team <- as.character(team)
  ids <- team_nba_ids_app[team]
  out <- ifelse(!is.na(ids),
                paste0("https://cdn.nba.com/logos/nba/", ids, "/primary/L/logo.svg"),
                NA_character_)
  unname(out)
}

# Plotly layout images are more reliable with raster PNGs than the NBA SVG
# logo endpoint. Keep NBA SVGs for normal HTML <img> tags, but use ESPN PNGs
# for plot overlays such as the Impact scatterplot.
espn_team_slug_app <- c(
  ATL = "atl", BOS = "bos", BKN = "bkn", CHA = "cha", CHI = "chi",
  CLE = "cle", DAL = "dal", DEN = "den", DET = "det", GSW = "gs",
  HOU = "hou", IND = "ind", LAC = "lac", LAL = "lal", MEM = "mem",
  MIA = "mia", MIL = "mil", MIN = "min", NOP = "no", NYK = "ny",
  OKC = "okc", ORL = "orl", PHI = "phi", PHX = "phx", POR = "por",
  SAC = "sac", SAS = "sa", TOR = "tor", UTA = "utah", WAS = "wsh"
)

team_logo_plot_url_app <- function(team) {
  team <- as.character(team)
  slug <- espn_team_slug_app[team]
  out <- ifelse(!is.na(slug),
                paste0("https://a.espncdn.com/i/teamlogos/nba/500/", slug, ".png"),
                team_logo_url_app(team))
  unname(out)
}

team_logo_img_app <- function(team, size = 24, alt = NULL) {
  src <- team_logo_url_app(team)
  label <- alt %||% team_full_name_app(team)
  if (is.na(src) || !nzchar(src)) {
    return(tags$span(as.character(team)))
  }
  tags$img(
    src = src,
    alt = label,
    title = label,
    style = sprintf(
      "height:%dpx; width:%dpx; object-fit:contain; vertical-align:middle; flex:0 0 auto;",
      as.integer(size), as.integer(size)
    )
  )
}

team_logo_html_app <- function(team, size = 22, show_abbr = TRUE) {
  vapply(as.character(team), function(tm) {
    src <- team_logo_url_app(tm)
    logo <- if (!is.na(src) && nzchar(src)) {
      sprintf('<img src="%s" alt="%s" title="%s" style="height:%dpx;width:%dpx;object-fit:contain;vertical-align:middle;margin-right:8px;">',
              src, team_full_name_app(tm), team_full_name_app(tm), as.integer(size), as.integer(size))
    } else {
      ""
    }
    if (isTRUE(show_abbr)) {
      sprintf('<span class="team-logo-cell">%s<span>%s</span></span>', logo, tm)
    } else {
      logo
    }
  }, character(1))
}

team_header_tag_app <- function(team, size = 30, show_full = TRUE, suffix = NULL) {
  label <- if (isTRUE(show_full)) team_full_name_app(team) else as.character(team)
  tags$div(
    style = "display:flex; align-items:center; gap:10px; min-width:0;",
    team_logo_img_app(team, size = size),
    tags$span(style = "font-weight:850; white-space:normal;", label),
    if (!is.null(suffix)) tags$span(style = "font-weight:850; white-space:normal;", suffix)
  )
}

team_logo_layout_images_app <- function(df, x_col, y_col, sizex, sizey, opacity = 0.95) {
  if (is.null(df) || nrow(df) == 0L) return(list())
  purrr::map(seq_len(nrow(df)), function(i) {
    src <- team_logo_plot_url_app(df$team[i])
    if (is.na(src) || !nzchar(src)) return(NULL)
    list(
      source = src,
      xref = "x", yref = "y",
      x = df[[x_col]][i], y = df[[y_col]][i],
      xanchor = "center", yanchor = "middle",
      sizex = sizex, sizey = sizey,
      sizing = "contain",
      opacity = opacity,
      layer = "above"
    )
  }) %>% purrr::compact()
}

app_theme <- bs_theme(
  bg         = "#0a0a14",
  fg         = "#d0d0d0",
  primary    = "#6d28d9",
  base_font  = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font  = font_google("IBM Plex Mono"),
  font_scale = 0.85,
  "navbar-bg" = "#0f0f1a"
)

plotly_dark <- function(p, ...) {
  dots <- list(...)

  xaxis <- modifyList(
    list(gridcolor = "#1a1a2a", zerolinecolor = "#333"),
    dots$xaxis %||% list()
  )
  yaxis <- modifyList(
    list(gridcolor = "#1a1a2a", zerolinecolor = "#333"),
    dots$yaxis %||% list()
  )
  dots$xaxis <- NULL
  dots$yaxis <- NULL

  do.call(
    layout,
    c(
      list(
        p,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "#0f0f1a",
        font          = list(family = "IBM Plex Mono", color = "#999"),
        xaxis = xaxis,
        yaxis = yaxis
      ),
      dots
    )
  )
}


# ============================================================================
# UI
# ============================================================================

ui <- page_navbar(
  theme  = app_theme,
  title  = "NBA 3-2-1 Lottery Reform",
  header = div(
    style = "text-align:center; padding:8px 0 4px; border-bottom:1px solid #1a1a2a;",
    tags$small(
      style = "color:#777;",
      "EPV is the posterior mean value of each pick slot; 4-Yr WS shows simulated player-outcome uncertainty over a rookie-scale four-year horizon."
    )
  ),

  tags$head(tags$style(HTML("
    /* ---- Font policy ----
       Inter (the base font) carries all prose, labels, nav, and headers so the
       app reads like a product. IBM Plex Mono is reserved for dense numeric /
       tabular content where column alignment and a 'data' feel actually help. */
    .mono, code, kbd, pre,
    table.dataTable, table.dataTable td, table.dataTable th,
    .tm-page .tm-pick-table, .tm-page .tm-pick-table td, .tm-page .tm-pick-table th,
    .team-detail-card table, .team-detail-card table td, .team-detail-card table th {
      font-family: 'IBM Plex Mono', ui-monospace, SFMono-Regular, Menlo, monospace !important;
      font-variant-numeric: tabular-nums;
    }
    /* Card headers and ad-hoc section titles: larger and heavier so they
       anchor each panel instead of blending into the body text. */
    .card-header, .bslib-card .card-header {
      font-size: 1.15rem !important;
      font-weight: 800 !important;
      letter-spacing: -0.01em;
      color: #e8e8ef;
    }
    .section-title {
      font-size: 1.15rem;
      font-weight: 800;
      letter-spacing: -0.01em;
      color: #e8e8ef;
    }
    /* The full-table title is a textOutput; keep it prominent too. */
    #full_table_title, #impact_title {
      font-size: 1.15rem;
      font-weight: 800;
      letter-spacing: -0.01em;
      color: #e8e8ef;
    }
    .navbar-brand { font-weight: 800 !important; letter-spacing: -0.01em; }
    .navbar .nav-link { font-weight: 600; }
    .navbar, .bslib-page-navbar > .navbar {
      position: sticky;
      top: 0;
      z-index: 1050;
      box-shadow: 0 1px 0 #1a1a2a;
    }
    .sidebar .shiny-options-group label {
      white-space: nowrap;
    }
    .team-logo-cell {
      display: inline-flex;
      align-items: center;
      gap: 2px;
      white-space: nowrap;
      font-weight: 700;
    }
    table.dataTable tbody td {
      color: #d7d8e2 !important;
    }
    table.dataTable tbody td.dt-right,
    table.dataTable tbody td.dt-center {
      color: #d7d8e2 !important;
    }
    .ft-scroll-wrap table.dataTable thead,
    .ft-scroll-wrap table.dataTable thead tr,
    .ft-scroll-wrap table.dataTable thead th {
      position: sticky !important;
      top: 0 !important;
      z-index: 20 !important;
      background: #0a0a14 !important;
    }
    .pm-simple-wrap {
      max-height: 650px;
      overflow-y: auto;
      overflow-x: auto;
      padding: 8px 10px 12px;
    }
    .pm-simple-table {
      width: 100%;
      border-collapse: collapse;
      font-family: 'IBM Plex Mono', ui-monospace, SFMono-Regular, Menlo, monospace !important;
      font-variant-numeric: tabular-nums;
      font-size: 12px;
    }
    .pm-simple-table th {
      position: sticky;
      top: 0;
      z-index: 2;
      background: #0a0a14;
      color: #cfd2dc;
      text-align: left;
      padding: 8px 8px;
      border-bottom: 1px solid #1a1a2a;
      white-space: nowrap;
    }
    .pm-simple-table td {
      color: #d7d8e2;
      padding: 7px 8px;
      border-bottom: 1px solid rgba(255,255,255,0.035);
      white-space: nowrap;
    }
    .pm-simple-table td.num, .pm-simple-table th.num {
      text-align: right;
    }
    .pm-simple-table tr:nth-child(even) td {
      background: rgba(109,40,217,0.06);
    }
    /* Full Table: a SINGLE scroll container that we control. DT renders flat
       (no internal scroll body), so this wrapper is the only thing that ever
       scrolls on the tab. Its height is set by the fullTableFit controller so
       the detail panel below always stays fully visible. */
    .ft-scroll-wrap {
      overflow-y: auto;
      overflow-x: auto;
      max-height: var(--full-table-scroll, calc(100vh - 315px));
    }
    /* Keep the header row pinned while the body scrolls inside the wrapper. */
    .ft-scroll-wrap table.dataTable thead th {
      position: sticky;
      top: 0;
      z-index: 2;
      background: #0a0a14;
    }
    #team_detail,
    #team_detail .card,
    #team_detail .card-body,
    #team_detail .bslib-card-body {
      overflow: visible !important;
      max-height: none !important;
    }
    .team-detail-card,
    .team-detail-card .card-body,
    .team-detail-card .bslib-card-body,
    .team-detail-card [data-card-body] {
      height: auto !important;
      max-height: none !important;
      overflow: visible !important;
    }
    .tm-page .tm-pick-table {
      overflow-x: visible !important;
    }
    .tm-page .tm-pick-table table {
      width: 100%;
      table-layout: fixed;
      border-collapse: collapse;
      font-size: 14px;
    }
    .tm-page .tm-pick-table th {
      color: #d0d0d0;
      font-weight: 700;
      padding: 5px 6px;
      border-bottom: 1px solid #1a1a2a;
    }
    .tm-page .tm-pick-table td {
      padding: 5px 6px;
      vertical-align: middle;
      border: none;
    }
    .tm-page .tm-pick-table tbody tr:not(.tm-total-row) {
      height: 74px;
    }
    .tm-page .tm-pick-table tbody tr:not(.tm-total-row) td:first-child {
      white-space: normal;
      overflow-wrap: anywhere;
      word-break: normal;
      line-height: 1.25;
    }
    .tm-page .tm-pick-label {
      display: block;
      white-space: normal;
      overflow-wrap: anywhere;
      line-height: 1.25;
      max-height: 3.75em;
      overflow: hidden;
    }
    .tm-page .tm-pick-table .tm-spacer-row td {
      height: 74px !important;
      padding: 5px 6px !important;
    }
    .tm-page .tm-pick-table .form-group {
      margin-bottom: 0;
    }
    .tm-page .tm-pick-table .checkbox {
      margin: 0;
    }
    .tm-page .tm-total-row td {
      border-top: 1px solid #333 !important;
      font-weight: 800;
      color: #e5e7eb;
    }
    .tm-page .tm-selection-card {
      height: 100%;
    }
    .tm-page .card-header, .tm-page .bslib-card .card-header {
      font-size: 18px;
      font-weight: 850;
    }
    .tm-page label {
      font-size: 15px;
      font-weight: 700;
    }
    .tm-page .selectize-input {
      font-size: 14px;
    }
  "))),

  # Dynamically size the Full Table scroll-body so that when the team detail
  # panel is open, the table shrinks just enough to keep the detail panel fully
  # visible. The table remains the only scrollable element on the tab.
  tags$head(tags$script(HTML("
    (function() {
      function px(n) { return Math.max(140, Math.round(n)) + 'px'; }

      function fit() {
        var wrap = document.getElementById('ft-scroll-wrap');
        if (!wrap) return;

        // Only the detail panel competes for vertical space below the table.
        var detail = document.getElementById('team_detail');
        var detailH = (detail && detail.children.length > 0)
          ? detail.getBoundingClientRect().height : 0;

        // Top of the scroll wrapper relative to the viewport already accounts
        // for the navbar, the global header note, and the card header.
        var top = wrap.getBoundingClientRect().top;

        var gapAboveDetail = detailH > 0 ? 14 : 0;
        var bottomMargin = 16;
        var avail = window.innerHeight - top - detailH - gapAboveDetail - bottomMargin;

        document.documentElement.style.setProperty('--full-table-scroll', px(avail));
      }

      // Expose so the DataTables row-click callback can trigger an immediate fit.
      window.fullTableFit = fit;

      var scheduled = false;
      function scheduleFit() {
        if (scheduled) return;
        scheduled = true;
        requestAnimationFrame(function() {
          scheduled = false;
          fit();
        });
      }

      window.addEventListener('load', function() { setTimeout(fit, 120); });
      window.addEventListener('resize', scheduleFit);

      // React to the detail panel being injected / removed / re-rendered.
      var bodyObserver = new MutationObserver(scheduleFit);
      bodyObserver.observe(document.body, { childList: true, subtree: true });

      // React to the detail panel's own content height changing.
      if (window.ResizeObserver) {
        var ro = new ResizeObserver(scheduleFit);
        var roTarget = null;
        var attach = function() {
          var detail = document.getElementById('team_detail');
          if (detail && detail !== roTarget) {
            if (roTarget) ro.unobserve(roTarget);
            ro.observe(detail);
            roTarget = detail;
          }
        };
        var attachObserver = new MutationObserver(attach);
        attachObserver.observe(document.body, { childList: true, subtree: true });
        attach();
      }

      setTimeout(scheduleFit, 300);
      setTimeout(scheduleFit, 800);
    })();
  "))),

  # ---- Tab 1 ----
  nav_panel(
    title = "Pick Landscape",
    icon  = icon("chart-area"),
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        radioButtons("impact_value_mode", "Value basis",
          choices = value_mode_choices, selected = "outcome", inline = TRUE),
        radioButtons("impact_x_mode", "X-axis",
          choices = c("Average value per pick" = "average",
                      "Δ value per pick" = "delta"),
          selected = "average", inline = FALSE),
        selectInput("impact_year", "Draft year",
          choices = c("All", sort(unique(pick_display_assets$year))), selected = "All"),
        selectInput("impact_round", "Round",
          choices = c("All" = "All", "Round 1" = "1", "Round 2" = "2"), selected = "All")
      ),
      card(
        card_header(textOutput("impact_title")),
        plotlyOutput("impact_chart", height = "680px")
      )
    )
  ),

  # ---- Tab 1b: Impact (dumbbell / tornado) ----
  nav_panel(
    title = "Impact",
    icon  = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        radioButtons("wl_value_mode", "Value basis",
          choices = value_mode_choices, selected = "outcome", inline = TRUE),
        radioButtons("wl_sort", "Sort teams by",
          choices = c("Δ value (biggest movers)" = "delta",
                      "Total 3-2-1 value" = "total"),
          selected = "delta", inline = FALSE),
        tags$p(class = "mono", style = "font-size:11px; color:#888; line-height:1.6;",
          "Each team's portfolio under the current system (hollow dot) vs 3-2-1 (filled dot). ",
          "The faint bar is the 10th-90th percentile range of the 3-2-1 portfolio value. ",
          "Color reflects confidence in the direction of the change: stronger green/red means the simulation is more confident, while faint green/red means the result is closer to a coin flip. ",
          "Most portfolio changes are small relative to lottery variance, so expect many muted/gray rows \u2014 that itself is a finding.")
      ),
      card(
        card_header(textOutput("wl_title")),
        plotlyOutput("wl_dumbbell", height = "860px")
      )
    )
  ),

  # ---- Tab 2 ----
  nav_panel(
    title = "Lottery Odds",
    icon  = icon("dice"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Expected Pick Position by Lottery Seed"),
        plotlyOutput("lottery_line", height = "640px")
      ),
      card(
        card_header("Probability of #1 Pick by Seed (%)"),
        plotlyOutput("lottery_bar", height = "640px")
      )
    )
  ),

  # ---- Tab 3 ----
  nav_panel(
    title = "Full Table",
    icon  = icon("table"),
    div(class = "ft-page",
      tags$style(HTML("
        /* The Full Table tab must have exactly ONE scrollbar: the table
           wrapper. Everything around it flows naturally and never gets its
           own scroll, so the detail panel can sit fully visible below. */
        .ft-page, .ft-page > .bslib-sidebar-layout > .main {
          height: auto !important;
          max-height: none !important;
          overflow: visible !important;
        }
        .ft-page .card,
        .ft-page .bslib-card,
        .ft-page .card-body,
        .ft-page .bslib-card .card-body,
        .ft-page .bslib-card-body,
        .ft-page [data-card-body] {
          height: auto !important;
          max-height: none !important;
          overflow: visible !important;
        }
        /* Re-arm scrolling on just the table wrapper (the rule above is broad). */
        .ft-page .ft-scroll-wrap { overflow: auto !important; }
      ")),
      layout_sidebar(
        sidebar = sidebar(
          width = 260,
          radioButtons("table_value_mode", "Value basis",
            choices = value_mode_choices, selected = "outcome", inline = TRUE),
          tags$p(style = "font-size:11px; color:#888; line-height:1.6;",
            "Toggle whether team portfolio columns reflect sampled player outcomes or posterior expected pick values.")
        ),
        card(
          card_header(textOutput("full_table_title")),
          div(id = "ft-scroll-wrap", class = "ft-scroll-wrap",
            DTOutput("full_table")
          )
        ),
        uiOutput("team_detail")
      )
    )
  ),

  # ---- Tab 4 ----
  nav_panel(
    title = "Markov + Curve",
    icon  = icon("project-diagram"),
    div(class = "markov-curve-page",
      tags$style(HTML("
        .markov-curve-page {
          padding: 6px 4px 12px;
          height: auto !important;
          max-height: none !important;
          overflow: visible !important;
        }
        .markov-curve-page .card,
        .markov-curve-page .bslib-card,
        .markov-curve-page .card-body,
        .markov-curve-page .bslib-card .card-body,
        .markov-curve-page .bslib-card-body,
        .markov-curve-page [data-card-body] {
          height: auto !important;
          max-height: none !important;
          overflow: visible !important;
        }
        .markov-curve-page .curve-stack {
          display: flex;
          flex-direction: column;
          gap: 12px;
        }
      ")),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Tier Transition Probabilities"),
          plotlyOutput("trans_heatmap", height = "360px")
        ),
        card(
          card_header("Tier Transition State Diagram"),
          plotOutput("trans_diagram", height = "360px")
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Combined Draft Pick Value Curve (Picks 1-60)"),
          plotlyOutput("pick_curve_plot_combined", height = "440px")
        )
      )
    )
  ),

  # ---- Tab 5: Single Pick Valuation ----
  nav_panel(
    title = "Single Pick",
    icon  = icon("basketball"),
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        selectInput("sp_year", "Draft year",
          choices  = sort(unique(pick_display_assets$year)),
          selected = 2026),
        selectInput("sp_team", "Team", choices = NULL),
        selectInput("sp_asset", "Pick", choices = NULL),
        hr(),
        uiOutput("sp_obligation")
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Projected Pick Value: Current vs 3-2-1"),
          uiOutput("sp_headline")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Expected Pick Value"),
          plotlyOutput("sp_dist_ev", height = "330px")
        ),
        card(
          card_header("Realized Outcome Simulation"),
          plotlyOutput("sp_dist_outcome", height = "330px")
        )
      )
    )
  ),


  # ---- Tab 6: Pick Movers ----
  nav_panel(
    title = "Pick Movers",
    icon  = icon("chart-line"),
    div(class = "pm-page",
      tags$style(HTML("
        .pm-page .dataTables_scrollBody { min-height: 200px; }
        .pm-page table.dataTable { font-size: 12px; }
        .pm-page .card-body, .pm-page .bslib-card-body, .pm-page [data-card-body] {
          overflow: visible !important;
        }
      ")),
      layout_sidebar(
        sidebar = sidebar(
          width = 250,
          selectInput("pm_year", "Draft year",
            choices = c("All", sort(unique(pick_display_assets$year))), selected = "All"),
          selectInput("pm_round", "Round",
            choices = c("All" = "All", "Round 1" = "1", "Round 2" = "2"), selected = "All")
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("All Picks by Expected Pick Value"),
            uiOutput("pm_ev_table")
          ),
          card(
            card_header("All Picks by 4-Yr Win Share Outcomes"),
            uiOutput("pm_outcome_table")
          )
        )
      )
    )
  ),

  # ---- Tab 7: Trade Machine ----
  nav_panel(
    title = "Trade Machine",
    icon  = icon("right-left"),
    div(class = "tm-page",
      tags$style(HTML("
        .tm-page {
          padding: 6px 4px 10px;
        }
        .tm-page .card,
        .tm-page .bslib-card,
        .tm-page .card-body,
        .tm-page .bslib-card .card-body,
        .tm-page .bslib-card-body,
        .tm-page [data-card-body] {
          height: auto !important;
          max-height: none !important;
          overflow: visible !important;
        }
        .tm-page .selectize-dropdown {
          z-index: 10000 !important;
        }
      ")),
      layout_columns(
        col_widths = c(6, 6),
        card(class = "tm-selection-card",
          card_header(uiOutput("tm_teamA_hdr")),
          selectInput("tm_teamA", "Team 1",
            choices = all_team_abbr, selected = all_team_abbr[1]),
          uiOutput("tm_picksA_picker")
        ),
        card(class = "tm-selection-card",
          card_header(uiOutput("tm_teamB_hdr")),
          selectInput("tm_teamB", "Team 2",
            choices = all_team_abbr, selected = all_team_abbr[2]),
          uiOutput("tm_picksB_picker")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        gap = "2.5rem",
        class = "tm-outgoing-row",
        card(class = "tm-selection-card",
          card_header(uiOutput("tm_picksA_out_hdr")),
          uiOutput("tm_picksA_table")
        ),
        card(class = "tm-selection-card",
          card_header(uiOutput("tm_picksB_out_hdr")),
          uiOutput("tm_picksB_table")
        )
      ),
      card(
        card_header("Trade Assessment"),
        uiOutput("tm_verdict")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          card_header("Expected Pick Value Edge"),
          plotlyOutput("tm_dist_ev", height = "300px")
        ),
        card(
          card_header("Realized Outcome Simulation"),
          plotlyOutput("tm_dist_outcome", height = "300px")
        ),
        card(
          card_header("Best Player Outcome Edge"),
          plotlyOutput("tm_dist_best", height = "300px")
        )
      )
    )
  )#,

  # nav_spacer(),
  # nav_item(tags$small(
  #   style = "color:#444; font-size:9px;",
  #   sprintf("Pick curve %s | Markov: %d transitions / %d seasons | mixing %.1f yr",
  #           stan_diag$pick_model$curve_type,
  #           stan_diag$markov_model$n_transitions,
  #           stan_diag$markov_model$n_seasons,
  #           stan_diag$markov_model$mixing_time)))
)



rescale_numeric_app <- function(x, to = c(0.75, 1.55)) {
  x <- as.numeric(x)
  out <- rep(mean(to), length(x))
  ok <- is.finite(x)
  if (sum(ok) <= 1L || diff(range(x[ok], na.rm = TRUE)) == 0) return(out)
  rng <- range(x[ok], na.rm = TRUE)
  out[ok] <- to[1] + (x[ok] - rng[1]) / (rng[2] - rng[1]) * diff(to)
  out
}

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  impact_selected_team <- reactiveVal(NULL)

  observeEvent(plotly::event_data("plotly_click", source = "impact_plot"), {
    ed <- plotly::event_data("plotly_click", source = "impact_plot")
    tm <- NULL
    if (!is.null(ed) && "customdata" %in% names(ed) && length(ed$customdata) > 0) {
      tm <- ed$customdata[[1]]
    }
    if (!is.null(tm) && length(tm) > 0 && nzchar(as.character(tm))) {
      tm <- as.character(tm)
      if (identical(impact_selected_team(), tm)) impact_selected_team(NULL) else impact_selected_team(tm)
    }
  }, ignoreInit = TRUE)

  observe({
    input$impact_value_mode
    input$impact_x_mode
    input$impact_year
    input$impact_round
    impact_selected_team(NULL)
  })

  caterpillar_delta_plot <- function(df, x_title, hover_suffix = "WS") {
    df <- df %>%
      mutate(
        sign_group = if_else(.data$delta_mean >= 0, "Positive", "Negative"),
        team_fct = factor(.data$team, levels = rev(.data$team)),
        hover_text = sprintf(
          "<b>%s</b><br>Impact: %+.1f %s<br>90%% CI: [%+.1f, %+.1f] %s",
          .data$team, .data$delta_mean, hover_suffix, .data$delta_q05, .data$delta_q95, hover_suffix
        )
      )

    p <- plot_ly()
    for (sg in c("Positive", "Negative")) {
      sub <- df %>% filter(.data$sign_group == .env$sg)
      if (nrow(sub) == 0) next
      col <- if (sg == "Positive") "#10b981" else "#ef4444"
      p <- p %>%
        add_segments(
          data = sub,
          x = ~delta_q05, xend = ~delta_q95,
          y = ~team_fct, yend = ~team_fct,
          line = list(color = col, width = 3),
          hoverinfo = "skip",
          showlegend = FALSE
        ) %>%
        add_markers(
          data = sub,
          x = ~delta_mean, y = ~team_fct,
          marker = list(color = col, size = 8, line = list(color = "#0f0f1a", width = 1)),
          text = ~hover_text,
          hovertemplate = "%{text}<extra></extra>",
          name = sg,
          showlegend = FALSE
        )
    }

    p %>%
      plotly_dark(
        xaxis = list(title = list(text = x_title), zerolinecolor = "#444", tickformat = ".1f"),
        yaxis = list(title = list(text = "Team"), tickfont = list(size = 10)),
        margin = list(l = 62, r = 24, t = 20, b = 56)
      )
  }

  # ---- Impact ----
  output$impact_title <- renderText({
    yr <- input$impact_year %||% "All"
    rnd <- input$impact_round %||% "All"
    yr_txt <- if (identical(yr, "All")) "all years" else yr
    rnd_txt <- if (identical(rnd, "All")) "all rounds" else paste("round", rnd)
    sprintf("Pick Quality vs Pick Quantity — %s, %s, %s", value_mode_label(input$impact_value_mode), yr_txt, rnd_txt)
  })

  output$impact_chart <- renderPlotly({
    x_mode <- input$impact_x_mode %||% "average"
    unit <- value_mode_unit(input$impact_value_mode)

    df_all <- portfolio_quality_quantity_summary_app(
      input$impact_value_mode,
      year_filter = input$impact_year %||% "All",
      round_filter = input$impact_round %||% "All"
    ) %>%
      mutate(
        sign_group = if_else(.data$delta_total_value >= 0, "Positive", "Negative"),
        cur_x = if (identical(x_mode, "delta")) 0 else .data$cur_avg_quality,
        new_x = if (identical(x_mode, "delta")) .data$delta_avg_quality else .data$new_avg_quality,
        cur_y = .data$cur_expected_picks,
        new_y = .data$new_expected_picks,
        size_metric = if (identical(x_mode, "delta")) {
          pmax(.data$cur_avg_quality, .data$new_avg_quality, 0.05)
        } else {
          pmax(.data$cur_total_value, .data$new_total_value, 0.05)
        },
        bubble_size = pmax(.data$size_metric, 0.05),
        hover_text = sprintf(
          paste0(
            "<b>%s</b><br>",
            "Δ total: %+0.2f %s<br>",
            "Δ / pick: %+0.2f<br>",
            "Δ picks: %+0.2f<br>",
            "P(+): %.1f%%<br>",
            "Current: %.1f | 3-2-1: %.1f<br>",
            "Picks: %.1f → %.1f"
          ),
          .data$team,
          .data$delta_total_value, unit,
          .data$delta_avg_quality,
          .data$delta_expected_picks,
          100 * .data$p_positive,
          .data$cur_total_value, .data$new_total_value,
          .data$cur_expected_picks, .data$new_expected_picks
        )
      )

    selected_team <- impact_selected_team()
    df <- if (!is.null(selected_team) && selected_team %in% df_all$team) {
      df_all %>% filter(.data$team == .env$selected_team)
    } else {
      df_all
    }

    x_vals_all <- c(df_all$cur_x, df_all$new_x)
    y_vals_all <- c(df_all$cur_y, df_all$new_y)
    x_ref <- if (identical(x_mode, "delta")) 0 else median(x_vals_all, na.rm = TRUE)
    y_ref <- median(y_vals_all, na.rm = TRUE)
    x_rng <- range(c(x_vals_all, x_ref), na.rm = TRUE)
    y_rng <- range(c(y_vals_all, y_ref), na.rm = TRUE)
    if (!all(is.finite(x_rng)) || diff(x_rng) == 0) x_rng <- c(-0.5, 0.5) + x_ref
    if (!all(is.finite(y_rng)) || diff(y_rng) == 0) y_rng <- c(-0.5, 0.5) + y_ref

    pad_x <- max(if (identical(x_mode, "delta")) 0.15 else 0.25, diff(x_rng) * 0.12)
    pad_y <- max(0.15, diff(y_rng) * 0.12)

    p <- plot_ly(source = "impact_plot")
    for (sg in c("Positive", "Negative")) {
      sub <- df %>% filter(.data$sign_group == .env$sg)
      if (nrow(sub) == 0) next
      col <- if (sg == "Positive") "#10b981" else "#ef4444"
      p <- p %>%
        add_segments(
          data = sub,
          x = ~cur_x, xend = ~new_x,
          y = ~cur_y, yend = ~new_y,
          customdata = ~team,
          line = list(color = col, width = 3),
          opacity = 0.82,
          text = ~hover_text,
          hovertemplate = "%{text}<extra></extra>",
          showlegend = FALSE
        ) %>%
        add_markers(
          data = sub,
          x = ~new_x, y = ~new_y,
          customdata = ~team,
          size = ~bubble_size,
          sizes = c(18, 54),
          marker = list(color = col, opacity = 0.92, line = list(color = "#0f0f1a", width = 1.5)),
          text = ~hover_text,
          hovertemplate = "%{text}<extra></extra>",
          name = sg,
          showlegend = TRUE
        )
    }

    # Transparent hit targets at the current-logo locations provide hover/click
    # without relying on Plotly layout images to capture events.
    p <- p %>%
      add_markers(
        data = df,
        x = ~cur_x, y = ~cur_y,
        customdata = ~team,
        size = ~bubble_size,
        sizes = c(22, 58),
        marker = list(color = "rgba(255,255,255,0.01)", line = list(color = "rgba(255,255,255,0)", width = 0)),
        text = ~hover_text,
        hovertemplate = "%{text}<extra></extra>",
        showlegend = FALSE,
        opacity = 0.01
      )

    logo_images <- team_logo_layout_images_app(
      df,
      x_col = "cur_x",
      y_col = "cur_y",
      sizex = max(0.12, diff(x_rng) * 0.040),
      sizey = max(0.90, diff(y_rng) * 0.065),
      opacity = 0.96
    )

    shapes <- list(
      list(type = "line", x0 = x_ref, x1 = x_ref, y0 = y_rng[1] - pad_y, y1 = y_rng[2] + pad_y,
           line = list(color = "rgba(255,255,255,0.18)", dash = "dash")),
      list(type = "line", x0 = x_rng[1] - pad_x, x1 = x_rng[2] + pad_x, y0 = y_ref, y1 = y_ref,
           line = list(color = "rgba(255,255,255,0.18)", dash = "dash"))
    )

    annotations <- list(
      list(x = x_rng[2] + pad_x * 0.6, y = y_rng[2] + pad_y * 0.5, text = "High quality / high quantity", showarrow = FALSE, font = list(size = 12, color = "#10b981")),
      list(x = x_rng[2] + pad_x * 0.6, y = y_rng[1] - pad_y * 0.3, text = "High quality / low quantity", showarrow = FALSE, font = list(size = 12, color = "#aaa")),
      list(x = x_rng[1] - pad_x * 0.6, y = y_rng[2] + pad_y * 0.5, text = "Low quality / high quantity", showarrow = FALSE, font = list(size = 12, color = "#aaa")),
      list(x = x_rng[1] - pad_x * 0.6, y = y_rng[1] - pad_y * 0.3, text = "Low quality / low quantity", showarrow = FALSE, font = list(size = 12, color = "#ef4444"))
    )

    x_title <- if (identical(x_mode, "delta")) paste0("Δ ", unit, " per pick") else paste0("Average ", unit, " per pick")

    p %>%
      plotly_dark(
        xaxis = list(title = list(text = x_title, font = list(size = 15)),
                     tickformat = ".2f", tickfont = list(size = 12), range = c(x_rng[1] - pad_x, x_rng[2] + pad_x)),
        yaxis = list(title = list(text = "Expected pick count", font = list(size = 15)), tickformat = ".2f",
                     tickfont = list(size = 12), range = c(y_rng[1] - pad_y, y_rng[2] + pad_y)),
        shapes = shapes,
        annotations = annotations,
        legend = list(orientation = "h", x = 0.02, y = 1.10, xanchor = "left", yanchor = "bottom", font = list(size = 12)),
        images = logo_images,
        margin = list(l = 74, r = 34, t = 82, b = 64)
      ) %>%
      event_register("plotly_click")
  })

  # ---- Winners & Losers: dumbbell / tornado ----
  output$wl_title <- renderText({
    sprintf("Who Wins and Loses Under 3-2-1 — %s", value_mode_label(input$wl_value_mode %||% "outcome"))
  })

  output$wl_dumbbell <- renderPlotly({
    mode <- input$wl_value_mode %||% "outcome"
    unit <- value_mode_unit(mode)
    sort_by <- input$wl_sort %||% "delta"

    base <- summary_for_value_mode(mode) %>%
      select(team, tier, current_mean, new_mean, delta_value)

    draws <- team_draw_summary_for_table_app(mode) %>%
      select(team, delta_q10, delta_q90, p_positive)

    df <- base %>%
      left_join(draws, by = "team") %>%
      mutate(
        # Plausible band for the 3-2-1 endpoint, anchored at the current value.
        lo = .data$current_mean + coalesce(.data$delta_q10, 0),
        hi = .data$current_mean + coalesce(.data$delta_q90, 0),
        p_pos = coalesce(.data$p_positive, 0.5),
        dir = if_else(.data$delta_value >= 0, "gain", "loss"),
        # Confidence that the change has the sign of the mean: how far P(+) is
        # from a coin flip. Instead of hard-gray thresholds, use a gentle
        # red/green alpha gradient so the direction is visible without
        # overstating noisy effects.
        conf = pmax(.data$p_pos, 1 - .data$p_pos),
        confidence_strength = pmin(1, pmax(0, (.data$conf - 0.50) / 0.50)),
        signal_level = dplyr::case_when(
          .data$conf >= 0.85 ~ "strong",
          .data$conf >= 0.65 ~ "lean",
          TRUE ~ "low"
        ),
        alpha = 0.26 + 0.64 * .data$confidence_strength,
        col = if_else(
          .data$dir == "gain",
          sprintf("rgba(16,185,129,%.2f)", .data$alpha),
          sprintf("rgba(239,68,68,%.2f)", .data$alpha)
        ),
        order_key = if (identical(sort_by, "total")) .data$new_mean else .data$delta_value
      ) %>%
      arrange(.data$order_key) %>%
      mutate(team = factor(.data$team, levels = .data$team))

    n_teams <- nrow(df)
    y_idx <- seq_len(n_teams)
    df$y <- y_idx

    hover <- sprintf(
      paste0(
        "<b>%s</b> (%s)<br>",
        "Current: %.1f %s<br>",
        "3-2-1: %.1f %s<br>",
        "Change: %+.1f %s<br>",
        "10th-90th of change: [%+.1f, %+.1f]<br>",
        "P(change is %s): %.0f%%  (%s)"
      ),
      as.character(df$team), tier_short[df$tier],
      df$current_mean, unit,
      df$new_mean, unit,
      df$delta_value, unit,
      coalesce(df$delta_q10, 0), coalesce(df$delta_q90, 0),
      df$dir, 100 * df$conf,
      dplyr::recode(df$signal_level, strong = "strong signal", lean = "leans this way", low = "low confidence")
    )

    p <- plot_ly()

    # Uncertainty whisker for the 3-2-1 endpoint (faint horizontal band).
    for (i in y_idx) {
      p <- p %>%
        add_segments(
          x = df$lo[i], xend = df$hi[i],
          y = df$y[i], yend = df$y[i],
          line = list(color = "rgba(255,255,255,0.16)", width = 6),
          hoverinfo = "skip", showlegend = FALSE
        )
    }

    # Connector from current to 3-2-1.
    for (i in y_idx) {
      p <- p %>%
        add_segments(
          x = df$current_mean[i], xend = df$new_mean[i],
          y = df$y[i], yend = df$y[i],
          line = list(color = df$col[i], width = 2.4),
          opacity = 0.9,
          hoverinfo = "skip", showlegend = FALSE
        )
    }

    # Current marker (hollow) and 3-2-1 marker (filled).
    p <- p %>%
      add_markers(
        x = df$current_mean, y = df$y,
        marker = list(color = "#0a0a14", size = 10,
                      line = list(color = "#9aa0aa", width = 2)),
        text = hover, hovertemplate = "%{text}<extra></extra>",
        name = "Current", showlegend = TRUE
      ) %>%
      add_markers(
        x = df$new_mean, y = df$y,
        marker = list(color = df$col, size = 12,
                      line = list(color = "#0a0a14", width = 1.5)),
        text = hover, hovertemplate = "%{text}<extra></extra>",
        name = "3-2-1", showlegend = TRUE
      )

    # Legend proxies for the color semantics; faint markers/lines indicate low confidence.
    p <- p %>%
      add_markers(x = NA, y = NA,
                  marker = list(color = "#10b981", size = 11), name = "Gain") %>%
      add_markers(x = NA, y = NA,
                  marker = list(color = "#ef4444", size = 11), name = "Loss") %>%
      add_markers(x = NA, y = NA,
                  marker = list(color = "rgba(255,255,255,0.32)", size = 11), name = "Low confidence = faded")

    p %>%
      plotly_dark(
        xaxis = list(title = list(text = sprintf("Portfolio value (%s)", unit), font = list(size = 15)),
                     tickfont = list(size = 12), zeroline = FALSE),
        yaxis = list(
          title = "",
          tickmode = "array",
          tickvals = y_idx,
          ticktext = as.character(df$team),
          tickfont = list(size = 11, family = "IBM Plex Mono"),
          range = c(0.3, n_teams + 0.7)
        ),
        legend = list(orientation = "h", x = 0.5, y = 1.03, xanchor = "center",
                      yanchor = "bottom", font = list(size = 11)),
        margin = list(l = 70, r = 34, t = 60, b = 60),
        hoverlabel = list(font = list(family = "IBM Plex Mono", size = 12))
      )
  })

  # ---- Lottery line ----
  output$lottery_line <- renderPlotly({
    cur <- lottery_dist %>% filter(system == "Current", seed <= 16)
    new <- lottery_dist %>% filter(system == "Proposed 3-2-1", seed <= 16)

    plot_ly() %>%
      add_trace(data = cur, x = ~seed, y = ~expected_pick, type = "scatter",
                mode = "lines+markers", name = "Current",
                line = list(color = "#3b82f6", width = 3),
                marker = list(color = "#3b82f6", size = 7),
                error_y = list(type = "data", array = ~(1.96 * expected_pick_se),
                               visible = TRUE, color = "#3b82f6", thickness = 0.7),
                hovertemplate = "Seed %{x}<br>Expected Pick: %{y:.1f}<extra></extra>") %>%
      add_trace(data = new, x = ~seed, y = ~expected_pick, type = "scatter",
                mode = "lines+markers", name = "3-2-1 sim",
                line = list(color = "#f59e0b", width = 3),
                marker = list(color = "#f59e0b", size = 7),
                error_y = list(type = "data", array = ~(1.96 * expected_pick_se),
                               visible = TRUE, color = "#f59e0b", thickness = 0.7),
                hovertemplate = "Seed %{x}<br>Expected Pick: %{y:.1f}<extra></extra>") %>%
      plotly_dark(
        xaxis = list(title = list(text = "Lottery Seed (1 = worst record)"), dtick = 1),
        yaxis = list(title = list(text = "Expected Pick"), range = c(16.5, 0.5), tickmode = "array", tickvals = 1:16),
        margin = list(l = 64, r = 24, t = 78, b = 56),
        legend = list(
          orientation = "h",
          x = 0.02,
          y = 1.16,
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = 10),
          bgcolor = "rgba(15,15,26,0.86)",
          bordercolor = "rgba(255,255,255,0.08)",
          borderwidth = 1
        )
      )
  })

  # ---- Lottery bar ----
  output$lottery_bar <- renderPlotly({
    cur <- lottery_dist %>% filter(system == "Current", seed <= 16)
    new <- lottery_dist %>% filter(system == "Proposed 3-2-1", seed <= 16)

    plot_ly() %>%
      add_bars(data = cur, x = ~seed, y = ~(prob_no1 * 100),
               name = "Current", marker = list(color = "#3b82f6"),
               error_y = list(type = "data", array = ~(1.96 * prob_no1_se * 100),
                              visible = TRUE, color = "#3b82f6", thickness = 0.7),
               hovertemplate = "Seed %{x}<br>#1 Pick Odds: %{y:.1f}%<extra></extra>") %>%
      add_bars(data = new, x = ~seed, y = ~(prob_no1 * 100),
               name = "3-2-1 sim", marker = list(color = "#f59e0b"),
               error_y = list(type = "data", array = ~(1.96 * prob_no1_se * 100),
                              visible = TRUE, color = "#f59e0b", thickness = 0.7),
               hovertemplate = "Seed %{x}<br>#1 Pick Odds: %{y:.1f}%<extra></extra>") %>%
      plotly_dark(
        barmode = "group",
        xaxis = list(title = list(text = "Lottery Seed"), dtick = 1),
        yaxis = list(title = list(text = "#1 Pick Odds")),
        margin = list(l = 64, r = 24, t = 78, b = 56),
        legend = list(
          orientation = "h",
          x = 0.02,
          y = 1.16,
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = 10),
          bgcolor = "rgba(15,15,26,0.86)",
          bordercolor = "rgba(255,255,255,0.08)",
          borderwidth = 1
        )
      )
  })

  output$lottery_validation_table <- renderDT({
    if (is.null(lottery_tier_validation) || nrow(lottery_tier_validation) == 0) {
      return(datatable(tibble(Message = "Re-run nba_lottery.R to generate the published-odds validation table."),
                       rownames = FALSE, options = list(dom = "t")))
    }

    tbl <- lottery_tier_validation %>%
      transmute(
        Tier = lottery_tier_label,
        Seeds = ifelse(seed_min == seed_max, as.character(seed_min), sprintf("%d-%d", seed_min, seed_max)),
        `Sim #1` = sprintf("%.1f%% ± %.1f", 100 * sim_prob_no1, 100 * 1.96 * sim_prob_no1_mc_se),
        `Pub #1` = sprintf("%.1f%%", 100 * official_prob_no1),
        `Sim Top 3` = sprintf("%.1f%% ± %.1f", 100 * sim_prob_top3, 100 * 1.96 * sim_prob_top3_mc_se),
        `Pub Top 3` = sprintf("%.0f%%", 100 * official_prob_top3),
        `Sim Top 5` = sprintf("%.1f%% ± %.1f", 100 * sim_prob_top5, 100 * 1.96 * sim_prob_top5_mc_se),
        `Pub Top 5` = sprintf("%.0f%%", 100 * official_prob_top5),
        `Sim Top 10` = sprintf("%.1f%% ± %.1f", 100 * sim_prob_top10, 100 * 1.96 * sim_prob_top10_mc_se),
        `Pub Top 10` = sprintf("%.0f%%", 100 * official_prob_top10),
        `Sim Avg` = sprintf("%.2f ± %.2f", sim_expected_pick, 1.96 * sim_expected_pick_mc_se),
        `Pub Avg` = sprintf("%.1f", official_expected_pick)
      )

    datatable(
      tbl, rownames = FALSE,
      options = list(pageLength = 4, dom = "t", scrollX = TRUE,
                     columnDefs = list(list(className = "dt-right", targets = 2:11)))
    )
  })

  # ---- Full table ----
  full_table_source <- reactive({
    summary_for_value_mode(input$table_value_mode)
  })

  output$full_table_title <- renderText({
    sprintf("All 30 Teams — %s; click a row for detail", value_mode_label(input$table_value_mode))
  })

  output$full_table <- renderDT({
    mode <- input$table_value_mode
    unit <- value_mode_unit(mode)
    current_col <- if (identical(mode, "ev")) "Current EPV" else "Current 4-Yr WS"
    new_col <- if (identical(mode, "ev")) "3-2-1 EPV" else "3-2-1 4-Yr WS"
    delta_col <- if (identical(mode, "ev")) "Δ EPV" else "Δ 4-Yr WS"

    draw_tbl <- team_draw_summary_for_table_app(mode)

    base <- full_table_source() %>%
      left_join(draw_tbl, by = "team") %>%
      arrange(.data$wins) %>%
      mutate(
        d_q10 = coalesce(.data$delta_q10, 0),
        d_q90 = coalesce(.data$delta_q90, 0),
        p_pos = coalesce(.data$p_positive, 0.5),
        # Confidence in the SIGN of the change: how far P(+) is from a coin flip.
        # This discriminates real movers far better at the portfolio level than
        # asking the 80% band to clear zero (which it almost never does when a
        # team holds many picks).
        conf = pmax(.data$p_pos, 1 - .data$p_pos),
        signal = .data$conf,
        # A row is "signal" (kept vivid) when the simulation at least leans one
        # way with >= 65% confidence; otherwise it is dimmed as noise.
        is_signal = .data$conf >= 0.65
      )

    # Shared x-domain for the inline interval bars so rows are comparable.
    dom_lo <- min(c(base$d_q10, 0), na.rm = TRUE)
    dom_hi <- max(c(base$d_q90, 0), na.rm = TRUE)
    if (!is.finite(dom_lo) || !is.finite(dom_hi) || dom_lo == dom_hi) {
      dom_lo <- -1; dom_hi <- 1
    }
    pad <- (dom_hi - dom_lo) * 0.06
    dom_lo <- dom_lo - pad; dom_hi <- dom_hi + pad

    interval_bar_svg <- function(q10, mid, q90, is_sig) {
      w <- 132; h <- 22
      sc <- function(v) (v - dom_lo) / (dom_hi - dom_lo) * w
      x0 <- sc(q10); x1 <- sc(q90); xm <- sc(mid); xz <- sc(0)
      band_col <- if (isTRUE(is_sig)) (if (mid >= 0) "#10b981" else "#ef4444") else "#6b7280"
      dot_col  <- band_col
      sprintf(paste0(
        "<svg width='%d' height='%d' viewBox='0 0 %d %d' style='vertical-align:middle;'>",
        "<line x1='%.1f' y1='2' x2='%.1f' y2='%d' stroke='#555' stroke-width='1' stroke-dasharray='2,2'/>",
        "<line x1='%.1f' y1='%d' x2='%.1f' y2='%d' stroke='%s' stroke-width='3' opacity='0.55'/>",
        "<circle cx='%.1f' cy='%d' r='3.4' fill='%s'/>",
        "</svg>"),
        w, h, w, h,
        xz, xz, h - 2,
        x0, h %/% 2, x1, h %/% 2, band_col,
        xm, h %/% 2, dot_col
      )
    }

    bar_html <- vapply(seq_len(nrow(base)), function(i) {
      interval_bar_svg(base$d_q10[i], base$delta_value[i], base$d_q90[i], base$is_signal[i])
    }, character(1))

    signal_html <- vapply(seq_len(nrow(base)), function(i) {
      conf <- base$conf[i]
      up <- base$delta_value[i] >= 0
      arrow <- if (up) "&#9650;" else "&#9660;"   # ▲ / ▼
      col <- if (up) "#10b981" else "#ef4444"
      opacity <- if (base$is_signal[i]) "1" else "0.72"
      sprintf("<span style='color:%s; opacity:%s; white-space:nowrap;'>%s %.0f%%</span>", col, opacity, arrow, 100 * conf)
    }, character(1))

    delta_pct_html <- vapply(seq_len(nrow(base)), function(i) {
      up <- base$delta_pct[i] >= 0
      col <- if (up) "#10b981" else "#ef4444"
      sprintf("<span style='color:%s; white-space:nowrap;'>%+.1f%%</span>", col, base$delta_pct[i])
    }, character(1))

    tbl <- base %>%
      transmute(
        Team = team_logo_html_app(.data$team, size = 22, show_abbr = TRUE),
        Tier = tier_short[.data$tier],
        Record = sprintf("%d-%d", .data$wins, .data$losses),
        `#Pk` = round(.data$n_picks_mean, 1),
        !!current_col := round(.data$current_mean, 1),
        !!new_col := round(.data$new_mean, 1),
        !!delta_col := round(.data$delta_value, 1),
        `Δ%` = delta_pct_html,
        Conf = signal_html,
        `Change (10-90)` = bar_html,
        `3-2-1 10th %ile` = round(coalesce(.data$new_q10, .data$new_q05), 1),
        `3-2-1 90th %ile` = round(coalesce(.data$new_q90, .data$new_q95), 1)
      )

    # Row-level styling target: dim entire rows whose change is within noise so
    # the eye lands on the real movers. Carried as hidden helper columns.
    tbl$`_noise` <- ifelse(base$is_signal, "0", "1")
    tbl$`_signal_num` <- round(base$signal, 3)
    noise_idx <- match("_noise", names(tbl)) - 1L
    signal_num_idx <- match("_signal_num", names(tbl)) - 1L

    datatable(
      tbl, selection = "single", rownames = FALSE, escape = FALSE,
      options = list(
        pageLength = 30, paging = FALSE, dom = "t", ordering = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(noise_idx, signal_num_idx)),
          list(className = "dt-right", targets = c(3, 4, 5, 6, 7, 10, 11)),
          list(className = "dt-center", targets = c(8, 9)),
          list(orderable = FALSE, targets = 9),
          # Sort the Conf column by its underlying numeric value.
          list(orderData = signal_num_idx, targets = 8)
        )
      ),
      callback = htmlwidgets::JS("
        table.on('click', 'tbody tr', function() {
          setTimeout(function() {
            if (window.fullTableFit) { window.fullTableFit(); }
          }, 60);
        });
      ")
    ) %>%
      formatStyle(delta_col, color = styleInterval(0, c("#ef4444", "#10b981"))) %>%
      formatStyle("_noise", target = "row",
                  opacity = styleEqual(c("0", "1"), c("1", "0.82")))
  }, server = FALSE)

  output$team_detail <- renderUI({
    sel <- input$full_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    t <- full_table_source() %>% arrange(.data$wins) %>% slice(sel)
    mode <- input$table_value_mode
    unit <- value_mode_unit(mode)
    basis <- value_mode_label(mode)

    pick_rows <- pick_impact_rows_app(mode) %>%
      filter(.data$owner == .env$t$team) %>%
      mutate(delta = coalesce(.data$delta, .data$new_mean - .data$cur_mean))

    pos_tbl <- pick_rows %>%
      filter(.data$delta > 0) %>%
      arrange(desc(.data$delta)) %>%
      slice_head(n = 3)

    neg_tbl <- pick_rows %>%
      filter(.data$delta < 0) %>%
      arrange(.data$delta) %>%
      slice_head(n = 3)

    impact_tbl <- pick_internal_breakout_rows_app(mode) %>%
      filter(.data$owner == .env$t$team) %>%
      mutate(
        pick_key = paste(.data$year, .data$round, .data$original_team, sep = "_"),
        delta = coalesce(.data$delta, .data$new_mean - .data$cur_mean)
      ) %>%
      group_by(.data$impact_bucket, .data$round) %>%
      summarise(
        n_picks = n_distinct(.data$pick_key),
        current = sum(.data$cur_mean, na.rm = TRUE),
        new = sum(.data$new_mean, na.rm = TRUE),
        delta = sum(.data$delta, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(
        impact_bucket = c("Own picks", "Incoming outright picks", "Swaps / protections"),
        round = c(1L, 2L),
        fill = list(n_picks = 0L, current = 0, new = 0, delta = 0)
      ) %>%
      arrange(factor(.data$impact_bucket, levels = c("Own picks", "Incoming outright picks", "Swaps / protections")), .data$round)

    html_tbl <- function(tbl, empty_msg = "No picks") {
      if (nrow(tbl) == 0) return(tags$div(style = "color:#777; font-size:11px;", empty_msg))
      tags$table(style = "width:100%; border-collapse:collapse; font-size:11px;",
        tags$thead(tags$tr(
          tags$th(style = "text-align:left; color:#aaa; padding:4px;", "Pick"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px;", paste0("Δ ", unit)),
          tags$th(style = "text-align:right; color:#aaa; padding:4px;", "Current"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px;", "3-2-1")
        )),
        tags$tbody(lapply(seq_len(nrow(tbl)), function(i) {
          r <- tbl[i, ]
          tags$tr(
            tags$td(style = "padding:4px;", sprintf("%s %s", r$year, r$short_label)),
            tags$td(style = sprintf("padding:4px; text-align:right; color:%s;", ifelse(r$delta >= 0, "#10b981", "#ef4444")), sprintf("%+.1f", r$delta)),
            tags$td(style = "padding:4px; text-align:right;", sprintf("%.1f", r$cur_mean)),
            tags$td(style = "padding:4px; text-align:right;", sprintf("%.1f", r$new_mean))
          )
        }))
      )
    }

    breakout_tbl_round <- function(tbl, round_no) {
      tbl <- tbl %>% filter(.data$round == .env$round_no)
      tags$table(style = "width:100%; border-collapse:collapse; font-size:11px; margin-top:8px;",
        tags$thead(tags$tr(
          tags$th(style = "text-align:left; color:#aaa; padding:4px;", "Bucket"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px;", "# Picks"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px;", "Current"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px;", "3-2-1"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px;", paste0("Δ ", unit))
        )),
        tags$tbody(lapply(seq_len(nrow(tbl)), function(i) {
          r <- tbl[i, ]
          tags$tr(
            tags$td(style = "padding:4px;", r$impact_bucket),
            tags$td(style = "padding:4px; text-align:right;", sprintf("%d", as.integer(r$n_picks))),
            tags$td(style = "padding:4px; text-align:right;", sprintf("%.1f", r$current)),
            tags$td(style = "padding:4px; text-align:right;", sprintf("%.1f", r$new)),
            tags$td(style = sprintf("padding:4px; text-align:right; color:%s;", ifelse(r$delta >= 0, "#10b981", "#ef4444")), sprintf("%+.1f", r$delta))
          )
        }))
      )
    }

    card(class = "mt-2 team-detail-card",
      card_header(style = "border-left:3px solid #6d28d9;",
        team_header_tag_app(
          t$team,
          size = 34,
          show_full = TRUE
        )),
      card_body(style = "font-size:12px; color:#bbb; line-height:1.5; overflow:visible; max-height:none; padding-top:14px;",
        layout_columns(
          col_widths = c(6, 6),
          tags$div(
            tags$div(style = "font-weight:700; color:#10b981; margin-bottom:4px;", "Most positive pick impacts"),
            html_tbl(pos_tbl, "No positive pick impacts")
          ),
          tags$div(
            tags$div(style = "font-weight:700; color:#ef4444; margin-bottom:4px;", "Most negative pick impacts"),
            html_tbl(neg_tbl, "No negative pick impacts")
          )
        ),
        tags$hr(style = "border-color:#1a1a2a; margin:10px 0 6px 0;"),
        tags$div(style = "font-weight:700; color:#d0d0d0; margin-top:2px;", "Impact by pick type and round"),
        layout_columns(
          col_widths = c(6, 6),
          tags$div(
            tags$div(style = "font-weight:700; color:#aaa; margin-top:6px;", "Round 1"),
            breakout_tbl_round(impact_tbl, 1L)
          ),
          tags$div(
            tags$div(style = "font-weight:700; color:#aaa; margin-top:6px;", "Round 2"),
            breakout_tbl_round(impact_tbl, 2L)
          )
        )
      )
    )
  })

  # ---- Transition heatmap ----
  output$trans_heatmap <- renderPlotly({
    m <- trans_mat
    y_order <- rev(rownames(m))
    m_y <- m[y_order, , drop = FALSE]
    z <- round(m_y * 100, 1)
    plot_ly(
      x = tier_short[colnames(m_y)],
      y = tier_short[y_order],
      z = z, type = "heatmap",
      colorscale = list(c(0, "#0f0f1a"), c(1, "#6d28d9")),
      text = matrix(sprintf("%.1f%%", z), nrow = nrow(z)),
      texttemplate = "%{text}", textfont = list(size = 10, color = "#ddd"),
      hovertemplate = "From %{y} to %{x}<br>Probability: %{z:.1f}%<extra></extra>"
    ) %>%
      plotly_dark(
        xaxis = list(title = list(text = "Year t + 1", standoff = 24), side = "bottom", automargin = TRUE),
        yaxis = list(title = list(text = "Year t", standoff = 42), autorange = "reversed", automargin = TRUE),
        margin = list(l = 95, r = 30, t = 30, b = 75)
      )
  })

  # ---- Transition state diagram ----
  output$trans_diagram <- renderPlot({
    par(bg = "#0f0f1a", mar = c(0, 0, 0, 0))
    K <- length(TIERS)
    # circular layout
    ang <- seq(pi/2, pi/2 - 2*pi, length.out = K + 1)[1:K]
    xs <- cos(ang); ys <- sin(ang)
    plot(xs, ys, type = "n", xlim = c(-1.6, 1.6), ylim = c(-1.6, 1.6),
         axes = FALSE, xlab = "", ylab = "", asp = 1)

    # draw edges with width ~ probability (skip tiny ones)
    for (i in 1:K) for (j in 1:K) {
      p <- trans_mat[i, j]
      if (p < 0.05) next
      if (i == j) {
        # self-loop label only (drawn as node ring later)
        next
      }
      x0 <- xs[i]; y0 <- ys[i]; x1 <- xs[j]; y1 <- ys[j]
      # shorten so arrows don't overlap nodes
      dx <- x1 - x0; dy <- y1 - y0; len <- sqrt(dx^2 + dy^2)
      ux <- dx/len; uy <- dy/len
      r <- 0.22
      arrows(x0 + ux*r, y0 + uy*r, x1 - ux*r, y1 - uy*r,
             length = 0.08, lwd = 1 + p * 8,
             col = adjustcolor("#6d28d9", alpha.f = min(1, 0.25 + p)))
    }

    # nodes with self-loop probability shown
    cols <- tier_colors[TIERS]
    for (i in 1:K) {
      sp <- trans_mat[i, i]
      symbols(xs[i], ys[i], circles = 0.20, add = TRUE, inches = FALSE,
              bg = adjustcolor(cols[i], alpha.f = 0.85), fg = "#ddd")
      text(xs[i], ys[i] + 0.005, tier_short[TIERS[i]],
           col = "white", cex = 0.8, font = 2)
      text(xs[i], ys[i] - 0.30, sprintf("stay %.0f%%", sp * 100),
           col = "#aaa", cex = 0.7)
    }
    title(main = "", col.main = "#ddd")
  })

  # ---- Pick value curves ----
  pick_curve_base_plot <- function(pc, x_title, y_title = "4-Yr Win Shares") {
    # The curve shows two distinct uncertainty concepts:
    #   1) EV credible interval = uncertainty around the posterior mean pick value.
    #   2) Player outcome interval = asymmetric 10th-90th percentile realized outcomes.
    # Do not use war_sd as the ribbon; that is predictive dispersion and can be
    # misleading for skewed / option-like pick outcomes, especially in Round 2.
    for (nm in c("ev_q05", "ev_q50", "ev_q95", "outcome_q10", "outcome_q90")) {
      if (!nm %in% names(pc)) pc[[nm]] <- NA_real_
    }

    pc <- pc %>%
      mutate(
        ev_q05 = coalesce(.data$ev_q05, .data$expected_war),
        ev_q50 = coalesce(.data$ev_q50, .data$expected_war),
        ev_q95 = coalesce(.data$ev_q95, .data$expected_war),
        has_ev_interval = is.finite(.data$ev_q05) & is.finite(.data$ev_q95),
        has_outcome_interval = is.finite(.data$outcome_q10) & is.finite(.data$outcome_q90),
        ev_hover = sprintf(
          "Pick %s<br>EV: %.1f<br>90%% EV CI: [%.1f, %.1f]",
          .data$pick, .data$expected_war, .data$ev_q05, .data$ev_q95
        ),
        outcome_hover = sprintf(
          "Pick %s<br>Player outcome 10th-90th: [%.1f, %.1f]",
          .data$pick, .data$outcome_q10, .data$outcome_q90
        )
      )

    p <- plot_ly(pc, x = ~pick)

    if (any(pc$has_ev_interval, na.rm = TRUE)) {
      p <- p %>%
        add_ribbons(
          data = pc %>% filter(.data$has_ev_interval),
          x = ~pick,
          ymin = ~ev_q05,
          ymax = ~ev_q95,
          name = "90% EV credible interval",
          text = ~ev_hover,
          hovertemplate = "%{text}<extra></extra>",
          line = list(color = "transparent"),
          fillcolor = "rgba(109,40,217,0.16)"
        )
    }

    if (any(pc$has_outcome_interval, na.rm = TRUE)) {
      p <- p %>%
        add_ribbons(
          data = pc %>% filter(.data$has_outcome_interval),
          x = ~pick,
          ymin = ~outcome_q10,
          ymax = ~outcome_q90,
          name = "Player outcomes 10th-90th",
          text = ~outcome_hover,
          hovertemplate = "%{text}<extra></extra>",
          line = list(color = "transparent"),
          fillcolor = "rgba(245,158,11,0.13)"
        )
    }

    if ("emp_mean" %in% names(pc) && any(is.finite(pc$emp_mean))) {
      p <- p %>%
        add_markers(y = ~emp_mean, name = "Empirical slot mean",
                    marker = list(color = "#f59e0b", size = 6),
                    hovertemplate = "Pick %{x}<br>Empirical mean: %{y:.1f}<extra></extra>")
    }

    p %>%
      add_lines(y = ~expected_war, name = "Posterior mean / EV",
                line = list(color = "#6d28d9", width = 2.5),
                hovertemplate = "Pick %{x}<br>EV: %{y:.1f}<extra></extra>") %>%
      plotly_dark(
        xaxis = list(title = list(text = x_title), dtick = 5),
        yaxis = list(title = list(text = y_title), tickformat = ".1f"),
        legend = list(
          orientation = "v",
          x = 1.08,
          y = 0.50,
          xanchor = "left",
          yanchor = "middle",
          font = list(size = 9),
          bgcolor = "rgba(15,15,26,0.86)",
          bordercolor = "rgba(255,255,255,0.08)",
          borderwidth = 1
        ),
        margin = list(l = 70, r = 225, t = 45, b = 58)
      )
  }

  output$pick_curve_plot_combined <- renderPlotly({
    pc <- pick_curve %>%
      filter(.data$pick >= 1, .data$pick <= 60) %>%
      arrange(.data$pick)

    if (nrow(pc) == 0) {
      return(plot_ly() %>%
               plotly_dark(
                 xaxis = list(title = list(text = "Pick")),
                 yaxis = list(title = list(text = "4-Yr Win Shares")),
                 annotations = list(text = "Combined pick curve unavailable", x = 0.5, y = 0.5,
                                    xref = "paper", yref = "paper", showarrow = FALSE)))
    }

    p <- pick_curve_base_plot(pc, x_title = "Pick", y_title = "4-Yr Win Shares")

    pc2 <- pc %>% filter(.data$pick >= 31, .data$pick <= 60)
    if (nrow(pc2) > 0 && "p_play" %in% names(pc2) && any(is.finite(pc2$p_play))) {
      if (all(c("p_play_q05", "p_play_q95") %in% names(pc2)) &&
          any(is.finite(pc2$p_play_q05)) && any(is.finite(pc2$p_play_q95))) {
        p <- p %>%
          add_ribbons(data = pc2 %>% filter(is.finite(.data$p_play_q05), is.finite(.data$p_play_q95)),
                      x = ~pick,
                      ymin = ~(100 * p_play_q05),
                      ymax = ~(100 * p_play_q95),
                      name = "90% P(play) interval",
                      yaxis = "y2",
                      line = list(color = "transparent"),
                      fillcolor = "rgba(16,185,129,0.13)",
                      hovertemplate = "Pick %{x}<br>90% P(play): %{y:.1f}%<extra></extra>")
      }

      p <- p %>%
        add_lines(data = pc2, x = ~pick, y = ~(100 * p_play),
                  name = "Modeled P(play) %",
                  yaxis = "y2",
                  line = list(color = "#10b981", width = 2, dash = "dot"),
                  hovertemplate = "Pick %{x}<br>P(play): %{y:.1f}%<extra></extra>")

      if ("emp_p_play" %in% names(pc2) && any(is.finite(pc2$emp_p_play))) {
        p <- p %>%
          add_markers(data = pc2 %>% filter(is.finite(.data$emp_p_play)),
                      x = ~pick, y = ~(100 * emp_p_play),
                      name = "Empirical P(play) %",
                      yaxis = "y2",
                      marker = list(color = "#e5e7eb", size = 6, symbol = "x"),
                      hovertemplate = "Pick %{x}<br>Empirical P(play): %{y:.1f}%<extra></extra>")
      }
    }

    p %>%
      layout(
        shapes = list(
          list(type = "line", x0 = 30.5, x1 = 30.5, xref = "x",
               y0 = 0, y1 = 1, yref = "paper",
               line = list(color = "rgba(229,231,235,0.55)", width = 1.2, dash = "dash"))
        ),
        annotations = list(
          list(x = 15.5, y = 1.04, xref = "x", yref = "paper",
               text = "Round 1", showarrow = FALSE,
               font = list(color = "#aaa", size = 11)),
          list(x = 45.5, y = 1.04, xref = "x", yref = "paper",
               text = "Round 2", showarrow = FALSE,
               font = list(color = "#aaa", size = 11))
        ),
        xaxis = list(title = list(text = "Pick"), dtick = 5, range = c(1, 60)),
        yaxis = list(title = list(text = "4-Yr Win Shares"), gridcolor = "#1a1a2a", zerolinecolor = "#333"),
        yaxis2 = list(
          title = list(text = "P(play) %", standoff = 18),
          overlaying = "y",
          side = "right",
          range = c(0, 100),
          automargin = TRUE,
          gridcolor = "rgba(0,0,0,0)",
          zerolinecolor = "#333"
        ),
        legend = list(
          orientation = "v",
          x = 1.08,
          y = 0.50,
          xanchor = "left",
          yanchor = "middle",
          font = list(size = 9),
          bgcolor = "rgba(15,15,26,0.86)",
          bordercolor = "rgba(255,255,255,0.08)",
          borderwidth = 1
        ),
        margin = list(l = 70, r = 245, t = 58, b = 58)
      )
  })

  # Backward-compatible alias in case an older UI still references the combined
  # curve output name.
  output$pick_curve_plot <- renderPlotly({
    pc <- pick_curve
    pick_curve_base_plot(pc, x_title = "Pick", y_title = "4-Yr Win Shares")
  })

  # ---- Validation panel ----
  output$validation_panel <- renderUI({
    pm <- stan_diag$pick_model
    pm2 <- stan_diag$pick2_model %||% NULL
    mk <- stan_diag$markov_model
    tags$div(style = "font-size:11px; color:#bbb; line-height:1.7;",
      tags$div(tags$strong(style = "color:#6d28d9;", "Pick-value model")),
      tags$div(sprintf("Curve: %s", pm$curve_type)),
      tags$div(sprintf("max R-hat = %.3f (want < 1.01)", pm$max_rhat)),
      tags$div(sprintf("min bulk-ESS = %s", format(pm$min_ess, big.mark = ","))),
      tags$div(sprintf("90%% PPC %s coverage = %.0f%%",
                       ifelse(is.null(pm$ppc_level), "slot", pm$ppc_level),
                       pm$ppc_cover * 100)),
      tags$div(sprintf("alpha=%.1f beta=%.3f gamma=%.1f", pm$alpha, pm$beta, pm$gamma)),
      if (!is.null(pm$nu)) tags$div(sprintf("Student-t nu=%.2f", pm$nu)),
      if (!is.null(pm$tau_log_sigma_rw)) tags$div(sprintf("sigma smoothing tau=%.4f", pm$tau_log_sigma_rw)),
      if (!is.null(pm$sigma_pick_1)) tags$div(sprintf(
        "sigma[pick]: #1 %.2f | #5 %.2f | #10 %.2f | #30 %.2f",
        pm$sigma_pick_1, pm$sigma_pick_5, pm$sigma_pick_10, pm$sigma_pick_30
      )),
      # Backward-compatible display for older dashboard_data.rds files.
      if (is.null(pm$sigma_pick_1) && !is.null(pm$sigma_base)) tags$div(sprintf(
        "sigma_base=%.2f | sigma_slope=%.4f", pm$sigma_base, pm$sigma_slope
      )),
      if (!is.null(pm$n_players)) tags$div(sprintf("player rows = %s", format(pm$n_players, big.mark = ","))),
      if (!is.null(pm2)) tags$div(style = "margin-top:8px;", tags$strong(style = "color:#6d28d9;", "Round-2 hurdle model")),
      if (!is.null(pm2)) tags$div(sprintf("Curve: %s", pm2$curve_type)),
      if (!is.null(pm2)) tags$div(sprintf("played rate: empirical %.1f%% | P(play): #31 %.1f%%, #45 %.1f%%, #60 %.1f%%",
                                           100 * pm2$played_rate, 100 * pm2$p_play_31, 100 * pm2$p_play_45, 100 * pm2$p_play_60)),
      if (!is.null(pm2)) tags$div(sprintf("max R-hat = %.3f | 90%% PPC coverage = %.0f%%",
                                           pm2$max_rhat, 100 * pm2$ppc_cover)),
      tags$hr(style = "border-color:#222;"),
      tags$div(tags$strong(style = "color:#6d28d9;", "Markov model")),
      tags$div(sprintf("%s transitions over %d seasons",
                       format(mk$n_transitions, big.mark = ","), mk$n_seasons)),
      tags$div(sprintf("2nd eigenvalue = %.3f", mk$lambda2)),
      tags$div(sprintf("mixing time = %.1f seasons", mk$mixing_time)),
      tags$div(sprintf("Stan vs closed-form max diff = %.4f", mk$max_abs_diff)),
      tags$hr(style = "border-color:#222;"),
      tags$div(style = "color:#888;",
        "Stationary tier mix: ",
        paste(sprintf("%s %.0f%%", tier_short[TIERS], stationary * 100),
              collapse = " | ")))
  })

  # ==========================================================================
  # TAB 6: SINGLE PICK VALUATION
  # ==========================================================================

  # populate team and pick choices for the chosen year / team
  observeEvent(input$sp_year, {
    teams <- pick_display_assets %>%
      filter(.data$year == as.integer(input$sp_year)) %>%
      distinct(owner) %>%
      arrange(owner) %>%
      pull(owner)
    if (length(teams) == 0L) {
      updateSelectInput(session, "sp_team", choices = character(0), selected = character(0))
    } else {
      updateSelectInput(session, "sp_team", choices = teams, selected = teams[1])
    }
  }, ignoreNULL = FALSE)

  observe({
    req(input$sp_year, input$sp_team)
    opts <- pick_display_assets %>%
      filter(.data$year == as.integer(input$sp_year), .data$owner == input$sp_team) %>%
      arrange(round, short_label)
    if (nrow(opts) == 0L) {
      updateSelectInput(session, "sp_asset", choices = character(0), selected = character(0))
    } else {
      choice_vec <- setNames(opts$display_asset_id, opts$short_label)
      updateSelectInput(session, "sp_asset", choices = choice_vec, selected = opts$display_asset_id[1])
    }
  })

  sp_row <- reactive({
    req(input$sp_asset)
    pick_display_assets %>% filter(display_asset_id == input$sp_asset)
  })

  sp_stats_outcome <- reactive({
    req(input$sp_asset)
    pick_display_value_summary %>% filter(display_asset_id == input$sp_asset)
  })

  sp_stats_ev <- reactive({
    req(input$sp_asset)
    pick_display_value_ev_summary %>% filter(display_asset_id == input$sp_asset)
  })

  swap_exercise_summary_for_display <- function(r, system = c("cur", "new")) {
    system <- match.arg(system)
    text <- paste(r$label %||% "", r$obligation %||% "", r$notes %||% "")
    origs <- split_abbrs(r$member_original_teams)
    terms <- extract_swap_terms_from_text(text, origs, r$owner)
    if (is.na(terms$holder) || is.na(terms$target)) return(NULL)
    arr <- team_slot_array_for_round_app(r$round, system)
    yr <- as.character(r$year)
    if (is.null(arr) || is.null(dimnames(arr)[[2]]) || is.null(dimnames(arr)[[3]])) return(NULL)
    if (!all(c(terms$holder, terms$target) %in% dimnames(arr)[[2]]) || !yr %in% dimnames(arr)[[3]]) return(NULL)
    holder_slot <- arr[, terms$holder, yr]
    target_slot <- arr[, terms$target, yr]
    exercised <- !is.na(holder_slot) & !is.na(target_slot) &
      target_slot < holder_slot &
      target_slot >= terms$min_pick & target_slot <= terms$max_pick
    probability_summary_from_indicator(exercised)
  }

  prob_box <- function(title, stat, col) {
    if (is.null(stat) || nrow(stat) == 0 || is.na(stat$prob)) return(NULL)
    tags$div(style = sprintf(
      "flex:1; min-width:0; padding:8px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;", col),
      tags$div(style = "font-size:10px; color:#888; white-space:nowrap;", title),
      tags$div(style = sprintf("font-size:18px; font-weight:700; color:%s;", col),
               sprintf("%.1f%%", 100 * stat$prob)),
      tags$div(style = "font-size:9px; color:#aaa; white-space:nowrap;",
               sprintf("90%% CI [%.1f%%, %.1f%%]", 100 * stat$q05, 100 * stat$q95)))
  }

  prob_card_row <- function(label, cur_stat, new_stat) {
    tags$div(style = "margin-top:10px;",
      tags$div(style = "font-size:11px; font-weight:700; color:#d0d0d0; margin-bottom:5px;", label),
      tags$div(style = "display:flex; gap:8px; align-items:stretch; flex-wrap:nowrap;",
        prob_box("Current", cur_stat, "#3b82f6"),
        prob_box("3-2-1", new_stat, "#f59e0b")
      )
    )
  }

  guaranteed_swap_entitlement <- function(r) {
    text <- paste(r$short_label %||% "", r$label %||% "", r$obligation %||% "", r$notes %||% "")
    stringr::str_detect(stringr::str_to_lower(text), "own or") &&
      stringr::str_detect(stringr::str_to_lower(text), "swap")
  }

  output$sp_obligation <- renderUI({
    r <- sp_row()
    s <- sp_stats_outcome()
    if (nrow(r) == 0) return(NULL)

    convey_cur <- if (r$display_asset_id %in% colnames(display_convey_cur_draws)) {
      probability_summary_from_indicator(display_convey_cur_draws[, r$display_asset_id] > 0)
    } else if (nrow(s) > 0 && "cur_convey_prob" %in% names(s)) {
      tibble(prob = s$cur_convey_prob, q05 = s$cur_convey_prob, q95 = s$cur_convey_prob)
    } else NULL

    convey_new <- if (r$display_asset_id %in% colnames(display_convey_new_draws)) {
      probability_summary_from_indicator(display_convey_new_draws[, r$display_asset_id] > 0)
    } else if (nrow(s) > 0 && "new_convey_prob" %in% names(s)) {
      tibble(prob = s$new_convey_prob, q05 = s$new_convey_prob, q95 = s$new_convey_prob)
    } else NULL

    if (isTRUE(guaranteed_swap_entitlement(r))) {
      convey_cur <- tibble(prob = 1, q05 = 1, q95 = 1)
      convey_new <- tibble(prob = 1, q05 = 1, q95 = 1)
    }

    swap_cur <- swap_exercise_summary_for_display(r, "cur")
    swap_new <- swap_exercise_summary_for_display(r, "new")

    line_div <- function(..., extra_style = "") tags$div(style = paste0("margin-bottom:10px;", extra_style), ...)

    tags$div(style = "font-size:11px; color:#aaa; line-height:1.55;",
      tags$div(tags$strong(style = "color:#6d28d9;", "Pick details")),
      line_div(sprintf("Current Team: %s", r$owner)),
      line_div(sprintf("Original Team%s: %s", ifelse(str_detect(r$member_original_teams, ","), "s", ""), r$member_original_teams)),
      if (!is.na(r$fixed_slot_display)) line_div(tags$span(style = "color:#10b981;", sprintf("Actual Pick: %s", r$fixed_slot_display))),
      line_div(sprintf("Obligation: %s", pick_obligation_display_app(r))),
      prob_card_row("Conveyance Probability", convey_cur, convey_new),
      if (!is.null(swap_cur) || !is.null(swap_new)) prob_card_row("Swap Exercise Probability", swap_cur, swap_new),
      if (r$year == 2026)
        line_div("Locked to the actual 2026 draft result", extra_style = "color:#10b981;"))
  })

  output$sp_headline <- renderUI({
    s_out <- sp_stats_outcome()
    s_ev  <- sp_stats_ev()
    if (nrow(s_out) == 0 || nrow(s_ev) == 0) return(NULL)

    box <- function(title, mean, q05, q95, col, subtitle = "90% interval") {
      tags$div(style = sprintf(
        "flex:1; padding:12px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;", col),
        tags$div(style = "font-size:11px; color:#888;", title),
        tags$div(style = sprintf("font-size:26px; font-weight:700; color:%s;", col),
                 sprintf("%.1f", mean)),
        tags$div(style = "font-size:11px; color:#aaa;",
                 sprintf("%s: [%.1f, %.1f]", subtitle, q05, q95)))
    }

    delta_box <- function(delta, detail) {
      tags$div(style = sprintf(
        "flex:1; padding:12px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;",
        ifelse(delta >= 0, "#10b981", "#ef4444")),
        tags$div(style = "font-size:11px; color:#888;", "Delta (3-2-1 - current)"),
        tags$div(style = sprintf("font-size:26px; font-weight:700; color:%s;",
                                 ifelse(delta >= 0, "#10b981", "#ef4444")),
                 sprintf("%+.1f", delta)),
        tags$div(style = "font-size:11px; color:#aaa;", detail))
    }

    metric_row <- function(title, s, detail) {
      delta <- s$new_mean - s$cur_mean
      tags$div(style = "margin-bottom:12px;",
        tags$div(style = "font-size:12px; font-weight:700; color:#d0d0d0; margin:0 0 6px 2px;", title),
        tags$div(style = "display:flex; gap:12px; align-items:stretch;",
          box("Current system", s$cur_mean, s$cur_q05, s$cur_q95, "#3b82f6"),
          box("3-2-1 system",   s$new_mean, s$new_q05, s$new_q95, "#f59e0b"),
          delta_box(delta, detail)
        )
      )
    }

    tags$div(
      metric_row(
        "4-Yr Win Share Outcomes impact",
        s_out,
        "sampled player-level 4-Yr WS outcomes"
      ),
      metric_row(
        "Expected pick value impact",
        s_ev,
        "posterior mean slot value, 4-Yr WS scale"
      )
    )
  })

  single_pick_density_plot <- function(cur, new, stats_row, x_title, hover_label) {
    cur_density <- density_curve_df(cur)
    new_density <- density_curve_df(new)

    plot_ly() %>%
      add_lines(data = cur_density, x = ~x, y = ~density,
                name = "Current", fill = "tozeroy",
                line = list(color = "#3b82f6", width = 2.5),
                hovertemplate = sprintf("Current %s<br>WS: %%{x:.1f}<br>Density: %%{y:.1f}<extra></extra>", hover_label)) %>%
      add_lines(data = new_density, x = ~x, y = ~density,
                name = "3-2-1", fill = "tozeroy",
                line = list(color = "#f59e0b", width = 2.5),
                hovertemplate = sprintf("3-2-1 %s<br>WS: %%{x:.1f}<br>Density: %%{y:.1f}<extra></extra>", hover_label)) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "#0f0f1a",
        font = list(family = "IBM Plex Mono", color = "#999"),
        xaxis = list(title = x_title, gridcolor = "#1a1a2a", zerolinecolor = "#333"),
        yaxis = list(title = "Density", gridcolor = "#1a1a2a"),
        legend = list(x = 0.65, y = 0.95, font = list(size = 10)),
        shapes = list(
          list(type = "line", x0 = stats_row$cur_mean, x1 = stats_row$cur_mean, y0 = 0, y1 = 1,
               yref = "paper", line = list(color = "#3b82f6", dash = "dash")),
          list(type = "line", x0 = stats_row$new_mean, x1 = stats_row$new_mean, y0 = 0, y1 = 1,
               yref = "paper", line = list(color = "#f59e0b", dash = "dash"))))
  }

  output$sp_dist_ev <- renderPlotly({
    req(input$sp_asset)
    single_pick_density_plot(
      cur = display_asset_cur_ev_draws[, input$sp_asset],
      new = display_asset_new_ev_draws[, input$sp_asset],
      stats_row = sp_stats_ev(),
      x_title = "Expected Pick Value (4-Yr WS scale)",
      hover_label = "EV"
    )
  })

  output$sp_dist_outcome <- renderPlotly({
    req(input$sp_asset)
    single_pick_density_plot(
      cur = display_asset_cur_draws[, input$sp_asset],
      new = display_asset_new_draws[, input$sp_asset],
      stats_row = sp_stats_outcome(),
      x_title = "Realized 4-Yr Win Shares outcome",
      hover_label = "outcome"
    )
  })


  # ===========================================================================
  # TAB 7: PICK MOVERS
  # ===========================================================================

  pick_movers_table <- function(value_mode, year_filter, round_filter) {
    year_filter <- if (is.null(year_filter) || length(year_filter) == 0L) "All" else as.character(year_filter[[1]])
    round_filter <- if (is.null(round_filter) || length(round_filter) == 0L) "All" else as.character(round_filter[[1]])
    unit <- value_mode_unit(value_mode)
    delta_nm <- paste0("Δ ", unit)

    rows <- pick_impact_rows_app(value_mode)
    if (!is.null(year_filter) && !identical(year_filter, "All")) {
      rows <- rows %>% filter(.data$year == as.integer(.env$year_filter))
    }
    if (!is.null(round_filter) && !identical(round_filter, "All")) {
      rows <- rows %>% filter(.data$round == as.integer(.env$round_filter))
    }

    if (is.null(rows) || nrow(rows) == 0L) {
      return(tibble(
        Direction = character(), Owner = character(), Year = integer(),
        Round = integer(), Current = numeric(), `3-2-1` = numeric(),
        !!delta_nm := numeric(), PickInfo = character()
      ))
    }

    rows %>%
      mutate(
        Direction = if_else(.data$delta >= 0, "Positive", "Negative"),
        PickInfo = sprintf("%s %s", .data$year, .data$short_label),
        Current = round(.data$cur_mean, 1),
        `3-2-1` = round(.data$new_mean, 1),
        Delta = round(.data$delta, 1)
      ) %>%
      arrange(desc(.data$delta), .data$year, .data$round, .data$owner, .data$short_label) %>%
      transmute(Direction, Owner = .data$owner, Year = as.integer(.data$year),
                Round = .data$round, Current, `3-2-1`, !!delta_nm := Delta, PickInfo)
  }

  pick_movers_ui_table <- function(value_mode) {
    tryCatch({
      tbl <- pick_movers_table(value_mode, input$pm_year %||% "All", input$pm_round %||% "All")
      unit <- value_mode_unit(value_mode)
      delta_col <- paste0("Δ ", unit)

      if (is.null(tbl) || nrow(tbl) == 0L) {
        return(tags$div(class = "pm-simple-wrap",
          tags$div(style = "color:#999; padding:10px;", "No picks match the selected filters.")
        ))
      }

      col_names <- setdiff(names(tbl), "PickInfo")
      right_cols <- c("Year", "Round", "Current", "3-2-1", delta_col)

      tags$div(class = "pm-simple-wrap",
        tags$table(class = "pm-simple-table",
          tags$thead(tags$tr(lapply(col_names, function(nm) {
            cls <- if (nm %in% right_cols) "num" else NULL
            tags$th(class = cls, nm)
          }))),
          tags$tbody(lapply(seq_len(nrow(tbl)), function(i) {
            r <- tbl[i, , drop = FALSE]
            tags$tr(title = as.character(r$PickInfo),
              lapply(col_names, function(nm) {
                val <- r[[nm]][[1]]
                cls <- if (nm %in% right_cols) "num" else NULL
                style <- ""
                if (identical(nm, delta_col)) {
                  dv <- suppressWarnings(as.numeric(val))
                  style <- sprintf("color:%s;", ifelse(is.na(dv) || dv >= 0, "#10b981", "#ef4444"))
                  val <- sprintf("%+.1f", dv)
                }
                tags$td(class = cls, style = style, as.character(val))
              })
            )
          }))
        )
      )
    }, error = function(e) {
      tags$div(class = "pm-simple-wrap",
        tags$div(style = "color:#ef4444; padding:10px; font-weight:700;",
                 paste("Pick Movers error:", conditionMessage(e)))
      )
    })
  }

  output$pm_ev_table <- renderUI({
    pick_movers_ui_table("ev")
  })

  output$pm_outcome_table <- renderUI({
    pick_movers_ui_table("outcome")
  })

  # ==========================================================================
  # TAB 7: TRADE MACHINE
  # ==========================================================================

  output$tm_teamA_hdr <- renderUI(team_header_tag_app(input$tm_teamA %||% "Team 1", size = 34, show_full = TRUE))
  output$tm_teamB_hdr <- renderUI(team_header_tag_app(input$tm_teamB %||% "Team 2", size = 34, show_full = TRUE))

  # picks each team currently OWNS (can send out)
  picks_owned_by <- function(team) {
    pick_display_assets %>%
      filter(owner == team) %>%
      arrange(year, round, short_label)
  }

  output$tm_picksA_picker <- renderUI({
    opts <- picks_owned_by(input$tm_teamA)
    choices <- if (nrow(opts) == 0L) character(0) else setNames(opts$display_asset_id, opts$trade_label)
    selectizeInput("tm_picksA", sprintf("Picks %s sends out", input$tm_teamA %||% "Team 1"),
                   choices = choices, multiple = TRUE,
                   options = list(placeholder = "select one or more picks"))
  })

  output$tm_picksB_picker <- renderUI({
    opts <- picks_owned_by(input$tm_teamB)
    choices <- if (nrow(opts) == 0L) character(0) else setNames(opts$display_asset_id, opts$trade_label)
    selectizeInput("tm_picksB", sprintf("Picks %s sends out", input$tm_teamB %||% "Team 2"),
                   choices = choices, multiple = TRUE,
                   options = list(placeholder = "select one or more picks"))
  })

  output$tm_picksA_out_hdr <- renderUI(
    tags$div(style = "display:flex; align-items:center; gap:8px; font-size:18px; font-weight:850;",
      team_logo_img_app(input$tm_teamA %||% "Team 1", size = 28),
      tags$span(sprintf("%s outgoing picks", input$tm_teamA %||% "Team 1"))
    )
  )
  output$tm_picksB_out_hdr <- renderUI(
    tags$div(style = "display:flex; align-items:center; gap:8px; font-size:18px; font-weight:850;",
      team_logo_img_app(input$tm_teamB %||% "Team 2", size = 28),
      tags$span(sprintf("%s outgoing picks", input$tm_teamB %||% "Team 2"))
    )
  )


  observeEvent(input$tm_teamA, {
    opts <- picks_owned_by(input$tm_teamA)
    choices <- if (nrow(opts) == 0L) character(0) else setNames(opts$display_asset_id, opts$trade_label)
    updateSelectizeInput(session, "tm_picksA", choices = choices, selected = character(0), server = TRUE)
  })
  observeEvent(input$tm_teamB, {
    opts <- picks_owned_by(input$tm_teamB)
    choices <- if (nrow(opts) == 0L) character(0) else setNames(opts$display_asset_id, opts$trade_label)
    updateSelectizeInput(session, "tm_picksB", choices = choices, selected = character(0), server = TRUE)
  })

  # ---- per-pick obligation controls (always shown for eligible picks) ----
  # For each selected pick we render a protection dropdown + a swap checkbox.
  # Input ids are deterministic: prot_<side>_<assetid>, swap_<side>_<assetid>.
  safe_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

  display_member_ids <- function(display_id) {
    pick_display_members %>%
      filter(.data$display_asset_id == .env$display_id) %>%
      pull(asset_id)
  }

  display_is_single_leg <- function(display_id) {
    r <- pick_display_assets %>% filter(display_asset_id == display_id)
    ids <- display_member_ids(display_id)
    nrow(r) == 1 && length(ids) == 1L && isTRUE(r$group_type == "single_asset")
  }

  # ---- value (per sim) of a single sent pick TO ITS RECEIVER ----
  # The Trade Machine keeps two concepts separate:
  #   1. Expected pick value: posterior mean slot value, no player-outcome noise.
  #   2. Realized player outcome: sampled player-level 4-Year WS draws.
  pick_conveys_app <- function(pos, protection) {
    if (is.null(protection) || length(protection) == 0 || is.na(protection)) {
      return(rep(TRUE, length(pos)))
    }
    if (protection == "none")    return(rep(TRUE, length(pos)))
    if (protection == "top1")    return(pos > 1)
    if (protection == "top2")    return(pos > 2)
    if (protection == "top3")    return(pos > 3)
    if (protection == "top4")    return(pos > 4)
    if (protection == "top5")    return(pos > 5)
    if (protection == "top6")    return(pos > 6)
    if (protection == "top8")    return(pos > 8)
    if (protection == "top10")   return(pos > 10)
    if (protection == "lottery") return(pos > 14)
    if (protection == "top16")   return(pos > 16)
    if (protection == "top20")   return(pos > 20)
    m <- stringr::str_match(protection, "^protected(\\d+)_(\\d+)$")
    if (!is.na(m[1, 1])) {
      lo <- as.integer(m[1, 2]); hi <- as.integer(m[1, 3])
      return(!(pos >= lo & pos <= hi))
    }
    m <- stringr::str_match(protection, "^convey(\\d+)_(\\d+)$")
    if (!is.na(m[1, 1])) {
      lo <- as.integer(m[1, 2]); hi <- as.integer(m[1, 3])
      return(pos >= lo & pos <= hi)
    }
    rep(TRUE, length(pos))
  }

  zero_trade_vec <- function() rep(0, nrow(asset_new_draws))

  team_slot_array_for_round <- function(round, system = c("new", "cur")) {
    system <- match.arg(system)
    if (as.integer(round) == 2L) {
      arr <- if (system == "new") team_slot2_new_draws else team_slot2_cur_draws
      if (!is.null(arr)) return(arr)
    }
    if (system == "new") team_slot_new_draws else team_slot_cur_draws
  }

  receiver_slot_vec <- function(receiver, yr, round, system = "new") {
    arr <- team_slot_array_for_round(round, system)
    yc <- as.character(yr)
    if (!is.null(arr) && !is.null(dimnames(arr)[[2]]) &&
        receiver %in% dimnames(arr)[[2]] &&
        yc %in% dimnames(arr)[[3]]) {
      return(arr[, receiver, yc])
    }
    rep(NA_real_, nrow(asset_new_draws))
  }

  underlying_pick_ev_base <- function(aid) {
    r <- pick_assets %>% filter(asset_id == aid)
    if (nrow(r) == 0) return(zero_trade_vec())

    if (r$year == 2026L && !is.na(r$fixed_slot)) {
      val <- slot_value_vec(rep(r$fixed_slot, nrow(asset_new_draws)))
      val[is.na(val)] <- 0
      return(val)
    }

    og_slot <- asset_slot_new_draws[, aid]
    val <- slot_value_vec(og_slot)
    allocated <- asset_convey_new_draws[, aid] > 0
    out <- ifelse(!is.na(og_slot) & allocated, val, 0)
    out[is.na(out)] <- 0
    out
  }

  sent_pick_ev_value <- function(aid, side, receiver, ctrl_id = aid) {
    r <- pick_assets %>% filter(asset_id == aid)
    if (nrow(r) == 0) return(zero_trade_vec())

    base <- underlying_pick_ev_base(aid)
    if (r$year == 2026L && !is.na(r$fixed_slot)) return(base)

    pid <- paste0("prot_", side, "_", safe_id(ctrl_id))
    sid <- paste0("swap_", side, "_", safe_id(ctrl_id))
    prot <- input[[pid]]
    swap <- isTRUE(input[[sid]])
    yc   <- as.character(r$year)

    if (swap) {
      og_slot <- asset_slot_new_draws[, aid]
      rc_slot <- receiver_slot_vec(receiver, r$year, r$round, "new")
      val_og <- slot_value_vec(og_slot)
      val_rc <- slot_value_vec(rc_slot)
      eligible <- pick_conveys_app(og_slot, prot)
      sent_pick_is_better <- !is.na(og_slot) & !is.na(rc_slot) & og_slot < rc_slot
      out <- ifelse(eligible & sent_pick_is_better, pmax(val_og - val_rc, 0), 0)
      out[is.na(out)] <- 0
      return(out)
    }

    if (!is.null(prot) && prot != "none") {
      og_slot <- asset_slot_new_draws[, aid]
      raw_ev <- slot_value_vec(og_slot)
      out <- ifelse(!is.na(og_slot) & pick_conveys_app(og_slot, prot), raw_ev, 0)
      out[is.na(out)] <- 0
      return(out)
    }

    base
  }

  underlying_pick_outcome_base <- function(aid) {
    base <- asset_new_draws[, aid]
    base[is.na(base)] <- 0
    base
  }

  receiver_raw_outcome_vec <- function(receiver, yr, round) {
    cand <- pick_assets %>%
      filter(.data$year == as.integer(.env$yr),
             .data$round == as.integer(.env$round),
             .data$original_team == .env$receiver,
             .data$owner == .env$receiver) %>%
      arrange(pick_type != "own")
    if (nrow(cand) > 0) {
      aid_receiver <- cand$asset_id[1]
      if (aid_receiver %in% colnames(asset_raw_new_draws)) {
        out <- asset_raw_new_draws[, aid_receiver]
        out[is.na(out)] <- 0
        return(out)
      }
    }

    out <- slot_value_vec(receiver_slot_vec(receiver, yr, round, "new"))
    out[is.na(out)] <- 0
    out
  }

  sent_pick_outcome_value <- function(aid, side, receiver, ctrl_id = aid) {
    r <- pick_assets %>% filter(asset_id == aid)
    if (nrow(r) == 0) return(zero_trade_vec())

    base <- underlying_pick_outcome_base(aid)
    if (r$year == 2026L && !is.na(r$fixed_slot)) return(base)

    pid <- paste0("prot_", side, "_", safe_id(ctrl_id))
    sid <- paste0("swap_", side, "_", safe_id(ctrl_id))
    prot <- input[[pid]]
    swap <- isTRUE(input[[sid]])
    yc   <- as.character(r$year)

    if (swap) {
      og_slot <- asset_slot_new_draws[, aid]
      rc_slot <- receiver_slot_vec(receiver, r$year, r$round, "new")
      raw_og <- asset_raw_new_draws[, aid]
      raw_rc <- receiver_raw_outcome_vec(receiver, r$year, r$round)
      eligible <- pick_conveys_app(og_slot, prot)
      sent_pick_is_better <- !is.na(og_slot) & !is.na(rc_slot) & og_slot < rc_slot
      out <- ifelse(eligible & sent_pick_is_better, raw_og - raw_rc, 0)
      out[is.na(out)] <- 0
      return(out)
    }

    if (!is.null(prot) && prot != "none") {
      og_slot <- asset_slot_new_draws[, aid]
      raw_outcome <- asset_raw_new_draws[, aid]
      out <- ifelse(!is.na(og_slot) & pick_conveys_app(og_slot, prot), raw_outcome, 0)
      out[is.na(out)] <- 0
      return(out)
    }

    base
  }

  sent_display_ev_value <- function(display_id, side, receiver) {
    ids <- display_member_ids(display_id)
    ids <- ids[ids %in% colnames(asset_new_draws)]
    if (length(ids) == 0) return(zero_trade_vec())

    if (length(ids) == 1L && display_is_single_leg(display_id)) {
      return(sent_pick_ev_value(ids[1], side, receiver, ctrl_id = display_id))
    }

    if (display_id %in% colnames(display_asset_new_ev_draws)) {
      out <- display_asset_new_ev_draws[, display_id]
      out[is.na(out)] <- 0
      return(out)
    }

    cols <- vapply(ids, underlying_pick_ev_base, numeric(nrow(asset_new_draws)))
    if (is.null(dim(cols))) return(cols)
    rowSums(cols)
  }

  sent_display_outcome_value <- function(display_id, side, receiver) {
    ids <- display_member_ids(display_id)
    ids <- ids[ids %in% colnames(asset_new_draws)]
    if (length(ids) == 0) return(zero_trade_vec())

    if (length(ids) == 1L && display_is_single_leg(display_id)) {
      return(sent_pick_outcome_value(ids[1], side, receiver, ctrl_id = display_id))
    }

    if (display_id %in% colnames(display_asset_new_draws)) {
      out <- display_asset_new_draws[, display_id]
      out[is.na(out)] <- 0
      return(out)
    }

    cols <- vapply(ids, underlying_pick_outcome_base, numeric(nrow(asset_new_draws)))
    if (is.null(dim(cols))) return(cols)
    rowSums(cols)
  }

  side_out_ev_value <- function(ids, side, receiver) {
    if (is.null(ids) || length(ids) == 0) return(zero_trade_vec())
    cols <- vapply(ids, function(did) sent_display_ev_value(did, side, receiver),
                   numeric(nrow(asset_new_draws)))
    if (is.null(dim(cols))) return(cols)
    rowSums(cols)
  }

  side_outcome_value <- function(ids, side, receiver) {
    if (is.null(ids) || length(ids) == 0) return(zero_trade_vec())
    cols <- vapply(ids, function(did) sent_display_outcome_value(did, side, receiver),
                   numeric(nrow(asset_new_draws)))
    if (is.null(dim(cols))) return(cols)
    rowSums(cols)
  }

  side_outcome_component_matrix <- function(ids, side, receiver) {
    n <- nrow(asset_new_draws)
    if (is.null(ids) || length(ids) == 0) {
      return(matrix(numeric(0), nrow = n, ncol = 0))
    }
    cols <- vapply(ids, function(did) sent_display_outcome_value(did, side, receiver),
                   numeric(n))
    if (is.null(dim(cols))) {
      cols <- matrix(cols, nrow = n, ncol = 1)
    }
    colnames(cols) <- ids
    cols
  }


  trade_pick_stats <- function(x) {
    x <- as.numeric(x)
    x[!is.finite(x)] <- 0
    c(mean = mean(x, na.rm = TRUE), q10 = as.numeric(quantile(x, 0.10, na.rm = TRUE)), q90 = as.numeric(quantile(x, 0.90, na.rm = TRUE)))
  }

  render_trade_pick_table <- function(ids, side, receiver, pad_to = NULL) {
    if (is.null(ids)) ids <- character(0)
    rows <- pick_display_assets %>% filter(.data$display_asset_id %in% ids)
    if (nrow(rows) == 0L && (is.null(pad_to) || pad_to <= 0L)) {
      return(tags$div(style = "font-size:13px; color:#666; padding:4px 0;", "No picks selected"))
    }

    body_rows <- lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      did <- r$display_asset_id
      pid <- paste0("prot_", side, "_", safe_id(did))
      sid <- paste0("swap_", side, "_", safe_id(did))
      locked <- r$year == 2026
      eligible <- display_is_single_leg(did) && !locked

      ev_stat <- trade_pick_stats(sent_display_ev_value(did, side, receiver))
      tags$tr(
        tags$td(style = "max-width:0; font-size:15px;", tags$strong(class = "tm-pick-label", r$trade_label)),
        tags$td(if (eligible) selectInput(pid, NULL, choices = protection_choices, selected = input[[pid]] %||% "none", width = "115px") else tags$span(style = "color:#777;", "N/A")),
        tags$td(style = "text-align:center;", if (eligible) checkboxInput(sid, NULL, value = isTRUE(input[[sid]])) else tags$input(type = "checkbox", disabled = "disabled")),
        tags$td(style = "text-align:right; font-size:15px;", sprintf("%.1f", ev_stat["mean"]))
      )
    })

    if (is.null(pad_to) || !is.finite(pad_to)) pad_to <- nrow(rows)
    pad_n <- max(0L, as.integer(pad_to) - nrow(rows))
    if (pad_n > 0L) {
      spacer_rows <- lapply(seq_len(pad_n), function(i) {
        tags$tr(class = "tm-spacer-row",
          tags$td(style = "height:74px; padding:6px 0; border-bottom:0;", HTML("&nbsp;")),
          tags$td(style = "height:74px; padding:6px 0; border-bottom:0;", HTML("&nbsp;")),
          tags$td(style = "height:74px; padding:6px 0; border-bottom:0;", HTML("&nbsp;")),
          tags$td(style = "height:74px; padding:6px 0; border-bottom:0;", HTML("&nbsp;"))
        )
      })
      body_rows <- c(body_rows, spacer_rows)
    }

    total_ev <- trade_pick_stats(side_out_ev_value(ids, side, receiver))

    tags$div(class = "tm-pick-table",
      tags$table(
        tags$colgroup(
          tags$col(style = "width:50%;"),
          tags$col(style = "width:22%;"),
          tags$col(style = "width:14%;"),
          tags$col(style = "width:14%;")
        ),
        tags$thead(tags$tr(
          tags$th(style = "text-align:left;", "Picks"),
          tags$th(style = "text-align:left;", "Protection"),
          tags$th(style = "text-align:center;", "Swap Rights"),
          tags$th(style = "text-align:right;", "EPV")
        )),
        tags$tbody(
          body_rows,
          tags$tr(class = "tm-total-row",
            tags$td("Total"),
            tags$td(""),
            tags$td(""),
            tags$td(style = "text-align:right; font-size:15px;", sprintf("%.1f", total_ev["mean"]))
          )
        )
      )
    )
  }

  selected_pick_count <- function(x) {
    if (is.null(x)) 0L else length(x)
  }

  output$tm_picksA_table <- renderUI({
    pad_to <- max(selected_pick_count(input$tm_picksA), selected_pick_count(input$tm_picksB))
    render_trade_pick_table(input$tm_picksA, "A", receiver = input$tm_teamB, pad_to = pad_to)
  })

  output$tm_picksB_table <- renderUI({
    pad_to <- max(selected_pick_count(input$tm_picksA), selected_pick_count(input$tm_picksB))
    render_trade_pick_table(input$tm_picksB, "B", receiver = input$tm_teamA, pad_to = pad_to)
  })

  row_max_or_neginf <- function(mat) {
    if (is.null(mat) || ncol(mat) == 0L) {
      return(rep(-Inf, nrow(asset_new_draws)))
    }
    out <- apply(mat, 1, max, na.rm = TRUE)
    out[!is.finite(out)] <- -Inf
    out
  }

  trade_draws <- reactive({
    idsA <- input$tm_picksA
    idsB <- input$tm_picksB
    if ((is.null(idsA) || length(idsA) == 0) &&
        (is.null(idsB) || length(idsB) == 0)) return(NULL)

    A_out_ev <- side_out_ev_value(idsA, "A", receiver = input$tm_teamB)
    B_out_ev <- side_out_ev_value(idsB, "B", receiver = input$tm_teamA)

    A_out_outcome <- side_outcome_value(idsA, "A", receiver = input$tm_teamB)
    B_out_outcome <- side_outcome_value(idsB, "B", receiver = input$tm_teamA)

    # Per-pick realized outcome comparison after the trade. Team 1 receives the
    # picks selected on Team 2's side, and Team 2 receives the picks selected on
    # Team 1's side. This estimates who gets the single best player outcome, not
    # just the larger aggregate package.
    A_receives_components <- side_outcome_component_matrix(idsB, "B", receiver = input$tm_teamA)
    B_receives_components <- side_outcome_component_matrix(idsA, "A", receiver = input$tm_teamB)
    A_best_outcome <- row_max_or_neginf(A_receives_components)
    B_best_outcome <- row_max_or_neginf(B_receives_components)
    A_has_pick <- is.finite(A_best_outcome)
    B_has_pick <- is.finite(B_best_outcome)

    tibble(
      net_to_A_ev      = B_out_ev - A_out_ev,
      net_to_B_ev      = A_out_ev - B_out_ev,
      net_to_A_outcome = B_out_outcome - A_out_outcome,
      net_to_B_outcome = A_out_outcome - B_out_outcome,
      best_outcome_to_A = A_has_pick & (!B_has_pick | A_best_outcome > B_best_outcome),
      best_outcome_to_B = B_has_pick & (!A_has_pick | B_best_outcome > A_best_outcome),
      best_outcome_tie  = A_has_pick & B_has_pick & A_best_outcome == B_best_outcome,
      best_outcome_edge_to_A = ifelse(A_has_pick, A_best_outcome, 0) - ifelse(B_has_pick, B_best_outcome, 0)
    )
  })

  output$tm_verdict <- renderUI({
    d <- trade_draws()
    if (is.null(d)) return(tags$div(style = "color:#666;",
      "Select picks from one or both teams to assess the trade."))

    eA_ev  <- mean(d$net_to_A_ev)
    q05_ev <- quantile(d$net_to_A_ev, 0.05)
    q95_ev <- quantile(d$net_to_A_ev, 0.95)
    pA_ev  <- mean(d$net_to_A_ev > 0)
    pB_ev  <- mean(d$net_to_A_ev < 0)

    eA_outcome  <- mean(d$net_to_A_outcome)
    q05_outcome <- quantile(d$net_to_A_outcome, 0.05)
    q95_outcome <- quantile(d$net_to_A_outcome, 0.95)
    pA_outcome  <- mean(d$net_to_A_outcome > 0)
    pB_outcome  <- mean(d$net_to_A_outcome < 0)

    pA_best_outcome <- mean(d$best_outcome_to_A, na.rm = TRUE)
    pB_best_outcome <- mean(d$best_outcome_to_B, na.rm = TRUE)

    best_prob_ci <- function(x) {
      x <- as.logical(x)
      x <- x[!is.na(x)]
      n <- length(x)
      if (n == 0L) return(c(NA_real_, NA_real_))
      k <- sum(x)
      stats::qbeta(c(0.05, 0.95), k + 1, n - k + 1)
    }

    pA_best_ci <- best_prob_ci(d$best_outcome_to_A)
    pB_best_ci <- best_prob_ci(d$best_outcome_to_B)

    best_team <- if (pA_best_outcome >= pB_best_outcome) input$tm_teamA else input$tm_teamB
    best_prob <- max(pA_best_outcome, pB_best_outcome, na.rm = TRUE)
    best_ci <- if (identical(best_team, input$tm_teamA)) pA_best_ci else pB_best_ci
    best_col <- if (identical(best_team, input$tm_teamA)) "#3b82f6" else "#f59e0b"

    signed_metric_card <- function(mean_to_A, q05_to_A, q95_to_A, label) {
      to_team <- if (mean_to_A >= 0) input$tm_teamA else input$tm_teamB
      shown_mean <- abs(mean_to_A)
      shown_q05 <- if (mean_to_A >= 0) q05_to_A else -q95_to_A
      shown_q95 <- if (mean_to_A >= 0) q95_to_A else -q05_to_A
      tags$div(style = paste0(
        "min-height:112px; padding:15px; border:1px solid #1a1a2a; ",
        "border-radius:10px; border-left:4px solid #10b981; background:rgba(15,15,26,0.72); ",
        "display:flex; flex-direction:column; justify-content:center; box-sizing:border-box;"),
        tags$div(style = "display:flex; align-items:center; gap:10px; font-size:30px; line-height:1.1; font-weight:850; color:#10b981; text-align:left;",
                 team_logo_img_app(to_team, size = 38),
                 tags$span(sprintf("%s %+.1f %s", to_team, shown_mean, label))),
        tags$div(style = "font-size:14px; color:#aaa; margin-top:8px; text-align:left;",
                 sprintf("90%% CI: [%+.1f, %+.1f] %s", shown_q05, shown_q95, label))
      )
    }

    prob_card <- function(label, p, col) {
      tags$div(style = sprintf(
        "flex:1; min-width:0; padding:11px 13px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;", col),
        tags$div(style = "font-size:11px; color:#888; white-space:nowrap;", label),
        tags$div(style = sprintf("font-size:23px; font-weight:850; color:%s;", col), sprintf("%.1f%%", 100 * p))
      )
    }

    best_outcome_card <- function() {
      tags$div(style = sprintf(
        paste0(
          "min-width:0; padding:15px; border:1px solid #1a1a2a; border-radius:10px; ",
          "border-left:4px solid %s; background:rgba(15,15,26,0.72); display:flex; ",
          "flex-direction:column; justify-content:center; height:100%%; min-height:204px; box-sizing:border-box;"
        ), best_col),
        tags$div(style = sprintf("display:flex; align-items:center; gap:10px; font-size:30px; line-height:1.1; font-weight:850; color:%s; text-align:left;", best_col),
                 team_logo_img_app(best_team, size = 38),
                 tags$span(sprintf("%s %.1f%% Best Player", best_team, 100 * best_prob))),
        tags$div(style = "font-size:14px; color:#aaa; margin-top:8px; text-align:left;",
                 sprintf("90%% CI: [%.1f%%, %.1f%%]", 100 * best_ci[1], 100 * best_ci[2])),
        tags$div(style = "font-size:12px; color:#777; margin-top:8px; text-align:left;",
                 "Single highest 4-Yr WS outcome among selected picks")
      )
    }

    metric_block <- function(metric_card, prob_left, prob_right) {
      tags$div(style = "display:flex; flex-direction:column; gap:8px; min-width:0; height:100%; align-self:stretch;",
        metric_card,
        tags$div(style = "display:flex; gap:8px; align-items:stretch; min-width:0; flex:1;",
          prob_left,
          prob_right
        )
      )
    }

    tags$div(style = "font-size:13px; color:#bbb; line-height:1.7;",
      tags$div(
        style = "display:grid; grid-template-columns:repeat(3, minmax(0, 1fr)); gap:12px; align-items:stretch;",
        metric_block(
          signed_metric_card(eA_ev, q05_ev, q95_ev, "EPV"),
          prob_card(sprintf("%s higher EPV", input$tm_teamA), pA_ev, "#3b82f6"),
          prob_card(sprintf("%s higher EPV", input$tm_teamB), pB_ev, "#f59e0b")
        ),
        metric_block(
          signed_metric_card(eA_outcome, q05_outcome, q95_outcome, "4-Yr WS"),
          prob_card(sprintf("%s more 4-Yr WS", input$tm_teamA), pA_outcome, "#3b82f6"),
          prob_card(sprintf("%s more 4-Yr WS", input$tm_teamB), pB_outcome, "#f59e0b")
        ),
        best_outcome_card()
      )
    )
  })

  trade_density_plot <- function(x, line_color, fill_color, x_title, hover_label = "Net") {
    ddf <- density_curve_df(x)
    plot_ly() %>%
      add_lines(data = ddf, x = ~x, y = ~density,
                name = "Density", fill = "tozeroy",
                line = list(color = line_color, width = 2.5),
                fillcolor = fill_color,
                hovertemplate = sprintf("%s: %%{x:.2f}<br>Density: %%{y:.2f}<extra></extra>", hover_label)) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "#0f0f1a",
        font = list(family = "IBM Plex Mono", color = "#999"),
        xaxis = list(title = x_title,
                     gridcolor = "#1a1a2a", zerolinecolor = "#f59e0b"),
        yaxis = list(title = "Density", gridcolor = "#1a1a2a"),
        showlegend = FALSE,
        shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1,
                           yref = "paper",
                           line = list(color = "#f59e0b", dash = "dash"))))
  }

  output$tm_dist_ev <- renderPlotly({
    d <- trade_draws()
    if (is.null(d)) return(plotly_empty())
    trade_density_plot(
      d$net_to_A_ev,
      line_color = "#f59e0b",
      fill_color = "rgba(14,165,233,0.35)",
      x_title = sprintf("Net EPV transferred to %s (>0 favors %s)",
                        input$tm_teamA, input$tm_teamA),
      hover_label = "Net EPV"
    )
  })

  output$tm_dist_outcome <- renderPlotly({
    d <- trade_draws()
    if (is.null(d)) return(plotly_empty())
    trade_density_plot(
      d$net_to_A_outcome,
      line_color = "#7c3aed",
      fill_color = "rgba(14,165,233,0.35)",
      x_title = sprintf("Net realized 4-Yr WS transferred to %s (>0 favors %s)",
                        input$tm_teamA, input$tm_teamA),
      hover_label = "Net 4-Yr WS"
    )
  })

  output$tm_dist_best <- renderPlotly({
    d <- trade_draws()
    if (is.null(d)) return(plotly_empty())
    trade_density_plot(
      d$best_outcome_edge_to_A,
      line_color = "#10b981",
      fill_color = "rgba(16,185,129,0.28)",
      x_title = sprintf("Best-player 4-Yr WS edge to %s (>0 favors %s)",
                        input$tm_teamA, input$tm_teamA),
      hover_label = "Best-player edge"
    )
  })


}

shinyApp(ui = ui, server = server)
