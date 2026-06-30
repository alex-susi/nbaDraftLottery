################################################################################
# NBA 3-2-1 Lottery Reform — Shiny Dashboard
# Reads dashboard_data.rds produced by nba_lottery.R
#
# Tabs:
#   About           — audience-facing landing page / glossary
#   Pick Landscape  — pick scatterplot or EPV leaderboard
#   Team Summaries  — all 30 teams with portfolio details
#   Methodology     — transition heatmaps, rank trajectory explorer, pick-value curve,
#                     and model-validation diagnostics
#   Single Pick     — one-asset value distribution
#   Pick Movers     — pick-level EPV changes
#   Trade Machine   — hypothetical pick trade assessment
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

transition_matrix_export_app <- dd$transition_matrix %||% NULL

# New rank-based team-strength exports retain both the 30-rank transition
# surface and its five-tier aggregation. Accept the current export names plus
# a few backward-compatible aliases so the Methodology tab works across saved
# dashboard_data.rds versions.
rank_trans_mat <- dd$rank_transition_matrix %||%
  dd$rank_transition_matrix_mean %||%
  dd$post_rank_trans %||%
  dd$transition_matrix_30 %||%
  NULL

if (is.null(rank_trans_mat) && !is.null(transition_matrix_export_app) &&
    is.matrix(transition_matrix_export_app) &&
    identical(dim(transition_matrix_export_app), c(30L, 30L))) {
  rank_trans_mat <- transition_matrix_export_app
}

trans_mat <- dd$tier_transition_matrix %||%
  dd$transition_matrix_tier %||%
  dd$post_tier_trans %||%
  NULL

if (is.null(trans_mat) && !is.null(transition_matrix_export_app) &&
    is.matrix(transition_matrix_export_app) &&
    identical(dim(transition_matrix_export_app), c(5L, 5L))) {
  trans_mat <- transition_matrix_export_app
}

# If a new export contains only the 30-rank matrix, derive the legacy five-tier
# view used by the existing heatmap/state diagram. Rows are averaged within the
# origin tier and destination-rank probabilities are summed within each tier.
if (is.null(trans_mat) && !is.null(rank_trans_mat) &&
    is.matrix(rank_trans_mat) && identical(dim(rank_trans_mat), c(30L, 30L))) {
  rank_tier_groups_app <- list(
    relegation = 1:3,
    nonplayin = 4:10,
    playin_seed = 11:14,
    playin_loser = 15:16,
    playoff = 17:30
  )
  trans_mat <- matrix(
    0,
    nrow = length(rank_tier_groups_app),
    ncol = length(rank_tier_groups_app),
    dimnames = list(names(rank_tier_groups_app), names(rank_tier_groups_app))
  )
  for (i in seq_along(rank_tier_groups_app)) {
    for (j in seq_along(rank_tier_groups_app)) {
      trans_mat[i, j] <- mean(rowSums(
        rank_trans_mat[
          rank_tier_groups_app[[i]],
          rank_tier_groups_app[[j]],
          drop = FALSE
        ],
        na.rm = TRUE
      ))
    }
  }
}

if (is.null(trans_mat)) {
  stop("dashboard_data.rds does not contain a tier or 30-rank transition matrix.", call. = FALSE)
}

if (!is.null(rank_trans_mat)) {
  rank_trans_mat <- as.matrix(rank_trans_mat)
  if (identical(dim(rank_trans_mat), c(30L, 30L))) {
    dimnames(rank_trans_mat) <- list(
      sprintf("Rank %02d", 1:30),
      sprintf("Rank %02d", 1:30)
    )
  } else {
    warning("Ignoring rank transition export because it is not a 30x30 matrix.")
    rank_trans_mat <- NULL
  }
}
trans_counts <- dd$transition_counts
stationary   <- dd$stationary
tier_balls   <- dd$tier_balls
meta         <- dd$metadata
stan_diag    <- dd$stan_diagnostics

validation_decision_path <- c(
  "03_validation/validation_decision_table_latest_models.csv",
  "validation_decision_table_latest_models.csv"
)
validation_decision_path <- validation_decision_path[file.exists(validation_decision_path)][1]
validation_decision_tbl <- if (!is.na(validation_decision_path)) {
  tryCatch(readr::read_csv(validation_decision_path, show_col_types = FALSE),
           error = function(e) tibble())
} else {
  tibble()
}

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

# A display asset is a single user-facing economic pick entitlement. Older or
# intermediate dashboard exports can contain the same registry row more than
# once. Remove repeated IDs before any ranked-pool splitting or summary work;
# otherwise one duplicated source row becomes two duplicated ranked legs.
pick_display_assets <- pick_display_assets %>%
  distinct(.data$display_asset_id, .keep_all = TRUE)
pick_display_members <- pick_display_members %>%
  distinct(.data$display_asset_id, .data$asset_id)

filter_display_cols_early_app <- function(mat, ids) {
  if (is.null(mat) || length(mat) == 0 || is.null(dim(mat)) || ncol(mat) == 0L) return(mat)
  ids <- ids[ids %in% colnames(mat)]
  mat[, ids, drop = FALSE]
}

# Suppress standalone retained-own rows that are already represented as members
# of a grouped display entitlement. This prevents double-counting display picks
# such as HOU 2027 own inside the HOU/BKN swap, or BKN 2028 own inside BKN's
# two-most-favorable pool.
drop_duplicate_internal_own_display_assets_app <- function() {
  if (is.null(pick_display_assets) || is.null(pick_display_members) ||
      nrow(pick_display_assets) == 0L || nrow(pick_display_members) == 0L) {
    return(invisible(FALSE))
  }
  if (!all(c("asset_id", "owner", "original_team", "pick_type") %in% names(pick_assets))) {
    return(invisible(FALSE))
  }

  duplicate_asset_ids <- pick_display_members %>%
    count(asset_id, name = "display_n") %>%
    filter(.data$display_n > 1L) %>%
    pull(asset_id)

  if (length(duplicate_asset_ids) == 0L) return(invisible(FALSE))

  stale_display_ids <- pick_display_members %>%
    filter(.data$asset_id %in% duplicate_asset_ids) %>%
    left_join(pick_display_assets %>% select(display_asset_id, group_type), by = "display_asset_id") %>%
    left_join(pick_assets %>% select(asset_id, asset_owner = owner, original_team, pick_type), by = "asset_id") %>%
    filter(
      .data$group_type == "single_asset",
      .data$pick_type == "own",
      .data$asset_owner == .data$original_team
    ) %>%
    pull(display_asset_id) %>%
    unique()

  if (length(stale_display_ids) == 0L) return(invisible(FALSE))

  keep_ids <- setdiff(pick_display_assets$display_asset_id, stale_display_ids)
  pick_display_assets <<- pick_display_assets %>% filter(.data$display_asset_id %in% keep_ids)
  pick_display_members <<- pick_display_members %>% filter(.data$display_asset_id %in% keep_ids)

  display_asset_cur_draws <<- filter_display_cols_early_app(display_asset_cur_draws, keep_ids)
  display_asset_new_draws <<- filter_display_cols_early_app(display_asset_new_draws, keep_ids)
  display_asset_cur_ev_draws <<- filter_display_cols_early_app(display_asset_cur_ev_draws, keep_ids)
  display_asset_new_ev_draws <<- filter_display_cols_early_app(display_asset_new_ev_draws, keep_ids)
  display_convey_cur_draws <<- filter_display_cols_early_app(display_convey_cur_draws, keep_ids)
  display_convey_new_draws <<- filter_display_cols_early_app(display_convey_new_draws, keep_ids)

  message(sprintf("Removed %d duplicate retained-own display rows already covered by grouped entitlements.", length(stale_display_ids)))
  invisible(TRUE)
}


# Ensure future retained-own picks are represented in the user-facing display layer.
# Some older dashboard exports omit future own-pick display rows unless the pick
# is involved in an obligation. That made Team Summaries details undercount own picks
# for teams like BKN. Add any missing owner==original_team asset rows as simple
# display entitlements, then let the conveyance matrices determine whether they
# actually count/value in each draw.
ensure_retained_own_display_assets_app <- function() {
  if (is.null(pick_assets) || nrow(pick_assets) == 0L || !"asset_id" %in% names(pick_assets)) return(invisible(FALSE))

  represented_asset_ids <- unique(pick_display_members$asset_id)

  own_rows <- pick_assets %>%
    filter(
      !is.na(.data$owner),
      !is.na(.data$original_team),
      .data$owner == .data$original_team,
      !.data$asset_id %in% represented_asset_ids
    ) %>%
    mutate(
      display_asset_id = paste0("display_", .data$asset_id),
      member_original_teams = .data$original_team,
      group_type = "single_asset",
      display_group = .data$asset_id,
      member_n = 1L
    )

  if (nrow(own_rows) == 0L) return(invisible(FALSE))

  existing_key <- pick_display_assets %>%
    transmute(key = paste(.data$owner, .data$year, .data$round, .data$member_original_teams, sep = "|")) %>%
    pull(key)

  add_rows <- own_rows %>%
    mutate(key = paste(.data$owner, .data$year, .data$round, .data$member_original_teams, sep = "|")) %>%
    filter(!.data$key %in% existing_key)

  if (nrow(add_rows) == 0L) return(invisible(FALSE))

  for (nm in setdiff(names(pick_display_assets), names(add_rows))) add_rows[[nm]] <- NA
  add_display <- add_rows %>% select(any_of(names(pick_display_assets)))
  add_members <- add_rows %>% transmute(display_asset_id, asset_id)

  append_cols_from_asset_matrix <- function(mat, asset_mat, add_tbl, default = 0) {
    if (is.null(mat) || length(mat) == 0) return(mat)
    new_cols <- matrix(default, nrow = nrow(mat), ncol = nrow(add_tbl),
                       dimnames = list(NULL, add_tbl$display_asset_id))
    if (!is.null(asset_mat) && length(asset_mat) > 0) {
      for (i in seq_len(nrow(add_tbl))) {
        aid <- add_tbl$asset_id[i]
        did <- add_tbl$display_asset_id[i]
        if (!is.na(aid) && aid %in% colnames(asset_mat)) new_cols[, did] <- asset_mat[, aid]
      }
    }
    cbind(mat, new_cols)
  }

  pick_display_assets <<- bind_rows(pick_display_assets, add_display)
  pick_display_members <<- bind_rows(pick_display_members, add_members)

  display_asset_cur_draws <<- append_cols_from_asset_matrix(display_asset_cur_draws, asset_cur_draws, add_members)
  display_asset_new_draws <<- append_cols_from_asset_matrix(display_asset_new_draws, asset_new_draws, add_members)
  display_asset_cur_ev_draws <<- append_cols_from_asset_matrix(display_asset_cur_ev_draws, asset_cur_ev_draws, add_members)
  display_asset_new_ev_draws <<- append_cols_from_asset_matrix(display_asset_new_ev_draws, asset_new_ev_draws, add_members)
  display_convey_cur_draws <<- append_cols_from_asset_matrix(display_convey_cur_draws, asset_convey_cur_draws, add_members)
  display_convey_new_draws <<- append_cols_from_asset_matrix(display_convey_new_draws, asset_convey_new_draws, add_members)
  invisible(TRUE)
}

ensure_retained_own_display_assets_app()
drop_duplicate_internal_own_display_assets_app()

# Split display entitlements that RealGM treats as multiple separately tradeable
# picks (e.g., "two most favorable" or "most and least favorable") into separate
# user-facing assets. The underlying allocator already values the total correctly;
# this split exposes the first and second legs independently for dropdowns/tables.
split_ranked_display_assets_app <- function() {
  if (is.null(pick_display_assets) || nrow(pick_display_assets) == 0L) return(invisible(FALSE))

  txt <- stringr::str_to_lower(paste(
    pick_display_assets$label %||% "",
    pick_display_assets$obligation %||% "",
    pick_display_assets$notes %||% "",
    pick_display_assets$group_type %||% ""
  ))

  target <- (
    (pick_display_assets$year == 2027L & pick_display_assets$owner == "NOP" & pick_display_assets$round == 1L & stringr::str_detect(txt, "mil|nop") & stringr::str_detect(txt, "both.*1-4|both.*top")) |
    (pick_display_assets$year == 2027L & pick_display_assets$owner == "OKC" & pick_display_assets$round == 1L & stringr::str_detect(txt, "two.*favorable|two.*most|two most")) |
    (pick_display_assets$year == 2028L & pick_display_assets$owner == "BKN" & pick_display_assets$round == 1L & stringr::str_detect(txt, "two.*favorable|two.*most|most / two")) |
    (pick_display_assets$year == 2029L & pick_display_assets$owner == "HOU" & pick_display_assets$round == 1L & stringr::str_detect(txt, "two.*favorable|two.*most|two most")) |
    (pick_display_assets$year == 2029L & pick_display_assets$owner == "POR" & pick_display_assets$round == 1L & stringr::str_detect(txt, "most.*least|least.*most")) |
    (pick_display_assets$year == 2029L & pick_display_assets$owner == "UTA" & pick_display_assets$round == 1L & stringr::str_detect(txt, "two.*favorable|two.*most|most / two")) |
    (pick_display_assets$year == 2029L & pick_display_assets$owner == "DET" & pick_display_assets$round == 2L & stringr::str_detect(txt, "two.*favorable|two.*most|two most"))
  )

  target[is.na(target)] <- FALSE
  split_rows <- pick_display_assets[target, , drop = FALSE] %>%
    distinct(.data$display_asset_id, .keep_all = TRUE)
  if (nrow(split_rows) == 0L) return(invisible(FALSE))

  # Helper creates first-leg and residual-leg columns from the underlying member
  # asset matrix. For "two most" this is most and second-most; for "most and
  # least" it is most and residual least. If a conditional second leg does not
  # exist in a draw, residual is zero.
  split_member_matrix <- function(mat, did) {
    ids <- pick_display_members %>% filter(.data$display_asset_id == .env$did) %>% pull(asset_id)
    ids <- ids[ids %in% colnames(mat)]
    z <- matrix(0, nrow = nrow(mat), ncol = 2)
    if (length(ids) == 0L) return(z)
    sub <- as.matrix(mat[, ids, drop = FALSE])
    sub[!is.finite(sub)] <- 0
    first <- apply(sub, 1, max, na.rm = TRUE)
    total <- rowSums(sub, na.rm = TRUE)
    second <- pmax(total - first, 0)
    cbind(first, second)
  }

  replace_split_cols <- function(mat, did, new_ids, member_mat_source = NULL) {
    if (is.null(mat) || length(mat) == 0) return(mat)
    split_vals <- split_member_matrix(member_mat_source %||% mat, did)
    keep <- setdiff(colnames(mat), did)
    out <- cbind(mat[, keep, drop = FALSE], split_vals)
    colnames(out)[(ncol(out) - 1L):ncol(out)] <- new_ids
    out
  }

  new_asset_rows <- list()
  new_member_rows <- list()
  for (i in seq_len(nrow(split_rows))) {
    r <- split_rows[i, ]
    did <- r$display_asset_id
    new_ids <- paste0(did, c("__rank1", "__rank2"))
    txt_i <- stringr::str_to_lower(paste(r$label %||% "", r$notes %||% "", r$obligation %||% ""))
    rank1 <- "most favorable"
    rank2 <- if (stringr::str_detect(txt_i, "most.*least|least.*most")) "least favorable" else "second most favorable"
    if (r$owner == "NOP" && r$year == 2027L) {
      rank1 <- "more favorable"
      rank2 <- "other if both 1-4"
    }
    row1 <- r; row2 <- r
    row1$display_asset_id <- new_ids[1]
    row2$display_asset_id <- new_ids[2]
    row1$display_group <- paste0(r$display_group %||% did, "__rank1")
    row2$display_group <- paste0(r$display_group %||% did, "__rank2")
    row1$group_type <- "ranked_split"
    row2$group_type <- "ranked_split"
    row1$member_n <- 1L
    row2$member_n <- 1L
    row1$label <- stringr::str_squish(paste(rank1, "of", r$member_original_teams))
    row2$label <- stringr::str_squish(paste(rank2, "of", r$member_original_teams))
    row1$notes <- stringr::str_squish(paste(r$notes %||% "", "[display split: first entitlement]"))
    row2$notes <- stringr::str_squish(paste(r$notes %||% "", "[display split: second entitlement]"))
    new_asset_rows[[length(new_asset_rows) + 1L]] <- row1
    new_asset_rows[[length(new_asset_rows) + 1L]] <- row2

    mem <- pick_display_members %>% filter(.data$display_asset_id == .env$did)
    if (nrow(mem) > 0L) {
      mem1 <- mem; mem2 <- mem
      mem1$display_asset_id <- new_ids[1]
      mem2$display_asset_id <- new_ids[2]
      new_member_rows[[length(new_member_rows) + 1L]] <- mem1
      new_member_rows[[length(new_member_rows) + 1L]] <- mem2
    }

    display_asset_cur_draws <<- replace_split_cols(display_asset_cur_draws, did, new_ids, asset_cur_draws)
    display_asset_new_draws <<- replace_split_cols(display_asset_new_draws, did, new_ids, asset_new_draws)
    display_asset_cur_ev_draws <<- replace_split_cols(display_asset_cur_ev_draws, did, new_ids, asset_cur_ev_draws)
    display_asset_new_ev_draws <<- replace_split_cols(display_asset_new_ev_draws, did, new_ids, asset_new_ev_draws)
    display_convey_cur_draws <<- replace_split_cols(display_convey_cur_draws, did, new_ids, asset_convey_cur_draws)
    display_convey_new_draws <<- replace_split_cols(display_convey_new_draws, did, new_ids, asset_convey_new_draws)
  }

  pick_display_assets <<- bind_rows(
    pick_display_assets %>% filter(!.data$display_asset_id %in% split_rows$display_asset_id),
    bind_rows(new_asset_rows)
  )
  pick_display_members <<- bind_rows(
    pick_display_members %>% filter(!.data$display_asset_id %in% split_rows$display_asset_id),
    bind_rows(new_member_rows)
  )
  invisible(TRUE)
}

split_ranked_display_assets_app()
drop_duplicate_internal_own_display_assets_app()

# Recompute display-level summaries after any app-side display split so team
# totals, selectors, and mover tables all use the same user-facing asset layer.
pick_display_value_summary <- summarise_pick_draws_app(pick_display_assets, display_asset_cur_draws, display_asset_new_draws, "display_asset_id") %>%
  mutate(
    cur_convey_prob = colMeans(display_convey_cur_draws > 0)[display_asset_id],
    new_convey_prob = colMeans(display_convey_new_draws > 0)[display_asset_id],
    cur_expected_pick_count = colMeans(display_convey_cur_draws)[display_asset_id],
    new_expected_pick_count = colMeans(display_convey_new_draws)[display_asset_id]
  )

pick_display_value_ev_summary <- summarise_pick_draws_app(pick_display_assets, display_asset_cur_ev_draws, display_asset_new_ev_draws, "display_asset_id") %>%
  mutate(
    cur_convey_prob = colMeans(display_convey_cur_draws > 0)[display_asset_id],
    new_convey_prob = colMeans(display_convey_new_draws > 0)[display_asset_id],
    cur_expected_pick_count = colMeans(display_convey_cur_draws)[display_asset_id],
    new_expected_pick_count = colMeans(display_convey_new_draws)[display_asset_id]
  )

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

# Internal conveyance matrices describe which original-team leg is active in a
# simulation. User-facing pick counts describe whether the displayed entitlement
# exists. For ordinary protected picks those are the same; for swap-return
# entitlements they are not. Example: BKN's 2027 R1 is guaranteed to be one pick
# even though its value can come from either BKN or HOU after HOU's swap right.
is_guaranteed_display_entitlement_app <- function(display_assets) {
  if (is.null(display_assets) || nrow(display_assets) == 0L) return(logical(0))

  get_chr <- function(nm) {
    if (nm %in% names(display_assets)) {
      x <- as.character(display_assets[[nm]])
      x[is.na(x)] <- ""
      x
    } else {
      rep("", nrow(display_assets))
    }
  }

  txt <- stringr::str_to_lower(stringr::str_squish(paste(
    get_chr("short_label"),
    get_chr("label"),
    get_chr("obligation"),
    get_chr("notes"),
    get_chr("group_type")
  )))

  has_swap <- stringr::str_detect(txt, "\\bswap\\b")
  own_or_swap <- stringr::str_detect(txt, "own\\s+or") & has_swap
  less_fav_swap <- stringr::str_detect(txt, "less favou?rable") & has_swap
  more_fav_swap <- stringr::str_detect(txt, "more favou?rable|most favou?rable") &
    has_swap & stringr::str_detect(txt, "may swap|swap right|swap for|swap with|own\\s+or")

  own_or_swap | less_fav_swap | more_fav_swap
}

pre_count_cur_app <- expected_display_count_app(display_convey_cur_draws, pick_display_assets$display_asset_id)
pre_count_new_app <- expected_display_count_app(display_convey_new_draws, pick_display_assets$display_asset_id)
pre_guaranteed_ids_app <- pick_display_assets$display_asset_id[is_guaranteed_display_entitlement_app(pick_display_assets)]
pre_hit_app <- intersect(pre_guaranteed_ids_app, names(pre_count_cur_app))
pre_count_cur_app[pre_hit_app] <- pmax(pre_count_cur_app[pre_hit_app], 1)
pre_hit_app <- intersect(pre_guaranteed_ids_app, names(pre_count_new_app))
pre_count_new_app[pre_hit_app] <- pmax(pre_count_new_app[pre_hit_app], 1)

display_convey_audit_app <- tibble(
  display_asset_id = pick_display_assets$display_asset_id,
  cur_expected_pick_count = pre_count_cur_app[pick_display_assets$display_asset_id],
  new_expected_pick_count = pre_count_new_app[pick_display_assets$display_asset_id]
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

allowed_first_round_protection_label_app <- function(year, original_team, recipient, protection = NA_character_) {
  yr <- suppressWarnings(as.integer(year))
  og <- as.character(original_team %||% "")
  rec <- as.character(recipient %||% "")
  prot <- as.character(protection %||% "")

  # Only these future first-round protections should create user-facing
  # "own if / via if" labels. This prevents stale protection language from
  # leaking onto unrelated own-pick years in older display registries.
  isTRUE(
    (yr == 2027L && og == "LAL" && rec == "MEM" && prot == "top4") ||
    (yr == 2027L && og == "DAL" && rec == "CHA" && prot == "top2") ||
    (yr == 2027L && og == "MIA" && rec == "CHA" && prot == "lottery") ||
    (yr %in% 2028:2030 && og == "DEN" && rec == "OKC" && prot == "top5") ||
    (yr == 2030L && og == "GSW" && rec == "DAL" && prot == "top20")
  )
}

protected_pick_label_app <- function(year, round, owner, original_team, slot_txt = "") {
  slot_txt <- as.character(slot_txt %||% "")
  slot_txt <- slot_txt[1]
  if (is.na(slot_txt) || !nzchar(slot_txt)) slot_txt <- ""
  if (is.na(year) || is.na(round) || is.na(owner) || is.na(original_team)) return(NA_character_)
  row <- protected_outgoing_lookup_app %>%
    filter(.data$year == as.integer(year),
           .data$round == as.integer(round),
           .data$original_team == .env$original_team)
  if (nrow(row) == 0L) return(NA_character_)
  row <- row[1, ]
  draft_round_int <- suppressWarnings(as.integer(round))
  if (!is.na(draft_round_int) && draft_round_int == 1L &&
      !allowed_first_round_protection_label_app(year, original_team, row$recipient, row$protection)) {
    return(NA_character_)
  }
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
  short <- as.character(r$short_label %||% NA_character_)
  obligation <- as.character(r$obligation %||% NA_character_)

  if (length(origs) == 1L) {
    slot_txt <- r$fixed_slot_display %||% ""
    slot_txt <- as.character(slot_txt[1])
    if (is.na(slot_txt) || !nzchar(slot_txt)) slot_txt <- ""
    special <- protected_pick_label_app(r$year, r$round, r$owner, origs[1], slot_txt)
    if (!is.na(special) && nzchar(special)) return(special)

    short_simple_own <- !is.na(short) &&
      stringr::str_detect(short, regex("^R[12](?:\\s+#[0-9]+(?:/#[0-9]+)*)?\\s+own$", ignore_case = TRUE))

    # Future unprotected picks still owned by the original team should read as a
    # plain own pick. Older dashboard exports can carry generic retained-own
    # notes such as "retained if outgoing protection/swap does not convey" on
    # otherwise clean own-pick rows; those notes are internal allocator context,
    # not a user-facing obligation.
    if (identical(as.character(r$owner), origs[1]) && short_simple_own) {
      return("Own pick")
    }

    stale_protection_text <- !is.na(obligation) &&
      stringr::str_detect(obligation, regex("\\bif\\b|otherwise to|protected", ignore_case = TRUE)) &&
      !is.na(short) &&
      !stringr::str_detect(short, regex("\\bif\\b|otherwise to|protected", ignore_case = TRUE))
    if (stale_protection_text) return(short)
  }

  obligation
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
  label_clean <- str_squish(stringr::str_remove(label_clean, paste0("\\s+to\\s+", owner, "$")))
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


clean_pick_display_language_app <- function(short_label) {
  x <- stringr::str_squish(as.character(short_label %||% ""))
  if (!nzchar(x)) return(x)

  # Remove redundant recipient language inside parenthetical rank/priority labels.
  # Examples:
  #   R2 DAL or BKN 2nd (less favorable to DET)      -> R2 DAL or BKN (less favorable)
  #   R1 MIL or NOP (less favorable to ATL if 5-30) -> R1 MIL or NOP (less favorable if 5-30)
  #   R1 ATL, CLE or UTA (more favorable to ATL)    -> R1 ATL, CLE or UTA (most favorable)
  x <- stringr::str_replace_all(x, "\\((less favorable)\\s+to\\s+[A-Z]{2,3}\\s+if\\s+([^\\)]+)\\)", "(\\1 if \\2)")
  x <- stringr::str_replace_all(x, "\\((least favorable)\\s+to\\s+[A-Z]{2,3}\\s+if\\s+([^\\)]+)\\)", "(\\1 if \\2)")
  x <- stringr::str_replace_all(x, "\\((more favorable)\\s+to\\s+[A-Z]{2,3}\\s+if\\s+([^\\)]+)\\)", "(\\1 if \\2)")
  x <- stringr::str_replace_all(x, "\\((most favorable)\\s+to\\s+[A-Z]{2,3}\\s+if\\s+([^\\)]+)\\)", "(\\1 if \\2)")
  x <- stringr::str_replace_all(x, "\\((best eligible)\\s+to\\s+[A-Z]{2,3}\\s+if\\s+([^\\)]+)\\)", "(\\1 if \\2)")
  x <- stringr::str_replace_all(x, "\\((second most favorable)\\s+to\\s+[A-Z]{2,3}\\)", "(\\1)")
  x <- stringr::str_replace_all(x, "\\((least favorable)\\s+to\\s+[A-Z]{2,3}\\)", "(\\1)")
  x <- stringr::str_replace_all(x, "\\((less favorable)\\s+to\\s+[A-Z]{2,3}\\)", "(\\1)")
  x <- stringr::str_replace_all(x, "\\((most favorable)\\s+to\\s+[A-Z]{2,3}\\)", "(\\1)")
  x <- stringr::str_replace_all(x, "\\((best eligible)\\s+to\\s+[A-Z]{2,3}\\)", "(\\1)")
  x <- stringr::str_replace_all(x, "\\((more favorable)\\s+to\\s+[A-Z]{2,3}\\)", "(most favorable)")

  # For multi-team second-round pools, the leading R2 already communicates that
  # these are second-round entitlements. Remove duplicated "2nd" before the
  # priority parenthetical.
  x <- stringr::str_replace_all(x, "\\s+2nd\\s+\\(", " (")
  stringr::str_squish(x)
}


known_pick_label_override_app <- function(year, round, owner, member_original_teams, short_label,
                                          group_type = NA_character_, label = NA_character_, notes = NA_character_,
                                          fixed_slot_display = NA_character_) {
  yr <- suppressWarnings(as.integer(year))
  rnd <- suppressWarnings(as.integer(round))
  ow <- as.character(owner %||% "")
  origs <- split_abbrs(member_original_teams)
  txt <- stringr::str_to_lower(paste(group_type %||% "", label %||% "", notes %||% "", short_label %||% ""))

  if (rnd %in% c(1L, 2L) && yr == 2026L && length(origs) == 1L) {
    return(fallback_simple_pick_label_app(round, owner, member_original_teams, fixed_slot_display))
  }

  # Known one-year protected firsts that should not leak protection language to
  # later own-pick years in older/stale display registries.
  if (rnd == 1L && ow == "LAL" && length(origs) == 1L && identical(origs[1], "LAL") && yr %in% c(2026L, 2028L, 2030L, 2031L, 2032L)) return(fallback_simple_pick_label_app(round, owner, member_original_teams, fixed_slot_display))
  if (rnd == 1L && ow == "DAL" && length(origs) == 1L && identical(origs[1], "DAL") && yr %in% c(2026L, 2028L, 2029L, 2030L, 2031L, 2032L)) return(fallback_simple_pick_label_app(round, owner, member_original_teams, fixed_slot_display))
  if (rnd == 1L && ow == "MIA" && length(origs) == 1L && identical(origs[1], "MIA") && yr %in% c(2026L, 2029L, 2030L, 2031L, 2032L)) return(fallback_simple_pick_label_app(round, owner, member_original_teams, fixed_slot_display))
  if (rnd == 1L && ow == "GSW" && length(origs) == 1L && identical(origs[1], "GSW") && yr %in% c(2026L, 2027L, 2028L, 2031L, 2032L)) return(fallback_simple_pick_label_app(round, owner, member_original_teams, fixed_slot_display))

  # Authoritative labels for common protected/single-year obligations.
  if (rnd == 1L && yr == 2027L && length(origs) == 1L && identical(origs[1], "LAL") && ow == "LAL") return("R1 own if 1-4, otherwise to MEM")
  if (rnd == 1L && yr == 2027L && length(origs) == 1L && identical(origs[1], "LAL") && ow == "MEM") return("R1 via LAL if 5-30")
  if (rnd == 1L && yr == 2029L && length(origs) == 1L && identical(origs[1], "LAL") && ow == "DAL") return("R1 via LAL")

  if (rnd == 1L && yr == 2027L && length(origs) == 1L && identical(origs[1], "DAL") && ow == "DAL") return("R1 own if 1-2, otherwise to CHA")
  if (rnd == 1L && yr == 2027L && length(origs) == 1L && identical(origs[1], "DAL") && ow == "CHA") return("R1 via DAL if 3-30")
  if (rnd == 1L && yr == 2027L && length(origs) == 1L && identical(origs[1], "MIA") && ow == "MIA") return("R1 own if 1-14, otherwise to CHA")
  if (rnd == 1L && yr == 2027L && length(origs) == 1L && identical(origs[1], "MIA") && ow == "CHA") return("R1 via MIA if 15-30")
  if (rnd == 1L && yr == 2028L && length(origs) == 1L && identical(origs[1], "MIA") && ow == "CHA") return("R1 via MIA if 2027 does not convey")
  if (rnd == 1L && yr == 2027L && length(origs) == 1L && identical(origs[1], "SAS") && ow == "SAC") return("R1 via SAS if 1-16")
  if (rnd == 1L && yr == 2027L && length(origs) == 1L && identical(origs[1], "SAS") && ow == "OKC") return("R1 via SAS if 17-30")
  if (rnd == 1L && yr == 2030L && length(origs) == 1L && identical(origs[1], "GSW") && ow == "GSW") return("R1 own if 1-20, otherwise to DAL")
  if (rnd == 1L && yr == 2030L && length(origs) == 1L && identical(origs[1], "GSW") && ow == "DAL") return("R1 via GSW if 21-30")

  # Display-split ranked entitlements: make the two tradeable legs explicit.
  if (stringr::str_detect(txt, "display split: first entitlement")) {
    clean <- stringr::str_squish(gsub("^\\d+\\s+", "", as.character(label %||% short_label)))
    clean <- stringr::str_squish(stringr::str_remove(clean, paste0("\\s+to\\s+", ow, "$")))
    return(stringr::str_squish(paste0("R", rnd, " ", clean)))
  }
  if (stringr::str_detect(txt, "display split: second entitlement")) {
    clean <- stringr::str_squish(gsub("^\\d+\\s+", "", as.character(label %||% short_label)))
    clean <- stringr::str_squish(stringr::str_remove(clean, paste0("\\s+to\\s+", ow, "$")))
    return(stringr::str_squish(paste0("R", rnd, " ", clean)))
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
    short_label = known_pick_label_override_app(
      year, round, owner, member_original_teams, short_label,
      group_type, label, notes, fixed_slot_display
    ),
    short_label_raw = short_label,
    short_label = clean_pick_display_language_app(short_label),
    trade_label = str_squish(sprintf("%d %s", year, short_label)),
    trade_label_raw = str_squish(sprintf("%d %s", year, short_label_raw))
  ) %>%
  ungroup()

label_cleanup_audit_app <- pick_display_assets %>%
  filter(!is.na(.data$short_label_raw), .data$short_label_raw != .data$short_label) %>%
  transmute(
    owner, year, round,
    before = .data$trade_label_raw,
    after = .data$trade_label
  ) %>%
  distinct()

if (nrow(label_cleanup_audit_app) > 0L) {
  message("Cleaned ", nrow(label_cleanup_audit_app), " pick display label(s). See label_cleanup_audit_app for details.")
}

pick_display_assets <- pick_display_assets %>%
  select(-short_label_raw, -trade_label_raw)

# Final semantic guardrail. Different source display IDs can occasionally
# resolve to the same economic pick after ranked pools are split (the prior bug
# produced two identical BKN 2028 "most favorable" rows and two identical
# "second most favorable" rows). Keep one active representation for each
# owner/year/round/label combination and remove the redundant ID everywhere.
deduplicate_logical_display_assets_app <- function() {
  if (is.null(pick_display_assets) || nrow(pick_display_assets) == 0L) {
    return(invisible(character(0)))
  }

  activity <- setNames(rep(0, nrow(pick_display_assets)), pick_display_assets$display_asset_id)
  for (mat in list(display_convey_cur_draws, display_convey_new_draws)) {
    if (!is.null(mat) && length(mat) > 0L && ncol(mat) > 0L) {
      mat_activity <- colMeans(as.matrix(mat) > 0, na.rm = TRUE)
      hit <- intersect(names(activity), names(mat_activity))
      activity[hit] <- pmax(activity[hit], mat_activity[hit])
    }
  }

  keyed <- pick_display_assets %>%
    mutate(
      .logical_label = stringr::str_to_lower(stringr::str_squish(as.character(.data$short_label))),
      .logical_label = if_else(
        is.na(.data$.logical_label) | !nzchar(.data$.logical_label),
        paste0("__id__", .data$display_asset_id),
        .data$.logical_label
      ),
      .logical_key = paste(.data$owner, .data$year, .data$round, .data$.logical_label, sep = "|") ,
      .activity = coalesce(as.numeric(activity[.data$display_asset_id]), 0)
    ) %>%
    arrange(.data$.logical_key, desc(.data$.activity), .data$display_asset_id) %>%
    group_by(.data$.logical_key) %>%
    mutate(.keep_display_id = first(.data$display_asset_id)) %>%
    ungroup()

  stale_ids <- keyed %>%
    filter(.data$display_asset_id != .data$.keep_display_id) %>%
    pull(display_asset_id) %>%
    unique()

  pick_display_assets <<- keyed %>%
    filter(.data$display_asset_id == .data$.keep_display_id) %>%
    select(-starts_with(".logical_"), -.activity, -.keep_display_id)

  if (length(stale_ids) == 0L) return(invisible(stale_ids))

  keep_ids <- pick_display_assets$display_asset_id
  pick_display_members <<- pick_display_members %>%
    filter(.data$display_asset_id %in% .env$keep_ids) %>%
    distinct(.data$display_asset_id, .data$asset_id)

  display_asset_cur_draws <<- filter_display_cols_early_app(display_asset_cur_draws, keep_ids)
  display_asset_new_draws <<- filter_display_cols_early_app(display_asset_new_draws, keep_ids)
  display_asset_cur_ev_draws <<- filter_display_cols_early_app(display_asset_cur_ev_draws, keep_ids)
  display_asset_new_ev_draws <<- filter_display_cols_early_app(display_asset_new_ev_draws, keep_ids)
  display_convey_cur_draws <<- filter_display_cols_early_app(display_convey_cur_draws, keep_ids)
  display_convey_new_draws <<- filter_display_cols_early_app(display_convey_new_draws, keep_ids)

  pick_display_value_summary <<- pick_display_value_summary %>%
    filter(.data$display_asset_id %in% .env$keep_ids) %>%
    distinct(.data$display_asset_id, .keep_all = TRUE)
  pick_display_value_ev_summary <<- pick_display_value_ev_summary %>%
    filter(.data$display_asset_id %in% .env$keep_ids) %>%
    distinct(.data$display_asset_id, .keep_all = TRUE)

  message(sprintf(
    "Removed %d duplicate logical pick display row%s across all app views.",
    length(stale_ids),
    ifelse(length(stale_ids) == 1L, "", "s")
  ))
  invisible(stale_ids)
}

deduplicate_logical_display_assets_app()

if (anyDuplicated(pick_display_assets$display_asset_id)) {
  stop("Duplicate display_asset_id values remain after display reconciliation.", call. = FALSE)
}
logical_label_check_app <- stringr::str_to_lower(stringr::str_squish(pick_display_assets$short_label))
logical_label_check_app <- paste(
  pick_display_assets$owner,
  pick_display_assets$year,
  pick_display_assets$round,
  logical_label_check_app,
  sep = "|"
)
logical_label_check_app <- logical_label_check_app[
  !is.na(pick_display_assets$short_label) & nzchar(stringr::str_squish(pick_display_assets$short_label))
]
if (anyDuplicated(logical_label_check_app)) {
  stop("Duplicate logical pick labels remain after display reconciliation.", call. = FALSE)
}

# Final display-level pick-count matrices. These are intentionally separate from
# display_convey_* matrices: conveyance is leg-level; count is entitlement-level.
build_display_pick_count_matrix_app <- function(convey_mat, display_assets) {
  if (!is.null(convey_mat) && length(convey_mat) > 0L && !is.null(dim(convey_mat))) {
    n_draws <- nrow(convey_mat)
  } else if (!is.null(display_asset_new_ev_draws) && length(display_asset_new_ev_draws) > 0L) {
    n_draws <- nrow(display_asset_new_ev_draws)
  } else {
    n_draws <- 0L
  }

  out <- matrix(
    0,
    nrow = n_draws,
    ncol = nrow(display_assets),
    dimnames = list(NULL, display_assets$display_asset_id)
  )

  if (!is.null(convey_mat) && length(convey_mat) > 0L && !is.null(dim(convey_mat)) && n_draws > 0L) {
    ids <- intersect(display_assets$display_asset_id, colnames(convey_mat))
    if (length(ids) > 0L) {
      out[, ids] <- 1L * (as.matrix(convey_mat[, ids, drop = FALSE]) > 0)
    }
  }

  guaranteed_ids <- display_assets$display_asset_id[is_guaranteed_display_entitlement_app(display_assets)]
  guaranteed_ids <- guaranteed_ids[guaranteed_ids %in% colnames(out)]
  if (length(guaranteed_ids) > 0L && nrow(out) > 0L) {
    out[, guaranteed_ids] <- 1L
  }

  out[!is.finite(out)] <- 0
  out[out > 1] <- 1
  out
}

display_pick_count_cur_draws <- build_display_pick_count_matrix_app(display_convey_cur_draws, pick_display_assets)
display_pick_count_new_draws <- build_display_pick_count_matrix_app(display_convey_new_draws, pick_display_assets)

display_count_prob_app <- function(count_mat, ids) {
  vals <- setNames(rep(0, length(ids)), ids)
  if (!is.null(count_mat) && length(count_mat) > 0L && !is.null(dim(count_mat)) && ncol(count_mat) > 0L) {
    cm <- colMeans(count_mat > 0, na.rm = TRUE)
    hit <- intersect(names(cm), ids)
    vals[hit] <- cm[hit]
  }
  vals
}

# Recompute display summaries after label cleanup, logical de-duplication, and
# count reconciliation so every downstream user-facing view shares one source.
pick_display_value_summary <- summarise_pick_draws_app(pick_display_assets, display_asset_cur_draws, display_asset_new_draws, "display_asset_id") %>%
  mutate(
    cur_convey_prob = display_count_prob_app(display_pick_count_cur_draws, display_asset_id),
    new_convey_prob = display_count_prob_app(display_pick_count_new_draws, display_asset_id),
    cur_expected_pick_count = as.numeric(colMeans(display_pick_count_cur_draws, na.rm = TRUE)[display_asset_id]),
    new_expected_pick_count = as.numeric(colMeans(display_pick_count_new_draws, na.rm = TRUE)[display_asset_id])
  )

pick_display_value_ev_summary <- summarise_pick_draws_app(pick_display_assets, display_asset_cur_ev_draws, display_asset_new_ev_draws, "display_asset_id") %>%
  mutate(
    cur_convey_prob = display_count_prob_app(display_pick_count_cur_draws, display_asset_id),
    new_convey_prob = display_count_prob_app(display_pick_count_new_draws, display_asset_id),
    cur_expected_pick_count = as.numeric(colMeans(display_pick_count_cur_draws, na.rm = TRUE)[display_asset_id]),
    new_expected_pick_count = as.numeric(colMeans(display_pick_count_new_draws, na.rm = TRUE)[display_asset_id])
  )

is_guaranteed_display_id_app <- function(display_id) {
  r <- pick_display_assets %>% filter(.data$display_asset_id == .env$display_id)
  if (nrow(r) == 0L) return(FALSE)
  isTRUE(any(is_guaranteed_display_entitlement_app(r)))
}

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
team_delta_quantity <- team_quantity_delta_summary_app(display_pick_count_cur_draws, display_pick_count_new_draws, pick_display_assets, all_summary_teams)

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

team_draw_summary_for_table_app <- function(value_mode, teams = all_summary_teams, display = FALSE) {
  mats <- value_mats_for_mode_app(value_mode, display = display)
  asset_tbl <- if (isTRUE(display)) pick_display_assets else pick_assets
  id_col <- if (isTRUE(display)) "display_asset_id" else "asset_id"

  purrr::map_dfr(teams, function(tm) {
    ids <- asset_tbl %>% filter(.data$owner == .env$tm) %>% pull(dplyr::all_of(id_col))
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
    count_ids <- ids[ids %in% colnames(display_pick_count_cur_draws) & ids %in% colnames(display_pick_count_new_draws)]

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
      # Swap-return display rows are guaranteed entitlements even when the
      # underlying active leg alternates across original teams.
      cur_count_draw <- rowSums(display_pick_count_cur_draws[, count_ids, drop = FALSE], na.rm = TRUE)
      new_count_draw <- rowSums(display_pick_count_new_draws[, count_ids, drop = FALSE], na.rm = TRUE)
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
    new_avg_quality_draw <- if (display_pick_count > 0L) new_total / display_pick_count else rep(0, length(new_total))

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
      new_avg_quality_q10 = as.numeric(quantile(new_avg_quality_draw, 0.10, na.rm = TRUE)),
      new_avg_quality_q90 = as.numeric(quantile(new_avg_quality_draw, 0.90, na.rm = TRUE)),
      quantity_effect = cur_avg_quality * delta_count,
      quality_effect = cur_count * delta_quality,
      interaction_effect = delta_count * delta_quality,
      p_positive = mean((new_total - cur_total) > 0, na.rm = TRUE),
      delta_q05 = as.numeric(quantile(new_total - cur_total, 0.05, na.rm = TRUE)),
      delta_q10 = as.numeric(quantile(new_total - cur_total, 0.10, na.rm = TRUE)),
      delta_q90 = as.numeric(quantile(new_total - cur_total, 0.90, na.rm = TRUE)),
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

  # Retained own-pick display rows should stay in the own-pick bucket even when
  # their notes came from a broader obligation pool. This prevents future own
  # picks like BKN 2028-2032 from being absorbed into swaps/protections solely
  # because an older dashboard export carried complex-group notes on the row.
  if (own_team && stringr::str_detect(short_txt, "^r[12](\\s+#[0-9/]+)?\\s+own\\b") &&
      !stringr::str_detect(short_txt, "own or|swap|favorable|ranked|pool")) {
    return("Own picks")
  }

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



# Internal asset-level breakout for Team Summaries details. The user-facing display
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

  # Count actual expected conveyance at the internal asset level. This fixes the
  # detail breakout for retained own future picks: an internal own-pick row only
  # counts when that owner actually receives that pick in the simulation.
  cur_asset_counts <- if (!is.null(asset_convey_cur_draws) && length(asset_convey_cur_draws) > 0) {
    colMeans(asset_convey_cur_draws, na.rm = TRUE)
  } else {
    numeric(0)
  }
  new_asset_counts <- if (!is.null(asset_convey_new_draws) && length(asset_convey_new_draws) > 0) {
    colMeans(asset_convey_new_draws, na.rm = TRUE)
  } else {
    numeric(0)
  }
  out$cur_expected_pick_count <- as.numeric(cur_asset_counts[out$asset_id])
  out$new_expected_pick_count <- as.numeric(new_asset_counts[out$asset_id])
  out$cur_expected_pick_count[is.na(out$cur_expected_pick_count)] <- if ("cur_convey_prob" %in% names(out)) out$cur_convey_prob[is.na(out$cur_expected_pick_count)] else 0
  out$new_expected_pick_count[is.na(out$new_expected_pick_count)] <- if ("new_convey_prob" %in% names(out)) out$new_convey_prob[is.na(out$new_expected_pick_count)] else 0
  out$cur_expected_pick_count[is.na(out$cur_expected_pick_count)] <- 0
  out$new_expected_pick_count[is.na(out$new_expected_pick_count)] <- 0

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

# Protection options offered in the Trade Machine.
# Round 1 excludes lottery/top-14 because the 3-2-1 rule bars newly traded
# picks from carrying top-12 through top-15 protections.
# Round 2 uses only second-round protected ranges.
protection_choices_round1 <- c(
  "None" = "none",
  "Top-1"  = "top1",  "Top-2"  = "top2",  "Top-3"  = "top3",  "Top-4"  = "top4",
  "Top-5"  = "top5",  "Top-6"  = "top6",  "Top-8"  = "top8",
  "Top-10" = "top10", "Top-16" = "top16", "Top-20" = "top20"
)

protection_choices_round2 <- c(
  "None" = "none",
  "31-45 Prot." = "protected31_45",
  "31-50 Prot." = "protected31_50",
  "31-55 Prot." = "protected31_55"
)

protection_choices_for_round <- function(round) {
  round <- suppressWarnings(as.integer(round %||% 1L))
  if (!is.na(round) && round == 2L) protection_choices_round2 else protection_choices_round1
}

normalize_protection_for_round <- function(protection, round) {
  protection <- as.character(protection %||% "none")
  if (length(protection) == 0L || is.na(protection[1]) || !nzchar(protection[1])) {
    return("none")
  }
  protection <- protection[1]
  allowed <- unname(protection_choices_for_round(round))
  if (protection %in% allowed) protection else "none"
}

protection_floor_app <- function(p) {
  switch(p, top1 = 1, top2 = 2, top3 = 3, top4 = 4, top5 = 5, top6 = 6,
         top8 = 8, top10 = 10, top16 = 16, top20 = 20,
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
  # Use the same NBA CDN SVG endpoint used by the rest of the app. The prior
  # ESPN PNG endpoint can be blocked inside Plotly layout images in some
  # browsers, which leaves the Pick Landscape charts with hover targets but no
  # visible logos.
  team_logo_url_app(team)
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


team_select_choices_app <- function(teams, size = 20) {
  teams <- as.character(teams)
  setNames(teams, team_logo_html_app(teams, size = size, show_abbr = TRUE))
}

selectize_logo_options_app <- list(
  render = I("{
    option: function(item, escape) { return '<div class=\"selectize-logo-option\">' + item.label + '</div>'; },
    item: function(item, escape) { return '<div class=\"selectize-logo-item\">' + item.label + '</div>'; }
  }")
)

delta_color_app <- function(x, digits = 1,
                            pos = "#10b981", neg = "#ef4444",
                            neutral = "#d0d0d0") {
  x <- suppressWarnings(as.numeric(x))
  y <- round(x, digits)
  ifelse(is.na(y) | y == 0, neutral, ifelse(y > 0, pos, neg))
}

fmt_delta_app <- function(x, digits = 1, suffix = "") {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", paste0(sprintf(paste0("%+.", digits, "f"), x), suffix))
}

fmt_num1_app <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", sprintf("%.1f", x))
}

fmt_delta1_app <- function(x, suffix = "") {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", paste0(sprintf("%+.1f", x), suffix))
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

# HTML overlay fallback for Pick Landscape logos. Plotly layout images can be
# browser/CORS-sensitive; these ordinary <img> overlays use the same CDN SVGs
# that already render reliably in selectize inputs, tables, and the Trade Machine.
attach_plot_logo_overlays_app <- function(p, df, x_col, y_col, size = 38, opacity = 0.98) {
  if (is.null(df) || nrow(df) == 0L) return(p)
  logo_data <- df %>%
    transmute(
      team = as.character(.data$team),
      x = as.numeric(.data[[x_col]]),
      y = as.numeric(.data[[y_col]]),
      src = team_logo_url_app(.data$team),
      size = as.numeric(.env$size),
      opacity = as.numeric(.env$opacity)
    ) %>%
    filter(is.finite(.data$x), is.finite(.data$y), !is.na(.data$src), nzchar(.data$src))

  if (nrow(logo_data) == 0L) return(p)

  htmlwidgets::onRender(
    p,
    "function(el, x, data) {
      var points = data || [];
      function getPlot() {
        if (el._fullLayout) return el;
        return el.querySelector('.js-plotly-plot') || el.querySelector('.plotly') || el;
      }
      function drawLogoOverlays() {
        var gd = getPlot();
        if (!gd || !gd._fullLayout || !gd._fullLayout.xaxis || !gd._fullLayout.yaxis) return;
        el.style.position = 'relative';
        var old = el.querySelectorAll('.ct-logo-overlay');
        old.forEach(function(node) { node.remove(); });
        var xa = gd._fullLayout.xaxis;
        var ya = gd._fullLayout.yaxis;
        if (!xa.l2p || !ya.l2p) return;
        points.forEach(function(d) {
          var xp = xa.l2p(+d.x) + xa._offset;
          var yp = ya.l2p(+d.y) + ya._offset;
          if (!isFinite(xp) || !isFinite(yp)) return;
          var sz = +d.size || 38;
          var img = document.createElement('img');
          img.className = 'ct-logo-overlay';
          img.src = d.src;
          img.alt = d.team || '';
          img.title = d.team || '';
          img.style.position = 'absolute';
          img.style.left = (xp - sz / 2) + 'px';
          img.style.top = (yp - sz / 2) + 'px';
          img.style.width = sz + 'px';
          img.style.height = sz + 'px';
          img.style.objectFit = 'contain';
          img.style.pointerEvents = 'none';
          img.style.zIndex = '6';
          img.style.opacity = d.opacity || 0.98;
          img.setAttribute('data-base-opacity', img.style.opacity);
          img.setAttribute('data-team', d.team || '');
          img.style.transition = 'opacity 90ms linear';
          el.appendChild(img);
        });
      }
      function hoveredTeamFromEvent(ev) {
        if (!ev || !ev.points || !ev.points.length) return null;
        for (var i = 0; i < ev.points.length; i++) {
          var cd = ev.points[i].customdata;
          if (Array.isArray(cd) && cd.length) cd = cd[0];
          if (cd !== undefined && cd !== null && String(cd).length) return String(cd);
          if (ev.points[i].data && ev.points[i].data.customdata) {
            var dcd = ev.points[i].data.customdata;
            if (Array.isArray(dcd) && dcd.length) {
              var idx = ev.points[i].pointIndex || ev.points[i].pointNumber || 0;
              var val = dcd[idx] || dcd[0];
              if (Array.isArray(val) && val.length) val = val[0];
              if (val !== undefined && val !== null && String(val).length) return String(val);
            }
          }
        }
        return null;
      }
      function dimLogoOverlays(hoveredTeam) {
        var logos = el.querySelectorAll('.ct-logo-overlay');
        logos.forEach(function(node) {
          var tm = node.getAttribute('data-team') || '';
          if (hoveredTeam && tm === hoveredTeam) {
            node.style.opacity = node.getAttribute('data-base-opacity') || '0.98';
            node.style.zIndex = '8';
          } else {
            node.style.opacity = '0.16';
            node.style.zIndex = '5';
          }
        });
        var hoverLayer = el.querySelector('.hoverlayer');
        if (hoverLayer) {
          hoverLayer.style.position = 'relative';
          hoverLayer.style.zIndex = '1000';
        }
      }
      function restoreLogoOverlays() {
        var logos = el.querySelectorAll('.ct-logo-overlay');
        logos.forEach(function(node) {
          node.style.opacity = node.getAttribute('data-base-opacity') || '0.98';
          node.style.zIndex = '6';
        });
      }
      setTimeout(drawLogoOverlays, 100);
      var gd = getPlot();
      if (gd && gd.on) {
        gd.on('plotly_afterplot', drawLogoOverlays);
        gd.on('plotly_relayout', function() { setTimeout(drawLogoOverlays, 25); });
        gd.on('plotly_doubleclick', function() { setTimeout(drawLogoOverlays, 100); });
        gd.on('plotly_hover', function(ev) { dimLogoOverlays(hoveredTeamFromEvent(ev)); });
        gd.on('plotly_unhover', restoreLogoOverlays);
      }
    }",
    data = logo_data
  )
}

apply_logo_collision_offsets_app <- function(df, x_col = "x", y_col = "y", min_sep = 0.092, max_iter = 260L) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  x <- suppressWarnings(as.numeric(df[[x_col]]))
  y <- suppressWarnings(as.numeric(df[[y_col]]))
  finite <- is.finite(x) & is.finite(y)
  if (sum(finite) <= 1L) {
    df$x_logo <- x
    df$y_logo <- y
    return(df)
  }

  xr <- range(x[finite], na.rm = TRUE)
  yr <- range(y[finite], na.rm = TRUE)
  if (!all(is.finite(xr)) || diff(xr) == 0) xr <- c(0, 1)
  if (!all(is.finite(yr)) || diff(yr) == 0) yr <- c(0, 1)

  # Work in normalized plot coordinates so horizontal and vertical spacing are
  # comparable. This deterministic force pass separates clustered logos without
  # changing the reported hover values.
  xn <- (x - xr[1]) / diff(xr)
  yn <- (y - yr[1]) / diff(yr)
  xn0 <- xn
  yn0 <- yn
  idx <- which(finite)

  for (iter in seq_len(max_iter)) {
    moved <- FALSE
    for (a in seq_len(length(idx) - 1L)) {
      i <- idx[a]
      for (b in seq.int(a + 1L, length(idx))) {
        j <- idx[b]
        dx <- xn[j] - xn[i]
        dy <- yn[j] - yn[i]
        dist <- sqrt(dx * dx + dy * dy)
        if (!is.finite(dist) || dist < min_sep) {
          if (!is.finite(dist) || dist < 1e-6) {
            # Deterministic fallback angle by team names / row order.
            key <- sum(utf8ToInt(paste0(df$team[i] %||% i, df$team[j] %||% j)))
            angle <- (key %% 360) * pi / 180
            ux <- cos(angle)
            uy <- sin(angle)
            dist <- 0
          } else {
            ux <- dx / dist
            uy <- dy / dist
          }
          push <- (min_sep - dist) * 0.58
          xn[i] <- xn[i] - ux * push
          yn[i] <- yn[i] - uy * push
          xn[j] <- xn[j] + ux * push
          yn[j] <- yn[j] + uy * push
          moved <- TRUE
        }
      }
    }
    # Keep labels inside a modest expanded plotting area and gently pull any
    # isolated over-correction back toward the original coordinates.
    xn[idx] <- pmin(pmax(xn[idx], -0.045), 1.045)
    yn[idx] <- pmin(pmax(yn[idx], -0.055), 1.055)
    if (!moved) break
  }

  # Tiny deterministic nudge for teams that are still nearly stacked after
  # boundary clamping.
  for (a in seq_len(length(idx) - 1L)) {
    i <- idx[a]
    for (b in seq.int(a + 1L, length(idx))) {
      j <- idx[b]
      dx <- xn[j] - xn[i]
      dy <- yn[j] - yn[i]
      dist <- sqrt(dx * dx + dy * dy)
      if (is.finite(dist) && dist < min_sep * 0.88) {
        sign_i <- ifelse((a %% 2L) == 0L, -1, 1)
        xn[i] <- xn[i] - sign_i * min_sep * 0.18
        xn[j] <- xn[j] + sign_i * min_sep * 0.18
      }
    }
  }

  df$x_logo <- xr[1] + xn * diff(xr)
  df$y_logo <- yr[1] + yn * diff(yr)
  df
}

# For the EPV Leaderboard, rows are fixed but nearby logos can still collide
# when adjacent teams have very similar 3-2-1 EPV. Offset only the rendered logo
# and hover target by a small deterministic amount, leaving the row ordering and
# underlying EPV values unchanged.
apply_leaderboard_logo_offsets_app <- function(df, x_col = "new_mean", y_col = "y") {
  if (is.null(df) || nrow(df) == 0L) return(df)
  x <- suppressWarnings(as.numeric(df[[x_col]]))
  y <- suppressWarnings(as.numeric(df[[y_col]]))
  finite <- is.finite(x) & is.finite(y)
  if (sum(finite) <= 1L) {
    df$x_logo <- x
    df$y_logo <- y
    return(df)
  }

  xr <- range(c(df$lo, df$hi, df$current_mean, df$new_mean), na.rm = TRUE)
  if (!all(is.finite(xr)) || diff(xr) == 0) xr <- range(x[finite], na.rm = TRUE)
  if (!all(is.finite(xr)) || diff(xr) == 0) xr <- c(0, 1)
  yr <- range(y[finite], na.rm = TRUE)
  if (!all(is.finite(yr)) || diff(yr) == 0) yr <- c(0, 1)

  xn <- (x - xr[1]) / diff(xr)
  yn <- (y - yr[1]) / diff(yr)
  idx <- which(finite)
  min_dx <- 0.028
  min_dy <- 0.044

  for (iter in 1:80) {
    moved <- FALSE
    for (a in seq_len(length(idx) - 1L)) {
      i <- idx[a]
      for (b in seq.int(a + 1L, length(idx))) {
        j <- idx[b]
        if (abs(yn[j] - yn[i]) < min_dy && abs(xn[j] - xn[i]) < min_dx) {
          dir <- ifelse((a + b) %% 2L == 0L, 1, -1)
          push <- (min_dx - abs(xn[j] - xn[i])) / 2 + 0.004
          xn[i] <- xn[i] - dir * push
          xn[j] <- xn[j] + dir * push
          moved <- TRUE
        }
      }
    }
    xn[idx] <- pmin(pmax(xn[idx], -0.025), 1.025)
    if (!moved) break
  }

  df$x_logo <- xr[1] + xn * diff(xr)
  df$y_logo <- y
  df
}


logo_hover_enlarger_app <- function(widget, scale = 1.45) {
  htmlwidgets::onRender(widget, sprintf("
    function(el, x) {
      var gd = document.getElementById(el.id);
      if (!gd || !gd.layout || !gd.layout.images) return;

      function captureBase() {
        if (!gd.layout || !gd.layout.images) return [];
        return gd.layout.images.map(function(img) {
          return {
            name: img.name,
            sizex: img.sizex,
            sizey: img.sizey,
            opacity: img.opacity,
            x: img.x,
            y: img.y
          };
        });
      }

      var baseImages = captureBase();
      function resetImages() {
        if (!baseImages || !baseImages.length) return;
        var update = {};
        baseImages.forEach(function(img, i) {
          update['images[' + i + '].sizex'] = img.sizex;
          update['images[' + i + '].sizey'] = img.sizey;
          update['images[' + i + '].opacity'] = img.opacity;
        });
        Plotly.relayout(gd, update);
      }

      function pointTeam(pt) {
        if (!pt) return null;
        var cd = pt.customdata;
        if (Array.isArray(cd)) return cd[0];
        return cd;
      }

      gd.on('plotly_afterplot', function() {
        if (!baseImages || baseImages.length === 0) baseImages = captureBase();
      });

      gd.on('plotly_hover', function(ev) {
        if (!ev || !ev.points || !ev.points.length) return;
        var pt = ev.points[0];
        var team = pointTeam(pt);
        if (!baseImages || baseImages.length === 0) baseImages = captureBase();
        var hitIndex = -1;
        if (team) {
          hitIndex = baseImages.findIndex(function(img) { return img.name === team; });
        }
        if (hitIndex < 0 && pt && isFinite(pt.x) && isFinite(pt.y)) {
          var bestDist = Infinity;
          baseImages.forEach(function(img, i) {
            var dx = Number(img.x) - Number(pt.x);
            var dy = Number(img.y) - Number(pt.y);
            var dist = dx * dx + dy * dy;
            if (isFinite(dist) && dist < bestDist) { bestDist = dist; hitIndex = i; }
          });
        }
        if (hitIndex < 0) return;
        var update = {};
        baseImages.forEach(function(img, i) {
          var hit = i === hitIndex;
          update['images[' + i + '].sizex'] = img.sizex * (hit ? %s : 1);
          update['images[' + i + '].sizey'] = img.sizey * (hit ? %s : 1);
          update['images[' + i + '].opacity'] = hit ? 1 : img.opacity;
        });
        Plotly.relayout(gd, update);
      });

      gd.on('plotly_unhover', function() { resetImages(); });
    }
  ", scale, scale))
}


# Primary team colors for hover labels / accents. These are intentionally simple
# high-level brand colors; the app uses them for context, not official art.
team_primary_colors_app <- c(
  ATL = "#c8102e", BOS = "#007A33", BKN = "#000000", CHA = "#1D1160",
  CHI = "#CE1141", CLE = "#860038", DAL = "#00538C", DEN = "#0E2240",
  DET = "#C8102E", GSW = "#1D428A", HOU = "#CE1141", IND = "#FDBB30",
  LAC = "#C8102E", LAL = "#552583", MEM = "#5D76A9", MIA = "#98002E",
  MIL = "#00471B", MIN = "#0C2340", NOP = "#0C2340", NYK = "#006BB6",
  OKC = "#007AC1", ORL = "#0077C0", PHI = "#006BB6", PHX = "#1D1160",
  POR = "#E03A3E", SAC = "#5A2D81", SAS = "#C4CED4", TOR = "#CE1141",
  UTA = "#753bbd", WAS = "#002B5C"
)

team_primary_color_app <- function(team) {
  out <- team_primary_colors_app[as.character(team)]
  out[is.na(out)] <- "#6d28d9"
  unname(out)
}

team_secondary_colors_app <- c(
  ATL = "#ffcd00", BOS = "#BA9653", BKN = "#FFFFFF", CHA = "#00788C",
  CHI = "#FFFFFF", CLE = "#FDBB30", DAL = "#B8C4CA", DEN = "#FEC524",
  DET = "#1D42BA", GSW = "#FFC72C", HOU = "#c4ced4", IND = "#002d62",
  LAC = "#1D428A", LAL = "#FDB927", MEM = "#707271", MIA = "#F9A01B",
  MIL = "#EEE1C6", MIN = "#78BE20", NOP = "#C8102E", NYK = "#F58426",
  OKC = "#EF3B24", ORL = "#C4CED4", PHI = "#ED174C", PHX = "#E56020",
  POR = "#d9dddc", SAC = "#63727A", SAS = "#000000", TOR = "#753bbd",
  UTA = "#00a9e0", WAS = "#E31837"
)

team_secondary_color_app <- function(team) {
  out <- team_secondary_colors_app[as.character(team)]
  out[is.na(out)] <- "#a78bfa"
  unname(out)
}

hex_to_rgba_app <- function(hex, alpha = 0.35) {
  hex <- gsub("#", "", as.character(hex %||% "6d28d9"))
  if (nchar(hex) != 6 || is.na(hex)) return(sprintf("rgba(109,40,217,%.2f)", alpha))
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha)
}

contrast_text_color_app <- function(hex) {
  hex <- gsub("#", "", as.character(hex %||% "000000"))
  if (nchar(hex) != 6) return("#ffffff")
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  lum <- (0.299 * r + 0.587 * g + 0.114 * b) / 255
  if (is.na(lum) || lum < 0.55) "#ffffff" else "#111827"
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

rank_matrix_plain <- function(m) {
  if (is.null(m) || length(m) != 900L) return(NULL)
  out <- matrix(suppressWarnings(as.numeric(m)), nrow = 30L, ncol = 30L)
  if (!all(is.finite(out))) return(NULL)
  row_sums <- rowSums(out)
  if (any(!is.finite(row_sums)) || any(row_sums <= 0)) return(NULL)
  out <- out / row_sums
  dimnames(out) <- list(sprintf("Rank %02d", 1:30), sprintf("Rank %02d", 1:30))
  out
}

# The production Markov model uses rank_worst: 1 = worst record, 30 = best.
# Methodology visuals use the more intuitive display convention requested here:
# Rank 01 = best team in the league, Rank 30 = worst. These helpers convert
# only the display objects; the simulation model and dashboard data remain in
# the production rank_worst convention.
rank_display_matrix <- function(P) {
  P <- rank_matrix_plain(P)
  if (is.null(P)) return(NULL)
  idx <- 30:1
  out <- P[idx, idx, drop = FALSE]
  dimnames(out) <- list(sprintf("Rank %02d", 1:30), sprintf("Rank %02d", 1:30))
  out
}

rank_display_to_model <- function(display_rank) {
  out <- 31L - as.integer(display_rank)
  out[is.na(out) | out < 1L | out > 30L] <- NA_integer_
  out
}

rank_horizon_interval_probs <- function(choice) {
  switch(
    as.character(choice %||% "10_90"),
    "40_60" = c(lo = 0.40, mid = 0.50, hi = 0.60),
    "25_75" = c(lo = 0.25, mid = 0.50, hi = 0.75),
    "10_90" = c(lo = 0.10, mid = 0.50, hi = 0.90),
    c(lo = 0.10, mid = 0.50, hi = 0.90)
  )
}

rank_horizon_interval_label <- function(choice) {
  switch(
    as.character(choice %||% "10_90"),
    "40_60" = "[40%, 60%]",
    "25_75" = "[25%, 75%]",
    "10_90" = "[10%, 90%]",
    "[10%, 90%]"
  )
}

rank_weighted_quantile <- function(prob, probs = c(0.10, 0.50, 0.90), values = seq_along(prob)) {
  prob <- as.numeric(prob)
  values <- as.numeric(values)
  prob[!is.finite(prob)] <- 0
  if (sum(prob) <= 0) prob <- rep(1 / length(prob), length(prob)) else prob <- prob / sum(prob)
  o <- order(values)
  prob <- prob[o]
  values <- values[o]
  cdf <- cumsum(prob)
  vapply(probs, function(q) {
    idx <- which(cdf >= q)[1]
    if (is.na(idx)) values[length(values)] else values[idx]
  }, numeric(1))
}

rank_horizon_tbl_one <- function(P, display_start_rank, max_horizon = 7L,
                                 interval_choice = "10_90") {
  P <- rank_matrix_plain(P)
  if (is.null(P)) return(tibble())

  display_start_rank <- as.integer(display_start_rank)
  if (is.na(display_start_rank) || display_start_rank < 1L || display_start_rank > 30L) {
    display_start_rank <- 15L
  }
  model_start_rank <- rank_display_to_model(display_start_rank)
  if (is.na(model_start_rank)) model_start_rank <- 16L

  interval_probs <- rank_horizon_interval_probs(interval_choice)
  interval_label <- rank_horizon_interval_label(interval_choice)

  v <- rep(0, 30)
  v[model_start_rank] <- 1
  model_ranks <- seq_len(30)
  display_values <- 31 - model_ranks
  out <- vector("list", max_horizon + 1L)

  for (h in 0:max_horizon) {
    qs <- rank_weighted_quantile(v, interval_probs, values = display_values)
    mean_display_rank <- sum(display_values * v)
    out[[h + 1L]] <- tibble(
      years_ahead = h,
      start_rank  = display_start_rank,
      mean_rank   = mean_display_rank,
      qlo         = qs[["lo"]],
      q50         = qs[["mid"]],
      qhi         = qs[["hi"]],
      interval_label = interval_label,
      hover = sprintf(
        "Start Rank %02d<br>Years ahead: %d<br>Expected rank: %.1f<br>Median rank: %02d<br>%s interval: Rank %02d to Rank %02d",
        display_start_rank, h, mean_display_rank, qs[["mid"]], interval_label, qs[["lo"]], qs[["hi"]]
      )
    )
    v <- as.numeric(v %*% P)
  }

  bind_rows(out)
}

rank_horizon_tbl_all <- function(P, max_horizon = 7L, interval_choice = "10_90") {
  bind_rows(lapply(seq_len(30), function(r) {
    rank_horizon_tbl_one(P, r, max_horizon, interval_choice)
  }))
}


# ============================================================================
# UI
# ============================================================================

ui <- page_navbar(
  theme  = app_theme,
  title  = "NBA 3-2-1 Lottery Reform",
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
    .selectize-logo-option {
      display: flex !important;
      align-items: center;
      gap: 7px;
      width: 100%;
      min-width: 0;
      padding: 2px 0;
      box-sizing: border-box;
    }
    .selectize-logo-item {
      display: inline-flex !important;
      align-items: center;
      gap: 7px;
      min-width: 0;
      max-width: 100%;
      white-space: nowrap;
    }
    .selectize-logo-option img, .selectize-logo-item img {
      margin-right: 6px !important;
      flex: 0 0 auto;
    }
    .selectize-dropdown .option { display:block !important; width:100%; }
    .selectize-input > div { max-width: 100%; }
    .sp-team-detail-row {
      display: grid;
      grid-template-columns: 108px minmax(0, 1fr);
      column-gap: 4px;
      align-items: start;
      margin-bottom: 13px;
      font-size: 14px;
    }
    .sp-team-detail-row .sp-team-label {
      padding-top: 0;
      line-height: 1.28;
      color: #aaa;
    }
    .sp-team-detail-row .sp-team-value {
      display: flex;
      align-items: flex-start;
      gap: 7px;
      flex-wrap: wrap;
      line-height: 1.28;
      font-size: 15px;
    }
    .sp-team-inline {
      display: inline-flex;
      align-items: flex-start;
      gap: 6px;
      margin-right: 9px;
      line-height: 1.25;
      font-size: 15px;
    }
    .sp-team-inline img {
      flex: 0 0 auto;
      transform: translateY(-2px);
    }
    .sp-details-separator {
      margin: 10px 0 14px !important;
    }
    #sp_asset + .selectize-control {
      margin-bottom: 0 !important;
    }
    .sp-prob-card-row {
      display: flex;
      gap: 10px;
      align-items: stretch;
      flex-wrap: nowrap;
      width: 374px;
      max-width: 374px;
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
      overflow-y: visible;
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
      cursor: pointer;
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
    /* Team Summaries: a SINGLE scroll container that we control. DT renders flat
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
      max-width: 100%;
    }
    .tm-page .tm-pick-table table {
      width: 100%;
      min-width: 0;
      table-layout: fixed;
      border-collapse: collapse;
      font-size: 12px;
    }
    .tm-page .tm-pick-table th {
      color: #d0d0d0;
      font-weight: 700;
      padding: 5px 4px;
      border-bottom: 1px solid #1a1a2a;
      white-space: normal;
      overflow-wrap: anywhere;
      line-height: 1.15;
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
    .tm-page .tm-pick-table .tm-protection-select {
      min-width: 118px;
      max-width: 145px;
    }
    .tm-page .tm-pick-table .tm-protection-select .selectize-input,
    .tm-page .tm-pick-table .tm-protection-select .selectize-input > div,
    .tm-page .tm-pick-table .tm-protection-select .selectize-dropdown .option {
      white-space: nowrap !important;
      overflow-wrap: normal !important;
      word-break: keep-all !important;
    }
    .tm-page .tm-pick-table .tm-protection-select .selectize-dropdown .option {
      overflow: hidden !important;
      text-overflow: ellipsis !important;
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

    .btn-outline-light {
      color: #ffffff !important;
      border-color: rgba(255,255,255,0.92) !important;
      background: transparent !important;
    }
    .btn-outline-light:hover,
    .btn-outline-light:focus {
      color: #0a0a14 !important;
      background: #ffffff !important;
      border-color: #ffffff !important;
    }
    .pm-sortable-th {
      cursor: pointer;
      user-select: none;
    }
    .pm-sortable-th::after {
      content: ' ↕';
      color: rgba(255,255,255,0.35);
      font-size: 0.85em;
    }
    .tm-team1-input-card .card-header,
    .tm-team1-input-card .bslib-card .card-header,
    .tm-team1-input-card > .card-header,
    .tm-team1-out-card .card-header,
    .tm-team1-out-card > .card-header {
      text-align: right !important;
      justify-content: flex-end !important;
    }
    .tm-team1-input-card .form-group,
    .tm-team1-input-card .shiny-input-container {
      margin-left: auto !important;
      margin-right: 0 !important;
      text-align: right !important;
    }
    .tm-team1-input-card label {
      text-align: right !important;
      width: 100%;
    }
    .tm-team1-input-card .selectize-control,
    .tm-team1-input-card .selectize-input,
    .tm-team1-input-card .selectize-dropdown {
      text-align: left !important;
    }
  "))),

  # Dynamically size the Team Summaries scroll-body so that when the team detail
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

      window.pmSortTable = function(tableId, colIdx) {
        var table = document.getElementById(tableId);
        if (!table) return;
        var tbody = table.tBodies[0];
        if (!tbody) return;
        var th = table.tHead && table.tHead.rows[0] ? table.tHead.rows[0].cells[colIdx] : null;
        var dir = th && th.getAttribute('data-sort-dir') === 'asc' ? 'desc' : 'asc';
        if (table.tHead && table.tHead.rows[0]) {
          Array.from(table.tHead.rows[0].cells).forEach(function(h) { h.removeAttribute('data-sort-dir'); });
        }
        if (th) th.setAttribute('data-sort-dir', dir);
        var rows = Array.from(tbody.rows);
        rows.sort(function(a, b) {
          var av = (a.cells[colIdx] && a.cells[colIdx].getAttribute('data-sort')) || (a.cells[colIdx] ? a.cells[colIdx].innerText : '');
          var bv = (b.cells[colIdx] && b.cells[colIdx].getAttribute('data-sort')) || (b.cells[colIdx] ? b.cells[colIdx].innerText : '');
          var an = parseFloat(String(av).replace(/[^0-9.-]/g, ''));
          var bn = parseFloat(String(bv).replace(/[^0-9.-]/g, ''));
          var cmp;
          if (!isNaN(an) && !isNaN(bn) && String(av).match(/[0-9]/) && String(bv).match(/[0-9]/)) {
            cmp = an === bn ? 0 : (an < bn ? -1 : 1);
          } else {
            cmp = String(av).localeCompare(String(bv));
          }
          return dir === 'asc' ? cmp : -cmp;
        });
        rows.forEach(function(r) { tbody.appendChild(r); });
      };

      document.addEventListener('click', function(e) {
        var th = e.target.closest && e.target.closest('.pm-sortable-th');
        if (!th) return;
        var tid = th.getAttribute('data-table-id');
        var col = parseInt(th.getAttribute('data-sort-col'), 10);
        if (tid && !isNaN(col) && window.pmSortTable) {
          window.pmSortTable(tid, col);
        }
      });

    })();
  "))),

  # ---- Tab 0: About ----
  nav_panel(
    title = "About",
    icon  = icon("circle-info"),
    div(class = "about-page",
      tags$style(HTML("
        .about-page { max-width: 1180px; margin: 0 auto; padding: 10px 6px 24px; }
        .about-hero {
          border: 1px solid rgba(255,255,255,0.08);
          background: linear-gradient(135deg, rgba(109,40,217,0.18), rgba(15,15,26,0.96));
          border-radius: 16px;
          padding: 22px 24px;
          margin-bottom: 14px;
        }
        .about-hero h2 { margin: 0 0 8px; font-weight: 900; letter-spacing: -0.03em; color: #f4f4f8; }
        .about-hero p, .about-card p, .about-card li { color:#cfd2dc; line-height:1.65; font-size:14px; }
        .about-card h4 { color:#f4f4f8; font-weight:850; margin:0 0 8px; }
        .about-card { height:100%; }
        .about-glossary { display:grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 9px 18px; margin-top: 6px; }
        .about-start { display:grid; grid-template-columns: 1fr; gap: 14px; margin-top: 6px; }
        .about-term { color:#f4f4f8; font-weight:850; }
        .about-def { color:#b8bcc9; }
        @media (max-width: 900px) { .about-glossary { grid-template-columns: 1fr; } }
      ")),
      div(class = "about-hero",
        tags$h2("NBA Draft Pick Valuations Under the New 3-2-1 Lottery"),
        tags$p("This dashboard values each team's draft-pick portfolio under the current lottery structure and the approved 3-2-1 format. It is meant to translate a complicated asset-allocation problem into three questions: how valuable are a team's picks, how much does the rule change move that value, and which picks explain the movement?"),
        tags$p(
          "Details can be found on the Methodology tab and the ",
          tags$a(
            "GitHub repo",
            href = "https://github.com/alex-susi/nbaDraftLottery",
            target = "_blank",
            rel = "noopener noreferrer"
          ),
          "."
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(class = "about-card",
          card_header("Expected Pick Value"),
          tags$p("Expected Pick Value (EPV) is the model's posterior mean value of a draft asset before we know the actual player selected. The app values every future pick through simulated team trajectories, lottery outcomes, protections, swaps, and conveyance rules, then maps the resulting draft slot to a Bayesian pick-value curve. EPV is useful for trade analysis because it separates the asset value of a pick from the randomness of a specific player's career outcome.")
        ),
        card(class = "about-card",
          card_header("The 3-2-1 Rule"),
          tags$p("The 3-2-1 system gives the 16 non-playoff teams lottery balls by competitive tier: three balls for non-play-in teams, two balls for the three relegation teams and the 9/10 play-in seeds, and one ball for the 7v8 play-in losers. The model also applies the associated anti-tank rules and the relegation floor, then compares each team's portfolio against the current lottery system on the same simulated seasons.")
        )
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(class = "about-card",
          card_header("Glossary"),
          div(class = "about-glossary",
            div(tags$span(class = "about-term", "EPV"), div(class = "about-def", "Expected Pick Value; posterior mean draft-asset value on the four-year Win Shares scale.")),
            div(tags$span(class = "about-term", "4-YR WS"), div(class = "about-def", "A player's cumulative Basketball-Reference Win Shares over his first four NBA seasons.")),
            div(tags$span(class = "about-term", "Conveyance"), div(class = "about-def", "Whether a traded pick actually transfers to the receiving team after protections and conditions are applied.")),
            div(tags$span(class = "about-term", "Protection"), div(class = "about-def", "A condition that lets the original team keep the pick in certain ranges, such as top-4 or lottery protected.")),
            div(tags$span(class = "about-term", "Swap right"), div(class = "about-def", "The right to exchange picks with another team when the swap holder's outcome is better.")),
            div(tags$span(class = "about-term", "Relegation"), div(class = "about-def", "The three worst teams overall; under 3-2-1 they receive two lottery balls and cannot fall past pick 12.")),
            div(tags$span(class = "about-term", "Non-Play-In"), div(class = "about-def", "Non-relegated teams that miss the play-in; under 3-2-1 they receive three balls.")),
            div(tags$span(class = "about-term", "9/10 Seeds"), div(class = "about-def", "The four conference 9- and 10-seeds; under 3-2-1 they receive two balls.")),
            div(tags$span(class = "about-term", "7v8 Losers"), div(class = "about-def", "The two teams that lose the 7-vs-8 play-in games; under 3-2-1 they receive one ball.")),
            div(tags$span(class = "about-term", "Playoff"), div(class = "about-def", "The 14 playoff teams, ordered after the lottery teams for draft-position purposes."))
          )
        ),
        card(class = "about-card",
          card_header("Where to Start"),
          div(class = "about-start",
            div(tags$span(class = "about-term", "Pick Landscape"), div(class = "about-def", "A high-level view of each team's pick quantity, pick quality, and total portfolio value.")),
            div(tags$span(class = "about-term", "Team Summaries"), div(class = "about-def", "Impact of the 3-2-1 Lottery on each team's pick portfolio, including a summary of the most affected picks.")),
            div(tags$span(class = "about-term", "Single Pick"), div(class = "about-def", "A distribution view for an individual asset.")),
            div(tags$span(class = "about-term", "Pick Movers"), div(class = "about-def", "Impact of the 3-2-1 Lottery on every individual pick.")),
            div(tags$span(class = "about-term", "Trade Machine"), div(class = "about-def", "Evaluate a real or hypothetical transaction. Select picks from each team, attach protections or swap rights, and compare expected asset value, realized outcome simulations, and best-player probabilities side by side."))
          )
        )
      )
    )
  ),

  # ---- Tab 1: Pick Landscape ----
  nav_panel(
    title = "Pick Landscape",
    icon  = icon("chart-area"),
    layout_sidebar(
      sidebar = sidebar(
        width = 285,
        selectInput("impact_year", "Draft year",
          choices = c("All", sort(unique(pick_display_assets$year))), selected = "All"),
        selectInput("impact_round", "Round",
          choices = c("All" = "All", "Round 1" = "1", "Round 2" = "2"), selected = "All"),
        selectInput("impact_view", "View",
          choices = c("Pick Scatterplot" = "scatter", "EPV Leaderboard" = "leaderboard"),
          selected = "scatter"),
        conditionalPanel(
          condition = "input.impact_view == 'leaderboard'",
          selectInput("impact_sort", "Sort teams by",
            choices = c("Δ EPV (biggest movers)" = "delta",
                        "Total 3-2-1 EPV" = "total"),
            selected = "total")
        ),
        actionButton("impact_clear", "Clear filters", class = "btn btn-outline-light btn-sm"),
        tags$p(class = "mono", style = "font-size:11px; color:#888; line-height:1.6; margin-top:12px;",
          "Pick Scatterplot shows 3-2-1 average EPV per pick against expected pick count. EPV Leaderboard compares each team's current EPV to new 3-2-1 EPV, with the logo placed at the 3-2-1 midpoint.")
      ),
      card(
        card_header(textOutput("impact_title")),
        plotlyOutput("impact_chart", height = "820px")
      )
    )
  ),

  # ---- Tab 3 ----
  nav_panel(
    title = "Team Summaries",
    icon  = icon("table"),
    div(class = "ft-page",
      tags$style(HTML("
        .ft-split {
          display: grid;
          grid-template-columns: minmax(620px, 1.05fr) minmax(390px, 0.95fr);
          gap: 12px;
          height: calc(100vh - 92px);
          min-height: 620px;
          overflow: hidden;
        }
        .ft-table-card, .ft-detail-pane {
          min-height: 0;
          height: 100%;
          overflow: hidden;
        }
        .ft-table-card {
          display: flex;
          flex-direction: column;
        }
        .ft-table-card .card-header,
        .ft-table-card > .card-header {
          flex: 0 0 auto;
        }
        .ft-table-card .card-body, .ft-table-card .bslib-card-body,
        .ft-table-card [data-card-body] {
          min-height: 0 !important;
          overflow: hidden !important;
          padding-bottom: 8px !important;
        }
        .ft-scroll-wrap {
          flex: 1 1 auto;
          min-height: 0;
          height: auto !important;
          max-height: none !important;
          overflow-y: auto !important;
          overflow-x: hidden !important;
          padding-right: 4px;
        }
        .ft-scroll-wrap table.dataTable thead,
        .ft-scroll-wrap table.dataTable thead tr,
        .ft-scroll-wrap table.dataTable thead th {
          position: sticky !important;
          top: 0 !important;
          z-index: 30 !important;
          background: #0a0a14 !important;
        }
        .ft-detail-pane {
          height: 100%;
          overflow-y: auto;
          overflow-x: hidden;
        }
        .ft-detail-pane:empty { display:none; }
        .ft-detail-pane:empty + * { display:none; }
        .ft-split:has(.ft-detail-pane:empty) {
          grid-template-columns: 1fr;
        }
        .ft-split:has(.ft-detail-pane:empty) .ft-table-card {
          grid-column: 1 / -1;
        }
        .ft-split:has(.ft-detail-pane:empty) .ft-detail-pane {
          display: none;
        }
        .team-detail-card table {
          table-layout: fixed;
          width: 100%;
        }
        .team-detail-card td, .team-detail-card th {
          white-space: normal;
          overflow-wrap: anywhere;
        }
        .team-detail-card th {
          font-weight: 800 !important;
          color: #cfd2dc !important;
        }
      ")),
      div(class = "ft-split",
        card(class = "ft-table-card",
          card_header(textOutput("full_table_title")),
          div(id = "ft-scroll-wrap", class = "ft-scroll-wrap",
            DTOutput("full_table")
          )
        ),
        div(class = "ft-detail-pane", uiOutput("team_detail"))
      )
    )
  ),

  # ---- Tab 5: Single Pick Valuation ----
  nav_panel(
    title = "Single Pick",
    icon  = icon("basketball"),
    layout_sidebar(
      sidebar = sidebar(
        width = 500,
        selectInput("sp_year", "Draft year",
          choices  = sort(unique(pick_display_assets$year)),
          selected = 2026),
        selectizeInput("sp_team", "Team", choices = team_select_choices_app(all_team_abbr),
          options = selectize_logo_options_app),
        selectInput("sp_asset", "Pick", choices = NULL),
        hr(class = "sp-details-separator"),
        uiOutput("sp_obligation")
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Expected Pick Value Impact"),
          uiOutput("sp_headline")
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Expected Pick Value"),
          plotlyOutput("sp_dist_ev", height = "380px")
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
        .pm-page { height: calc(100vh - 92px); min-height: 0; overflow: hidden; }
        .pm-page table.dataTable { font-size: 12px; color:#e6e8ef; table-layout: fixed !important; width: 100% !important; }
        .pm-page table.dataTable th, .pm-page table.dataTable td { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
        .pm-page table.dataTable td:nth-child(4), .pm-page table.dataTable th:nth-child(4) { white-space: normal; overflow-wrap: anywhere; }
        .pm-page .card,
        .pm-page .bslib-card,
        .pm-page .card-body,
        .pm-page .bslib-card-body,
        .pm-page [data-card-body] {
          min-height: 0 !important;
        }
        .pm-page .dataTables_scrollBody {
          height: calc(100vh - 260px) !important;
          max-height: calc(100vh - 260px) !important;
        }
        .pm-page .card, .pm-page .bslib-card { height: calc(100vh - 138px); overflow:hidden; }
      ")),
      layout_sidebar(
        sidebar = sidebar(
          width = 235,
          selectInput("pm_year", "Draft year",
            choices = c("All", sort(unique(pick_display_assets$year))), selected = "All"),
          selectInput("pm_round", "Round",
            choices = c("All" = "All", "Round 1" = "1", "Round 2" = "2"), selected = "All"),
          selectizeInput("pm_team", "Team",
            choices = c("All teams" = "All", team_select_choices_app(sort(unique(pick_display_assets$owner)))),
            selected = "All", options = selectize_logo_options_app),
          actionButton("pm_clear", "Clear filters", class = "btn btn-outline-light btn-sm")
        ),
        layout_columns(
          col_widths = c(12),
          card(
            card_header("All Picks by Expected Pick Value"),
            DTOutput("pm_ev_table", height = "calc(100vh - 210px)")
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
        .tm-page .tm-top-reset {
          display:flex;
          justify-content:center;
          margin: -2px 0 2px;
        }
        .tm-page .tm-clear-btn {
          font-size: 1.05rem;
          font-weight: 800;
          padding: 8px 18px;
          border-radius: 8px;
        }
        .tm-page .tm-team-picker-grid {
          display: grid;
          grid-template-columns: minmax(155px, 0.62fr) minmax(330px, 1.38fr);
          gap: 18px;
          align-items: start;
          padding: 2px 4px 0;
        }
        .tm-page .tm-team-picker-grid-right {
          grid-template-columns: minmax(330px, 1.38fr) minmax(155px, 0.62fr);
        }
        .tm-page .tm-team-identity-pane {
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          min-height: 214px;
          text-align: center;
        }
        .tm-page .tm-team-identity-pane img {
          margin: 0 0 13px 0 !important;
        }
        .tm-page .tm-team-controls {
          align-self: start;
          width: 100%;
          padding-top: 16px;
        }
        .tm-page .tm-team-name {
          font-weight: 900;
          color: #e6e8ef;
          font-size: 24px;
          line-height: 1.15;
          text-align: center;
        }
        .tm-page .tm-team-controls .shiny-input-container,
        .tm-page .tm-team-controls .form-group {
          margin-bottom: 10px;
        }
        .tm-page .tm-team-controls label {
          font-weight: 800;
          color: #cfd2dc;
        }
        .tm-page .tm-trade-assessment-title {
          font-size: 24px;
          line-height: 1.15;
          font-weight: 900;
          color: #e6e8ef;
          text-align: left;
        }
        .tm-page .tm-graph-card .card-header,
        .tm-page .tm-graph-card > .card-header,
        .tm-page .tm-graph-card [data-card-header] {
          text-align: center !important;
          justify-content: center !important;
          font-weight: 850;
        }
      ")),
      uiOutput("tm_dynamic_css"),
      div(class = "tm-top-reset",
        actionButton("tm_reset", "Clear Trade Inputs", class = "btn btn-outline-light tm-clear-btn")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(class = "tm-selection-card tm-team1-input-card",
          div(class = "tm-team-picker-grid tm-team-picker-grid-left",
            div(class = "tm-team-identity-pane", uiOutput("tm_teamA_hdr")),
            div(class = "tm-team-controls",
              selectizeInput("tm_teamA", "Team 1",
                choices = team_select_choices_app(all_team_abbr), selected = all_team_abbr[1],
                options = selectize_logo_options_app),
              uiOutput("tm_picksA_picker")
            )
          )
        ),
        card(class = "tm-selection-card",
          div(class = "tm-team-picker-grid tm-team-picker-grid-right",
            div(class = "tm-team-controls",
              selectizeInput("tm_teamB", "Team 2",
                choices = team_select_choices_app(all_team_abbr), selected = all_team_abbr[2],
                options = selectize_logo_options_app),
              uiOutput("tm_picksB_picker")
            ),
            div(class = "tm-team-identity-pane", uiOutput("tm_teamB_hdr"))
          )
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        gap = "2.5rem",
        class = "tm-outgoing-row",
        card(class = "tm-selection-card tm-team1-out-card",
          card_header(uiOutput("tm_picksA_out_hdr")),
          uiOutput("tm_picksA_table")
        ),
        card(class = "tm-selection-card",
          card_header(uiOutput("tm_picksB_out_hdr")),
          uiOutput("tm_picksB_table")
        )
      ),
      card(
        card_header(tags$div(class = "tm-trade-assessment-title", "Trade Assessment")),
        uiOutput("tm_verdict")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        card(class = "tm-graph-card",
          card_header("Expected Pick Value"),
          plotlyOutput("tm_dist_ev", height = "300px")
        ),
        card(class = "tm-graph-card",
          card_header("4-Yr WS Outcome Simulation"),
          plotlyOutput("tm_dist_outcome", height = "300px")
        ),
        card(class = "tm-graph-card",
          card_header("Best Player Outcome"),
          plotlyOutput("tm_dist_best", height = "300px")
        )
      )
    )
  ),

  # ---- Tab 7b: Methodology ----
  nav_panel(
    title = "Methodology",
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
          card_header(
            div(
              style = "display:flex; align-items:center; justify-content:space-between; gap:12px; width:100%;",
              span("Team-Strength Transition Matrix"),
              div(
                style = "width:170px; margin-bottom:-14px;",
                selectInput(
                  "methodology_matrix_type",
                  NULL,
                  choices = c("Tier Matrix" = "tier", "30 Rank" = "rank"),
                  selected = "tier",
                  width = "100%"
                )
              )
            )
          ),
          plotlyOutput("methodology_transition_heatmap", height = "390px")
        ),
        card(
          card_header(
            div(
              style = "display:flex; align-items:center; justify-content:space-between; gap:12px; width:100%; flex-wrap:wrap;",
              span("Seven-Year Rank Trajectory"),
              div(
                style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap; margin-bottom:-14px;",
                div(
                  style = "width:175px;",
                  selectInput(
                    "rank_horizon_start_rank",
                    NULL,
                    choices = c("All ranks" = "all", setNames(as.character(1:30), sprintf("Rank %02d", 1:30))),
                    selected = "15",
                    width = "100%"
                  )
                ),
                div(
                  style = "width:155px;",
                  selectInput(
                    "rank_horizon_interval",
                    NULL,
                    choices = c("[40%, 60%]" = "40_60", "[25%, 75%]" = "25_75", "[10%, 90%]" = "10_90"),
                    selected = "10_90",
                    width = "100%"
                  )
                )
              )
            )
          ),
          plotlyOutput("rank_horizon_plot", height = "390px")
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Draft Pick Value Curve"),
          plotlyOutput("pick_curve_plot_combined", height = "440px")
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Model Validation & Diagnostics"),
          uiOutput("validation_panel")
        )
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
  )

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


  observeEvent(input$impact_clear, {
    updateSelectInput(session, "impact_year", selected = "All")
    updateSelectInput(session, "impact_round", selected = "All")
    impact_selected_team(NULL)
  })

  observeEvent(input$wl_clear, {
    updateSelectInput(session, "wl_year", selected = "All")
    updateSelectInput(session, "wl_round", selected = "All")
  })

  observeEvent(input$pm_clear, {
    updateSelectInput(session, "pm_year", selected = "All")
    updateSelectInput(session, "pm_round", selected = "All")
    updateSelectizeInput(
      session, "pm_team",
      choices = c("All teams" = "All", team_select_choices_app(sort(unique(pick_display_assets$owner)))),
      selected = "All",
      options = selectize_logo_options_app,
      server = FALSE
    )
  })

  observeEvent(input$tm_reset, {
    teamA <- isolate(input$tm_teamA %||% all_team_abbr[1])
    teamB <- isolate(input$tm_teamB %||% all_team_abbr[2])

    optsA <- picks_owned_by(teamA)
    optsB <- picks_owned_by(teamB)
    choicesA <- if (nrow(optsA) == 0L) character(0) else setNames(optsA$display_asset_id, optsA$trade_label)
    choicesB <- if (nrow(optsB) == 0L) character(0) else setNames(optsB$display_asset_id, optsB$trade_label)

    updateSelectizeInput(session, "tm_picksA", choices = choicesA, selected = character(0), server = TRUE)
    updateSelectizeInput(session, "tm_picksB", choices = choicesB, selected = character(0), server = TRUE)
  })

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
    input$impact_year
    input$impact_round
    input$impact_view
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
    view <- input$impact_view %||% "scatter"
    yr_txt <- if (identical(yr, "All")) "All Years" else yr
    rnd_txt <- if (identical(rnd, "All")) "All Rounds" else paste("Round", rnd)
    if (identical(view, "leaderboard")) {
      sprintf("EPV Leaderboard — %s, %s", yr_txt, rnd_txt)
    } else {
      sprintf("Pick Scatterplot — %s, %s", yr_txt, rnd_txt)
    }
  })

  output$impact_chart <- renderPlotly({
    if (identical(input$impact_view %||% "scatter", "leaderboard")) {
      sort_by <- input$impact_sort %||% "total"
      df <- portfolio_quality_quantity_summary_app(
        "ev",
        year_filter = input$impact_year %||% "All",
        round_filter = input$impact_round %||% "All"
      ) %>%
        transmute(
          team,
          current_mean = .data$cur_total_value,
          new_mean = .data$new_total_value,
          delta_value = .data$delta_total_value,
          p_pos = .data$p_positive,
          n_picks = .data$new_expected_picks,
          lo = .data$cur_total_value + coalesce(.data$delta_q10, 0),
          hi = .data$cur_total_value + coalesce(.data$delta_q90, 0)
        ) %>%
        mutate(
          dir = if_else(.data$delta_value >= 0, "gain", "loss"),
          col = if_else(.data$delta_value >= 0, "#10b981", "#ef4444"),
          primary = team_primary_color_app(.data$team),
          text_col = vapply(.data$primary, contrast_text_color_app, character(1)),
          hover_text = sprintf(
            paste0(
              "<b>%s</b><br>",
              "Total Picks: %.1f<br>",
              "Current: %.1f EPV<br>",
              "3-2-1: %.1f EPV<br>",
              "Change: %+.1f EPV<br>",
              "90%% CI: [%.1f, %.1f]"
            ),
            .data$team, .data$n_picks, .data$current_mean, .data$new_mean, .data$delta_value, .data$lo, .data$hi
          ),
          order_key = if (identical(sort_by, "total")) .data$new_mean else .data$delta_value
        ) %>%
        arrange(.data$order_key) %>%
        mutate(
          y = seq_len(n()),
          x_logo = .data$new_mean,
          y_logo = .data$y
        )

      if (nrow(df) == 0L) {
        return(plot_ly() %>% plotly_dark(annotations = list(text = "No picks match the selected filters",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE)))
      }

      p <- plot_ly(source = "impact_leaderboard")
      for (i in seq_len(nrow(df))) {
        row_i <- df[i, , drop = FALSE]
        p <- p %>%
          add_segments(
            data = row_i,
            x = ~lo, xend = ~hi, y = ~y, yend = ~y, customdata = ~team,
            line = list(color = "rgba(255,255,255,0.16)", width = 6),
            hovertemplate = paste0(row_i$hover_text[1], "<extra></extra>"),
            hoverlabel = list(bgcolor = row_i$primary[1], font = list(color = row_i$text_col[1]), align = "right"),
            showlegend = FALSE
          ) %>%
          add_segments(
            data = row_i,
            x = ~current_mean, xend = ~new_mean, y = ~y, yend = ~y, customdata = ~team,
            line = list(color = row_i$col[1], width = 2), opacity = 0.62,
            hovertemplate = paste0(row_i$hover_text[1], "<extra></extra>"),
            hoverlabel = list(bgcolor = row_i$primary[1], font = list(color = row_i$text_col[1]), align = "right"),
            showlegend = FALSE
          ) %>%
          add_segments(
            data = row_i,
            x = ~current_mean, xend = ~current_mean, y = ~(y - 0.16), yend = ~(y + 0.16), customdata = ~team,
            line = list(color = "rgba(229,231,235,0.62)", width = 2),
            hoverinfo = "skip", showlegend = FALSE
          ) %>%
          add_markers(
            data = row_i,
            x = ~new_mean, y = ~y, customdata = ~team,
            marker = list(color = "rgba(255,255,255,0.01)", size = 28, line = list(color = "rgba(255,255,255,0)", width = 0)),
            hovertemplate = paste0(row_i$hover_text[1], "<extra></extra>"),
            hoverlabel = list(bgcolor = row_i$primary[1], font = list(color = row_i$text_col[1]), align = "right"),
            showlegend = FALSE, opacity = 0.01
          )
      }

      logo_images <- team_logo_layout_images_app(
        df %>% transmute(team, x = new_mean, y = y),
        x_col = "x", y_col = "y",
        sizex = max(3.8, diff(range(c(df$current_mean, df$new_mean), na.rm = TRUE)) * 0.036),
        sizey = 1.18,
        opacity = 0.98
      )

      rng <- range(c(df$lo, df$hi, df$current_mean, df$new_mean, df$x_logo), na.rm = TRUE)
      if (!all(is.finite(rng)) || diff(rng) == 0) rng <- c(0, 1)
      pad <- max(1, diff(rng) * 0.06)

      plot_obj <- p %>%
        plotly_dark(
          xaxis = list(title = list(text = "Total EPV"), range = c(rng[1] - pad, rng[2] + pad), tickformat = ".1f"),
          yaxis = list(title = list(text = "Team"), tickmode = "array", tickvals = df$y, ticktext = as.character(df$team), range = c(0.5, nrow(df) + 0.5), autorange = FALSE),
          margin = list(l = 76, r = 30, t = 12, b = 36),
          showlegend = FALSE
        )

      return(attach_plot_logo_overlays_app(plot_obj, df %>% transmute(team, x = x_logo, y = y_logo), "x", "y", size = 34))
    }

    df_all <- portfolio_quality_quantity_summary_app(
      "ev",
      year_filter = input$impact_year %||% "All",
      round_filter = input$impact_round %||% "All"
    ) %>%
      mutate(
        x = .data$new_avg_quality,
        y = .data$new_expected_picks,
        primary = team_primary_color_app(.data$team),
        text_col = vapply(.data$primary, contrast_text_color_app, character(1)),
        hover_text = sprintf(
          "<b>%s</b><br>Avg EPV / pick: %.1f<br>Number of picks: %.1f",
          .data$team, .data$new_avg_quality, .data$new_expected_picks
        )
      ) %>%
      apply_logo_collision_offsets_app(x_col = "x", y_col = "y")

    selected_team <- impact_selected_team()
    df <- if (!is.null(selected_team) && selected_team %in% df_all$team) {
      df_all %>% filter(.data$team == .env$selected_team)
    } else {
      df_all
    }

    x_rng <- range(c(df_all$x, df_all$x_logo), na.rm = TRUE)
    y_rng <- range(c(df_all$y, df_all$y_logo), na.rm = TRUE)
    if (!all(is.finite(x_rng)) || diff(x_rng) == 0) x_rng <- c(0, 1)
    if (!all(is.finite(y_rng)) || diff(y_rng) == 0) y_rng <- c(0, 1)
    pad_x <- max(0.25, diff(x_rng) * 0.14)
    pad_y <- max(0.75, diff(y_rng) * 0.14)
    x_ref <- median(df_all$x, na.rm = TRUE)
    y_ref <- median(df_all$y, na.rm = TRUE)

    p <- plot_ly(source = "impact_plot")

    # Invisible, clickable/hoverable hit targets under each logo. Use one
    # small data-backed trace per team so Plotly receives real hover text rather
    # than the literal "%{text}" placeholder.
    for (i in seq_len(nrow(df))) {
      row_i <- df[i, , drop = FALSE]
      p <- p %>% add_markers(
        data = row_i,
        x = ~x_logo, y = ~y_logo, customdata = ~team,
        marker = list(color = "rgba(255,255,255,0.01)", size = 48, line = list(color = "rgba(255,255,255,0)", width = 0)),
        hovertemplate = paste0(row_i$hover_text[1], "<extra></extra>"),
        hoverlabel = list(bgcolor = row_i$primary[1], font = list(color = row_i$text_col[1])),
        showlegend = FALSE, opacity = 0.01
      )
    }

    logo_images <- team_logo_layout_images_app(
      df, x_col = "x_logo", y_col = "y_logo",
      sizex = max(0.24, diff(x_rng) * 0.070),
      sizey = max(1.30, diff(y_rng) * 0.095),
      opacity = 0.98
    )

    shapes <- list(
      list(type = "line", x0 = x_ref, x1 = x_ref, y0 = y_rng[1] - pad_y, y1 = y_rng[2] + pad_y,
           line = list(color = "rgba(255,255,255,0.18)", dash = "dash")),
      list(type = "line", x0 = x_rng[1] - pad_x, x1 = x_rng[2] + pad_x, y0 = y_ref, y1 = y_ref,
           line = list(color = "rgba(255,255,255,0.18)", dash = "dash"))
    )

    annotations <- list(
      list(x = x_rng[2] + pad_x * 0.08, y = y_rng[2] + pad_y * 0.58, text = "<b>High quality / high quantity</b>", showarrow = FALSE, xanchor = "right", font = list(size = 13, color = "#10f0a5")),
      list(x = x_rng[2] + pad_x * 0.08, y = y_rng[1] - pad_y * 0.38, text = "<b>High quality / low quantity</b>", showarrow = FALSE, xanchor = "right", font = list(size = 13, color = "#d1d5db")),
      list(x = x_rng[1] - pad_x * 0.08, y = y_rng[2] + pad_y * 0.58, text = "<b>Low quality / high quantity</b>", showarrow = FALSE, xanchor = "left", font = list(size = 13, color = "#d1d5db")),
      list(x = x_rng[1] - pad_x * 0.08, y = y_rng[1] - pad_y * 0.38, text = "<b>Low quality / low quantity</b>", showarrow = FALSE, xanchor = "left", font = list(size = 13, color = "#ff6060"))
    )

    plot_obj <- p %>%
      plotly_dark(
        xaxis = list(title = list(text = "Average EPV per pick", font = list(size = 15)), tickformat = ".1f",
                     tickfont = list(size = 12), range = c(x_rng[1] - pad_x, x_rng[2] + pad_x)),
        yaxis = list(title = list(text = "Number of Picks", font = list(size = 15)), tickformat = ".0f",
                     tickfont = list(size = 12), range = c(y_rng[1] - pad_y, y_rng[2] + pad_y)),
        shapes = shapes, annotations = annotations,
        margin = list(l = 74, r = 34, t = 72, b = 64),
        showlegend = FALSE
      ) %>%
      event_register("plotly_click")

    attach_plot_logo_overlays_app(plot_obj, df, "x_logo", "y_logo", size = 40)
  })

  # ---- Impact: dumbbell / tornado ----
  output$wl_title <- renderText({
    yr <- input$wl_year %||% "All"
    rnd <- input$wl_round %||% "All"
    yr_txt <- if (identical(yr, "All")) "all years" else yr
    rnd_txt <- if (identical(rnd, "All")) "all rounds" else paste("round", rnd)
    sprintf("Who Wins and Loses Under 3-2-1 — Expected Pick Value, %s, %s", yr_txt, rnd_txt)
  })

  output$wl_dumbbell <- renderPlotly({
    sort_by <- input$wl_sort %||% "delta"
    df <- portfolio_quality_quantity_summary_app(
      "ev",
      year_filter = input$wl_year %||% "All",
      round_filter = input$wl_round %||% "All"
    ) %>%
      transmute(
        team,
        current_mean = .data$cur_total_value,
        new_mean = .data$new_total_value,
        delta_value = .data$delta_total_value,
        p_pos = .data$p_positive,
        n_picks = .data$new_expected_picks,
        lo = .data$cur_total_value + coalesce(.data$delta_q10, 0),
        hi = .data$cur_total_value + coalesce(.data$delta_q90, 0)
      ) %>%
      mutate(
        dir = if_else(.data$delta_value >= 0, "gain", "loss"),
        col = if_else(.data$delta_value >= 0, "#10b981", "#ef4444"),
        hover_text = sprintf(
          paste0(
            "<b>%s</b><br>",
            "Total Picks: %.1f<br>",
            "Current: %.1f EPV<br>",
            "3-2-1: %.1f EPV<br>",
            "Change: %+.1f EPV<br>",
            "90%% CI: [%.1f, %.1f]"
          ),
          .data$team, .data$n_picks, .data$current_mean, .data$new_mean, .data$delta_value, .data$lo, .data$hi
        ),
        order_key = if (identical(sort_by, "total")) .data$new_mean else .data$delta_value
      ) %>%
      arrange(.data$order_key) %>%
      mutate(team_factor = factor(.data$team, levels = .data$team), y = seq_len(n()))

    p <- plot_ly(source = "wl_plot")
    logo_images <- team_logo_layout_images_app(
      df %>% transmute(team, x = current_mean, y = y),
      x_col = "x", y_col = "y",
      sizex = max(2.8, diff(range(c(df$current_mean, df$new_mean), na.rm = TRUE)) * 0.024),
      sizey = 0.90,
      opacity = 0.98
    )

    for (i in seq_len(nrow(df))) {
      row_i <- df[i, , drop = FALSE]
      # CI band: hover enabled with same team hover.
      p <- p %>% add_segments(
        data = row_i,
        x = ~lo, xend = ~hi, y = ~y, yend = ~y,
        line = list(color = "rgba(255,255,255,0.18)", width = 6),
        hovertemplate = paste0(row_i$hover_text[1], "<extra></extra>"),
        hoverlabel = list(bgcolor = row_i$col[1], font = list(color = "white"), align = "right"),
        showlegend = FALSE
      )
      # Connector from current logo to 3-2-1 dot.
      p <- p %>% add_segments(
        data = row_i,
        x = ~current_mean, xend = ~new_mean, y = ~y, yend = ~y,
        line = list(color = row_i$col[1], width = 2), opacity = 0.55,
        hovertemplate = paste0(row_i$hover_text[1], "<extra></extra>"),
        hoverlabel = list(bgcolor = row_i$col[1], font = list(color = "white"), align = "right"),
        showlegend = FALSE
      )
      # Transparent hit target for the current logo.
      p <- p %>% add_markers(
        data = row_i,
        x = ~current_mean, y = ~y, customdata = ~team,
        marker = list(color = "rgba(255,255,255,0.01)", size = 26, line = list(color = "rgba(255,255,255,0)", width = 0)),
        hovertemplate = paste0(row_i$hover_text[1], "<extra></extra>"),
        hoverlabel = list(bgcolor = row_i$col[1], font = list(color = "white"), align = "right"),
        showlegend = FALSE, opacity = 0.01
      )
      # 3-2-1 endpoint.
      p <- p %>% add_markers(
        data = row_i,
        x = ~new_mean, y = ~y, customdata = ~team,
        marker = list(color = row_i$col[1], size = 10.5, opacity = 0.95, line = list(color = "#0a0a14", width = 1.3)),
        hovertemplate = paste0(row_i$hover_text[1], "<extra></extra>"),
        hoverlabel = list(bgcolor = row_i$col[1], font = list(color = "white"), align = "right"),
        showlegend = FALSE
      )
    }
    rng <- range(c(df$lo, df$hi, df$current_mean, df$new_mean), na.rm = TRUE)
    pad <- max(1, diff(rng) * 0.06)

    p %>%
      plotly_dark(
        xaxis = list(title = "Total EPV", range = c(rng[1] - pad, rng[2] + pad)),
        yaxis = list(title = "", tickmode = "array", tickvals = df$y, ticktext = as.character(df$team), range = c(0.5, nrow(df) + 0.5), autorange = FALSE),
        images = logo_images,
        margin = list(l = 70, r = 30, t = 70, b = 60),
        showlegend = FALSE
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

  # ---- Team Summaries ----
  full_table_source <- reactive({
    base <- summary_ev_df %>% select(team, tier, wins, losses)
    portfolio_quality_quantity_summary_app("ev") %>%
      transmute(
        team,
        n_picks_mean = .data$new_expected_picks,
        current_mean = .data$cur_total_value,
        new_mean = .data$new_total_value,
        delta_value = .data$delta_total_value,
        delta_pct = (.data$new_total_value / pmax(.data$cur_total_value, 0.01) - 1) * 100
      ) %>%
      left_join(base, by = "team")
  })

  output$full_table_title <- renderText({
    "All 30 Teams — Click a row for detail"
  })

  output$full_table <- renderDT({
    base <- full_table_source() %>% arrange(.data$wins)
    delta_pct_html <- vapply(seq_len(nrow(base)), function(i) {
      col <- delta_color_app(base$delta_pct[i])
      sprintf("<span style='color:%s; white-space:nowrap;'>%s</span>", col, fmt_delta_app(base$delta_pct[i], suffix = "%"))
    }, character(1))

    delta_epv_html <- vapply(seq_len(nrow(base)), function(i) {
      col <- delta_color_app(base$delta_value[i])
      sprintf("<span style='color:%s; white-space:nowrap;'>%s</span>", col, fmt_delta1_app(base$delta_value[i]))
    }, character(1))

    tbl <- base %>%
      transmute(
        Team = team_logo_html_app(.data$team, size = 22, show_abbr = TRUE),
        Tier = tier_short[.data$tier],
        Record = sprintf("%d-%d", .data$wins, .data$losses),
        `#Pk` = fmt_num1_app(.data$n_picks_mean),
        `Current EPV` = fmt_num1_app(.data$current_mean),
        `3-2-1 EPV` = fmt_num1_app(.data$new_mean),
        "\u0394 EPV" := delta_epv_html,
        "\u0394 %" := delta_pct_html
      )

    datatable(
      tbl, selection = "single", rownames = FALSE, escape = FALSE,
      options = list(
        pageLength = 30, paging = FALSE, dom = "t", ordering = TRUE,
        scrollX = FALSE,
        columnDefs = list(
          list(className = "dt-right", targets = c(3, 4, 5, 6, 7))
        )
      )
    )
  }, server = FALSE)

  output$team_detail <- renderUI({
    sel <- input$full_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    t <- full_table_source() %>% arrange(.data$wins) %>% slice(sel)
    mode <- "ev"
    unit <- "EPV"

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

    impact_tbl <- pick_impact_rows_app(mode) %>%
      filter(.data$owner == .env$t$team) %>%
      mutate(
        delta = coalesce(.data$delta, .data$new_mean - .data$cur_mean),
        expected_pick_count = pmax(coalesce(.data$cur_expected_pick_count, 0), coalesce(.data$new_expected_pick_count, 0))
      ) %>%
      group_by(.data$impact_bucket, .data$round) %>%
      summarise(
        n_picks = sum(.data$expected_pick_count, na.rm = TRUE),
        current = sum(.data$cur_mean, na.rm = TRUE),
        new = sum(.data$new_mean, na.rm = TRUE),
        delta = sum(.data$delta, na.rm = TRUE),
        delta_pct = ifelse(abs(current) > 1e-9, delta / current * 100, NA_real_),
        .groups = "drop"
      ) %>%
      complete(
        impact_bucket = c("Own picks", "Incoming outright picks", "Swaps / protections"),
        round = c(1L, 2L),
        fill = list(n_picks = 0, current = 0, new = 0, delta = 0, delta_pct = NA_real_)
      ) %>%
      arrange(factor(.data$impact_bucket, levels = c("Own picks", "Incoming outright picks", "Swaps / protections")), .data$round)

    html_tbl <- function(tbl, first_header, empty_msg = "No picks", first_color = "#aaa") {
      if (nrow(tbl) == 0) return(tags$div(style = "color:#777; font-size:12px;", empty_msg))
      tbl <- tbl %>% mutate(delta_pct = ifelse(abs(.data$cur_mean) > 1e-9, .data$delta / .data$cur_mean * 100, NA_real_))
      tags$table(style = "width:100%; border-collapse:collapse; font-size:12px; table-layout:fixed;",
        tags$thead(tags$tr(
          tags$th(style = sprintf("text-align:left; color:%s !important; font-weight:800; padding:4px; width:48%%;", first_color), first_header),
          tags$th(style = "text-align:right; color:#aaa; padding:4px; width:13%;", "Cur"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px; width:13%;", "3-2-1"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px; width:13%;", paste0("Δ ", unit)),
          tags$th(style = "text-align:right; color:#aaa; padding:4px; width:13%;", paste0("Δ ", unit, " %"))
        )),
        tags$tbody(lapply(seq_len(nrow(tbl)), function(i) {
          r <- tbl[i, ]
          pct_txt <- ifelse(is.na(r$delta_pct), "—", sprintf("%+.1f%%", r$delta_pct))
          tags$tr(
            tags$td(style = "padding:4px; overflow-wrap:anywhere;", sprintf("%s %s", r$year, r$short_label)),
            tags$td(style = "padding:4px; text-align:right;", sprintf("%.1f", r$cur_mean)),
            tags$td(style = "padding:4px; text-align:right;", sprintf("%.1f", r$new_mean)),
            tags$td(style = sprintf("padding:4px; text-align:right; color:%s;", delta_color_app(r$delta)), sprintf("%+.1f", r$delta)),
            tags$td(style = sprintf("padding:4px; text-align:right; color:%s;", delta_color_app(r$delta_pct)), pct_txt)
          )
        }))
      )
    }

    breakout_tbl_round <- function(tbl, round_no) {
      tbl <- tbl %>% filter(.data$round == .env$round_no)
      total <- tibble(
        impact_bucket = "Total",
        round = round_no,
        n_picks = sum(tbl$n_picks, na.rm = TRUE),
        current = sum(tbl$current, na.rm = TRUE),
        new = sum(tbl$new, na.rm = TRUE),
        delta = sum(tbl$delta, na.rm = TRUE),
        delta_pct = ifelse(abs(sum(tbl$current, na.rm = TRUE)) > 1e-9,
                           sum(tbl$delta, na.rm = TRUE) / sum(tbl$current, na.rm = TRUE) * 100,
                           NA_real_)
      )
      tbl2 <- bind_rows(tbl, total)
      tags$table(style = "width:100%; border-collapse:collapse; font-size:12px; margin-top:1px; table-layout:fixed;",
        tags$thead(tags$tr(
          tags$th(style = "text-align:left; color:#aaa; font-weight:800; padding:4px; width:36%;", paste0("Round ", round_no)),
          tags$th(style = "text-align:right; color:#aaa; padding:4px; width:13%;", "# Picks"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px; width:13%;", "Cur"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px; width:13%;", "3-2-1"),
          tags$th(style = "text-align:right; color:#aaa; padding:4px; width:13%;", paste0("Δ ", unit)),
          tags$th(style = "text-align:right; color:#aaa; padding:4px; width:12%;", paste0("Δ ", unit, " %"))
        )),
        tags$tbody(lapply(seq_len(nrow(tbl2)), function(i) {
          r <- tbl2[i, ]
          is_total <- identical(as.character(r$impact_bucket), "Total")
          pct_txt <- ifelse(is.na(r$delta_pct), "—", sprintf("%+.1f%%", r$delta_pct))
          zero_row <- isTRUE(abs(r$n_picks) < 1e-9)
          delta_txt <- ifelse(zero_row, "—", sprintf("%+.1f", r$delta))
          delta_pct_txt <- ifelse(zero_row || is.na(r$delta_pct), "—", pct_txt)
          row_style <- if (is_total) "font-weight:800; border-top:1px solid #303044;" else ""
          tags$tr(style = row_style,
            tags$td(style = "padding:4px; overflow-wrap:anywhere;", r$impact_bucket),
            tags$td(style = "padding:4px; text-align:right;", ifelse(abs(r$n_picks - round(r$n_picks)) < 0.05, sprintf("%d", as.integer(round(r$n_picks))), sprintf("%.1f", r$n_picks))),
            tags$td(style = "padding:4px; text-align:right;", sprintf("%.1f", r$current)),
            tags$td(style = "padding:4px; text-align:right;", sprintf("%.1f", r$new)),
            tags$td(style = sprintf("padding:4px; text-align:right; color:%s;", ifelse(zero_row, "#d0d0d0", delta_color_app(r$delta))), delta_txt),
            tags$td(style = sprintf("padding:4px; text-align:right; color:%s;", ifelse(zero_row, "#d0d0d0", delta_color_app(r$delta_pct))), delta_pct_txt)
          )
        }))
      )
    }

    detail_primary_col <- team_primary_color_app(t$team)
    detail_secondary_col <- team_secondary_color_app(t$team)

    card(class = "team-detail-card",
      card_header(style = sprintf("border:1px solid %s; border-left:3px solid %s; padding-top:5px; padding-bottom:8px; background:rgba(15,15,26,0.72);", detail_primary_col, detail_secondary_col),
      #card_header(style = sprintf("border:1px solid %s; border-left:3px solid %s; padding-top:8px; padding-bottom:8px; background:rgba(15,15,26,0.72);", detail_primary_col, detail_secondary_col),
        team_header_tag_app(t$team, size = 34, show_full = TRUE)
      ),
      #card_body(style = "font-size:12px; color:#bbb; line-height:1.45; overflow-x:hidden; padding-top:10px;",
      card_body(style = "font-size:12px; color:#bbb; line-height:1.45; overflow-x:hidden; padding-top:10px;",
        layout_columns(
          col_widths = c(12),
          gap = "8px",
          #gap = "8px",
          #tags$div(style = "margin-bottom:16px;",
          tags$div(style = "margin-bottom:16px;",
            html_tbl(pos_tbl, "Most positive pick impacts", "No positive pick impacts", "#10b981")
          ),
          tags$div(style = "margin-top:4px;",
          #tags$div(style = "margin-top:6px;",
            html_tbl(neg_tbl, "Most negative pick impacts", "No negative pick impacts", "#ef4444")
          )
        ),
        #tags$hr(style = "border-color:#1a1a2a; margin:4px 0 3px 0;"),
        tags$hr(style = "border-color:#1a1a2a; margin:4px 0 3px 0;"),
        tags$div(style = "font-weight:700; color:#d0d0d0; margin-top:0; margin-bottom:0;", "Impact by pick type and round"),
        layout_columns(
          col_widths = c(12),
          gap = "1px",
          #gap = "8px",
          tags$div(style = "margin-top:-6px; margin-bottom:8px;", breakout_tbl_round(impact_tbl, 1L)),
          #tags$div(style = "margin-top:-6px; margin-bottom:16px;", breakout_tbl_round(impact_tbl, 1L)),
          tags$div(style = "margin-top:6px; margin-bottom:0;", breakout_tbl_round(impact_tbl, 2L))
        )
      )
    )
  })

  # ---- Methodology transition heatmap: tier matrix or full 30-rank matrix ----
  output$methodology_transition_heatmap <- renderPlotly({
    matrix_type <- as.character(input$methodology_matrix_type %||% "tier")

    if (identical(matrix_type, "rank")) {
      m <- rank_display_matrix(rank_trans_mat)
      if (is.null(m)) {
        return(
          plot_ly() %>%
            plotly_dark(
              annotations = list(list(
                text = "A valid 30 x 30 rank transition matrix was not found in dashboard_data.rds.",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(color = "#d1d5db", size = 14)
              )),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }

      rank_names <- sprintf("Rank %02d", 1:30)
      rank_labels <- sprintf("%02d", 1:30)
      z <- round(100 * m[rank_names, rank_names, drop = FALSE], 2)
      dimnames(z) <- list(rank_labels, rank_labels)
      hover_txt <- outer(
        rank_labels,
        rank_labels,
        Vectorize(function(from_rank, to_rank) {
          sprintf(
            "From Rank %s<br>To Rank %s<br>Probability: %.2f%%",
            from_rank, to_rank, z[from_rank, to_rank]
          )
        })
      )

      return(
        plot_ly(
          x = rank_labels,
          y = rank_labels,
          z = z,
          type = "heatmap",
          colorscale = list(
            c(0.00, "#0f0f1a"),
            c(0.35, "#172554"),
            c(0.70, "#4c1d95"),
            c(1.00, "#a78bfa")
          ),
          text = hover_txt,
          hovertemplate = "%{text}<extra></extra>",
          showscale = TRUE
        ) %>%
          plotly_dark(
            xaxis = list(
              title = "Year t + 1 Rank",
              side = "bottom",
              tickangle = -45,
              tickmode = "array",
              tickvals = rank_labels,
              ticktext = rank_labels,
              categoryorder = "array",
              categoryarray = rank_labels,
              automargin = TRUE
            ),
            yaxis = list(
              title = "Year t Rank",
              tickmode = "array",
              tickvals = rank_labels,
              ticktext = rank_labels,
              categoryorder = "array",
              categoryarray = rank_labels,
              autorange = "reversed",
              automargin = TRUE
            ),
            margin = list(l = 75, r = 45, t = 18, b = 82)
          )
      )
    }

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
      hovertemplate = "From %{y} <br>To %{x} <br>Probability: %{z:.1f}%<extra></extra>"
    ) %>%
      plotly_dark(
        xaxis = list(title = list(text = "Year t + 1", standoff = 24), side = "bottom", automargin = TRUE),
        yaxis = list(title = list(text = "Year t", standoff = 42), autorange = "reversed", automargin = TRUE),
        margin = list(l = 95, r = 30, t = 28, b = 75)
      )
  })

  # ---- Seven-year rank horizon trajectory ----
  output$rank_horizon_plot <- renderPlotly({
    interval_choice <- input$rank_horizon_interval %||% "10_90"
    interval_label <- rank_horizon_interval_label(interval_choice)
    selected_rank <- as.character(input$rank_horizon_start_rank %||% "15")

    if (identical(selected_rank, "all")) {
      tbl <- rank_horizon_tbl_all(rank_trans_mat, 7L, interval_choice)
    } else {
      tbl <- rank_horizon_tbl_one(rank_trans_mat, selected_rank, 7L, interval_choice)
    }

    if (nrow(tbl) == 0) {
      return(
        plot_ly() %>%
          plotly_dark(
            annotations = list(list(
              text = "A valid 30 x 30 rank transition matrix was not found in dashboard_data.rds.",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(color = "#d1d5db", size = 14)
            )),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }

    if (identical(selected_rank, "all")) {
      tbl <- tbl %>% mutate(start_label = sprintf("Rank %02d", .data$start_rank))
      p <- plot_ly()
      for (sr in seq_len(30)) {
        d <- tbl %>% filter(.data$start_rank == sr)
        rank_col <- rainbow(30, start = 0.58, end = 0.96)[sr]
        p <- p %>%
          add_lines(
            data = d,
            x = ~years_ahead,
            y = ~mean_rank,
            name = sprintf("%02d", sr),
            text = ~hover,
            hovertemplate = "%{text}<extra></extra>",
            line = list(color = rank_col, width = 1.45),
            showlegend = FALSE
          )
      }
      title_text <- sprintf("All Starting Ranks: Next Seven Seasons (%s interval in hover)", interval_label)
    } else {
      start_rank <- as.integer(selected_rank)
      if (is.na(start_rank)) start_rank <- 15L
      p <- plot_ly(tbl, x = ~years_ahead) %>%
        add_ribbons(
          ymin = ~qlo,
          ymax = ~qhi,
          name = interval_label,
          fillcolor = "rgba(96, 165, 250, 0.18)",
          line = list(color = "rgba(96, 165, 250, 0)"),
          hoverinfo = "skip"
        ) %>%
        add_lines(
          y = ~mean_rank,
          name = "Expected rank",
          line = list(color = "#60a5fa", width = 3),
          text = ~hover,
          hovertemplate = "%{text}<extra></extra>"
        ) %>%
        add_markers(
          y = ~mean_rank,
          name = "Expected rank",
          marker = list(size = 5, color = "#60a5fa"),
          text = ~hover,
          hovertemplate = "%{text}<extra></extra>",
          showlegend = FALSE
        )
      title_text <- sprintf("Start Rank %02d: Next Seven Seasons", start_rank)
    }

    p %>%
      plotly_dark(
        title = list(
          text = title_text,
          font = list(color = "#e5e7eb", size = 13)
        ),
        xaxis = list(
          title = "Years Ahead",
          tickmode = "linear",
          dtick = 1,
          range = c(0, 7),
          automargin = TRUE
        ),
        yaxis = list(
          title = "League Rank (1 = best)",
          range = c(30.5, 0.5),
          tickmode = "array",
          tickvals = 1:30,
          ticktext = sprintf("%02d", 1:30),
          automargin = TRUE
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.24,
          yanchor = "top"
        ),
        margin = list(l = 70, r = 20, t = 38, b = 70)
      )
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
    pm <- stan_diag$pick_model %||% list()
    pm2 <- stan_diag$pick2_model %||% NULL
    mk <- stan_diag$markov_model %||% list()

    fmt_metric <- function(x, digits = 3) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) return("—")
      x <- suppressWarnings(as.numeric(x[[1]]))
      if (!is.finite(x)) return("—")
      sprintf(paste0("%.", digits, "f"), x)
    }
    pass_badge <- function(pass) {
      if (is.na(pass)) return(tags$span(style = "color:#9ca3af; font-weight:800;", "INFO"))
      if (isTRUE(pass)) tags$span(style = "color:#10b981; font-weight:900;", "PASS")
      else tags$span(style = "color:#ef4444; font-weight:900;", "CHECK")
    }


    validation_model_display_name_app <- function(model) {
      src <- stringr::str_squish(as.character(model %||% ""))
      dplyr::case_when(
        src %in% c("picks_Round1.stan", "picks_Round1", "pick_value_v3_r1", "pick_value_v3", "Round 1 pick value") ~ "picks_Round1.stan",
        src %in% c("picks_Round2.stan", "picks_Round2", "pick_play_r2_v6_declining_upside", "Round 2 hurdle") ~ "picks_Round2.stan",
        stringr::str_detect(src, "team_strength_v3") ~ "team_strength_v3.stan",
        src %in% c("team_strength", "team_strength_v2", "Team-strength Markov") ~ "team_strength_v3.stan",
        TRUE ~ src
      )
    }

    loo_row_from_summary <- function(model_name, lsum) {
      if (is.null(lsum) || !is.data.frame(lsum) || nrow(lsum) == 0L) return(NULL)
      lsum <- as_tibble(lsum)
      max_k <- if ("max_pareto_k" %in% names(lsum)) suppressWarnings(as.numeric(lsum$max_pareto_k[1])) else NA_real_
      elpd <- if ("elpd_loo" %in% names(lsum)) suppressWarnings(as.numeric(lsum$elpd_loo[1])) else NA_real_
      p_loo <- if ("p_loo" %in% names(lsum)) suppressWarnings(as.numeric(lsum$p_loo[1])) else NA_real_
      tibble(
        model = model_name,
        check = "PSIS-LOO / Pareto-k",
        metric = sprintf("elpd_loo %s; p_loo %s; max Pareto-k %s",
                         ifelse(is.finite(elpd), sprintf("%.1f", elpd), "—"),
                         ifelse(is.finite(p_loo), sprintf("%.1f", p_loo), "—"),
                         ifelse(is.finite(max_k), sprintf("%.2f", max_k), "—")),
        pass = ifelse(is.finite(max_k), max_k <= 0.70, NA)
      )
    }

    fallback_tbl <- bind_rows(
      tibble(
        model = "picks_Round1.stan",
        check = "R-hat / ESS",
        metric = sprintf("max R-hat %s; min bulk-ESS %s",
                         fmt_metric(pm$max_rhat, 3),
                         ifelse(is.null(pm$min_ess), "—", format(pm$min_ess, big.mark = ","))),
        pass = ifelse(is.null(pm$max_rhat), NA, pm$max_rhat <= 1.01)
      ),
      tibble(
        model = "picks_Round1.stan",
        check = "Posterior predictive coverage",
        metric = sprintf("90%% %s coverage %s%%",
                         pm$ppc_level %||% "player-row",
                         ifelse(is.null(pm$ppc_cover), "—", sprintf("%.0f", 100 * pm$ppc_cover))),
        pass = ifelse(is.null(pm$ppc_cover), NA, pm$ppc_cover >= 0.70)
      ),
      loo_row_from_summary("picks_Round1.stan", pm$loo_summary),
      if (!is.null(pm2)) tibble(
        model = "picks_Round2.stan",
        check = "R-hat / ESS",
        metric = sprintf("max R-hat %s; min bulk-ESS %s",
                         fmt_metric(pm2$max_rhat, 3),
                         ifelse(is.null(pm2$min_ess), "—", format(pm2$min_ess, big.mark = ","))),
        pass = ifelse(is.null(pm2$max_rhat), NA, pm2$max_rhat <= 1.01)
      ),
      if (!is.null(pm2)) tibble(
        model = "picks_Round2.stan",
        check = "Posterior predictive / play rate",
        metric = sprintf("90%% PPC %s%%; empirical play %.1f%%; P(play) #31/#45/#60 %.1f%% / %.1f%% / %.1f%%",
                         ifelse(is.null(pm2$ppc_cover), "—", sprintf("%.0f", 100 * pm2$ppc_cover)),
                         100 * (pm2$played_rate %||% NA_real_),
                         100 * (pm2$p_play_31 %||% NA_real_),
                         100 * (pm2$p_play_45 %||% NA_real_),
                         100 * (pm2$p_play_60 %||% NA_real_)),
        pass = ifelse(is.null(pm2$ppc_cover), NA, pm2$ppc_cover >= 0.70)
      ),
      if (!is.null(pm2)) loo_row_from_summary("picks_Round2.stan", pm2$loo_summary),
      tibble(
        model = "team_strength_v3.stan",
        check = "Transition-matrix code check",
        metric = sprintf("%s transitions over %s seasons; Stan vs closed-form max diff %s; mixing time %s seasons",
                         ifelse(is.null(mk$n_transitions), "—", format(mk$n_transitions, big.mark = ",")),
                         ifelse(is.null(mk$n_seasons), "—", mk$n_seasons),
                         fmt_metric(mk$max_abs_diff, 4),
                         fmt_metric(mk$mixing_time, 1)),
        pass = ifelse(is.null(mk$max_abs_diff), NA, mk$max_abs_diff < 0.01)
      ),
      NULL
    )

    why_for_check_app <- function(check) {
      chk <- stringr::str_to_lower(as.character(check))
      dplyr::case_when(
        stringr::str_detect(chk, "r-hat|ess") ~ "Confirms the posterior draws mixed reliably, so the reported means and intervals are not chain artifacts.",
        stringr::str_detect(chk, "nuts|geometry|divergence|tree") ~ "Checks whether Stan explored the posterior without pathological geometry; divergences or treedepth hits would undermine trust in the draws.",
        stringr::str_detect(chk, "posterior predictive|ppc|play rate") ~ "Compares simulated draft outcomes to historical outcomes; good coverage means the model's uncertainty is realistic, not just the average curve.",
        stringr::str_detect(chk, "loo|pareto") ~ "Tests out-of-sample reliability and whether a few extreme players dominate the fit; acceptable Pareto-k makes the comparison credible.",
        stringr::str_detect(chk, "sbc|rank") ~ "Uses simulated data where the truth is known to verify that Bayesian credible intervals are calibrated.",
        stringr::str_detect(chk, "transition|markov") ~ "Verifies the tier-transition engine against the conjugate closed-form check, which supports the future team-path simulations.",
        TRUE ~ "Provides an additional guardrail that the model is stable enough for portfolio EPV comparisons."
      )
    }

    dashboard_diag_available <- is.list(stan_diag) && length(stan_diag) > 0L &&
      (!is.null(stan_diag$pick_model) || !is.null(stan_diag$pick2_model) || !is.null(stan_diag$markov_model))
    csv_diag_available <- !is.null(validation_decision_tbl) && nrow(validation_decision_tbl) > 0L &&
      all(c("model", "check", "metric", "pass") %in% names(validation_decision_tbl))

    # Default to the validation metrics bundled inside dashboard_data.rds so the
    # app reflects the exact model stack used to build the current dashboard.
    # The CSV written by 05_model_validation_rank_v3.R is retained only as a
    # fallback for standalone validation-review sessions where the RDS has no
    # embedded diagnostics.
    validation_source_app <- if (dashboard_diag_available) "dashboard_data.rds" else if (csv_diag_available) "validation_csv" else "dashboard_data.rds_empty"
    diag_tbl <- if (dashboard_diag_available) {
      fallback_tbl
    } else if (csv_diag_available) {
      validation_decision_tbl %>% mutate(pass = as.logical(.data$pass))
    } else {
      fallback_tbl
    }
    if (!"why" %in% names(diag_tbl)) diag_tbl$why <- NA_character_
    diag_tbl <- diag_tbl %>%
      mutate(
        source_model = as.character(.data$model),
        model = validation_model_display_name_app(.data$model),
        why = coalesce(as.character(.data$why), why_for_check_app(.data$check)),
        model_order = dplyr::case_when(
          .data$model == "picks_Round1.stan" ~ 1L,
          .data$model == "picks_Round2.stan" ~ 2L,
          .data$model == "team_strength_v3.stan" ~ 3L,
          TRUE ~ 99L
        ),
        check_order = dplyr::case_when(
          stringr::str_detect(stringr::str_to_lower(.data$check), "r-hat|rhat|ess") ~ 1L,
          stringr::str_detect(stringr::str_to_lower(.data$check), "nuts|geometry|divergence|tree") ~ 2L,
          stringr::str_detect(stringr::str_to_lower(.data$check), "posterior predictive|ppc|play rate") ~ 3L,
          stringr::str_detect(stringr::str_to_lower(.data$check), "loo|pareto") ~ 4L,
          stringr::str_detect(stringr::str_to_lower(.data$check), "sbc|rank uniformity") ~ 5L,
          stringr::str_detect(stringr::str_to_lower(.data$check), "transition|markov") ~ 6L,
          TRUE ~ 50L
        )
      ) %>%
      arrange(.data$model_order, .data$model, .data$check_order, .data$check) %>%
      select(-model_order, -check_order)

    table_tag <- tags$table(
      style = "width:100%; border-collapse:collapse; font-size:12px; table-layout:fixed;",
      tags$thead(tags$tr(
        tags$th(style = "text-align:left; color:#cfd2dc; padding:7px; width:16%; border-bottom:1px solid #1a1a2a;", "Model"),
        tags$th(style = "text-align:left; color:#cfd2dc; padding:7px; width:15%; border-bottom:1px solid #1a1a2a;", "Check"),
        tags$th(style = "text-align:left; color:#cfd2dc; padding:7px; width:35%; border-bottom:1px solid #1a1a2a;", "Metric"),
        tags$th(style = "text-align:left; color:#cfd2dc; padding:7px; width:26%; border-bottom:1px solid #1a1a2a;", "Use Case"),
        tags$th(style = "text-align:center; color:#cfd2dc; padding:7px; width:8%; border-bottom:1px solid #1a1a2a;", "Status")
      )),
      tags$tbody(lapply(seq_len(nrow(diag_tbl)), function(i) {
        r <- diag_tbl[i, ]
        tags$tr(
          tags$td(style = "padding:7px; border-bottom:1px solid rgba(255,255,255,0.035); color:#e6e8ef; overflow-wrap:anywhere;", as.character(r$model)),
          tags$td(style = "padding:7px; border-bottom:1px solid rgba(255,255,255,0.035); color:#d7d8e2; overflow-wrap:anywhere;", as.character(r$check)),
          tags$td(style = "padding:7px; border-bottom:1px solid rgba(255,255,255,0.035); color:#b8bcc9; overflow-wrap:anywhere;", as.character(r$metric)),
          tags$td(style = "padding:7px; border-bottom:1px solid rgba(255,255,255,0.035); color:#b8bcc9; overflow-wrap:anywhere;", as.character(r$why)),
          tags$td(style = "padding:7px; text-align:center; border-bottom:1px solid rgba(255,255,255,0.035);", pass_badge(r$pass))
        )
      }))
    )

    tags$div(style = "font-size:12px; color:#bbb; line-height:1.65;",
      table_tag,
      tags$div(style = "margin-top:10px; color:#888;",
        if (identical(validation_source_app, "dashboard_data.rds")) {
          "Loaded validation metrics from dashboard_data.rds. Rebuild dashboard_data.rds after model changes to refresh these metrics."
        } else if (identical(validation_source_app, "validation_csv")) {
          sprintf("dashboard_data.rds did not include embedded diagnostics, so this table is using fallback validation CSV: %s.", validation_decision_path)
        } else {
          "dashboard_data.rds did not include embedded diagnostics. Rebuild the pipeline to populate dd$stan_diagnostics."
        }
      )
    )
  })

  # ==========================================================================
  # TAB 6: SINGLE PICK VALUATION
  # ==========================================================================

  # populate team and pick choices for the chosen year / team
  observeEvent(input$sp_year, {
    cur_team <- input$sp_team %||% all_team_abbr[1]
    if (!cur_team %in% all_team_abbr) cur_team <- all_team_abbr[1]
    updateSelectizeInput(session, "sp_team", choices = team_select_choices_app(all_team_abbr), selected = cur_team, server = TRUE)
  }, ignoreNULL = FALSE)

  observe({
    req(input$sp_year, input$sp_team)
    opts <- pick_display_assets %>%
      filter(.data$year == as.integer(input$sp_year), .data$owner == input$sp_team) %>%
      mutate(.slot_sort = suppressWarnings(as.integer(stringr::str_extract(.data$fixed_slot_display %||% NA_character_, "[0-9]+")))) %>%
      arrange(.data$round, coalesce(.data$.slot_sort, 999L), .data$short_label)
    if (nrow(opts) == 0L) {
      updateSelectInput(session, "sp_asset", choices = character(0), selected = character(0))
    } else {
      current <- input$sp_asset
      selected <- if (!is.null(current) && current %in% opts$display_asset_id) current else opts$display_asset_id[1]
      choice_vec <- setNames(opts$display_asset_id, opts$short_label)
      updateSelectInput(session, "sp_asset", choices = choice_vec, selected = selected)
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
      "flex:0 0 182px; width:182px; max-width:182px; min-width:182px; padding:10px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;", col),
      tags$div(style = "font-size:14px; color:#aaa; white-space:nowrap;", title),
      tags$div(style = sprintf("font-size:25px; font-weight:700; color:%s; line-height:1.1; margin-top:3px;", col),
               sprintf("%.1f%%", 100 * stat$prob)),
      tags$div(style = "font-size:13px; color:#c5c8d2; white-space:nowrap; margin-top:5px;",
               sprintf("90%% CI [%.1f%%, %.1f%%]", 100 * stat$q05, 100 * stat$q95)))
  }

  probability_is_certain_app <- function(stat) {
    !is.null(stat) && nrow(stat) > 0 &&
      isTRUE(all.equal(as.numeric(stat$prob), 1, tolerance = 1e-12)) &&
      isTRUE(all.equal(as.numeric(stat$q05), 1, tolerance = 1e-12)) &&
      isTRUE(all.equal(as.numeric(stat$q95), 1, tolerance = 1e-12))
  }

  prob_card_row <- function(label, cur_stat, new_stat) {
    tags$div(style = "margin-top:10px;",
      tags$div(style = "font-size:14px; font-weight:700; color:#d0d0d0; margin-bottom:6px;", label),
      tags$div(class = "sp-prob-card-row",
        prob_box("Current", cur_stat, "#3b82f6"),
        prob_box("3-2-1", new_stat, "#f59e0b")
      )
    )
  }

  guaranteed_swap_entitlement <- function(r) {
    isTRUE(any(is_guaranteed_display_entitlement_app(r)))
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
    team_detail_row <- function(label, value_ui) {
      tags$div(class = "sp-team-detail-row",
        tags$div(class = "sp-team-label", label),
        tags$div(class = "sp-team-value", value_ui)
      )
    }

    obligation_txt <- pick_obligation_display_app(r)
    obligation_txt <- stringr::str_replace_all(as.character(obligation_txt), " \\(conveys\\)", "")
    obligation_txt <- stringr::str_replace_all(obligation_txt, "Own pick, no obligations", "Own pick")
    show_conveyance_probability <- !(probability_is_certain_app(convey_cur) && probability_is_certain_app(convey_new))

    tags$div(style = "font-size:14px; color:#c5c8d2; line-height:1.65; max-width:100%; overflow-x:hidden;",
      tags$div(style = "margin-bottom:18px;", tags$strong(style = "color:#ffffff; font-size:21px;", "Pick Details")),
      team_detail_row(
        "Current Team:",
        tags$span(class = "sp-team-inline", team_logo_img_app(r$owner, size = 24), tags$span(r$owner))
      ),
      team_detail_row(
        sprintf("Original Team%s:", ifelse(str_detect(r$member_original_teams, ","), "s", "")),
        tagList(lapply(split_abbrs(r$member_original_teams), function(tm) {
          tags$span(class = "sp-team-inline", team_logo_img_app(tm, size = 24), tags$span(tm))
        }))
      ),
      if (!is.na(r$fixed_slot_display)) line_div(tags$span(style = "color:#10b981;", sprintf("Actual Pick: %s", r$fixed_slot_display))),
      team_detail_row("Obligation:", tags$span(style = "display:block;", obligation_txt)),
      if (isTRUE(show_conveyance_probability)) prob_card_row("Conveyance Probability", convey_cur, convey_new),
      if (!is.null(swap_cur) || !is.null(swap_new)) prob_card_row("Swap Exercise Probability", swap_cur, swap_new),
      if (r$year == 2026)
        line_div("Locked to the actual 2026 draft result", extra_style = "color:#10b981;"))
  })

  output$sp_headline <- renderUI({
    s_ev  <- sp_stats_ev()
    if (nrow(s_ev) == 0) return(NULL)

    box <- function(title, mean, q05, q95, col, subtitle = "90% interval") {
      tags$div(style = sprintf(
        "flex:1; padding:12px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;", col),
        tags$div(style = "font-size:11px; color:#888;", title),
        tags$div(style = sprintf("font-size:26px; font-weight:700; color:%s;", col),
                 sprintf("%.1f", mean)),
        tags$div(style = "font-size:11px; color:#aaa;",
                 sprintf("%s: [%.1f, %.1f]", subtitle, q05, q95)))
    }

    delta_box <- function(delta) {
      tags$div(style = sprintf(
        "flex:1; padding:12px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;",
        delta_color_app(delta)),
        tags$div(style = "font-size:11px; color:#888;", "Δ EPV"),
        tags$div(style = sprintf("font-size:26px; font-weight:700; color:%s;",
                                 delta_color_app(delta)),
                 sprintf("%+.1f", delta)),
        tags$div(style = "font-size:11px; color:#aaa;", "3-2-1 minus current"))
    }

    pct_box <- function(cur, new) {
      pct <- ifelse(abs(cur) > 1e-9, (new - cur) / cur * 100, NA_real_)
      col <- delta_color_app(pct)
      tags$div(style = sprintf(
        "flex:1; padding:12px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;", col),
        tags$div(style = "font-size:11px; color:#888;", "Δ EPV %"),
        tags$div(style = sprintf("font-size:26px; font-weight:700; color:%s;", col),
                 ifelse(is.na(pct), "—", sprintf("%+.1f%%", pct))),
        tags$div(style = "font-size:11px; color:#aaa;", "relative to current"))
    }

    delta <- s_ev$new_mean - s_ev$cur_mean
    tags$div(
      tags$div(style = "display:flex; gap:12px; align-items:stretch;",
        box("Current system", s_ev$cur_mean, s_ev$cur_q05, s_ev$cur_q95, "#3b82f6"),
        box("3-2-1 system",   s_ev$new_mean, s_ev$new_q05, s_ev$new_q95, "#f59e0b"),
        delta_box(delta),
        pct_box(s_ev$cur_mean, s_ev$new_mean)
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

  pick_movers_table <- function(value_mode, year_filter, round_filter, team_filter = "All") {
    year_filter <- if (is.null(year_filter) || length(year_filter) == 0L) "All" else as.character(year_filter[[1]])
    round_filter <- if (is.null(round_filter) || length(round_filter) == 0L) "All" else as.character(round_filter[[1]])
    team_filter <- if (is.null(team_filter) || length(team_filter) == 0L) "All" else as.character(team_filter[[1]])
    if (is.na(team_filter) || !nzchar(team_filter) || identical(team_filter, "All teams")) team_filter <- "All"

    rows <- pick_impact_rows_app("ev")
    if (!is.null(year_filter) && !identical(year_filter, "All")) {
      rows <- rows %>% filter(.data$year == as.integer(.env$year_filter))
    }
    if (!is.null(round_filter) && !identical(round_filter, "All")) {
      rows <- rows %>% filter(.data$round == as.integer(.env$round_filter))
    }
    if (!is.null(team_filter) && !identical(team_filter, "All")) {
      rows <- rows %>% filter(.data$owner == .env$team_filter)
    }

    if (is.null(rows) || nrow(rows) == 0L) {
      return(tibble(
        Owner = character(), Year = integer(), Round = integer(),
        Pick = character(), Current = character(), `3-2-1` = character(), `Delta EPV` = character(), `Delta EPV %` = character()
      ))
    }

    rows %>%
      mutate(
        Owner = team_logo_html_app(.data$owner, size = 20, show_abbr = TRUE),
        Pick = .data$short_label,
        Current = fmt_num1_app(.data$cur_mean),
        `3-2-1` = fmt_num1_app(.data$new_mean),
        delta_epv_num = round(.data$delta, 1),
        delta_epv_pct_num = round(ifelse(abs(.data$cur_mean) > 1e-9, .data$delta / .data$cur_mean * 100, NA_real_), 1),
        `Delta EPV` = sprintf("<span style='color:%s; white-space:nowrap;'>%s</span>", delta_color_app(.data$delta), fmt_delta1_app(.data$delta)),
        `Delta EPV %` = sprintf("<span style='color:%s; white-space:nowrap;'>%s</span>", delta_color_app(ifelse(abs(.data$cur_mean) > 1e-9, .data$delta / .data$cur_mean * 100, NA_real_)), fmt_delta1_app(ifelse(abs(.data$cur_mean) > 1e-9, .data$delta / .data$cur_mean * 100, NA_real_), suffix = "%"))
      ) %>%
      arrange(desc(.data$delta), .data$year, .data$round, .data$owner, .data$short_label) %>%
      transmute(
        Owner,
        Year = as.integer(.data$year),
        Round = .data$round,
        Pick,
        Current,
        `3-2-1`,
        `Delta EPV`,
        `Delta EPV %`
      ) %>%
      mutate(across(where(is.character), enc2utf8))
  }

  output$pm_ev_table <- renderDT({
    tbl <- pick_movers_table("ev", input$pm_year %||% "All", input$pm_round %||% "All", input$pm_team %||% "All")
    datatable(
      tbl,
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        paging = FALSE,
        dom = "t",
        ordering = TRUE,
        scrollX = FALSE,
        scrollY = "calc(100vh - 260px)",
        scrollCollapse = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "96px", targets = 0),
          list(width = "64px", targets = 1),
          list(width = "58px", targets = 2),
          list(width = "42%", targets = 3),
          list(width = "84px", targets = 4),
          list(width = "84px", targets = 5),
          list(width = "84px", targets = 6),
          list(width = "92px", targets = 7),
          list(className = "dt-right", targets = c(1, 2, 4, 5, 6, 7)),
          list(targets = 6, render = JS("function(data,type,row,meta){ var txt = $('<div>').html(data).text().replace('%',''); var x = parseFloat(txt); if(type === 'sort' || type === 'type') return isNaN(x) ? 0 : x; return data; }")),
          list(targets = 7, render = JS("function(data,type,row,meta){ var txt = $('<div>').html(data).text().replace('%',''); var x = parseFloat(txt); if(type === 'sort' || type === 'type') return isNaN(x) ? 0 : x; return data; }"))
        )
      )
    )
  }, server = FALSE)

  # ==========================================================================
  # TAB 7: TRADE MACHINE
  # ==========================================================================

  tm_identity_tag_app <- function(team) {
    tags$div(
      team_logo_img_app(team, size = 118),
      tags$div(class = "tm-team-name", team_full_name_app(team))
    )
  }

  output$tm_teamA_hdr <- renderUI(tm_identity_tag_app(input$tm_teamA %||% all_team_abbr[1]))
  output$tm_teamB_hdr <- renderUI(tm_identity_tag_app(input$tm_teamB %||% all_team_abbr[2]))

  output$tm_dynamic_css <- renderUI({
    teamA <- input$tm_teamA %||% all_team_abbr[1]
    teamB <- input$tm_teamB %||% all_team_abbr[2]
    colA <- team_secondary_color_app(teamA)
    colB <- team_secondary_color_app(teamB)
    rgbaA <- hex_to_rgba_app(colA, 0.26)
    rgbaB <- hex_to_rgba_app(colB, 0.26)
    glowA <- hex_to_rgba_app(colA, 0.36)
    glowB <- hex_to_rgba_app(colB, 0.36)

    tags$style(HTML(sprintf("
      #tm_teamA + .selectize-control .selectize-input,
      #tm_picksA + .selectize-control .selectize-input {
        --tm-side-color: %s;
        --tm-side-color-soft: %s;
      }
      #tm_teamB + .selectize-control .selectize-input,
      #tm_picksB + .selectize-control .selectize-input {
        --tm-side-color: %s;
        --tm-side-color-soft: %s;
      }
      #tm_teamA + .selectize-control .selectize-input.focus,
      #tm_teamA + .selectize-control .selectize-input.input-active,
      #tm_picksA + .selectize-control .selectize-input.focus,
      #tm_picksA + .selectize-control .selectize-input.input-active {
        border-color: %s !important;
        box-shadow: 0 0 0 0.22rem %s !important;
      }
      #tm_teamB + .selectize-control .selectize-input.focus,
      #tm_teamB + .selectize-control .selectize-input.input-active,
      #tm_picksB + .selectize-control .selectize-input.focus,
      #tm_picksB + .selectize-control .selectize-input.input-active {
        border-color: %s !important;
        box-shadow: 0 0 0 0.22rem %s !important;
      }
      #tm_picksA + .selectize-control.multi .selectize-input > div {
        background: %s !important;
        border: 1px solid %s !important;
      }
      #tm_picksB + .selectize-control.multi .selectize-input > div {
        background: %s !important;
        border: 1px solid %s !important;
      }
      #tm_teamA + .selectize-control .selectize-dropdown .active,
      #tm_picksA + .selectize-control .selectize-dropdown .active,
      #tm_teamA + .selectize-control .selectize-dropdown .selected,
      #tm_picksA + .selectize-control .selectize-dropdown .selected {
        background: %s !important;
      }
      #tm_teamB + .selectize-control .selectize-dropdown .active,
      #tm_picksB + .selectize-control .selectize-dropdown .active,
      #tm_teamB + .selectize-control .selectize-dropdown .selected,
      #tm_picksB + .selectize-control .selectize-dropdown .selected {
        background: %s !important;
      }
    ", colA, rgbaA, colB, rgbaB, colA, glowA, colB, glowB, rgbaA, colA, rgbaB, colB, rgbaA, rgbaB)))
  })

  # picks each team currently OWNS (can send out)
  picks_owned_by <- function(team) {
    pick_display_assets %>%
      filter(owner == team) %>%
      mutate(.slot_sort = suppressWarnings(as.integer(stringr::str_extract(.data$fixed_slot_display %||% NA_character_, "[0-9]+")))) %>%
      arrange(.data$year, .data$round, coalesce(.data$.slot_sort, 999L), .data$short_label)
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
    prot <- normalize_protection_for_round(input[[pid]] %||% "none", r$round)
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
    prot <- normalize_protection_for_round(input[[pid]] %||% "none", r$round)
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

  # Initial EPV is the value of the underlying original-team pick before
  # user-added protection / swap constraints. Final EPV is the value actually
  # sent to the receiver after the table toggles are applied.
  underlying_pick_initial_ev_value <- function(aid) {
    r <- pick_assets %>% filter(asset_id == aid)
    if (nrow(r) == 0) return(zero_trade_vec())

    if (r$year == 2026L && !is.na(r$fixed_slot)) {
      val <- slot_value_vec(rep(r$fixed_slot, nrow(asset_new_draws)))
      val[is.na(val)] <- 0
      return(val)
    }

    if (!aid %in% colnames(asset_slot_new_draws)) {
      return(underlying_pick_ev_base(aid))
    }

    og_slot <- asset_slot_new_draws[, aid]
    val <- slot_value_vec(og_slot)
    val[is.na(val)] <- 0
    val
  }

  sent_pick_convey_prob <- function(aid, side, receiver, ctrl_id = aid) {
    r <- pick_assets %>% filter(asset_id == aid)
    if (nrow(r) == 0) return(NA_real_)
    if (r$year == 2026L && !is.na(r$fixed_slot)) return(1)

    pid <- paste0("prot_", side, "_", safe_id(ctrl_id))
    sid <- paste0("swap_", side, "_", safe_id(ctrl_id))
    prot <- normalize_protection_for_round(input[[pid]] %||% "none", r$round)
    swap <- isTRUE(input[[sid]])

    if (!aid %in% colnames(asset_slot_new_draws)) return(NA_real_)
    og_slot <- asset_slot_new_draws[, aid]

    if (swap) {
      rc_slot <- receiver_slot_vec(receiver, r$year, r$round, "new")
      eligible <- !is.na(og_slot) & pick_conveys_app(og_slot, prot)
      exercised <- eligible & !is.na(rc_slot) & og_slot < rc_slot
      return(mean(exercised, na.rm = TRUE))
    }

    if (!is.null(prot) && prot != "none") {
      conveys <- !is.na(og_slot) & pick_conveys_app(og_slot, prot)
      return(mean(conveys, na.rm = TRUE))
    }

    if (aid %in% colnames(asset_convey_new_draws)) {
      return(mean(asset_convey_new_draws[, aid] > 0, na.rm = TRUE))
    }

    1
  }

  sent_display_initial_ev_value <- function(display_id, side, receiver) {
    ids <- display_member_ids(display_id)
    ids <- ids[ids %in% colnames(asset_new_draws)]
    if (length(ids) == 0) return(zero_trade_vec())

    if (length(ids) == 1L && display_is_single_leg(display_id)) {
      return(underlying_pick_initial_ev_value(ids[1]))
    }

    # Complex / grouped display entitlements already embed their as-is
    # protection/swap allocation. They are not editable with the simple toggles,
    # so initial and final EPV are the same in the outgoing table.
    sent_display_ev_value(display_id, side, receiver)
  }

  sent_display_convey_prob <- function(display_id, side, receiver) {
    ids <- display_member_ids(display_id)
    ids <- ids[ids %in% colnames(asset_new_draws)]
    if (length(ids) == 0) return(NA_real_)

    if (is_guaranteed_display_id_app(display_id) && display_id %in% colnames(display_pick_count_new_draws)) {
      return(mean(display_pick_count_new_draws[, display_id] > 0, na.rm = TRUE))
    }

    if (length(ids) == 1L && display_is_single_leg(display_id)) {
      return(sent_pick_convey_prob(ids[1], side, receiver, ctrl_id = display_id))
    }

    if (display_id %in% colnames(display_pick_count_new_draws)) {
      return(mean(display_pick_count_new_draws[, display_id] > 0, na.rm = TRUE))
    }

    if (display_id %in% colnames(display_convey_new_draws)) {
      return(mean(display_convey_new_draws[, display_id] > 0, na.rm = TRUE))
    }

    1
  }

  sent_display_epv_impact_value <- function(display_id, side, receiver) {
    sent_display_ev_value(display_id, side, receiver) -
      sent_display_initial_ev_value(display_id, side, receiver)
  }
  
  side_initial_ev_value <- function(ids, side, receiver) {
    if (is.null(ids) || length(ids) == 0) return(zero_trade_vec())
    cols <- vapply(ids, function(did) sent_display_initial_ev_value(did, side, receiver),
                   numeric(nrow(asset_new_draws)))
    if (is.null(dim(cols))) return(cols)
    rowSums(cols)
  }

  side_epv_impact_value <- function(ids, side, receiver) {
    if (is.null(ids) || length(ids) == 0) return(zero_trade_vec())
    side_out_ev_value(ids, side, receiver) - side_initial_ev_value(ids, side, receiver)
  }

  format_trade_pct <- function(x) {
    if (!is.finite(x)) return("—")
    sprintf("%.0f%%", 100 * pmin(pmax(x, 0), 1))
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
    convey_header <- sprintf("Convey to %s", receiver %||% "Other")

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
      prot_choices <- protection_choices_for_round(r$round)
      prot_selected <- normalize_protection_for_round(input[[pid]] %||% "none", r$round)

      convey_prob <- sent_display_convey_prob(did, side, receiver)
      initial_stat <- trade_pick_stats(sent_display_initial_ev_value(did, side, receiver))
      impact_stat  <- trade_pick_stats(sent_display_epv_impact_value(did, side, receiver))
      final_stat   <- trade_pick_stats(sent_display_ev_value(did, side, receiver))

      tags$tr(
        tags$td(style = "max-width:0; font-size:14px;", tags$strong(class = "tm-pick-label", r$trade_label)),
        tags$td(if (eligible) tags$div(class = "tm-protection-select", selectInput(pid, NULL, choices = prot_choices, selected = prot_selected, width = "100%")) else tags$span(style = "color:#777;", "N/A")),
        tags$td(style = "text-align:center;", if (eligible) checkboxInput(sid, NULL, value = isTRUE(input[[sid]])) else tags$input(type = "checkbox", disabled = "disabled")),
        tags$td(style = "text-align:right; font-size:14px;", format_trade_pct(convey_prob)),
        tags$td(style = "text-align:right; font-size:14px;", sprintf("%.1f", initial_stat["mean"])),
        tags$td(style = "text-align:right; font-size:14px;", sprintf("%.1f", impact_stat["mean"])),
        tags$td(style = "text-align:right; font-size:14px;", sprintf("%.1f", final_stat["mean"]))
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
          tags$td(style = "height:74px; padding:6px 0; border-bottom:0;", HTML("&nbsp;")),
          tags$td(style = "height:74px; padding:6px 0; border-bottom:0;", HTML("&nbsp;")),
          tags$td(style = "height:74px; padding:6px 0; border-bottom:0;", HTML("&nbsp;")),
          tags$td(style = "height:74px; padding:6px 0; border-bottom:0;", HTML("&nbsp;"))
        )
      })
      body_rows <- c(body_rows, spacer_rows)
    }

    total_initial <- trade_pick_stats(side_initial_ev_value(ids, side, receiver))
    total_impact  <- trade_pick_stats(side_epv_impact_value(ids, side, receiver))
    total_final   <- trade_pick_stats(side_out_ev_value(ids, side, receiver))

    tags$div(class = "tm-pick-table",
      tags$table(
        tags$colgroup(
          tags$col(style = "width:27%;"),
          tags$col(style = "width:20%;"),
          tags$col(style = "width:8%;"),
          tags$col(style = "width:12%;"),
          tags$col(style = "width:10%;"),
          tags$col(style = "width:13%;"),
          tags$col(style = "width:10%;")
        ),
        tags$thead(tags$tr(
          tags$th(style = "text-align:left;", "Picks"),
          tags$th(style = "text-align:left;", HTML("Protection")),
          tags$th(style = "text-align:center;", HTML("Swap<br>Right")),
          tags$th(style = "text-align:right;", HTML(sub("^Convey to ", "Convey<br>to&nbsp;", convey_header))),
          tags$th(style = "text-align:right;", HTML("Initial<br>EPV")),
          tags$th(style = "text-align:right;", HTML("Impact<br>EPV")),
          tags$th(style = "text-align:right;", HTML("Final<br>EPV"))
        )),
        tags$tbody(
          body_rows,
          tags$tr(class = "tm-total-row",
            tags$td("Total"),
            tags$td(""),
            tags$td(""),
            tags$td(""),
            tags$td(style = "text-align:right; font-size:14px;", sprintf("%.1f", total_initial["mean"])),
            tags$td(style = "text-align:right; font-size:14px;", sprintf("%.1f", total_impact["mean"])),
            tags$td(style = "text-align:right; font-size:14px;", sprintf("%.1f", total_final["mean"]))
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
    best_col <- team_secondary_color_app(best_team)

    signed_metric_card <- function(mean_to_A, q05_to_A, q95_to_A, label) {
      to_team <- if (mean_to_A >= 0) input$tm_teamA else input$tm_teamB
      metric_col <- team_secondary_color_app(to_team)
      border_col <- team_primary_color_app(to_team)
      shown_mean <- abs(mean_to_A)
      shown_q05 <- if (mean_to_A >= 0) q05_to_A else -q95_to_A
      shown_q95 <- if (mean_to_A >= 0) q95_to_A else -q05_to_A
      tags$div(style = sprintf(paste0(
        "min-height:112px; padding:15px; border:1px solid %s; ",
        "border-radius:10px; border-left:4px solid %s; background:rgba(15,15,26,0.72); ",
        "display:flex; align-items:center; gap:14px; justify-content:flex-start; box-sizing:border-box;"), border_col, metric_col),
        team_logo_img_app(to_team, size = 78),
        tags$div(style = "display:flex; flex-direction:column; justify-content:center; min-width:0;",
          tags$div(style = sprintf("font-size:30px; line-height:1.05; font-weight:850; color:%s; text-align:left; white-space:nowrap;", metric_col),
                   sprintf("%s %+.1f %s", to_team, shown_mean, label)),
          tags$div(style = "font-size:14px; color:#aaa; margin-top:8px; text-align:left;",
                   sprintf("90%% CI: [%+.1f, %+.1f] %s", shown_q05, shown_q95, label))
        )
      )
    }

    prob_card <- function(label, p, col, border_col = col) {
      tags$div(style = sprintf(
        "flex:1 1 0; min-width:0; height:84px; box-sizing:border-box; padding:10px 13px; border:1px solid %s; border-radius:8px; border-left:3px solid %s; display:flex; flex-direction:column; justify-content:center;", border_col, col),
        tags$div(style = "font-size:11px; color:#888; white-space:nowrap;", label),
        tags$div(style = sprintf("font-size:23px; line-height:1.1; font-weight:850; color:%s;", col), sprintf("%.1f%%", 100 * p))
      )
    }

    best_outcome_card <- function() {
      best_border_col <- team_primary_color_app(best_team)
      tags$div(style = sprintf(
        paste0(
          "min-width:0; padding:15px; border:1px solid %s; border-radius:10px; ",
          "border-left:4px solid %s; background:rgba(15,15,26,0.72); display:flex; ",
          "align-items:center; gap:14px; justify-content:flex-start; height:100%%; min-height:204px; box-sizing:border-box;"
        ), best_border_col, best_col),
        team_logo_img_app(best_team, size = 78),
        tags$div(style = "display:flex; flex-direction:column; justify-content:center; min-width:0;",
          tags$div(style = sprintf("font-size:30px; line-height:1.05; font-weight:850; color:%s; text-align:left; white-space:nowrap;", best_col),
                   sprintf("%s %.1f%% Best Player", best_team, 100 * best_prob)),
          tags$div(style = "font-size:14px; color:#aaa; margin-top:8px; text-align:left;",
                   sprintf("90%% CI: [%.1f%%, %.1f%%]", 100 * best_ci[1], 100 * best_ci[2]))
        )
      )
    }

    metric_blurb <- function(txt) {
      tags$div(style = "font-size:12px; color:#8b8fa3; line-height:1.45; margin-top:2px; padding:0 2px;", txt)
    }

    metric_block <- function(metric_card, prob_left, prob_right, description) {
      tags$div(style = "display:flex; flex-direction:column; gap:8px; min-width:0; height:100%; align-self:stretch;",
        metric_card,
        tags$div(style = "display:flex; gap:8px; align-items:stretch; min-width:0; height:84px; flex:0 0 84px;",
          prob_left,
          prob_right
        ),
        metric_blurb(description)
      )
    }

    tags$div(style = "font-size:13px; color:#bbb; line-height:1.7;",
      tags$div(
        style = "display:grid; grid-template-columns:repeat(3, minmax(0, 1fr)); gap:12px; align-items:stretch;",
        metric_block(
          signed_metric_card(eA_ev, q05_ev, q95_ev, "EPV"),
          prob_card(sprintf("%s higher EPV", input$tm_teamA), pA_ev, team_secondary_color_app(input$tm_teamA), team_primary_color_app(input$tm_teamA)),
          prob_card(sprintf("%s higher EPV", input$tm_teamB), pB_ev, team_secondary_color_app(input$tm_teamB), team_primary_color_app(input$tm_teamB)),
          "Expected Pick Value compares the typical value of the draft assets each side receives, using the model's pick-value curve."
        ),
        metric_block(
          signed_metric_card(eA_outcome, q05_outcome, q95_outcome, "4-Yr WS"),
          prob_card(sprintf("%s more 4-Yr WS", input$tm_teamA), pA_outcome, team_secondary_color_app(input$tm_teamA), team_primary_color_app(input$tm_teamA)),
          prob_card(sprintf("%s more 4-Yr WS", input$tm_teamB), pB_outcome, team_secondary_color_app(input$tm_teamB), team_primary_color_app(input$tm_teamB)),
          "4-Yr WS shows the simulated player outcomes those picks could become over a rookie-scale four-year window."
        ),
        tags$div(style = "display:flex; flex-direction:column; gap:8px; min-width:0; height:100%;",
          best_outcome_card(),
          metric_blurb("Best Player estimates which side is more likely to receive the single best 4-Yr WS player outcome among the picks in the trade.")
        )
      )
    )
  })

  trade_density_plot <- function(x, teamA, teamB, x_title, hover_label = "Net") {
    # In these distribution plots, the LEFT side (< 0) favors Team 1 and the
    # RIGHT side (> 0) favors Team 2. The trade_draws object stores net values
    # to Team 1, so callers pass the negative of that value.
    x <- as.numeric(x)
    x[!is.finite(x)] <- 0
    ddf <- density_curve_df(x)
    left_df <- ddf %>% filter(.data$x <= 0)
    right_df <- ddf %>% filter(.data$x >= 0)

    colA <- team_secondary_color_app(teamA)
    #fillA <- hex_to_rgba_app(team_primary_color_app(teamA), 0.30)
    fillA <- hex_to_rgba_app(team_primary_color_app(teamA), 0.8)
    colB <- team_secondary_color_app(teamB)
    #fillB <- hex_to_rgba_app(team_primary_color_app(teamB), 0.30)
    fillB <- hex_to_rgba_app(team_primary_color_app(teamB), 0.8)
    pA <- mean(x < 0, na.rm = TRUE)
    pB <- mean(x > 0, na.rm = TRUE)

    hover_txt <- sprintf(
      "%s: %%{x:.2f}<br>Density: %%{y:.2f}<br>%s wins: %.1f%%<br>%s wins: %.1f%%<extra></extra>",
      hover_label, teamA, 100 * pA, teamB, 100 * pB
    )

    p <- plot_ly()
    if (nrow(left_df) > 0) {
      p <- p %>% add_lines(data = left_df, x = ~x, y = ~density,
                           name = teamA, fill = "tozeroy",
                           line = list(color = colA, width = 2.6),
                           fillcolor = fillA,
                           hovertemplate = hover_txt)
    }
    if (nrow(right_df) > 0) {
      p <- p %>% add_lines(data = right_df, x = ~x, y = ~density,
                           name = teamB, fill = "tozeroy",
                           line = list(color = colB, width = 2.6),
                           fillcolor = fillB,
                           hovertemplate = hover_txt)
    }
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "#0f0f1a",
      font = list(family = "IBM Plex Mono", color = "#d9dddc"),
      xaxis = list(title = x_title, gridcolor = "#24243a", zerolinecolor = "#6b7280"),
      yaxis = list(title = "Density", gridcolor = "#24243a"),
      showlegend = FALSE,
      shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1,
                         yref = "paper",
                         line = list(color = "#6b7280", dash = "dash", width = 1.5))))
  }

  trade_waiting_plot_app <- function() {
    plot_ly() %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "#0f0f1a",
        font = list(family = "IBM Plex Mono", color = "#d9dddc"),
        xaxis = list(visible = FALSE, zeroline = FALSE, showgrid = FALSE),
        yaxis = list(visible = FALSE, zeroline = FALSE, showgrid = FALSE),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        annotations = list(list(
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          text = "Select picks from both teams to assess the trade.",
          showarrow = FALSE,
          font = list(size = 14, color = "#8b8fa3"),
          align = "center"
        ))
      )
  }

  output$tm_dist_ev <- renderPlotly({
    d <- trade_draws()
    if (is.null(d)) return(trade_waiting_plot_app())
    trade_density_plot(
      -d$net_to_A_ev,
      teamA = input$tm_teamA,
      teamB = input$tm_teamB,
      x_title = "Net EPV",
      hover_label = "Net EPV"
    )
  })

  output$tm_dist_outcome <- renderPlotly({
    d <- trade_draws()
    if (is.null(d)) return(trade_waiting_plot_app())
    trade_density_plot(
      -d$net_to_A_outcome,
      teamA = input$tm_teamA,
      teamB = input$tm_teamB,
      x_title = "Net 4-Yr WS Outcome",
      hover_label = "Net 4-Yr WS"
    )
  })

  output$tm_dist_best <- renderPlotly({
    d <- trade_draws()
    if (is.null(d)) return(trade_waiting_plot_app())
    trade_density_plot(
      -d$best_outcome_edge_to_A,
      teamA = input$tm_teamA,
      teamB = input$tm_teamB,
      x_title = "Best Player 4-Yr WS Edge",
      hover_label = "Best-player edge"
    )
  })


}

shinyApp(ui = ui, server = server)
