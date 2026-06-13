################################################################################
# NBA 3-2-1 Lottery Reform — Shiny Dashboard (v5)
# VERSION NOTE: adds explicit second-round draft-pick value curve display.
#
# Reads dashboard_data.rds produced by nba_lottery.R
#
# Tabs:
#   Impact Δ        — change in expected draft-asset value by team
#   Side by Side    — current vs 3-2-1 on value / quality / quantity
#   Lottery Odds    — seed-level expected pick and P(#1)
#   Full Table      — all 30 teams with credible intervals
#   Markov + Curve  — transition heatmap, state diagram, pick-value curve,
#                     and model-validation panel
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
  "4-year WS player outcomes" = "outcome",
  "Expected Asset Value" = "ev"
)

value_mode_label <- function(mode) {
  if (identical(mode, "ev")) "Expected Asset Value" else "4-year WS player outcomes"
}

value_mode_short_label <- function(mode) {
  if (identical(mode, "ev")) "expected asset value" else "realized player outcomes"
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
  terms <- extract_swap_terms_from_text(text, origs, owner)
  if (!is.na(terms$holder) && !is.na(terms$target)) {
    other <- setdiff(origs, owner)
    if (length(other) == 0L) other <- terms$target
    return(str_squish(sprintf("%s%s own or %s via %s swap", rnd, slot_txt, other[1], terms$holder)))
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

pick_display_assets <- pick_display_assets %>%
  rowwise() %>%
  mutate(
    short_label = display_pick_short_label(
      year, round, owner, member_original_teams,
      group_type, label, obligation, notes, fixed_slot_display
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
  "None (unprotected)" = "none",
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

app_theme <- bs_theme(
  bg         = "#0a0a14",
  fg         = "#d0d0d0",
  primary    = "#6d28d9",
  base_font  = font_google("IBM Plex Mono"),
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
      style = "color:#666;",
      sprintf("Bayesian 5-tier Markov chain x %sK MC sims | %s",
              format(meta$n_sims / 1000, nsmall = 0), meta$system_note)
    )
  ),

  tags$head(tags$style(HTML("
    .navbar, .bslib-page-navbar > .navbar {
      position: sticky;
      top: 0;
      z-index: 1050;
      box-shadow: 0 1px 0 #1a1a2a;
    }
    .sidebar .shiny-options-group label {
      white-space: nowrap;
    }
  "))),

  # ---- Tab 1 ----
  nav_panel(
    title = "Impact",
    icon  = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        radioButtons("impact_value_mode", "Value basis",
          choices = value_mode_choices, selected = "outcome", inline = TRUE),
        selectInput("impact_sort", "Sort by",
          choices = c("Rule change impact" = "delta_value",
                      "Current value"       = "current_mean",
                      "Proposed value"      = "new_mean",
                      "Record (worst first)" = "wins"),
          selected = "delta_value"),
        hr(),
        h6("How to read this"),
        p(style = "font-size:11px; color:#888; line-height:1.6;",
          "Each point is a team's change in expected draft-asset value; the line shows the 90% interval. ",
          "(4-year Win Shares) summed over 2026-2032, moving from the old ",
          "lottery to the approved 3-2-1 system.",
          br(), br(),
          "2026 is the ACTUAL post-lottery result and is identical under ",
          "both systems, so all movement comes from 2027-2032.")
      ),
      card(
        card_header(textOutput("impact_title")),
        plotlyOutput("impact_chart", height = "600px")
      )
    )
  ),

  # ---- Tab 2 ----
  nav_panel(
    title = "Side by Side",
    icon  = icon("columns"),
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        radioButtons("compare_value_mode", "Value basis",
          choices = value_mode_choices, selected = "outcome", inline = TRUE),
        radioButtons("compare_metric", "Metric",
          choices = c("Total portfolio value" = "total",
                      "Best single pick"       = "quality",
                      "Number of picks"        = "quantity"),
          selected = "total")
      ),
      card(
        card_header(textOutput("compare_title")),
        plotlyOutput("compare_chart", height = "600px")
      )
    )
  ),

  # ---- Tab 3 ----
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

  # ---- Tab 4 ----
  nav_panel(
    title = "Full Table",
    icon  = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        radioButtons("table_value_mode", "Value basis",
          choices = value_mode_choices, selected = "outcome", inline = TRUE),
        tags$p(style = "font-size:11px; color:#888; line-height:1.6;",
          "Toggle whether team portfolio columns reflect sampled player outcomes or posterior expected asset values.")
      ),
      card(
        card_header(textOutput("full_table_title")),
        DTOutput("full_table")
      ),
      uiOutput("team_detail")
    )
  ),

  # ---- Tab 5 ----
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
        col_widths = c(7, 5),
        div(class = "curve-stack",
          card(
            card_header("Round 1 Draft Pick Value Curve"),
            plotlyOutput("pick_curve_plot_r1", height = "320px")
          ),
          card(
            card_header("Round 2 Draft Pick Value Curve"),
            plotlyOutput("pick_curve_plot_r2", height = "390px")
          )
        ),
        card(
          card_header("Model Validation"),
          uiOutput("validation_panel")
        )
      )
    )
  ),

  # ---- Tab 6: Single Pick Valuation ----
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
          card_header("Expected Asset Value"),
          plotlyOutput("sp_dist_ev", height = "330px")
        ),
        card(
          card_header("Realized Outcome Simulation"),
          plotlyOutput("sp_dist_outcome", height = "330px")
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
        .tm-page .tm-help {
          color: #888;
          font-size: 11px;
          line-height: 1.6;
          margin: 0 0 12px 0;
        }
      ")),
      tags$p(class = "tm-help",
        "Eligible single-leg picks always show protection and swap-right controls; swap rights are valued as the incremental option only, not as an outright pick. ",
        "Locked 2026 picks use the actual post-lottery slot, and protections in the 12-15 band are disallowed under the approved rules and are not offered."
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(textOutput("tm_teamA_hdr")),
          selectInput("tm_teamA", "Team A",
            choices = all_team_abbr, selected = all_team_abbr[1]),
          selectizeInput("tm_picksA", "Picks A sends out",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "select one or more picks")),
          uiOutput("tm_obligA"),
          uiOutput("tm_picksA_detail")
        ),
        card(
          card_header(textOutput("tm_teamB_hdr")),
          selectInput("tm_teamB", "Team B",
            choices = all_team_abbr, selected = all_team_abbr[2]),
          selectizeInput("tm_picksB", "Picks B sends out",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "select one or more picks")),
          uiOutput("tm_obligB"),
          uiOutput("tm_picksB_detail")
        )
      ),
      card(
        card_header("Trade Assessment"),
        uiOutput("tm_verdict")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Expected Asset Edge"),
          plotlyOutput("tm_dist_ev", height = "300px")
        ),
        card(
          card_header("Realized Outcome Simulation"),
          plotlyOutput("tm_dist_outcome", height = "300px")
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


# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

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
    sprintf("Change in %s (4-yr WS scale, 2026-2032)", value_mode_label(input$impact_value_mode))
  })

  output$impact_chart <- renderPlotly({
    sc <- input$impact_sort
    asc <- sc == "wins"
    df <- summary_for_value_mode(input$impact_value_mode) %>%
      left_join(team_delta_for_mode(input$impact_value_mode, "total"), by = "team") %>%
      mutate(
        delta_mean = coalesce(.data$delta_mean, .data$delta_value),
        delta_q05 = coalesce(.data$delta_q05, .data$delta_value),
        delta_q95 = coalesce(.data$delta_q95, .data$delta_value)
      ) %>%
      arrange(if (asc) !!sym(sc) else desc(!!sym(sc)))

    caterpillar_delta_plot(
      df,
      x_title = "Impact",
      hover_suffix = "WS"
    )
  })

  # ---- Compare ----
  output$compare_title <- renderText({
    basis <- value_mode_label(input$compare_value_mode)
    switch(input$compare_metric,
      total    = sprintf("Total Portfolio Value Impact — %s (4-yr WS scale, 2026-2032)", basis),
      quality  = sprintf("Best Single Pick Impact — %s (4-yr WS scale)", basis),
      quantity = "Pick-Count Impact")
  })

  output$compare_chart <- renderPlotly({
    metric <- input$compare_metric
    deltas <- team_delta_for_mode(input$compare_value_mode, metric)

    df <- summary_for_value_mode(input$compare_value_mode) %>%
      left_join(deltas, by = "team") %>%
      mutate(
        delta_mean = case_when(
          metric == "total" ~ coalesce(.data$delta_mean, .data$delta_value),
          metric == "quality" ~ coalesce(.data$delta_mean, .data$delta_quality),
          TRUE ~ coalesce(.data$delta_mean, 0)
        ),
        delta_q05 = coalesce(.data$delta_q05, .data$delta_mean),
        delta_q95 = coalesce(.data$delta_q95, .data$delta_mean)
      ) %>%
      arrange(desc(.data$delta_mean))

    x_title <- switch(metric,
      total = "Impact",
      quality = "Best-pick impact",
      quantity = "Pick-count impact"
    )
    suffix <- if (metric == "quantity") "picks" else "WS"

    caterpillar_delta_plot(df, x_title = x_title, hover_suffix = suffix)
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
    tbl <- full_table_source() %>%
      mutate(
        Tier      = tier_short[tier],
        Record    = sprintf("%d-%d", wins, losses),
        `#Pk`     = round(n_picks_mean, 1),
        Current   = round(current_mean, 1),
        `3-2-1`   = round(new_mean, 1),
        `D WS`    = round(delta_value, 1),
        `D%`      = sprintf("%+.1f%%", delta_pct),
        `s Cur`   = round(current_sd, 1),
        `s New`   = round(new_sd, 1),
        `90% Cur` = sprintf("[%.0f, %.0f]", current_q05, current_q95),
        `90% New` = sprintf("[%.0f, %.0f]", new_q05, new_q95)
      ) %>%
      select(Team = team, Tier, Record, `#Pk`, Current, `3-2-1`,
             `D WS`, `D%`, `s Cur`, `s New`, `90% Cur`, `90% New`)
    datatable(
      tbl, selection = "single", rownames = FALSE,
      options = list(pageLength = 30, dom = "t", ordering = TRUE,
                     columnDefs = list(list(className = "dt-right", targets = 3:11)))
    ) %>%
      formatStyle("D WS", color = styleInterval(0, c("#ef4444", "#10b981")))
  })

  output$team_detail <- renderUI({
    sel <- input$full_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    t <- full_table_source()[sel, ]
    basis <- value_mode_label(input$table_value_mode)
    card(class = "mt-2",
      card_header(style = "border-left:3px solid #6d28d9;",
        sprintf("%s — %s (%d-%d)", t$team, tier_labels[t$tier], t$wins, t$losses)),
      card_body(style = "font-size:12px; color:#bbb; line-height:1.8;",
        tags$div(style = "color:#888;", sprintf("Value basis: %s", basis)),
        tags$div(tags$strong("Current: "),
          sprintf("value=%.1f +/- %.1f | 90%% CI [%.0f, %.0f] | best %.1f",
                  t$current_mean, t$current_sd, t$current_q05, t$current_q95,
                  t$best_current)),
        tags$div(tags$strong("3-2-1: "),
          sprintf("value=%.1f +/- %.1f | 90%% CI [%.0f, %.0f] | best %.1f",
                  t$new_mean, t$new_sd, t$new_q05, t$new_q95, t$best_new)),
        tags$div(tags$strong(
          style = ifelse(t$delta_value >= 0, "color:#10b981;", "color:#ef4444;"),
          sprintf("Impact %+.1f WS (%+.1f%%) | sigma change %+.1f | quality %+.2f",
                  t$delta_value, t$delta_pct, t$sigma_change, t$delta_quality))))
    )
  })

  # ---- Transition heatmap ----
  output$trans_heatmap <- renderPlotly({
    m <- trans_mat
    z <- round(m * 100, 1)
    plot_ly(
      x = tier_short[colnames(m)],
      y = tier_short[rownames(m)],
      z = z, type = "heatmap",
      colorscale = list(c(0, "#0f0f1a"), c(1, "#6d28d9")),
      text = matrix(sprintf("%.1f%%", z), nrow = nrow(z)),
      texttemplate = "%{text}", textfont = list(size = 10, color = "#ddd"),
      hovertemplate = "From %{y} to %{x}<br>Probability: %{z:.1f}%<extra></extra>"
    ) %>%
      plotly_dark(
        xaxis = list(title = list(text = "Year t + 1"), side = "bottom"),
        yaxis = list(title = list(text = "Year t"), autorange = "reversed")
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
  pick_curve_base_plot <- function(pc, x_title, y_title = "4-year Win Shares") {
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
          orientation = "h",
          x = 0.02,
          y = 1.18,
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = 9),
          bgcolor = "rgba(15,15,26,0.86)",
          bordercolor = "rgba(255,255,255,0.08)",
          borderwidth = 1
        ),
        margin = list(l = 70, r = 40, t = 82, b = 58)
      )
  }

  output$pick_curve_plot_r1 <- renderPlotly({
    pc <- pick_curve %>%
      filter(.data$pick <= 30) %>%
      mutate(round = 1L)

    if (nrow(pc) == 0) {
      return(plot_ly() %>%
               plotly_dark(
                 xaxis = list(title = list(text = "Pick")),
                 yaxis = list(title = list(text = "4-yr Win Shares")),
                 annotations = list(text = "Round 1 curve unavailable", x = 0.5, y = 0.5,
                                    xref = "paper", yref = "paper", showarrow = FALSE)))
    }

    pick_curve_base_plot(pc, x_title = "Pick", y_title = "4-yr Win Shares")
  })

  output$pick_curve_plot_r2 <- renderPlotly({
    pc <- pick_curve %>%
      filter(.data$pick >= 31, .data$pick <= 60) %>%
      mutate(round = 2L)

    if (nrow(pc) == 0) {
      return(plot_ly() %>%
               plotly_dark(
                 xaxis = list(title = list(text = "Pick")),
                 yaxis = list(title = list(text = "4-yr Win Shares")),
                 annotations = list(text = "Round 2 curve unavailable — dashboard_data.rds lacks picks 31-60 / mu_31:mu_60. Rerun 04_lotterySims_.R", x = 0.5, y = 0.5,
                                    xref = "paper", yref = "paper", showarrow = FALSE)))
    }

    r2_y_title <- "4-yr Win Shares"

    p <- pick_curve_base_plot(
      pc,
      x_title = "Pick",
      y_title = r2_y_title
    )

    # The second-round model estimates P(play) in Stan. When p_play is
    # available, add it on a secondary axis so the plot shows both expected
    # value and the modeled probability that the pick produces an NBA-minutes
    # outcome. The pick-value ribbons remain EV CI and outcome quantiles.
    if ("p_play" %in% names(pc) && any(is.finite(pc$p_play))) {
      if (all(c("p_play_q05", "p_play_q95") %in% names(pc)) &&
          any(is.finite(pc$p_play_q05)) && any(is.finite(pc$p_play_q95))) {
        p <- p %>%
          add_ribbons(data = pc %>% filter(is.finite(.data$p_play_q05), is.finite(.data$p_play_q95)),
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
        add_lines(data = pc, x = ~pick, y = ~(100 * p_play),
                  name = "Modeled P(play) %",
                  yaxis = "y2",
                  line = list(color = "#10b981", width = 2, dash = "dot"),
                  hovertemplate = "Pick %{x}<br>P(play): %{y:.1f}%<extra></extra>")

      if ("emp_p_play" %in% names(pc) && any(is.finite(pc$emp_p_play))) {
        p <- p %>%
          add_markers(data = pc %>% filter(is.finite(.data$emp_p_play)),
                      x = ~pick, y = ~(100 * emp_p_play),
                      name = "Empirical P(play) %",
                      yaxis = "y2",
                      marker = list(color = "#e5e7eb", size = 6, symbol = "x"),
                      hovertemplate = "Pick %{x}<br>Empirical P(play): %{y:.1f}%<extra></extra>")
      }

      p <- p %>%
        layout(
          # Keep the dual-axis labels readable and prevent the right-side
          # P(play) title/ticks from being clipped by the card boundary.
          margin = list(l = 70, r = 150, t = 112, b = 58),
          yaxis = list(
            title = list(text = r2_y_title),
            automargin = TRUE,
            gridcolor = "#1a1a2a",
            zerolinecolor = "#333"
          ),
          yaxis2 = list(
            title = list(text = "P(play) %", standoff = 18),
            overlaying = "y",
            side = "right",
            range = c(0, 100),
            automargin = TRUE,
            gridcolor = "rgba(0,0,0,0)",
            zerolinecolor = "#333"
          ),
          # Move the legend above the plotting region instead of over the data.
          legend = list(
            orientation = "h",
            x = 0.02,
            y = 1.30,
            xanchor = "left",
            yanchor = "bottom",
            font = list(size = 9),
            bgcolor = "rgba(15,15,26,0.86)",
            bordercolor = "rgba(255,255,255,0.08)",
            borderwidth = 1
          )
        )
    }

    p
  })

  # Backward-compatible alias in case an older UI still references the combined
  # curve output name.
  output$pick_curve_plot <- renderPlotly({
    pc <- pick_curve
    pick_curve_base_plot(pc, x_title = "Pick", y_title = "4-yr Win Shares")
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

    swap_cur <- swap_exercise_summary_for_display(r, "cur")
    swap_new <- swap_exercise_summary_for_display(r, "new")

    expected_count_txt <- if (nrow(s) > 0 && all(c("cur_expected_pick_count", "new_expected_pick_count") %in% names(s))) {
      sprintf("Expected Pick Count: current %.1f | 3-2-1 %.1f",
              s$cur_expected_pick_count, s$new_expected_pick_count)
    } else {
      NULL
    }

    line_div <- function(..., extra_style = "") tags$div(style = paste0("margin-bottom:10px;", extra_style), ...)

    tags$div(style = "font-size:11px; color:#aaa; line-height:1.55;",
      tags$div(tags$strong(style = "color:#6d28d9;", "Pick details")),
      line_div(sprintf("Current Team: %s", r$owner)),
      line_div(sprintf("Original Team%s: %s", ifelse(str_detect(r$member_original_teams, ","), "s", ""), r$member_original_teams)),
      if (!is.na(r$fixed_slot_display)) line_div(tags$span(style = "color:#10b981;", sprintf("Actual Pick: %s", r$fixed_slot_display))),
      line_div(sprintf("Obligation: %s", r$obligation)),
      prob_card_row("Conveyance Probability", convey_cur, convey_new),
      if (!is.null(swap_cur) || !is.null(swap_new)) prob_card_row("Swap Exercise Probability", swap_cur, swap_new),
      if (!is.null(expected_count_txt)) line_div(expected_count_txt, extra_style = "color:#888; margin-top:10px;"),
      if (!is.na(r$group_type) && r$group_type != "single_asset")
        line_div("Grouped RealGM-style entitlement; underlying conditional legs are modeled internally.", extra_style = "color:#f59e0b;"),
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
        "4-year WS player-outcome impact",
        s_out,
        "sampled player-level 4-yr WS outcomes"
      ),
      metric_row(
        "Expected asset value impact",
        s_ev,
        "posterior mean slot value, 4-yr WS scale"
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
      x_title = "Expected asset value (4-yr WS scale)",
      hover_label = "EV"
    )
  })

  output$sp_dist_outcome <- renderPlotly({
    req(input$sp_asset)
    single_pick_density_plot(
      cur = display_asset_cur_draws[, input$sp_asset],
      new = display_asset_new_draws[, input$sp_asset],
      stats_row = sp_stats_outcome(),
      x_title = "Realized 4-yr Win Shares outcome",
      hover_label = "outcome"
    )
  })

  # ==========================================================================
  # TAB 7: TRADE MACHINE
  # ==========================================================================

  output$tm_teamA_hdr <- renderText(sprintf("Team A — %s", input$tm_teamA))
  output$tm_teamB_hdr <- renderText(sprintf("Team B — %s", input$tm_teamB))

  # picks each team currently OWNS (can send out)
  picks_owned_by <- function(team) {
    pick_display_assets %>%
      filter(owner == team) %>%
      arrange(year, round, short_label)
  }

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

  obligation_controls <- function(ids, side) {
    if (is.null(ids) || length(ids) == 0)
      return(NULL)
    rows <- pick_display_assets %>% filter(display_asset_id %in% ids)
    tags$div(style = "border-top:1px solid #1a1a2a; margin-top:8px; padding-top:8px;",
      tags$div(style = "font-size:11px; color:#6d28d9; margin-bottom:4px;",
               "Attach obligations to eligible single-leg picks being sent"),
      lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        did <- r$display_asset_id
        pid <- paste0("prot_", side, "_", safe_id(did))
        sid <- paste0("swap_", side, "_", safe_id(did))
        locked <- r$year == 2026
        eligible <- display_is_single_leg(did) && !locked
        tags$div(style = "margin-bottom:8px; font-size:11px; color:#aaa;",
          tags$div(tags$strong(r$trade_label),
            if (locked) tags$span(style = "color:#10b981;",
                                  " (2026 locked — obligations N/A)"),
            if (!eligible && !locked) tags$span(style = "color:#f59e0b;",
                                                " (grouped entitlement — encoded obligations used)")),
          if (eligible) tags$div(style = "display:flex; gap:8px; align-items:center;",
            tags$div(style = "flex:1;",
              selectInput(pid, NULL, choices = protection_choices,
                          selected = "none", width = "100%")),
            tags$div(style = "flex:1;",
              checkboxInput(sid, "Swap right only (incremental value)", value = FALSE))))
      }))
  }
  output$tm_obligA <- renderUI(obligation_controls(input$tm_picksA, "A"))
  output$tm_obligB <- renderUI(obligation_controls(input$tm_picksB, "B"))

  # detail panels listing the EXISTING obligations on selected picks
  render_pick_detail <- function(ids) {
    if (is.null(ids) || length(ids) == 0)
      return(tags$div(style = "font-size:11px; color:#666;", "No picks selected"))
    rows <- pick_display_assets %>% filter(display_asset_id %in% ids)
    tags$div(style = "font-size:11px; color:#aaa; line-height:1.6;",
      lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        pvs <- pick_display_value_summary %>% filter(display_asset_id == r$display_asset_id)
        prob_txt <- if (nrow(pvs) > 0 && "new_convey_prob" %in% names(pvs)) {
          sprintf(" | alloc %.0f%%", 100 * pvs$new_convey_prob)
        } else ""
        tags$div(
          tags$strong(r$trade_label),
          if (!is.na(r$fixed_slot_display)) tags$span(style = "color:#10b981;", sprintf(" (%s)", r$fixed_slot_display)),
          tags$span(style = "color:#888;", sprintf(" [%s%s]", r$obligation, prob_txt)))
      }))
  }
  output$tm_picksA_detail <- renderUI(render_pick_detail(input$tm_picksA))
  output$tm_picksB_detail <- renderUI(render_pick_detail(input$tm_picksB))

  # ---- value (per sim) of a single sent pick TO ITS RECEIVER ----
  # The Trade Machine keeps two concepts separate:
  #   1. Expected asset value: posterior mean slot value, no player-outcome noise.
  #   2. Realized player outcome: sampled player-level 4-year WS draws.
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

    # Per-pick realized outcome comparison after the trade. Team A receives the
    # picks selected on Team B's side, and Team B receives the picks selected on
    # Team A's side. This estimates who gets the single best player outcome, not
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
      best_outcome_tie  = A_has_pick & B_has_pick & A_best_outcome == B_best_outcome
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
    pTie_ev <- mean(d$net_to_A_ev == 0)

    eA_outcome  <- mean(d$net_to_A_outcome)
    q05_outcome <- quantile(d$net_to_A_outcome, 0.05)
    q95_outcome <- quantile(d$net_to_A_outcome, 0.95)
    pA_outcome  <- mean(d$net_to_A_outcome > 0)
    pB_outcome  <- mean(d$net_to_A_outcome < 0)
    pTie_outcome <- mean(d$net_to_A_outcome == 0)

    pA_best_outcome <- mean(d$best_outcome_to_A, na.rm = TRUE)
    pB_best_outcome <- mean(d$best_outcome_to_B, na.rm = TRUE)
    pBest_tie <- mean(d$best_outcome_tie, na.rm = TRUE)

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
    pBest_tie_ci <- best_prob_ci(d$best_outcome_tie)

    best_team <- if (pA_best_outcome >= pB_best_outcome) input$tm_teamA else input$tm_teamB
    best_prob <- max(pA_best_outcome, pB_best_outcome, na.rm = TRUE)
    best_ci <- if (identical(best_team, input$tm_teamA)) pA_best_ci else pB_best_ci
    best_col <- if (identical(best_team, input$tm_teamA)) "#3b82f6" else "#f59e0b"

    edge_team <- if (abs(eA_ev) < 1e-8) "neither team" else if (eA_ev > 0) input$tm_teamA else input$tm_teamB
    wcol <- if (abs(eA_ev) < 1e-8) "#aaa" else if (eA_ev > 0) "#10b981" else "#ef4444"
    edge_val <- abs(eA_ev)

    metric_card <- function(title, value, q05, q95, col, subtitle) {
      tags$div(style = sprintf(
        "flex:1; min-width:220px; padding:14px; border:1px solid #1a1a2a; border-radius:10px; border-left:4px solid %s; background:rgba(15,15,26,0.72);", col),
        tags$div(style = "font-size:11px; color:#888;", title),
        tags$div(style = sprintf("font-size:30px; line-height:1.1; font-weight:800; color:%s;", col),
                 sprintf("%+.1f", value)),
        tags$div(style = "font-size:11px; color:#aaa; margin-top:4px;",
                 sprintf("90%% CI: [%+.1f, %+.1f] WS", q05, q95)),
        tags$div(style = "font-size:10px; color:#777; margin-top:4px;", subtitle)
      )
    }

    prob_card <- function(label, p, col) {
      tags$div(style = sprintf(
        "flex:1; min-width:140px; padding:10px 12px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;", col),
        tags$div(style = "font-size:10px; color:#888; white-space:nowrap;", label),
        tags$div(style = sprintf("font-size:22px; font-weight:800; color:%s;", col), sprintf("%.1f%%", 100 * p))
      )
    }

    best_outcome_card <- function() {
      tags$div(style = sprintf(
        paste0(
          "grid-column:3; grid-row:1 / span 2; min-width:260px; padding:14px; ",
          "border:1px solid #1a1a2a; border-radius:10px; border-left:4px solid %s; ",
          "background:rgba(15,15,26,0.72); display:flex; flex-direction:column; ",
          "justify-content:center; min-height:138px; box-sizing:border-box;"
        ), best_col),
        tags$div(style = "font-size:11px; color:#888;", "Best player outcome"),
        tags$div(style = sprintf("font-size:30px; line-height:1.1; font-weight:800; color:%s;", best_col),
                 sprintf("%s %.1f%%", best_team, 100 * best_prob)),
        tags$div(style = "font-size:11px; color:#aaa; margin-top:4px;",
                 sprintf("90%% CI: [%.1f%%, %.1f%%]", 100 * best_ci[1], 100 * best_ci[2])),
        tags$div(style = "font-size:11px; color:#aaa; margin-top:4px;",
                 sprintf("%s %.1f%% [%.1f%%, %.1f%%] | %s %.1f%% [%.1f%%, %.1f%%] | tie %.1f%% [%.1f%%, %.1f%%]",
                         input$tm_teamA, 100 * pA_best_outcome, 100 * pA_best_ci[1], 100 * pA_best_ci[2],
                         input$tm_teamB, 100 * pB_best_outcome, 100 * pB_best_ci[1], 100 * pB_best_ci[2],
                         100 * pBest_tie, 100 * pBest_tie_ci[1], 100 * pBest_tie_ci[2])),
        tags$div(style = "font-size:10px; color:#777; margin-top:4px;",
                 "Probability of receiving the single highest realized 4-yr WS outcome among selected picks")
      )
    }

    tags$div(style = "font-size:12px; color:#bbb; line-height:1.7;",
      tags$div(
        style = paste(
          "display:grid;",
          "grid-template-columns:minmax(220px, 1fr) minmax(220px, 1fr) minmax(260px, 1fr);",
          "grid-template-rows:auto auto;",
          "gap:12px; align-items:stretch; margin-bottom:12px;"
        ),
        tags$div(style = "grid-column:1; grid-row:1; min-width:0;",
          metric_card(
            sprintf("Expected asset edge to %s", edge_team),
            ifelse(edge_team == input$tm_teamA, eA_ev, ifelse(edge_team == input$tm_teamB, -eA_ev, 0)),
            ifelse(edge_team == input$tm_teamA, q05_ev, ifelse(edge_team == input$tm_teamB, -q95_ev, q05_ev)),
            ifelse(edge_team == input$tm_teamA, q95_ev, ifelse(edge_team == input$tm_teamB, -q05_ev, q95_ev)),
            wcol,
            sprintf("Mean edge: %.1f WS", edge_val)
          )
        ),
        tags$div(style = "grid-column:2; grid-row:1; min-width:0;",
          metric_card(
            sprintf("Realized outcome net to %s", input$tm_teamA),
            eA_outcome, q05_outcome, q95_outcome,
            ifelse(eA_outcome >= 0, "#10b981", "#ef4444"),
            "Sampled player-level 4-yr WS outcomes"
          )
        ),
        best_outcome_card(),
        tags$div(style = "grid-column:1 / span 2; grid-row:2; display:flex; gap:10px; flex-wrap:wrap; min-width:0;",
          prob_card(sprintf("%s higher EV", input$tm_teamA), pA_ev, "#3b82f6"),
          prob_card(sprintf("%s higher EV", input$tm_teamB), pB_ev, "#f59e0b"),
          if (pTie_ev > 0.001) prob_card("EV tie", pTie_ev, "#9ca3af"),
          prob_card(sprintf("%s better outcome", input$tm_teamA), pA_outcome, "#3b82f6"),
          prob_card(sprintf("%s better outcome", input$tm_teamB), pB_outcome, "#f59e0b"),
          if (pTie_outcome > 0.001) prob_card("Outcome tie", pTie_outcome, "#9ca3af")
        )
      ),
      tags$div(style = "color:#888; margin-top:6px;",
        sprintf("A sends %d pick(s); B sends %d pick(s). EV uses posterior mean slot curves; realized outcome uses sampled player-level 4-yr WS draws, so a later pick can still beat an earlier pick ex post.",
                length(input$tm_picksA %||% character(0)),
                length(input$tm_picksB %||% character(0)))),
      tags$div(style = "color:#888; margin-top:4px;",
        "Hypothetical protections / swaps are available for eligible single-leg picks; swap rights are valued as incremental option value relative to the receiver's own pick, while realized-outcome draws apply protections and the ex-post give-up leg directly.")
    )
  })

  trade_density_plot <- function(x, line_color, fill_color, x_title) {
    ddf <- density_curve_df(x)
    plot_ly() %>%
      add_lines(data = ddf, x = ~x, y = ~density,
                name = "Density", fill = "tozeroy",
                line = list(color = line_color, width = 2.5),
                fillcolor = fill_color,
                hovertemplate = "Net WS: %{x:.1f}<br>Density: %{y:.1f}<extra></extra>") %>%
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
      x_title = sprintf("Net expected 4-yr WS transferred to %s (>0 favors %s)",
                        input$tm_teamA, input$tm_teamA)
    )
  })

  output$tm_dist_outcome <- renderPlotly({
    d <- trade_draws()
    if (is.null(d)) return(plotly_empty())
    trade_density_plot(
      d$net_to_A_outcome,
      line_color = "#7c3aed",
      fill_color = "rgba(14,165,233,0.35)",
      x_title = sprintf("Net realized 4-yr WS transferred to %s (>0 favors %s)",
                        input$tm_teamA, input$tm_teamA)
    )
  })

}

shinyApp(ui = ui, server = server)

