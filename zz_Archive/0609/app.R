################################################################################
# NBA 3-2-1 Lottery Reform — Shiny Dashboard (v5)
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
pick_curve   <- dd$pick_curve
trans_mat    <- dd$transition_matrix
trans_counts <- dd$transition_counts
stationary   <- dd$stationary
tier_balls   <- dd$tier_balls
meta         <- dd$metadata
stan_diag    <- dd$stan_diagnostics

# Per-pick objects (Single Pick & Trade Machine tabs)
pick_assets        <- dd$pick_assets
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

  slot_clamped <- pmin(pmax(slot_vec, 1L), 30L)
  slot_index <- ifelse(is.na(slot_clamped), 1L, slot_clamped)

  mu_cols <- paste0("mu_", 1:30)
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
  out <- a / (slot_clamped ^ b) + g
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
      owner, year, label, obligation, notes,
      group_type = "single_asset",
      display_group = asset_id,
      member_n = 1L,
      member_original_teams = original_team
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

# Team abbreviations present in the asset registry, sorted
all_team_abbr <- sort(unique(c(pick_display_assets$owner, pick_assets$owner, pick_assets$original_team)))

# protection options offered in the Trade Machine (top-N bands; 12-15 illegal)
protection_choices <- c(
  "None (unprotected)" = "none",
  "Top-1"  = "top1",  "Top-2"  = "top2",  "Top-3"  = "top3",  "Top-4"  = "top4",
  "Top-5"  = "top5",  "Top-6"  = "top6",  "Top-8"  = "top8",
  "Top-10" = "top10", "Lottery (top-14)" = "lottery",
  "Top-16" = "top16", "Top-20" = "top20"
)
protection_floor_app <- function(p) {
  switch(p, top1 = 1, top2 = 2, top3 = 3, top4 = 4, top5 = 5, top6 = 6,
         top8 = 8, top10 = 10, lottery = 14, top16 = 16, top20 = 20, 0)
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
  p %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "#0f0f1a",
      font          = list(family = "IBM Plex Mono", color = "#999"),
      xaxis = list(gridcolor = "#1a1a2a", zerolinecolor = "#333"),
      yaxis = list(gridcolor = "#1a1a2a", zerolinecolor = "#333"),
      ...
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

  # ---- Tab 1 ----
  nav_panel(
    title = "Impact",
    icon  = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        width = 230,
        radioButtons("impact_value_mode", "Value basis",
          choices = value_mode_choices, selected = "outcome"),
        selectInput("impact_sort", "Sort by",
          choices = c("Rule change impact" = "delta_value",
                      "Current value"       = "current_mean",
                      "Proposed value"      = "new_mean",
                      "Record (worst first)" = "wins"),
          selected = "delta_value"),
        hr(),
        h6("How to read this"),
        p(style = "font-size:11px; color:#888; line-height:1.6;",
          "Each bar is a team's change in expected draft-asset value ",
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
        width = 230,
        radioButtons("compare_value_mode", "Value basis",
          choices = value_mode_choices, selected = "outcome"),
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
      col_widths = c(12, 12),
      card(
        card_header("Expected Pick Position by Lottery Seed"),
        plotlyOutput("lottery_line", height = "280px")
      ),
      card(
        card_header("Probability of #1 Pick by Seed (%)"),
        plotlyOutput("lottery_bar", height = "280px")
      )
    ),
    card(class = "mt-2", card_body(
      style = "font-size:11px; color:#888; line-height:1.6;",
      tags$strong(style = "color:#f59e0b;", "The relegation effect: "),
      "Under 3-2-1 the three worst teams get only 2 balls each (~5.4% at #1), ",
      "while the seven non-play-in teams above them get 3 balls each ",
      "(~8.1% at #1). The worst record can pick no lower than 12th. ",
      "All 16 lottery slots are drawn."))
  ),

  # ---- Tab 4 ----
  nav_panel(
    title = "Full Table",
    icon  = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        width = 230,
        radioButtons("table_value_mode", "Value basis",
          choices = value_mode_choices, selected = "outcome"),
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
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Tier Transition Probabilities (row = from, col = to)"),
        plotlyOutput("trans_heatmap", height = "360px")
      ),
      card(
        card_header("Tier Transition State Diagram"),
        plotOutput("trans_diagram", height = "360px")
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Draft Pick Value Curve (4-yr Win Shares)"),
        plotlyOutput("pick_curve_plot", height = "320px")
      ),
      card(
        card_header("Model Validation"),
        uiOutput("validation_panel")
      )
    )
  ),

  # ---- Tab 6: Single Pick Valuation ----
  nav_panel(
    title = "Single Pick",
    icon  = icon("basketball"),
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        selectInput("sp_year", "Draft year",
          choices  = sort(unique(pick_assets$year)),
          selected = 2026),
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
  ),

  nav_spacer(),
  nav_item(tags$small(
    style = "color:#444; font-size:9px;",
    sprintf("Pick curve %s | Markov: %d transitions / %d seasons | mixing %.1f yr",
            stan_diag$pick_model$curve_type,
            stan_diag$markov_model$n_transitions,
            stan_diag$markov_model$n_seasons,
            stan_diag$markov_model$mixing_time)))
)


# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ---- Impact ----
  output$impact_title <- renderText({
    sprintf("Change in %s (4-yr WS scale, 2026-2032)", value_mode_label(input$impact_value_mode))
  })

  output$impact_chart <- renderPlotly({
    sc <- input$impact_sort
    asc <- sc == "wins"
    df <- summary_for_value_mode(input$impact_value_mode) %>%
      arrange(if (asc) !!sym(sc) else desc(!!sym(sc))) %>%
      mutate(team_fct = factor(team, levels = rev(team)))
    plot_ly(
      data = df, y = ~team_fct, x = ~delta_value,
      type = "bar", orientation = "h",
      marker = list(color = ~ifelse(delta_value >= 0, "#059669", "#dc2626")),
      hovertemplate = "<b>%{y}</b><br>Delta: %{x:.1f} WS<extra></extra>"
    ) %>%
      plotly_dark(
        xaxis = list(title = sprintf("Delta %s (4-yr WS scale)", value_mode_label(input$impact_value_mode)),
                     zerolinecolor = "#444"),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 52)
      )
  })

  # ---- Compare ----
  output$compare_title <- renderText({
    basis <- value_mode_label(input$compare_value_mode)
    switch(input$compare_metric,
      total    = sprintf("Total Portfolio Value — %s (4-yr WS scale, 2026-2032)", basis),
      quality  = sprintf("Best Single Pick — %s (4-yr WS scale)", basis),
      quantity = "Average Number of Picks Owned")
  })

  output$compare_chart <- renderPlotly({
    df <- summary_for_value_mode(input$compare_value_mode) %>%
      arrange(desc(current_mean)) %>%
      mutate(team_fct = factor(team, levels = rev(team)))
    if (input$compare_metric == "total") {
      cur <- df$current_mean; new <- df$new_mean
      x_title <- value_mode_label(input$compare_value_mode)
    } else if (input$compare_metric == "quality") {
      cur <- df$best_current; new <- df$best_new
      x_title <- value_mode_label(input$compare_value_mode)
    } else {
      cur <- df$n_picks_mean; new <- df$n_picks_mean
      x_title <- "Average number of picks owned"
    }
    plot_ly(data = df, y = ~team_fct) %>%
      add_bars(x = cur, name = "Current",
               marker = list(color = "#3b82f6"), orientation = "h") %>%
      add_bars(x = new, name = "3-2-1",
               marker = list(color = "#f59e0b"), orientation = "h") %>%
      plotly_dark(
        barmode = "group",
        xaxis = list(title = x_title),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 52),
        legend = list(x = 0.68, y = 0.04, font = list(size = 10))
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
                marker = list(color = "#3b82f6", size = 7)) %>%
      add_trace(data = new, x = ~seed, y = ~expected_pick, type = "scatter",
                mode = "lines+markers", name = "3-2-1",
                line = list(color = "#f59e0b", width = 3),
                marker = list(color = "#f59e0b", size = 7)) %>%
      plotly_dark(
        xaxis = list(title = "Lottery Seed (1 = worst record)", dtick = 2),
        yaxis = list(title = "Expected Pick", autorange = "reversed"),
        legend = list(x = 0.05, y = 0.05, font = list(size = 10))
      )
  })

  # ---- Lottery bar ----
  output$lottery_bar <- renderPlotly({
    cur <- lottery_dist %>% filter(system == "Current", seed <= 16)
    new <- lottery_dist %>% filter(system == "Proposed 3-2-1", seed <= 16)
    plot_ly() %>%
      add_bars(data = cur, x = ~seed, y = ~(prob_no1 * 100),
               name = "Current", marker = list(color = "#3b82f6")) %>%
      add_bars(data = new, x = ~seed, y = ~(prob_no1 * 100),
               name = "3-2-1", marker = list(color = "#f59e0b")) %>%
      plotly_dark(
        barmode = "group",
        xaxis = list(title = "Lottery Seed", dtick = 2),
        yaxis = list(title = "P(#1) %"),
        legend = list(x = 0.68, y = 0.95, font = list(size = 10))
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
      hovertemplate = "from %{y} to %{x}: %{z}%<extra></extra>"
    ) %>%
      plotly_dark(
        xaxis = list(title = "To tier (next season)", side = "bottom"),
        yaxis = list(title = "From tier", autorange = "reversed")
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

  # ---- Pick value curve ----
  output$pick_curve_plot <- renderPlotly({
    pc <- pick_curve
    p <- plot_ly(pc, x = ~pick) %>%
      add_ribbons(ymin = ~(expected_war - war_sd),
                  ymax = ~(expected_war + war_sd),
                  name = "+/- 1 SD",
                  line = list(color = "transparent"),
                  fillcolor = "rgba(109,40,217,0.18)")
    if ("emp_mean" %in% names(pc)) {
      p <- p %>% add_markers(y = ~emp_mean, name = "Empirical slot mean",
                             marker = list(color = "#f59e0b", size = 6))
    }
    p %>%
      add_lines(y = ~expected_war, name = "Posterior mean",
                line = list(color = "#6d28d9", width = 2.5)) %>%
      plotly_dark(
        xaxis = list(title = "Draft Pick", dtick = 5),
        yaxis = list(title = "4-year Win Shares"),
        legend = list(x = 0.55, y = 0.95, font = list(size = 10))
      )
  })

  # ---- Validation panel ----
  output$validation_panel <- renderUI({
    pm <- stan_diag$pick_model
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

  # populate pick choices for the chosen year
  observeEvent(input$sp_year, {
    opts <- pick_display_assets %>%
      filter(year == input$sp_year) %>%
      arrange(owner, label)
    choice_vec <- setNames(opts$display_asset_id, opts$label)
    updateSelectInput(session, "sp_asset", choices = choice_vec)
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

  output$sp_obligation <- renderUI({
    r <- sp_row()
    s <- sp_stats_outcome()
    if (nrow(r) == 0) return(NULL)

    prob_txt <- if (nrow(s) > 0 && "new_convey_prob" %in% names(s)) {
      sprintf("Modeled allocation probability: current %.0f%% | 3-2-1 %.0f%%",
              100 * s$cur_convey_prob, 100 * s$new_convey_prob)
    } else {
      NULL
    }

    expected_count_txt <- if (nrow(s) > 0 && all(c("cur_expected_pick_count", "new_expected_pick_count") %in% names(s))) {
      sprintf("Expected pick count: current %.2f | 3-2-1 %.2f",
              s$cur_expected_pick_count, s$new_expected_pick_count)
    } else {
      NULL
    }

    tags$div(style = "font-size:11px; color:#aaa; line-height:1.7;",
      tags$div(tags$strong(style = "color:#6d28d9;", "Pick details")),
      tags$div(sprintf("%s entitlement", r$owner)),
      tags$div(sprintf("Original pick(s): %s", r$member_original_teams)),
      tags$div(sprintf("Obligation: %s", r$obligation)),
      if (!is.null(prob_txt)) tags$div(prob_txt),
      if (!is.null(expected_count_txt)) tags$div(expected_count_txt),
      if (!is.na(r$group_type) && r$group_type != "single_asset")
        tags$div(style = "color:#f59e0b;", "Grouped RealGM-style entitlement; underlying conditional legs are modeled internally."),
      if (r$year == 2026)
        tags$div(style = "color:#10b981;", "Locked to the actual 2026 draft result"))
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
                hovertemplate = sprintf("Current %s<br>WS: %%{x:.1f}<br>Density: %%{y:.3f}<extra></extra>", hover_label)) %>%
      add_lines(data = new_density, x = ~x, y = ~density,
                name = "3-2-1", fill = "tozeroy",
                line = list(color = "#f59e0b", width = 2.5),
                hovertemplate = sprintf("3-2-1 %s<br>WS: %%{x:.1f}<br>Density: %%{y:.3f}<extra></extra>", hover_label)) %>%
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
      arrange(year, label)
  }

  observeEvent(input$tm_teamA, {
    opts <- picks_owned_by(input$tm_teamA)
    updateSelectizeInput(session, "tm_picksA",
      choices = setNames(opts$display_asset_id, opts$label), server = TRUE)
  })
  observeEvent(input$tm_teamB, {
    opts <- picks_owned_by(input$tm_teamB)
    updateSelectizeInput(session, "tm_picksB",
      choices = setNames(opts$display_asset_id, opts$label), server = TRUE)
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
          tags$div(tags$strong(r$label),
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
          tags$strong(r$label),
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
    rep(TRUE, length(pos))
  }

  zero_trade_vec <- function() rep(0, nrow(asset_new_draws))

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
      rc_slot <- team_slot_new_draws[, receiver, yc]
      val_og <- slot_value_vec(og_slot)
      val_rc <- slot_value_vec(rc_slot)
      eligible <- pick_conveys_app(og_slot, prot)
      sent_pick_is_better <- !is.na(og_slot) & !is.na(rc_slot) & og_slot < rc_slot
      out <- ifelse(eligible & sent_pick_is_better, pmax(val_og - val_rc, 0), 0)
      out[is.na(out)] <- 0
      return(out)
    }

    if (!is.null(prot) && prot != "none") {
      N <- protection_floor_app(prot)
      og_slot <- asset_slot_new_draws[, aid]
      raw_ev <- slot_value_vec(og_slot)
      out <- ifelse(!is.na(og_slot) & og_slot > N, raw_ev, 0)
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

  receiver_raw_outcome_vec <- function(receiver, yr) {
    cand <- pick_assets %>%
      filter(.data$year == as.integer(.env$yr), .data$original_team == .env$receiver)
    if (nrow(cand) > 0) {
      aid_receiver <- cand$asset_id[1]
      if (aid_receiver %in% colnames(asset_raw_new_draws)) {
        out <- asset_raw_new_draws[, aid_receiver]
        out[is.na(out)] <- 0
        return(out)
      }
    }

    yc <- as.character(yr)
    if (!is.null(dimnames(team_slot_new_draws)[[2]]) &&
        receiver %in% dimnames(team_slot_new_draws)[[2]] &&
        yc %in% dimnames(team_slot_new_draws)[[3]]) {
      out <- slot_value_vec(team_slot_new_draws[, receiver, yc])
      out[is.na(out)] <- 0
      return(out)
    }

    zero_trade_vec()
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
      rc_slot <- team_slot_new_draws[, receiver, yc]
      raw_og <- asset_raw_new_draws[, aid]
      raw_rc <- receiver_raw_outcome_vec(receiver, r$year)
      eligible <- pick_conveys_app(og_slot, prot)
      sent_pick_is_better <- !is.na(og_slot) & !is.na(rc_slot) & og_slot < rc_slot
      out <- ifelse(eligible & sent_pick_is_better, raw_og - raw_rc, 0)
      out[is.na(out)] <- 0
      return(out)
    }

    if (!is.null(prot) && prot != "none") {
      N <- protection_floor_app(prot)
      og_slot <- asset_slot_new_draws[, aid]
      raw_outcome <- asset_raw_new_draws[, aid]
      out <- ifelse(!is.na(og_slot) & og_slot > N, raw_outcome, 0)
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

  trade_draws <- reactive({
    idsA <- input$tm_picksA
    idsB <- input$tm_picksB
    if ((is.null(idsA) || length(idsA) == 0) &&
        (is.null(idsB) || length(idsB) == 0)) return(NULL)

    A_out_ev <- side_out_ev_value(idsA, "A", receiver = input$tm_teamB)
    B_out_ev <- side_out_ev_value(idsB, "B", receiver = input$tm_teamA)

    A_out_outcome <- side_outcome_value(idsA, "A", receiver = input$tm_teamB)
    B_out_outcome <- side_outcome_value(idsB, "B", receiver = input$tm_teamA)

    tibble(
      net_to_A_ev      = B_out_ev - A_out_ev,
      net_to_B_ev      = A_out_ev - B_out_ev,
      net_to_A_outcome = B_out_outcome - A_out_outcome,
      net_to_B_outcome = A_out_outcome - B_out_outcome
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

    winner <- if (abs(eA_ev) < 1e-8) "Neither team" else if (eA_ev > 0) input$tm_teamA else input$tm_teamB
    wcol   <- if (abs(eA_ev) < 1e-8) "#aaa" else if (eA_ev > 0) "#10b981" else "#ef4444"
    edge_team <- if (abs(eA_ev) < 1e-8) "neither team" else if (eA_ev > 0) input$tm_teamA else input$tm_teamB
    edge_val  <- abs(eA_ev)

    ev_prob_text <- if (pTie_ev > 0.001) {
      sprintf("P(%s higher EV) = %.0f%% | P(%s higher EV) = %.0f%% | tie = %.0f%%",
              input$tm_teamA, 100 * pA_ev,
              input$tm_teamB, 100 * pB_ev,
              100 * pTie_ev)
    } else {
      sprintf("P(%s higher EV) = %.0f%% | P(%s higher EV) = %.0f%%",
              input$tm_teamA, 100 * pA_ev,
              input$tm_teamB, 100 * pB_ev)
    }

    outcome_prob_text <- if (pTie_outcome > 0.001) {
      sprintf("P(%s's received pick(s) produce more realized 4-yr WS) = %.0f%% | P(%s) = %.0f%% | tie = %.0f%%",
              input$tm_teamA, 100 * pA_outcome,
              input$tm_teamB, 100 * pB_outcome,
              100 * pTie_outcome)
    } else {
      sprintf("P(%s's received pick(s) produce more realized 4-yr WS) = %.0f%% | P(%s) = %.0f%%",
              input$tm_teamA, 100 * pA_outcome,
              input$tm_teamB, 100 * pB_outcome)
    }

    tags$div(style = "font-size:12px; color:#bbb; line-height:1.8;",
      tags$div(style = "font-size:14px;",
        tags$strong(style = sprintf("color:%s;", wcol),
          sprintf("Expected asset edge to %s", edge_team)),
        sprintf(" — %+.1f WS to %s", edge_val, edge_team)),
      tags$div(sprintf("90%% posterior interval on expected asset edge to %s: [%+.1f, %+.1f] WS",
                       input$tm_teamA, q05_ev, q95_ev)),
      tags$div(ev_prob_text),
      tags$hr(style = "border-color:#1a1a2a; margin:8px 0;"),
      tags$div(tags$strong("Realized player-outcome simulation")),
      tags$div(sprintf("Mean realized-outcome net to %s: %+.1f WS | 90%% interval: [%+.1f, %+.1f] WS",
                       input$tm_teamA, eA_outcome, q05_outcome, q95_outcome)),
      tags$div(outcome_prob_text),
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
                hovertemplate = "Net WS: %{x:.1f}<br>Density: %{y:.3f}<extra></extra>") %>%
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

