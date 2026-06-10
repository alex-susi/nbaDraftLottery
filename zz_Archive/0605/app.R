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

has_igraph <- requireNamespace("igraph", quietly = TRUE)

if (!file.exists("dashboard_data.rds")) {
  stop("dashboard_data.rds not found. Run nba_lottery.R first.")
}

dd <- readRDS("dashboard_data.rds")

summary_df   <- dd$summary
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
team_slot_cur_draws     <- dd$team_slot_cur_draws
team_slot_new_draws     <- dd$team_slot_new_draws
sim_curve_par_draws     <- dd$sim_curve_par_draws
proj_years              <- dd$proj_years

# value a draft slot under each kept sim's pick-value curve (power-law form,
# matching nba_lottery.R's sampler). Vectorized over sims; deterministic mean
# (no extra noise) so hypothetical-protection deltas are stable.
slot_value_vec <- function(slot_vec) {
  a  <- sim_curve_par_draws[, "alpha"]
  b  <- sim_curve_par_draws[, "beta"]
  g  <- sim_curve_par_draws[, "gamma"]
  pmax(0, a / (slot_vec ^ b) + g)
}

# Team abbreviations present in the asset registry, sorted
all_team_abbr <- sort(unique(c(pick_assets$owner, pick_assets$original_team)))

# protection options offered in the Trade Machine (top-N bands; 12-15 illegal)
protection_choices <- c(
  "None (unprotected)" = "none",
  "Top-1"  = "top1",  "Top-3"  = "top3",  "Top-4"  = "top4",
  "Top-5"  = "top5",  "Top-6"  = "top6",  "Top-8"  = "top8",
  "Top-10" = "top10", "Lottery (top-14)" = "lottery",
  "Top-16" = "top16", "Top-20" = "top20"
)
protection_floor_app <- function(p) {
  switch(p, top1 = 1, top3 = 3, top4 = 4, top5 = 5, top6 = 6,
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
        card_header("Change in Expected Draft-Asset Value (4-yr WS, 2026-2032)"),
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
    card(
      card_header("All 30 Teams — click a row for detail"),
      DTOutput("full_table")
    ),
    uiOutput("team_detail")
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
      card(
        card_header("Value Distribution (4-yr Win Shares)"),
        plotlyOutput("sp_dist", height = "330px")
      )
    )
  ),

  # ---- Tab 7: Trade Machine ----
  nav_panel(
    title = "Trade Machine",
    icon  = icon("right-left"),
    div(style = "padding:6px 4px 10px;",
      checkboxInput("tm_obligations",
        "Allow attaching protections / swap rights to picks being sent",
        value = FALSE),
      tags$small(style = "color:#888;",
        "When on, each selected pick gets a protection dropdown and a ",
        "swap-right toggle below. Protections in the 12-15 band are ",
        "disallowed under the approved rules and are not offered.")
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
    card(
      card_header("Net Value Transferred to Each Team (per simulation)"),
      plotlyOutput("tm_dist", height = "300px")
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
  output$impact_chart <- renderPlotly({
    sc <- input$impact_sort
    asc <- sc == "wins"
    df <- summary_df %>%
      arrange(if (asc) !!sym(sc) else desc(!!sym(sc))) %>%
      mutate(team_fct = factor(team, levels = rev(team)))
    plot_ly(
      data = df, y = ~team_fct, x = ~delta_value,
      type = "bar", orientation = "h",
      marker = list(color = ~ifelse(delta_value >= 0, "#059669", "#dc2626")),
      hovertemplate = "<b>%{y}</b><br>Delta: %{x:.1f} WS<extra></extra>"
    ) %>%
      plotly_dark(
        xaxis = list(title = "Delta 4-yr WS", zerolinecolor = "#444"),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 52)
      )
  })

  # ---- Compare ----
  output$compare_title <- renderText({
    switch(input$compare_metric,
      total    = "Total Portfolio Value (4-yr WS, 2026-2032)",
      quality  = "Best Single Pick (expected 4-yr WS)",
      quantity = "Average Number of Picks Owned")
  })

  output$compare_chart <- renderPlotly({
    df <- summary_df %>%
      arrange(desc(current_mean)) %>%
      mutate(team_fct = factor(team, levels = rev(team)))
    if (input$compare_metric == "total") {
      cur <- df$current_mean; new <- df$new_mean
    } else if (input$compare_metric == "quality") {
      cur <- df$best_current; new <- df$best_new
    } else {
      cur <- df$n_picks_mean; new <- df$n_picks_mean
    }
    plot_ly(data = df, y = ~team_fct) %>%
      add_bars(x = cur, name = "Current",
               marker = list(color = "#3b82f6"), orientation = "h") %>%
      add_bars(x = new, name = "3-2-1",
               marker = list(color = "#f59e0b"), orientation = "h") %>%
      plotly_dark(
        barmode = "group",
        xaxis = list(title = ""),
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
  output$full_table <- renderDT({
    tbl <- summary_df %>%
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
    t <- summary_df[sel, ]
    card(class = "mt-2",
      card_header(style = "border-left:3px solid #6d28d9;",
        sprintf("%s — %s (%d-%d)", t$team, tier_labels[t$tier], t$wins, t$losses)),
      card_body(style = "font-size:12px; color:#bbb; line-height:1.8;",
        tags$div(tags$strong("Current: "),
          sprintf("E[value]=%.1f +/- %.1f | 90%% CI [%.0f, %.0f] | best %.1f",
                  t$current_mean, t$current_sd, t$current_q05, t$current_q95,
                  t$best_current)),
        tags$div(tags$strong("3-2-1: "),
          sprintf("E[value]=%.1f +/- %.1f | 90%% CI [%.0f, %.0f] | best %.1f",
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
      add_ribbons(ymin = ~pmax(0, expected_war - war_sd),
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
      tags$div(sprintf("90%% PPC slot coverage = %.0f%%", pm$ppc_cover * 100)),
      tags$div(sprintf("alpha=%.1f beta=%.3f gamma=%.1f", pm$alpha, pm$beta, pm$gamma)),
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
    opts <- pick_assets %>%
      filter(year == input$sp_year) %>%
      arrange(owner, original_team)
    choice_vec <- setNames(opts$asset_id, opts$label)
    updateSelectInput(session, "sp_asset", choices = choice_vec)
  })

  sp_row <- reactive({
    req(input$sp_asset)
    pick_assets %>% filter(asset_id == input$sp_asset)
  })

  sp_stats <- reactive({
    req(input$sp_asset)
    pvs <- pick_value_summary %>% filter(asset_id == input$sp_asset)
    pvs
  })

  output$sp_obligation <- renderUI({
    r <- sp_row()
    if (nrow(r) == 0) return(NULL)
    via_txt <- if (r$is_traded) {
      sprintf("Originally %s's pick, now owned by %s", r$original_team, r$owner)
    } else {
      sprintf("%s's own pick", r$owner)
    }
    tags$div(style = "font-size:11px; color:#aaa; line-height:1.7;",
      tags$div(tags$strong(style = "color:#6d28d9;", "Pick details")),
      tags$div(via_txt),
      tags$div(sprintf("Obligation: %s", r$obligation)),
      if (r$pick_type == "swap")
        tags$div(style = "color:#f59e0b;", "Swap: owner takes the more favorable slot"),
      if (r$year == 2026)
        tags$div(style = "color:#10b981;",
                 sprintf("Locked at pick #%d (2026 actual)", r$fixed_slot)))
  })

  output$sp_headline <- renderUI({
    s <- sp_stats()
    if (nrow(s) == 0) return(NULL)
    delta <- s$new_mean - s$cur_mean
    box <- function(title, mean, q05, q95, col) {
      tags$div(style = sprintf(
        "flex:1; padding:12px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;", col),
        tags$div(style = "font-size:11px; color:#888;", title),
        tags$div(style = sprintf("font-size:26px; font-weight:700; color:%s;", col),
                 sprintf("%.1f", mean)),
        tags$div(style = "font-size:11px; color:#aaa;",
                 sprintf("90%% CI: [%.1f, %.1f]", q05, q95)))
    }
    tags$div(style = "display:flex; gap:12px; align-items:stretch;",
      box("Current system E[value]", s$cur_mean, s$cur_q05, s$cur_q95, "#3b82f6"),
      box("3-2-1 system E[value]",   s$new_mean, s$new_q05, s$new_q95, "#f59e0b"),
      tags$div(style = sprintf(
        "flex:1; padding:12px; border:1px solid #1a1a2a; border-radius:8px; border-left:3px solid %s;",
        ifelse(delta >= 0, "#10b981", "#ef4444")),
        tags$div(style = "font-size:11px; color:#888;", "Delta (3-2-1 - current)"),
        tags$div(style = sprintf("font-size:26px; font-weight:700; color:%s;",
                                 ifelse(delta >= 0, "#10b981", "#ef4444")),
                 sprintf("%+.1f", delta)),
        tags$div(style = "font-size:11px; color:#aaa;", "expected 4-yr Win Shares")))
  })

  output$sp_dist <- renderPlotly({
    req(input$sp_asset)
    cur <- asset_cur_draws[, input$sp_asset]
    new <- asset_new_draws[, input$sp_asset]
    s   <- sp_stats()
    plot_ly() %>%
      add_histogram(x = cur, name = "Current", opacity = 0.6,
                    marker = list(color = "#3b82f6"), nbinsx = 40) %>%
      add_histogram(x = new, name = "3-2-1", opacity = 0.6,
                    marker = list(color = "#f59e0b"), nbinsx = 40) %>%
      layout(
        barmode = "overlay",
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "#0f0f1a",
        font = list(family = "IBM Plex Mono", color = "#999"),
        xaxis = list(title = "4-yr Win Shares", gridcolor = "#1a1a2a"),
        yaxis = list(title = "Simulations", gridcolor = "#1a1a2a"),
        legend = list(x = 0.75, y = 0.95, font = list(size = 10)),
        shapes = list(
          list(type = "line", x0 = s$cur_mean, x1 = s$cur_mean, y0 = 0, y1 = 1,
               yref = "paper", line = list(color = "#3b82f6", dash = "dash")),
          list(type = "line", x0 = s$new_mean, x1 = s$new_mean, y0 = 0, y1 = 1,
               yref = "paper", line = list(color = "#f59e0b", dash = "dash"))))
  })

  # ==========================================================================
  # TAB 7: TRADE MACHINE
  # ==========================================================================

  output$tm_teamA_hdr <- renderText(sprintf("Team A — %s", input$tm_teamA))
  output$tm_teamB_hdr <- renderText(sprintf("Team B — %s", input$tm_teamB))

  # picks each team currently OWNS (can send out)
  picks_owned_by <- function(team) {
    pick_assets %>%
      filter(owner == team) %>%
      arrange(year, original_team)
  }

  observeEvent(input$tm_teamA, {
    opts <- picks_owned_by(input$tm_teamA)
    updateSelectizeInput(session, "tm_picksA",
      choices = setNames(opts$asset_id, opts$label), server = TRUE)
  })
  observeEvent(input$tm_teamB, {
    opts <- picks_owned_by(input$tm_teamB)
    updateSelectizeInput(session, "tm_picksB",
      choices = setNames(opts$asset_id, opts$label), server = TRUE)
  })

  # ---- per-pick obligation controls (only when the toggle is on) ----
  # For each selected pick we render a protection dropdown + a swap checkbox.
  # Input ids are deterministic: prot_<side>_<assetid>, swap_<side>_<assetid>.
  safe_id <- function(x) gsub("[^A-Za-z0-9]", "_", x)

  obligation_controls <- function(ids, side) {
    if (!isTRUE(input$tm_obligations) || is.null(ids) || length(ids) == 0)
      return(NULL)
    rows <- pick_assets %>% filter(asset_id %in% ids)
    tags$div(style = "border-top:1px solid #1a1a2a; margin-top:8px; padding-top:8px;",
      tags$div(style = "font-size:11px; color:#6d28d9; margin-bottom:4px;",
               "Attach obligations to picks being sent"),
      lapply(seq_len(nrow(rows)), function(i) {
        r   <- rows[i, ]
        pid <- paste0("prot_", side, "_", safe_id(r$asset_id))
        sid <- paste0("swap_", side, "_", safe_id(r$asset_id))
        # 2026 picks are locked at a known slot; obligations are moot there.
        locked <- r$year == 2026
        tags$div(style = "margin-bottom:8px; font-size:11px; color:#aaa;",
          tags$div(tags$strong(r$label),
            if (locked) tags$span(style = "color:#10b981;",
                                  " (2026 locked — obligations N/A)")),
          if (!locked) tags$div(style = "display:flex; gap:8px; align-items:center;",
            tags$div(style = "flex:1;",
              selectInput(pid, NULL, choices = protection_choices,
                          selected = "none", width = "100%")),
            tags$div(style = "flex:1;",
              checkboxInput(sid, "Swap right (take better slot)", value = FALSE))))
      }))
  }
  output$tm_obligA <- renderUI(obligation_controls(input$tm_picksA, "A"))
  output$tm_obligB <- renderUI(obligation_controls(input$tm_picksB, "B"))

  # detail panels listing the EXISTING obligations on selected picks
  render_pick_detail <- function(ids) {
    if (is.null(ids) || length(ids) == 0)
      return(tags$div(style = "font-size:11px; color:#666;", "No picks selected"))
    rows <- pick_assets %>% filter(asset_id %in% ids)
    tags$div(style = "font-size:11px; color:#aaa; line-height:1.6;",
      lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        tags$div(
          tags$strong(r$label),
          tags$span(style = "color:#888;", sprintf(" [%s]", r$obligation)))
      }))
  }
  output$tm_picksA_detail <- renderUI(render_pick_detail(input$tm_picksA))
  output$tm_picksB_detail <- renderUI(render_pick_detail(input$tm_picksB))

  # ---- value (per sim) of a single sent pick TO ITS RECEIVER ----
  # base = the asset value as it exists today (asset_new_draws column).
  # If the user attaches obligations (toggle on) we recompute:
  #   * protection top-N: receiver gets raw value only if original slot > N,
  #                       else 0 (pick does not convey).
  #   * swap: receiver takes the more favorable (lower) of the receiver's own
  #           seat and the original team's seat that year, valued on the curve.
  # Swap takes precedence over protection if both are set on the same pick.
  sent_pick_value <- function(aid, side, receiver) {
    base <- asset_new_draws[, aid]
    if (!isTRUE(input$tm_obligations)) return(base)

    r <- pick_assets %>% filter(asset_id == aid)
    if (nrow(r) == 0 || r$year == 2026) return(base)   # 2026 locked

    pid <- paste0("prot_", side, "_", safe_id(aid))
    sid <- paste0("swap_", side, "_", safe_id(aid))
    prot <- input[[pid]]; swap <- isTRUE(input[[sid]])
    yc   <- as.character(r$year)

    if (swap) {
      og_slot <- asset_slot_new_draws[, aid]               # original team seat
      rc_slot <- team_slot_new_draws[, receiver, yc]       # receiver own seat
      # take the better (smaller) available seat, ignoring NAs
      best <- pmin(og_slot, rc_slot, na.rm = TRUE)
      best[is.infinite(best)] <- NA   # both seats missing that sim
      val  <- slot_value_vec(best)
      val[is.na(best)] <- 0
      return(val)
    }
    if (!is.null(prot) && prot != "none") {
      N   <- protection_floor_app(prot)
      og  <- asset_slot_new_draws[, aid]
      raw <- asset_raw_new_draws[, aid]
      out <- ifelse(!is.na(og) & og > N, raw, 0)
      return(out)
    }
    base
  }

  # sum sent-pick values for a side (vector over sims)
  side_out_value <- function(ids, side, receiver) {
    if (is.null(ids) || length(ids) == 0)
      return(rep(0, nrow(asset_new_draws)))
    cols <- vapply(ids, function(aid) sent_pick_value(aid, side, receiver),
                   numeric(nrow(asset_new_draws)))
    if (is.null(dim(cols))) return(cols)
    rowSums(cols)
  }

  # Joint per-sim net value transferred (A receives B's sent picks and vice versa).
  trade_draws <- reactive({
    idsA <- input$tm_picksA
    idsB <- input$tm_picksB
    if ((is.null(idsA) || length(idsA) == 0) &&
        (is.null(idsB) || length(idsB) == 0)) return(NULL)

    # A sends idsA (received by B); B sends idsB (received by A)
    A_out <- side_out_value(idsA, "A", receiver = input$tm_teamB)
    B_out <- side_out_value(idsB, "B", receiver = input$tm_teamA)
    tibble(
      net_to_A_new = B_out - A_out,    # A gains what B sends, loses what A sends
      net_to_B_new = A_out - B_out
    )
  })

  output$tm_verdict <- renderUI({
    d <- trade_draws()
    if (is.null(d)) return(tags$div(style = "color:#666;",
      "Select picks from one or both teams to assess the trade."))
    eA  <- mean(d$net_to_A_new)
    q05 <- quantile(d$net_to_A_new, 0.05)
    q95 <- quantile(d$net_to_A_new, 0.95)
    pA  <- mean(d$net_to_A_new > 0)
    winner <- if (eA > 0) input$tm_teamA else input$tm_teamB
    wcol   <- if (eA > 0) "#10b981" else "#ef4444"
    obl <- if (isTRUE(input$tm_obligations))
      " Hypothetical protections / swaps are applied to the sent picks." else ""
    tags$div(style = "font-size:12px; color:#bbb; line-height:1.8;",
      tags$div(style = "font-size:14px;",
        tags$strong(style = sprintf("color:%s;", wcol),
          sprintf("Edge to %s", winner)),
        sprintf(" — expected net %+.1f WS to %s", eA, input$tm_teamA)),
      tags$div(sprintf("90%% CI on net value to %s: [%+.1f, %+.1f] WS",
                       input$tm_teamA, q05, q95)),
      tags$div(sprintf("P(%s nets more future wins) = %.0f%%   |   P(%s) = %.0f%%",
                       input$tm_teamA, 100 * pA,
                       input$tm_teamB, 100 * (1 - pA))),
      tags$div(style = "color:#888; margin-top:6px;",
        sprintf("A sends %d pick(s); B sends %d pick(s). Values are 4-yr Win Shares under the 3-2-1 system, with within-simulation correlation so both sides move together.%s",
                length(input$tm_picksA %||% character(0)),
                length(input$tm_picksB %||% character(0)), obl)))
  })

  output$tm_dist <- renderPlotly({
    d <- trade_draws()
    if (is.null(d)) return(plotly_empty())
    plot_ly() %>%
      add_histogram(x = d$net_to_A_new, nbinsx = 50,
                    marker = list(color = "#6d28d9"),
                    name = sprintf("Net to %s", input$tm_teamA)) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "#0f0f1a",
        font = list(family = "IBM Plex Mono", color = "#999"),
        xaxis = list(title = sprintf("Net 4-yr WS transferred to %s (>0 favors %s)",
                                     input$tm_teamA, input$tm_teamA),
                     gridcolor = "#1a1a2a", zerolinecolor = "#f59e0b"),
        yaxis = list(title = "Simulations", gridcolor = "#1a1a2a"),
        shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1,
                           yref = "paper",
                           line = list(color = "#f59e0b", dash = "dash"))))
  })
}

shinyApp(ui = ui, server = server)

