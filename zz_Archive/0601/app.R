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
}

shinyApp(ui = ui, server = server)
