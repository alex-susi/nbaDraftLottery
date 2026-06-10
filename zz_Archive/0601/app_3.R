################################################################################
# NBA 3-2-1 Lottery Reform — Shiny Dashboard
#
# Reads dashboard_data.rds produced by nba_lottery.R (v4)
#
# Prerequisites:
#   install.packages(c("shiny", "plotly", "DT", "bslib"))
################################################################################

library(shiny)
library(plotly)
library(DT)
library(bslib)
library(tidyverse)

if (!file.exists("dashboard_data.rds")) {
  stop("dashboard_data.rds not found. Run nba_lottery.R first.")
}

dd <- readRDS("dashboard_data.rds")

summary_df   <- dd$summary
lottery_dist <- dd$lottery_dist
pick_curve   <- dd$pick_curve
meta         <- dd$metadata
stan_diag    <- dd$stan_diagnostics

tier_colors <- c(
  contender   = "#059669",
  playoff     = "#2563eb",
  playin      = "#7c3aed",
  mid_lottery = "#ca8a04",
  bottom      = "#dc2626"
)

tier_labels <- c(
  contender   = "Contender",
  playoff     = "Playoff",
  playin      = "Play-In",
  mid_lottery = "Mid Lottery",
  bottom      = "Bottom"
)

app_theme <- bs_theme(
  bg         = "#0a0a14",
  fg         = "#d0d0d0",
  primary    = "#6d28d9",
  base_font  = font_google("IBM Plex Mono"),
  font_scale = 0.85,
  "navbar-bg" = "#0f0f1a"
)

plotly_layout_dark <- function(p, ...) {
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

ui <- page_navbar(
  theme  = app_theme,
  title  = "NBA 3-2-1 Lottery Reform",
  header = div(
    style = "text-align:center; padding:8px 0 4px; border-bottom:1px solid #1a1a2a;",
    tags$small(
      style = "color:#666;",
      sprintf("Bayesian Markov Chain (5-tier) x %sK MC Sims | %s",
              format(meta$n_sims / 1000, nsmall = 0), meta$system_note)
    )
  ),

  nav_panel(
    title = "Impact",
    icon  = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        selectInput("impact_sort", "Sort by",
          choices  = c("Rule change impact" = "delta_value",
                       "Current value" = "current_mean",
                       "Proposed value" = "new_mean",
                       "Record (worst)" = "wins"),
          selected = "delta_value"),
        hr(), h6("Key Findings"),
        p(style = "font-size:11px; color:#888; line-height:1.6;",
          "Bottom-tier teams lose expected value under 3-2-1. ",
          "Mid-lottery and play-in teams gain. ",
          "2026 is identical under both systems. ",
          "Divergence compounds 2027-2032 via Markov chain.")
      ),
      card(
        card_header("Change in Expected Draft Asset Portfolio (WAR, 2026-2032)"),
        plotlyOutput("impact_chart", height = "600px"))
    )
  ),

  nav_panel(
    title = "Side by Side",
    icon  = icon("columns"),
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        radioButtons("compare_metric", "Metric",
          choices  = c("Total portfolio WAR" = "total",
                       "Best single pick" = "quality",
                       "Number of picks" = "quantity"),
          selected = "total")
      ),
      card(card_header(textOutput("compare_title")),
           plotlyOutput("compare_chart", height = "600px"))
    )
  ),

  nav_panel(
    title = "Lottery Odds",
    icon  = icon("dice"),
    layout_columns(
      col_widths = c(12, 12),
      card(card_header("Expected Pick Position by Lottery Seed"),
           plotlyOutput("lottery_line", height = "280px")),
      card(card_header("Probability of #1 Pick by Seed (%)"),
           plotlyOutput("lottery_bar", height = "280px"))
    ),
    card(class = "mt-2", card_body(
      style = "font-size:11px; color:#888; line-height:1.6;",
      tags$strong(style = "color:#f59e0b;", "The inversion:"),
      " Under 3-2-1, seeds 4-10 (3 balls) have ~8.5% chance at #1 ",
      "vs bottom 3 at ~5.7% (2 balls). All 16 positions drawn."))
  ),

  nav_panel(
    title = "Full Table",
    icon  = icon("table"),
    card(card_header("All 30 Teams - Click a row for details"),
         DTOutput("full_table")),
    uiOutput("team_detail_panel")
  ),

  nav_panel(
    title = "Markov Chain",
    icon  = icon("project-diagram"),
    layout_columns(
      col_widths = c(12, 12),
      card(card_header("Empirical Tier-to-Tier Transition Probabilities (%)"),
           DTOutput("trans_matrix_table")),
      card(card_header("Pick Value Curve (Posterior Mean +/- 1 SD)"),
           plotlyOutput("pick_curve_chart", height = "300px"))
    ),
    card(class = "mt-2", card_body(
      style = "font-size:11px; color:#888; line-height:1.6;",
      tags$strong(style = "color:#7c3aed;", "Methodology: "),
      "Teams classified into 5 tiers following the dribble analytics ",
      "Markov chain framework. Transition probabilities estimated with ",
      "Dirichlet-Multinomial Bayesian model. Each MC sim draws a ",
      "transition matrix from the posterior and evolves teams 7 years."))
  ),

  nav_spacer(),
  nav_item(tags$small(
    style = "color:#444; font-size:9px;",
    sprintf("Pick: a=%.1f b=%.4f g=%.1f | %d picks tracked",
            stan_diag$pick_model$alpha,
            stan_diag$pick_model$beta,
            stan_diag$pick_model$gamma,
            meta$n_picks)))
)

server <- function(input, output, session) {

  output$impact_chart <- renderPlotly({
    sort_col  <- input$impact_sort
    ascending <- sort_col == "wins"
    df <- summary_df %>%
      arrange(if (ascending) !!sym(sort_col) else desc(!!sym(sort_col))) %>%
      mutate(team_fct  = factor(team, levels = rev(team)),
             bar_color = ifelse(delta_value >= 0, "#059669", "#dc2626"))
    plot_ly(data = df, y = ~team_fct, x = ~delta_value,
            type = "bar", orientation = "h",
            marker = list(color = ~bar_color),
            hovertemplate = "<b>%{y}</b><br>Delta WAR: %{x:.1f}<extra></extra>") %>%
      plotly_layout_dark(
        xaxis  = list(title = "Delta WAR", gridcolor = "#1a1a2a", zerolinecolor = "#444"),
        yaxis  = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 50))
  })

  output$compare_title <- renderText({
    switch(input$compare_metric,
           "total" = "Total Portfolio WAR (2026-2032)",
           "quality" = "Best Single Pick E[WAR]",
           "quantity" = "Average Picks Owned")
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
      add_bars(x = cur, name = "Current", marker = list(color = "#3b82f6"), orientation = "h") %>%
      add_bars(x = new, name = "3-2-1", marker = list(color = "#f59e0b"), orientation = "h") %>%
      plotly_layout_dark(barmode = "group",
        xaxis = list(title = ""), yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 50), legend = list(x = 0.7, y = 0.05, font = list(size = 10)))
  })

  output$lottery_line <- renderPlotly({
    cur <- lottery_dist %>% filter(system == "Current", seed <= 10)
    new <- lottery_dist %>% filter(system == "Proposed 3-2-1", seed <= 10)
    plot_ly() %>%
      add_trace(data = cur, x = ~seed, y = ~expected_pick, type = "scatter",
                mode = "lines+markers", name = "Current",
                line = list(color = "#3b82f6", width = 3),
                marker = list(color = "#3b82f6", size = 8)) %>%
      add_trace(data = new, x = ~seed, y = ~expected_pick, type = "scatter",
                mode = "lines+markers", name = "3-2-1",
                line = list(color = "#f59e0b", width = 3),
                marker = list(color = "#f59e0b", size = 8)) %>%
      plotly_layout_dark(
        xaxis = list(title = "Lottery Seed", dtick = 1, tickvals = 1:10),
        yaxis = list(title = "E[Pick]", autorange = "reversed"),
        legend = list(x = 0.05, y = 0.05, font = list(size = 10)))
  })

  output$lottery_bar <- renderPlotly({
    cur <- lottery_dist %>% filter(system == "Current", seed <= 10)
    new <- lottery_dist %>% filter(system == "Proposed 3-2-1", seed <= 10)
    plot_ly() %>%
      add_bars(data = cur, x = ~seed, y = ~(prob_no1 * 100),
               name = "Current", marker = list(color = "#3b82f6")) %>%
      add_bars(data = new, x = ~seed, y = ~(prob_no1 * 100),
               name = "3-2-1", marker = list(color = "#f59e0b")) %>%
      plotly_layout_dark(barmode = "group",
        xaxis = list(title = "Lottery Seed", dtick = 1),
        yaxis = list(title = "P(#1) %"),
        legend = list(x = 0.7, y = 0.95, font = list(size = 10)))
  })

  output$full_table <- renderDT({
    tbl <- summary_df %>%
      mutate(
        Tier      = tier_labels[tier],
        Record    = sprintf("%d-%d", wins, losses),
        `#Pk`     = round(n_picks_mean, 1),
        Current   = round(current_mean, 1),
        `3-2-1`   = round(new_mean, 1),
        `D WAR`   = round(delta_value, 1),
        `D%`      = sprintf("%+.1f%%", delta_pct),
        `s Cur`   = round(current_sd, 1),
        `s New`   = round(new_sd, 1),
        `90% Cur` = sprintf("[%.0f, %.0f]", current_q05, current_q95),
        `90% New` = sprintf("[%.0f, %.0f]", new_q05, new_q95)
      ) %>%
      select(Team = team, Tier, Record, `#Pk`, Current, `3-2-1`,
             `D WAR`, `D%`, `s Cur`, `s New`, `90% Cur`, `90% New`)
    datatable(tbl, selection = "single", rownames = FALSE,
              options = list(pageLength = 30, dom = "t", ordering = TRUE,
                             columnDefs = list(list(className = "dt-right", targets = 3:11)))) %>%
      formatStyle("D WAR", color = styleInterval(0, c("#ef4444", "#10b981"))) %>%
      formatStyle("Tier", color = styleEqual(tier_labels, tier_colors))
  })

  output$team_detail_panel <- renderUI({
    sel <- input$full_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    t <- summary_df[sel, ]
    card(class = "mt-2",
      card_header(style = "border-left:3px solid #6d28d9;",
                  sprintf("%s - %s (%d-%d)", t$team, t$tier, t$wins, t$losses)),
      card_body(style = "font-size:12px; color:#bbb; line-height:1.8;",
        tags$table(style = "width:100%;",
          tags$tr(tags$td(tags$strong("Current: "),
            sprintf("E[WAR] = %.1f +/- %.1f | 90%% CI: [%.0f, %.0f] | Best: %.1f",
                    t$current_mean, t$current_sd, t$current_q05, t$current_q95, t$best_current))),
          tags$tr(tags$td(tags$strong("3-2-1: "),
            sprintf("E[WAR] = %.1f +/- %.1f | 90%% CI: [%.0f, %.0f] | Best: %.1f",
                    t$new_mean, t$new_sd, t$new_q05, t$new_q95, t$best_new))),
          tags$tr(tags$td(tags$strong(
            style = ifelse(t$delta_value >= 0, "color:#10b981;", "color:#ef4444;"),
            sprintf("Impact: %+.1f WAR (%+.1f%%) | s change: %+.1f | Quality: %+.2f",
                    t$delta_value, t$delta_pct, t$sigma_change, t$delta_quality)))))))
  })

  output$trans_matrix_table <- renderDT({
    trans <- dd$transition_matrix
    tbl <- as.data.frame(round(trans * 100, 1))
    names(tbl) <- tier_labels[colnames(trans)]
    tbl$From <- tier_labels[rownames(trans)]
    tbl <- tbl %>% select(From, everything())
    datatable(tbl, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })

  output$pick_curve_chart <- renderPlotly({
    plot_ly(data = pick_curve, x = ~pick) %>%
      add_ribbons(ymin = ~pmax(0, expected_war - war_sd),
                  ymax = ~(expected_war + war_sd),
                  fillcolor = "rgba(109,40,217,0.15)",
                  line = list(color = "transparent"), name = "+/- 1 SD") %>%
      add_lines(y = ~expected_war, line = list(color = "#6d28d9", width = 2), name = "E[WAR]") %>%
      plotly_layout_dark(
        xaxis = list(title = "Pick Position", dtick = 5),
        yaxis = list(title = "Expected WAR"),
        legend = list(x = 0.6, y = 0.9, font = list(size = 10)))
  })
}

shinyApp(ui = ui, server = server)
