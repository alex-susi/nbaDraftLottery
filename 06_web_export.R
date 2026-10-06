# =============================================================================
# 06_web_export.R
# Exports the 3-2-1 Trade Machine data for the static site in web/.
#
# - Loads app.R's prepared objects without launching the app. app.R stays the
#   reference, so pick labels, display grouping, and conveyance flags match it.
# - Writes to web/public/data/:
#     meta.json        picks, teams, labels, and block offsets
#     core.bin         per-simulation pick-value curves (mu_1..mu_60) and
#                      projected team slots for both rounds
#     teams/<ABB>.bin  draws for the picks each team owns, loaded on demand
#     methodology.json tables behind the Methodology tab's charts, plus its
#                      value-metric text and validation table as app.R renders them
#     summary.json     Summary tab numbers, team portfolios for every year /
#                      round filter, per-pick EPV changes, and the team table
#     single/<ABB>.json Single pick details and density curves for each pick a
#                      team owns, loaded on demand
# - Floats are little-endian float32 (NaN keeps R's NA); slots and flags are
#   uint8 with 0 = NA. Every block holds one value per retained simulation in
#   the export's row order, so draws stay aligned across files.
# - Run from the repository root after 04_lotterySims.R has written
#   01_data/dashboard_data.rds:  Rscript 06_web_export.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(shiny)
  library(xml2)
})

app_env <- new.env()
suppressMessages(sys.source("app.R", envir = app_env))
a <- app_env

n_draws    <- nrow(a$asset_new_draws)
proj_years <- as.integer(dimnames(a$team_slot_new_draws)[[3]])
slot_teams <- dimnames(a$team_slot_new_draws)[[2]]
teams      <- a$all_team_abbr
out_dir    <- file.path("web", "public", "data")

stopifnot(n_draws %% 4 == 0,
          nrow(a$sim_curve_par_draws) == n_draws,
          all(paste0("mu_", 1:60) %in% colnames(a$sim_curve_par_draws)),
          identical(dimnames(a$team_slot2_new_draws), dimnames(a$team_slot_new_draws)))

dir.create(file.path(out_dir, "teams"), recursive = TRUE, showWarnings = FALSE)


# ---- Display picks (Trade Machine choices) ----------------------------------
# Mirrors app.R: a display pick is single-leg when exactly one member asset has
# draws and the group is "single_asset"; only those take protection / swap
# toggles. Grouped entitlements use their display-level draws as-is.
leg_tbl <- a$pick_display_members %>%
  filter(.data$asset_id %in% colnames(a$asset_new_draws)) %>%
  group_by(.data$display_asset_id) %>%
  summarise(n_legs = n(),
            asset_id = first(.data$asset_id),
            .groups = "drop")

guaranteed_ids <- a$pick_display_assets$display_asset_id[a$is_guaranteed_display_entitlement_app(a$pick_display_assets)]
count_prob     <- colMeans(a$display_pick_count_new_draws > 0, na.rm = TRUE)

displays <- a$pick_display_assets %>%
  left_join(leg_tbl, by = "display_asset_id") %>%
  left_join(a$pick_assets %>% select(asset_id, fixed_slot),
            by = "asset_id") %>%
  mutate(n_legs = coalesce(.data$n_legs, 0L),
         single_leg = .data$n_legs == 1L & .data$group_type == "single_asset",
         slot_sort = suppressWarnings(as.integer(str_extract(.data$fixed_slot_display, "[0-9]+"))),
         guaranteed = .data$display_asset_id %in% guaranteed_ids,
         convey_prob_static = ifelse(.data$n_legs > 0L & (.data$guaranteed | !.data$single_leg),
                                     unname(count_prob[.data$display_asset_id]), NA_real_),
         fixed_slot = ifelse(.data$single_leg & .data$year == 2026, .data$fixed_slot, NA_integer_),
         asset_id = ifelse(.data$single_leg, .data$asset_id, NA_character_)) %>%
  arrange(.data$owner, .data$year, .data$round, coalesce(.data$slot_sort, 999L), .data$short_label)

stopifnot(!anyDuplicated(displays$display_asset_id),
          nrow(displays) == nrow(a$pick_display_assets),
          all(!is.na(displays$fixed_slot[displays$single_leg & displays$year == 2026])))

# Receiver's own pick for swap valuation: app.R takes the receiver's first
# own-and-original pick row (own type first) and falls back to the slot curve
# when that row has no raw outcome draws.
receiver_raw <- a$pick_assets %>%
  filter(.data$original_team == .data$owner,
         .data$year %in% proj_years) %>%
  arrange(.data$pick_type != "own") %>%
  group_by(.data$owner, .data$year, .data$round) %>%
  slice(1) %>%
  ungroup() %>%
  filter(.data$asset_id %in% colnames(a$asset_raw_new_draws)) %>%
  transmute(team = .data$owner,
            key = paste0(.data$year, "-", .data$round),
            asset_id = .data$asset_id)


# ---- Core file: pick-value curves and projected team slots ------------------
mu_mat <- as.matrix(a$sim_curve_par_draws[, paste0("mu_", 1:60), drop = FALSE])
slot1  <- a$team_slot_new_draws
slot2  <- a$team_slot2_new_draws
slot1[is.na(slot1)] <- 0
slot2[is.na(slot2)] <- 0

con <- file(file.path(out_dir, "core.bin"), "wb")
writeBin(as.double(t(mu_mat)), con, size = 4, endian = "little")   # [draw][slot]
writeBin(as.integer(slot1), con, size = 1)                         # [year][team][draw]
writeBin(as.integer(slot2), con, size = 1)
close(con)

core_layout <- list(
  mu         = list(offset = 0, length = n_draws * 60),
  team_slot1 = list(offset = n_draws * 60 * 4, length = length(slot1)),
  team_slot2 = list(offset = n_draws * 60 * 4 + length(slot1), length = length(slot2))
)


# ---- Team files: draws for each team's picks ---------------------------------
# Block keys: a:<asset_id>:<slot|convey|out|raw> for single-leg assets and
# d:<display_id>:<ev|out> for grouped display picks.
team_layout <- list()

for (tm in teams) {
  tm_displays <- displays %>% filter(.data$owner == .env$tm)
  leg_assets  <- tm_displays$asset_id[tm_displays$single_leg]
  raw_assets  <- setdiff(receiver_raw$asset_id[receiver_raw$team == tm], leg_assets)
  grouped     <- tm_displays$display_asset_id[!tm_displays$single_leg]

  blocks <- c(
    lapply(leg_assets, function(id) list(key = paste0("a:", id, ":out"), kind = "f32",
                                         v = a$asset_new_draws[, id])),
    lapply(c(leg_assets, raw_assets), function(id) list(key = paste0("a:", id, ":raw"), kind = "f32",
                                                        v = a$asset_raw_new_draws[, id])),
    lapply(grouped, function(id) list(key = paste0("d:", id, ":ev"), kind = "f32",
                                      v = coalesce(as.numeric(a$display_asset_new_ev_draws[, id]), 0))),
    lapply(grouped, function(id) list(key = paste0("d:", id, ":out"), kind = "f32",
                                      v = coalesce(as.numeric(a$display_asset_new_draws[, id]), 0))),
    lapply(leg_assets, function(id) list(key = paste0("a:", id, ":slot"), kind = "u8",
                                         v = coalesce(as.integer(a$asset_slot_new_draws[, id]), 0L))),
    lapply(leg_assets, function(id) list(key = paste0("a:", id, ":convey"), kind = "u8",
                                         v = as.integer(coalesce(a$asset_convey_new_draws[, id] > 0, FALSE))))
  )

  con <- file(file.path(out_dir, "teams", paste0(tm, ".bin")), "wb")
  offset <- 0
  index  <- list()
  for (b in blocks) {
    if (b$kind == "f32") {
      writeBin(as.double(b$v), con, size = 4, endian = "little")
    } else {
      writeBin(as.integer(b$v), con, size = 1)
    }
    index[[b$key]] <- list(b$kind, offset)
    offset <- offset + n_draws * if (b$kind == "f32") 4 else 1
  }
  close(con)
  team_layout[[tm]] <- list(file = paste0("teams/", tm, ".bin"), bytes = offset, blocks = index)
}


# ---- meta.json ---------------------------------------------------------------
choice_list <- function(x) unname(map2(names(x), unname(x), ~ list(label = .x, value = .y)))

meta <- list(
  generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  source = list(dashboard_path = a$dashboard_path,
                value_metric = a$dd$metadata$value_metric %||% NA),
  n_draws = n_draws,
  proj_years = proj_years,
  slot_teams = slot_teams,
  labels = list(value_outcome = a$VALUE_OUTCOME,
                value_unit_long = a$VALUE_UNIT_LONG),
  protection_choices = list(`1` = choice_list(a$protection_choices_round1),
                            `2` = choice_list(a$protection_choices_round2)),
  teams = map(teams, ~ list(abbr = .x,
                            name = a$team_full_name_app(.x),
                            primary = a$team_primary_color_app(.x),
                            secondary = a$team_secondary_color_app(.x),
                            logo = a$team_logo_url_app(.x))),
  displays = displays %>%
    transmute(id = .data$display_asset_id, owner = .data$owner,
              year = as.integer(.data$year), round = as.integer(.data$round),
              trade_label = .data$trade_label, short_label = .data$short_label,
              group_type = .data$group_type, n_legs = .data$n_legs,
              single_leg = .data$single_leg, asset_id = .data$asset_id,
              fixed_slot = as.integer(.data$fixed_slot), guaranteed = .data$guaranteed,
              convey_prob_static = .data$convey_prob_static),
  receiver_raw = map(set_names(teams), function(tm) {
    r <- receiver_raw %>% filter(.data$team == .env$tm)
    as.list(set_names(r$asset_id, r$key))
  }),
  core = core_layout,
  team_files = team_layout
)

write_json(meta, file.path(out_dir, "meta.json"), auto_unbox = TRUE, digits = NA,
           na = "null", null = "null")

# ---- methodology.json ----------------------------------------------------------
# Chart tables come from the objects and helpers the Methodology tab plots. The
# value-metric text and validation table are read from app.R's rendered output
# so their wording, checks, and PASS / CHECK badges stay identical.
rendered <- NULL
testServer(a$server, {
  rendered <<- list(value_metric = output$method_value_metric$html,
                    validation = output$validation_panel$html)
})
value_metric_doc <- read_html(as.character(rendered$value_metric))
validation_doc   <- read_html(as.character(rendered$validation))

validation_rows <- xml_find_all(validation_doc, "//tbody/tr") %>%
  map(~ set_names(as.list(xml_text(xml_find_all(.x, "./td"), trim = TRUE)),
                  c("model", "check", "metric", "why", "status")))

rank_display <- a$rank_display_matrix(a$rank_trans_mat)
tier_mat     <- a$trans_mat

methodology <- list(
  value_metric_paragraphs = xml_text(xml_find_all(value_metric_doc, "//p"), trim = TRUE),
  value_metric_desc = a$VALUE_METRIC_DESC,
  value_outcome = a$VALUE_OUTCOME,
  pick_curve = a$pick_curve %>%
    filter(.data$pick >= 1, .data$pick <= 60) %>%
    arrange(.data$pick) %>%
    select(any_of(c("pick", "expected_war", "ev_q05", "ev_q95", "outcome_q10", "outcome_q90",
                    "emp_mean", "p_play", "p_play_q05", "p_play_q95", "emp_p_play"))),
  tier_matrix = list(from = unname(a$tier_short[rownames(tier_mat)]),
                     to = unname(a$tier_short[colnames(tier_mat)]),
                     pct = unname(round(100 * tier_mat, 1))),
  rank_matrix_pct = if (is.null(rank_display)) NULL else unname(round(100 * rank_display, 2)),
  rank_horizon = map(set_names(c("40_60", "25_75", "10_90")), function(ch) {
    list(label = a$rank_horizon_interval_label(ch),
         rows = a$rank_horizon_tbl_all(a$rank_trans_mat, 7L, ch) %>%
           select(start_rank, years_ahead, mean_rank, qlo, qhi, hover))
  }),
  rank_colors = substr(rainbow(30, start = 0.58, end = 0.96), 1, 7),
  lottery = a$lottery_dist %>%
    filter(.data$seed <= 16, .data$system %in% c("Current", "Proposed 3-2-1")) %>%
    arrange(.data$system, .data$seed) %>%
    select(system, seed, expected_pick, expected_pick_se, prob_no1, prob_no1_se),
  validation = list(rows = validation_rows,
                    note = xml_text(xml_find_first(validation_doc, "//body/div/div"), trim = TRUE))
)

write_json(methodology, file.path(out_dir, "methodology.json"), auto_unbox = TRUE,
           digits = NA, na = "null", null = "null", matrix = "rowmajor")

# ---- summary.json and single/<ABB>.json --------------------------------------
# Summary and Team tab numbers come from app.R's own functions. Server-local
# pieces (summary_extremes, summary_future_picks, swap exercise summaries) are
# read inside testServer, where the server's environment is visible.
year_choices <- sort(unique(as.integer(a$pick_display_assets$year)))
filter_combos <- expand_grid(year = c("All", as.character(year_choices)), round = c("All", "1", "2"))

# Every Year x Round filter of the leaderboard and pick scatterplot, with the
# scatterplot's logo positions after app.R's collision offsets.
portfolio <- pmap(filter_combos, function(year, round) {
  tbl <- a$portfolio_quality_quantity_summary_app("ev", year_filter = year, round_filter = round) %>%
    mutate(x = .data$new_avg_quality, y = .data$new_expected_picks)
  a$apply_logo_collision_offsets_app(tbl, x_col = "x", y_col = "y") %>%
    select(team, display_pick_count, cur_total_value, new_total_value, delta_total_value,
           new_expected_picks, new_avg_quality, p_positive, delta_q10, delta_q90, x_logo, y_logo)
}) %>%
  set_names(paste(filter_combos$year, filter_combos$round, sep = "|"))

summary_bits <- NULL
single_details <- NULL
testServer(a$server, {
  ex  <- summary_extremes()
  top <- summary_future_picks %>% slice_max(abs(.data$delta), n = 1, with_ties = FALSE)
  summary_bits <<- list(
    gain = list(team = ex$gain$team, delta = ex$gain$delta_total_value, p_positive = ex$gain$p_positive),
    loss = list(team = ex$loss$team, delta = ex$loss$delta_total_value, p_positive = ex$loss$p_positive),
    shifted = ex$shifted,
    n_moved = sum(summary_future_picks$moved),
    n_future = nrow(summary_future_picks),
    top = if (nrow(top) == 1L) list(owner = top$owner, year = as.integer(top$year),
                                    short_label = top$short_label, delta = top$delta) else NULL,
    moved_ids = summary_future_picks$display_asset_id[summary_future_picks$moved],
    mover_ids = summary_future_picks %>%
      arrange(desc(abs(.data$delta))) %>%
      slice_head(n = 10) %>%
      pull(display_asset_id)
  )

  # output$sp_obligation: conveyance and swap-exercise probabilities per pick.
  # Only server-local objects are visible here, so app.R globals go through a$.
  single_details <<- map(a$pick_display_assets$display_asset_id, function(did) {
    r <- a$pick_display_assets %>% filter(.data$display_asset_id == .env$did)
    s <- a$pick_display_value_summary %>% filter(.data$display_asset_id == .env$did)
    convey_cur <- if (did %in% colnames(a$display_convey_cur_draws)) {
      a$probability_summary_from_indicator(a$display_convey_cur_draws[, did] > 0)
    } else if (nrow(s) > 0 && "cur_convey_prob" %in% names(s)) {
      tibble(prob = s$cur_convey_prob, q05 = s$cur_convey_prob, q95 = s$cur_convey_prob)
    } else NULL
    convey_new <- if (did %in% colnames(a$display_convey_new_draws)) {
      a$probability_summary_from_indicator(a$display_convey_new_draws[, did] > 0)
    } else if (nrow(s) > 0 && "new_convey_prob" %in% names(s)) {
      tibble(prob = s$new_convey_prob, q05 = s$new_convey_prob, q95 = s$new_convey_prob)
    } else NULL
    if (isTRUE(guaranteed_swap_entitlement(r))) {
      convey_cur <- tibble(prob = 1, q05 = 1, q95 = 1)
      convey_new <- tibble(prob = 1, q05 = 1, q95 = 1)
    }
    obligation_txt <- a$pick_obligation_display_app(r) %>%
      as.character() %>%
      str_replace_all(" \\(conveys\\)", "") %>%
      str_replace_all("Own pick, no obligations", "Own pick")
    one_row <- function(x) if (is.null(x) || nrow(x) == 0L) NULL else as.list(x[1, ])
    list(
      original_teams = I(a$split_abbrs(r$member_original_teams)),
      fixed_slot_display = r$fixed_slot_display,
      obligation = obligation_txt,
      show_convey = !(probability_is_certain_app(convey_cur) && probability_is_certain_app(convey_new)),
      convey = list(cur = one_row(convey_cur), new = one_row(convey_new)),
      swap = list(cur = one_row(swap_exercise_summary_for_display(r, "cur")),
                  new = one_row(swap_exercise_summary_for_display(r, "new")))
    )
  }) %>%
    set_names(a$pick_display_assets$display_asset_id)
})

pick_rows <- a$pick_impact_rows_app("ev") %>%
  mutate(slot_sort = suppressWarnings(as.integer(str_extract(.data$fixed_slot_display, "[0-9]+")))) %>%
  arrange(.data$year, .data$owner, .data$round, coalesce(.data$slot_sort, 999L), .data$short_label) %>%
  transmute(id = .data$display_asset_id, owner = .data$owner, year = as.integer(.data$year),
            round = as.integer(.data$round), short_label = .data$short_label,
            cur_mean = .data$cur_mean, new_mean = .data$new_mean, delta = .data$delta,
            impact_bucket = .data$impact_bucket,
            cur_expected_pick_count = .data$cur_expected_pick_count,
            new_expected_pick_count = .data$new_expected_pick_count)

summary_out <- c(
  summary_bits,
  list(
    first_projected_year = a$FIRST_PROJECTED_YEAR_APP,
    value_metric_desc = a$VALUE_METRIC_DESC,
    value_outcome = a$VALUE_OUTCOME,
    value_unit = a$VALUE_UNIT,
    years = year_choices,
    teams = map(teams, ~ list(abbr = .x,
                              name = a$team_full_name_app(.x),
                              primary = a$team_primary_color_app(.x),
                              secondary = a$team_secondary_color_app(.x),
                              text = a$contrast_text_color_app(a$team_primary_color_app(.x)),
                              logo = a$team_logo_url_app(.x))),
    team_table = a$summary_ev_df %>%
      transmute(team = .data$team, tier = unname(a$tier_short[.data$tier]),
                wins = as.integer(.data$wins), losses = as.integer(.data$losses)),
    portfolio = portfolio,
    picks = pick_rows
  )
)
write_json(summary_out, file.path(out_dir, "summary.json"), auto_unbox = TRUE,
           digits = NA, na = "null", null = "null")

# Single pick: summary stats and R's own density curves (density_curve_df) for
# both systems, as expected pick value and as player outcomes.
dir.create(file.path(out_dir, "single"), showWarnings = FALSE)
curve_json <- function(v) {
  d <- a$density_curve_df(v, n = 256)
  list(x = signif(d$x, 5), y = signif(d$density, 5))
}
stats_json <- function(tbl, did) {
  s <- tbl %>% filter(.data$display_asset_id == .env$did)
  as.list(s[1, c("cur_mean", "cur_q05", "cur_q95", "new_mean", "new_q05", "new_q95")])
}
for (tm in teams) {
  ids <- a$pick_display_assets$display_asset_id[a$pick_display_assets$owner == tm]
  out <- map(set_names(ids), function(did) {
    c(single_details[[did]], list(
      ev = list(stats = stats_json(a$pick_display_value_ev_summary, did),
                cur = curve_json(a$display_asset_cur_ev_draws[, did]),
                new = curve_json(a$display_asset_new_ev_draws[, did])),
      outcome = list(stats = stats_json(a$pick_display_value_summary, did),
                     cur = curve_json(a$display_asset_cur_draws[, did]),
                     new = curve_json(a$display_asset_new_draws[, did]))
    ))
  })
  write_json(out, file.path(out_dir, "single", paste0(tm, ".json")), auto_unbox = TRUE,
             digits = NA, na = "null", null = "null")
}

sizes <- file.size(c(file.path(out_dir, c("meta.json", "core.bin")),
                     file.path(out_dir, "teams", paste0(teams, ".bin"))))
cat(sprintf("Wrote %d display picks for %d teams to %s\n", nrow(displays), length(teams), out_dir))
cat(sprintf("meta.json %.0f KB, core.bin %.0f KB, team files %.0f-%.0f KB (total %.1f MB)\n",
            sizes[1] / 1024, sizes[2] / 1024, min(sizes[-(1:2)]) / 1024, max(sizes[-(1:2)]) / 1024,
            sum(sizes) / 1024^2))
