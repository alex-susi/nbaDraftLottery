# =============================================================================
# web/scripts/parity_reference.R
# Runs reference trades through app.R's own Trade Machine server code and saves
# the results to web/tests/parity_reference.json. The web test suite recomputes
# the same trades from the exported data and must match.
#
# - Uses shiny::testServer, so the numbers come from app.R itself, not a copy.
# - Trades are picked from the current export to cover fixed 2026 picks,
#   first- and second-round protections, swap rights, a ranked pool, grouped
#   entitlements, and a one-sided trade.
# - Run from the repository root after 06_web_export.R:
#     Rscript web/scripts/parity_reference.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(shiny)
})

app_env <- new.env()
suppressMessages(sys.source("app.R", envir = app_env))
meta <- read_json(file.path("web", "public", "data", "meta.json"), simplifyVector = TRUE)
disp <- as_tibble(meta$displays)

# ---- Reference trades ----------------------------------------------------------
pick_one <- function(...) {
  out <- disp %>% filter(...)
  stopifnot(nrow(out) > 0)
  out[1, ]
}

r1_future <- disp %>% filter(single_leg, year >= 2027, round == 1)
r2_future <- disp %>% filter(single_leg, year >= 2027, round == 2)
ranked    <- pick_one(group_type == "ranked_split")
grouped   <- pick_one(group_type == "complex_entitlement", owner != ranked$owner)
swap_disp <- disp %>% filter(group_type %in% c("simple_swap", "simple_swap_return"))

prot_team <- r1_future %>% count(owner) %>% filter(n >= 2) %>% slice(1) %>% pull(owner)
prot_pair <- r1_future %>% filter(owner == prot_team) %>% slice(1:2)
swap_team <- r1_future %>% filter(owner != prot_team) %>% slice(1)
r2_a      <- r2_future %>% filter(owner == prot_team) %>% slice(1)
r2_b      <- r2_future %>% filter(owner == swap_team$owner) %>% slice(1)
fixed_a   <- pick_one(year == 2026, round == 1, owner == ranked$owner)
fixed_b   <- pick_one(year == 2026, owner == grouped$owner)

trades <- list(
  list(name = "fixed 2026 + ranked pool vs fixed 2026 + grouped",
       teamA = ranked$owner, teamB = grouped$owner,
       picksA = c(fixed_a$id, ranked$id), picksB = c(fixed_b$id, grouped$id),
       controls = list()),
  list(name = "R1 top-4 protection vs R1 swap right",
       teamA = prot_team, teamB = swap_team$owner,
       picksA = prot_pair$id[1], picksB = swap_team$id,
       controls = setNames(list("top4", TRUE),
                           c(paste0("prot_A_", gsub("[^A-Za-z0-9]", "_", prot_pair$id[1])),
                             paste0("swap_B_", gsub("[^A-Za-z0-9]", "_", swap_team$id))))),
  list(name = "R1 top-10 protection and swap together, plus R2 protections",
       teamA = prot_team, teamB = swap_team$owner,
       picksA = c(prot_pair$id[2], r2_a$id), picksB = c(r2_b$id),
       controls = setNames(list("top10", TRUE, "protected31_45", "protected31_55", TRUE),
                           c(paste0("prot_A_", gsub("[^A-Za-z0-9]", "_", prot_pair$id[2])),
                             paste0("swap_A_", gsub("[^A-Za-z0-9]", "_", prot_pair$id[2])),
                             paste0("prot_A_", gsub("[^A-Za-z0-9]", "_", r2_a$id)),
                             paste0("prot_B_", gsub("[^A-Za-z0-9]", "_", r2_b$id)),
                             paste0("swap_B_", gsub("[^A-Za-z0-9]", "_", r2_b$id))))),
  list(name = "one-sided: a single future pick",
       teamA = swap_team$owner, teamB = prot_team,
       picksA = swap_team$id, picksB = character(0),
       controls = list())
)
if (nrow(swap_disp) > 0) {
  other <- disp %>% filter(owner != swap_disp$owner[1], single_leg, year >= 2027) %>% slice(1)
  trades[[length(trades) + 1]] <- list(
    name = "existing swap entitlement vs plain future pick",
    teamA = swap_disp$owner[1], teamB = other$owner,
    picksA = swap_disp$id[1], picksB = other$id,
    controls = list())
}


# ---- Run each trade through app.R's server --------------------------------------
summ <- function(x) {
  list(mean = mean(x), q05 = unname(quantile(x, 0.05)), q95 = unname(quantile(x, 0.95)),
       p_pos = mean(x > 0), p_neg = mean(x < 0))
}

results <- map(trades, function(tr) {
  res <- NULL
  testServer(app_env$server, {
    do.call(session$setInputs, c(list(tm_teamA = tr$teamA, tm_teamB = tr$teamB), tr$controls))
    session$setInputs(tm_picksA = if (length(tr$picksA)) tr$picksA else NULL,
                      tm_picksB = if (length(tr$picksB)) tr$picksB else NULL)

    pick_rows <- c(
      map(tr$picksA, ~ list(side = "A", id = .x, receiver = tr$teamB)),
      map(tr$picksB, ~ list(side = "B", id = .x, receiver = tr$teamA))
    )
    picks <- map(pick_rows, function(p) {
      final <- sent_display_ev_value(p$id, p$side, p$receiver)
      list(side = p$side, id = p$id,
           convey = sent_display_convey_prob(p$id, p$side, p$receiver),
           initial = mean(sent_display_initial_ev_value(p$id, p$side, p$receiver)),
           impact = mean(sent_display_epv_impact_value(p$id, p$side, p$receiver)),
           final = mean(final),
           final_q10 = unname(quantile(final, 0.10)),
           final_q90 = unname(quantile(final, 0.90)),
           outcome = mean(sent_display_outcome_value(p$id, p$side, p$receiver)))
    })

    d <- trade_draws()
    ci <- function(x) qbeta(c(0.05, 0.95), sum(x) + 1, length(x) - sum(x) + 1)
    res <<- list(
      picks = picks,
      net_ev = summ(d$net_to_A_ev),
      net_outcome = summ(d$net_to_A_outcome),
      best = list(p_A = mean(d$best_outcome_to_A), p_B = mean(d$best_outcome_to_B),
                  ci_A = ci(d$best_outcome_to_A), ci_B = ci(d$best_outcome_to_B),
                  edge_mean = mean(d$best_outcome_edge_to_A))
    )
  })
  c(tr[c("name", "teamA", "teamB", "picksA", "picksB")],
    list(controls = tr$controls), res)
})

dir.create(file.path("web", "tests"), showWarnings = FALSE)
write_json(list(generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                export_generated = meta$generated,
                trades = results),
           file.path("web", "tests", "parity_reference.json"),
           auto_unbox = TRUE, digits = NA, pretty = TRUE, null = "null", na = "null")

for (r in results) {
  cat(sprintf("%-62s net EPV to %s: %+.3f\n", r$name, r$teamA, r$net_ev$mean))
}
