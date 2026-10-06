## ═════════════════════════════════════════════════════════════════════════════
# NBA Draft Lottery Rule Change Impact Model - HELPER FUNCTIONS
#
# Every reusable function for the pipeline lives here. 01_data.R sources this
# file first, so the helpers are available to 01_data.R -> 04_lotterySims.R.
#
# Organization:
#   - Large headers group helpers by the script that FIRST calls them.
#   - Section headers inside each group match the section headers of that
#     script, so a helper can be traced to where it is used.
#   - Code that runs only once stays inline in the main scripts; only functions
#     called from two or more places are here.
#
# Many helpers read objects created by the main scripts (e.g. all_teams,
# pick_assets, posterior draw matrices). R looks those objects up when the
# helper is CALLED, so the helpers must run after the scripts that create them.
## ═════════════════════════════════════════════════════════════════════════════





## ═════════════════════════════════════════════════════════════════════════════
## ═════════════════════════════════════════════════════════════════════════════
##                                                                        
##      HELPERS FIRST CALLED IN 01_data.R          ═════════════════════════════                               
##                                                                        
## ═════════════════════════════════════════════════════════════════════════════
## ═════════════════════════════════════════════════════════════════════════════


## ═════════════════════════════════════════════════════════════════════════════
## 03 - DRAFT PRODUCTION CURVE -------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════

### 03.01 Player-season xRAPM wins above replacement --------------------------

# normalize_player_name()
#   - builds a join key so player names match across data sources
#     (Basketball-Reference, xRAPM, NBA possessions)
#   - converts accented characters to ASCII and lower case
#   - removes periods, apostrophes, hyphens and suffixes (Jr, Sr, II-V)
#   - input: character vector of names; output: character vector of keys
normalize_player_name <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- str_replace_all(x, "[.'`]", "")
  x <- str_replace_all(x, "-", " ")
  x <- str_replace_all(x, "\\b(jr|sr|ii|iii|iv|v)\\b", "")
  str_squish(x)
}





## ═════════════════════════════════════════════════════════════════════════════
## ═════════════════════════════════════════════════════════════════════════════
##                                                                        
##      HELPERS FIRST CALLED IN 02_picks.R          ════════════════════════════                               
##                                                                        
## ═════════════════════════════════════════════════════════════════════════════
## ═════════════════════════════════════════════════════════════════════════════


## ═════════════════════════════════════════════════════════════════════════════
## 06 - FUTURE PICK OWNERSHIP 2027-2032 (RealGM / prosportstransactions) -------
## ═════════════════════════════════════════════════════════════════════════════

# make_complex_assets()
#   - builds the first-round asset rows for one multi-team ranked pool
#   - one row per (possible owner, original team) pair, excluding a team's
#     own pick (own-pick rows are created separately)
#   - the Monte Carlo simulation decides which row actually receives the pick
#   - output: tibble with the same columns as traded_future (round = 1)
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
              complex_group = group_id,
              round = 1L)
}

# make_complex_second_assets()
#   - second-round version of make_complex_assets()
#   - adds an empty condition_id column to match traded_second
#   - output: tibble with the same columns as traded_second (round = 2)
make_complex_second_assets <- function(year, group_id, original_teams, 
                                       possible_owners, notes) {
  tidyr::expand_grid(owner = possible_owners,
                     original_team = original_teams) %>%
    filter(owner != original_team) %>%
    transmute(owner,
              original_team,
              year = as.integer(year),
              protection = "complex",
              pick_type = "complex",
              notes = notes,
              condition_id = NA_character_,
              complex_group = group_id,
              round = 2L)
}



## ═════════════════════════════════════════════════════════════════════════════
## REALGM AUDIT PATCHES: remaining first/second-round obligation registry fixes
## ═════════════════════════════════════════════════════════════════════════════

# protection_floor()
#   - converts a first-round protection code to the highest protected slot
#   - e.g. "top5" -> 5, "lottery" -> 14, "none" or unknown code -> 0
#   - used to flag protections in the 12-15 range, which the 3-2-1 rules ban
protection_floor <- function(protection) {
  switch(protection,
         top1 = 1, top2 = 2, top3 = 3, top4 = 4, top5 = 5, top6 = 6,
         top8 = 8, top10 = 10, lottery = 14, top16 = 16, top20 = 20, 0)
}


### PICK-ASSET REGISTRY ---------------------------------------------------------

# `%||%` (default-value operator)
#   - returns y when x is NULL or has length 0; otherwise returns x
#   - used to fill an optional value with a default
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


### USER-FACING PICK ENTITLEMENTS (DISPLAY ASSETS) ------------------------------

# add_display_group()
#   - groups several internal pick assets into one user-facing pick
#     (e.g. a swap holder's own pick + swap leg shown as one entitlement)
#   - selects member rows of pick_assets by year, round, owner, original
#     teams, pick types and (optionally) one complex_group
#   - appends one row to pick_display_assets and one row per member to
#     pick_display_members (both are global tibbles, updated with <<-)
#   - does nothing if no member assets match
add_display_group <- function(display_asset_id,
                              year,
                              owner,
                              original_teams,
                              label,
                              obligation,
                              notes = obligation,
                              group_type = "grouped",
                              draft_round = 1L,
                              complex_group_filter = NULL,
                              pick_types = c("own", "swap", "swap_return", "complex")) {
  members <- pick_assets %>%
    filter(.data$year == .env$year,
           .data$round == .env$draft_round,
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
                                           round = as.integer(draft_round),
                                           label = label,
                                           obligation = obligation,
                                           notes = notes,
                                           group_type = group_type,
                                           display_group = complex_group_filter %||% 
                                             display_asset_id,
                                           member_n = nrow(members),
                                           member_original_teams = paste(
                                             sort(unique(members$original_team)),
                                             collapse = ", ")))

  pick_display_members <<- bind_rows(pick_display_members,
                                     tibble(display_asset_id = display_asset_id,
                                            asset_id = members$asset_id))

  invisible(NULL)
}





## ═════════════════════════════════════════════════════════════════════════════
## ═════════════════════════════════════════════════════════════════════════════
##                                                                        
##      HELPERS FIRST CALLED IN 03_models.R          ═══════════════════════════                               
##                                                                        
## ═════════════════════════════════════════════════════════════════════════════
## ═════════════════════════════════════════════════════════════════════════════


## ═════════════════════════════════════════════════════════════════════════════
## 07 - BUILD RANK-STATE MARKOV TRANSITION DATA -------------------------------
## ═════════════════════════════════════════════════════════════════════════════

# normalize_rows()
#   - rescales each row of a matrix to sum to 1 (row-stochastic matrix)
#   - rows with a zero, negative or non-finite total become uniform rows
normalize_rows <- function(M) {
  rs <- rowSums(M)
  bad <- !is.finite(rs) | rs <= 0
  if (any(bad)) {
    M[bad, ] <- 1 / ncol(M)
    rs <- rowSums(M)
  }
  sweep(M, 1, rs, `/`)
}

# rank_kernel_weights()
#   - K x K smoothing weights between ranks: exp(-|i - j| / bandwidth)
#   - larger bandwidth = more borrowing from distant ranks
#   - rows are normalized to sum to 1
rank_kernel_weights <- function(K, bandwidth = 1.75) {
  idx <- seq_len(K)
  normalize_rows(exp(-abs(outer(idx, idx, "-")) / bandwidth))
}

# aggregate_rank_transition_to_tier()
#   - collapses a 30 x 30 rank transition matrix to the 5 x 5 tier matrix
#   - tier-to-tier probability = average (over starting ranks in the tier)
#     of the probability of landing in any rank of the destination tier
#   - uses the global rank_tier_lookup (rank_worst -> tier)
#   - output: row-stochastic 5 x 5 matrix with TIERS as dimnames
aggregate_rank_transition_to_tier <- function(P_rank) {
  out <- matrix(0, N_TIERS, N_TIERS, dimnames = list(from = TIERS, to = TIERS))
  for (a in seq_along(TIERS)) {
    from_ranks <- rank_tier_lookup$rank_worst[rank_tier_lookup$tier == TIERS[a]]
    for (b in seq_along(TIERS)) {
      to_ranks <- rank_tier_lookup$rank_worst[rank_tier_lookup$tier == TIERS[b]]
      out[a, b] <- mean(rowSums(P_rank[from_ranks, to_ranks, drop = FALSE]))
    }
  }
  out / rowSums(out)
}



## ═════════════════════════════════════════════════════════════════════════════
## 09 - VALIDATE STAN MODELS ---------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════

### 09.01 - SHARED VALIDATION FUNCTIONS -----------------------------------------

# validate_header()
#   - prints a boxed title to the console to separate validation output
validate_header <- function(title) {
  cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
}

# draws_matrix()
#   - posterior draws of one Stan variable as a plain matrix
#   - rows = posterior draws, columns = variable elements (e.g. war4_pred[1])
draws_matrix <- function(fit, variable) {
  as.matrix(fit$draws(variables = variable, format = "draws_matrix"))
}

# existing_stan_vars()
#   - keeps only the variable names that exist in a fitted Stan model
#   - avoids errors when a parameter list includes names from older models
existing_stan_vars <- function(fit, vars) {
  vars[vapply(vars, function(v) {
    !inherits(try(fit$summary(variables = v), silent = TRUE), "try-error")
  }, logical(1))]
}

# extract_stan_vector_draws()
#   - posterior draws of a Stan vector (e.g. war4_pred[1..30]) as a matrix
#   - columns sorted by numeric index (so [10] follows [9], not [1])
#   - warns if the number of columns differs from K
extract_stan_vector_draws <- function(fit, variable_base, K = 30L) {
  mat <- draws_matrix(fit, variable_base)
  idx <- stringr::str_match(colnames(mat),
                            paste0("^", variable_base, "\\[(\\d+)\\]$"))[, 2]

  if (any(is.na(idx))) {
    stop("Could not parse Stan vector indices for ", variable_base, call. = FALSE)
  }

  ord <- order(as.integer(idx))
  mat <- mat[, ord, drop = FALSE]
  colnames(mat) <- paste0(variable_base, "[", seq_len(ncol(mat)), "]")

  if (ncol(mat) != K) {
    warning("Expected ", K, " columns for ", variable_base, "; found ", ncol(mat), ".")
  }

  mat
}

# summarise_nuts()
#   - per-chain NUTS sampler diagnostics for a fitted Stan model:
#     divergent transitions, max-treedepth hits, max treedepth reached,
#     E-BFMI (energy Bayesian fraction of missing information)
#   - E-BFMI = mean(squared change in energy) / variance(energy); NA when a
#     chain has fewer than 3 finite energy values or zero energy variance
#   - prints the table and warns on divergences, treedepth hits, E-BFMI < 0.30
summarise_nuts <- function(fit, model_name, max_treedepth = 12L) {
  nuts <- posterior::as_draws_df(fit$sampler_diagnostics()) %>%
    as_tibble()

  if (!".chain" %in% names(nuts)) nuts$.chain <- 1L
  if (!".iteration" %in% names(nuts)) nuts$.iteration <- seq_len(nrow(nuts))

  out <- nuts %>%
    arrange(.chain, .iteration) %>%
    group_by(.chain) %>%
    summarise(draws = n(),
              divergences = sum(.data$divergent__ > 0, na.rm = TRUE),
              max_treedepth_hits = sum(.data$treedepth__ >= max_treedepth, na.rm = TRUE),
              max_treedepth_observed = max(.data$treedepth__, na.rm = TRUE),
              energy_n = sum(is.finite(.data$energy__)),
              energy_var = stats::var(.data$energy__[is.finite(.data$energy__)]),
              energy_sq_diff = mean(diff(.data$energy__[is.finite(.data$energy__)])^2),
              .groups = "drop") %>%
    mutate(ebfmi = if_else(energy_n >= 3 & !is.na(energy_var) & energy_var > 0,
                           energy_sq_diff / energy_var,
                           NA_real_)) %>%
    select(-energy_n, -energy_var, -energy_sq_diff) %>%
    mutate(model = model_name, .before = 1)

  print(out)

  if (any(out$divergences > 0, na.rm = TRUE)) {
    warning(model_name, ": divergent transitions detected.")
  }
  if (any(out$max_treedepth_hits > 0, na.rm = TRUE)) {
    warning(model_name, ": max treedepth hits detected.")
  }
  if (any(out$ebfmi < 0.30, na.rm = TRUE)) {
    warning(model_name, ": E-BFMI below 0.30 in at least one chain.")
  }

  out
}

# summarise_rhat_ess()
#   - posterior summary (mean, median, sd, R-hat, bulk/tail ESS) for the
#     requested parameters that exist in the fit
#   - prints the table and warns when R-hat > 1.01 or ESS < 400
summarise_rhat_ess <- function(fit, variables, model_name) {
  variables <- existing_stan_vars(fit, variables)
  if (length(variables) == 0L) {
    warning("No requested summary variables found for ", model_name)
    return(tibble())
  }

  out <- fit$summary(variables) %>%
    select(variable, mean, median, sd, rhat, ess_bulk, ess_tail)

  cat("\n  ", model_name, " posterior summary\n", sep = "")
  print(out)

  if (any(out$rhat > 1.01, na.rm = TRUE)) {
    warning(model_name, ": some R-hat values exceed 1.01.")
  }
  if (any(out$ess_bulk < 400, na.rm = TRUE) ||
      any(out$ess_tail < 400, na.rm = TRUE)) {
    warning(model_name, ": some ESS values are below 400.")
  }

  out
}

# second_round_band()
#   - groups second-round picks (31-60) into five-pick bands
#   - e.g. 33 -> "31-35", 58 -> "56-60"
second_round_band <- function(pick) {
  case_when(pick <= 35 ~ "31-35",
            pick <= 40 ~ "36-40",
            pick <= 45 ~ "41-45",
            pick <= 50 ~ "46-50",
            pick <= 55 ~ "51-55",
            TRUE       ~ "56-60")
}


### 09.02 - ROUND 1 PICK VALUE MODEL VALIDATION -------------------------------

# draw_r1_outcomes()
#   - simulates first-round player outcomes (4-year xRAPM WAR) at pick pk
#   - one outcome per element of draw_idx (posterior draw row numbers)
#   - ex-Gaussian: Normal(mean - tau_x, sigma_g) + Exponential(mean tau_x),
#     so the outcome mean equals that draw's pick mean
#   - reads the global posterior matrices pick_mu_draws, pick_sigma_draws,
#     pick_tau_draws
draw_r1_outcomes <- function(draw_idx, pk) {
  n       <- length(draw_idx)
  mean_pk <- pick_mu_draws[draw_idx, pk]
  sigma_g <- pick_sigma_draws[draw_idx, pk]
  tau_x   <- pick_tau_draws[draw_idx, pk]
  stats::rnorm(n, mean_pk - tau_x, sigma_g) + stats::rexp(n, 1 / tau_x)
}


### 09.03 - ROUND 2 PICK VALUE MODEL VALIDATION -------------------------------

# draw_r2_outcomes()
#   - simulates second-round player outcomes (4-year xRAPM WAR) at pick pk
#     (31-60); one outcome per element of draw_idx
#   - hurdle model:
#       - not played (probability 1 - pi_p) -> exactly 0
#       - played -> Normal(fringe_mean, sd_fringe), plus an Exponential
#         upside (mean upside_mean) with probability p_contrib
#   - reads the global pick2_*_draws posterior matrices (column = pick - 30)
draw_r2_outcomes <- function(draw_idx, pk) {
  j <- pk - 30L
  n <- length(draw_idx)
  played  <- stats::runif(n) < pick2_p_play_draws[draw_idx, j]
  contrib <- stats::runif(n) < pick2_contrib_draws[draw_idx, j]
  played_value <- stats::rnorm(n, pick2_fringe_draws[draw_idx, j],
                               pick2_fringe_sd_draws[draw_idx, j]) +
    ifelse(contrib, stats::rexp(n, 1 / pick2_upside_draws[draw_idx, j]), 0)
  ifelse(played, played_value, 0)
}


### 09.04 - RANK-STATE MARKOV TRANSITION MODEL VALIDATION ----------------------

# markov_diagnostics()
#   - summary statistics of a Markov transition matrix P (rows sum to 1)
#   - stationary: long-run share of time in each state (eigenvector of t(P)
#     for eigenvalue 1)
#   - lambda2: second-largest eigenvalue magnitude (closer to 1 = slower mixing)
#   - mixing_time: -1 / log(lambda2), in seasons
markov_diagnostics <- function(P) {
  ev <- eigen(t(P))
  idx <- which.min(abs(ev$values - 1))
  pi_stat <- Re(ev$vectors[, idx])
  pi_stat <- pi_stat / sum(pi_stat)

  lam <- sort(abs(Re(ev$values)), decreasing = TRUE)

  list(stationary = pi_stat,
       lambda2 = lam[2],
       mixing_time = -1 / log(lam[2]))
}





## ═════════════════════════════════════════════════════════════════════════════
## ═════════════════════════════════════════════════════════════════════════════
##                                                                        
##      HELPERS FIRST CALLED IN 04_lotterySims.R     ═══════════════════════════                               
##                                                                        
## ═════════════════════════════════════════════════════════════════════════════
## ═════════════════════════════════════════════════════════════════════════════



# ============================================================================
# SECTION 10: LOTTERY SIMULATORS
# ============================================================================

# sim_current_lottery()
#   - one draw of the current (pre-2027) 14-team lottery
#   - lottery combinations per seed: 140,140,140,125,...,5 (seed 1 = worst)
#   - picks 1-4 drawn by weighted sampling without replacement; the other 10
#     teams keep seed order for picks 5-14
#   - output: integer vector, element s = pick number won by seed s
sim_current_lottery <- function() {
  combos <- c(140,140,140,125,105,90,75,60,45,30,20,15,10,5)
  picks  <- integer(14)
  drawn <- integer(0)
  for (p in 1:4) {
    pr <- combos; pr[drawn] <- 0
    pr <- pr / sum(pr)
    w <- sample(14, 1, prob = pr)
    while (w %in% drawn) w <- sample(14, 1, prob = pr)
    picks[w] <- p
    drawn <- c(drawn, w)
  }
  rem <- setdiff(1:14, drawn)
  for (k in seq_along(rem)) picks[rem[k]] <- 4 + k
  picks
}

# sim_321_lottery()
#   - one draw of the approved 3-2-1 lottery: 16 teams, all 16 picks drawn
#   - lottery balls per seed (seed 1 = worst, 37 balls total):
#       - seeds 1-3   (three worst)         2 balls each
#       - seeds 4-10  (non-play-in)         3 balls each
#       - seeds 11-14 (9/10 play-in seeds)  2 balls each
#       - seeds 15-16 (7v8 play-in losers)  1 ball each
#   - seeds 1-3 cannot fall below pick 12 (enforced during the draw)
#   - output: integer vector, element s = pick number won by seed s
sim_321_lottery <- function() {
  balls16 <- c(2,2,2, 3,3,3,3,3,3,3, 2,2,2,2, 1,1)
  picks <- integer(16)
  drawn <- integer(0)

  for (p in 1:16) {
    undrawn <- setdiff(1:16, drawn)
    releg_remaining <- setdiff(1:3, drawn)

    # If the number of undrawn bottom-three seeds equals the number of picks
    # left through #12, the next pick must go to one of them.
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



# ============================================================================
# SECTION 11: RANK -> SEEDS, AND THE NEW ANTI-TANK PICK RESTRICTIONS
# ============================================================================
# ---- future-pick allocation helpers -----------------------------------------
# Lottery slots stay attached to the ORIGINAL team; these helpers then assign
# each original pick to exactly one owner per simulation and year.

# pick_conveys()
#   - TRUE if a protected pick at slot `pos` transfers to the new owner
#   - first-round codes: "none", "top1".."top20" (protected 1-N),
#     "lottery" (protected 1-14)
#   - second-round codes: "protectedLO_HI" (conveys outside LO-HI),
#     "conveyLO_HI" (conveys only inside LO-HI)
#   - missing slot -> FALSE; missing or unknown protection -> TRUE
pick_conveys <- function(pos, protection) {
  if (length(pos) == 0 || is.na(pos)) return(FALSE)
  if (is.null(protection) || length(protection) == 0 || is.na(protection)) return(TRUE)
  protection <- as.character(protection)
  if (protection == "none") return(TRUE)
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

  # Protected range, e.g. protected31_55 conveys only at 56-60
  m <- stringr::str_match(protection, "^protected(\\d+)_(\\d+)$")
  if (!is.na(m[1, 1])) {
    lo <- as.integer(m[1, 2])
    hi <- as.integer(m[1, 3])
    return(!(pos >= lo && pos <= hi))
  }

  # Conveyance range, e.g. convey56_60 conveys only at 56-60
  m <- stringr::str_match(protection, "^convey(\\d+)_(\\d+)$")
  if (!is.na(m[1, 1])) {
    lo <- as.integer(m[1, 2])
    hi <- as.integer(m[1, 3])
    return(pos >= lo && pos <= hi)
  }

  TRUE
}

# rank_teams_by_slot()
#   - orders a set of teams from best (lowest) to worst (highest) draft slot
#   - teams without a slot are dropped
#   - used to resolve swaps and ranked pools ("more favorable of A and B")
rank_teams_by_slot <- function(slots, teams) {
  teams <- teams[teams %in% names(slots)]
  teams[order(as.numeric(slots[teams]), na.last = TRUE)]
}

# normalize_obligation_state()
#   - fills in default values for the cross-year obligation flags
#   - flags record whether earlier picks conveyed (MIA 2027 to CHA, DEN to
#     OKC by 2028 / 2029, DEN 2028-2030 obligation settled)
#   - NULL input -> all flags FALSE
normalize_obligation_state <- function(state = NULL) {
  defaults <- list(
    mia_2027_frp_conveyed_to_cha = FALSE,
    den_first_potential_conveyed_to_okc_by_2028 = FALSE,
    den_first_potential_conveyed_to_okc_by_2029 = FALSE,
    den_2028_2030_obligation_settled = FALSE
  )

  if (is.null(state)) return(defaults)
  modifyList(defaults, state)
}

# resolve_pick_owners()
#   - first-round owner of every original team's pick for one year
#   - steps:
#       1. start with every team owning its own pick
#       2. simple obligations from traded_future (outright transfers with
#          protections, two-team swaps)
#       3. complex ranked-pool obligations, hard-coded by year (2027-2030)
#       4. update the cross-year obligation flags from this year's result
#   - inputs: slots = first-round slot per original team, yr = draft year,
#     state = obligation flags carried from earlier years
#   - output: list(owner_by_orig = owner per original team, state = flags)
resolve_pick_owners <- function(slots, yr, state = NULL) {
  state <- normalize_obligation_state(state)
  owner_by_orig <- setNames(all_teams, all_teams)
  rows <- traded_future %>% filter(year == yr, round == 1L)

  # ---- simple obligations ----------------------------------------------------
  # Outright protected/unprotected transfers. If protection does not convey,
  # owner_by_orig stays as the original team, so the retained own asset receives value.
  outrights <- rows %>% filter(pick_type == "outright")
  if (nrow(outrights) > 0) {
    for (j in seq_len(nrow(outrights))) {
      og <- outrights$original_team[j]
      ow <- outrights$owner[j]
      prot <- outrights$protection[j]

      # DEN top-5-protected chain:
      # 2028/2029: only if not already settled.
      # 2030: only if not already settled AND the RealGM by-2028 condition is true.
      if (identical(og, "DEN") && identical(ow, "OKC") && yr %in% 2028:2030) {
        if (isTRUE(state$den_2028_2030_obligation_settled)) next
        if (yr == 2030L && !isTRUE(state$den_first_potential_conveyed_to_okc_by_2028)) next
      }

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

  # ---- complex ranked-pool obligations ---------------------------------------
  # 2027
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

    # SAS: 1-16 SAC, 17-30 OKC.
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

  # 2028
  if (yr == 2028L) {
    # MIA rollover: MIA 2028 first to CHA if the 2027 MIA 15-30 obligation
    # did not convey.
    if (!isTRUE(state$mia_2027_frp_conveyed_to_cha) && !is.na(slots["MIA"])) {
      owner_by_orig["MIA"] <- "CHA"
    }

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

    # BKN/PHI/PHX/NYK/WAS/MIL/POR nested pool approximation.
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

  # 2029
  if (yr == 2029L) {
    r <- rank_teams_by_slot(slots, c("DAL", "HOU", "PHX"))
    if (length(r) == 3) {
      owner_by_orig[r[1:2]] <- "HOU"
      owner_by_orig[r[3]] <- "BKN"
    }

    r <- rank_teams_by_slot(slots, c("BOS", "MIL", "POR"))
    if (length(r) == 3) {
      owner_by_orig[r[c(1, 3)]] <- "POR"
      owner_by_orig[r[2]] <- "WAS"
    }

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

    if (!is.na(slots["ORL"]) && slots["ORL"] > 2 && !is.na(slots["MEM"])) {
      if (slots["ORL"] < slots["MEM"]) {
        owner_by_orig["ORL"] <- "MEM"
        owner_by_orig["MEM"] <- "ORL"
      }
    }

    if (!is.na(slots["LAC"]) && slots["LAC"] > 3 && !is.na(slots["PHI"])) {
      if (slots["LAC"] < slots["PHI"]) {
        owner_by_orig["LAC"] <- "PHI"
        owner_by_orig["PHI"] <- "LAC"
      }
    }
  }

  # 2030
  if (yr == 2030L) {
    wp <- rank_teams_by_slot(slots, c("WAS", "PHX"))
    if (length(wp) == 2) {
      owner_by_orig[wp[1]] <- "WAS"
      rem <- rank_teams_by_slot(slots, c("MEM", wp[2]))
      if (length(rem) == 2) {
        owner_by_orig[rem[1]] <- "MEM"
        owner_by_orig[rem[2]] <- "PHX"
      }
    }

    # Exact RealGM DAL/SAS/MIN logic:
    # SAS gets most favorable of SAS, DAL, and MIN 2-30.
    # DAL gets less favorable of SAS and DAL.
    # MIN keeps #1; otherwise MIN gets less favorable of MIN and more favorable SAS/DAL.
    if (!is.na(slots["MIN"]) && slots["MIN"] == 1) {
      sd <- rank_teams_by_slot(slots, c("SAS", "DAL"))
      if (length(sd) == 2) {
        owner_by_orig[sd[1]] <- "SAS"
        owner_by_orig[sd[2]] <- "DAL"
      }
      owner_by_orig["MIN"] <- "MIN"
    } else {
      sd <- rank_teams_by_slot(slots, c("SAS", "DAL"))
      all3 <- rank_teams_by_slot(slots, c("SAS", "DAL", "MIN"))
      if (length(sd) == 2 && length(all3) == 3) {
        best_sd  <- sd[1]
        worst_sd <- sd[2]
        best_all <- all3[1]

        owner_by_orig[best_all] <- "SAS"
        owner_by_orig[worst_sd] <- "DAL"

        min_pick <- if (identical(best_all, "MIN")) best_sd else "MIN"
        owner_by_orig[min_pick] <- "MIN"
      }
    }

    if (!is.na(slots["MIL"]) && !is.na(slots["POR"]) && slots["MIL"] < slots["POR"]) {
      owner_by_orig["MIL"] <- "POR"
      owner_by_orig["POR"] <- "MIL"
    }
  }

  # ---- update cross-year obligation flags ------------------------------------
  if (yr == 2027L) {
    state$mia_2027_frp_conveyed_to_cha <-
      identical(unname(owner_by_orig["MIA"]), "CHA")
  }

  # Tracks DEN first-potential conveyance to OKC. Used by DEN's later RealGM
  # conditional seconds and by the 2030 first-round condition.
  if (yr <= 2028L && identical(unname(owner_by_orig["DEN"]), "OKC")) {
    state$den_first_potential_conveyed_to_okc_by_2028 <- TRUE
  }
  if (yr <= 2029L && identical(unname(owner_by_orig["DEN"]), "OKC")) {
    state$den_first_potential_conveyed_to_okc_by_2029 <- TRUE
  }

  # Treat the direct DEN 2028-2030 top-5-protected chain as settled only when
  # the DEN original pick itself conveys to OKC in one of those future years.
  if (yr %in% 2028:2030 && identical(unname(owner_by_orig["DEN"]), "OKC")) {
    state$den_2028_2030_obligation_settled <- TRUE
  }

  list(owner_by_orig = owner_by_orig, state = state)
}

# assign_ranked_second_pool()
#   - second-round ranked pool: the k-th best pick among `teams` goes to
#     owners_by_rank[k]
#   - e.g. teams = c("DAL", "BKN"), owners_by_rank = c("WAS", "DET") gives
#     the better of DAL/BKN to WAS and the other to DET
#   - output: updated owner_by_orig
assign_ranked_second_pool <- function(owner_by_orig, slots, teams, owners_by_rank) {
  r <- rank_teams_by_slot(slots, teams)
  if (length(r) == 0) return(owner_by_orig)
  for (k in seq_along(r)) {
    if (k <= length(owners_by_rank) && !is.na(owners_by_rank[k])) {
      owner_by_orig[r[k]] <- owners_by_rank[k]
    }
  }
  owner_by_orig
}

# resolve_second_pick_owners()
#   - second-round owner of every original team's pick for one year
#   - steps:
#       1. start with every team owning its own pick
#       2. simple obligations from traded_second; a row applies only if its
#          condition_id (tied to first-round results) is met and its
#          protection conveys
#       3. complex ranked pools and swaps, hard-coded by year (2027-2032)
#   - inputs: slots = second-round slot per original team, yr = draft year,
#     first_owner_by_orig / first_slots = same year's first-round results,
#     obligation_state = cross-year flags from resolve_pick_owners()
#   - output: named vector, owner per original team
resolve_second_pick_owners <- function(slots,
                                       yr,
                                       first_owner_by_orig,
                                       first_slots = NULL,
                                       obligation_state = NULL) {
  obligation_state <- normalize_obligation_state(obligation_state)
  owner_by_orig <- setNames(all_teams, all_teams)

  # ---- simple obligations ----------------------------------------------------
  rows <- traded_second %>% filter(year == yr)
  for (j in seq_len(nrow(rows))) {
    og <- rows$original_team[j]
    ow <- rows$owner[j]
    prot <- rows$protection[j]
    cond <- rows$condition_id[j]
    if (is.na(slots[og])) next

    # Conditions tied to first-round conveyance or cross-year flags
    condition_met <- if (is.na(cond)) {
      TRUE
    } else {
      switch(cond,
             LAL_2027_FRP_TO_MEM     = identical(unname(first_owner_by_orig["LAL"]), "MEM"),
             LAL_2027_FRP_NOT_TO_MEM = !identical(unname(first_owner_by_orig["LAL"]), "MEM"),
             DAL_2027_FRP_TO_CHA     = identical(unname(first_owner_by_orig["DAL"]), "CHA"),
             DAL_2027_FRP_NOT_TO_CHA = !identical(unname(first_owner_by_orig["DAL"]), "CHA"),

             SAS_2027_FRP_TO_SAC     = identical(unname(first_owner_by_orig["SAS"]), "SAC"),
             SAS_2027_FRP_TO_OKC     = identical(unname(first_owner_by_orig["SAS"]), "OKC"),

             PHI_2028_FRP_RETAINED   = identical(unname(first_owner_by_orig["PHI"]), "PHI"),
             BOS_2028_FRP_SLOT_1     = !is.null(first_slots) &&
               !is.na(first_slots["BOS"]) && as.integer(first_slots["BOS"]) == 1L,

             DEN_FRP_CONVEYED_TO_OKC_BY_2029 =
               isTRUE(obligation_state$den_first_potential_conveyed_to_okc_by_2029),

             DEN_FRP_NOT_CONVEYED_TO_OKC_BY_2029 =
               !isTRUE(obligation_state$den_first_potential_conveyed_to_okc_by_2029),

             ORL_2029_FRP_RETAINED =
               !is.null(first_slots) &&
               !is.na(first_slots["ORL"]) && as.integer(first_slots["ORL"]) <= 2L,

             GSW_2030_FRP_NOT_TO_DAL =
               !identical(unname(first_owner_by_orig["GSW"]), "DAL"),

             TRUE)
    }
    if (!condition_met) next
    if (pick_conveys(slots[og], prot)) owner_by_orig[og] <- ow
  }

  # ---- complex ranked pools and swaps ----------------------------------------
  if (yr == 2027L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("DAL", "BKN"), c("WAS", "DET"))

    r4 <- rank_teams_by_slot(slots, c("HOU", "OKC", "IND", "MIA"))
    if (length(r4) == 4) {
      owner_by_orig[r4[1]] <- "PHI"
      owner_by_orig[r4[2]] <- "NOP"
      owner_by_orig[r4[3]] <- "NYK"
      san_pair <- rank_teams_by_slot(slots, c("SAS", r4[4]))
      if (length(san_pair) == 2) {
        owner_by_orig[san_pair[1]] <- "SAS"
        owner_by_orig[san_pair[2]] <- "MIA"
      }
    }

    r <- rank_teams_by_slot(slots, c("NOP", "POR"))
    if (length(r) == 2) {
      owner_by_orig[r[1]] <- "CHA"
      owner_by_orig[r[2]] <- if (!is.na(slots[r[2]]) && slots[r[2]] >= 56) "HOU" else "POR"
    }

    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("ORL", "BOS"), c("UTA", "CHA"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("PHX", "GSW"), c("PHI", "WAS"))
  }

  if (yr == 2028L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("CHA", "LAC"), 
                                               c("CHA", "DET"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("LAL", "WAS"), 
                                               c("ORL", "WAS"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("IND", "PHX"), 
                                               c("IND", "NYK"))
  }

  if (yr == 2029L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("ATL", "MIA"), 
                                               c("CHA", "OKC"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("DET", "MIL", "NYK"), 
                                               c("DET", "DET", "CHI"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("IND", "WAS"), 
                                               c("IND", "POR"))
  }

  if (yr == 2030L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("LAC", "UTA"), c("CHA", "UTA"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("NOP", "ORL"), c("ORL", "NOP"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("PHX", "POR"), c("PHI", "WAS"))
  }

  if (yr == 2031L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("MIN", "GSW"), c("CHI", "DET"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("BOS", "CLE"), c("UTA", "BOS"))

    # ATL/HOU protected swap: ATL may swap for HOU only if HOU is 31-55.
    # HOU 56-60 is handled by the simple BOS convey56_60 row.
    if (!is.na(slots["HOU"]) && slots["HOU"] <= 55 &&
        !is.na(slots["ATL"]) && slots["HOU"] < slots["ATL"]) {
      owner_by_orig["HOU"] <- "ATL"
      owner_by_orig["ATL"] <- "HOU"
    }

    # IND/MIA/MEM pool:
    # more favorable IND/MIA to WAS;
    # more favorable of MEM and less favorable IND/MIA to MEM;
    # remaining least favorable to IND.
    im <- rank_teams_by_slot(slots, c("IND", "MIA"))
    if (length(im) == 2) {
      owner_by_orig[im[1]] <- "WAS"
      mem_pair <- rank_teams_by_slot(slots, c("MEM", im[2]))
      if (length(mem_pair) == 2) {
        owner_by_orig[mem_pair[1]] <- "MEM"
        owner_by_orig[mem_pair[2]] <- "IND"
      }
    }

    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("NOP", "ORL"), c("ORL", "OKC"))
  }

  if (yr == 2032L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, 
                                               c("HOU", "PHX"), c("CHI", "PHX"))

    # MEM/PHI swap: MEM may swap for PHI if PHI's second is better.
    if (!is.na(slots["MEM"]) && !is.na(slots["PHI"]) && slots["PHI"] < slots["MEM"]) {
      owner_by_orig["PHI"] <- "MEM"
      owner_by_orig["MEM"] <- "PHI"
    }
  }

  owner_by_orig
}

# sample_pick_value()
#   - simulated 4-year xRAPM WAR for one player drafted at slot `pos`
#   - picks 1-30 use draw_r1_outcomes(); picks 31-60 use draw_r2_outcomes()
#   - draw_idx / draw_idx_r2 = posterior draw rows; passing the same rows for
#     every pick in a simulation keeps one pick-value curve per simulation
#   - missing slot -> 0
sample_pick_value <- function(pos,
                              draw_idx = sample(nrow(pick_draws), 1),
                              draw_idx_r2 = sample(nrow(pick2_draws), 1)) {
  pos <- as.integer(pos)
  if (is.na(pos)) return(0)

  if (pos <= 30L) {
    pos <- max(1L, min(30L, pos))
    return(draw_r1_outcomes(draw_idx, pos))
  }

  pos <- max(31L, min(60L, pos))
  draw_r2_outcomes(draw_idx_r2, pos)
}

# value_allocated_future_assets()
#   - values every pick asset of one projected year and round in one simulation
#   - samples one player value per original team's slot (sample_pick_value)
#   - an asset gets that value only if owner_by_orig assigns the original
#     team's pick to the asset's owner; otherwise 0
#   - writes slot, raw value, owner's own slot and conveyance (0/1) into the
#     global asset_*_cur or asset_*_new matrices (system = "cur" / "new")
#   - output: updated team totals (team_value), pick counts (team_n) and
#     best single pick value (team_best)
value_allocated_future_assets <- function(sim, yr, draft_round, slots, 
                                          owner_by_orig, d_pick, d_pick2,
                                          team_value, team_n, team_best,
                                          system = c("cur", "new")) {
  system <- match.arg(system)
  yr_assets <- pick_assets %>%
    filter(.data$year == .env$yr, .data$round == .env$draft_round)
  raw_by_orig <- setNames(rep(0, length(all_teams)), all_teams)
  for (tm in all_teams) {
    raw_by_orig[tm] <- if (!is.na(slots[tm])) {
      sample_pick_value(slots[tm], draw_idx = d_pick, draw_idx_r2 = d_pick2) 
      } else 0
  }

  for (j in seq_len(nrow(yr_assets))) {
    aid <- yr_assets$asset_id[j]
    og  <- yr_assets$original_team[j]
    ow  <- yr_assets$owner[j]
    allocated <- !is.na(owner_by_orig[og]) && owner_by_orig[og] == ow
    val <- if (allocated) raw_by_orig[og] else 0

    if (system == "cur") {
      asset_cur[sim, aid] <<- val
      asset_slot_cur[sim, aid] <<- slots[og]
      asset_raw_cur[sim, aid] <<- raw_by_orig[og]
      asset_ownslot_cur[sim, aid] <<- slots[ow]
      asset_convey_cur[sim, aid] <<- as.integer(allocated)
    } else {
      asset_new[sim, aid] <<- val
      asset_slot_new[sim, aid] <<- slots[og]
      asset_raw_new[sim, aid] <<- raw_by_orig[og]
      asset_ownslot_new[sim, aid] <<- slots[ow]
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

# asset_value_mean_from_slots()
#   - Expected Pick Value (EPV) matrix: simulations x assets
#   - each cell = that simulation's posterior mean value (mu_1..mu_60 in
#     sim_curve_par) at the asset's simulated slot
#   - 0 when the asset has no slot or did not convey (convey_mat = 0)
asset_value_mean_from_slots <- function(slot_mat, convey_mat) {
  out <- matrix(
    0,
    nrow = nrow(slot_mat),
    ncol = ncol(slot_mat),
    dimnames = dimnames(slot_mat)
  )

  mu_mat <- as.matrix(sim_curve_par[, paste0("mu_", 1:60), drop = FALSE])

  for (aid in colnames(slot_mat)) {
    slots <- as.integer(slot_mat[, aid])
    active <- !is.na(slots)
    if (!is.null(convey_mat) && aid %in% colnames(convey_mat)) {
      active <- active & convey_mat[, aid] > 0
    }
    if (any(active)) {
      idx <- which(active)
      slot_idx <- pmin(pmax(slots[idx], 1L), 60L)
      out[idx, aid] <- mu_mat[cbind(idx, slot_idx)]
    }
  }

  out[is.na(out)] <- 0
  out
}

# build_display_draw_matrix()
#   - converts an internal asset value matrix (simulations x asset_id) to a
#     user-facing pick matrix (simulations x display_asset_id)
#   - each display column = row sum of its member asset columns
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

# build_display_convey_matrix()
#   - same as build_display_draw_matrix() for conveyance indicators
#   - output = number of picks received per simulation (usually 0/1; can be
#     2 for some protected or ranked-pool entitlements)
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
      out[, did] <- rowSums(convey_mat[, ids, drop = FALSE])
    }
  }
  out
}

# keep_display_cols()
#   - keeps only the listed columns of a matrix, in the listed order
#   - ids not present in the matrix are skipped; always returns a matrix
keep_display_cols <- function(mat, ids) {
  ids <- ids[ids %in% colnames(mat)]
  mat[, ids, drop = FALSE]
}
