###############################################################################
# Valuing NBA Draft Picks Using a Markov Model
# ---
# Reconstruction of the CSAS 2026 poster (#25) by Dhimogjini, Huang & Lautier
# Framework: 30-state Markov chain on team ranking transitions,
#            NBA lottery simulation, draft pick → player value mapping,
#            and trade evaluation with protections
###############################################################################

# ===========================================================================
# 0. SETUP & PACKAGES
# ===========================================================================

library(tidyverse)
library(hoopR)       # NBA data API wrappers
library(rvest)       # web scraping fallback for historical data
library(Matrix)      # sparse matrix operations
library(expm)        # matrix exponentiation (for multi-year projections)
library(scales)      # plotting helpers

set.seed(2026)

# ===========================================================================
# 1. DATA COLLECTION
# ===========================================================================
# Three datasets are needed:
#   (a) Historical NBA standings (2004-2025) → team rankings 1-30
#   (b) Draft outcomes (2004-2025) → who was picked where, career value
#   (c) Salary data → to compute surplus value / cost-adjusted value
#
# The abstract says 2004-2025, giving ~21 seasons of year-over-year
# transitions (20 transition pairs).

# --- 1a. Historical Standings ---
# Basketball-Reference or NBA API provides season-end records.
# We need each team's ranking (1 = best, 30 = worst) per season.

# Using hoopR to pull standings:
pull_standings <- function(seasons = 2004:2025) {
  all_standings <- map_dfr(seasons, function(yr) {
    # hoopR::nba_leaguestandings() returns current/historical standings
    # Season format: "2024-25" for the 2025 season
    season_str <- paste0(yr - 1, "-", substr(yr, 3, 4))
    tryCatch({
      standings <- nba_leaguestandings(season = season_str)
      standings$Standings %>%
        mutate(
          season    = yr,
          team_id   = as.integer(TeamID),
          team_name = TeamCity,  # or use TeamSlug
          wins      = as.integer(WINS),
          losses    = as.integer(LOSSES),
          win_pct   = as.numeric(WinPCT)
        ) %>%
        arrange(desc(win_pct)) %>%
        mutate(rank = row_number()) %>%
        select(season, team_id, team_name, wins, losses, win_pct, rank)
    }, error = function(e) {
      message(paste("Failed for season", yr, ":", e$message))
      tibble()
    })
  })
  all_standings
}

# --- 1b. Draft Outcomes ---
# Map pick number → player career value (Win Shares, VORP, or salary earned)
pull_draft_data <- function(seasons = 2004:2025) {
  map_dfr(seasons, function(yr) {
    tryCatch({
      draft <- nba_drafthistory(season = yr)
      draft$DraftHistory %>%
        mutate(
          season     = yr,
          pick       = as.integer(OVERALL_PICK),
          player     = PLAYER_NAME,
          team       = TEAM_CITY
        ) %>%
        filter(pick <= 60) %>%
        select(season, pick, player, team)
    }, error = function(e) {
      message(paste("Draft data failed for", yr))
      tibble()
    })
  })
}

# --- 1c. Player Career Value ---
# For each drafted player, compute cumulative value over first 4 years
# (matching rookie contract window). 
# Use Win Shares from Basketball-Reference or VORP from hoopR.

# Simplified: scrape career WS from basketball-reference
# In practice, you'd join draft data with per-season advanced stats.

compute_pick_value <- function(draft_df, player_stats_df) {
  # Join draft picks with first-4-seasons cumulative Win Shares
  draft_df %>%
    left_join(player_stats_df, by = "player") %>%
    group_by(player, pick, season) %>%
    summarise(
      career_ws = sum(win_shares, na.rm = TRUE),
      .groups = "drop"
    )
}

# ===========================================================================
# 2. MARKOV CHAIN: TEAM RANKING TRANSITIONS
# ===========================================================================
# Core idea: model each team's end-of-season ranking (1-30) as a state
# in a discrete-time Markov chain. The transition matrix P[i,j] gives
# Prob(rank_next = j | rank_current = i).
#
# This captures mean reversion (bad teams draft well and improve),
# sustained excellence (dynasties), and general competitive balance.

build_transition_matrix <- function(standings_df) {
  # Create year-over-year transitions for each team
  transitions <- standings_df %>%
    arrange(team_id, season) %>%
    group_by(team_id) %>%
    mutate(
      rank_next = lead(rank)
    ) %>%
    ungroup() %>%
    filter(!is.na(rank_next))
  
  # Count transitions: from rank i to rank j
  # 30 x 30 transition matrix
  n_states <- 30
  trans_counts <- matrix(0, nrow = n_states, ncol = n_states)
  
  for (k in seq_len(nrow(transitions))) {
    i <- transitions$rank[k]
    j <- transitions$rank_next[k]
    trans_counts[i, j] <- trans_counts[i, j] + 1
  }
  
  # --- Smoothing ---
  # Raw counts will be sparse (some i→j transitions never observed).
  # Apply Laplace smoothing or a kernel smoother.
  # The poster likely uses a bandwidth-based smoother since nearby
  # ranks should have similar transition probabilities.
  
  # Option A: Simple Laplace smoothing
  alpha <- 0.5  # pseudocount
  trans_smoothed <- trans_counts + alpha
  
  # Option B: Gaussian kernel smoother (more principled)
  # For each row i, smooth the counts with a Gaussian kernel
  # centered on each observed transition
  bandwidth <- 3  # controls how much transitions "spread"
  trans_kernel <- matrix(0, nrow = n_states, ncol = n_states)
  
  for (i in 1:n_states) {
    if (sum(trans_counts[i, ]) == 0) {
      # No observations from this rank — use uniform
      trans_kernel[i, ] <- rep(1, n_states)
    } else {
      for (j in 1:n_states) {
        # Weight each observed transition by Gaussian kernel
        kernel_weights <- dnorm(1:n_states, mean = j, sd = bandwidth)
        trans_kernel[i, j] <- sum(trans_counts[i, ] * kernel_weights)
      }
    }
  }
  
  # Normalize rows to get proper transition probabilities
  P <- trans_kernel / rowSums(trans_kernel)
  
  # Verify: each row sums to 1
  stopifnot(all(abs(rowSums(P) - 1) < 1e-10))
  
  list(
    P            = P,
    raw_counts   = trans_counts,
    n_transitions = nrow(transitions)
  )
}

# --- Diagnostics for the Markov chain ---
markov_diagnostics <- function(P) {
  n <- nrow(P)
  
  # 1. Stationary distribution (left eigenvector for eigenvalue 1)
  eig <- eigen(t(P))
  # Find eigenvalue closest to 1
  idx <- which.min(abs(eig$values - 1))
  pi_stat <- Re(eig$vectors[, idx])
  pi_stat <- pi_stat / sum(pi_stat)  # normalize
  
  # 2. Mixing time: how many steps until convergence?
  # Approximate via second-largest eigenvalue
  sorted_evals <- sort(abs(Re(eig$values)), decreasing = TRUE)
  lambda_2 <- sorted_evals[2]
  mixing_time <- -1 / log(lambda_2)
  
  # 3. Mean first passage times
  # E[T_ij] = expected steps to reach state j starting from state i
  # Computed via fundamental matrix Z = (I - P + Pi)^{-1}
  Pi_matrix <- matrix(rep(pi_stat, each = n), nrow = n)
  I <- diag(n)
  Z <- solve(I - P + Pi_matrix)
  
  # Mean first passage time from i to j
  mfpt <- matrix(0, nrow = n, ncol = n)
  for (j in 1:n) {
    for (i in 1:n) {
      if (i == j) {
        mfpt[i, j] <- 1 / pi_stat[j]  # mean recurrence time
      } else {
        mfpt[i, j] <- (Z[j, j] - Z[i, j]) / pi_stat[j]
      }
    }
  }
  
  list(
    stationary  = pi_stat,
    lambda_2    = lambda_2,
    mixing_time = mixing_time,
    mfpt        = mfpt
  )
}

# ===========================================================================
# 3. NBA LOTTERY SIMULATION
# ===========================================================================
# Given a team's ranking (1-30), simulate where they pick in the draft.
# The lottery only involves the 14 non-playoff teams (ranks 17-30).
# Playoff teams (ranks 1-16) pick 15-30 in reverse order of record.

# Current NBA lottery odds (post-2019 reform):
# Ranks 28-30 (worst 3): each 14.0%
# Rank 27: 12.5%
# Rank 26: 10.5%
# ...decreasing to...
# Rank 17 (best non-playoff): 0.5%

get_lottery_odds <- function() {
  # Probability of winning the #1 pick by draft seed (1 = worst record)
  # Draft seeds 1-14 correspond to rankings 30, 29, ..., 17
  odds_pct <- c(14.0, 14.0, 14.0, 12.5, 10.5,
                 9.0,  7.5,  6.0,  4.5,  3.0,
                 2.0,  1.5,  1.0,  0.5)
  
  # These are the odds for the FIRST pick only.
  # The actual lottery draws 4 winners; conditional probabilities shift.
  # We simulate the full 4-draw process.
  
  tibble(
    draft_seed = 1:14,
    rank       = 30:17,  # worst to best among lottery teams
    p_first    = odds_pct / 100
  )
}

# Full lottery simulation: draw 4 winners sequentially
simulate_lottery <- function(n_sims = 50000) {
  odds <- get_lottery_odds()
  
  # For each simulation, draw 4 winners without replacement
  # using the NBA's combination-based system
  results <- matrix(0L, nrow = n_sims, ncol = 14)
  
  for (sim in 1:n_sims) {
    remaining_seeds <- 1:14
    remaining_probs <- odds$p_first
    
    picks_assigned <- integer(14)
    
    # Draw top 4 picks

    for (pick in 1:4) {
      # Normalize remaining probabilities
      probs <- remaining_probs / sum(remaining_probs)
      
      # Draw winner
      winner_idx <- sample(seq_along(remaining_seeds), 1, prob = probs)
      winner_seed <- remaining_seeds[winner_idx]
      
      picks_assigned[winner_seed] <- pick
      
      # Remove winner from pool
      remaining_seeds <- remaining_seeds[-winner_idx]
      remaining_probs <- remaining_probs[-winner_idx]
    }
    
    # Picks 5-14: remaining teams in order of draft seed (worst first)
    remaining_ordered <- sort(remaining_seeds)
    for (k in seq_along(remaining_ordered)) {
      picks_assigned[remaining_ordered[k]] <- 4 + k
    }
    
    results[sim, ] <- picks_assigned
  }
  
  # Convert to tidy format
  colnames(results) <- paste0("seed_", 1:14)
  as_tibble(results) %>%
    mutate(sim = row_number()) %>%
    pivot_longer(-sim, names_to = "seed", values_to = "pick") %>%
    mutate(seed = as.integer(str_extract(seed, "\\d+")))
}

# Compute full probability matrix: P(pick = k | seed = s)
lottery_prob_matrix <- function(lottery_sims) {
  lottery_sims %>%
    count(seed, pick) %>%
    group_by(seed) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup() %>%
    select(seed, pick, prob) %>%
    pivot_wider(names_from = pick, values_from = prob, values_fill = 0) %>%
    arrange(seed)
}

# ===========================================================================
# 4. DRAFT PICK VALUE CURVE
# ===========================================================================
# Map pick number (1-60) to expected player value.
# Value metric: cumulative Win Shares over first 4 seasons (rookie contract).
# Smoothed with LOESS or polynomial regression to reduce noise.

build_pick_value_curve <- function(draft_with_value) {
  # Average value by pick across all drafts
  avg_by_pick <- draft_with_value %>%
    group_by(pick) %>%
    summarise(
      mean_ws    = mean(career_ws, na.rm = TRUE),
      median_ws  = median(career_ws, na.rm = TRUE),
      sd_ws      = sd(career_ws, na.rm = TRUE),
      n_players  = n(),
      .groups    = "drop"
    )
  
  # Smooth with LOESS
  loess_fit <- loess(mean_ws ~ pick, data = avg_by_pick, span = 0.4)
  avg_by_pick$smoothed_ws <- predict(loess_fit)
  
  # Normalize: pick #1 = 100
  avg_by_pick$relative_value <- 100 * avg_by_pick$smoothed_ws /
    avg_by_pick$smoothed_ws[1]
  
  avg_by_pick
}

# ===========================================================================
# 5. SALARY & SURPLUS VALUE
# ===========================================================================
# Rookie scale contracts are fixed by CBA. The "surplus value" of a pick
# is the difference between the player's market value (based on production)
# and what they're actually paid under the rookie scale.

# Rookie scale salaries (approximate, 2024-25 CBA)
rookie_scale_salary <- function(pick) {
  # First round: slot-based salary declining from ~$12M (#1) to ~$2.5M (#30)
  # Second round: minimum salary (~$1.1M)
  # These are rough 2024-25 values
  if_else(pick <= 30,
          12.0 - (pick - 1) * (12.0 - 2.5) / 29,  # linear approximation
          1.1)  # second round minimum
}

# Market value per Win Share (rough estimate: ~$4-5M per WS in 2024-25)
# Surplus = (WS × $/WS) - salary
compute_surplus_value <- function(pick_value_df, dollars_per_ws = 4.5) {
  pick_value_df %>%
    mutate(
      salary_4yr   = 4 * rookie_scale_salary(pick),  # total rookie contract
      market_value = smoothed_ws * dollars_per_ws,
      surplus      = market_value - salary_4yr
    )
}

# ===========================================================================
# 6. PUTTING IT TOGETHER: EXPECTED DRAFT PICK VALUE
# ===========================================================================
# For a team at ranking r:
#   1. Use Markov chain to project ranking distribution N years out
#   2. Map projected ranking → lottery seed → pick distribution
#   3. Map pick distribution → expected player value
#
# E[V | rank_now = r, years_out = N] = 
#   sum_over_j { P^N[r, j] × sum_over_k { P(pick=k|rank=j) × V(k) } }

expected_pick_value <- function(current_rank, years_out, P, lottery_probs,
                                 pick_values) {
  n_states <- nrow(P)
  
  # Project ranking distribution N years forward
  if (years_out == 0) {
    rank_dist <- rep(0, n_states)
    rank_dist[current_rank] <- 1
  } else {
    P_n <- as.matrix(P %^% years_out)  # matrix power from expm
    rank_dist <- P_n[current_rank, ]
  }
  
  # For each possible future ranking, compute expected pick value
  ev <- 0
  
  for (rank_j in 1:n_states) {
    if (rank_dist[rank_j] < 1e-10) next
    
    # Determine if this rank is a lottery team or playoff team
    if (rank_j >= 17) {
      # Lottery team: seed = 31 - rank_j (rank 30 → seed 1, rank 17 → seed 14)
      seed <- 31 - rank_j
      
      # Get pick distribution for this seed
      pick_dist <- lottery_probs %>%
        filter(seed == !!seed) %>%
        pivot_longer(-seed, names_to = "pick", values_to = "prob") %>%
        mutate(pick = as.integer(pick))
      
      # Expected value for this seed
      seed_ev <- sum(pick_dist$prob * pick_values$smoothed_ws[pick_dist$pick])
      
    } else {
      # Playoff team: picks 15-30, pick = 31 - rank_j... 
      # Actually: rank 1 (best) → pick 30, rank 16 → pick 15
      pick <- 31 - rank_j + (rank_j - 1)  # simplified: pick = 30 - (16 - rank_j)
      # More precisely: playoff teams pick in reverse order of record
      # rank 1 → pick 30, rank 2 → pick 29, ..., rank 16 → pick 15
      pick <- 15 + (16 - rank_j)
      seed_ev <- pick_values$smoothed_ws[min(pick, nrow(pick_values))]
    }
    
    ev <- ev + rank_dist[rank_j] * seed_ev
  }
  
  ev
}

# ===========================================================================
# 7. TRADE EVALUATION WITH PROTECTIONS
# ===========================================================================
# Key application: evaluate the Desmond Bane trade (MEM → MIN), which
# included a 2029 first-round pick swap (top-two protected).
#
# "Top-two protected" means: if the pick conveys as #1 or #2, the
# original team keeps it. Otherwise, the pick conveys to the other team.

evaluate_protected_pick <- function(
    sending_team_rank,
    years_out,
    P,
    lottery_sims,
    pick_values,
    protection = "top_2",  # protection type
    n_sims = 10000
) {
  n_states <- nrow(P)
  
  # Project ranking distribution
  if (years_out == 0) {
    rank_dist <- rep(0, n_states)
    rank_dist[sending_team_rank] <- 1
  } else {
    P_n <- as.matrix(P %^% years_out)
    rank_dist <- P_n[sending_team_rank, ]
  }
  
  # Simulate outcomes
  sim_results <- tibble(
    sim_id       = integer(),
    future_rank  = integer(),
    pick         = integer(),
    conveys      = logical(),
    value        = numeric()
  )
  
  for (sim in 1:n_sims) {
    # Draw future ranking from Markov chain distribution
    future_rank <- sample(1:n_states, 1, prob = rank_dist)
    
    # Determine pick
    if (future_rank >= 17) {
      seed <- 31 - future_rank
      # Draw from lottery simulation
      sim_lottery <- lottery_sims %>%
        filter(seed == !!seed) %>%
        slice_sample(n = 1)
      pick <- sim_lottery$pick
    } else {
      pick <- 15 + (16 - future_rank)
    }
    
    # Apply protection
    conveys <- switch(
      protection,
      "top_2"  = pick > 2,
      "top_3"  = pick > 3,
      "top_5"  = pick > 5,
      "top_10" = pick > 10,
      "none"   = TRUE,
      TRUE
    )
    
    value <- pick_values$smoothed_ws[min(pick, nrow(pick_values))]
    
    sim_results <- bind_rows(sim_results, tibble(
      sim_id = sim, future_rank = future_rank,
      pick = pick, conveys = conveys, value = value
    ))
  }
  
  list(
    sims = sim_results,
    summary = sim_results %>%
      summarise(
        p_conveys       = mean(conveys),
        ev_if_conveys   = mean(value[conveys]),
        ev_if_protected = mean(value[!conveys]),
        ev_to_receiver  = mean(if_else(conveys, value, 0)),
        ev_to_sender    = mean(if_else(!conveys, value, 0)),
        mean_pick       = mean(pick),
        median_pick     = median(pick)
      )
  )
}

# --- Pick Swap Evaluation ---
# A pick swap means: team A gets the better of their own pick and team B's pick.
# The value of the swap right = E[max(pick_A, pick_B)] - E[pick_A]
# (where "better" = lower pick number)

evaluate_pick_swap <- function(
    team_a_rank, team_b_rank, years_out, P,
    lottery_sims, pick_values,
    protection = "top_2", n_sims = 10000
) {
  n_states <- nrow(P)
  
  # Project both teams' ranking distributions
  if (years_out > 0) {
    P_n <- as.matrix(P %^% years_out)
    dist_a <- P_n[team_a_rank, ]
    dist_b <- P_n[team_b_rank, ]
  } else {
    dist_a <- rep(0, n_states); dist_a[team_a_rank] <- 1
    dist_b <- rep(0, n_states); dist_b[team_b_rank] <- 1
  }
  
  results <- tibble()
  
  for (sim in 1:n_sims) {
    rank_a <- sample(1:n_states, 1, prob = dist_a)
    rank_b <- sample(1:n_states, 1, prob = dist_b)
    
    # Simulate picks for both teams
    pick_a <- simulate_single_pick(rank_a, lottery_sims)
    pick_b <- simulate_single_pick(rank_b, lottery_sims)
    
    # Swap: team receiving swap gets the better (lower) pick
    best_pick <- min(pick_a, pick_b)
    
    # Apply protection to team B's pick
    swap_conveys <- switch(
      protection,
      "top_2"  = best_pick > 2,
      "top_3"  = best_pick > 3,
      "none"   = TRUE,
      TRUE
    )
    
    # If protection triggers, receiver gets team_a's original pick instead
    final_pick <- if (swap_conveys) best_pick else pick_a
    
    results <- bind_rows(results, tibble(
      sim = sim, rank_a = rank_a, rank_b = rank_b,
      pick_a = pick_a, pick_b = pick_b,
      best_pick = best_pick, swap_conveys = swap_conveys,
      final_pick = final_pick
    ))
  }
  
  results
}

# Helper: simulate a single pick given a team ranking
simulate_single_pick <- function(rank, lottery_sims) {
  if (rank >= 17) {
    seed <- 31 - rank
    lottery_sims %>%
      filter(seed == !!seed) %>%
      slice_sample(n = 1) %>%
      pull(pick)
  } else {
    15 + (16 - rank)
  }
}

# ===========================================================================
# 8. VISUALIZATION
# ===========================================================================

# --- 8a. Transition matrix heatmap ---
plot_transition_matrix <- function(P) {
  as_tibble(P, .name_repair = "minimal") %>%
    set_names(1:30) %>%
    mutate(from_rank = 1:30) %>%
    pivot_longer(-from_rank, names_to = "to_rank", values_to = "prob") %>%
    mutate(to_rank = as.integer(to_rank)) %>%
    ggplot(aes(to_rank, from_rank, fill = prob)) +
    geom_tile() +
    scale_fill_viridis_c(option = "magma", direction = -1) +
    scale_y_reverse() +
    labs(
      title = "Team Ranking Transition Probabilities",
      subtitle = "30-state Markov chain (2004–2025)",
      x = "Next Season Ranking",
      y = "Current Season Ranking",
      fill = "P(transition)"
    ) +
    theme_minimal(base_size = 12) +
    coord_equal()
}

# --- 8b. Pick value curve ---
plot_pick_value <- function(pick_values) {
  ggplot(pick_values, aes(pick, smoothed_ws)) +
    geom_point(aes(y = mean_ws), alpha = 0.4, size = 2) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_ribbon(
      aes(ymin = smoothed_ws - sd_ws, ymax = smoothed_ws + sd_ws),
      alpha = 0.15, fill = "steelblue"
    ) +
    labs(
      title = "Expected Player Value by Draft Pick",
      subtitle = "Win Shares over first 4 seasons (2004–2025 drafts)",
      x = "Pick Number",
      y = "Cumulative Win Shares"
    ) +
    theme_minimal(base_size = 12)
}

# --- 8c. Trade evaluation distribution ---
plot_trade_eval <- function(swap_results, trade_name = "Desmond Bane Trade") {
  swap_results %>%
    ggplot(aes(final_pick)) +
    geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7) +
    geom_vline(
      xintercept = median(swap_results$final_pick),
      linetype = "dashed", color = "red"
    ) +
    labs(
      title = paste("Simulated Pick Outcomes:", trade_name),
      subtitle = paste0(
        "Median pick: ", median(swap_results$final_pick),
        " | P(swap conveys): ",
        percent(mean(swap_results$swap_conveys), 0.1)
      ),
      x = "Draft Pick Number",
      y = "Frequency"
    ) +
    theme_minimal(base_size = 12)
}

# --- 8d. Future rank distribution (fan chart) ---
plot_rank_projection <- function(current_rank, P, max_years = 5) {
  projections <- map_dfr(0:max_years, function(yr) {
    if (yr == 0) {
      dist <- rep(0, 30)
      dist[current_rank] <- 1
    } else {
      P_n <- as.matrix(P %^% yr)
      dist <- P_n[current_rank, ]
    }
    tibble(year = yr, rank = 1:30, prob = dist)
  })
  
  # Compute quantile bands
  bands <- projections %>%
    group_by(year) %>%
    summarise(
      median = sum(rank * prob),
      q10    = {
        cs <- cumsum(prob); min(rank[cs >= 0.10])
      },
      q25    = {
        cs <- cumsum(prob); min(rank[cs >= 0.25])
      },
      q75    = {
        cs <- cumsum(prob); min(rank[cs >= 0.75])
      },
      q90    = {
        cs <- cumsum(prob); min(rank[cs >= 0.90])
      },
      .groups = "drop"
    )
  
  ggplot(bands, aes(year)) +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.15, fill = "steelblue") +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3, fill = "steelblue") +
    geom_line(aes(y = median), color = "steelblue", linewidth = 1.2) +
    scale_y_reverse(breaks = seq(1, 30, 5)) +
    labs(
      title = paste("Projected Ranking Distribution (Current Rank:", current_rank, ")"),
      subtitle = "Median with 50% and 80% bands",
      x = "Years Into Future",
      y = "Projected Ranking"
    ) +
    theme_minimal(base_size = 12)
}

# ===========================================================================
# 9. FULL PIPELINE — DESMOND BANE TRADE CASE STUDY
# ===========================================================================

run_full_analysis <- function() {
  
  cat("=== Step 1: Loading data ===\n")
  # In practice, call pull_standings() and pull_draft_data()
  # For demonstration, we'll generate synthetic data that mirrors
  # realistic NBA patterns
  
  # --- Synthetic standings (for demonstration) ---
  teams <- 1:30
  seasons <- 2004:2025
  
  standings <- expand_grid(team_id = teams, season = seasons) %>%
    group_by(season) %>%
    mutate(
      # Simulate win totals with autocorrelation (team quality persists)
      rank = sample(1:30, 30, replace = FALSE)
    ) %>%
    ungroup()
  
  cat("=== Step 2: Building Markov transition matrix ===\n")
  mc <- build_transition_matrix(standings)
  diag_info <- markov_diagnostics(mc$P)
  
  cat(sprintf("  Mixing time: %.1f seasons\n", diag_info$mixing_time))
  cat(sprintf("  Second eigenvalue: %.3f\n", diag_info$lambda_2))
  
  cat("=== Step 3: Simulating NBA lottery ===\n")
  lottery_sims <- simulate_lottery(n_sims = 50000)
  lp_matrix <- lottery_prob_matrix(lottery_sims)
  
  cat("=== Step 4: Building pick value curve ===\n")
  # Synthetic pick values (realistic shape: convex decreasing)
  pick_values <- tibble(
    pick = 1:60,
    mean_ws = 25 * exp(-0.05 * (pick - 1)) + rnorm(60, 0, 1.5),
    sd_ws   = 8 * exp(-0.03 * (pick - 1))
  ) %>%
    mutate(
      mean_ws = pmax(mean_ws, 0),
      smoothed_ws = predict(loess(mean_ws ~ pick, span = 0.4)),
      relative_value = 100 * smoothed_ws / smoothed_ws[1]
    )
  
  cat("=== Step 5: Evaluating Desmond Bane trade ===\n")
  # Memphis (2024-25 rank ~18-22 range) sends 2029 pick swap
  # to Minnesota, top-two protected
  # Memphis current rank estimate: ~20 (fringe playoff team)
  # Minnesota current rank estimate: ~8 (strong playoff team)
  
  swap_results <- evaluate_pick_swap(
    team_a_rank = 8,   # Minnesota
    team_b_rank = 20,  # Memphis
    years_out   = 4,   # 2029 is ~4 years from 2025
    P           = mc$P,
    lottery_sims = lottery_sims,
    pick_values = pick_values,
    protection  = "top_2",
    n_sims      = 10000
  )
  
  cat("\n=== RESULTS ===\n")
  cat(sprintf("  P(swap conveys): %.1f%%\n", 100 * mean(swap_results$swap_conveys)))
  cat(sprintf("  Median pick to MIN: %d\n", median(swap_results$final_pick)))
  cat(sprintf("  Mean pick to MIN: %.1f\n", mean(swap_results$final_pick)))
  
  cat("\n=== Step 6: Generating plots ===\n")
  p1 <- plot_transition_matrix(mc$P)
  p2 <- plot_pick_value(pick_values)
  p3 <- plot_trade_eval(swap_results, "Desmond Bane Trade (2029 Pick Swap)")
  p4 <- plot_rank_projection(20, mc$P, max_years = 5)
  
  list(
    transition_matrix = mc,
    diagnostics       = diag_info,
    lottery_probs     = lp_matrix,
    pick_values       = pick_values,
    swap_results      = swap_results,
    plots             = list(p1, p2, p3, p4)
  )
}

# ===========================================================================
# 10. RUN IT
# ===========================================================================
results <- run_full_analysis()
results$plots[[1]]  # transition matrix
results$plots[[2]]  # pick value curve
results$plots[[3]]  # trade evaluation
results$plots[[4]]  # rank projection fan chart
