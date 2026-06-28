## ═════════════════════════════════════════════════════════════════════════════
## 07 - BUILD RANK-STATE MARKOV TRANSITION DATA -------------------------------
## ═════════════════════════════════════════════════════════════════════════════

cat("\n--- Building 30-Rank Markov Transition Counts ---\n")

# Production team-strength state: exact league-wide draft rank, where
#   rank_worst = 1  is the worst record / old lottery seed 1
#   rank_worst = 30 is the best record / pick 30 by inverse record.
# This rank-state model replaces the old 5-tier production simulator. The five
# 3-2-1 tiers are retained below as derived validation / display buckets.
N_RANKS <- 30L
RANK_STATES <- seq_len(N_RANKS)
RANK_STATE_LABELS <- sprintf("Rank %02d", RANK_STATES)

if (!"rank_worst" %in% names(all_standings)) {
  all_standings <- all_standings %>%
    group_by(season) %>%
    mutate(rank_worst = as.integer(max(overall_rank, na.rm = TRUE) + 1L - overall_rank)) %>%
    ungroup()
}

current_standings <- current_standings %>%
  mutate(rank_worst = as.integer(max(overall_rank, na.rm = TRUE) + 1L - overall_rank))

current_rank_worst0 <- setNames(as.integer(current_standings$rank_worst),
                                current_standings$abbr)

rank_to_tier_from_worst <- function(rank_worst) {
  rank_worst <- as.integer(rank_worst)
  dplyr::case_when(
    rank_worst <= 3L  ~ "relegation",
    rank_worst <= 10L ~ "nonplayin",
    rank_worst <= 14L ~ "playin_seed",
    rank_worst <= 16L ~ "playin_loser",
    TRUE              ~ "playoff"
  )
}

rank_tier_lookup <- tibble(
  rank_worst = RANK_STATES,
  tier = factor(rank_to_tier_from_worst(rank_worst), levels = TIERS)
)

rank_transitions <- all_standings %>%
  arrange(abbr, season) %>%
  group_by(abbr) %>%
  mutate(rank_next = lead(rank_worst), season_next = lead(season)) %>%
  ungroup() %>%
  filter(!is.na(rank_next), season_next == season + 1) %>%
  transmute(abbr, season,
            rank_worst = as.integer(rank_worst),
            rank_next = as.integer(rank_next))

rank_counts_mat <- matrix(0L, N_RANKS, N_RANKS,
                          dimnames = list(from = RANK_STATE_LABELS,
                                          to   = RANK_STATE_LABELS))
for (r in seq_len(nrow(rank_transitions))) {
  i <- rank_transitions$rank_worst[r]
  j <- rank_transitions$rank_next[r]
  rank_counts_mat[i, j] <- rank_counts_mat[i, j] + 1L
}

cat("  Observed rank transition count total:\n")
print(sum(rank_counts_mat))
cat("  Row totals by current rank:\n")
print(rowSums(rank_counts_mat))

# Keep the old five-tier counts as a benchmark and as a dashboard-friendly
# aggregation of the rank model. These are no longer the production simulator.
tier_transitions <- all_standings %>%
  arrange(abbr, season) %>%
  group_by(abbr) %>%
  mutate(tier_next = lead(tier), season_next = lead(season)) %>%
  ungroup() %>%
  filter(!is.na(tier_next), season_next == season + 1)

tier_counts_mat <- matrix(0L, N_TIERS, N_TIERS,
                          dimnames = list(from = TIERS, to = TIERS))
for (r in seq_len(nrow(tier_transitions))) {
  i <- match(as.character(tier_transitions$tier[r]), TIERS)
  j <- match(as.character(tier_transitions$tier_next[r]), TIERS)
  tier_counts_mat[i, j] <- tier_counts_mat[i, j] + 1L
}

cat("\n  Five-tier benchmark transition counts:\n")
print(tier_counts_mat)

# Dirichlet prior for the legacy 5-tier benchmark only. The production rank
# model below uses a smoothed softmax surface rather than a conjugate Dirichlet.
build_alpha <- function(K, stay = 3, adj = 1.5, far = 0.4, decay = 0.6) {
  a <- matrix(far, K, K)
  for (i in 1:K) for (j in 1:K) {
    d <- abs(i - j)
    a[i, j] <- if (d == 0) stay else if (d == 1) adj
    else max(far, adj * decay^(d - 1))
  }
  a
}

tier_alpha_prior <- build_alpha(N_TIERS)
tier_posterior_mean_closed <- (tier_counts_mat + tier_alpha_prior) /
  rowSums(tier_counts_mat + tier_alpha_prior)

# Backward-compatible aliases for scripts that inspect transition counts. From
# v2 onward, counts_mat is the production 30-rank count matrix.
counts_mat <- rank_counts_mat
posterior_mean_closed <- tier_posterior_mean_closed

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
## 08 - FIT STAN MODELS --------------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════


# Player-level pick-value data
pick_fit_data <- draft_4yr %>%
  transmute(draft_year = as.integer(draft_year),
            pick       = as.integer(pick),
            player     = as.character(player),
            ws4        = as.numeric(ws4)) %>%
    filter(!is.na(pick), pick >= 1, pick <= 30, !is.na(ws4))

pick_stan_data_player <- list(N    = nrow(pick_fit_data),
                              pick = pick_fit_data$pick,
                              ws4  = pick_fit_data$ws4)

# The oldest model has the legacy slot-mean data names
pick_stan_data_legacy <- list(N       = nrow(pick_fit_data),
                              pick    = pick_fit_data$pick,
                              war_obs = pick_fit_data$ws4,
                              war_se  = rep(0, nrow(pick_fit_data)))


# Constant Sigma Model
model_r1_v1 <- cmdstan_model("02_models/zz_Archive/picks_Round1_0601.stan")
fit_r1_v1 <- model_r1_v1$sample(data            = pick_stan_data_legacy,
                                chains          = 4,
                                parallel_chains = 4,
                                iter_warmup     = 1000,
                                iter_sampling   = 2000,
                                adapt_delta     = 0.95,
                                max_treedepth   = 12,
                                seed            = 2026,
                                refresh         = 100)
fit_r1_v1$cmdstan_diagnose()


# Linear Sigma Model
model_r1_v2 <- cmdstan_model("02_models/zz_Archive/picks_Round1_0605.stan")
fit_r1_v2 <- model_r1_v2$sample(data            = pick_stan_data_player,
                                chains          = 4,
                                parallel_chains = 4,
                                iter_warmup     = 1000,
                                iter_sampling   = 2000,
                                adapt_delta     = 0.95,
                                max_treedepth   = 12,
                                seed            = 2026,
                                refresh         = 100)
fit_r1_v2$cmdstan_diagnose()


# Random Walk Hierarchical Model (Production Version)
model_r1_v3 <- cmdstan_model("02_models/picks_Round1.stan")
fit_r1_v3 <- model_r1_v3$sample(data            = pick_stan_data_player,
                                chains          = 4,
                                parallel_chains = 4,
                                iter_warmup     = 1000,
                                iter_sampling   = 2000,
                                adapt_delta     = 0.95,
                                max_treedepth   = 12,
                                seed            = 2026,
                                refresh         = 100)
fit_r1_v3$cmdstan_diagnose()



# ---- ROUND 2: structural-zero hurdle model
pick_fit_data_r2 <- draft_4yr_r2 %>%
  transmute(draft_year = as.integer(draft_year),
            pick       = as.integer(pick),
            player     = as.character(player),
            played     = as.integer(played),
            ws4        = as.numeric(ws4)) %>%
  filter(!is.na(pick), pick >= 31, pick <= 60, !is.na(played), !is.na(ws4))

# Practical lower bound for the shifted-lognormal R2 outcome model. Keep a
# stable basketball floor around -8 WS, but move it lower if the historical
# played-player data ever requires it so Stan's log shift is always valid.
R2_WS_FLOOR <- min(-2, min(pick_fit_data_r2$ws4[pick_fit_data_r2$played == 1], 
                           na.rm = TRUE) - 0.25)

pick_stan_data_r2 <- list(N        = nrow(pick_fit_data_r2),
                          pick     = pick_fit_data_r2$pick,
                          played   = pick_fit_data_r2$played,
                          ws4      = pick_fit_data_r2$ws4,
                          ws_floor = R2_WS_FLOOR)

model_r2 <- cmdstan_model("02_models/picks_Round2.stan")
fit_r2 <- model_r2$sample(data            = pick_stan_data_r2,
                          chains          = 4,
                          parallel_chains = 4,
                          iter_warmup     = 1000,
                          iter_sampling   = 2000,
                          adapt_delta     = 0.99,
                          max_treedepth   = 12,
                          seed            = 2026,
                          refresh         = 100)
fit_r2$cmdstan_diagnose()





## ═════════════════════════════════════════════════════════════════════════════
## 09 - VALIDATE STAN MODELS ---------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════
# Order:
#   1. Shared validation helpers
#   2. Round 1 pick-value model validation
#   3. Round 2 pick-value model validation
#   4. Markov transition model validation



### 09.01 - SHARED VALIDATION FUNCTIONS -----------------------------------------

validate_header <- function(title) {
  cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
}


get_fit_object <- function(primary, fallback = NULL) {
  if (exists(primary, inherits = TRUE)) return(get(primary, inherits = TRUE))
  if (!is.null(fallback) && exists(fallback, inherits = TRUE)) {
    return(get(fallback, inherits = TRUE))
  }
  stop("Missing fit object: ", primary, call. = FALSE)
}


draws_matrix <- function(fit, variable) {
  as.matrix(fit$draws(variables = variable, format = "draws_matrix"))
}


orient_draws_x_obs <- function(mat, n_obs, name = "draw matrix") {
  mat <- as.matrix(mat)
  if (ncol(mat) == n_obs) return(mat)
  if (nrow(mat) == n_obs) {
    warning(name, " was observations x draws; transposing to draws x observations.")
    return(t(mat))
  }
  stop(name, " has dimensions ", paste(dim(mat), collapse = " x "),
       "; expected draws x ", n_obs, ".", call. = FALSE)
}


existing_stan_vars <- function(fit, vars) {
  vars[vapply(vars, function(v) {
    !inherits(try(fit$summary(variables = v), silent = TRUE), "try-error")
  }, logical(1))]
}


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


compute_ebfmi <- function(energy) {
  energy <- energy[is.finite(energy)]
  if (length(energy) < 3 || is.na(stats::var(energy)) || stats::var(energy) == 0) {
    return(NA_real_)
  }
  mean(diff(energy)^2) / stats::var(energy)
}


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
              ebfmi = compute_ebfmi(.data$energy__),
              .groups = "drop") %>%
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


first_round_band <- function(pick) {
  case_when(pick <= 5  ~ "1-5",
            pick <= 10 ~ "6-10",
            pick <= 15 ~ "11-15",
            pick <= 20 ~ "16-20",
            TRUE       ~ "21-30")
}


second_round_band <- function(pick) {
  case_when(pick <= 35 ~ "31-35",
            pick <= 40 ~ "36-40",
            pick <= 45 ~ "41-45",
            pick <= 50 ~ "46-50",
            pick <= 55 ~ "51-55",
            TRUE       ~ "56-60")
}

second_round_band10 <- function(pick) {
  case_when(pick <= 40 ~ "31-40",
            pick <= 50 ~ "41-50",
            TRUE       ~ "51-60")
}

R2_BANDS <- c("31-35", "36-40", "41-45", "46-50", "51-55", "56-60")



### 09.02 - ROUND 1 PICK VALUE MODEL VALIDATION -------------------------------

validate_header("Round 1 pick-value model validation")

# PSIS-LOO comparison across all three first round pick models
loo_r1_v1 <- loo::loo(as.matrix(fit_r1_v1$draws("log_lik", format = "draws_matrix")))
loo_r1_v2 <- loo::loo(as.matrix(fit_r1_v2$draws("log_lik", format = "draws_matrix")))
loo_r1_v3 <- loo::loo(as.matrix(fit_r1_v3$draws("log_lik", format = "draws_matrix")))

pick_loo_compare <- loo_compare(loo_r1_v1,
                                loo_r1_v2,
                                loo_r1_v3)

as.data.frame(pick_loo_compare) %>%
  tibble::rownames_to_column("model") %>%
  as_tibble()

pareto_k_table(loo_r1_v1)
pareto_k_table(loo_r1_v2)
pareto_k_table(loo_r1_v3)


# Production first-round model parameters.
pick_params <- c("alpha", "beta", "gamma", "log_sigma_1", "tau_log_sigma_rw", "nu")

pick_draws <- fit_r1_v3$draws(variables = existing_stan_vars(fit_r1_v3,
                                                             c("alpha", 
                                                               "beta", 
                                                               "gamma", 
                                                               "tau_log_sigma_rw", 
                                                               "nu")), 
                              format = "df") %>%
  as_tibble()

# Summary of parameters
print(fit_r1_v3$summary(pick_params))

pick_diag <- summarise_rhat_ess(fit = fit_r1_v3,
                                variables = pick_params,
                                model_name = "Round 1 Production RW-sigma model")

cat("\n  Round 1 NUTS diagnostics\n")
nuts_r1 <- summarise_nuts(fit_r1_v3, "round1_rw_sigma", max_treedepth = 12L)


pick_mu_draws <- extract_stan_vector_draws(fit_r1_v3, "ws4_pred", K = 30L)

pick_sd_draws <- extract_stan_vector_draws(fit_r1_v3, "ws4_pred_sd", K = 30L)

# One player-outcome draw per pick for curve display. EAV still uses pick_mu_draws.
pick_outcome_draws <- matrix(NA_real_,
                             nrow = nrow(pick_mu_draws),
                             ncol = 30,
                             dimnames = list(NULL, paste0("pick_", 1:30)))

for (pk in 1:30) {
  pick_outcome_draws[, pk] <- pick_mu_draws[, pk] +
    pick_sd_draws[, pk] * stats::rt(nrow(pick_mu_draws), df = pick_draws$nu)
}

# Player-level PPC for the production first-round model.
ws4_rep_mat <- draws_matrix(fit_r1_v3, "ws4_rep")
ws4_rep_mat <- orient_draws_x_obs(ws4_rep_mat, nrow(pick_fit_data), "Round 1 ws4_rep")

ppc_tbl <- tibble(row_id = seq_len(nrow(pick_fit_data)),
                  draft_year = pick_fit_data$draft_year,
                  pick = pick_fit_data$pick,
                  player = pick_fit_data$player,
                  obs = pick_fit_data$ws4,
                  pred_mean = colMeans(ws4_rep_mat),
                  lo = apply(ws4_rep_mat, 2, quantile, probs = 0.05, na.rm = TRUE),
                  hi = apply(ws4_rep_mat, 2, quantile, probs = 0.95, na.rm = TRUE)) %>%
  mutate(covered = obs >= lo & obs <= hi,
         pick_band = first_round_band(pick))

cat(sprintf("\n  Round 1 PPC 90%% coverage: %.1f%% of player rows\n",
            100 * mean(ppc_tbl$covered)))

ppc_band_tbl <- ppc_tbl %>%
  group_by(pick_band) %>%
  summarise(n = n(),
            coverage_90 = mean(covered),
            mean_obs = mean(obs),
            mean_pred = mean(pred_mean),
            .groups = "drop")

cat("\n  Round 1 PPC coverage by pick band\n")
print(ppc_band_tbl)

ppc_pick_resid <- ppc_tbl %>%
  group_by(pick) %>%
  summarise(n = n(),
            obs_mean = mean(obs),
            pred_mean = mean(pred_mean),
            resid = obs_mean - pred_mean,
            coverage_90 = mean(covered),
            .groups = "drop")

cat("\n  Round 1 mean residuals by exact pick\n")
print(ppc_pick_resid, n = 30)

sigma_curve <- tibble(pick = 1:30,
                      sigma_mean = colMeans(pick_sd_draws),
                      sigma_q05 = apply(pick_sd_draws, 2, quantile, 
                                        probs = 0.05, na.rm = TRUE),
                      sigma_q50 = apply(pick_sd_draws, 2, quantile, 
                                        probs = 0.50, na.rm = TRUE),
                      sigma_q95 = apply(pick_sd_draws, 2, quantile, 
                                        probs = 0.95, na.rm = TRUE))

cat("\n  Round 1 learned residual-scale curve\n")
print(sigma_curve, n = 30)



### 09.03 - ROUND 2 PICK VALUE MODEL VALIDATION -------------------------------

validate_header("Round 2 pick-value model validation")

pick_fit_data_r2 <- pick_fit_data_r2 %>%
  mutate(pick_band = second_round_band(pick))

# Core parameters
pick2_params <- c("eta_31", "d_pi", "tau_pi",
                  "m_31", "d_m", "tau_m",
                  "s_log_31", "tau_s",
                  "u_eta_31", "d_u",
                  "u_31", "u_45", "u_60",
                  "delta", "kappa")

# Summary of parameters
print(fit_r2$summary(pick2_params))

pick2_params <- existing_stan_vars(fit_r2, pick2_params)

pick2_draws <- fit_r2$draws(variables = pick2_params,
                            format = "df") %>%
  as_tibble()

pick2_diag <- summarise_rhat_ess(fit = fit_r2,
                                 variables = pick2_params,
                                 model_name = "Round 2 smoothed hurdle model")

cat("\n  Round 2 NUTS diagnostics\n")
nuts_r2 <- summarise_nuts(fit_r2, "round2_smoothed_hurdle", max_treedepth = 12L)


# Round 2 generated curves used by downstream simulation and app exports.
pick2_p_play_draws      <- extract_stan_vector_draws(fit_r2, "pi_p", K = 30L)
pick2_cond_mu_draws     <- extract_stan_vector_draws(fit_r2, "mean_play", K = 30L)
pick2_cond_scale_draws  <- extract_stan_vector_draws(fit_r2, "sd_typ", K = 30L)
pick2_cond_sd_draws     <- extract_stan_vector_draws(fit_r2, "sd_play", K = 30L)
pick2_mu_draws          <- extract_stan_vector_draws(fit_r2, "ev", K = 30L)
pick2_mu_draws          <- extract_stan_vector_draws(fit_r2, "ev_sd", K = 30L)
pick2_outcome_draws     <- extract_stan_vector_draws(fit_r2, "y_pick_rep", K = 30L)
pick2_upside_prob_draws <- extract_stan_vector_draws(fit_r2, "u_prob", K = 30L)
n_pick2_draws <- nrow(pick2_p_play_draws)


# Useful identity check: EV should equal P(play) x E[WS4 | played].
pick2_cond_m2_draws <- pick2_cond_sd_draws^2 + pick2_cond_mu_draws^2
r2_ev_identity_check <- tibble(pick = 31:60,
                               pick_band = second_round_band(pick),
                               p_play = colMeans(pick2_p_play_draws),
                               cond_played_mean_ws = colMeans(pick2_cond_mu_draws),
                               manual_ev = p_play * cond_played_mean_ws,
                               model_ev = colMeans(pick2_mu_draws),
                               diff = model_ev - manual_ev)

cat("\n  Round 2 EV identity check\n")
print(r2_ev_identity_check, n = 30)

played_rep_mat_r2 <- extract_stan_vector_draws(fit_r2, "play_rep", K = 30L)



played_rep_mat_r2 <- orient_draws_x_obs(played_rep_mat_r2,
                                        nrow(pick_fit_data_r2),
                                        "Round 2 played_rep")


ws4_rep_mat_r2 <- extract_stan_vector_draws(fit_r2, "y_rep", K = 30L)
ws4_rep_mat_r2 <- orient_draws_x_obs(ws4_rep_mat_r2,
                                     nrow(pick_fit_data_r2),
                                     "Round 2 ws4_rep")

ppc_tbl_r2 <- tibble(row_id = seq_len(nrow(pick_fit_data_r2)),
                     draft_year = pick_fit_data_r2$draft_year,
                     pick = pick_fit_data_r2$pick,
                     pick_band = pick_fit_data_r2$pick_band,
                     player = pick_fit_data_r2$player,
                     played = pick_fit_data_r2$played,
                     obs = pick_fit_data_r2$ws4,
                     lo = apply(ws4_rep_mat_r2, 2, quantile, 
                                probs = 0.05, na.rm = TRUE),
                     hi = apply(ws4_rep_mat_r2, 2, quantile, 
                                probs = 0.95, na.rm = TRUE),
                     played_rep_prob = colMeans(played_rep_mat_r2)) %>%
  mutate(covered = obs >= lo & obs <= hi)

cat(sprintf("  Round 2 PPC 90%% coverage: %.1f%% of player rows\n",
            100 * mean(ppc_tbl_r2$covered)))

cat(sprintf("  Round 2 empirical played rate %.1f%% | posterior predictive %.1f%%\n",
            100 * mean(ppc_tbl_r2$played == 1),
            100 * mean(ppc_tbl_r2$played_rep_prob)))

ppc_coverage_by_played_r2 <- ppc_tbl_r2 %>%
  group_by(played) %>%
  summarise(n = n(),
            coverage_90 = mean(covered),
            miss_low = mean(obs < lo),
            miss_high = mean(obs > hi),
            mean_obs = mean(obs),
            mean_lo = mean(lo),
            mean_hi = mean(hi),
            .groups = "drop")

cat("\n  Round 2 PPC coverage by played flag\n")
print(ppc_coverage_by_played_r2)

ppc_coverage_by_bucket_r2 <- ppc_tbl_r2 %>%
  filter(played == 1) %>%
  mutate(obs_bucket = case_when(obs <= 0  ~ "<=0 WS",
                                obs <= 2  ~ "0-2 WS",
                                obs <= 5  ~ "2-5 WS",
                                obs <= 10 ~ "5-10 WS",
                                TRUE      ~ "10+ WS")) %>%
  group_by(obs_bucket) %>%
  summarise(n = n(),
            coverage_90 = mean(covered),
            miss_low = mean(obs < lo),
            miss_high = mean(obs > hi),
            mean_obs = mean(obs),
            mean_hi = mean(hi),
            .groups = "drop")

cat("\n  Round 2 PPC coverage by played-player outcome bucket\n")
print(ppc_coverage_by_bucket_r2)

ws_dist_band_check <- purrr::map_dfr(R2_BANDS, function(b) {
  idx <- which(pick_fit_data_r2$pick_band == b)
  obs <- pick_fit_data_r2$ws4[idx]
  rep <- as.vector(ws4_rep_mat_r2[, idx, drop = FALSE])
  
  tibble(pick_band = b,
         n = length(idx),
         obs_mean = mean(obs),
         rep_mean = mean(rep),
         obs_q05 = quantile(obs, 0.05),
         rep_q05 = quantile(rep, 0.05),
         obs_q50 = quantile(obs, 0.50),
         rep_q50 = quantile(rep, 0.50),
         obs_q95 = quantile(obs, 0.95),
         rep_q95 = quantile(rep, 0.95),
         obs_ge_5 = mean(obs >= 5),
         rep_ge_5 = mean(rep >= 5),
         obs_ge_10 = mean(obs >= 10),
         rep_ge_10 = mean(rep >= 10))
})

cat("\n  Round 2 distribution check by five-pick band\n")
print(ws_dist_band_check)

band10_check <- pick_fit_data_r2 %>%
  mutate(band10 = second_round_band10(pick)) %>%
  group_by(band10) %>%
  summarise(n = n(),
            mean_ws4 = mean(ws4),
            p_play = mean(played),
            p_ge_5 = mean(ws4 >= 5),
            p_ge_10 = mean(ws4 >= 10),
            q05 = quantile(ws4, 0.05),
            q95 = quantile(ws4, 0.95),
            .groups = "drop")

cat("\n  Round 2 empirical summary by ten-pick band\n")
print(band10_check)

r2_overall_check <- pick_fit_data_r2 %>%
  summarise(n = n(),
            mean_ws4 = mean(ws4),
            p_play = mean(played),
            p_ge_5 = mean(ws4 >= 5),
            p_ge_10 = mean(ws4 >= 10),
            q05 = quantile(ws4, 0.05),
            q95 = quantile(ws4, 0.95))

cat("\n  Round 2 empirical overall summary\n")
print(r2_overall_check)

# P(play) calibration by exact slot.
play_rep_slot_rate_mat_r2 <- sapply(31:60, function(pk) {
  idx <- which(pick_fit_data_r2$pick == pk)
  if (length(idx) == 0L) return(rep(NA_real_, nrow(played_rep_mat_r2)))
  rowMeans(played_rep_mat_r2[, idx, drop = FALSE])
})
colnames(play_rep_slot_rate_mat_r2) <- as.character(31:60)

ppc_play_pick_r2 <- tibble(pick = 31:60,
                           n = as.integer(table(factor(pick_fit_data_r2$pick, 
                                                       levels = 31:60))),
                           emp_p_play = as.numeric(tapply(pick_fit_data_r2$played,
                                                          factor(pick_fit_data_r2$pick, 
                                                                 levels = 31:60),
                                                          mean)),
                           model_p_play_mean = colMeans(pick2_p_play_draws),
                           model_p_play_q05 = apply(pick2_p_play_draws, 2, 
                                                    quantile, 0.05, na.rm = TRUE),
                           model_p_play_q50 = apply(pick2_p_play_draws, 2, 
                                                    quantile, 0.50, na.rm = TRUE),
                           model_p_play_q95 = apply(pick2_p_play_draws, 2, 
                                                    quantile, 0.95, na.rm = TRUE),
                           pred_rep_p_play_q05 = apply(play_rep_slot_rate_mat_r2, 2, 
                                                       quantile, 0.05, na.rm = TRUE),
                           pred_rep_p_play_q50 = apply(play_rep_slot_rate_mat_r2, 2, 
                                                       quantile, 0.50, na.rm = TRUE),
                           pred_rep_p_play_q95 = apply(play_rep_slot_rate_mat_r2, 2, 
                                                       quantile, 0.95, na.rm = TRUE)) %>%
  mutate(emp_within_90_rep_interval = emp_p_play >= pred_rep_p_play_q05 &
           emp_p_play <= pred_rep_p_play_q95)

cat("\n  Round 2 P(play) calibration by exact pick\n")
print(ppc_play_pick_r2, n = 30)

cat(sprintf("  Round 2 exact-pick empirical P(play) inside 90%% PPC intervals: %.1f%%\n",
            100 * mean(ppc_play_pick_r2$emp_within_90_rep_interval, na.rm = TRUE)))

# P(play) calibration by five-pick band.
pplay_band_draws_r2 <- sapply(R2_BANDS, function(b) {
  keep <- second_round_band(31:60) == b
  rowMeans(pick2_p_play_draws[, keep, drop = FALSE])
})
colnames(pplay_band_draws_r2) <- R2_BANDS

ppc_play_band_r2 <- pick_fit_data_r2 %>%
  group_by(pick_band) %>%
  summarise(n = n(),
            emp_p_play = mean(played),
            .groups = "drop") %>%
  mutate(model_p_play_mean = colMeans(pplay_band_draws_r2)[as.character(pick_band)],
         model_p_play_q05 = apply(pplay_band_draws_r2, 2, 
                                  quantile, 0.05)[as.character(pick_band)],
         model_p_play_q50 = apply(pplay_band_draws_r2, 2, 
                                  quantile, 0.50)[as.character(pick_band)],
         model_p_play_q95 = apply(pplay_band_draws_r2, 2, 
                                  quantile, 0.95)[as.character(pick_band)])

cat("\n  Round 2 P(play) calibration by pick band\n")
print(ppc_play_band_r2)

loo_pick_r2 <- loo::loo(draws_matrix(fit_r2, "log_lik"))

cat("\n  Round 2 production model LOO\n")
print(loo_pick_r2)
print(loo::pareto_k_table(loo_pick_r2))



### 09.04 - RANK-STATE MARKOV TRANSITION MODEL VALIDATION ----------------------
cat("\n--- Fitting 30-Rank Team-Strength Stan Model ---\n")

TEAM_STRENGTH_V2_PATH <- if (file.exists("02_models/team_strength_v2.stan")) {
  "02_models/team_strength_v2.stan"
} else {
  "team_strength_v2.stan"
}

markov_model <- cmdstan_model(TEAM_STRENGTH_V2_PATH)

markov_fit <- markov_model$sample(data = list(K = N_RANKS,
                                              counts = rank_counts_mat),
                                  chains = 4,
                                  parallel_chains = 4,
                                  iter_warmup = 1000,
                                  iter_sampling = 2000,
                                  adapt_delta = 0.95,
                                  max_treedepth = 12,
                                  seed = 2026,
                                  refresh = 100)

nuts_markov <- summarise_nuts(markov_fit, "rank_markov_transition", max_treedepth = 12L)
markov_fit$cmdstan_diagnose()
markov_fit$cmdstan_summary()

# theta[draw, from_rank, to_rank], where rank 1 = worst and rank 30 = best.
theta_draws <- draws_matrix(markov_fit, "theta")
n_markov_draws <- nrow(theta_draws)

get_theta_row <- function(draw_idx, i) {
  vapply(seq_len(N_RANKS),
         function(j) theta_draws[draw_idx, sprintf("theta[%d,%d]", i, j)],
         numeric(1))
}

post_trans <- matrix(0, N_RANKS, N_RANKS,
                     dimnames = list(from = RANK_STATE_LABELS,
                                     to   = RANK_STATE_LABELS))
for (i in seq_len(N_RANKS)) {
  for (j in seq_len(N_RANKS)) {
    post_trans[i, j] <- mean(theta_draws[, sprintf("theta[%d,%d]", i, j)])
  }
}

post_rank_trans <- post_trans
post_tier_trans <- aggregate_rank_transition_to_tier(post_rank_trans)

cat("\n  Posterior-mean 30-rank transition matrix: first 10x10 block\n")
print(round(post_rank_trans[1:10, 1:10], 3))

cat("\n  Rank-model-implied five-tier transition matrix\n")
print(round(post_tier_trans, 3))

cat("\n  Legacy five-tier closed-form benchmark matrix\n")
print(round(tier_posterior_mean_closed, 3))

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

mc_diag <- markov_diagnostics(post_rank_trans)
names(mc_diag$stationary) <- RANK_STATE_LABELS
mc_diag_tier <- markov_diagnostics(post_tier_trans)
names(mc_diag_tier$stationary) <- TIERS

rank_transition_smoothness <- tibble(
  mean_abs_adjacent_row_diff = mean(abs(post_rank_trans[-1, ] - post_rank_trans[-N_RANKS, ])),
  mean_abs_adjacent_col_diff = mean(abs(post_rank_trans[, -1] - post_rank_trans[, -N_RANKS])),
  mean_diagonal_probability = mean(diag(post_rank_trans)),
  max_row_sum_error = max(abs(rowSums(post_rank_trans) - 1))
)

cat("\n  Rank transition smoothness diagnostics\n")
print(rank_transition_smoothness)

cat("\n  Stationary rank distribution: first 10 ranks\n")
print(round(mc_diag$stationary[1:10], 3))

cat(sprintf("  Rank-chain 2nd eigenvalue %.3f -> mixing time %.1f years\n",
            mc_diag$lambda2,
            mc_diag$mixing_time))




# ============================================================================
# 10 - PICK-VALUE SAMPLERS ---------------------------------------------------
# ============================================================================
# Both return a sampled 4-year-WS value for a given draft slot, carrying full
# uncertainty. The Monte Carlo loop calls sample_pick_value().

# Bayesian: draw curve params from the posterior, then draw a player-level
# first-4-year WS outcome from the appropriate round-specific predictive model.
sample_pick_value <- function(pos,
                                    draw_idx = sample(nrow(pick_draws), 1),
                                    draw_idx_r2 = sample(nrow(pick2_draws), 1)) {
  pos <- as.integer(pos)
  if (is.na(pos)) return(0)
  
  if (pos <= 30L) {
    pos <- max(1L, min(30L, pos))
    mu <- pick_mu_draws[draw_idx, pos]
    sg <- pick_sd_draws[draw_idx, pos]
    nu <- pick_draws$nu[draw_idx]
    return(mu + sg * rt(1, df = nu))
  }
  
  pos <- max(31L, min(60L, pos))
  rel <- pos - 30L
  # The R2 Stan model now generates one exact-pick posterior predictive outcome
  # per posterior draw. This preserves structural zeroes, right skew, rare upside,
  # and the practical negative floor from the fitted model.
  pick2_outcome_draws[draw_idx_r2, rel]
}


