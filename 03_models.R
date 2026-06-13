## ═════════════════════════════════════════════════════════════════════════════
## 07 - BUILD MARKOV TRANSITION COUNTS + DIRICHLET PRIOR -----------------------
## ═════════════════════════════════════════════════════════════════════════════

cat("\n--- Building 5-Tier Markov Transition Counts ---\n")

transitions <- all_standings %>%
  arrange(abbr, season) %>%
  group_by(abbr) %>%
  mutate(tier_next = lead(tier), season_next = lead(season)) %>%
  ungroup() %>%
  filter(!is.na(tier_next), season_next == season + 1)

counts_mat <- matrix(0L, N_TIERS, N_TIERS,
                     dimnames = list(TIERS, TIERS))
for (r in seq_len(nrow(transitions))) {
  i <- match(as.character(transitions$tier[r]), TIERS)
  j <- match(as.character(transitions$tier_next[r]), TIERS)
  counts_mat[i, j] <- counts_mat[i, j] + 1L
}

cat("  Observed transition counts:\n")
print(counts_mat)

# Dirichlet prior: adjacency-aware concentrations. Staying put or moving one
# tier is a priori likelier than big jumps. This regularizes sparse rows
# (e.g. relegation -> playoff is rarely observed but should not be exactly 0).
build_alpha <- function(K, stay = 3, adj = 1.5, far = 0.4, decay = 0.6) {
  a <- matrix(far, K, K)
  for (i in 1:K) for (j in 1:K) {
    d <- abs(i - j)
    a[i, j] <- if (d == 0) stay else if (d == 1) adj
    else max(far, adj * decay^(d - 1))
  }
  a
}
alpha_prior <- build_alpha(N_TIERS)

# Closed-form posterior mean (Dirichlet-Multinomial conjugacy) as a check.
posterior_mean_closed <- (counts_mat + alpha_prior) /
  rowSums(counts_mat + alpha_prior)
cat("\n  Closed-form posterior-mean transition matrix:\n")
print(round(posterior_mean_closed, 3))


## ═════════════════════════════════════════════════════════════════════════════
## 08 - FIT & VALIDATE STAN MODELS ---------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════


# Three pick-value model versions are fit on the same drafted-player rows so
# PSIS-LOO is pointwise-comparable across all models.
#   * 0601: oldest normal model with legacy war_obs / war_se data block
#   * 0605: player-level Student-t model with linear sigma[p]
#   * v2:   player-level Student-t model with adjacent-pick RW sigma[p]
# The v2 / random-walk model remains the production model used downstream.
PICK_MODEL_CONSTANT_SIGMA_PATH <- "zz_Archive/0605/pick_value_0601.stan"
PICK_MODEL_LINEAR_SIGMA_PATH   <- "zz_Archive/0605_1130/pick_value_0605.stan"
PICK_MODEL_RW_SIGMA_PATH       <- c("02_models/pick_value_v3.stan", "pick_value_v3.stan")
PICK_MODEL_R2_HURDLE_PATH     <- c("02_models/pick_play_r2_v6_declining_upside.stan")

resolve_required_file <- function(path) {
  candidates <- as.character(path)
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    stop(sprintf("Required file not found. Checked: %s", paste(candidates, collapse = ", ")), call. = FALSE)
  }
  hit
}

# Player-level pick-value data: one row per drafted player, outcome = first-four
# season Win Shares. All three Stan models are compared on these same rows.
pick_fit_data <- draft_4yr %>%
  transmute(draft_year = as.integer(draft_year),
            pick       = as.integer(pick),
            player     = as.character(player),
            ws4        = as.numeric(ws4)) %>%
    filter(!is.na(pick), pick >= 1, pick <= 30, !is.na(ws4))

pick_stan_data_player <- list(N    = nrow(pick_fit_data),
                              pick = pick_fit_data$pick,
                              ws4  = pick_fit_data$ws4)

# The oldest model has the legacy slot-mean data names. Passing ws4 as war_obs
# and war_se = 0 makes its generated log_lik one row per drafted player, so
# loo_compare() is valid against the two newer player-level models.
pick_stan_data_legacy <- list(N       = nrow(pick_fit_data),
                              pick    = pick_fit_data$pick,
                              war_obs = pick_fit_data$ws4,
                              war_se  = rep(0, nrow(pick_fit_data)))

sample_pick_stan_model <- function(model_path,
                                   data,
                                   seed,
                                   label,
                                   adapt_delta = 0.95,
                                   max_treedepth = 12) {
  model_file <- resolve_required_file(model_path)
  cat(sprintf("\n  [pick model] Compiling %s: %s\n", label, model_file))
  model <- cmdstan_model(model_file)
  cat(sprintf("  [pick model] Sampling %s\n", label))
  model$sample(data            = data,
               chains          = 4,
               parallel_chains = 4,
               iter_warmup     = 1000,
               iter_sampling   = 2000,
               adapt_delta     = adapt_delta,
               max_treedepth   = max_treedepth,
               seed            = seed,
               refresh         = 100)
}

pick_fit_constant_sigma <- sample_pick_stan_model(
  model_path = PICK_MODEL_CONSTANT_SIGMA_PATH,
  data       = pick_stan_data_legacy,
  seed       = 202601,
  label      = "constant_sigma / 0601"
)

pick_fit_linear_sigma <- sample_pick_stan_model(
  model_path = PICK_MODEL_LINEAR_SIGMA_PATH,
  data       = pick_stan_data_player,
  seed       = 202605,
  label      = "linear_sigma / 0605"
)

pick_fit_rw_sigma <- sample_pick_stan_model(
  model_path = PICK_MODEL_RW_SIGMA_PATH,
  data       = pick_stan_data_player,
  seed       = 202602,
  label      = "rw_sigma / v3"
)

# Keep the random-walk hierarchical model as the production fit used below.
pick_fit <- pick_fit_rw_sigma

# ---- ROUND 2: structural-zero hurdle model ---------------------------------
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

pick_fit_r2_hurdle <- sample_pick_stan_model(
  model_path = PICK_MODEL_R2_HURDLE_PATH,
  data       = pick_stan_data_r2,
  seed       = 202632,
  label      = "round2_right_skew_declining_upside_ws / exact_pick_rep_draws",
  adapt_delta = 0.99,
  max_treedepth = 12
)

pick_fit_r2_hurdle$cmdstan_diagnose()


# ---- VALIDATION 0: PSIS-LOO comparison across all three pick-value models ----
# These object names intentionally match the validation call you want to run.
loo_constant_sigma <- loo::loo(
  as.matrix(pick_fit_constant_sigma$draws("log_lik", format = "draws_matrix"))
)
loo_linear_sigma <- loo::loo(
  as.matrix(pick_fit_linear_sigma$draws("log_lik", format = "draws_matrix"))
)
loo_rw_sigma <- loo::loo(
  as.matrix(pick_fit_rw_sigma$draws("log_lik", format = "draws_matrix"))
)

cat("\n  [validate] PSIS-LOO comparison: constant vs linear vs RW sigma\n")
pick_loo_compare <- loo_compare(
  loo_constant_sigma,
  loo_linear_sigma,
  loo_rw_sigma
)
print(pick_loo_compare)

cat("\n  [validate] Pareto-k table: constant_sigma / 0601\n")
print(pareto_k_table(loo_constant_sigma))
cat("\n  [validate] Pareto-k table: linear_sigma / 0605\n")
print(pareto_k_table(loo_linear_sigma))
cat("\n  [validate] Pareto-k table: rw_sigma / v2\n")
print(pareto_k_table(loo_rw_sigma))

pick_loo_compare_tbl <- as.data.frame(pick_loo_compare) %>%
  rownames_to_column("model") %>%
  as_tibble()

pick_loo_summary <- tibble(
  model = c("constant_sigma_0601", "linear_sigma_0605", "rw_sigma_v2"),
  elpd_loo = c(
    loo_constant_sigma$estimates["elpd_loo", "Estimate"],
    loo_linear_sigma$estimates["elpd_loo", "Estimate"],
    loo_rw_sigma$estimates["elpd_loo", "Estimate"]
  ),
  p_loo = c(
    loo_constant_sigma$estimates["p_loo", "Estimate"],
    loo_linear_sigma$estimates["p_loo", "Estimate"],
    loo_rw_sigma$estimates["p_loo", "Estimate"]
  ),
  looic = c(
    loo_constant_sigma$estimates["looic", "Estimate"],
    loo_linear_sigma$estimates["looic", "Estimate"],
    loo_rw_sigma$estimates["looic", "Estimate"]
  ),
  max_pareto_k = c(
    max(loo::pareto_k_values(loo_constant_sigma), na.rm = TRUE),
    max(loo::pareto_k_values(loo_linear_sigma), na.rm = TRUE),
    max(loo::pareto_k_values(loo_rw_sigma), na.rm = TRUE)
  )
)

cat("\n  [validate] LOO summary by pick-value model\n")
print(pick_loo_summary)

# Core scalar variables for diagnostics. sigma_base/sigma_slope were removed
# in the adjacent-pick variance model. Residual scales now live in the
# generated vector war_pred_sd[1:30] / transformed vector sigma_pick[1:30].
pick_core_vars <- c("alpha", "beta", "gamma", "log_sigma_1", "tau_log_sigma_rw", "nu")

pick_draws <- pick_fit$draws(
  variables = c("alpha", "beta", "gamma", "tau_log_sigma_rw", "nu"),
  format = "df"
) %>% as_tibble()

# Extract Stan vector draws in numeric index order, regardless of CmdStan's
# column ordering. These matrices are draws x pick slot.
extract_stan_vector_draws <- function(fit, variable_base, K = 30L) {
  mat <- as.matrix(fit$draws(variables = variable_base, format = "draws_matrix"))
  idx <- str_match(colnames(mat), paste0("^", variable_base, "\\[(\\d+)\\]$"))[, 2]
  if (any(is.na(idx))) {
    stop("Could not parse Stan vector indices for ", variable_base, call. = FALSE)
  }
  ord <- order(as.integer(idx))
  mat <- mat[, ord, drop = FALSE]
  colnames(mat) <- paste0(variable_base, "[", seq_len(ncol(mat)), "]")
  if (ncol(mat) != K) {
    warning("Expected ", K, " columns for ", variable_base, ", found ", ncol(mat), ".")
  }
  mat
}

pick_mu_draws <- extract_stan_vector_draws(pick_fit, "war_pred", 30L)
pick_sd_draws <- extract_stan_vector_draws(pick_fit, "war_pred_sd", 30L)

pick2_p_play_draws <- extract_stan_vector_draws(pick_fit_r2_hurdle, "p_play", 30L)
pick2_cond_mu_draws <- extract_stan_vector_draws(pick_fit_r2_hurdle, "cond_mean_ws", 30L)
pick2_cond_scale_draws <- extract_stan_vector_draws(pick_fit_r2_hurdle, 
                                                    "cond_scale_ws", 30L)
pick2_cond_sd_draws <- extract_stan_vector_draws(pick_fit_r2_hurdle, "cond_sd_ws", 30L)
pick2_mu_draws <- extract_stan_vector_draws(pick_fit_r2_hurdle, "ev", 30L)
pick2_sd_draws <- extract_stan_vector_draws(pick_fit_r2_hurdle, "ev_sd", 30L)
pick2_outcome_draws <- extract_stan_vector_draws(pick_fit_r2_hurdle, "ws4_pick_rep", 30L)
pick2_upside_prob_draws <- extract_stan_vector_draws(pick_fit_r2_hurdle, "p_upside", 30L)

# One posterior predictive player-outcome draw per first-round pick slot. These
# are used only for display quantiles on the pick-value curve; the EAV curve
# itself still uses pick_mu_draws.
pick_outcome_draws <- matrix(NA_real_, nrow = nrow(pick_mu_draws), ncol = 30,
                             dimnames = list(NULL, paste0("pick_", 1:30)))
for (pk in 1:30) {
  pick_outcome_draws[, pk] <- pick_mu_draws[, pk] +
    pick_sd_draws[, pk] * stats::rt(nrow(pick_mu_draws), df = pick_draws$nu)
}

pick2_draws <- pick_fit_r2_hurdle$draws(
  variables = c(
    "logit_play_31", "delta_logit_play", "tau_logit_play_rw",
    "log_cond_mean_ws_31", "delta_log_cond_mean_ws", "tau_log_cond_mean_ws_rw",
    "log_sigma_ws_31", "tau_log_sigma_ws_rw",
    "logit_upside_31", "delta_logit_upside",
    "upside_prob_31", "upside_prob_45", "upside_prob_60",
    "upside_log_shift", "upside_sigma_mult"
  ),
  format = "df"
) %>% as_tibble()

# Round 2 production is now modeled as a smoothed, right-skewed hurdle in Stan:
#   * P(play) borrows from adjacent pick slots through a logit random walk.
#   * The typical played-player shifted outcome borrows through a log-scale RW.
#   * A pick-declining rare-upside component captures star outcomes without
#     letting every exact pick inherit the same far-right tail.
#   * A practical WS floor removes the unrealistic symmetric negative tail.
second_round_band <- function(pick) {
  case_when(
    pick <= 35 ~ "31-35",
    pick <= 40 ~ "36-40",
    pick <= 45 ~ "41-45",
    pick <= 50 ~ "46-50",
    pick <= 55 ~ "51-55",
    TRUE       ~ "56-60"
  )
}

R2_BANDS <- c("31-35", "36-40", "41-45", "46-50", "51-55", "56-60")

pick_fit_data_r2 <- pick_fit_data_r2 %>%
  mutate(pick_band = second_round_band(pick))

# Empirical played-player pools are retained only for diagnostics / bootstrap
# fallback. They are no longer used to construct the Bayesian round-2 EV curve.
r2_played_ws_by_band <- split(
  pick_fit_data_r2$ws4[pick_fit_data_r2$played == 1],
  pick_fit_data_r2$pick_band[pick_fit_data_r2$played == 1]
)
r2_all_played_ws <- pick_fit_data_r2 %>%
  filter(played == 1) %>%
  pull(ws4)
for (b in R2_BANDS) {
  if (is.null(r2_played_ws_by_band[[b]]) || length(r2_played_ws_by_band[[b]]) == 0L) {
    r2_played_ws_by_band[[b]] <- r2_all_played_ws
  }
}

n_pick2_draws <- nrow(pick2_p_play_draws)

# Conditional second moment for the played-player component. cond_sd_ws is the
# full shifted-lognormal mixture predictive SD, not the typical lognormal scale.
pick2_cond_m2_draws <- pick2_cond_sd_draws^2 + pick2_cond_mu_draws^2

cat("\n  Pick-value posterior summary: rw_sigma / v2 production model\n")
print(pick_fit$summary(pick_core_vars))

# ---- VALIDATION 1: convergence diagnostics ----
pick_diag <- pick_fit$summary(pick_core_vars) %>%
  select(variable, rhat, ess_bulk, ess_tail)
cat("\n  [validate] Pick model R-hat / ESS: rw_sigma / v2 production model\n")
print(pick_diag)
if (any(pick_diag$rhat > 1.01, na.rm = TRUE)) {
  warning("Pick model: some R-hat > 1.01 — inspect convergence.")
}

pick2_core_vars <- c(
  "logit_play_31", "delta_logit_play", "tau_logit_play_rw",
  "log_cond_mean_ws_31", "delta_log_cond_mean_ws", "tau_log_cond_mean_ws_rw",
  "log_sigma_ws_31", "tau_log_sigma_ws_rw",
  "logit_upside_31", "delta_logit_upside",
  "upside_prob_31", "upside_prob_45", "upside_prob_60",
  "upside_log_shift", "upside_sigma_mult"
)
print(pick_fit_r2_hurdle$summary(pick2_core_vars))

pick2_diag <- pick_fit_r2_hurdle$summary(pick2_core_vars) %>%
  select(variable, rhat, ess_bulk, ess_tail)
cat("\n  [validate] Round-2 smoothed hurdle model R-hat / ESS\n")
print(pick2_diag)
if (any(pick2_diag$rhat > 1.01, na.rm = TRUE)) {
  warning("Round-2 smoothed hurdle model: some R-hat > 1.01 — inspect convergence.")
}

played_rep_mat_r2 <- as.matrix(pick_fit_r2_hurdle$draws(variables = "played_rep", format = "draws_matrix"))
ws4_rep_mat_r2 <- as.matrix(pick_fit_r2_hurdle$draws(variables = "ws4_rep", format = "draws_matrix"))

ppc_tbl_r2 <- tibble(
  row_id = seq_len(nrow(pick_fit_data_r2)),
  draft_year = pick_fit_data_r2$draft_year,
  pick = pick_fit_data_r2$pick,
  player = pick_fit_data_r2$player,
  played = pick_fit_data_r2$played,
  obs = pick_fit_data_r2$ws4,
  lo = apply(ws4_rep_mat_r2, 2, quantile, probs = 0.05, na.rm = TRUE),
  hi = apply(ws4_rep_mat_r2, 2, quantile, probs = 0.95, na.rm = TRUE),
  played_rep_prob = colMeans(played_rep_mat_r2)
) %>%
  mutate(covered = obs >= lo & obs <= hi)

cat(sprintf("  [validate] Round-2 smoothed-hurdle PPC 90%% coverage: %.0f%% of player rows\n",
            100 * mean(ppc_tbl_r2$covered)))
cat(sprintf("  [validate] Round-2 empirical played rate %.1f%% | posterior predictive %.1f%%\n",
            100 * mean(ppc_tbl_r2$played == 1),
            100 * mean(ppc_tbl_r2$played_rep_prob)))





nuts_r2 <- posterior::as_draws_df(
  pick_fit_r2_hurdle$sampler_diagnostics()
)

nuts_r2 %>%
  group_by(.chain) %>%
  summarise(
    divergences = sum(divergent__),
    max_treedepth_hits = sum(treedepth__ >= 12),
    ebfmi = mean(diff(energy__)^2) / var(energy__),
    .groups = "drop"
  )



r2_band_vec <- pick_fit_data_r2$pick_band

ws_dist_band_check <- purrr::map_dfr(unique(r2_band_vec), function(b) {
  idx <- which(r2_band_vec == b)
  
  obs <- pick_fit_data_r2$ws4[idx]
  rep <- as.vector(ws4_rep_mat_r2[, idx, drop = FALSE])
  
  tibble(
    pick_band = b,
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
    rep_ge_10 = mean(rep >= 10)
  )
}) %>%
  arrange(pick_band)

print(ws_dist_band_check)


loyo_ws_check <- purrr::map_dfr(sort(unique(pick_fit_data_r2$draft_year)), function(yr) {
  train <- pick_fit_data_r2 %>% filter(draft_year != yr, played == 1)
  test  <- pick_fit_data_r2 %>% filter(draft_year == yr)
  
  pools <- split(train$ws4, train$pick_band)
  
  test %>%
    rowwise() %>%
    mutate(
      pool_mean = mean(pools[[pick_band]], na.rm = TRUE),
      pool_q05  = quantile(pools[[pick_band]], 0.05, na.rm = TRUE),
      pool_q95  = quantile(pools[[pick_band]], 0.95, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    summarise(
      draft_year = yr,
      n = n(),
      obs_mean = mean(ws4),
      pred_mean_naive = mean(if_else(played == 1L, pool_mean, 0)),
      obs_ge_5 = mean(ws4 >= 5),
      pred_ge_5_naive = mean(if_else(played == 1L, ws4 >= 5, FALSE)),
      .groups = "drop"
    )
})

print(loyo_ws_check)

loyo_ws_check %>%
  summarise(
    mae_mean = mean(abs(obs_mean - pred_mean_naive), na.rm = TRUE),
    bias_mean = mean(pred_mean_naive - obs_mean, na.rm = TRUE)
  )





second_round_band10 <- function(pick) {
  case_when(
    pick <= 40 ~ "31-40",
    pick <= 50 ~ "41-50",
    TRUE       ~ "51-60"
  )
}

band10_check <- pick_fit_data_r2 %>%
  mutate(band10 = second_round_band10(pick)) %>%
  group_by(band10) %>%
  summarise(
    n = n(),
    mean_ws4 = mean(ws4),
    p_play = mean(played),
    p_ge_5 = mean(ws4 >= 5),
    p_ge_10 = mean(ws4 >= 10),
    q05 = quantile(ws4, 0.05),
    q95 = quantile(ws4, 0.95),
    .groups = "drop"
  )

print(band10_check)



pick_fit_data_r2 %>%
  summarise(
    n = n(),
    mean_ws4 = mean(ws4),
    p_play = mean(played),
    p_ge_5 = mean(ws4 >= 5),
    p_ge_10 = mean(ws4 >= 10),
    q05 = quantile(ws4, 0.05),
    q95 = quantile(ws4, 0.95)
  )


pick_fit_data_r2 %>%
  mutate(
    era = case_when(
      draft_year <= 1994 ~ "1985-1994",
      draft_year <= 2004 ~ "1995-2004",
      draft_year <= 2014 ~ "2005-2014",
      TRUE               ~ "2015-2021"
    )
  ) %>%
  group_by(era, pick_band) %>%
  summarise(
    n = n(),
    p_play = mean(played),
    mean_ws4 = mean(ws4),
    p_ge_5 = mean(ws4 >= 5),
    p_ge_10 = mean(ws4 >= 10),
    .groups = "drop"
  ) %>% as.data.frame()


pick_fit_data_r2 %>%
  group_by(pick_band) %>%
  arrange(desc(ws4), .by_group = TRUE) %>%
  mutate(rank_desc = row_number()) %>%
  summarise(
    n = n(),
    mean_all = mean(ws4),
    mean_ex_top1 = mean(ws4[rank_desc > 1]),
    mean_ex_top3 = mean(ws4[rank_desc > 3]),
    mean_winsor_95 = mean(pmin(ws4, quantile(ws4, 0.95))),
    top_player = player[which.max(ws4)],
    top_ws4 = max(ws4),
    .groups = "drop"
  )%>% as.data.frame


r2_ev_identity_check <- tibble(
  pick = 31:60,
  pick_band = second_round_band(pick),
  p_play = colMeans(pick2_p_play_draws),
  cond_played_mean_ws = colMeans(pick2_cond_mu_draws),
  manual_ev = p_play * cond_played_mean_ws,
  model_ev = colMeans(pick2_mu_draws),
  diff = model_ev - manual_ev
)

print(r2_ev_identity_check, n = 30)

ppc_tbl_r2 %>%
  group_by(played) %>%
  summarise(
    n = n(),
    coverage_90 = mean(covered),
    miss_low = mean(obs < lo),
    miss_high = mean(obs > hi),
    mean_obs = mean(obs),
    mean_lo = mean(lo),
    mean_hi = mean(hi),
    .groups = "drop"
  )


ppc_tbl_r2 %>%
  filter(played == 1) %>%
  mutate(
    obs_bucket = case_when(
      obs <= 0  ~ "<=0 WS",
      obs <= 2  ~ "0-2 WS",
      obs <= 5  ~ "2-5 WS",
      obs <= 10 ~ "5-10 WS",
      TRUE      ~ "10+ WS"
    )
  ) %>%
  group_by(obs_bucket) %>%
  summarise(
    n = n(),
    coverage_90 = mean(covered),
    miss_low = mean(obs < lo),
    miss_high = mean(obs > hi),
    mean_obs = mean(obs),
    mean_hi = mean(hi),
    .groups = "drop"
  )

# Round-2 hurdle calibration: played-rate checks by exact slot and by broad
# second-round bands. These are useful for diagnosing whether the play-probability
# random-walk prior is too loose (overfits slot-level dips/spikes) or too tight
# (misses broad early/late second-round gradients).
played_rep_slot_rate_mat_r2 <- sapply(31:60, function(pk) {
  idx <- which(pick_fit_data_r2$pick == pk)
  if (length(idx) == 0L) return(rep(NA_real_, nrow(played_rep_mat_r2)))
  rowMeans(played_rep_mat_r2[, idx, drop = FALSE])
})
colnames(played_rep_slot_rate_mat_r2) <- as.character(31:60)

ppc_play_pick_r2 <- tibble(pick = 31:60,
                           n = as.integer(table(factor(pick_fit_data_r2$pick, levels = 31:60))),
                           emp_p_play = as.numeric(tapply(pick_fit_data_r2$played, 
                                                          factor(pick_fit_data_r2$pick, 
                                                                 levels = 31:60), mean)),
                           model_p_play_mean = colMeans(pick2_p_play_draws),
                           model_p_play_q05 = apply(pick2_p_play_draws, 2, 
                                                    quantile, 0.05, na.rm = TRUE),
                           model_p_play_q50 = apply(pick2_p_play_draws, 2, 
                                                    quantile, 0.50, na.rm = TRUE),
                           model_p_play_q95 = apply(pick2_p_play_draws, 2, 
                                                    quantile, 0.95, na.rm = TRUE),
                           pred_rep_p_play_q05 = apply(played_rep_slot_rate_mat_r2, 2, 
                                                       quantile, 0.05, na.rm = TRUE),
                           pred_rep_p_play_q50 = apply(played_rep_slot_rate_mat_r2, 2, 
                                                       quantile, 0.50, na.rm = TRUE),
                           pred_rep_p_play_q95 = apply(played_rep_slot_rate_mat_r2, 2, 
                                                       quantile, 0.95, na.rm = TRUE)) %>%
  mutate(emp_within_90_rep_interval = emp_p_play >= pred_rep_p_play_q05 & emp_p_play <= pred_rep_p_play_q95)

cat("\n  [validate] Round-2 P(play) calibration by pick slot\n")
print(ppc_play_pick_r2, n = 30)
cat(sprintf("  [validate] Round-2 empirical slot P(play) inside 90%% posterior predictive intervals: %.0f%%\n",
            100 * mean(ppc_play_pick_r2$emp_within_90_rep_interval, na.rm = TRUE)))

second_round_band <- function(pick) {
  case_when(
    pick <= 35 ~ "31-35",
    pick <= 40 ~ "36-40",
    pick <= 45 ~ "41-45",
    pick <= 50 ~ "46-50",
    pick <= 55 ~ "51-55",
    TRUE       ~ "56-60"
  )
}

pplay_band_draws_r2 <- sapply(c("31-35", "36-40", "41-45", "46-50", "51-55", "56-60"), function(b) {
  pks <- 31:60
  keep <- second_round_band(pks) == b
  rowMeans(pick2_p_play_draws[, keep, drop = FALSE])
})

ppc_play_band_r2 <- pick_fit_data_r2 %>%
  mutate(pick_band = second_round_band(pick)) %>%
  group_by(pick_band) %>%
  summarise(n = n(), emp_p_play = mean(played), .groups = "drop") %>%
  mutate(
    model_p_play_mean = colMeans(pplay_band_draws_r2)[pick_band],
    model_p_play_q05 = apply(pplay_band_draws_r2, 2, quantile, 0.05)[pick_band],
    model_p_play_q50 = apply(pplay_band_draws_r2, 2, quantile, 0.50)[pick_band],
    model_p_play_q95 = apply(pplay_band_draws_r2, 2, quantile, 0.95)[pick_band]
  )

cat("\n  [validate] Round-2 P(play) calibration by pick band\n")
print(ppc_play_band_r2)

loo_pick_r2 <- loo::loo(
  as.matrix(pick_fit_r2_hurdle$draws("log_lik", format = "draws_matrix"))
)
cat("\n  [validate] Round-2 smoothed hurdle model LOO\n")
print(loo_pick_r2)
print(pareto_k_table(loo_pick_r2))

# ---- VALIDATION 2: posterior-predictive player-level coverage ----
# Share of drafted-player outcomes whose realized first-4-year WS falls in the
# 90% posterior predictive interval. PPCs are shown for the production v2 model.
ws4_rep_mat <- as.matrix(pick_fit$draws(variables = "ws4_rep", format = "draws_matrix"))

ppc_tbl <- tibble(
  row_id = seq_len(nrow(pick_fit_data)),
  draft_year = pick_fit_data$draft_year,
  pick = pick_fit_data$pick,
  player = pick_fit_data$player,
  obs = pick_fit_data$ws4,
  lo = apply(ws4_rep_mat, 2, quantile, probs = 0.05, na.rm = TRUE),
  hi = apply(ws4_rep_mat, 2, quantile, probs = 0.95, na.rm = TRUE)
) %>%
  mutate(covered = obs >= lo & obs <= hi)

cat(sprintf("  [validate] PPC 90%% coverage: %.0f%% of player rows\n",
            100 * mean(ppc_tbl$covered)))

ppc_band_tbl <- ppc_tbl %>%
  mutate(
    pick_band = case_when(
      pick <= 5  ~ "1-5",
      pick <= 10 ~ "6-10",
      pick <= 15 ~ "11-15",
      pick <= 20 ~ "16-20",
      pick <= 30 ~ "21-30"
    )
  ) %>%
  group_by(pick_band) %>%
  summarise(
    n = n(),
    coverage_90 = mean(covered),
    mean_obs = mean(obs),
    mean_pred_mid = mean((lo + hi) / 2),
    .groups = "drop"
  )

print(ppc_band_tbl)

ws4_rep_mean <- colMeans(ws4_rep_mat)

ppc_pick_resid <- ppc_tbl %>%
  mutate(pred_mean = ws4_rep_mean) %>%
  group_by(pick) %>%
  summarise(
    n = n(),
    obs_mean = mean(obs),
    pred_mean = mean(pred_mean),
    resid = obs_mean - pred_mean,
    coverage_90 = mean(covered),
    .groups = "drop"
  )

print(ppc_pick_resid, n = 30)

sigma_draws <- as.matrix(pick_fit$draws("war_pred_sd", format = "draws_matrix"))

sigma_curve <- tibble(
  pick = 1:30,
  sigma_mean = colMeans(sigma_draws),
  sigma_q05 = apply(sigma_draws, 2, quantile, 0.05),
  sigma_q50 = apply(sigma_draws, 2, quantile, 0.50),
  sigma_q95 = apply(sigma_draws, 2, quantile, 0.95)
)

print(sigma_curve, n = 30)

# Keep the original single-model LOO alias for backward compatibility, but the
# main model-selection object is pick_loo_compare / loo_compare(...) above.
loo_pick <- loo_rw_sigma
cat("\n  [validate] Production pick model LOO: rw_sigma / v2\n")
print(loo_pick)
print(pareto_k_table(loo_pick))


cat("\n--- Fitting Markov Transition Stan Model ---\n")

markov_model <- cmdstan_model("02_models/team_strength.stan")

markov_fit <- markov_model$sample(data            = list(K = N_TIERS, 
                                                         counts = counts_mat, 
                                                         alpha = alpha_prior),
                                  chains          = 4,
                                  parallel_chains = 4,
                                  iter_warmup     = 1000,
                                  iter_sampling   = 2000,
                                  seed            = 2026,
                                  refresh         = 0)

# Posterior draws of each transition row as an array [draws, K, K].
theta_draws <- markov_fit$draws("theta", format = "draws_matrix")
n_markov_draws <- nrow(theta_draws)

get_theta_row <- function(draw_idx, i) {
  vapply(1:N_TIERS,
         function(j) theta_draws[draw_idx, sprintf("theta[%d,%d]", i, j)],
         numeric(1))
}

# Posterior-mean transition matrix from Stan (should match closed form).
post_trans <- matrix(0, N_TIERS, N_TIERS, dimnames = list(TIERS, TIERS))
for (i in 1:N_TIERS) for (j in 1:N_TIERS) {
  post_trans[i, j] <- mean(theta_draws[, sprintf("theta[%d,%d]", i, j)])
}
cat("\n  Stan posterior-mean transition matrix:\n")
print(round(post_trans, 3))
cat(sprintf("  [validate] max |Stan - closed-form| = %.4f\n",
            max(abs(post_trans - posterior_mean_closed))))

# ---- VALIDATION 3: stationary distribution + mixing time ----
markov_diagnostics <- function(P) {
  ev <- eigen(t(P))
  idx <- which.min(abs(ev$values - 1))
  pi_stat <- Re(ev$vectors[, idx]); pi_stat <- pi_stat / sum(pi_stat)
  lam <- sort(abs(Re(ev$values)), decreasing = TRUE)
  list(stationary = pi_stat, lambda2 = lam[2],
       mixing_time = -1 / log(lam[2]))
}
mc_diag <- markov_diagnostics(post_trans)
names(mc_diag$stationary) <- TIERS
cat("\n  [validate] Stationary tier distribution:\n")
print(round(mc_diag$stationary, 3))
cat(sprintf("  [validate] 2nd eigenvalue %.3f -> mixing time %.1f yrs\n",
            mc_diag$lambda2, mc_diag$mixing_time))


# ============================================================================
# SECTION 9: PICK-VALUE SAMPLERS (Bayesian curve OR bootstrap)
# ============================================================================
# Both return a sampled 4-year-WS value for a given draft slot, carrying full
# uncertainty. The Monte Carlo loop calls sample_pick_value().

# Bayesian: draw curve params from the posterior, then draw a player-level
# first-4-year WS outcome from the appropriate round-specific predictive model.
sample_pick_value_bayes <- function(pos,
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

# Bootstrap fallback: round 1 resamples actual first-round player outcomes;
# round 2 uses the empirical structural-zero hurdle pool.
boot_index <- split(pick_boot_pool$ws4, pick_boot_pool$pick)
boot_index_r2 <- split(draft_4yr_r2$ws4, draft_4yr_r2$pick)

sample_pick_value_boot <- function(pos) {
  pos <- as.integer(pos)
  if (is.na(pos)) return(0)
  if (pos <= 30L) {
    nb <- as.character(c(pos - 1, pos, pos + 1))
    pool <- unlist(boot_index[nb[nb %in% names(boot_index)]], use.names = FALSE)
    if (length(pool) == 0) pool <- unlist(boot_index, use.names = FALSE)
    return(sample(pool, 1))
  }
  
  nb <- as.character(c(pos - 1, pos, pos + 1))
  pool <- unlist(boot_index_r2[nb[nb %in% names(boot_index_r2)]], use.names = FALSE)
  if (length(pool) == 0) pool <- unlist(boot_index_r2, use.names = FALSE)
  sample(pool, 1)
}

sample_pick_value <- if (USE_BAYESIAN_PICK_CURVE) {
  sample_pick_value_bayes
} else {
  function(pos, draw_idx = NULL, draw_idx_r2 = NULL) sample_pick_value_boot(pos)
}
