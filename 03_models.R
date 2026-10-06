## ═════════════════════════════════════════════════════════════════════════════
## 07 - BUILD RANK-STATE MARKOV TRANSITION DATA -------------------------------
## ═════════════════════════════════════════════════════════════════════════════


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

# League-rank bands for the five 3-2-1 tiers (rank_worst 1 = worst record)
rank_tier_lookup <- tibble(rank_worst = RANK_STATES) %>%
  mutate(tier = case_when(rank_worst <= 3L  ~ "relegation",
                          rank_worst <= 10L ~ "nonplayin",
                          rank_worst <= 14L ~ "playin_seed",
                          rank_worst <= 16L ~ "playin_loser",
                          TRUE              ~ "playoff"),
         tier = factor(tier, levels = TIERS))

rank_transitions <- all_standings %>%
  arrange(abbr, season) %>%
  group_by(abbr) %>%
  mutate(rank_next = lead(rank_worst), season_next = lead(season)) %>%
  ungroup() %>%
  filter(!is.na(rank_next), season_next == season + 1) %>%
  transmute(abbr, season,
            rank_worst = as.integer(rank_worst),
            rank_next = as.integer(rank_next))

# Count matrix: rows = rank this season, columns = rank next season
rank_counts_tbl <- rank_transitions %>%
  count(rank_worst, rank_next)

rank_counts_mat <- matrix(0L, N_RANKS, N_RANKS,
                          dimnames = list(from = RANK_STATE_LABELS,
                                          to   = RANK_STATE_LABELS))
rank_counts_mat[cbind(rank_counts_tbl$rank_worst, 
                      rank_counts_tbl$rank_next)] <- rank_counts_tbl$n

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

tier_counts_tbl <- tier_transitions %>%
  count(tier, tier_next)

tier_counts_mat <- matrix(0L, N_TIERS, N_TIERS,
                          dimnames = list(from = TIERS, to = TIERS))
tier_counts_mat[cbind(match(as.character(tier_counts_tbl$tier), TIERS),
                      match(as.character(tier_counts_tbl$tier_next), TIERS))] <- tier_counts_tbl$n

cat("\n  Five-tier benchmark transition counts:\n")
print(tier_counts_mat)

# Dirichlet prior for the legacy 5-tier benchmark only. The production rank
# model below uses a smoothed softmax surface rather than a conjugate Dirichlet.
# Prior pseudo-counts by distance between tiers:
#   - same tier: 3
#   - one tier apart: 1.5
#   - further apart: 1.5 x 0.6^(distance - 1), with a floor of 0.4
tier_distance <- abs(outer(seq_len(N_TIERS), seq_len(N_TIERS), "-"))
tier_alpha_prior <- ifelse(tier_distance == 0, 3,
                           ifelse(tier_distance == 1, 1.5,
                                  pmax(0.4, 1.5 * 0.6^(tier_distance - 1))))
tier_posterior_mean_closed <- (tier_counts_mat + tier_alpha_prior) /
  rowSums(tier_counts_mat + tier_alpha_prior)

# Backward-compatible aliases for scripts that inspect transition counts. From
# v2 onward, counts_mat is the production 30-rank count matrix.
counts_mat <- rank_counts_mat
posterior_mean_closed <- tier_posterior_mean_closed

# ---- Kernel-smoothed empirical rank transition baseline ----------------------
# Stable no-HMC benchmark. It shares information across neighboring starting
# ranks and neighboring ending ranks, then adds a small distance-decay prior so
# sparse / empty empirical cells never become structural zeroes. This is the
# transparent baseline the Stan model must beat before it replaces production.
#
# Pseudo-counts per cell (i = current rank, j = next rank):
#   observed counts
#   + 0.75 x kernel-smoothed counts (source bandwidth 1.50, destination 1.75)
#   + 4.0  x distance prior, exp(-|i - j| / 7), rows normalized
#   + 0.05 baseline in every cell
# then each row is normalized to sum to 1.

# Smoothed empirical counts: source ranks near i and destination ranks near j
# contribute to cell (i, j), but the original counts still retain local identity.
rank_smoothed_counts <- rank_kernel_weights(N_RANKS, bandwidth = 1.50) %*%
  rank_counts_mat %*%
  t(rank_kernel_weights(N_RANKS, bandwidth = 1.75))

# Light structural prior: nearby future ranks should be a priori more likely
# than extreme jumps, without preventing big jumps when observed.
rank_distance_prior <- normalize_rows(exp(-abs(outer(RANK_STATES, RANK_STATES, "-")) / 7.0))

rank_kernel_transition_matrix <- normalize_rows(rank_counts_mat +
                                                  0.75 * rank_smoothed_counts +
                                                  4.0 * rank_distance_prior +
                                                  0.05)
dimnames(rank_kernel_transition_matrix) <- dimnames(rank_counts_mat)

rank_kernel_tier_transition_matrix <- aggregate_rank_transition_to_tier(rank_kernel_transition_matrix)

cat("\n  Kernel-smoothed empirical 30-rank baseline: first 10x10 block\n")
print(round(rank_kernel_transition_matrix[1:10, 1:10], 3))
print(round(rank_kernel_transition_matrix, 4))

cat("\n  Kernel-smoothed empirical five-tier implied baseline\n")
print(round(rank_kernel_tier_transition_matrix, 3))








## ═════════════════════════════════════════════════════════════════════════════
## 08 - FIT STAN MODELS --------------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════


# Player-level pick-value data
pick_fit_data <- draft_4yr %>%
  transmute(draft_year = as.integer(draft_year),
            pick       = as.integer(pick),
            player     = as.character(player),
            war4       = as.numeric(war4)) %>%
    filter(!is.na(pick), pick >= 1, pick <= 30, !is.na(war4))

pick_stan_data_player <- list(N    = nrow(pick_fit_data),
                              pick = pick_fit_data$pick,
                              war4 = pick_fit_data$war4)


# Legacy comparison models (constant and linear sigma). Their priors are on the
# old Win Shares scale, so they are off by default; turn them on only after
# rescaling those priors. The archived files use the old data names.
if (FIT_R1_COMPARISON_MODELS) {
  pick_stan_data_legacy <- list(N       = nrow(pick_fit_data),
                                pick    = pick_fit_data$pick,
                                war_obs = pick_fit_data$war4,
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
  fit_r1_v2 <- model_r1_v2$sample(data            = list(N    = nrow(pick_fit_data),
                                                         pick = pick_fit_data$pick,
                                                         ws4  = pick_fit_data$war4),
                                  chains          = 4,
                                  parallel_chains = 4,
                                  iter_warmup     = 1000,
                                  iter_sampling   = 2000,
                                  adapt_delta     = 0.95,
                                  max_treedepth   = 12,
                                  seed            = 2026,
                                  refresh         = 100)
  fit_r1_v2$cmdstan_diagnose()

  # Shifted-lognormal mixture with the mean-curve constraint (slow: one chain
  # can take ~13 minutes). Best LOO fit, but its tail extrapolation overstated
  # picks 1-3 by ~4.8 wins in draft-class cross-validation.
  model_r1_mix <- cmdstan_model("02_models/zz_Archive/picks_Round1_lognormal_mixture_xrapm.stan")
  fit_r1_mix <- model_r1_mix$sample(data = c(pick_stan_data_player,
                                             list(war_floor = min(-2, 
                                                                  min(pick_fit_data$war4) - 0.25))),
                                    chains          = 4,
                                    parallel_chains = 4,
                                    iter_warmup     = 1000,
                                    iter_sampling   = 2000,
                                    adapt_delta     = 0.95,
                                    max_treedepth   = 12,
                                    seed            = 2026,
                                    refresh         = 100)
  fit_r1_mix$cmdstan_diagnose()
}


# Student-t comparison on the xRAPM scale. Its location is the t center, which
# sits below the mean of the right-skewed outcome (-1.3 wins of held-out bias),
# so it is not used for EPV; it stays as a LOO benchmark.
model_r1_t <- cmdstan_model("02_models/zz_Archive/picks_Round1_student_t_xrapm.stan")
fit_r1_t <- model_r1_t$sample(data            = list(N    = nrow(pick_fit_data),
                                                     pick = pick_fit_data$pick,
                                                     war4 = pick_fit_data$war4),
                              chains          = 4,
                              parallel_chains = 4,
                              iter_warmup     = 1000,
                              iter_sampling   = 2000,
                              adapt_delta     = 0.95,
                              max_treedepth   = 12,
                              seed            = 2026,
                              refresh         = 100)
fit_r1_t$cmdstan_diagnose()


# Previous production model: heteroskedastic Gaussian mean curve. Kept as a LOO
# benchmark; it ties the ex-Gaussian on held-out mean error but its symmetric
# likelihood misdescribes the skewed player outcomes.
model_r1_gauss <- cmdstan_model("02_models/zz_Archive/picks_Round1_gaussian_xrapm.stan")
fit_r1_gauss <- model_r1_gauss$sample(data            = pick_stan_data_player,
                                      chains          = 4,
                                      parallel_chains = 4,
                                      iter_warmup     = 1000,
                                      iter_sampling   = 2000,
                                      adapt_delta     = 0.95,
                                      max_treedepth   = 12,
                                      seed            = 2026,
                                      refresh         = 100)
fit_r1_gauss$cmdstan_diagnose()


# Ex-Gaussian with a strictly decreasing, pooled-gap mean curve and
# second-order random-walk spread / skew curves (Production Version). Ties the
# Gaussian on held-out mean error and beats it on held-out log density in
# leave-one-draft-class-out cross-validation
# (03_validation/r1_likelihood_cv_xrapm.csv). Player outcomes are drawn from
# this likelihood.
#
# Starting points near the prior means: second-order walks compound every slope
# change across later picks, so generic random inits can start with absurd
# spread curves and strand a chain at the treedepth limit.
# One list of starting values per chain (4 chains), each with small jitter.
r1_inits <- replicate(4,
                      list(log_alpha = log(14) + stats::rnorm(1, 0, 0.1),
                           log_beta  = log(0.6) + stats::rnorm(1, 0, 0.1),
                           gamma     = 1.5 + stats::rnorm(1, 0, 0.3),
                           z_eps     = stats::rnorm(29, 0, 0.1),
                           tau_eps   = 0.15,
                           log_sd_1  = log(12) + stats::rnorm(1, 0, 0.1),
                           d_sd      = -0.03,
                           z_sd      = stats::rnorm(28, 0, 0.1),
                           tau_sd    = 0.01,
                           logit_r_1 = 1 + stats::rnorm(1, 0, 0.2),
                           d_r       = 0.05,
                           z_r       = stats::rnorm(28, 0, 0.1),
                           tau_r     = 0.01),
                      simplify = FALSE)

model_r1_v3 <- cmdstan_model("02_models/picks_Round1.stan")
fit_r1_v3 <- model_r1_v3$sample(data            = pick_stan_data_player,
                                chains          = 4,
                                parallel_chains = 4,
                                iter_warmup     = 1000,
                                iter_sampling   = 2000,
                                adapt_delta     = 0.99,  # 0.95 left divergences
                                max_treedepth   = 12,
                                seed            = 2026,
                                init            = r1_inits,
                                refresh         = 100)
fit_r1_v3$cmdstan_diagnose()



# Second round picks: structural-zero hurdle with a strictly decreasing,
# pooled-gap expected-value curve and fringe/contributor played outcomes
# (03_validation/r2_likelihood_cv_xrapm.csv)
pick_fit_data_r2 <- draft_4yr_r2 %>%
  transmute(draft_year = as.integer(draft_year),
            pick       = as.integer(pick),
            player     = as.character(player),
            played     = as.integer(played),
            war4       = as.numeric(war4)) %>%
  filter(!is.na(pick), pick >= 31, pick <= 60, !is.na(played), !is.na(war4))

pick_stan_data_r2 <- list(N      = nrow(pick_fit_data_r2),
                          pick   = pick_fit_data_r2$pick,
                          played = pick_fit_data_r2$played,
                          war4   = pick_fit_data_r2$war4)

# Starting points near the prior means, with small jitter. Generic random inits
# (even init = 0.5) can start a chain with EV gaps of ~1 win per pick, which puts
# the derived fringe level far from every played outcome; that chain's step size
# collapses and it never moves (seen in draft-class CV).
# One list of starting values per chain (4 chains), each with small jitter.
r2_mixture_inits <- replicate(4,
                              list(eta_31     = qlogis(0.90) + stats::rnorm(1, 0, 0.2),
                                   d_pi       = -0.05,
                                   tau_pi     = 0.05,
                                   z_pi       = stats::rnorm(29, 0, 0.1),
                                   ev_31      = 1.5 + stats::rnorm(1, 0, 0.2),
                                   log_gap    = log(0.05) + stats::rnorm(1, 0, 0.1),
                                   tau_eps    = 0.2,
                                   z_eps      = stats::rnorm(29, 0, 0.1),
                                   logit_q_31 = qlogis(0.35) + stats::rnorm(1, 0, 0.2),
                                   d_q        = -0.04,
                                   z_q        = stats::rnorm(29, 0, 0.1),
                                   tau_q      = 0.05,
                                   log_T_31   = log(6) + stats::rnorm(1, 0, 0.1),
                                   d_T        = -0.01,
                                   z_T        = stats::rnorm(29, 0, 0.1),
                                   tau_T      = 0.05,
                                   log_s0_31  = log(2) + stats::rnorm(1, 0, 0.1),
                                   d_s0       = -0.03,
                                   z_s0       = stats::rnorm(29, 0, 0.1),
                                   tau_s0     = 0.05),
                              simplify = FALSE)

model_r2 <- cmdstan_model("02_models/picks_Round2.stan")
fit_r2 <- model_r2$sample(data            = pick_stan_data_r2,
                          chains          = 4,
                          parallel_chains = 4,
                          iter_warmup     = 1000,
                          iter_sampling   = 2000,
                          adapt_delta     = 0.999, # sharp late-pick geometry from the
                          max_treedepth   = 12,    # derived fringe level
                          seed            = 2026,
                          init            = r2_mixture_inits,
                          refresh         = 100)
fit_r2$cmdstan_diagnose()



# Markov Chain model for team strength
rank_markov_stan_data <- list(K = N_RANKS,
                              counts = rank_counts_mat,
                              eta_scale = 0.30,
                              row_smooth_scale = 0.12,
                              col_smooth_scale = 0.12,
                              dest_scale = 0.35)

markov_model <- cmdstan_model("02_models/team_strength_v3.stan")

markov_fit <- markov_model$sample(data = rank_markov_stan_data,
                                  chains = 4,
                                  parallel_chains = 4,
                                  iter_warmup = 1000,
                                  iter_sampling = 2000,
                                  adapt_delta = 0.95,
                                  max_treedepth = 12,
                                  seed = 2026,
                                  refresh = 100)
markov_fit$cmdstan_diagnose()





## ═════════════════════════════════════════════════════════════════════════════
## 09 - VALIDATE STAN MODELS ---------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════
# Order:
#   1. Shared validation helpers
#   2. Round 1 pick-value model validation
#   3. Round 2 pick-value model validation
#   4. Markov transition model validation



### 09.01 - SHARED VALIDATION FUNCTIONS -----------------------------------------
# Validation helpers (validate_header, draws_matrix, existing_stan_vars,
# extract_stan_vector_draws, summarise_nuts, summarise_rhat_ess,
# second_round_band) are in 00_helpers.R.

# Five-pick bands for second-round checks (labels from second_round_band())
R2_BANDS <- c("31-35", "36-40", "41-45", "46-50", "51-55", "56-60")



### 09.02 - ROUND 1 PICK VALUE MODEL VALIDATION -------------------------------

validate_header("Round 1 pick-value model validation")

# PSIS-LOO for the production model and the Gaussian / Student-t benchmarks,
# plus the comparison models when fit. Production stays first: the app reports
# row 1 of pick_loo_summary. LOO scores the whole outcome density; a better LOO
# alone does not qualify a model for EPV (the lognormal mixture wins LOO but
# overstates top picks), so models are also compared on held-out mean error.
loo_r1_v3    <- loo::loo(as.matrix(fit_r1_v3$draws("log_lik", format = "draws_matrix")))
loo_r1_gauss <- loo::loo(as.matrix(fit_r1_gauss$draws("log_lik", format = "draws_matrix")))
loo_r1_t     <- loo::loo(as.matrix(fit_r1_t$draws("log_lik", format = "draws_matrix")))
pick_loo_list <- list(exgauss_pooled_gaps = loo_r1_v3,
                      mean_curve_gaussian = loo_r1_gauss,
                      student_t_xrapm     = loo_r1_t)

if (FIT_R1_COMPARISON_MODELS) {
  loo_r1_v1  <- loo::loo(as.matrix(fit_r1_v1$draws("log_lik", format = "draws_matrix")))
  loo_r1_v2  <- loo::loo(as.matrix(fit_r1_v2$draws("log_lik", format = "draws_matrix")))
  loo_r1_mix <- loo::loo(as.matrix(fit_r1_mix$draws("log_lik", format = "draws_matrix")))
  pick_loo_list <- c(pick_loo_list,
                     list(lognormal_mixture_xrapm = loo_r1_mix,
                          constant_sigma_0601     = loo_r1_v1,
                          linear_sigma_0605       = loo_r1_v2))
}

pick_loo_summary <- purrr::imap_dfr(pick_loo_list, function(l, nm) {
  tibble(model        = nm,
         elpd_loo     = l$estimates["elpd_loo", "Estimate"],
         p_loo        = l$estimates["p_loo", "Estimate"],
         looic        = l$estimates["looic", "Estimate"],
         max_pareto_k = max(loo::pareto_k_values(l), na.rm = TRUE))
})

pick_loo_compare_tbl <- if (length(pick_loo_list) > 1) {
  cmp <- as.data.frame(loo::loo_compare(pick_loo_list))
  # Newer loo versions already carry a `model` column
  if (!"model" %in% names(cmp)) cmp <- tibble::rownames_to_column(cmp, "model")
  as_tibble(cmp)
} else {
  pick_loo_summary %>%
    transmute(model, elpd_diff = 0, se_diff = 0, elpd_loo, p_loo, looic)
}

print(pick_loo_compare_tbl)
purrr::walk(pick_loo_list, function(l) print(loo::pareto_k_table(l)))


# Production first-round model parameters.
pick_params <- c("alpha", "beta", "gamma", "tau_eps",
                 "log_sd_1", "d_sd", "tau_sd", "logit_r_1", "d_r", "tau_r")

pick_draws <- fit_r1_v3$draws(variables = existing_stan_vars(fit_r1_v3,
                                                             c("alpha",
                                                               "beta",
                                                               "gamma",
                                                               "tau_eps",
                                                               "tau_sd",
                                                               "tau_r")),
                              format = "df") %>%
  as_tibble()

# Summary of parameters
print(fit_r1_v3$summary(pick_params))

pick_diag <- summarise_rhat_ess(fit = fit_r1_v3,
                                variables = pick_params,
                                model_name = "Round 1 ex-Gaussian pooled-gap model")

cat("\n  Round 1 NUTS diagnostics\n")
nuts_r1 <- summarise_nuts(fit_r1_v3, "round1_exgauss_pooled_gaps", max_treedepth = 12L)


# Expected value (the mean curve), outcome SD, and ex-Gaussian components by pick
pick_mu_draws    <- extract_stan_vector_draws(fit_r1_v3, "war4_pred", K = 30L)
pick_sd_draws    <- extract_stan_vector_draws(fit_r1_v3, "war4_pred_sd", K = 30L)
pick_tau_draws   <- extract_stan_vector_draws(fit_r1_v3, "tau_x", K = 30L)
pick_sigma_draws <- extract_stan_vector_draws(fit_r1_v3, "sigma_g", K = 30L)

# Player outcomes are simulated with draw_r1_outcomes() (00_helpers.R):
# Normal(mean - tau_x, sigma_g) + Exponential(mean tau_x).

# Player-outcome draws per pick for curve display. Several outcomes per
# posterior draw keep Monte Carlo noise out of the plotted quantiles.
OUTCOME_REPS_PER_DRAW <- 10L
outcome_display_idx <- rep(seq_len(nrow(pick_mu_draws)), OUTCOME_REPS_PER_DRAW)
pick_outcome_draws <- sapply(1:30, function(pk) {
  draw_r1_outcomes(outcome_display_idx, pk)
})
colnames(pick_outcome_draws) <- paste0("pick_", 1:30)

# Player-level PPC from the same outcome process the simulator uses
ppc_draw_idx <- sample(nrow(pick_mu_draws), min(2000L, nrow(pick_mu_draws)))
war4_rep_mat <- sapply(seq_len(nrow(pick_fit_data)), function(n) {
  draw_r1_outcomes(ppc_draw_idx, pick_fit_data$pick[n])
})

ppc_tbl <- tibble(row_id = seq_len(nrow(pick_fit_data)),
                  draft_year = pick_fit_data$draft_year,
                  pick = pick_fit_data$pick,
                  player = pick_fit_data$player,
                  obs = pick_fit_data$war4,
                  pred_mean = colMeans(war4_rep_mat),
                  lo = apply(war4_rep_mat, 2, quantile, probs = 0.05, na.rm = TRUE),
                  hi = apply(war4_rep_mat, 2, quantile, probs = 0.95, na.rm = TRUE)) %>%
  mutate(covered = obs >= lo & obs <= hi,
         pick_band = case_when(pick <= 5  ~ "1-5",
                               pick <= 10 ~ "6-10",
                               pick <= 15 ~ "11-15",
                               pick <= 20 ~ "16-20",
                               TRUE       ~ "21-30"))

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

cat("\n  Round 1 outcome SD curve\n")
print(sigma_curve, n = 30)



### 09.03 - ROUND 2 PICK VALUE MODEL VALIDATION -------------------------------

validate_header("Round 2 pick-value model validation")

pick_fit_data_r2 <- pick_fit_data_r2 %>%
  mutate(pick_band = second_round_band(pick))

# Core parameters
pick2_params <- c("eta_31", "d_pi", "tau_pi",
                  "ev_31", "log_gap", "tau_eps",
                  "logit_q_31", "d_q", "tau_q",
                  "log_T_31", "d_T", "tau_T",
                  "log_s0_31", "d_s0", "tau_s0")

# Summary of parameters
print(fit_r2$summary(pick2_params))

pick2_params <- existing_stan_vars(fit_r2, pick2_params)

pick2_draws <- fit_r2$draws(variables = pick2_params,
                            format = "df") %>%
  as_tibble()

pick2_diag <- summarise_rhat_ess(fit = fit_r2,
                                 variables = pick2_params,
                                 model_name = "Round 2 fringe/contributor hurdle model")

cat("\n  Round 2 NUTS diagnostics\n")
nuts_r2 <- summarise_nuts(fit_r2, "round2_mixture_hurdle", max_treedepth = 12L)


# Round 2 generated curves used by downstream simulation and app exports.
pick2_p_play_draws  <- extract_stan_vector_draws(fit_r2, "pi_p", K = 30L)
pick2_cond_mu_draws <- extract_stan_vector_draws(fit_r2, "mean_play", K = 30L)
pick2_cond_sd_draws <- extract_stan_vector_draws(fit_r2, "sd_play", K = 30L)
pick2_mu_draws      <- extract_stan_vector_draws(fit_r2, "ev", K = 30L)
pick2_sd_draws      <- extract_stan_vector_draws(fit_r2, "ev_sd", K = 30L)
pick2_contrib_draws <- extract_stan_vector_draws(fit_r2, "p_contrib", K = 30L)
pick2_upside_draws  <- extract_stan_vector_draws(fit_r2, "upside_mean", K = 30L)
pick2_fringe_draws  <- extract_stan_vector_draws(fit_r2, "fringe_mean", K = 30L)
pick2_fringe_sd_draws <- extract_stan_vector_draws(fit_r2, "sd_fringe", K = 30L)
n_pick2_draws <- nrow(pick2_p_play_draws)

# Outcomes come from draw_r2_outcomes() (00_helpers.R): exactly zero if the
# player never plays. Played players sit at this draw's fringe level plus
# Gaussian noise, and contributors add an exponential upside; the mixture mean
# is the draw's played mean.

# Full outcome draws per pick for curve display (several per posterior draw)
outcome_display_idx_r2 <- rep(seq_len(n_pick2_draws), OUTCOME_REPS_PER_DRAW)
pick2_outcome_draws <- sapply(31:60, function(pk) {
  draw_r2_outcomes(outcome_display_idx_r2, pk)
})
colnames(pick2_outcome_draws) <- paste0("y_pick_rep[", 1:30, "]")


# Useful identity check: EV should equal P(play) x E[war4 | played].
r2_ev_identity_check <- tibble(pick = 31:60,
                               pick_band = second_round_band(pick),
                               p_play = colMeans(pick2_p_play_draws),
                               cond_played_mean_war = colMeans(pick2_cond_mu_draws),
                               manual_ev = p_play * cond_played_mean_war,
                               model_ev = colMeans(pick2_mu_draws),
                               diff = model_ev - manual_ev)

cat("\n  Round 2 EV identity check\n")
print(r2_ev_identity_check, n = 30)

# Replicated played indicators: rows = posterior draws, columns = players
played_rep_mat_r2 <- draws_matrix(fit_r2, "play_rep")
if (ncol(played_rep_mat_r2) != nrow(pick_fit_data_r2)) {
  if (nrow(played_rep_mat_r2) == nrow(pick_fit_data_r2)) {
    warning("Round 2 played_rep was observations x draws; transposing to draws x observations.")
    played_rep_mat_r2 <- t(played_rep_mat_r2)
  } else {
    stop("Round 2 played_rep has dimensions ", paste(dim(played_rep_mat_r2), collapse = " x "),
         "; expected draws x ", nrow(pick_fit_data_r2), ".", call. = FALSE)
  }
}

# Player-level PPC from the same outcome process the simulator uses
ppc_draw_idx_r2 <- sample(n_pick2_draws, min(2000L, n_pick2_draws))
war4_rep_mat_r2 <- sapply(seq_len(nrow(pick_fit_data_r2)), function(n) {
  draw_r2_outcomes(ppc_draw_idx_r2, pick_fit_data_r2$pick[n])
})
played_rep_mat_r2 <- played_rep_mat_r2[ppc_draw_idx_r2, , drop = FALSE]

ppc_tbl_r2 <- tibble(row_id = seq_len(nrow(pick_fit_data_r2)),
                     draft_year = pick_fit_data_r2$draft_year,
                     pick = pick_fit_data_r2$pick,
                     pick_band = pick_fit_data_r2$pick_band,
                     player = pick_fit_data_r2$player,
                     played = pick_fit_data_r2$played,
                     obs = pick_fit_data_r2$war4,
                     lo = apply(war4_rep_mat_r2, 2, quantile, 
                                probs = 0.05, na.rm = TRUE),
                     hi = apply(war4_rep_mat_r2, 2, quantile, 
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
  mutate(obs_bucket = case_when(obs <= 0  ~ "<=0 wins",
                                obs <= 2  ~ "0-2 wins",
                                obs <= 5  ~ "2-5 wins",
                                obs <= 10 ~ "5-10 wins",
                                TRUE      ~ "10+ wins")) %>%
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

war_dist_band_check <- purrr::map_dfr(R2_BANDS, function(b) {
  idx <- which(pick_fit_data_r2$pick_band == b)
  obs <- pick_fit_data_r2$war4[idx]
  rep <- as.vector(war4_rep_mat_r2[, idx, drop = FALSE])
  
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
print(war_dist_band_check)

band10_check <- pick_fit_data_r2 %>%
  mutate(band10 = case_when(pick <= 40 ~ "31-40",
                            pick <= 50 ~ "41-50",
                            TRUE       ~ "51-60")) %>%
  group_by(band10) %>%
  summarise(n = n(),
            mean_war4 = mean(war4),
            p_play = mean(played),
            p_ge_5 = mean(war4 >= 5),
            p_ge_10 = mean(war4 >= 10),
            q05 = quantile(war4, 0.05),
            q95 = quantile(war4, 0.95),
            .groups = "drop")

cat("\n  Round 2 empirical summary by ten-pick band\n")
print(band10_check)

r2_overall_check <- pick_fit_data_r2 %>%
  summarise(n = n(),
            mean_war4 = mean(war4),
            p_play = mean(played),
            p_ge_5 = mean(war4 >= 5),
            p_ge_10 = mean(war4 >= 10),
            q05 = quantile(war4, 0.05),
            q95 = quantile(war4, 0.95))

cat("\n  Round 2 empirical overall summary\n")
print(r2_overall_check)

# P(play) calibration by exact slot.
play_rep_slot_rate_mat_r2 <- sapply(31:60, function(pk) {
  idx <- which(pick_fit_data_r2$pick == pk)
  if (length(idx) == 0L) return(rep(NA_real_, nrow(played_rep_mat_r2)))
  rowMeans(played_rep_mat_r2[, idx, drop = FALSE])
})
colnames(play_rep_slot_rate_mat_r2) <- as.character(31:60)

ppc_play_pick_r2 <- tibble(pick = 31:60) %>%
  left_join(pick_fit_data_r2 %>%
              group_by(pick) %>%
              summarise(n = n(),
                        emp_p_play = mean(played),
                        .groups = "drop"),
            by = "pick") %>%
  mutate(n = coalesce(n, 0L),
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
                                     quantile, 0.95, na.rm = TRUE),
         emp_within_90_rep_interval = emp_p_play >= pred_rep_p_play_q05 &
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

markov_fit$cmdstan_summary()

rank_markov_core_vars <- c("distance_slope")
rank_markov_param_summary <- summarise_rhat_ess(fit = markov_fit,
                                                variables = rank_markov_core_vars,
                                                model_name = "30-rank fixed-smoothing Markov model")

nuts_markov <- summarise_nuts(markov_fit, "rank_markov_transition_v3", max_treedepth = 12L)

# theta[draw, from_rank, to_rank], where rank 1 = worst and rank 30 = best.
theta_draws <- draws_matrix(markov_fit, "theta")
n_markov_draws <- nrow(theta_draws)

# Column name of theta_draws for each (from, to) cell: theta_cols[i, j] = "theta[i,j]".
# matrix(theta_draws[d, theta_cols], N_RANKS, N_RANKS) rebuilds draw d as a
# 30 x 30 transition matrix.
theta_cols <- matrix(sprintf("theta[%d,%d]",
                             rep(RANK_STATES, times = N_RANKS),
                             rep(RANK_STATES, each = N_RANKS)),
                     N_RANKS, N_RANKS)

# Posterior-mean transition matrix
post_trans <- matrix(colMeans(theta_draws)[theta_cols], N_RANKS, N_RANKS,
                     dimnames = list(from = RANK_STATE_LABELS,
                                     to   = RANK_STATE_LABELS))

post_rank_trans <- post_trans
post_tier_trans <- aggregate_rank_transition_to_tier(post_rank_trans)

cat("\n  Stan v3 posterior-mean 30-rank transition matrix: first 10x10 block\n")
print(round(post_rank_trans[1:10, 1:10], 3))

cat("\n  Kernel-smoothed empirical 30-rank baseline: first 10x10 block\n")
print(round(rank_kernel_transition_matrix[1:10, 1:10], 3))

cat("\n  Stan v3 rank-model-implied five-tier transition matrix\n")
print(round(post_tier_trans, 3))

cat("\n  Kernel-smoothed empirical five-tier implied baseline\n")
print(round(rank_kernel_tier_transition_matrix, 3))

cat("\n  Legacy five-tier closed-form benchmark matrix\n")
print(round(tier_posterior_mean_closed, 3))

# Stationary distribution and mixing time (markov_diagnostics() in 00_helpers.R)
mc_diag <- markov_diagnostics(post_rank_trans)
names(mc_diag$stationary) <- RANK_STATE_LABELS
mc_diag_tier <- markov_diagnostics(post_tier_trans)
names(mc_diag_tier$stationary) <- TIERS

kernel_diag <- markov_diagnostics(rank_kernel_transition_matrix)
names(kernel_diag$stationary) <- RANK_STATE_LABELS
kernel_diag_tier <- markov_diagnostics(rank_kernel_tier_transition_matrix)
names(kernel_diag_tier$stationary) <- TIERS

rank_transition_smoothness <- tibble(
  model = "stan_v3_fixed_smoothing",
  mean_abs_adjacent_row_diff = mean(abs(post_rank_trans[-1, ] - post_rank_trans[-N_RANKS, ])),
  mean_abs_adjacent_col_diff = mean(abs(post_rank_trans[, -1] - post_rank_trans[, -N_RANKS])),
  mean_diagonal_probability = mean(diag(post_rank_trans)),
  max_row_sum_error = max(abs(rowSums(post_rank_trans) - 1))
)

kernel_transition_smoothness <- tibble(
  model = "kernel_smoothed_empirical",
  mean_abs_adjacent_row_diff = mean(abs(rank_kernel_transition_matrix[-1, ] - 
                                          rank_kernel_transition_matrix[-N_RANKS, ])),
  mean_abs_adjacent_col_diff = mean(abs(rank_kernel_transition_matrix[, -1] - 
                                          rank_kernel_transition_matrix[, -N_RANKS])),
  mean_diagonal_probability = mean(diag(rank_kernel_transition_matrix)),
  max_row_sum_error = max(abs(rowSums(rank_kernel_transition_matrix) - 1))
)

rank_transition_smoothness <- bind_rows(rank_transition_smoothness, 
                                        kernel_transition_smoothness)

cat("\n  Rank transition smoothness diagnostics\n")
print(rank_transition_smoothness)

cat("\n  Stationary rank distribution: all ranks\n")
print(round(mc_diag$stationary, 3))

cat(sprintf("  Stan v3 rank-chain 2nd eigenvalue %.3f -> mixing time %.1f years\n",
            mc_diag$lambda2,
            mc_diag$mixing_time))
cat(sprintf("  Kernel baseline rank-chain 2nd eigenvalue %.3f -> mixing time %.1f years\n",
            kernel_diag$lambda2,
            kernel_diag$mixing_time))
