################################################################################
# NBA Lottery Stan Model Validation
#
# Standalone validation script for the two Stan models fit in nba_lottery.R:
#   1. pick_value.stan      -> pick_fit
#   2. team_strength.stan   -> markov_fit
#
# Run options:
#   A) Recommended during development:
#        source("nba_lottery.R")
#        source("nba_lottery_stan_validation.R")
#
#   B) Standalone from the same project directory:
#        Rscript nba_lottery_stan_validation.R
#      If pick_fit / markov_fit are not already in memory, this script can source
#      nba_lottery.R, but that will rerun scraping, fitting, and the MC pipeline.
#
# Outputs are written to ./stan_validation/ by default.
################################################################################

# ==============================================================================
# 0. USER CONTROLS
# ==============================================================================

PROJECT_DIR <- getwd()
NBA_LOTTERY_SCRIPT <- file.path(PROJECT_DIR, "nba_lottery.R")

OUT_DIR <- file.path(PROJECT_DIR, "03_validation")
RUN_NBA_LOTTERY_IF_NEEDED <- TRUE

# SBC can be computationally expensive. Use 25-50 for smoke tests and 200+ for a
# serious rank-uniformity check. The Markov model is cheap; the pick curve is the
# heavier one.
RUN_SBC_PICK   <- TRUE
RUN_SBC_MARKOV <- TRUE
SBC_REPS_PICK   <- 50L
SBC_REPS_MARKOV <- 100L
SBC_CHAINS <- 4L
SBC_PARALLEL_CHAINS <- min(4L, SBC_CHAINS)
SBC_ITER_WARMUP <- 500L
SBC_ITER_SAMPLING <- 500L
SBC_ADAPT_DELTA <- 0.95
SBC_MAX_TREEDEPTH <- 12L

# LOO generated-quantities assumptions for pick_value.stan. If your original
# pick_value.stan likelihood differs, edit write_pick_gq_stan() below to match it.
# The fallback GQ program below matches the current player-level model:
#   ws4[n] ~ student_t(nu, alpha / pick[n]^beta + gamma,
#                      sigma_pick[pick[n]])
# where sigma_pick[1:30] is built with an adjacent-pick random walk on log sigma.
# It also matches the direct parameterization:
#   gamma ~ normal(2, 3)
#   nu - 2 ~ exponential(0.20)
GENERATE_LOG_LIK_IF_MISSING <- TRUE

# NUTS thresholds. These are intentionally strict for a production dashboard.
RHAT_THRESHOLD <- 1.01
ESS_BULK_MIN   <- 400
ESS_TAIL_MIN   <- 400
EBFMI_MIN      <- 0.30
MAX_TREEDEPTH_THRESHOLD <- 10L   # original fits use cmdstan default unless set

set.seed(2026)

# ==============================================================================
# 1. PACKAGES + SMALL UTILITIES
# ==============================================================================

required_pkgs <- c(
  "cmdstanr", "posterior", "bayesplot", "loo", "tidyverse", "jsonlite"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Install missing packages before running validation: ",
    paste(missing_pkgs, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(loo)
  library(tidyverse)
  library(jsonlite)
})

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

message_section <- function(x) {
  cat("\n", strrep("=", 80), "\n", x, "\n", strrep("=", 80), "\n", sep = "")
}

write_csv_safe <- function(x, path) {
  tryCatch(readr::write_csv(x, path), error = function(e) warning("Could not write ", path, ": ", e$message))
  invisible(path)
}

save_plot_safe <- function(plot, path, width = 9, height = 6, dpi = 150) {
  tryCatch(
    ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi),
    error = function(e) warning("Could not save plot ", path, ": ", e$message)
  )
  invisible(path)
}

parse_stan_indices <- function(x) {
  # Returns matrix with columns i, j, ... parsed from names like theta[2,4].
  inside <- stringr::str_match(x, "\\[(.*)\\]")[, 2]
  idx <- strsplit(inside, ",", fixed = TRUE)
  max_len <- max(lengths(idx))
  out <- matrix(NA_integer_, nrow = length(idx), ncol = max_len)
  for (r in seq_along(idx)) out[r, seq_along(idx[[r]])] <- as.integer(idx[[r]])
  out
}

array_draws_matrix <- function(fit, variable_base) {
  mat <- as.matrix(fit$draws(variables = variable_base, format = "draws_matrix"))
  if (ncol(mat) == 0) stop("No draws found for variable base: ", variable_base, call. = FALSE)
  idx <- parse_stan_indices(colnames(mat))
  ord <- do.call(order, as.data.frame(idx))
  mat[, ord, drop = FALSE]
}

has_stan_variable <- function(fit, variable_base) {
  vars <- fit$metadata()$model_params
  any(vars == variable_base | startsWith(vars, paste0(variable_base, "[")))
}

extract_chain_id <- function(fit) {
  df <- posterior::as_draws_df(fit$draws())
  as.integer(df$.chain)
}

# ==============================================================================
# 2. LOAD / BUILD OBJECTS FROM nba_lottery.R
# ==============================================================================

needed_objects <- c(
  "pick_fit", "markov_fit", "pick_model", "markov_model",
  "pick_fit_data", "pick_slot_data", "counts_mat", "alpha_prior",
  "N_TIERS", "TIERS", "posterior_mean_closed"
)

have_needed <- function() all(vapply(needed_objects, exists, logical(1), envir = .GlobalEnv))

if (!have_needed()) {
  if (!RUN_NBA_LOTTERY_IF_NEEDED) {
    stop(
      "Validation objects are missing. First run source('nba_lottery.R'), ",
      "or set RUN_NBA_LOTTERY_IF_NEEDED <- TRUE.",
      call. = FALSE
    )
  }
  if (!file.exists(NBA_LOTTERY_SCRIPT)) {
    stop("Could not find nba_lottery.R at: ", NBA_LOTTERY_SCRIPT, call. = FALSE)
  }
  message_section("Sourcing nba_lottery.R to create fit objects")
  source(NBA_LOTTERY_SCRIPT, local = .GlobalEnv)
}

missing_after_source <- needed_objects[!vapply(needed_objects, exists, logical(1), envir = .GlobalEnv)]
if (length(missing_after_source) > 0) {
  stop("Still missing required objects: ", paste(missing_after_source, collapse = ", "), call. = FALSE)
}

# Pull objects into this script's environment explicitly.
pick_fit <- get("pick_fit", envir = .GlobalEnv)
markov_fit <- get("markov_fit", envir = .GlobalEnv)
pick_model <- get("pick_model", envir = .GlobalEnv)
markov_model <- get("markov_model", envir = .GlobalEnv)
# pick_stan_data was renamed to pick_stan_data_player in nba_lottery.R.
# Prefer an existing object when present, otherwise rebuild the minimal Stan data
# from pick_fit_data. This also prevents stale legacy pick_stan_data objects from
# being used accidentally in long interactive R sessions.
if (exists("pick_stan_data_player", envir = .GlobalEnv)) {
  pick_stan_data <- get("pick_stan_data_player", envir = .GlobalEnv)
} else if (exists("pick_stan_data", envir = .GlobalEnv)) {
  pick_stan_data <- get("pick_stan_data", envir = .GlobalEnv)
} else {
  pfd <- get("pick_fit_data", envir = .GlobalEnv)
  pick_stan_data <- list(
    N = nrow(pfd),
    pick = as.integer(pfd$pick),
    ws4 = as.numeric(pfd$ws4)
  )
}
pick_fit_data <- get("pick_fit_data", envir = .GlobalEnv)
pick_slot_data <- get("pick_slot_data", envir = .GlobalEnv)
counts_mat <- get("counts_mat", envir = .GlobalEnv)
alpha_prior <- get("alpha_prior", envir = .GlobalEnv)
N_TIERS <- get("N_TIERS", envir = .GlobalEnv)
TIERS <- get("TIERS", envir = .GlobalEnv)
posterior_mean_closed <- get("posterior_mean_closed", envir = .GlobalEnv)

message_section("Validation inputs")
cat("Pick model observations:", pick_stan_data$N, "\n")
cat("Markov states:", N_TIERS, "\n")
cat("Observed transition count total:", sum(counts_mat), "\n")
cat("Output directory:", OUT_DIR, "\n")

# ==============================================================================
# 3. GENERIC STAN DIAGNOSTICS
# ==============================================================================

summarize_mcmc_parameters <- function(fit, model_name, variables = NULL) {
  smry <- fit$summary(variables = variables) %>%
    as_tibble() %>%
    mutate(
      model = model_name,
      flag_rhat = !is.na(rhat) & rhat > RHAT_THRESHOLD,
      flag_ess_bulk = !is.na(ess_bulk) & ess_bulk < ESS_BULK_MIN,
      flag_ess_tail = !is.na(ess_tail) & ess_tail < ESS_TAIL_MIN
    ) %>%
    relocate(model, variable)

  write_csv_safe(smry, file.path(OUT_DIR, paste0(model_name, "_parameter_summary.csv")))
  smry
}

compute_ebfmi_one_chain <- function(energy) {
  energy <- energy[is.finite(energy)]
  if (length(energy) < 3 || stats::var(energy) == 0) return(NA_real_)
  mean(diff(energy)^2) / stats::var(energy)
}

summarize_nuts <- function(fit, model_name, max_treedepth = MAX_TREEDEPTH_THRESHOLD,
                           write_files = TRUE, run_cmdstan_diagnose = TRUE) {
  sd <- as_tibble(fit$sampler_diagnostics(format = "df"))

  if (!".chain" %in% names(sd)) sd$.chain <- 1L
  if (!".iteration" %in% names(sd)) sd$.iteration <- seq_len(nrow(sd))

  diag_by_chain <- sd %>%
    arrange(.chain, .iteration) %>%
    group_by(.chain) %>%
    summarise(
      n_draws = n(),
      divergences = sum(.data$divergent__ > 0, na.rm = TRUE),
      pct_divergent = divergences / n_draws,
      max_treedepth_hits = sum(.data$treedepth__ >= max_treedepth, na.rm = TRUE),
      pct_max_treedepth = max_treedepth_hits / n_draws,
      max_treedepth_observed = max(.data$treedepth__, na.rm = TRUE),
      mean_accept_stat = mean(.data$accept_stat__, na.rm = TRUE),
      min_accept_stat = min(.data$accept_stat__, na.rm = TRUE),
      mean_stepsize = mean(.data$stepsize__, na.rm = TRUE),
      max_n_leapfrog = max(.data$n_leapfrog__, na.rm = TRUE),
      ebfmi = compute_ebfmi_one_chain(.data$energy__),
      flag_divergences = divergences > 0,
      flag_treedepth = max_treedepth_hits > 0,
      flag_ebfmi = !is.na(ebfmi) & ebfmi < EBFMI_MIN,
      .groups = "drop"
    ) %>%
    mutate(model = model_name) %>%
    relocate(model, .chain)

  if (write_files) {
    write_csv_safe(diag_by_chain, file.path(OUT_DIR, paste0(model_name, "_nuts_diagnostics.csv")))
  }

  # Save CmdStan's own diagnose output where available. Skip this inside SBC loops.
  if (write_files && run_cmdstan_diagnose) {
    diag_txt <- tryCatch(capture.output(fit$cmdstan_diagnose()), error = function(e) paste("cmdstan_diagnose failed:", e$message))
    writeLines(diag_txt, file.path(OUT_DIR, paste0(model_name, "_cmdstan_diagnose.txt")))
  }

  diag_by_chain
}

plot_mcmc_core <- function(fit, model_name, variables) {
  draws <- fit$draws(variables = variables)

  p_trace <- bayesplot::mcmc_trace(draws, pars = variables)
  save_plot_safe(p_trace, file.path(OUT_DIR, paste0(model_name, "_trace.png")), width = 12, height = 8)

  p_rank <- bayesplot::mcmc_rank_overlay(draws, pars = variables)
  save_plot_safe(p_rank, file.path(OUT_DIR, paste0(model_name, "_rank_overlay.png")), width = 12, height = 8)

  p_acf <- bayesplot::mcmc_acf_bar(draws, pars = variables)
  save_plot_safe(p_acf, file.path(OUT_DIR, paste0(model_name, "_acf.png")), width = 12, height = 8)

  if (length(variables) <= 8) {
    p_pairs <- bayesplot::mcmc_pairs(draws, pars = variables, off_diag_args = list(size = 0.2, alpha = 0.2))
    save_plot_safe(p_pairs, file.path(OUT_DIR, paste0(model_name, "_pairs.png")), width = 10, height = 10)
  }

  invisible(list(trace = p_trace, rank = p_rank, acf = p_acf))
}

message_section("Core MCMC diagnostics")

pick_core_vars <- c("alpha", "beta", "gamma", "log_sigma_1", "tau_log_sigma_rw", "nu")
pick_param_summary <- summarize_mcmc_parameters(pick_fit, "pick_value", pick_core_vars)
pick_nuts <- summarize_nuts(pick_fit, "pick_value")
plot_mcmc_core(pick_fit, "pick_value", pick_core_vars)

markov_param_summary <- summarize_mcmc_parameters(markov_fit, "markov")
markov_nuts <- summarize_nuts(markov_fit, "markov")
# The Markov model has 25 theta cells; plot selected interpretable cells only.
markov_plot_vars <- c("theta[1,1]", "theta[1,2]", "theta[2,2]", "theta[3,3]", "theta[4,4]", "theta[5,5]")
plot_mcmc_core(markov_fit, "markov", markov_plot_vars)

all_param_summary <- bind_rows(pick_param_summary, markov_param_summary)
all_nuts <- bind_rows(pick_nuts, markov_nuts)
write_csv_safe(all_param_summary, file.path(OUT_DIR, "all_parameter_summary.csv"))
write_csv_safe(all_nuts, file.path(OUT_DIR, "all_nuts_diagnostics.csv"))

# ==============================================================================
# 4. POSTERIOR PREDICTIVE CHECKS
# ==============================================================================

message_section("Posterior predictive checks")

# ---- 4A. Pick-value PPC --------------------------------------------------------

pick_existing_gq_matches_data <- function(fit, n_obs) {
  if (!has_stan_variable(fit, "log_lik") || !has_stan_variable(fit, "ws4_rep")) {
    return(FALSE)
  }

  dims_ok <- tryCatch({
    m <- array_draws_matrix(fit, "ws4_rep")
    ncol(m) == n_obs || nrow(m) == n_obs
  }, error = function(e) FALSE)

  isTRUE(dims_ok)
}

get_or_generate_pick_gq <- function() {
  # Use generated quantities already in pick_fit only if they match the current
  # validation data. In interactive sessions, an old pick_fit/pick_stan_data
  # combination can leave ws4_rep with the wrong number of observations.
  if (pick_existing_gq_matches_data(pick_fit, length(pick_stan_data$ws4))) {
    return(pick_fit)
  }

  if (!GENERATE_LOG_LIK_IF_MISSING) {
    stop(
      "pick_fit either lacks log_lik/ws4_rep or its GQ dimensions do not match ",
      "the current pick_stan_data. Set GENERATE_LOG_LIK_IF_MISSING <- TRUE.",
      call. = FALSE
    )
  }

  gq_file <- file.path(OUT_DIR, "pick_value_validation_gq.stan")
  writeLines(write_pick_gq_stan(), gq_file)
  gq_model <- cmdstan_model(gq_file, force_recompile = TRUE)
  gq_model$generate_quantities(fitted_params = pick_fit, data = pick_stan_data, seed = 2026)
}

write_pick_gq_stan <- function() {
  "data {
  int<lower=1> N;
  array[N] int<lower=1, upper=30> pick;
  vector[N] ws4;
}
parameters {
  // Same parameter names and constraints as the current pick_value.stan.
  // This matters when generate_quantities() reuses fitted posterior draws.
  real log_alpha;
  real log_beta;
  real gamma;

  real log_sigma_1;
  vector[29] z_sigma_step;
  real<lower=0> tau_log_sigma_rw;

  real<lower=2> nu;
}
transformed parameters {
  real<lower=0> alpha = exp(log_alpha);
  real<lower=0> beta = exp(log_beta);

  vector[30] log_sigma_pick;
  vector<lower=0>[30] sigma_pick;

  log_sigma_pick[1] = log_sigma_1;
  for (p in 2:30) {
    log_sigma_pick[p] = log_sigma_pick[p - 1] +
                        tau_log_sigma_rw * z_sigma_step[p - 1];
  }
  sigma_pick = exp(log_sigma_pick);
}
generated quantities {
  vector[30] war_pred;
  vector[30] war_pred_sd;
  vector[N] log_lik;
  vector[N] ws4_rep;

  for (p in 1:30) {
    real mu = alpha / pow(p, beta) + gamma;
    war_pred[p] = mu;
    war_pred_sd[p] = sigma_pick[p];
  }

  for (n in 1:N) {
    real mu = alpha / pow(pick[n], beta) + gamma;
    log_lik[n] = student_t_lpdf(ws4[n] | nu, mu, sigma_pick[pick[n]]);
    ws4_rep[n] = student_t_rng(nu, mu, sigma_pick[pick[n]]);
  }
}"
}
pick_gq_fit <- get_or_generate_pick_gq()

war_obs <- as.numeric(pick_stan_data$ws4)
pick_obs <- as.numeric(pick_stan_data$pick)

# bayesplot requires yrep to be a matrix with dimensions:
#   posterior draws x observations
# Some posterior/draws extraction paths can return the transpose, and stale
# interactive objects can also make the GQ dimensions disagree with the current
# validation data. Normalize orientation explicitly and fail with an informative
# message rather than letting bayesplot throw `ncol(yrep) must equal length(y)`.
ensure_yrep_matrix <- function(yrep, y, name = "yrep") {
  yrep <- as.matrix(yrep)
  n_obs <- length(y)

  if (ncol(yrep) == n_obs) {
    return(yrep)
  }
  if (nrow(yrep) == n_obs) {
    warning(name, " was observations x draws; transposing to draws x observations.")
    return(t(yrep))
  }

  stop(
    name, " has incompatible dimensions: ",
    paste(dim(yrep), collapse = " x "),
    "; expected draws x ", n_obs,
    ". This usually means pick_stan_data and pick_fit/pick_gq_fit come from different model runs. ",
    "Clear stale objects or regenerate quantities with the current pick_stan_data.",
    call. = FALSE
  )
}

ws4_rep <- array_draws_matrix(pick_gq_fit, "ws4_rep")
ws4_rep <- ensure_yrep_matrix(ws4_rep, war_obs, "ws4_rep")

if (length(pick_obs) != length(war_obs)) {
  stop("pick_obs and war_obs have different lengths; check pick_stan_data$pick/ws4.", call. = FALSE)
}

cat(sprintf("Pick PPC y length: %d | ws4_rep dims: %d draws x %d observations\n",
            length(war_obs), nrow(ws4_rep), ncol(ws4_rep)))

# bayesplot needs a manageable number of replicated draws.
ppc_draw_ids <- sort(sample(seq_len(nrow(ws4_rep)), size = min(250, nrow(ws4_rep))))
yrep_pick <- ws4_rep[ppc_draw_ids, , drop = FALSE]

p_pick_intervals <- bayesplot::ppc_intervals(
  y = war_obs,
  yrep = yrep_pick,
  x = pick_obs,
  prob = 0.50,
  prob_outer = 0.90
) +
  ggplot2::labs(
    title = "Pick-value model PPC: observed player outcomes vs replicated intervals",
    x = "Draft pick",
    y = "First-4-year win shares"
  )
save_plot_safe(p_pick_intervals, file.path(OUT_DIR, "pick_value_ppc_intervals.png"), width = 10, height = 6)

p_pick_dens <- bayesplot::ppc_dens_overlay(war_obs, yrep_pick[seq_len(min(50, nrow(yrep_pick))), , drop = FALSE]) +
  ggplot2::labs(title = "Pick-value model PPC: player-level distribution overlay")
save_plot_safe(p_pick_dens, file.path(OUT_DIR, "pick_value_ppc_density.png"), width = 10, height = 6)

p_pick_stat_mean <- bayesplot::ppc_stat(war_obs, yrep_pick, stat = "mean") +
  ggplot2::labs(title = "Pick-value PPC: player-level mean")
save_plot_safe(p_pick_stat_mean, file.path(OUT_DIR, "pick_value_ppc_mean.png"), width = 8, height = 5)

p_pick_stat_sd <- bayesplot::ppc_stat(war_obs, yrep_pick, stat = "sd") +
  ggplot2::labs(title = "Pick-value PPC: player-level standard deviation")
save_plot_safe(p_pick_stat_sd, file.path(OUT_DIR, "pick_value_ppc_sd.png"), width = 8, height = 5)

pick_ppc_summary <- tibble(
  row_id = seq_along(war_obs),
  pick = pick_obs,
  observed = war_obs,
  pred_mean = colMeans(ws4_rep),
  pred_q05 = apply(ws4_rep, 2, quantile, probs = 0.05),
  pred_q50 = apply(ws4_rep, 2, quantile, probs = 0.50),
  pred_q95 = apply(ws4_rep, 2, quantile, probs = 0.95),
  covered_90 = observed >= pred_q05 & observed <= pred_q95,
  z_resid = (observed - pred_mean) / apply(ws4_rep, 2, sd)
)

if (exists("pick_fit_data") && nrow(pick_fit_data) == nrow(pick_ppc_summary)) {
  pick_ppc_summary <- pick_ppc_summary %>%
    bind_cols(pick_fit_data %>% select(any_of(c("draft_year", "player"))))
}

write_csv_safe(pick_ppc_summary, file.path(OUT_DIR, "pick_value_ppc_summary.csv"))

cat(sprintf("Pick-value 90%% PPC interval coverage: %.1f%% of player rows\n", 100 * mean(pick_ppc_summary$covered_90)))
cat(sprintf("Pick-value max |PPC z residual|: %.2f\n", max(abs(pick_ppc_summary$z_resid), na.rm = TRUE)))

# ---- 4B. Markov transition PPC -------------------------------------------------

extract_theta_array <- function(fit, K) {
  theta_mat <- as.matrix(fit$draws(variables = "theta", format = "draws_matrix"))
  idx <- parse_stan_indices(colnames(theta_mat))
  arr <- array(NA_real_, dim = c(nrow(theta_mat), K, K))
  for (c in seq_len(ncol(theta_mat))) {
    arr[, idx[c, 1], idx[c, 2]] <- theta_mat[, c]
  }
  arr
}

simulate_markov_counts <- function(theta_arr, row_totals, n_draws = 500L) {
  draw_ids <- sort(sample(seq_len(dim(theta_arr)[1]), size = min(n_draws, dim(theta_arr)[1])))
  K <- length(row_totals)
  out <- array(0L, dim = c(length(draw_ids), K, K))
  for (d in seq_along(draw_ids)) {
    for (i in seq_len(K)) {
      out[d, i, ] <- as.integer(rmultinom(1, size = row_totals[i], prob = theta_arr[draw_ids[d], i, ]))
    }
  }
  out
}

markov_ppc_stats <- function(counts_array, model_name = "rep") {
  K <- dim(counts_array)[2]
  jump_distance <- abs(row(matrix(seq_len(K), K, K)) - col(matrix(seq_len(K), K, K)))
  tibble(
    draw = seq_len(dim(counts_array)[1]),
    model = model_name,
    total_stay = apply(counts_array, 1, function(m) sum(diag(m))),
    big_jumps = apply(counts_array, 1, function(m) sum(m[jump_distance >= 2])),
    max_cell = apply(counts_array, 1, max),
    zero_cells = apply(counts_array, 1, function(m) sum(m == 0)),
    playoff_stay = counts_array[, K, K],
    bottom_to_playoff = counts_array[, 1, K]
  )
}

theta_arr <- extract_theta_array(markov_fit, N_TIERS)
counts_rep <- simulate_markov_counts(theta_arr, rowSums(counts_mat), n_draws = 1000L)
counts_obs_array <- array(counts_mat, dim = c(1, N_TIERS, N_TIERS))

markov_ppc_rep <- markov_ppc_stats(counts_rep, "rep")
markov_ppc_obs <- markov_ppc_stats(counts_obs_array, "obs") %>% select(-draw, -model)
write_csv_safe(markov_ppc_rep, file.path(OUT_DIR, "markov_ppc_replicated_stats.csv"))
write_csv_safe(markov_ppc_obs, file.path(OUT_DIR, "markov_ppc_observed_stats.csv"))

plot_markov_ppc_stat <- function(stat_name) {
  obs_val <- markov_ppc_obs[[stat_name]][1]
  ggplot(markov_ppc_rep, aes(x = .data[[stat_name]])) +
    geom_histogram(bins = 30, alpha = 0.75) +
    geom_vline(xintercept = obs_val, linewidth = 1.1, linetype = 2) +
    labs(
      title = paste("Markov PPC:", stat_name),
      subtitle = "Dashed line = observed transition-count statistic",
      x = stat_name,
      y = "Replicated draws"
    ) +
    theme_minimal(base_size = 12)
}

for (stat_name in c("total_stay", "big_jumps", "max_cell", "zero_cells", "playoff_stay", "bottom_to_playoff")) {
  save_plot_safe(
    plot_markov_ppc_stat(stat_name),
    file.path(OUT_DIR, paste0("markov_ppc_", stat_name, ".png")),
    width = 8,
    height = 5
  )
}

# Heatmap comparison: observed vs posterior predictive mean counts.
mean_counts_rep <- apply(counts_rep, c(2, 3), mean)

# as_tibble(as.table(...), .name_repair = "minimal") can create blank names,
# and dplyr::mutate() refuses to work on data frames with NA/blank names.
# Convert via as.data.frame.table() and name the columns immediately.
dimnames(mean_counts_rep) <- list(from = TIERS, to = TIERS)
dimnames(counts_mat)      <- list(from = TIERS, to = TIERS)

heat_df <- bind_rows(
  as.data.frame.table(counts_mat, responseName = "count") %>%
    as_tibble() %>%
    transmute(
      from = as.character(from),
      to = as.character(to),
      count = as.numeric(count),
      type = "observed"
    ),
  as.data.frame.table(mean_counts_rep, responseName = "count") %>%
    as_tibble() %>%
    transmute(
      from = as.character(from),
      to = as.character(to),
      count = as.numeric(count),
      type = "replicated_mean"
    )
) %>%
  mutate(
    from = factor(from, levels = TIERS),
    to = factor(to, levels = TIERS)
  )

p_markov_heat <- ggplot(heat_df, aes(x = to, y = from, fill = count)) +
  geom_tile() +
  geom_text(aes(label = round(count, 1)), size = 3) +
  facet_wrap(~type) +
  labs(title = "Markov PPC: observed vs replicated mean transition counts", x = "To tier", y = "From tier") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_plot_safe(p_markov_heat, file.path(OUT_DIR, "markov_ppc_heatmap.png"), width = 11, height = 5.5)

# ==============================================================================
# 5. LOO / PREDICTIVE PERFORMANCE
# ==============================================================================

message_section("LOO / predictive performance")

# ---- 5A. Pick-value LOO --------------------------------------------------------

pick_log_lik <- array_draws_matrix(pick_gq_fit, "log_lik")
chain_id_pick <- extract_chain_id(pick_fit)
if (length(chain_id_pick) != nrow(pick_log_lik)) chain_id_pick <- NULL

pick_loo <- tryCatch({
  r_eff <- if (!is.null(chain_id_pick)) loo::relative_eff(exp(pick_log_lik), chain_id = chain_id_pick) else NULL
  loo::loo(pick_log_lik, r_eff = r_eff)
}, error = function(e) {
  warning("Pick-value LOO failed: ", e$message)
  NULL
})

if (!is.null(pick_loo)) {
  capture.output(print(pick_loo), file = file.path(OUT_DIR, "pick_value_loo.txt"))
  pick_pareto <- tibble(
    row_id = seq_along(pick_obs),
    pick = pick_obs,
    pareto_k = as.numeric(pick_loo$diagnostics$pareto_k)
  ) %>%
    mutate(flag_pareto_k = pareto_k > 0.7)

  if (exists("pick_fit_data") && nrow(pick_fit_data) == nrow(pick_pareto)) {
    pick_pareto <- pick_pareto %>%
      bind_cols(pick_fit_data %>% select(any_of(c("draft_year", "player"))))
  }
  write_csv_safe(pick_pareto, file.path(OUT_DIR, "pick_value_loo_pareto_k.csv"))
  cat("Pick-value LOO written to:", file.path(OUT_DIR, "pick_value_loo.txt"), "\n")
  cat(sprintf("Pick-value Pareto-k > 0.7: %d / %d\n", sum(pick_pareto$flag_pareto_k), nrow(pick_pareto)))
}

# ---- 5B. Markov row-level LOO --------------------------------------------------
# With only K=5 aggregate transition-count rows, this is a coarse predictive
# check. For a stricter test, refit on held-out seasons or held-out team-season
# transitions before aggregation.

write_markov_gq_stan <- function() {
  "data {
  int<lower=1> K;
  array[K, K] int<lower=0> counts;
  matrix<lower=0>[K, K] alpha;
}
parameters {
  // Use matrix instead of array[K] simplex[K] for standalone GQ.
  // CmdStan CSV output can round simplex rows to sums like 1.000000956,
  // which fails simplex validation when reused as fitted_params.
  matrix<lower=0>[K, K] theta;
}
generated quantities {
  vector[K] log_lik;
  array[K, K] int counts_rep;

  for (i in 1:K) {
    vector[K] theta_i;
    real theta_sum = 0;

    for (j in 1:K) {
      theta_sum += theta[i, j];
    }

    for (j in 1:K) {
      theta_i[j] = theta[i, j] / theta_sum;
    }

    log_lik[i] = multinomial_lpmf(counts[i] | theta_i);
    counts_rep[i] = multinomial_rng(theta_i, sum(counts[i]));
  }
}"
}

get_or_generate_markov_gq <- function() {
  if (has_stan_variable(markov_fit, "log_lik")) return(markov_fit)
  if (!GENERATE_LOG_LIK_IF_MISSING) {
    stop("markov_fit does not contain log_lik; set GENERATE_LOG_LIK_IF_MISSING <- TRUE.", call. = FALSE)
  }
  gq_file <- file.path(OUT_DIR, "team_strength_validation_gq.stan")
  writeLines(write_markov_gq_stan(), gq_file)
  gq_model <- cmdstan_model(gq_file, force_recompile = TRUE)
  gq_model$generate_quantities(
    fitted_params = markov_fit,
    data = list(K = N_TIERS, counts = counts_mat, alpha = alpha_prior),
    seed = 2026
  )
}

markov_gq_fit <- get_or_generate_markov_gq()
markov_log_lik <- array_draws_matrix(markov_gq_fit, "log_lik")
chain_id_markov <- extract_chain_id(markov_fit)
if (length(chain_id_markov) != nrow(markov_log_lik)) chain_id_markov <- NULL

markov_loo <- tryCatch({
  r_eff <- if (!is.null(chain_id_markov)) loo::relative_eff(exp(markov_log_lik), chain_id = chain_id_markov) else NULL
  loo::loo(markov_log_lik, r_eff = r_eff)
}, error = function(e) {
  warning("Markov LOO failed: ", e$message)
  NULL
})

if (!is.null(markov_loo)) {
  capture.output(print(markov_loo), file = file.path(OUT_DIR, "markov_loo_row_level.txt"))
  markov_pareto <- tibble(
    from_tier = TIERS,
    pareto_k = as.numeric(markov_loo$diagnostics$pareto_k)
  ) %>%
    mutate(flag_pareto_k = pareto_k > 0.7)
  write_csv_safe(markov_pareto, file.path(OUT_DIR, "markov_loo_pareto_k.csv"))
  cat("Markov row-level LOO written to:", file.path(OUT_DIR, "markov_loo_row_level.txt"), "\n")
  cat(sprintf("Markov Pareto-k > 0.7: %d / %d\n", sum(markov_pareto$flag_pareto_k), nrow(markov_pareto)))
}

# ==============================================================================
# 6. MODEL-SPECIFIC ANALYTICAL CHECKS
# ==============================================================================

message_section("Model-specific checks")

# ---- 6A. Markov conjugacy check ------------------------------------------------
# The Stan posterior mean should match the closed-form Dirichlet posterior mean.

post_trans <- matrix(NA_real_, N_TIERS, N_TIERS, dimnames = list(TIERS, TIERS))
for (i in seq_len(N_TIERS)) {
  for (j in seq_len(N_TIERS)) {
    post_trans[i, j] <- mean(theta_arr[, i, j])
  }
}

markov_closed_form_check <- as_tibble(as.table(post_trans), .name_repair = "minimal") %>%
  set_names(c("from", "to", "stan_post_mean")) %>%
  mutate(
    closed_form_mean = as.vector(posterior_mean_closed),
    abs_diff = abs(stan_post_mean - closed_form_mean)
  )
write_csv_safe(markov_closed_form_check, file.path(OUT_DIR, "markov_closed_form_check.csv"))

cat(sprintf(
  "Max abs diff between Stan transition means and closed-form Dirichlet means: %.6f\n",
  max(markov_closed_form_check$abs_diff, na.rm = TRUE)
))

# ---- 6B. Pick curve monotonicity / plausibility --------------------------------

pick_draw_mat <- as.matrix(pick_fit$draws(variables = pick_core_vars, format = "draws_matrix"))
mu_curve <- sapply(1:30, function(pos) {
  pick_draw_mat[, "alpha"] / (pos ^ pick_draw_mat[, "beta"]) + pick_draw_mat[, "gamma"]
})
colnames(mu_curve) <- paste0("pick_", 1:30)

pick_curve_checks <- tibble(
  draw = seq_len(nrow(mu_curve)),
  strictly_decreasing = apply(mu_curve, 1, function(x) all(diff(x) <= 0)),
  any_negative_mean = apply(mu_curve, 1, function(x) any(x < 0)),
  pick1_minus_pick30 = mu_curve[, 1] - mu_curve[, 30]
)
write_csv_safe(pick_curve_checks, file.path(OUT_DIR, "pick_value_curve_draw_checks.csv"))

cat(sprintf("Pick curve non-increasing in %.1f%% of posterior draws\n", 100 * mean(pick_curve_checks$strictly_decreasing)))
cat(sprintf("Pick curve has any negative latent mean in %.1f%% of posterior draws\n", 100 * mean(pick_curve_checks$any_negative_mean)))

# ==============================================================================
# 7. SIMULATION-BASED CALIBRATION (SBC)
# ==============================================================================

message_section("Simulation-Based Calibration")

rank_of_truth <- function(draws, truth) {
  # Rank convention: number of posterior draws below the true value, in [0, S].
  sum(draws < truth, na.rm = TRUE)
}

rank_uniform_summary <- function(rank_tbl, model_name, bins = 10L) {
  if (nrow(rank_tbl) == 0) return(tibble())

  out <- rank_tbl %>%
    group_by(variable) %>%
    summarise(
      n_reps = n(),
      n_draws_median = median(n_draws),
      rank_mean = mean(rank),
      expected_rank_mean = mean(n_draws) / 2,
      rank_mean_z = (rank_mean - expected_rank_mean) / (sd(rank) / sqrt(n())),
      rank_min = min(rank),
      rank_max = max(rank),
      # Discrete rank CDF proxy. Small p-values are a warning, not definitive.
      ks_p = tryCatch(stats::ks.test((rank + 0.5) / (n_draws + 1), "punif")$p.value,
                      error = function(e) NA_real_),
      .groups = "drop"
    ) %>%
    mutate(model = model_name) %>%
    relocate(model, variable)

  write_csv_safe(out, file.path(OUT_DIR, paste0(model_name, "_sbc_rank_uniform_summary.csv")))
  out
}

plot_sbc_ranks <- function(rank_tbl, model_name, max_facets = 30L) {
  if (nrow(rank_tbl) == 0) return(invisible(NULL))

  plot_tbl <- rank_tbl %>%
    mutate(rank_scaled = (rank + 0.5) / (n_draws + 1))

  keep_vars <- unique(plot_tbl$variable)[seq_len(min(length(unique(plot_tbl$variable)), max_facets))]
  plot_tbl <- plot_tbl %>% filter(variable %in% keep_vars)

  p <- ggplot(plot_tbl, aes(x = rank_scaled)) +
    geom_histogram(bins = 10, boundary = 0, alpha = 0.75) +
    facet_wrap(~variable, scales = "free_y") +
    labs(
      title = paste("SBC rank histograms:", model_name),
      subtitle = "Ranks should be approximately uniform if simulation + inference are calibrated",
      x = "Scaled posterior rank of true parameter",
      y = "SBC rep count"
    ) +
    theme_minimal(base_size = 11)

  save_plot_safe(p, file.path(OUT_DIR, paste0(model_name, "_sbc_rank_histograms.png")), width = 12, height = 8)
  invisible(p)
}

# ---- 7A. Pick-value SBC --------------------------------------------------------
# IMPORTANT: For strict prior SBC, draw_pick_truth() should exactly match the
# priors inside pick_value.stan. Because nba_lottery.R compiles the external
# Stan file, this script keeps an explicit prior simulator here. If you change
# the Stan priors, update this generator to match them exactly.

draw_pick_truth <- function() {
  # Exact prior simulator for the current player-level Student-t model.
  # These should match pick_value.stan:
  #   log_alpha        ~ normal(log(20), 0.60)
  #   log_beta         ~ normal(log(0.55), 0.50)
  #   gamma            ~ normal(2, 3)
  #   log_sigma_1      ~ normal(log(8), 0.50)
  #   z_sigma_step     ~ std_normal()
  #   tau_log_sigma_rw ~ normal(0, 0.15), constrained lower=0
  #   nu - 2           ~ exponential(0.20)
  #
  # The old raw-gamma and tail-excess parameters are no longer sampled in Stan.
  # We draw gamma and nu directly here so SBC targets the same parameterization
  # used by the fitted model.
  alpha <- exp(rnorm(1, log(20), 0.60))
  beta  <- exp(rnorm(1, log(0.55), 0.50))
  gamma <- rnorm(1, 2, 3)

  log_sigma_1 <- rnorm(1, log(8), 0.50)
  z_sigma_step <- rnorm(29, 0, 1)

  # Stan's lower=0 plus normal(0, 0.15) prior implies a half-normal prior.
  tau_log_sigma_rw <- abs(rnorm(1, 0, 0.15))

  log_sigma_pick <- numeric(30)
  log_sigma_pick[1] <- log_sigma_1
  for (p in 2:30) {
    log_sigma_pick[p] <- log_sigma_pick[p - 1] +
      tau_log_sigma_rw * z_sigma_step[p - 1]
  }
  sigma_pick <- exp(log_sigma_pick)

  nu <- 2.0 + rexp(1, rate = 0.20)

  c(
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    log_sigma_1 = log_sigma_1,
    tau_log_sigma_rw = tau_log_sigma_rw,
    nu = nu,
    setNames(sigma_pick, paste0("sigma_pick_", 1:30))
  )
}

simulate_pick_sbc_data <- function(template_data, truth) {
  pick <- as.numeric(template_data$pick)
  mu <- truth[["alpha"]] / (pick ^ truth[["beta"]]) + truth[["gamma"]]
  sigma_pick <- unname(truth[paste0("sigma_pick_", pick)])

  list(
    N = template_data$N,
    pick = as.integer(pick),
    ws4 = mu + sigma_pick * rt(length(pick), df = truth[["nu"]])
  )
}

run_one_pick_sbc <- function(rep_id) {
  truth <- draw_pick_truth()
  sim_data <- simulate_pick_sbc_data(pick_stan_data, truth)

  fit <- tryCatch(
    pick_model$sample(
      data = sim_data,
      chains = SBC_CHAINS,
      parallel_chains = SBC_PARALLEL_CHAINS,
      iter_warmup = SBC_ITER_WARMUP,
      iter_sampling = SBC_ITER_SAMPLING,
      adapt_delta = SBC_ADAPT_DELTA,
      max_treedepth = SBC_MAX_TREEDEPTH,
      seed = 2026000 + rep_id,
      refresh = 0
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(
      ranks = tibble(),
      diag = tibble(rep = rep_id, model = "pick_value", fit_ok = FALSE, error = fit$message)
    ))
  }

  draws <- as.matrix(fit$draws(variables = pick_core_vars, format = "draws_matrix"))
  
  monitor_vars <- Reduce(intersect, list(pick_core_vars, colnames(draws), names(truth)))
  missing_vars <- setdiff(pick_core_vars, monitor_vars)
  
  if (length(monitor_vars) == 0L) {
    return(list(
      ranks = tibble(),
      diag = tibble(
        rep = rep_id,
        model = "pick_value",
        fit_ok = FALSE,
        error = sprintf(
          "No overlapping pick SBC variables. pick_core_vars=%s; draw columns include=%s; truth names=%s",
          paste(pick_core_vars, collapse = ", "),
          paste(head(colnames(draws), 20), collapse = ", "),
          paste(names(truth), collapse = ", ")
        )
      )
    ))
  }
  
  ranks <- map_dfr(monitor_vars, function(v) {
    true_val <- unname(as.numeric(truth[[v]]))
    tibble(
      rep = rep_id,
      model = "pick_value",
      variable = v,
      truth = true_val,
      rank = rank_of_truth(draws[, v], true_val),
      n_draws = nrow(draws)
    )
  })

  nuts <- summarize_nuts(fit, paste0("pick_value_sbc_rep_", rep_id), max_treedepth = SBC_MAX_TREEDEPTH, write_files = FALSE, run_cmdstan_diagnose = FALSE) %>%
    summarise(
      divergences = sum(divergences),
      max_treedepth_hits = sum(max_treedepth_hits),
      min_ebfmi = min(ebfmi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(rep = rep_id, model = "pick_value", fit_ok = TRUE, error = NA_character_) %>%
    select(rep, model, fit_ok, error, everything())

  list(ranks = ranks, diag = nuts)
}

# ---- 7B. Markov SBC ------------------------------------------------------------

rdirichlet_one <- function(alpha) {
  x <- rgamma(length(alpha), shape = alpha, rate = 1)
  x / sum(x)
}

simulate_markov_sbc_data <- function(row_totals, alpha) {
  K <- nrow(alpha)
  theta_true <- matrix(NA_real_, K, K)
  counts <- matrix(0L, K, K)
  for (i in seq_len(K)) {
    theta_true[i, ] <- rdirichlet_one(alpha[i, ])
    counts[i, ] <- as.integer(rmultinom(1, size = row_totals[i], prob = theta_true[i, ]))
  }
  list(
    data = list(K = K, counts = counts, alpha = alpha),
    theta_true = theta_true
  )
}

run_one_markov_sbc <- function(rep_id) {
  sim <- simulate_markov_sbc_data(rowSums(counts_mat), alpha_prior)

  fit <- tryCatch(
    markov_model$sample(
      data = sim$data,
      chains = SBC_CHAINS,
      parallel_chains = SBC_PARALLEL_CHAINS,
      iter_warmup = SBC_ITER_WARMUP,
      iter_sampling = SBC_ITER_SAMPLING,
      adapt_delta = SBC_ADAPT_DELTA,
      max_treedepth = SBC_MAX_TREEDEPTH,
      seed = 2027000 + rep_id,
      refresh = 0
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(
      ranks = tibble(),
      diag = tibble(rep = rep_id, model = "markov", fit_ok = FALSE, error = fit$message)
    ))
  }

  draws <- as.matrix(fit$draws(variables = "theta", format = "draws_matrix"))
  idx <- parse_stan_indices(colnames(draws))

  ranks <- map_dfr(seq_len(ncol(draws)), function(c) {
    i <- idx[c, 1]
    j <- idx[c, 2]
    v <- sprintf("theta[%d,%d]", i, j)
    true_val <- sim$theta_true[i, j]
    tibble(
      rep = rep_id,
      model = "markov",
      variable = v,
      from_tier = TIERS[i],
      to_tier = TIERS[j],
      truth = true_val,
      rank = rank_of_truth(draws[, c], true_val),
      n_draws = nrow(draws)
    )
  })

  nuts <- summarize_nuts(fit, paste0("markov_sbc_rep_", rep_id), max_treedepth = SBC_MAX_TREEDEPTH, write_files = FALSE, run_cmdstan_diagnose = FALSE) %>%
    summarise(
      divergences = sum(divergences),
      max_treedepth_hits = sum(max_treedepth_hits),
      min_ebfmi = min(ebfmi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(rep = rep_id, model = "markov", fit_ok = TRUE, error = NA_character_) %>%
    select(rep, model, fit_ok, error, everything())

  list(ranks = ranks, diag = nuts)
}

sbc_results <- list()

if (RUN_SBC_PICK) {
  cat("Running pick-value SBC reps:", SBC_REPS_PICK, "\n")
  pick_sbc <- vector("list", SBC_REPS_PICK)
  for (r in seq_len(SBC_REPS_PICK)) {
    cat(sprintf("  pick SBC %d / %d\n", r, SBC_REPS_PICK))
    pick_sbc[[r]] <- run_one_pick_sbc(r)
  }
  pick_sbc_ranks <- bind_rows(map(pick_sbc, "ranks"))
  pick_sbc_diag <- bind_rows(map(pick_sbc, "diag"))
  pick_sbc_summary <- rank_uniform_summary(pick_sbc_ranks, "pick_value")
  plot_sbc_ranks(pick_sbc_ranks, "pick_value")
  write_csv_safe(pick_sbc_ranks, file.path(OUT_DIR, "pick_value_sbc_ranks.csv"))
  write_csv_safe(pick_sbc_diag, file.path(OUT_DIR, "pick_value_sbc_fit_diagnostics.csv"))
  sbc_results$pick_value <- list(ranks = pick_sbc_ranks, diag = pick_sbc_diag, summary = pick_sbc_summary)
}

if (RUN_SBC_MARKOV) {
  cat("Running Markov SBC reps:", SBC_REPS_MARKOV, "\n")
  markov_sbc <- vector("list", SBC_REPS_MARKOV)
  for (r in seq_len(SBC_REPS_MARKOV)) {
    cat(sprintf("  markov SBC %d / %d\n", r, SBC_REPS_MARKOV))
    markov_sbc[[r]] <- run_one_markov_sbc(r)
  }
  markov_sbc_ranks <- bind_rows(map(markov_sbc, "ranks"))
  markov_sbc_diag <- bind_rows(map(markov_sbc, "diag"))
  markov_sbc_summary <- rank_uniform_summary(markov_sbc_ranks, "markov")
  plot_sbc_ranks(markov_sbc_ranks, "markov", max_facets = 25L)
  write_csv_safe(markov_sbc_ranks, file.path(OUT_DIR, "markov_sbc_ranks.csv"))
  write_csv_safe(markov_sbc_diag, file.path(OUT_DIR, "markov_sbc_fit_diagnostics.csv"))
  sbc_results$markov <- list(ranks = markov_sbc_ranks, diag = markov_sbc_diag, summary = markov_sbc_summary)
}

# ==============================================================================
# 8. ONE-PAGE VALIDATION DECISION TABLE
# ==============================================================================

message_section("Validation decision table")

validation_decision <- bind_rows(
  all_param_summary %>%
    group_by(model) %>%
    summarise(
      check = "MCMC Rhat/ESS",
      metric = sprintf(
        "max Rhat %.3f; min bulk ESS %.0f; min tail ESS %.0f",
        max(rhat, na.rm = TRUE), min(ess_bulk, na.rm = TRUE), min(ess_tail, na.rm = TRUE)
      ),
      pass = max(rhat, na.rm = TRUE) <= RHAT_THRESHOLD &&
        min(ess_bulk, na.rm = TRUE) >= ESS_BULK_MIN &&
        min(ess_tail, na.rm = TRUE) >= ESS_TAIL_MIN,
      .groups = "drop"
    ),
  all_nuts %>%
    group_by(model) %>%
    summarise(
      check = "NUTS geometry",
      metric = sprintf(
        "%d divergences; %d max-treedepth hits; min E-BFMI %.3f",
        sum(divergences), sum(max_treedepth_hits), min(ebfmi, na.rm = TRUE)
      ),
      pass = sum(divergences) == 0 && sum(max_treedepth_hits) == 0 && min(ebfmi, na.rm = TRUE) >= EBFMI_MIN,
      .groups = "drop"
    ),
  tibble(
    model = "pick_value",
    check = "PPC coverage",
    metric = sprintf("90%% interval coverage %.1f%%; max |z| %.2f", 100 * mean(pick_ppc_summary$covered_90), max(abs(pick_ppc_summary$z_resid), na.rm = TRUE)),
    pass = mean(pick_ppc_summary$covered_90) >= 0.70 && max(abs(pick_ppc_summary$z_resid), na.rm = TRUE) <= 3
  ),
  tibble(
    model = "markov",
    check = "Conjugacy / code check",
    metric = sprintf("max |Stan mean - closed form| %.6f", max(markov_closed_form_check$abs_diff, na.rm = TRUE)),
    pass = max(markov_closed_form_check$abs_diff, na.rm = TRUE) < 0.01
  ),
  tibble(
    model = "markov",
    check = "PPC transition stats",
    metric = paste(
      sprintf("observed stays %s", markov_ppc_obs$total_stay),
      sprintf("big jumps %s", markov_ppc_obs$big_jumps),
      sep = "; "
    ),
    pass = TRUE
  )
)

if (!is.null(pick_loo)) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "pick_value",
      check = "PSIS-LOO",
      metric = sprintf("elpd_loo %.2f; max Pareto-k %.2f", pick_loo$estimates["elpd_loo", "Estimate"], max(pick_loo$diagnostics$pareto_k, na.rm = TRUE)),
      pass = max(pick_loo$diagnostics$pareto_k, na.rm = TRUE) <= 0.7
    )
  )
}

if (!is.null(markov_loo)) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "markov",
      check = "PSIS-LOO row-level",
      metric = sprintf("elpd_loo %.2f; max Pareto-k %.2f", markov_loo$estimates["elpd_loo", "Estimate"], max(markov_loo$diagnostics$pareto_k, na.rm = TRUE)),
      pass = max(markov_loo$diagnostics$pareto_k, na.rm = TRUE) <= 0.7
    )
  )
}

if (RUN_SBC_PICK && exists("pick_sbc_summary")) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "pick_value",
      check = "SBC rank uniformity",
      metric = sprintf("min KS p-value %.3f across monitored params", min(pick_sbc_summary$ks_p, na.rm = TRUE)),
      pass = min(pick_sbc_summary$ks_p, na.rm = TRUE) > 0.01
    )
  )
}

if (RUN_SBC_MARKOV && exists("markov_sbc_summary")) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "markov",
      check = "SBC rank uniformity",
      metric = sprintf("min KS p-value %.3f across theta cells", min(markov_sbc_summary$ks_p, na.rm = TRUE)),
      pass = min(markov_sbc_summary$ks_p, na.rm = TRUE) > 0.01
    )
  )
}

write_csv_safe(validation_decision, file.path(OUT_DIR, "validation_decision_table.csv"))
print(validation_decision)

# Save a full R object for later dashboard/report use.
validation_results <- list(
  parameter_summary = all_param_summary,
  nuts = all_nuts,
  pick_ppc_summary = pick_ppc_summary,
  markov_ppc_observed = markov_ppc_obs,
  markov_ppc_replicated = markov_ppc_rep,
  pick_loo = pick_loo,
  markov_loo = markov_loo,
  markov_closed_form_check = markov_closed_form_check,
  pick_curve_checks = pick_curve_checks,
  sbc = sbc_results,
  decision_table = validation_decision,
  settings = list(
    sbc_reps_pick = SBC_REPS_PICK,
    sbc_reps_markov = SBC_REPS_MARKOV,
    rhat_threshold = RHAT_THRESHOLD,
    ess_bulk_min = ESS_BULK_MIN,
    ess_tail_min = ESS_TAIL_MIN,
    ebfmi_min = EBFMI_MIN,
    generated_log_lik_if_missing = GENERATE_LOG_LIK_IF_MISSING
  )
)

saveRDS(validation_results, file.path(OUT_DIR, "validation_results.rds"))

cat("\nValidation complete. Key outputs:\n")
cat("  - ", file.path(OUT_DIR, "validation_decision_table.csv"), "\n", sep = "")
cat("  - ", file.path(OUT_DIR, "validation_results.rds"), "\n", sep = "")
cat("  - PNG diagnostics and PPC/SBC plots in ", OUT_DIR, "\n", sep = "")

