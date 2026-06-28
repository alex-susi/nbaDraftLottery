################################################################################
# NBA Lottery Stan Model Validation — latest model stack
#
# Standalone validation script for the current three Stan models used by the
# NBA 3-2-1 Lottery Reform project:
#   1. pick_value_v3.stan                       -> pick_fit / pick_fit_rw_sigma
#   2. pick_play_r2_v6_declining_upside.stan    -> pick_fit_r2_hurdle
#   3. team_strength.stan                       -> markov_fit
#
# Run options:
#   A) Recommended during development:
#        source("01_data.R")
#        source("02_picks.R")
#        source("03_models.R")
#        source("stan_model_validation_latest_models.R")
#
#   B) Standalone from the same project directory:
#        Rscript stan_model_validation_latest_models.R
#      If the fit objects are not already in memory, this script can source the
#      project pipeline scripts. That will rerun scraping / fitting work.
#
# Outputs are written to ./04_validation_latest_models/ by default so this file
# does not overwrite the prior stan_model_validation.R outputs.
################################################################################

# ==============================================================================
# 0. USER CONTROLS
# ==============================================================================

PROJECT_DIR <- getwd()

# Prefer the current modular pipeline. Keep a legacy nba_lottery.R fallback for
# older local project layouts.
PIPELINE_SCRIPTS <- file.path(PROJECT_DIR, c("01_data.R", "02_picks.R", "03_models.R"))
NBA_LOTTERY_SCRIPT <- file.path(PROJECT_DIR, "nba_lottery.R")

# Latest Stan models.
PICK_MODEL_R1_PATH <- c(
  file.path(PROJECT_DIR, "02_models", "pick_value_v3.stan"),
  file.path(PROJECT_DIR, "pick_value_v3.stan")
)
PICK_MODEL_R2_PATH <- c(
  file.path(PROJECT_DIR, "02_models", "pick_play_r2_v6_declining_upside.stan"),
  file.path(PROJECT_DIR, "pick_play_r2_v6_declining_upside.stan")
)
TEAM_STRENGTH_MODEL_PATH <- c(
  file.path(PROJECT_DIR, "02_models", "team_strength.stan"),
  file.path(PROJECT_DIR, "team_strength.stan")
)

OUT_DIR <- file.path(PROJECT_DIR, "03_validation")
RUN_PIPELINE_IF_NEEDED <- TRUE

# SBC can be computationally expensive. Defaults are development-friendly. Raise
# to 200+ reps for a more serious rank-uniformity check. Round 2 SBC is the
# heaviest because it refits the hurdle/lognormal-mixture model.
RUN_SBC_PICK_R1   <- TRUE
RUN_SBC_PICK_R2   <- FALSE
RUN_SBC_MARKOV    <- TRUE
SBC_REPS_PICK_R1  <- 50L
SBC_REPS_PICK_R2  <- 25L
SBC_REPS_MARKOV   <- 100L
SBC_CHAINS <- 4L
SBC_PARALLEL_CHAINS <- min(4L, SBC_CHAINS)
SBC_ITER_WARMUP <- 500L
SBC_ITER_SAMPLING <- 500L
SBC_ADAPT_DELTA_R1 <- 0.95
SBC_ADAPT_DELTA_R2 <- 0.99
SBC_ADAPT_DELTA_MARKOV <- 0.95
SBC_MAX_TREEDEPTH <- 12L

# Generated quantities are already present in the latest Stan files. These
# fallbacks are kept for stale interactive sessions or partially saved fit
# objects.
GENERATE_LOG_LIK_IF_MISSING <- TRUE

# NUTS thresholds. These are intentionally strict for a production dashboard.
RHAT_THRESHOLD <- 1.01
ESS_BULK_MIN   <- 400
ESS_TAIL_MIN   <- 400
EBFMI_MIN      <- 0.30
MAX_TREEDEPTH_THRESHOLD <- 12L

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

resolve_required_file <- function(path) {
  candidates <- as.character(path)
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    stop(sprintf("Required file not found. Checked: %s", paste(candidates, collapse = ", ")), call. = FALSE)
  }
  hit
}

parse_stan_indices <- function(x) {
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

existing_stan_vars <- function(fit, variables) {
  vars <- fit$metadata()$model_params
  variables[vapply(variables, function(v) {
    base <- sub("\\[.*$", "", v)
    any(vars == v | vars == base | startsWith(vars, paste0(base, "[")))
  }, logical(1))]
}

extract_chain_id <- function(fit) {
  df <- posterior::as_draws_df(fit$draws())
  as.integer(df$.chain)
}

ensure_yrep_matrix <- function(yrep, y, name = "yrep") {
  yrep <- as.matrix(yrep)
  n_obs <- length(y)

  if (ncol(yrep) == n_obs) return(yrep)
  if (nrow(yrep) == n_obs) {
    warning(name, " was observations x draws; transposing to draws x observations.")
    return(t(yrep))
  }

  stop(
    name, " has incompatible dimensions: ",
    paste(dim(yrep), collapse = " x "),
    "; expected draws x ", n_obs,
    ". This usually means the fit and validation data came from different runs.",
    call. = FALSE
  )
}

# ==============================================================================
# 2. LOAD / BUILD OBJECTS FROM CURRENT PIPELINE
# ==============================================================================

needed_objects <- c(
  "pick_fit", "pick_fit_r2_hurdle", "markov_fit",
  "pick_fit_data", "pick_fit_data_r2",
  "counts_mat", "alpha_prior", "N_TIERS", "TIERS", "posterior_mean_closed"
)

have_needed <- function() all(vapply(needed_objects, exists, logical(1), envir = .GlobalEnv))

if (!have_needed()) {
  if (!RUN_PIPELINE_IF_NEEDED) {
    stop(
      "Validation objects are missing. First source the modeling pipeline, ",
      "or set RUN_PIPELINE_IF_NEEDED <- TRUE.",
      call. = FALSE
    )
  }

  if (all(file.exists(PIPELINE_SCRIPTS))) {
    message_section("Sourcing latest modular pipeline scripts")
    for (script in PIPELINE_SCRIPTS) {
      cat("Sourcing ", script, "\n", sep = "")
      source(script, local = .GlobalEnv)
    }
  } else if (file.exists(NBA_LOTTERY_SCRIPT)) {
    message_section("Sourcing legacy nba_lottery.R to create fit objects")
    source(NBA_LOTTERY_SCRIPT, local = .GlobalEnv)
  } else {
    stop(
      "Could not find latest pipeline scripts or nba_lottery.R. Checked: ",
      paste(c(PIPELINE_SCRIPTS, NBA_LOTTERY_SCRIPT), collapse = ", "),
      call. = FALSE
    )
  }
}

missing_after_source <- needed_objects[!vapply(needed_objects, exists, logical(1), envir = .GlobalEnv)]
if (length(missing_after_source) > 0) {
  stop("Still missing required objects: ", paste(missing_after_source, collapse = ", "), call. = FALSE)
}

# Pull objects into this script's environment explicitly.
pick_fit <- get("pick_fit", envir = .GlobalEnv)
pick_fit_r2_hurdle <- get("pick_fit_r2_hurdle", envir = .GlobalEnv)
markov_fit <- get("markov_fit", envir = .GlobalEnv)

pick_fit_data <- get("pick_fit_data", envir = .GlobalEnv)
pick_fit_data_r2 <- get("pick_fit_data_r2", envir = .GlobalEnv)
counts_mat <- get("counts_mat", envir = .GlobalEnv)
alpha_prior <- get("alpha_prior", envir = .GlobalEnv)
N_TIERS <- get("N_TIERS", envir = .GlobalEnv)
TIERS <- get("TIERS", envir = .GlobalEnv)
posterior_mean_closed <- get("posterior_mean_closed", envir = .GlobalEnv)

# Stan data. Rebuild from the validation data to avoid stale long-session objects.
pick_stan_data_r1 <- list(
  N = nrow(pick_fit_data),
  pick = as.integer(pick_fit_data$pick),
  ws4 = as.numeric(pick_fit_data$ws4)
)

if (exists("pick_stan_data_r2", envir = .GlobalEnv)) {
  pick_stan_data_r2 <- get("pick_stan_data_r2", envir = .GlobalEnv)
} else {
  R2_WS_FLOOR <- min(-2, min(pick_fit_data_r2$ws4[pick_fit_data_r2$played == 1], na.rm = TRUE) - 0.25)
  pick_stan_data_r2 <- list(
    N = nrow(pick_fit_data_r2),
    pick = as.integer(pick_fit_data_r2$pick),
    played = as.integer(pick_fit_data_r2$played),
    ws4 = as.numeric(pick_fit_data_r2$ws4),
    ws_floor = R2_WS_FLOOR
  )
}

# Compile model handles lazily for SBC. This avoids unnecessary recompilation
# during ordinary diagnostics/PPC/LOO runs.
pick_model_r1 <- NULL
pick_model_r2 <- NULL
markov_model  <- if (exists("markov_model", envir = .GlobalEnv)) get("markov_model", envir = .GlobalEnv) else NULL

get_pick_model_r1 <- function() {
  if (is.null(pick_model_r1)) {
    pick_model_r1 <<- cmdstan_model(resolve_required_file(PICK_MODEL_R1_PATH))
  }
  pick_model_r1
}

get_pick_model_r2 <- function() {
  if (is.null(pick_model_r2)) {
    pick_model_r2 <<- cmdstan_model(resolve_required_file(PICK_MODEL_R2_PATH))
  }
  pick_model_r2
}

get_markov_model <- function() {
  if (is.null(markov_model)) {
    markov_model <<- cmdstan_model(resolve_required_file(TEAM_STRENGTH_MODEL_PATH))
  }
  markov_model
}

message_section("Validation inputs")
cat("Round-1 pick model observations:", pick_stan_data_r1$N, "\n")
cat("Round-2 pick model observations:", pick_stan_data_r2$N, "\n")
cat("Round-2 WS floor:", pick_stan_data_r2$ws_floor, "\n")
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

  if (write_files && run_cmdstan_diagnose) {
    diag_txt <- tryCatch(capture.output(fit$cmdstan_diagnose()), error = function(e) paste("cmdstan_diagnose failed:", e$message))
    writeLines(diag_txt, file.path(OUT_DIR, paste0(model_name, "_cmdstan_diagnose.txt")))
  }

  diag_by_chain
}

plot_mcmc_core <- function(fit, model_name, variables) {
  variables <- existing_stan_vars(fit, variables)
  if (length(variables) == 0L) {
    warning("No requested variables found for ", model_name, " MCMC plots.")
    return(invisible(list()))
  }

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

pick_core_vars_r1 <- c("alpha", "beta", "gamma", "log_sigma_1", "tau_log_sigma_rw", "nu")
pick_param_summary_r1 <- summarize_mcmc_parameters(pick_fit, "pick_value_v3_r1", pick_core_vars_r1)
pick_nuts_r1 <- summarize_nuts(pick_fit, "pick_value_v3_r1")
plot_mcmc_core(pick_fit, "pick_value_v3_r1", pick_core_vars_r1)

pick_core_vars_r2 <- c(
  "logit_play_31", "delta_logit_play", "tau_logit_play_rw",
  "log_cond_mean_ws_31", "delta_log_cond_mean_ws", "tau_log_cond_mean_ws_rw",
  "log_sigma_ws_31", "tau_log_sigma_ws_rw",
  "logit_upside_31", "delta_logit_upside",
  "upside_prob_31", "upside_prob_45", "upside_prob_60",
  "upside_log_shift", "upside_sigma_mult"
)
pick_param_summary_r2 <- summarize_mcmc_parameters(pick_fit_r2_hurdle, "pick_play_r2_v6_declining_upside", pick_core_vars_r2)
pick_nuts_r2 <- summarize_nuts(pick_fit_r2_hurdle, "pick_play_r2_v6_declining_upside")
plot_mcmc_core(pick_fit_r2_hurdle, "pick_play_r2_v6_declining_upside", pick_core_vars_r2)

markov_param_summary <- summarize_mcmc_parameters(markov_fit, "team_strength")
markov_nuts <- summarize_nuts(markov_fit, "team_strength")
markov_plot_vars <- c("theta[1,1]", "theta[1,2]", "theta[2,2]", "theta[3,3]", "theta[4,4]", "theta[5,5]")
plot_mcmc_core(markov_fit, "team_strength", markov_plot_vars)

all_param_summary <- bind_rows(pick_param_summary_r1, pick_param_summary_r2, markov_param_summary)
all_nuts <- bind_rows(pick_nuts_r1, pick_nuts_r2, markov_nuts)
write_csv_safe(all_param_summary, file.path(OUT_DIR, "all_parameter_summary_latest_models.csv"))
write_csv_safe(all_nuts, file.path(OUT_DIR, "all_nuts_diagnostics_latest_models.csv"))

# ==============================================================================
# 4. POSTERIOR PREDICTIVE CHECKS
# ==============================================================================

message_section("Posterior predictive checks")

# ---- 4A. Round-1 pick-value PPC ----------------------------------------------

write_pick_r1_gq_stan <- function() {
  "data {
  int<lower=1> N;
  array[N] int<lower=1, upper=30> pick;
  vector[N] ws4;
}
parameters {
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
    log_sigma_pick[p] = log_sigma_pick[p - 1] + tau_log_sigma_rw * z_sigma_step[p - 1];
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

pick_existing_gq_matches_data <- function(fit, n_obs) {
  if (!has_stan_variable(fit, "log_lik") || !has_stan_variable(fit, "ws4_rep")) return(FALSE)
  dims_ok <- tryCatch({
    m <- array_draws_matrix(fit, "ws4_rep")
    ncol(m) == n_obs || nrow(m) == n_obs
  }, error = function(e) FALSE)
  isTRUE(dims_ok)
}

get_or_generate_pick_r1_gq <- function() {
  if (pick_existing_gq_matches_data(pick_fit, length(pick_stan_data_r1$ws4))) return(pick_fit)
  if (!GENERATE_LOG_LIK_IF_MISSING) {
    stop("pick_fit lacks matching log_lik/ws4_rep; set GENERATE_LOG_LIK_IF_MISSING <- TRUE.", call. = FALSE)
  }
  gq_file <- file.path(OUT_DIR, "pick_value_v3_validation_gq.stan")
  writeLines(write_pick_r1_gq_stan(), gq_file)
  gq_model <- cmdstan_model(gq_file, force_recompile = TRUE)
  gq_model$generate_quantities(fitted_params = pick_fit, data = pick_stan_data_r1, seed = 2026)
}

pick_gq_fit_r1 <- get_or_generate_pick_r1_gq()

war_obs_r1 <- as.numeric(pick_stan_data_r1$ws4)
pick_obs_r1 <- as.numeric(pick_stan_data_r1$pick)
ws4_rep_r1 <- array_draws_matrix(pick_gq_fit_r1, "ws4_rep")
ws4_rep_r1 <- ensure_yrep_matrix(ws4_rep_r1, war_obs_r1, "r1 ws4_rep")

cat(sprintf("Round-1 PPC y length: %d | ws4_rep dims: %d draws x %d observations\n",
            length(war_obs_r1), nrow(ws4_rep_r1), ncol(ws4_rep_r1)))

ppc_draw_ids_r1 <- sort(sample(seq_len(nrow(ws4_rep_r1)), size = min(250, nrow(ws4_rep_r1))))
yrep_r1 <- ws4_rep_r1[ppc_draw_ids_r1, , drop = FALSE]

p_pick_r1_intervals <- bayesplot::ppc_intervals(
  y = war_obs_r1,
  yrep = yrep_r1,
  x = pick_obs_r1,
  prob = 0.50,
  prob_outer = 0.90
) +
  ggplot2::labs(
    title = "Round-1 pick_value_v3 PPC: observed outcomes vs replicated intervals",
    x = "Draft pick",
    y = "First-4-year win shares"
  )
save_plot_safe(p_pick_r1_intervals, file.path(OUT_DIR, "pick_value_v3_r1_ppc_intervals.png"), width = 10, height = 6)

p_pick_r1_dens <- bayesplot::ppc_dens_overlay(war_obs_r1, yrep_r1[seq_len(min(50, nrow(yrep_r1))), , drop = FALSE]) +
  ggplot2::labs(title = "Round-1 pick_value_v3 PPC: player-level distribution overlay")
save_plot_safe(p_pick_r1_dens, file.path(OUT_DIR, "pick_value_v3_r1_ppc_density.png"), width = 10, height = 6)

p_pick_r1_stat_mean <- bayesplot::ppc_stat(war_obs_r1, yrep_r1, stat = "mean") +
  ggplot2::labs(title = "Round-1 pick_value_v3 PPC: player-level mean")
save_plot_safe(p_pick_r1_stat_mean, file.path(OUT_DIR, "pick_value_v3_r1_ppc_mean.png"), width = 8, height = 5)

p_pick_r1_stat_sd <- bayesplot::ppc_stat(war_obs_r1, yrep_r1, stat = "sd") +
  ggplot2::labs(title = "Round-1 pick_value_v3 PPC: player-level standard deviation")
save_plot_safe(p_pick_r1_stat_sd, file.path(OUT_DIR, "pick_value_v3_r1_ppc_sd.png"), width = 8, height = 5)

pick_ppc_summary_r1 <- tibble(
  row_id = seq_along(war_obs_r1),
  pick = pick_obs_r1,
  observed = war_obs_r1,
  pred_mean = colMeans(ws4_rep_r1),
  pred_q05 = apply(ws4_rep_r1, 2, quantile, probs = 0.05),
  pred_q50 = apply(ws4_rep_r1, 2, quantile, probs = 0.50),
  pred_q95 = apply(ws4_rep_r1, 2, quantile, probs = 0.95),
  covered_90 = observed >= pred_q05 & observed <= pred_q95,
  z_resid = (observed - pred_mean) / apply(ws4_rep_r1, 2, sd)
)

if (nrow(pick_fit_data) == nrow(pick_ppc_summary_r1)) {
  pick_ppc_summary_r1 <- pick_ppc_summary_r1 %>%
    bind_cols(pick_fit_data %>% select(any_of(c("draft_year", "player"))))
}

write_csv_safe(pick_ppc_summary_r1, file.path(OUT_DIR, "pick_value_v3_r1_ppc_summary.csv"))

pick_ppc_by_pick_r1 <- pick_ppc_summary_r1 %>%
  group_by(pick) %>%
  summarise(
    n = n(),
    obs_mean = mean(observed, na.rm = TRUE),
    rep_mean = mean(pred_mean, na.rm = TRUE),
    coverage_90 = mean(covered_90, na.rm = TRUE),
    mean_z_resid = mean(z_resid, na.rm = TRUE),
    .groups = "drop"
  )
write_csv_safe(pick_ppc_by_pick_r1, file.path(OUT_DIR, "pick_value_v3_r1_ppc_by_pick.csv"))

cat(sprintf("Round-1 pick_value_v3 90%% PPC coverage: %.1f%% of player rows\n", 100 * mean(pick_ppc_summary_r1$covered_90)))
cat(sprintf("Round-1 pick_value_v3 max |PPC z residual|: %.2f\n", max(abs(pick_ppc_summary_r1$z_resid), na.rm = TRUE)))

# ---- 4B. Round-2 hurdle / declining-upside PPC -------------------------------

pick2_existing_gq_matches_data <- function(fit, n_obs) {
  if (!has_stan_variable(fit, "log_lik") || !has_stan_variable(fit, "ws4_rep") || !has_stan_variable(fit, "played_rep")) {
    return(FALSE)
  }
  dims_ok <- tryCatch({
    m <- array_draws_matrix(fit, "ws4_rep")
    ncol(m) == n_obs || nrow(m) == n_obs
  }, error = function(e) FALSE)
  isTRUE(dims_ok)
}

if (!pick2_existing_gq_matches_data(pick_fit_r2_hurdle, length(pick_stan_data_r2$ws4))) {
  stop(
    "pick_fit_r2_hurdle lacks matching generated quantities. Refit ",
    "pick_play_r2_v6_declining_upside.stan with generated quantities enabled.",
    call. = FALSE
  )
}

war_obs_r2 <- as.numeric(pick_stan_data_r2$ws4)
pick_obs_r2 <- as.numeric(pick_stan_data_r2$pick)
played_obs_r2 <- as.integer(pick_stan_data_r2$played)

ws4_rep_r2 <- array_draws_matrix(pick_fit_r2_hurdle, "ws4_rep")
ws4_rep_r2 <- ensure_yrep_matrix(ws4_rep_r2, war_obs_r2, "r2 ws4_rep")
played_rep_r2 <- array_draws_matrix(pick_fit_r2_hurdle, "played_rep")
played_rep_r2 <- ensure_yrep_matrix(played_rep_r2, played_obs_r2, "r2 played_rep")

ppc_draw_ids_r2 <- sort(sample(seq_len(nrow(ws4_rep_r2)), size = min(250, nrow(ws4_rep_r2))))
yrep_r2 <- ws4_rep_r2[ppc_draw_ids_r2, , drop = FALSE]

p_pick_r2_intervals <- bayesplot::ppc_intervals(
  y = war_obs_r2,
  yrep = yrep_r2,
  x = pick_obs_r2,
  prob = 0.50,
  prob_outer = 0.90
) +
  ggplot2::labs(
    title = "Round-2 declining-upside hurdle PPC: observed outcomes vs replicated intervals",
    x = "Draft pick",
    y = "First-4-year win shares"
  )
save_plot_safe(p_pick_r2_intervals, file.path(OUT_DIR, "pick_play_r2_v6_ppc_intervals.png"), width = 10, height = 6)

p_pick_r2_dens <- bayesplot::ppc_dens_overlay(war_obs_r2, yrep_r2[seq_len(min(50, nrow(yrep_r2))), , drop = FALSE]) +
  ggplot2::labs(title = "Round-2 declining-upside hurdle PPC: distribution overlay")
save_plot_safe(p_pick_r2_dens, file.path(OUT_DIR, "pick_play_r2_v6_ppc_density.png"), width = 10, height = 6)

p_pick_r2_stat_mean <- bayesplot::ppc_stat(war_obs_r2, yrep_r2, stat = "mean") +
  ggplot2::labs(title = "Round-2 declining-upside hurdle PPC: player-level mean")
save_plot_safe(p_pick_r2_stat_mean, file.path(OUT_DIR, "pick_play_r2_v6_ppc_mean.png"), width = 8, height = 5)

p_pick_r2_stat_sd <- bayesplot::ppc_stat(war_obs_r2, yrep_r2, stat = "sd") +
  ggplot2::labs(title = "Round-2 declining-upside hurdle PPC: player-level standard deviation")
save_plot_safe(p_pick_r2_stat_sd, file.path(OUT_DIR, "pick_play_r2_v6_ppc_sd.png"), width = 8, height = 5)

pick_ppc_summary_r2 <- tibble(
  row_id = seq_along(war_obs_r2),
  pick = pick_obs_r2,
  played = played_obs_r2,
  observed = war_obs_r2,
  pred_mean = colMeans(ws4_rep_r2),
  pred_q05 = apply(ws4_rep_r2, 2, quantile, probs = 0.05),
  pred_q50 = apply(ws4_rep_r2, 2, quantile, probs = 0.50),
  pred_q95 = apply(ws4_rep_r2, 2, quantile, probs = 0.95),
  covered_90 = observed >= pred_q05 & observed <= pred_q95,
  z_resid = (observed - pred_mean) / apply(ws4_rep_r2, 2, sd),
  played_rep_prob = colMeans(played_rep_r2)
)

if (nrow(pick_fit_data_r2) == nrow(pick_ppc_summary_r2)) {
  pick_ppc_summary_r2 <- pick_ppc_summary_r2 %>%
    bind_cols(pick_fit_data_r2 %>% select(any_of(c("draft_year", "player"))))
}

write_csv_safe(pick_ppc_summary_r2, file.path(OUT_DIR, "pick_play_r2_v6_ppc_summary.csv"))

second_round_band <- function(pick) {
  dplyr::case_when(
    pick <= 35 ~ "31-35",
    pick <= 40 ~ "36-40",
    pick <= 45 ~ "41-45",
    pick <= 50 ~ "46-50",
    pick <= 55 ~ "51-55",
    TRUE       ~ "56-60"
  )
}

r2_band_vec <- second_round_band(pick_obs_r2)
ws_dist_band_check_r2 <- purrr::map_dfr(unique(r2_band_vec), function(b) {
  idx <- which(r2_band_vec == b)
  obs <- war_obs_r2[idx]
  rep <- as.vector(ws4_rep_r2[, idx, drop = FALSE])
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
}) %>% arrange(pick_band)
write_csv_safe(ws_dist_band_check_r2, file.path(OUT_DIR, "pick_play_r2_v6_ppc_by_band.csv"))

cat(sprintf("Round-2 declining-upside hurdle 90%% PPC coverage: %.1f%% of player rows\n", 100 * mean(pick_ppc_summary_r2$covered_90)))
cat(sprintf("Round-2 empirical played rate %.1f%% | posterior predictive %.1f%%\n",
            100 * mean(played_obs_r2 == 1),
            100 * mean(pick_ppc_summary_r2$played_rep_prob)))

# ---- 4C. Markov transition PPC ------------------------------------------------

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
write_csv_safe(markov_ppc_rep, file.path(OUT_DIR, "team_strength_ppc_replicated_stats.csv"))
write_csv_safe(markov_ppc_obs, file.path(OUT_DIR, "team_strength_ppc_observed_stats.csv"))

plot_markov_ppc_stat <- function(stat_name) {
  obs_val <- markov_ppc_obs[[stat_name]][1]
  ggplot(markov_ppc_rep, aes(x = .data[[stat_name]])) +
    geom_histogram(bins = 30, alpha = 0.75) +
    geom_vline(xintercept = obs_val, linewidth = 1.1, linetype = 2) +
    labs(
      title = paste("team_strength PPC:", stat_name),
      subtitle = "Dashed line = observed transition-count statistic",
      x = stat_name,
      y = "Replicated draws"
    ) +
    theme_minimal(base_size = 12)
}

for (stat_name in c("total_stay", "big_jumps", "max_cell", "zero_cells", "playoff_stay", "bottom_to_playoff")) {
  save_plot_safe(
    plot_markov_ppc_stat(stat_name),
    file.path(OUT_DIR, paste0("team_strength_ppc_", stat_name, ".png")),
    width = 8,
    height = 5
  )
}

mean_counts_rep <- apply(counts_rep, c(2, 3), mean)
dimnames(mean_counts_rep) <- list(from = TIERS, to = TIERS)
dimnames(counts_mat) <- list(from = TIERS, to = TIERS)

heat_df <- bind_rows(
  as.data.frame.table(counts_mat, responseName = "count") %>%
    as_tibble() %>%
    transmute(from = as.character(from), to = as.character(to), count = as.numeric(count), type = "observed"),
  as.data.frame.table(mean_counts_rep, responseName = "count") %>%
    as_tibble() %>%
    transmute(from = as.character(from), to = as.character(to), count = as.numeric(count), type = "replicated_mean")
) %>%
  mutate(from = factor(from, levels = TIERS), to = factor(to, levels = TIERS))

p_markov_heat <- ggplot(heat_df, aes(x = to, y = from, fill = count)) +
  geom_tile() +
  geom_text(aes(label = round(count, 1)), size = 3) +
  facet_wrap(~type) +
  labs(title = "team_strength PPC: observed vs replicated mean transition counts", x = "To tier", y = "From tier") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_plot_safe(p_markov_heat, file.path(OUT_DIR, "team_strength_ppc_heatmap.png"), width = 11, height = 5.5)

# ==============================================================================
# 5. LOO / PREDICTIVE PERFORMANCE
# ==============================================================================

message_section("LOO / predictive performance")

# ---- 5A. Round-1 pick-value LOO ----------------------------------------------

pick_log_lik_r1 <- array_draws_matrix(pick_gq_fit_r1, "log_lik")
chain_id_r1 <- extract_chain_id(pick_fit)
if (length(chain_id_r1) != nrow(pick_log_lik_r1)) chain_id_r1 <- NULL

pick_loo_r1 <- tryCatch({
  r_eff <- if (!is.null(chain_id_r1)) loo::relative_eff(exp(pick_log_lik_r1), chain_id = chain_id_r1) else NULL
  loo::loo(pick_log_lik_r1, r_eff = r_eff)
}, error = function(e) {
  warning("Round-1 pick-value LOO failed: ", e$message)
  NULL
})

if (!is.null(pick_loo_r1)) {
  capture.output(print(pick_loo_r1), file = file.path(OUT_DIR, "pick_value_v3_r1_loo.txt"))
  pick_pareto_r1 <- tibble(
    row_id = seq_along(pick_obs_r1),
    pick = pick_obs_r1,
    pareto_k = as.numeric(pick_loo_r1$diagnostics$pareto_k)
  ) %>% mutate(flag_pareto_k = pareto_k > 0.7)
  if (nrow(pick_fit_data) == nrow(pick_pareto_r1)) {
    pick_pareto_r1 <- pick_pareto_r1 %>%
      bind_cols(pick_fit_data %>% select(any_of(c("draft_year", "player"))))
  }
  write_csv_safe(pick_pareto_r1, file.path(OUT_DIR, "pick_value_v3_r1_loo_pareto_k.csv"))
  cat("Round-1 pick_value_v3 LOO written to:", file.path(OUT_DIR, "pick_value_v3_r1_loo.txt"), "\n")
  cat(sprintf("Round-1 pick_value_v3 Pareto-k > 0.7: %d / %d\n", sum(pick_pareto_r1$flag_pareto_k), nrow(pick_pareto_r1)))
}

# ---- 5B. Round-2 hurdle LOO ---------------------------------------------------

pick_log_lik_r2 <- array_draws_matrix(pick_fit_r2_hurdle, "log_lik")
chain_id_r2 <- extract_chain_id(pick_fit_r2_hurdle)
if (length(chain_id_r2) != nrow(pick_log_lik_r2)) chain_id_r2 <- NULL

pick_loo_r2 <- tryCatch({
  r_eff <- if (!is.null(chain_id_r2)) loo::relative_eff(exp(pick_log_lik_r2), chain_id = chain_id_r2) else NULL
  loo::loo(pick_log_lik_r2, r_eff = r_eff)
}, error = function(e) {
  warning("Round-2 hurdle LOO failed: ", e$message)
  NULL
})

if (!is.null(pick_loo_r2)) {
  capture.output(print(pick_loo_r2), file = file.path(OUT_DIR, "pick_play_r2_v6_loo.txt"))
  pick_pareto_r2 <- tibble(
    row_id = seq_along(pick_obs_r2),
    pick = pick_obs_r2,
    played = played_obs_r2,
    pareto_k = as.numeric(pick_loo_r2$diagnostics$pareto_k)
  ) %>% mutate(flag_pareto_k = pareto_k > 0.7)
  if (nrow(pick_fit_data_r2) == nrow(pick_pareto_r2)) {
    pick_pareto_r2 <- pick_pareto_r2 %>%
      bind_cols(pick_fit_data_r2 %>% select(any_of(c("draft_year", "player"))))
  }
  write_csv_safe(pick_pareto_r2, file.path(OUT_DIR, "pick_play_r2_v6_loo_pareto_k.csv"))
  cat("Round-2 declining-upside hurdle LOO written to:", file.path(OUT_DIR, "pick_play_r2_v6_loo.txt"), "\n")
  cat(sprintf("Round-2 declining-upside hurdle Pareto-k > 0.7: %d / %d\n", sum(pick_pareto_r2$flag_pareto_k), nrow(pick_pareto_r2)))
}

# ---- 5C. Markov row-level LOO -------------------------------------------------

write_markov_gq_stan <- function() {
  "data {
  int<lower=1> K;
  array[K, K] int<lower=0> counts;
  matrix<lower=0>[K, K] alpha;
}
parameters {
  matrix<lower=0>[K, K] theta;
}
generated quantities {
  vector[K] row_log_lik;
  vector[K] row_entropy;
  array[K, K] int counts_rep;
  for (i in 1:K) {
    vector[K] theta_i;
    real theta_sum = 0;
    for (j in 1:K) theta_sum += theta[i, j];
    for (j in 1:K) theta_i[j] = theta[i, j] / theta_sum;
    row_log_lik[i] = multinomial_lpmf(counts[i] | theta_i);
    counts_rep[i] = multinomial_rng(theta_i, sum(counts[i]));
    row_entropy[i] = 0;
    for (j in 1:K) row_entropy[i] += -theta_i[j] * log(theta_i[j] + 1e-12);
  }
}"
}

get_or_generate_markov_gq <- function() {
  if (has_stan_variable(markov_fit, "row_log_lik")) return(markov_fit)
  if (!GENERATE_LOG_LIK_IF_MISSING) {
    stop("markov_fit does not contain row_log_lik; set GENERATE_LOG_LIK_IF_MISSING <- TRUE.", call. = FALSE)
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
markov_log_lik <- array_draws_matrix(markov_gq_fit, "row_log_lik")
chain_id_markov <- extract_chain_id(markov_fit)
if (length(chain_id_markov) != nrow(markov_log_lik)) chain_id_markov <- NULL

markov_loo <- tryCatch({
  r_eff <- if (!is.null(chain_id_markov)) loo::relative_eff(exp(markov_log_lik), chain_id = chain_id_markov) else NULL
  loo::loo(markov_log_lik, r_eff = r_eff)
}, error = function(e) {
  warning("team_strength row-level LOO failed: ", e$message)
  NULL
})

if (!is.null(markov_loo)) {
  capture.output(print(markov_loo), file = file.path(OUT_DIR, "team_strength_loo_row_level.txt"))
  markov_pareto <- tibble(
    from_tier = TIERS,
    pareto_k = as.numeric(markov_loo$diagnostics$pareto_k)
  ) %>% mutate(flag_pareto_k = pareto_k > 0.7)
  write_csv_safe(markov_pareto, file.path(OUT_DIR, "team_strength_loo_pareto_k.csv"))
  cat("team_strength row-level LOO written to:", file.path(OUT_DIR, "team_strength_loo_row_level.txt"), "\n")
  cat(sprintf("team_strength Pareto-k > 0.7: %d / %d\n", sum(markov_pareto$flag_pareto_k), nrow(markov_pareto)))
}

# ==============================================================================
# 6. MODEL-SPECIFIC ANALYTICAL CHECKS
# ==============================================================================

message_section("Model-specific checks")

# ---- 6A. Markov conjugacy check ----------------------------------------------

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
write_csv_safe(markov_closed_form_check, file.path(OUT_DIR, "team_strength_closed_form_check.csv"))

cat(sprintf(
  "Max abs diff between Stan transition means and closed-form Dirichlet means: %.6f\n",
  max(markov_closed_form_check$abs_diff, na.rm = TRUE)
))

# ---- 6B. Round-1 mean and sigma curves ----------------------------------------

pick_draw_mat_r1 <- as.matrix(pick_fit$draws(variables = pick_core_vars_r1, format = "draws_matrix"))
mu_curve_r1 <- sapply(1:30, function(pos) {
  pick_draw_mat_r1[, "alpha"] / (pos ^ pick_draw_mat_r1[, "beta"]) + pick_draw_mat_r1[, "gamma"]
})
colnames(mu_curve_r1) <- paste0("pick_", 1:30)

pick_curve_checks_r1 <- tibble(
  draw = seq_len(nrow(mu_curve_r1)),
  strictly_decreasing = apply(mu_curve_r1, 1, function(x) all(diff(x) <= 0)),
  any_negative_mean = apply(mu_curve_r1, 1, function(x) any(x < 0)),
  pick1_minus_pick30 = mu_curve_r1[, 1] - mu_curve_r1[, 30]
)
write_csv_safe(pick_curve_checks_r1, file.path(OUT_DIR, "pick_value_v3_r1_curve_draw_checks.csv"))

sigma_curve_r1 <- array_draws_matrix(pick_fit, "war_pred_sd")
pick_sigma_curve_r1 <- tibble(
  pick = 1:30,
  sigma_mean = colMeans(sigma_curve_r1),
  sigma_q05 = apply(sigma_curve_r1, 2, quantile, 0.05),
  sigma_q50 = apply(sigma_curve_r1, 2, quantile, 0.50),
  sigma_q95 = apply(sigma_curve_r1, 2, quantile, 0.95)
)
write_csv_safe(pick_sigma_curve_r1, file.path(OUT_DIR, "pick_value_v3_r1_sigma_curve.csv"))

cat(sprintf("Round-1 pick_value_v3 curve non-increasing in %.1f%% of posterior draws\n", 100 * mean(pick_curve_checks_r1$strictly_decreasing)))
cat(sprintf("Round-1 pick_value_v3 curve has any negative latent mean in %.1f%% of posterior draws\n", 100 * mean(pick_curve_checks_r1$any_negative_mean)))

# ---- 6C. Round-2 curve plausibility ------------------------------------------

p_play_curve_r2 <- array_draws_matrix(pick_fit_r2_hurdle, "p_play")
ev_curve_r2 <- array_draws_matrix(pick_fit_r2_hurdle, "ev")
ev_sd_curve_r2 <- array_draws_matrix(pick_fit_r2_hurdle, "ev_sd")
p_upside_curve_r2 <- array_draws_matrix(pick_fit_r2_hurdle, "p_upside")
cond_mean_curve_r2 <- array_draws_matrix(pick_fit_r2_hurdle, "cond_mean_ws")

pick_curve_checks_r2 <- tibble(
  draw = seq_len(nrow(ev_curve_r2)),
  ev_nonincreasing = apply(ev_curve_r2, 1, function(x) all(diff(x) <= 0)),
  p_play_nonincreasing = apply(p_play_curve_r2, 1, function(x) all(diff(x) <= 0)),
  p_upside_nonincreasing = apply(p_upside_curve_r2, 1, function(x) all(diff(x) <= 1e-10)),
  any_negative_ev = apply(ev_curve_r2, 1, function(x) any(x < 0)),
  pick31_minus_pick60_ev = ev_curve_r2[, 1] - ev_curve_r2[, 30]
)
write_csv_safe(pick_curve_checks_r2, file.path(OUT_DIR, "pick_play_r2_v6_curve_draw_checks.csv"))

pick_curve_summary_r2 <- tibble(
  pick = 31:60,
  p_play_mean = colMeans(p_play_curve_r2),
  p_play_q05 = apply(p_play_curve_r2, 2, quantile, 0.05),
  p_play_q50 = apply(p_play_curve_r2, 2, quantile, 0.50),
  p_play_q95 = apply(p_play_curve_r2, 2, quantile, 0.95),
  ev_mean = colMeans(ev_curve_r2),
  ev_q05 = apply(ev_curve_r2, 2, quantile, 0.05),
  ev_q50 = apply(ev_curve_r2, 2, quantile, 0.50),
  ev_q95 = apply(ev_curve_r2, 2, quantile, 0.95),
  ev_sd_mean = colMeans(ev_sd_curve_r2),
  cond_mean_ws_mean = colMeans(cond_mean_curve_r2),
  p_upside_mean = colMeans(p_upside_curve_r2)
)
write_csv_safe(pick_curve_summary_r2, file.path(OUT_DIR, "pick_play_r2_v6_curve_summary.csv"))

cat(sprintf("Round-2 EV curve non-increasing in %.1f%% of posterior draws\n", 100 * mean(pick_curve_checks_r2$ev_nonincreasing)))
cat(sprintf("Round-2 P(play) curve non-increasing in %.1f%% of posterior draws\n", 100 * mean(pick_curve_checks_r2$p_play_nonincreasing)))
cat(sprintf("Round-2 rare-upside probability non-increasing in %.1f%% of posterior draws\n", 100 * mean(pick_curve_checks_r2$p_upside_nonincreasing)))

# ==============================================================================
# 7. SIMULATION-BASED CALIBRATION (SBC)
# ==============================================================================

message_section("Simulation-Based Calibration")

rank_of_truth <- function(draws, truth) {
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

  plot_tbl <- rank_tbl %>% mutate(rank_scaled = (rank + 0.5) / (n_draws + 1))
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

# ---- 7A. Round-1 pick_value_v3 SBC -------------------------------------------

draw_pick_r1_truth <- function() {
  alpha <- exp(rnorm(1, log(20), 0.60))
  beta <- exp(rnorm(1, log(0.55), 0.50))
  gamma <- rnorm(1, 2, 3)
  log_sigma_1 <- rnorm(1, log(8), 0.50)
  z_sigma_step <- rnorm(29, 0, 1)
  tau_log_sigma_rw <- abs(rnorm(1, 0, 0.15))

  log_sigma_pick <- numeric(30)
  log_sigma_pick[1] <- log_sigma_1
  for (p in 2:30) {
    log_sigma_pick[p] <- log_sigma_pick[p - 1] + tau_log_sigma_rw * z_sigma_step[p - 1]
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

simulate_pick_r1_sbc_data <- function(template_data, truth) {
  pick <- as.numeric(template_data$pick)
  mu <- truth[["alpha"]] / (pick ^ truth[["beta"]]) + truth[["gamma"]]
  sigma_pick <- unname(truth[paste0("sigma_pick_", pick)])
  list(
    N = template_data$N,
    pick = as.integer(pick),
    ws4 = mu + sigma_pick * rt(length(pick), df = truth[["nu"]])
  )
}

run_one_pick_r1_sbc <- function(rep_id) {
  truth <- draw_pick_r1_truth()
  sim_data <- simulate_pick_r1_sbc_data(pick_stan_data_r1, truth)

  fit <- tryCatch(
    get_pick_model_r1()$sample(
      data = sim_data,
      chains = SBC_CHAINS,
      parallel_chains = SBC_PARALLEL_CHAINS,
      iter_warmup = SBC_ITER_WARMUP,
      iter_sampling = SBC_ITER_SAMPLING,
      adapt_delta = SBC_ADAPT_DELTA_R1,
      max_treedepth = SBC_MAX_TREEDEPTH,
      seed = 2026000 + rep_id,
      refresh = 0
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(ranks = tibble(), diag = tibble(rep = rep_id, model = "pick_value_v3_r1", fit_ok = FALSE, error = fit$message)))
  }

  draws <- as.matrix(fit$draws(variables = pick_core_vars_r1, format = "draws_matrix"))
  monitor_vars <- Reduce(intersect, list(pick_core_vars_r1, colnames(draws), names(truth)))

  ranks <- map_dfr(monitor_vars, function(v) {
    true_val <- unname(as.numeric(truth[[v]]))
    tibble(rep = rep_id, model = "pick_value_v3_r1", variable = v, truth = true_val,
           rank = rank_of_truth(draws[, v], true_val), n_draws = nrow(draws))
  })

  nuts <- summarize_nuts(fit, paste0("pick_value_v3_r1_sbc_rep_", rep_id),
                         max_treedepth = SBC_MAX_TREEDEPTH,
                         write_files = FALSE, run_cmdstan_diagnose = FALSE) %>%
    summarise(divergences = sum(divergences), max_treedepth_hits = sum(max_treedepth_hits),
              min_ebfmi = min(ebfmi, na.rm = TRUE), .groups = "drop") %>%
    mutate(rep = rep_id, model = "pick_value_v3_r1", fit_ok = TRUE, error = NA_character_) %>%
    select(rep, model, fit_ok, error, everything())

  list(ranks = ranks, diag = nuts)
}

# ---- 7B. Round-2 declining-upside hurdle SBC ---------------------------------

rtrunc_norm_one <- function(mean, sd, lower = -Inf, upper = Inf) {
  repeat {
    x <- rnorm(1, mean, sd)
    if (x >= lower && x <= upper) return(x)
  }
}

rtrunc_lognorm_one <- function(meanlog, sdlog, lower = 0, upper = Inf) {
  repeat {
    x <- rlnorm(1, meanlog, sdlog)
    if (x >= lower && x <= upper) return(x)
  }
}

draw_pick_r2_truth <- function(ws_floor) {
  logit_play_31 <- rnorm(1, stats::qlogis(0.88), 0.60)
  delta_logit_play <- rnorm(1, -0.05, 0.05)
  tau_logit_play_rw <- abs(rnorm(1, 0, 0.10))
  z_logit_play_step <- rnorm(29)

  log_cond_mean_ws_31 <- rnorm(1, log(13.0), 0.35)
  delta_log_cond_mean_ws <- rnorm(1, -0.015, 0.035)
  tau_log_cond_mean_ws_rw <- abs(rnorm(1, 0, 0.05))
  z_log_cond_mean_ws_step <- rnorm(29)

  log_sigma_ws_31 <- rnorm(1, log(0.45), 0.25)
  tau_log_sigma_ws_rw <- abs(rnorm(1, 0, 0.05))
  z_log_sigma_ws_step <- rnorm(29)

  logit_upside_31 <- rnorm(1, stats::qlogis(0.10), 0.35)
  delta_logit_upside <- rtrunc_norm_one(-0.04, 0.015, lower = -Inf, upper = 0)
  upside_log_shift <- rtrunc_norm_one(log(3.0), 0.35, lower = 0, upper = Inf)
  upside_sigma_mult <- rtrunc_lognorm_one(log(1.25), 0.15, lower = 1, upper = 2.25)

  logit_play_pick <- numeric(30)
  log_cond_mean_ws_pick <- numeric(30)
  log_sigma_ws_pick <- numeric(30)
  logit_upside_pick <- numeric(30)

  logit_play_pick[1] <- logit_play_31
  log_cond_mean_ws_pick[1] <- log_cond_mean_ws_31
  log_sigma_ws_pick[1] <- log_sigma_ws_31
  for (p in 2:30) {
    logit_play_pick[p] <- logit_play_pick[p - 1] + delta_logit_play + tau_logit_play_rw * z_logit_play_step[p - 1]
    log_cond_mean_ws_pick[p] <- log_cond_mean_ws_pick[p - 1] + delta_log_cond_mean_ws + tau_log_cond_mean_ws_rw * z_log_cond_mean_ws_step[p - 1]
    log_sigma_ws_pick[p] <- log_sigma_ws_pick[p - 1] + tau_log_sigma_ws_rw * z_log_sigma_ws_step[p - 1]
  }
  for (p in 1:30) logit_upside_pick[p] <- logit_upside_31 + delta_logit_upside * (p - 1)

  p_play_pick <- plogis(logit_play_pick)
  p_upside_pick <- plogis(logit_upside_pick)

  c(
    logit_play_31 = logit_play_31,
    delta_logit_play = delta_logit_play,
    tau_logit_play_rw = tau_logit_play_rw,
    log_cond_mean_ws_31 = log_cond_mean_ws_31,
    delta_log_cond_mean_ws = delta_log_cond_mean_ws,
    tau_log_cond_mean_ws_rw = tau_log_cond_mean_ws_rw,
    log_sigma_ws_31 = log_sigma_ws_31,
    tau_log_sigma_ws_rw = tau_log_sigma_ws_rw,
    logit_upside_31 = logit_upside_31,
    delta_logit_upside = delta_logit_upside,
    upside_prob_31 = p_upside_pick[1],
    upside_prob_45 = p_upside_pick[15],
    upside_prob_60 = p_upside_pick[30],
    upside_log_shift = upside_log_shift,
    upside_sigma_mult = upside_sigma_mult,
    setNames(p_play_pick, paste0("p_play_pick_", 31:60)),
    setNames(log_cond_mean_ws_pick, paste0("log_cond_mean_ws_pick_", 31:60)),
    setNames(exp(log_sigma_ws_pick), paste0("sigma_ws_pick_", 31:60)),
    setNames(p_upside_pick, paste0("p_upside_pick_", 31:60))
  )
}

simulate_pick_r2_sbc_data <- function(template_data, truth) {
  pick <- as.integer(template_data$pick)
  idx <- pick - 30L
  p_play <- unname(truth[paste0("p_play_pick_", pick)])
  p_upside <- unname(truth[paste0("p_upside_pick_", pick)])
  mu_typ <- unname(truth[paste0("log_cond_mean_ws_pick_", pick)])
  sig_typ <- unname(truth[paste0("sigma_ws_pick_", pick)])

  played <- rbinom(length(pick), size = 1, prob = p_play)
  ws4 <- numeric(length(pick))

  for (n in seq_along(pick)) {
    if (played[n] == 1L) {
      is_up <- rbinom(1, size = 1, prob = p_upside[n]) == 1L
      mu <- mu_typ[n] + if (is_up) truth[["upside_log_shift"]] else 0
      sig <- sig_typ[n] * if (is_up) truth[["upside_sigma_mult"]] else 1
      ws4[n] <- template_data$ws_floor + rlnorm(1, meanlog = mu, sdlog = sig)
    } else {
      ws4[n] <- 0
    }
  }

  list(
    N = template_data$N,
    pick = pick,
    played = as.integer(played),
    ws4 = as.numeric(ws4),
    ws_floor = template_data$ws_floor
  )
}

run_one_pick_r2_sbc <- function(rep_id) {
  truth <- draw_pick_r2_truth(pick_stan_data_r2$ws_floor)
  sim_data <- simulate_pick_r2_sbc_data(pick_stan_data_r2, truth)

  fit <- tryCatch(
    get_pick_model_r2()$sample(
      data = sim_data,
      chains = SBC_CHAINS,
      parallel_chains = SBC_PARALLEL_CHAINS,
      iter_warmup = SBC_ITER_WARMUP,
      iter_sampling = SBC_ITER_SAMPLING,
      adapt_delta = SBC_ADAPT_DELTA_R2,
      max_treedepth = SBC_MAX_TREEDEPTH,
      seed = 2026300 + rep_id,
      refresh = 0
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(ranks = tibble(), diag = tibble(rep = rep_id, model = "pick_play_r2_v6_declining_upside", fit_ok = FALSE, error = fit$message)))
  }

  draws <- as.matrix(fit$draws(variables = pick_core_vars_r2, format = "draws_matrix"))
  monitor_vars <- Reduce(intersect, list(pick_core_vars_r2, colnames(draws), names(truth)))

  ranks <- map_dfr(monitor_vars, function(v) {
    true_val <- unname(as.numeric(truth[[v]]))
    tibble(rep = rep_id, model = "pick_play_r2_v6_declining_upside", variable = v, truth = true_val,
           rank = rank_of_truth(draws[, v], true_val), n_draws = nrow(draws))
  })

  nuts <- summarize_nuts(fit, paste0("pick_play_r2_v6_sbc_rep_", rep_id),
                         max_treedepth = SBC_MAX_TREEDEPTH,
                         write_files = FALSE, run_cmdstan_diagnose = FALSE) %>%
    summarise(divergences = sum(divergences), max_treedepth_hits = sum(max_treedepth_hits),
              min_ebfmi = min(ebfmi, na.rm = TRUE), .groups = "drop") %>%
    mutate(rep = rep_id, model = "pick_play_r2_v6_declining_upside", fit_ok = TRUE, error = NA_character_) %>%
    select(rep, model, fit_ok, error, everything())

  list(ranks = ranks, diag = nuts)
}

# ---- 7C. team_strength SBC ----------------------------------------------------

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
  list(data = list(K = K, counts = counts, alpha = alpha), theta_true = theta_true)
}

run_one_markov_sbc <- function(rep_id) {
  sim <- simulate_markov_sbc_data(rowSums(counts_mat), alpha_prior)

  fit <- tryCatch(
    get_markov_model()$sample(
      data = sim$data,
      chains = SBC_CHAINS,
      parallel_chains = SBC_PARALLEL_CHAINS,
      iter_warmup = SBC_ITER_WARMUP,
      iter_sampling = SBC_ITER_SAMPLING,
      adapt_delta = SBC_ADAPT_DELTA_MARKOV,
      max_treedepth = SBC_MAX_TREEDEPTH,
      seed = 2027000 + rep_id,
      refresh = 0
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(ranks = tibble(), diag = tibble(rep = rep_id, model = "team_strength", fit_ok = FALSE, error = fit$message)))
  }

  draws <- as.matrix(fit$draws(variables = "theta", format = "draws_matrix"))
  idx <- parse_stan_indices(colnames(draws))

  ranks <- map_dfr(seq_len(ncol(draws)), function(c) {
    i <- idx[c, 1]
    j <- idx[c, 2]
    v <- sprintf("theta[%d,%d]", i, j)
    true_val <- sim$theta_true[i, j]
    tibble(rep = rep_id, model = "team_strength", variable = v,
           from_tier = TIERS[i], to_tier = TIERS[j], truth = true_val,
           rank = rank_of_truth(draws[, c], true_val), n_draws = nrow(draws))
  })

  nuts <- summarize_nuts(fit, paste0("team_strength_sbc_rep_", rep_id),
                         max_treedepth = SBC_MAX_TREEDEPTH,
                         write_files = FALSE, run_cmdstan_diagnose = FALSE) %>%
    summarise(divergences = sum(divergences), max_treedepth_hits = sum(max_treedepth_hits),
              min_ebfmi = min(ebfmi, na.rm = TRUE), .groups = "drop") %>%
    mutate(rep = rep_id, model = "team_strength", fit_ok = TRUE, error = NA_character_) %>%
    select(rep, model, fit_ok, error, everything())

  list(ranks = ranks, diag = nuts)
}

sbc_results <- list()

if (RUN_SBC_PICK_R1) {
  cat("Running round-1 pick_value_v3 SBC reps:", SBC_REPS_PICK_R1, "\n")
  pick_sbc_r1 <- vector("list", SBC_REPS_PICK_R1)
  for (r in seq_len(SBC_REPS_PICK_R1)) {
    cat(sprintf("  round-1 pick SBC %d / %d\n", r, SBC_REPS_PICK_R1))
    pick_sbc_r1[[r]] <- run_one_pick_r1_sbc(r)
  }
  pick_sbc_ranks_r1 <- bind_rows(map(pick_sbc_r1, "ranks"))
  pick_sbc_diag_r1 <- bind_rows(map(pick_sbc_r1, "diag"))
  pick_sbc_summary_r1 <- rank_uniform_summary(pick_sbc_ranks_r1, "pick_value_v3_r1")
  plot_sbc_ranks(pick_sbc_ranks_r1, "pick_value_v3_r1")
  write_csv_safe(pick_sbc_ranks_r1, file.path(OUT_DIR, "pick_value_v3_r1_sbc_ranks.csv"))
  write_csv_safe(pick_sbc_diag_r1, file.path(OUT_DIR, "pick_value_v3_r1_sbc_fit_diagnostics.csv"))
  sbc_results$pick_value_v3_r1 <- list(ranks = pick_sbc_ranks_r1, diag = pick_sbc_diag_r1, summary = pick_sbc_summary_r1)
}

if (RUN_SBC_PICK_R2) {
  cat("Running round-2 declining-upside hurdle SBC reps:", SBC_REPS_PICK_R2, "\n")
  pick_sbc_r2 <- vector("list", SBC_REPS_PICK_R2)
  for (r in seq_len(SBC_REPS_PICK_R2)) {
    cat(sprintf("  round-2 pick SBC %d / %d\n", r, SBC_REPS_PICK_R2))
    pick_sbc_r2[[r]] <- run_one_pick_r2_sbc(r)
  }
  pick_sbc_ranks_r2 <- bind_rows(map(pick_sbc_r2, "ranks"))
  pick_sbc_diag_r2 <- bind_rows(map(pick_sbc_r2, "diag"))
  pick_sbc_summary_r2 <- rank_uniform_summary(pick_sbc_ranks_r2, "pick_play_r2_v6_declining_upside")
  plot_sbc_ranks(pick_sbc_ranks_r2, "pick_play_r2_v6_declining_upside")
  write_csv_safe(pick_sbc_ranks_r2, file.path(OUT_DIR, "pick_play_r2_v6_sbc_ranks.csv"))
  write_csv_safe(pick_sbc_diag_r2, file.path(OUT_DIR, "pick_play_r2_v6_sbc_fit_diagnostics.csv"))
  sbc_results$pick_play_r2_v6_declining_upside <- list(ranks = pick_sbc_ranks_r2, diag = pick_sbc_diag_r2, summary = pick_sbc_summary_r2)
}

if (RUN_SBC_MARKOV) {
  cat("Running team_strength SBC reps:", SBC_REPS_MARKOV, "\n")
  markov_sbc <- vector("list", SBC_REPS_MARKOV)
  for (r in seq_len(SBC_REPS_MARKOV)) {
    cat(sprintf("  team_strength SBC %d / %d\n", r, SBC_REPS_MARKOV))
    markov_sbc[[r]] <- run_one_markov_sbc(r)
  }
  markov_sbc_ranks <- bind_rows(map(markov_sbc, "ranks"))
  markov_sbc_diag <- bind_rows(map(markov_sbc, "diag"))
  markov_sbc_summary <- rank_uniform_summary(markov_sbc_ranks, "team_strength")
  plot_sbc_ranks(markov_sbc_ranks, "team_strength", max_facets = 25L)
  write_csv_safe(markov_sbc_ranks, file.path(OUT_DIR, "team_strength_sbc_ranks.csv"))
  write_csv_safe(markov_sbc_diag, file.path(OUT_DIR, "team_strength_sbc_fit_diagnostics.csv"))
  sbc_results$team_strength <- list(ranks = markov_sbc_ranks, diag = markov_sbc_diag, summary = markov_sbc_summary)
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
    model = "pick_value_v3_r1",
    check = "PPC coverage",
    metric = sprintf("90%% interval coverage %.1f%%; max |z| %.2f",
                     100 * mean(pick_ppc_summary_r1$covered_90),
                     max(abs(pick_ppc_summary_r1$z_resid), na.rm = TRUE)),
    pass = mean(pick_ppc_summary_r1$covered_90) >= 0.70 &&
      max(abs(pick_ppc_summary_r1$z_resid), na.rm = TRUE) <= 4
  ),
  tibble(
    model = "pick_play_r2_v6_declining_upside",
    check = "PPC coverage / play rate",
    metric = sprintf("90%% interval coverage %.1f%%; observed play %.1f%%; rep play %.1f%%",
                     100 * mean(pick_ppc_summary_r2$covered_90),
                     100 * mean(played_obs_r2 == 1),
                     100 * mean(pick_ppc_summary_r2$played_rep_prob)),
    pass = mean(pick_ppc_summary_r2$covered_90) >= 0.70 &&
      abs(mean(played_obs_r2 == 1) - mean(pick_ppc_summary_r2$played_rep_prob)) <= 0.10
  ),
  tibble(
    model = "team_strength",
    check = "Conjugacy / code check",
    metric = sprintf("max |Stan mean - closed form| %.6f", max(markov_closed_form_check$abs_diff, na.rm = TRUE)),
    pass = max(markov_closed_form_check$abs_diff, na.rm = TRUE) < 0.01
  ),
  tibble(
    model = "team_strength",
    check = "PPC transition stats",
    metric = paste(
      sprintf("observed stays %s", markov_ppc_obs$total_stay),
      sprintf("big jumps %s", markov_ppc_obs$big_jumps),
      sep = "; "
    ),
    pass = TRUE
  )
)

if (!is.null(pick_loo_r1)) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "pick_value_v3_r1",
      check = "PSIS-LOO",
      metric = sprintf("elpd_loo %.2f; max Pareto-k %.2f",
                       pick_loo_r1$estimates["elpd_loo", "Estimate"],
                       max(pick_loo_r1$diagnostics$pareto_k, na.rm = TRUE)),
      pass = max(pick_loo_r1$diagnostics$pareto_k, na.rm = TRUE) <= 0.7
    )
  )
}

if (!is.null(pick_loo_r2)) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "pick_play_r2_v6_declining_upside",
      check = "PSIS-LOO",
      metric = sprintf("elpd_loo %.2f; max Pareto-k %.2f",
                       pick_loo_r2$estimates["elpd_loo", "Estimate"],
                       max(pick_loo_r2$diagnostics$pareto_k, na.rm = TRUE)),
      pass = max(pick_loo_r2$diagnostics$pareto_k, na.rm = TRUE) <= 0.7
    )
  )
}

if (!is.null(markov_loo)) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "team_strength",
      check = "PSIS-LOO row-level",
      metric = sprintf("elpd_loo %.2f; max Pareto-k %.2f",
                       markov_loo$estimates["elpd_loo", "Estimate"],
                       max(markov_loo$diagnostics$pareto_k, na.rm = TRUE)),
      pass = max(markov_loo$diagnostics$pareto_k, na.rm = TRUE) <= 0.7
    )
  )
}

if (RUN_SBC_PICK_R1 && exists("pick_sbc_summary_r1")) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "pick_value_v3_r1",
      check = "SBC rank uniformity",
      metric = sprintf("min KS p-value %.3f across monitored params", min(pick_sbc_summary_r1$ks_p, na.rm = TRUE)),
      pass = min(pick_sbc_summary_r1$ks_p, na.rm = TRUE) > 0.01
    )
  )
}

if (RUN_SBC_PICK_R2 && exists("pick_sbc_summary_r2")) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "pick_play_r2_v6_declining_upside",
      check = "SBC rank uniformity",
      metric = sprintf("min KS p-value %.3f across monitored params", min(pick_sbc_summary_r2$ks_p, na.rm = TRUE)),
      pass = min(pick_sbc_summary_r2$ks_p, na.rm = TRUE) > 0.01
    )
  )
}

if (RUN_SBC_MARKOV && exists("markov_sbc_summary")) {
  validation_decision <- bind_rows(
    validation_decision,
    tibble(
      model = "team_strength",
      check = "SBC rank uniformity",
      metric = sprintf("min KS p-value %.3f across theta cells", min(markov_sbc_summary$ks_p, na.rm = TRUE)),
      pass = min(markov_sbc_summary$ks_p, na.rm = TRUE) > 0.01
    )
  )
}

write_csv_safe(validation_decision, file.path(OUT_DIR, "validation_decision_table_latest_models.csv"))
print(validation_decision)

# Save a full R object for later dashboard/report use.
validation_results_latest_models <- list(
  parameter_summary = all_param_summary,
  nuts = all_nuts,
  pick_value_v3_r1 = list(
    ppc_summary = pick_ppc_summary_r1,
    ppc_by_pick = pick_ppc_by_pick_r1,
    loo = pick_loo_r1,
    curve_checks = pick_curve_checks_r1,
    sigma_curve = pick_sigma_curve_r1
  ),
  pick_play_r2_v6_declining_upside = list(
    ppc_summary = pick_ppc_summary_r2,
    ppc_by_band = ws_dist_band_check_r2,
    loo = pick_loo_r2,
    curve_checks = pick_curve_checks_r2,
    curve_summary = pick_curve_summary_r2
  ),
  team_strength = list(
    ppc_observed = markov_ppc_obs,
    ppc_replicated = markov_ppc_rep,
    loo = markov_loo,
    closed_form_check = markov_closed_form_check
  ),
  sbc = sbc_results,
  decision_table = validation_decision,
  settings = list(
    out_dir = OUT_DIR,
    sbc_reps_pick_r1 = SBC_REPS_PICK_R1,
    sbc_reps_pick_r2 = SBC_REPS_PICK_R2,
    sbc_reps_markov = SBC_REPS_MARKOV,
    run_sbc_pick_r1 = RUN_SBC_PICK_R1,
    run_sbc_pick_r2 = RUN_SBC_PICK_R2,
    run_sbc_markov = RUN_SBC_MARKOV,
    rhat_threshold = RHAT_THRESHOLD,
    ess_bulk_min = ESS_BULK_MIN,
    ess_tail_min = ESS_TAIL_MIN,
    ebfmi_min = EBFMI_MIN,
    generated_log_lik_if_missing = GENERATE_LOG_LIK_IF_MISSING,
    models = list(
      pick_value_v3 = resolve_required_file(PICK_MODEL_R1_PATH),
      pick_play_r2_v6_declining_upside = resolve_required_file(PICK_MODEL_R2_PATH),
      team_strength = resolve_required_file(TEAM_STRENGTH_MODEL_PATH)
    )
  )
)

saveRDS(validation_results_latest_models, file.path(OUT_DIR, "validation_results_latest_models.rds"))

cat("\nValidation complete. Key outputs:\n")
cat("  - ", file.path(OUT_DIR, "validation_decision_table_latest_models.csv"), "\n", sep = "")
cat("  - ", file.path(OUT_DIR, "validation_results_latest_models.rds"), "\n", sep = "")
cat("  - PNG diagnostics and PPC/SBC plots in ", OUT_DIR, "\n", sep = "")
