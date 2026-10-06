## ═════════════════════════════════════════════════════════════════════════════
## First-round likelihood selection: draft-class cross-validation
## ═════════════════════════════════════════════════════════════════════════════
# Expected pick value is the posterior mean curve, so first-round likelihoods
# are compared first on held-out MEAN error rather than log density (LOO).
# Models that also describe player outcomes are scored on held-out outcome
# calibration: log predictive density (elpd per player) and how often a held-out
# player falls below the 10th / above the 90th percentile (10% each if
# calibrated).
#
# The production Gaussian is scored two ways for outcomes: its own Gaussian
# density, and the +/-2-pick empirical-residual process (outcomes = mean curve +
# centered residual from training players drafted within two picks).
#
# Run after source("01_data.R") so draft_4yr (war4 outcomes) exists.
# Leave-one-draft-class-out folds, 2 chains x 1000 draws per fit.
# Writes 03_validation/r1_likelihood_cv_xrapm.csv.

library(dplyr)
library(cmdstanr)
library(posterior)

r1 <- draft_4yr
r1_floor <- min(-2, min(r1$war4) - 0.25)

cv_models <- list(
  gaussian   = cmdstan_model("02_models/zz_Archive/picks_Round1_gaussian_xrapm.stan"),
  exgauss    = cmdstan_model("02_models/zz_Archive/picks_Round1_exgauss_powerlaw_xrapm.stan"),
  exgauss_rw = cmdstan_model("02_models/zz_Archive/picks_Round1_exgauss_rw1_xrapm.stan"),  # first-order RW spread / skew
  exgauss_rw2 = cmdstan_model("02_models/picks_Round1.stan"),  # production: second-order RW spread / skew
  student_t  = cmdstan_model("02_models/zz_Archive/picks_Round1_student_t_xrapm.stan"),
  mixture    = cmdstan_model("02_models/zz_Archive/picks_Round1_lognormal_mixture_xrapm.stan")
)

cv_years <- sort(unique(r1$draft_year))
r1$fold  <- match(r1$draft_year, cv_years)
n_folds  <- length(cv_years)

N_OUTCOME_REPS <- 10L  # outcome draws per posterior draw for held-out quantiles

# Second-order random walks compound every slope change across later picks, so
# generic random inits can start with absurd spread curves and strand a chain.
# Start near the prior means instead (as in 03_models.R).
r1_rw2_init <- function() {
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
       tau_r     = 0.01)
}

cv_divergences <- list()

draws_mat <- function(fit, v) {
  as.matrix(fit$draws(v, format = "draws_matrix"))
}

# Ex-Gaussian log density: Normal(m, s) + Exponential(rate lam)
exgauss_lpdf <- function(y, m, s, lam) {
  log(lam) + lam * (m - y) + lam^2 * s^2 / 2 + pnorm((y - m) / s - lam * s, log.p = TRUE)
}

# log mean exp over posterior draws (rows) for each held-out player (columns)
log_mean_exp_cols <- function(ll) {
  apply(ll, 2, function(x) max(x) + log(mean(exp(x - max(x)))))
}

# Held-out 10th / 90th percentiles from an outcome-draw matrix (draws x 30)
outcome_q <- function(out) {
  list(q10 = apply(out, 2, quantile, 0.10),
       q90 = apply(out, 2, quantile, 0.90))
}

cv_pred <- list()
for (k in seq_len(n_folds)) {
  train <- r1 %>% filter(fold != k)
  test  <- r1 %>% filter(fold == k)
  dat <- list(N = nrow(train), pick = train$pick, war4 = train$war4, war_floor = r1_floor)

  for (nm in names(cv_models)) {
    fit <- cv_models[[nm]]$sample(data = dat, chains = 2, parallel_chains = 2,
                                  iter_warmup = 1000, iter_sampling = 1000,
                                  adapt_delta = if (nm == "exgauss_rw2") 0.99 else 0.95,
                                  init = if (nm == "exgauss_rw2") r1_rw2_init else 0.5,
                                  seed = 100 + k, refresh = 0, show_messages = FALSE)
    cv_divergences[[length(cv_divergences) + 1]] <-
      tibble(model = nm, fold = k,
             divergences = sum(fit$diagnostic_summary(quiet = TRUE)$num_divergent),
             treedepth_hits = sum(fit$diagnostic_summary(quiet = TRUE)$num_max_treedepth))
    mu_draws <- draws_mat(fit, "war4_pred")
    mu <- colMeans(mu_draws)
    tp <- test$pick

    elpd <- NA_real_
    q    <- NULL

    if (nm == "gaussian") {
      sd_draws <- draws_mat(fit, "war4_pred_sd")
      ll <- sapply(seq_along(tp), function(i) {
        dnorm(test$war4[i], mu_draws[, tp[i]], sd_draws[, tp[i]], log = TRUE)
      })
      elpd <- log_mean_exp_cols(ll)

      # Production outcome process: centered +/-2-pick training residual pools
      resid <- train$war4 - mu[train$pick]
      pools <- lapply(1:30, function(p) {
        r <- resid[abs(train$pick - p) <= 2]
        r - mean(r)
      })
      out_w <- sapply(1:30, function(p) {
        rep(mu_draws[, p], N_OUTCOME_REPS) +
          sample(pools[[p]], N_OUTCOME_REPS * nrow(mu_draws), replace = TRUE)
      })
      qw <- outcome_q(out_w)
      cv_pred[[length(cv_pred) + 1]] <- tibble(model = "gaussian_resid_window", fold = k,
                                               pick = tp, y = test$war4, pred = mu[tp],
                                               elpd = NA_real_,
                                               q10 = qw$q10[tp], q90 = qw$q90[tp])

      out_g <- sapply(1:30, function(p) {
        rnorm(N_OUTCOME_REPS * nrow(mu_draws), rep(mu_draws[, p], N_OUTCOME_REPS),
              rep(sd_draws[, p], N_OUTCOME_REPS))
      })
      q <- outcome_q(out_g)
    }

    if (nm %in% c("exgauss", "exgauss_rw", "exgauss_rw2")) {
      tau_draws <- draws_mat(fit, "tau_x")
      sig_draws <- draws_mat(fit, "sigma_g")
      ll <- sapply(seq_along(tp), function(i) {
        p <- tp[i]
        exgauss_lpdf(test$war4[i], mu_draws[, p] - tau_draws[, p],
                     sig_draws[, p], 1 / tau_draws[, p])
      })
      elpd <- log_mean_exp_cols(ll)

      out_x <- sapply(1:30, function(p) {
        n_out <- N_OUTCOME_REPS * nrow(mu_draws)
        rnorm(n_out, rep(mu_draws[, p] - tau_draws[, p], N_OUTCOME_REPS),
              rep(sig_draws[, p], N_OUTCOME_REPS)) +
          rexp(n_out, rep(1 / tau_draws[, p], N_OUTCOME_REPS))
      })
      q <- outcome_q(out_x)
    }

    cv_pred[[length(cv_pred) + 1]] <- tibble(model = nm, fold = k, pick = tp,
                                             y = test$war4, pred = mu[tp],
                                             elpd = if (length(elpd) > 1) elpd else NA_real_,
                                             q10 = if (is.null(q)) NA_real_ else q$q10[tp],
                                             q90 = if (is.null(q)) NA_real_ else q$q90[tp])
  }

  # Baseline: raw training slot means
  slot_means <- train %>% group_by(pick) %>% summarise(m = mean(war4), .groups = "drop")
  cv_pred[[length(cv_pred) + 1]] <- tibble(model = "slot_mean", fold = k, pick = test$pick,
                                           y = test$war4,
                                           pred = slot_means$m[match(test$pick, slot_means$pick)],
                                           elpd = NA_real_, q10 = NA_real_, q90 = NA_real_)
  cat(sprintf("  fold %d/%d (draft %d) done\n", k, n_folds, cv_years[k]))
}

cv_pred <- bind_rows(cv_pred)

cv_summary <- cv_pred %>%
  mutate(band = cut(pick, c(0, 3, 10, 20, 30), labels = c("1-3", "4-10", "11-20", "21-30"))) %>%
  group_by(model) %>%
  summarise(rmse       = sqrt(mean((y - pred)^2)),
            bias       = mean(pred - y),
            bias_1_3   = mean((pred - y)[band == "1-3"]),
            bias_4_10  = mean((pred - y)[band == "4-10"]),
            bias_11_20 = mean((pred - y)[band == "11-20"]),
            bias_21_30 = mean((pred - y)[band == "21-30"]),
            elpd       = mean(elpd),
            below_q10  = mean(y < q10),
            above_q90  = mean(y > q90),
            # Share of held-out players inside the 10th-90th band (80% if calibrated)
            cov80_1_10  = mean((y >= q10 & y <= q90)[pick <= 10]),
            cov80_11_20 = mean((y >= q10 & y <= q90)[pick > 10 & pick <= 20]),
            cov80_21_30 = mean((y >= q10 & y <= q90)[pick > 20]),
            .groups    = "drop") %>%
  left_join(bind_rows(cv_divergences) %>%
              group_by(model) %>%
              summarise(divergences    = sum(divergences),
                        treedepth_hits = sum(treedepth_hits),
                        .groups        = "drop"),
            by = "model") %>%
  arrange(rmse)

print(cv_summary, width = Inf)
write.csv(cv_summary, "03_validation/r1_likelihood_cv_xrapm.csv", row.names = FALSE)
