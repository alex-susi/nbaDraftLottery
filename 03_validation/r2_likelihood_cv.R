## ═════════════════════════════════════════════════════════════════════════════
## Second-round likelihood selection: draft-class cross-validation
## ═════════════════════════════════════════════════════════════════════════════
# Compares three fringe/contributor mixture hurdles that differ only in how the
# contributor share, upside and fringe spread change across the round:
#   mixture_linear  log-linear trends (zz_Archive/picks_Round2_mixture_linear_xrapm.stan)
#   mixture_curved  quadratic trends (zz_Archive/picks_Round2_mixture_curved_xrapm.stan)
#   mixture_rw      adjacent-pick random walks with drift (production,
#                   02_models/picks_Round2.stan)
#   mixture_rw_s0rw2  as mixture_rw, but a second-order walk on the fringe
#                   spread (zz_Archive/picks_Round2_mixture_rw_s0rw2_xrapm.stan)
# plus the ex-Gaussian hurdle and the earlier Gaussian conditional-mean hurdle.
#
# Expected pick value (ev = P(play) x mean if played) is scored on held-out mean
# error over every drafted player, including non-players at exactly zero.
# Outcome calibration uses log predictive density (elpd per player, hurdle
# included) and how often a held-out player falls below the 10th / above the
# 90th percentile. The Gaussian is also scored with the previous +/-2-pick
# empirical-residual outcome process.
#
# Run after source("01_data.R") so draft_4yr_r2 exists.
# Leave-one-draft-class-out folds, 2 chains x 1000 draws per fit.
# Writes 03_validation/r2_likelihood_cv_xrapm.csv.

library(dplyr)
library(cmdstanr)
library(posterior)

r2 <- draft_4yr_r2

cv_models <- list(
  mixture_linear = cmdstan_model("02_models/zz_Archive/picks_Round2_mixture_linear_xrapm.stan"),
  mixture_curved = cmdstan_model("02_models/zz_Archive/picks_Round2_mixture_curved_xrapm.stan"),
  mixture_rw     = cmdstan_model("02_models/picks_Round2.stan"),  # production
  mixture_rw_s0rw2 = cmdstan_model("02_models/zz_Archive/picks_Round2_mixture_rw_s0rw2_xrapm.stan"),  # 2nd-order fringe spread
  exgauss        = cmdstan_model("02_models/zz_Archive/picks_Round2_exgauss_xrapm.stan"),
  gaussian       = cmdstan_model("02_models/zz_Archive/picks_Round2_gaussian_xrapm.stan")
)
mixture_models <- c("mixture_linear", "mixture_curved", "mixture_rw", "mixture_rw_s0rw2")

# Mixture starting points near the prior means (as in 03_models.R). Generic
# random inits can strand a chain far from the data with a collapsed step size.
r2_base_init <- function() {
  list(eta_31  = qlogis(0.90) + stats::rnorm(1, 0, 0.2),
       d_pi    = -0.05,
       tau_pi  = 0.05,
       z_pi    = stats::rnorm(29, 0, 0.1),
       ev_31   = 1.5 + stats::rnorm(1, 0, 0.2),
       log_gap = log(0.05) + stats::rnorm(1, 0, 0.1),
       tau_eps = 0.2,
       z_eps   = stats::rnorm(29, 0, 0.1))
}

r2_init_for <- function(variant) {
  function() {
    extra <- switch(variant,
                    mixture_linear = list(logit_q_31 = qlogis(0.35) + stats::rnorm(1, 0, 0.2),
                                   d_q = -0.04,
                                   log_T_31 = log(6) + stats::rnorm(1, 0, 0.1),
                                   d_T = -0.01,
                                   log_s0_31 = log(2) + stats::rnorm(1, 0, 0.1),
                                   d_s0 = -0.03),
                    mixture_curved = list(a_q = qlogis(0.25) + stats::rnorm(1, 0, 0.2),
                                          b_q = -0.5, c_q = 0,
                                          a_T = log(5.5) + stats::rnorm(1, 0, 0.1),
                                          b_T = -0.2, c_T = 0,
                                          a_s0 = log(1.3) + stats::rnorm(1, 0, 0.1),
                                          b_s0 = -0.5, c_s0 = 0),
                    mixture_rw = list(logit_q_31 = qlogis(0.35) + stats::rnorm(1, 0, 0.2),
                                      d_q = -0.04, z_q = stats::rnorm(29, 0, 0.1), tau_q = 0.05,
                                      log_T_31 = log(6) + stats::rnorm(1, 0, 0.1),
                                      d_T = -0.01, z_T = stats::rnorm(29, 0, 0.1), tau_T = 0.05,
                                      log_s0_31 = log(2) + stats::rnorm(1, 0, 0.1),
                                      d_s0 = -0.03, z_s0 = stats::rnorm(29, 0, 0.1),
                                      tau_s0 = 0.05),
                    mixture_rw_s0rw2 = list(logit_q_31 = qlogis(0.35) + stats::rnorm(1, 0, 0.2),
                                            d_q = -0.04, z_q = stats::rnorm(29, 0, 0.1), tau_q = 0.05,
                                            log_T_31 = log(6) + stats::rnorm(1, 0, 0.1),
                                            d_T = -0.01, z_T = stats::rnorm(29, 0, 0.1), tau_T = 0.05,
                                            log_s0_31 = log(2) + stats::rnorm(1, 0, 0.1),
                                            d_s0 = -0.03, z_s0 = stats::rnorm(28, 0, 0.1),
                                            tau_s0 = 0.01))
    c(r2_base_init(), extra)
  }
}

cv_divergences <- list()

cv_years <- sort(unique(r2$draft_year))
r2$fold  <- match(r2$draft_year, cv_years)
n_folds  <- length(cv_years)

N_OUTCOME_REPS <- 10L  # outcome draws per posterior draw for held-out quantiles

draws_mat <- function(fit, v) {
  as.matrix(fit$draws(v, format = "draws_matrix"))
}

exgauss_lpdf <- function(y, m, s, lam) {
  log(lam) + lam * (m - y) + lam^2 * s^2 / 2 + pnorm((y - m) / s - lam * s, log.p = TRUE)
}

log_mean_exp_cols <- function(ll) {
  apply(ll, 2, function(x) max(x) + log(mean(exp(x - max(x)))))
}

# Hurdle outcome draws (draws x 30): zero if not played, else played draw
hurdle_draws <- function(pi_mat, played_fun) {
  sapply(1:30, function(j) {
    n_out  <- N_OUTCOME_REPS * nrow(pi_mat)
    played <- stats::runif(n_out) < rep(pi_mat[, j], N_OUTCOME_REPS)
    ifelse(played, played_fun(j, n_out), 0)
  })
}

cv_pred <- list()
for (k in seq_len(n_folds)) {
  train <- r2 %>% filter(fold != k)
  test  <- r2 %>% filter(fold == k)
  dat <- list(N = nrow(train), pick = train$pick, played = train$played, war4 = train$war4)
  j_test <- test$pick - 30L

  for (nm in names(cv_models)) {
    fit <- cv_models[[nm]]$sample(data = dat, chains = 2, parallel_chains = 2,
                                  iter_warmup = 1000, iter_sampling = 1000,
                                  adapt_delta = 0.999, max_treedepth = 12,
                                  init = if (nm %in% mixture_models) r2_init_for(nm) else 0.5,
                                  seed = 100 + k, refresh = 0, show_messages = FALSE)
    cv_divergences[[length(cv_divergences) + 1]] <-
      tibble(model = nm, fold = k,
             divergences = sum(fit$diagnostic_summary(quiet = TRUE)$num_divergent))
    pi_mat <- draws_mat(fit, "pi_p")
    mp_mat <- draws_mat(fit, "mean_play")
    sd_mat <- draws_mat(fit, "sd_play")
    ev <- colMeans(pi_mat * mp_mat)

    if (nm %in% mixture_models) {
      q_mat  <- draws_mat(fit, "p_contrib")
      t_mat  <- draws_mat(fit, "upside_mean")
      s0_mat <- draws_mat(fit, "sd_fringe")
      c_mat  <- draws_mat(fit, "fringe_mean")
      played_lpdf <- function(i) {
        j <- j_test[i]
        a <- log(q_mat[, j]) + exgauss_lpdf(test$war4[i], c_mat[, j], s0_mat[, j], 1 / t_mat[, j])
        b <- log1p(-q_mat[, j]) + dnorm(test$war4[i], c_mat[, j], s0_mat[, j], log = TRUE)
        pmax(a, b) + log1p(exp(-abs(a - b)))
      }
      played_fun <- function(j, n_out) {
        contrib <- stats::runif(n_out) < rep(q_mat[, j], N_OUTCOME_REPS)
        stats::rnorm(n_out, rep(c_mat[, j], N_OUTCOME_REPS), rep(s0_mat[, j], N_OUTCOME_REPS)) +
          ifelse(contrib, stats::rexp(n_out, rep(1 / t_mat[, j], N_OUTCOME_REPS)), 0)
      }
    } else if (nm == "exgauss") {
      tau_mat <- draws_mat(fit, "tau_x")
      sig_mat <- draws_mat(fit, "sigma_g")
      played_lpdf <- function(i) {
        j <- j_test[i]
        exgauss_lpdf(test$war4[i], mp_mat[, j] - tau_mat[, j], sig_mat[, j], 1 / tau_mat[, j])
      }
      played_fun <- function(j, n_out) {
        stats::rnorm(n_out, rep(mp_mat[, j] - tau_mat[, j], N_OUTCOME_REPS),
                     rep(sig_mat[, j], N_OUTCOME_REPS)) +
          stats::rexp(n_out, rep(1 / tau_mat[, j], N_OUTCOME_REPS))
      }
    } else {
      played_lpdf <- function(i) {
        j <- j_test[i]
        dnorm(test$war4[i], mp_mat[, j], sd_mat[, j], log = TRUE)
      }
      played_fun <- function(j, n_out) {
        stats::rnorm(n_out, rep(mp_mat[, j], N_OUTCOME_REPS), rep(sd_mat[, j], N_OUTCOME_REPS))
      }

      # Previous outcome process: centered +/-2-pick played-residual pools
      tr_played <- train %>% filter(.data$played == 1L)
      resid <- tr_played$war4 - colMeans(mp_mat)[tr_played$pick - 30L]
      pools <- lapply(31:60, function(p) {
        r <- resid[abs(tr_played$pick - p) <= 2]
        r - mean(r)
      })
      out_w <- hurdle_draws(pi_mat, function(j, n_out) {
        rep(mp_mat[, j], N_OUTCOME_REPS) + sample(pools[[j]], n_out, replace = TRUE)
      })
      cv_pred[[length(cv_pred) + 1]] <- tibble(model = "gaussian_resid_window", fold = k,
                                               pick = test$pick, y = test$war4, pred = ev[j_test],
                                               elpd = NA_real_,
                                               q10 = apply(out_w, 2, quantile, 0.10)[j_test],
                                               q90 = apply(out_w, 2, quantile, 0.90)[j_test])
    }

    ll <- sapply(seq_len(nrow(test)), function(i) {
      j <- j_test[i]
      if (test$played[i] == 1L) log(pi_mat[, j]) + played_lpdf(i) else log1p(-pi_mat[, j])
    })
    out <- hurdle_draws(pi_mat, played_fun)

    cv_pred[[length(cv_pred) + 1]] <- tibble(model = nm, fold = k, pick = test$pick,
                                             y = test$war4, pred = ev[j_test],
                                             elpd = log_mean_exp_cols(ll),
                                             q10 = apply(out, 2, quantile, 0.10)[j_test],
                                             q90 = apply(out, 2, quantile, 0.90)[j_test])
  }

  slot_means <- train %>% group_by(pick) %>% summarise(m = mean(war4), .groups = "drop")
  cv_pred[[length(cv_pred) + 1]] <- tibble(model = "slot_mean", fold = k, pick = test$pick,
                                           y = test$war4,
                                           pred = slot_means$m[match(test$pick, slot_means$pick)],
                                           elpd = NA_real_, q10 = NA_real_, q90 = NA_real_)
  cat(sprintf("  fold %d/%d (draft %d) done\n", k, n_folds, cv_years[k]))
}

cv_pred <- bind_rows(cv_pred)

cv_summary <- cv_pred %>%
  mutate(band = cut(pick, c(30, 40, 50, 60), labels = c("31-40", "41-50", "51-60"))) %>%
  group_by(model) %>%
  summarise(rmse        = sqrt(mean((y - pred)^2)),
            bias        = mean(pred - y),
            bias_31_40  = mean((pred - y)[band == "31-40"]),
            bias_41_50  = mean((pred - y)[band == "41-50"]),
            bias_51_60  = mean((pred - y)[band == "51-60"]),
            elpd        = mean(elpd),
            below_q10   = mean(y < q10),
            above_q90   = mean(y > q90),
            # Share of held-out players inside the 10th-90th band (80% if
            # calibrated; exact zeros make this coarse for non-players)
            cov80_31_40 = mean((y >= q10 & y <= q90)[band == "31-40"]),
            cov80_41_50 = mean((y >= q10 & y <= q90)[band == "41-50"]),
            cov80_51_60 = mean((y >= q10 & y <= q90)[band == "51-60"]),
            .groups     = "drop") %>%
  left_join(bind_rows(cv_divergences) %>%
              group_by(model) %>%
              summarise(divergences = sum(divergences), .groups = "drop"),
            by = "model") %>%
  arrange(rmse)

print(cv_summary, width = Inf)
write.csv(cv_summary, "03_validation/r2_likelihood_cv_xrapm.csv", row.names = FALSE)
