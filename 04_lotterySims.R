# ============================================================================
# 04_lotterySims.R
# ============================================================================
# Monte Carlo of future standings, both lottery systems, pick ownership and
# pick values, followed by summaries and the dashboard export.
#
# Lottery systems:
#   - Current (pre-2027): 14 teams, weighted combinations, top 4 picks drawn.
#   - Approved 3-2-1: 16 teams, 2/3/2/1 balls by tier, all 16 picks drawn;
#     the three worst teams cannot land below #12.
#
# Helper functions are in 00_helpers.R, under "HELPERS FIRST CALLED IN
# 04_lotterySims.R":
#   - SECTION 10: lottery simulators (sim_current_lottery, sim_321_lottery)
#   - SECTION 11: pick ownership and valuation (pick_conveys,
#     resolve_pick_owners, resolve_second_pick_owners, sample_pick_value,
#     value_allocated_future_assets)
#   - SECTION 12: Expected Pick Value and display matrices
#     (asset_value_mean_from_slots, build_display_draw_matrix,
#     build_display_convey_matrix, keep_display_cols)





## ═════════════════════════════════════════════════════════════════════════════
## SECTION 12: FULL MONTE CARLO ------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════
# For each simulation:
#   * 2026 is FIXED to the actual draft order (both systems identical) so its
#     values are not random — only 2027-2032 are projected.
#   * Draw one 30-rank Markov transition matrix and one pick-value posterior index.
#   * Initialize each team's exact rank_worst state from 2025-26, then evolve
#     year by year.
#   * Each year, seed BOTH lotteries from the same simulated full standings
#     permutation, resolve swaps, apply protections and the new pick restrictions,
#     and value every owned pick.
#
# Rank convention:
#   rank_worst = 1  -> worst record / old lottery seed 1
#   rank_worst = 30 -> best record / pick 30 by inverse record
#
# Seeding 2026 baseline rank states from the final 2025-26 standings:
if (!exists("current_rank_worst0")) {
  current_rank_worst0 <- setNames(
    as.integer(max(current_standings$overall_rank, na.rm = TRUE) + 1L - current_standings$overall_rank),
    current_standings$abbr
  )
}

# Actual 2026 picks: slots are locked, so each sim samples only the player
# outcome at the fixed slot (pick-value uncertainty, no lottery uncertainty).
a26_assets <- pick_assets %>% filter(year == 2026L)
a26_ids    <- a26_assets$asset_id

# Per-asset value stores: [N_SIMS x n_assets] under each system.
# 2026 assets get identical current/new values; future assets differ.
asset_cur <- matrix(NA_real_, nrow = N_SIMS, ncol = n_assets,
                    dimnames = list(NULL, asset_ids))
asset_new <- matrix(NA_real_, nrow = N_SIMS, ncol = n_assets,
                    dimnames = list(NULL, asset_ids))

# --- Extra stores so the Trade Machine can apply HYPOTHETICAL protections /
#     swaps to a pick a user is sending. For every asset we record, per sim:
#       *_slot_*   = the ORIGINAL team's drafted slot (1-60)
#       *_raw_*    = the pick VALUE at that slot, BEFORE any conveyance test
#       *_ownslot_*= the OWNER team's own slot that year (needed for swaps)
#   With these the app can recompute "value if top-N protected" (= raw if
#   slot > N else 0) or "value if swap" (= value at min(own, target) slot)
#   on the fly, with correct within-sim correlation.
asset_slot_cur   <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_slot_new   <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_raw_cur    <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_raw_new    <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_ownslot_cur <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_ownslot_new <- matrix(NA_real_, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_convey_cur <- matrix(0L, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))
asset_convey_new <- matrix(0L, N_SIMS, n_assets, dimnames = list(NULL, asset_ids))

# Full team x year slot record (worst-to-best draft seat each projected year)
# plus the pick-value curve parameters used in each sim. Together these let the
# Trade Machine value ANY slot in ANY sim, so a user can attach a hypothetical
# protection or swap to a pick and we recompute conveyance with correct
# within-sim correlation. Years are FIRST_PROJECTED_DRAFT..LAST_PROJECTED_DRAFT.
proj_years   <- FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT
n_proj_years <- length(proj_years)
team_slot_cur <- array(NA_real_, dim = c(N_SIMS, 30, n_proj_years),
                       dimnames = list(NULL, all_teams, as.character(proj_years)))
team_slot_new <- array(NA_real_, dim = c(N_SIMS, 30, n_proj_years),
                       dimnames = list(NULL, all_teams, as.character(proj_years)))
team_slot2_cur <- array(NA_real_, dim = c(N_SIMS, 30, n_proj_years),
                        dimnames = list(NULL, all_teams, as.character(proj_years)))
team_slot2_new <- array(NA_real_, dim = c(N_SIMS, 30, n_proj_years),
                        dimnames = list(NULL, all_teams, as.character(proj_years)))
team_rank_worst <- array(NA_real_, dim = c(N_SIMS, 30, n_proj_years),
                         dimnames = list(NULL, all_teams, as.character(proj_years)))
sim_curve_par_cols <- c(
  "alpha", "beta", "gamma", "tau_eps",
  "eta_31_r2", "tau_pi_r2",
  paste0("mu_", 1:60),
  paste0("sigma_", 1:60),
  paste0("p_play_", 31:60),
  paste0("r2_cond_played_war_mean_", 31:60)
)
sim_curve_par <- matrix(NA_real_, N_SIMS, length(sim_curve_par_cols),
                        dimnames = list(NULL, sim_curve_par_cols))

# Map from (year, owner, original_team, pick_type) -> asset_id for fast lookup
asset_key <- pick_assets %>%
  mutate(key = sprintf("%d|R%d|%s|%s|%s", year, round, owner, original_team, pick_type))
asset_lookup <- setNames(asset_key$asset_id, asset_key$key)

cat(sprintf("\n--- Running %s Monte Carlo Simulations ---\n",
            format(N_SIMS, big.mark = ",")))
cat("  2026 = ACTUAL results (locked) | 2027-2029 = 3-2-1 | both tracked\n\n")

results <- vector("list", N_SIMS)

for (sim in 1:N_SIMS) {
  d_pick <- sample(nrow(pick_draws), 1)
  d_pick2 <- sample(nrow(pick2_draws), 1)
  d_mk   <- sample(n_markov_draws, 1)
  # record this sim's pick-value curve params (for app-side hypothetical picks)
  sim_curve_par[sim, ] <- c(
    pick_draws$alpha[d_pick],
    pick_draws$beta[d_pick],
    pick_draws$gamma[d_pick],
    pick_draws$tau_eps[d_pick],
    pick2_draws$eta_31[d_pick2],
    pick2_draws$tau_pi[d_pick2],
    as.numeric(pick_mu_draws[d_pick, ]),
    as.numeric(pick2_mu_draws[d_pick2, ]),
    as.numeric(pick_sd_draws[d_pick, ]),
    as.numeric(pick2_sd_draws[d_pick2, ]),
    as.numeric(pick2_p_play_draws[d_pick2, ]),
    as.numeric(pick2_cond_mu_draws[d_pick2, ])
  )
  
  # rank-transition matrix for this sim (rows = current rank_worst, cols = next rank_worst)
  P <- matrix(theta_draws[d_mk, as.vector(theta_cols)], N_RANKS, N_RANKS)

  # accumulators: team total value, number of picks, best single pick value
  tv_c <- setNames(rep(0, 30), all_teams)
  tn_c <- setNames(rep(0L, 30), all_teams)
  tb_c <- setNames(rep(-Inf, 30), all_teams)

  # ---- 2026 actual picks (identical under both systems) ----
  a26_val <- setNames(numeric(nrow(a26_assets)), a26_ids)
  for (r in seq_len(nrow(a26_assets))) {
    own <- a26_assets$owner[r]
    val <- sample_pick_value(a26_assets$fixed_slot[r], draw_idx = d_pick, draw_idx_r2 = d_pick2)
    a26_val[r] <- val
    tv_c[own]  <- tv_c[own] + val
    tn_c[own]  <- tn_c[own] + 1L
    tb_c[own]  <- max(tb_c[own], val)
  }
  tv_n <- tv_c; tn_n <- tn_c; tb_n <- tb_c

  # store per-asset 2026 values (same under both systems). 2026 is locked, so
  # slot == fixed slot, raw value == realized value, and own-slot == slot.
  asset_cur[sim, a26_ids] <- a26_val
  asset_new[sim, a26_ids] <- a26_val
  asset_slot_cur[sim, a26_ids]    <- a26_assets$fixed_slot
  asset_slot_new[sim, a26_ids]    <- a26_assets$fixed_slot
  asset_raw_cur[sim, a26_ids]     <- a26_val
  asset_raw_new[sim, a26_ids]     <- a26_val
  asset_ownslot_cur[sim, a26_ids] <- a26_assets$fixed_slot
  asset_ownslot_new[sim, a26_ids] <- a26_assets$fixed_slot
  asset_convey_cur[sim, a26_ids] <- 1L
  asset_convey_new[sim, a26_ids] <- 1L
  
  # top-pick history per ORIGINAL team for the new restrictions.
  # 2025 (UTA #5) and 2026 actuals seed the look-back.
  top_hist <- setNames(vector("list", 30), all_teams)
  # 2026 actual top-5 original teams: slots 1-5 -> WAS,UTA,MEM,CHI,IND
  for (tm in c("WAS","UTA","MEM","CHI","IND")) {
    top_hist[[tm]]$top5 <- c(top_hist[[tm]]$top5, 2026)
  }
  top_hist[["WAS"]]$no1 <- c(top_hist[["WAS"]]$no1, 2026)
  # 2025 look-back: Jazz picked 5 (top-5), Mavs won 2025 (#1)
  top_hist[["UTA"]]$top5 <- c(top_hist[["UTA"]]$top5, 2025)
  top_hist[["DAL"]]$no1  <- c(top_hist[["DAL"]]$no1, 2025)
  top_hist[["DAL"]]$top5 <- c(top_hist[["DAL"]]$top5, 2025)
  
  team_rank_state <- current_rank_worst0
  obligation_state_c <- normalize_obligation_state()
  obligation_state_n <- normalize_obligation_state()
  
  for (yr in FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT) {
    # ---- simulate next season's full standings order ----
    # Each team draws a desired next-season rank from its row of the 30-rank
    # Markov transition matrix. Independent draws can give two teams the same
    # rank, so teams are sorted by desired rank with a random tiebreaker; this
    # gives exactly one team per rank (a tied team shifts down one rank).
    desired_rank <- setNames(integer(length(all_teams)), all_teams)
    for (tm in all_teams) {
      i <- as.integer(team_rank_state[tm])
      if (is.na(i) || i < 1L || i > N_RANKS) i <- sample(N_RANKS, 1)
      desired_rank[tm] <- sample(N_RANKS, 1, prob = P[i, ])
    }

    rank_order <- tibble(team = all_teams,
                         desired_rank = pmin(pmax(desired_rank, 1L), 30L),
                         tie_break = runif(length(all_teams))) %>%
      arrange(desired_rank, tie_break)

    ord <- rank_order$team                                # worst -> best
    rank_of <- setNames(seq_along(ord), ord)              # 1 = worst overall
    team_rank_state <- rank_of
    
    # ---- CURRENT system seats (14-team lottery) ----
    lot14 <- ord[1:14]
    cp    <- sim_current_lottery()
    slot_c <- setNames(integer(0), character(0))
    for (k in seq_along(lot14)) slot_c[lot14[k]] <- cp[k]
    # non-lottery 15-30 by record (best gets 30)
    nonlot <- ord[15:30]
    for (k in seq_along(nonlot)) slot_c[nonlot[k]] <- 14 + k
    
    # ---- 3-2-1 system seats (16-team lottery) ----
    lot16 <- ord[1:16]
    np    <- sim_321_lottery()
    slot_n <- setNames(integer(0), character(0))
    for (k in seq_along(lot16)) slot_n[lot16[k]] <- np[k]
    nonlot16 <- ord[17:30]
    for (k in seq_along(nonlot16)) slot_n[nonlot16[k]] <- 16 + k
    
    # ---- apply NEW anti-tank restrictions (3-2-1 system only) ----
    # Restrictions look back at the ORIGINAL team's recent top picks:
    #   - no #1 pick in consecutive years    -> earliest legal slot = 2
    #   - no top-5 pick three years running  -> earliest legal slot = 6
    # Slots are refilled in lottery order: each slot goes to the best-placed
    # remaining team that is allowed to take it, so a restricted team drops to
    # its first legal slot and the teams behind it move up one slot. The
    # result is still one team per slot. Restrictions do not change who owns
    # the pick; ownership is resolved below.
    restriction_order <- names(slot_n)[order(slot_n)]
    earliest_legal_slot <- setNames(rep(1L, length(restriction_order)), restriction_order)
    for (tm in restriction_order) {
      if ((yr - 1) %in% top_hist[[tm]]$no1) {
        earliest_legal_slot[tm] <- max(earliest_legal_slot[tm], 2L)
      }
      if (all(c(yr - 1, yr - 2) %in% top_hist[[tm]]$top5)) {
        earliest_legal_slot[tm] <- max(earliest_legal_slot[tm], 6L)
      }
    }

    unseated_teams <- restriction_order
    slot_n_restricted <- setNames(rep(NA_integer_, length(slot_n)), names(slot_n))
    for (seat in seq_along(restriction_order)) {
      legal_idx <- which(earliest_legal_slot[unseated_teams] <= seat)

      if (length(legal_idx) == 0L) {
        # Not reachable under the current rules; keeps the simulation moving
        # if a future rule change makes the assignment infeasible.
        warning(sprintf(
          "No legal team available for restricted slot %d in %d; using original order fallback.",
          seat, yr
        ))
        chosen_idx <- 1L
      } else {
        chosen_idx <- legal_idx[1]
      }

      slot_n_restricted[unseated_teams[chosen_idx]] <- seat
      unseated_teams <- unseated_teams[-chosen_idx]
    }

    if (anyDuplicated(slot_n_restricted) ||
        !identical(sort(as.integer(slot_n_restricted)), seq_along(restriction_order))) {
      warning("Pick restriction reseating produced a non-permutation draft order.")
    }
    slot_n <- slot_n_restricted

    # ---- second-round slots -------------------------------------------------
    # Current system: inverse record for all 30 slots (31 = worst team,
    # 60 = best team). Under 3-2-1, the 16 lottery teams get
    # 47 - final first-round slot (slots 31-46, the inverse of the lottery
    # result), then playoff teams fill 47-60 by inverse record.
    slot2_c <- setNames(rep(NA_integer_, length(all_teams)), all_teams)
    slot2_c[ord] <- 30L + seq_along(ord)

    slot2_n <- setNames(rep(NA_integer_, length(all_teams)), all_teams)
    lottery_teams_n <- names(slot_n)[slot_n <= 16]
    slot2_n[lottery_teams_n] <- 47L - as.integer(slot_n[lottery_teams_n])
    slot2_n[ord[17:30]] <- 46L + seq_along(ord[17:30])
    
    # ---- record realized ORIGINAL-team seats this year ----------------------
    yc <- as.character(yr)
    for (tm in all_teams) {
      if (!is.na(slot_c[tm])) team_slot_cur[sim, tm, yc] <- slot_c[tm]
      if (!is.na(slot_n[tm])) team_slot_new[sim, tm, yc] <- slot_n[tm]
      if (!is.na(slot2_c[tm])) team_slot2_cur[sim, tm, yc] <- slot2_c[tm]
      if (!is.na(slot2_n[tm])) team_slot2_new[sim, tm, yc] <- slot2_n[tm]
      if (!is.na(rank_of[tm])) team_rank_worst[sim, tm, yc] <- rank_of[tm]
    }
    
    # ---- resolve all simple + complex ownership obligations -----------------
    resolved_c <- resolve_pick_owners(slot_c, yr, obligation_state_c)
    owner_by_orig_c <- resolved_c$owner_by_orig
    obligation_state_c <- resolved_c$state

    resolved_n <- resolve_pick_owners(slot_n, yr, obligation_state_n)
    owner_by_orig_n <- resolved_n$owner_by_orig
    obligation_state_n <- resolved_n$state

    owner2_by_orig_c <- resolve_second_pick_owners(
      slot2_c, yr, owner_by_orig_c, slot_c, obligation_state_c
    )

    owner2_by_orig_n <- resolve_second_pick_owners(
      slot2_n, yr, owner_by_orig_n, slot_n, obligation_state_n
    )
    
    # ---- value every possible future pick asset -----------------------------
    # Each original team's pick can be allocated to exactly one owner under each
    # system. Assets whose condition is not met get zero value in that draw; the
    # retained own-pick asset gets value when protections/swaps do not convey.
    val_c <- value_allocated_future_assets(
      sim = sim, yr = yr, draft_round = 1L, slots = slot_c, owner_by_orig = owner_by_orig_c,
      d_pick = d_pick, d_pick2 = d_pick2,
      team_value = tv_c, team_n = tn_c, team_best = tb_c,
      system = "cur"
    )
    tv_c <- val_c$team_value; tn_c <- val_c$team_n; tb_c <- val_c$team_best
    
    val_c2 <- value_allocated_future_assets(
      sim = sim, yr = yr, draft_round = 2L, slots = slot2_c, owner_by_orig = owner2_by_orig_c,
      d_pick = d_pick, d_pick2 = d_pick2,
      team_value = tv_c, team_n = tn_c, team_best = tb_c,
      system = "cur"
    )
    tv_c <- val_c2$team_value; tn_c <- val_c2$team_n; tb_c <- val_c2$team_best
    
    val_n <- value_allocated_future_assets(
      sim = sim, yr = yr, draft_round = 1L, slots = slot_n, owner_by_orig = owner_by_orig_n,
      d_pick = d_pick, d_pick2 = d_pick2,
      team_value = tv_n, team_n = tn_n, team_best = tb_n,
      system = "new"
    )
    tv_n <- val_n$team_value; tn_n <- val_n$team_n; tb_n <- val_n$team_best
    
    val_n2 <- value_allocated_future_assets(
      sim = sim, yr = yr, draft_round = 2L, slots = slot2_n, owner_by_orig = owner2_by_orig_n,
      d_pick = d_pick, d_pick2 = d_pick2,
      team_value = tv_n, team_n = tn_n, team_best = tb_n,
      system = "new"
    )
    tv_n <- val_n2$team_value; tn_n <- val_n2$team_n; tb_n <- val_n2$team_best
    
    # ---- update top-pick history from the 3-2-1 seats (original teams) ----
    for (tm in all_teams) {
      sl <- slot_n[tm]
      if (!is.na(sl)) {
        if (sl == 1) top_hist[[tm]]$no1  <- c(top_hist[[tm]]$no1, yr)
        if (sl <= 5) top_hist[[tm]]$top5 <- c(top_hist[[tm]]$top5, yr)
      }
    }
  }
  
  results[[sim]] <- tibble(
    team = all_teams,
    current_total = tv_c, new_total = tv_n,
    current_n = tn_c, new_n = tn_n,
    current_best = ifelse(is.finite(tb_c), tb_c, NA_real_),
    new_best = ifelse(is.finite(tb_n), tb_n, NA_real_),
    sim_id = sim
  )
  if (sim %% 100 == 0) cat(sprintf("  %d / %d\n", sim, N_SIMS))
}

all_res <- bind_rows(results)
cat("Simulations complete.\n")

# ---- structural validation tests before export ------------------------------
# Each check stops the script if it fails:
#   - round-1 assets only use slots 1-30; round-2 assets only use 31-60
#   - 3-2-1 second round: each lottery team's slot = 47 - final first-round slot
#   - every sim-year standings order is a 1:30 permutation
#   - non-lottery teams keep inverse-record slots (15-30 current, 17-30 3-2-1)
#   - every pick asset has round 1 or 2
r1_cols <- pick_assets$round == 1L
r2_cols <- pick_assets$round == 2L

r1_slots <- as.vector(asset_slot_new[, r1_cols, drop = FALSE])
r2_slots <- as.vector(asset_slot_new[, r2_cols, drop = FALSE])

bad_first <- sum(!is.na(r1_slots) & !(r1_slots %in% 1:30))
bad_second <- sum(!is.na(r2_slots) & !(r2_slots %in% 31:60))
if (bad_first > 0) stop("Round-1 assets have non-1:30 slots.", call. = FALSE)
if (bad_second > 0) {
  bad_second_cols <- which(r2_cols)[colSums(!is.na(asset_slot_new[, r2_cols, drop = FALSE]) &
                                              !(asset_slot_new[, r2_cols, drop = FALSE] %in% 31:60)) > 0]
  bad_examples <- head(pick_assets$asset_id[bad_second_cols], 10)
  stop(sprintf(
    "Round-2 assets have non-31:60 slots. This usually means a first-round slot write leaked into round-2 asset columns. Examples: %s",
    paste(bad_examples, collapse = ", ")
  ), call. = FALSE)
}

# 3-2-1 second-round inversion: for every projected sim-year, each lottery
# team's second-round slot should equal 47 - final first-round slot.
inv_bad <- 0L
for (yc in as.character(proj_years)) {
  fr <- team_slot_new[, , yc]
  sr <- team_slot2_new[, , yc]
  lot_idx <- which(!is.na(fr) & fr <= 16, arr.ind = TRUE)
  if (nrow(lot_idx) > 0) {
    inv_bad <- inv_bad + sum(sr[lot_idx] != 47L - fr[lot_idx], na.rm = TRUE)
  }
}
if (inv_bad > 0) stop("3-2-1 second-round inversion validation failed.", call. = FALSE)

# Every projected sim-year must contain exactly one team in each of the 30
# rank slots. This guards against duplicate/gap errors after categorical rank
# draws are resolved into a standings permutation.
rank_bad <- 0L
old_nonlot_bad <- 0L
new_nonlot_bad <- 0L
for (yc in as.character(proj_years)) {
  rk <- team_rank_worst[, , yc]
  sc <- team_slot_cur[, , yc]
  sn <- team_slot_new[, , yc]
  for (rr in seq_len(nrow(rk))) {
    if (!identical(sort(as.integer(rk[rr, ])), 1:30)) rank_bad <- rank_bad + 1L

    # Old/current system: non-lottery teams receive picks 15-30 by inverse
    # record, so simulated rank_worst 15 maps to pick 15 and rank_worst 30
    # maps to pick 30.
    old_idx <- which(rk[rr, ] >= 15)
    old_nonlot_bad <- old_nonlot_bad + sum(sc[rr, old_idx] != rk[rr, old_idx], na.rm = TRUE)

    # 3-2-1 system: ranks 17-30 are non-lottery and keep inverse-record
    # slots 17-30 after the 16-team lottery is drawn.
    new_idx <- which(rk[rr, ] >= 17)
    new_nonlot_bad <- new_nonlot_bad + sum(sn[rr, new_idx] != rk[rr, new_idx], na.rm = TRUE)
  }
}
if (rank_bad > 0) stop("Rank-state validation failed: at least one sim-year is not a 1:30 permutation.", call. = FALSE)
if (old_nonlot_bad > 0) stop("Old-system non-lottery inverse-record validation failed.", call. = FALSE)
if (new_nonlot_bad > 0) stop("3-2-1 non-lottery inverse-record validation failed.", call. = FALSE)

if (any(is.na(pick_assets$round)) || any(!pick_assets$round %in% c(1L, 2L))) {
  stop("pick_assets has missing or invalid round values.", call. = FALSE)
}

round_validation_passed <- TRUE
cat("Round-aware slot/allocation validation passed.\n")


# PER-PICK SUMMARIES + DOWNSAMPLED JOINT DRAWS (for Single Pick & Trade tabs)
# Replace any NA (asset not valued in a sim — e.g. an own pick that was traded
# away that year, which shouldn't happen, or a non-existent combo) with 0.
asset_cur[is.na(asset_cur)] <- 0
asset_new[is.na(asset_new)] <- 0
# raw/slot stores: leave slots as NA where a pick had no seat (e.g. own pick
# traded away that year); raw values default to 0 so protection math is safe.
asset_raw_cur[is.na(asset_raw_cur)] <- 0
asset_raw_new[is.na(asset_raw_new)] <- 0

# Expected Asset Value (EV) matrices: same simulated pick slots / conveyance
# events, but valued with the posterior mean slot curve instead of a sampled
# player-level outcome draw. These are the team / pick values shown when
# the app toggles to "Expected Asset Value" and match the left-side Trade
# Machine interpretation.
asset_cur_ev <- asset_value_mean_from_slots(asset_slot_cur, asset_convey_cur)
asset_new_ev <- asset_value_mean_from_slots(asset_slot_new, asset_convey_new)

# Per-asset distribution summary (sampled player-outcome value + 90% credible interval).
pick_value_summary <- pick_assets %>%
  mutate(
    cur_mean = colMeans(asset_cur)[asset_id],
    cur_q05  = apply(asset_cur, 2, quantile, 0.05)[asset_id],
    cur_q50  = apply(asset_cur, 2, quantile, 0.50)[asset_id],
    cur_q95  = apply(asset_cur, 2, quantile, 0.95)[asset_id],
    cur_sd   = apply(asset_cur, 2, sd)[asset_id],
    new_mean = colMeans(asset_new)[asset_id],
    new_q05  = apply(asset_new, 2, quantile, 0.05)[asset_id],
    new_q50  = apply(asset_new, 2, quantile, 0.50)[asset_id],
    new_q95  = apply(asset_new, 2, quantile, 0.95)[asset_id],
    new_sd   = apply(asset_new, 2, sd)[asset_id],
    cur_convey_prob = colMeans(asset_convey_cur)[asset_id],
    new_convey_prob = colMeans(asset_convey_new)[asset_id],
    delta    = new_mean - cur_mean
  )

# Per-asset Expected Asset Value summary. Same columns as pick_value_summary,
# but based on posterior mean slot values instead of sampled player outcomes.
pick_value_ev_summary <- pick_assets %>%
  mutate(
    cur_mean = colMeans(asset_cur_ev)[asset_id],
    cur_q05  = apply(asset_cur_ev, 2, quantile, 0.05)[asset_id],
    cur_q50  = apply(asset_cur_ev, 2, quantile, 0.50)[asset_id],
    cur_q95  = apply(asset_cur_ev, 2, quantile, 0.95)[asset_id],
    cur_sd   = apply(asset_cur_ev, 2, sd)[asset_id],
    new_mean = colMeans(asset_new_ev)[asset_id],
    new_q05  = apply(asset_new_ev, 2, quantile, 0.05)[asset_id],
    new_q50  = apply(asset_new_ev, 2, quantile, 0.50)[asset_id],
    new_q95  = apply(asset_new_ev, 2, quantile, 0.95)[asset_id],
    new_sd   = apply(asset_new_ev, 2, sd)[asset_id],
    cur_convey_prob = colMeans(asset_convey_cur)[asset_id],
    new_convey_prob = colMeans(asset_convey_new)[asset_id],
    delta    = new_mean - cur_mean
  )


# Summed display-entitlement matrices. These collapse mutually exclusive or
# grouped internal assets into one user-facing pick, without changing the team
# portfolio totals already computed above.
display_asset_cur_full <- build_display_draw_matrix(asset_cur, pick_display_members, pick_display_assets)
display_asset_new_full <- build_display_draw_matrix(asset_new, pick_display_members, pick_display_assets)
display_asset_cur_ev_full <- build_display_draw_matrix(asset_cur_ev, pick_display_members, pick_display_assets)
display_asset_new_ev_full <- build_display_draw_matrix(asset_new_ev, pick_display_members, pick_display_assets)
display_convey_cur_full <- build_display_convey_matrix(asset_convey_cur, pick_display_members, pick_display_assets)
display_convey_new_full <- build_display_convey_matrix(asset_convey_new, pick_display_members, pick_display_assets)

# Drop display-only entitlements that never convey in either system. These are
# internal allocation rows, not real picks a user can trade/value. This prevents
# fully assigned-away own/retained rows from appearing as zero-EV picks in the
# Single Pick and Trade Machine tabs.
display_convey_audit <- tibble(
  display_asset_id = pick_display_assets$display_asset_id,
  cur_expected_pick_count = colMeans(display_convey_cur_full)[pick_display_assets$display_asset_id],
  new_expected_pick_count = colMeans(display_convey_new_full)[pick_display_assets$display_asset_id]
) %>%
  mutate(
    cur_expected_pick_count = coalesce(cur_expected_pick_count, 0),
    new_expected_pick_count = coalesce(new_expected_pick_count, 0),
    active_display_asset = cur_expected_pick_count > 0 | new_expected_pick_count > 0
  )

hidden_zero_convey_display_assets <- pick_display_assets %>%
  left_join(display_convey_audit, by = "display_asset_id") %>%
  filter(!.data$active_display_asset)

active_display_ids <- display_convey_audit %>%
  filter(.data$active_display_asset) %>%
  pull(display_asset_id)

if (nrow(hidden_zero_convey_display_assets) > 0) {
  cat(sprintf(
    "Hiding %d zero-conveyance display-only pick rows from user-facing selectors\n",
    nrow(hidden_zero_convey_display_assets)
  ))
}

pick_display_assets <- pick_display_assets %>%
  filter(.data$display_asset_id %in% active_display_ids)
pick_display_members <- pick_display_members %>%
  semi_join(pick_display_assets %>% select(display_asset_id), by = "display_asset_id")

display_asset_cur_full <- keep_display_cols(display_asset_cur_full, active_display_ids)
display_asset_new_full <- keep_display_cols(display_asset_new_full, active_display_ids)
display_asset_cur_ev_full <- keep_display_cols(display_asset_cur_ev_full, active_display_ids)
display_asset_new_ev_full <- keep_display_cols(display_asset_new_ev_full, active_display_ids)
display_convey_cur_full <- keep_display_cols(display_convey_cur_full, active_display_ids)
display_convey_new_full <- keep_display_cols(display_convey_new_full, active_display_ids)

pick_display_value_summary <- pick_display_assets %>%
  mutate(
    cur_mean = colMeans(display_asset_cur_full)[display_asset_id],
    cur_q05  = apply(display_asset_cur_full, 2, quantile, 0.05)[display_asset_id],
    cur_q50  = apply(display_asset_cur_full, 2, quantile, 0.50)[display_asset_id],
    cur_q95  = apply(display_asset_cur_full, 2, quantile, 0.95)[display_asset_id],
    cur_sd   = apply(display_asset_cur_full, 2, sd)[display_asset_id],
    new_mean = colMeans(display_asset_new_full)[display_asset_id],
    new_q05  = apply(display_asset_new_full, 2, quantile, 0.05)[display_asset_id],
    new_q50  = apply(display_asset_new_full, 2, quantile, 0.50)[display_asset_id],
    new_q95  = apply(display_asset_new_full, 2, quantile, 0.95)[display_asset_id],
    new_sd   = apply(display_asset_new_full, 2, sd)[display_asset_id],
    cur_convey_prob = colMeans(display_convey_cur_full > 0)[display_asset_id],
    new_convey_prob = colMeans(display_convey_new_full > 0)[display_asset_id],
    cur_expected_pick_count = colMeans(display_convey_cur_full)[display_asset_id],
    new_expected_pick_count = colMeans(display_convey_new_full)[display_asset_id],
    delta = new_mean - cur_mean
  )

pick_display_value_ev_summary <- pick_display_assets %>%
  mutate(
    cur_mean = colMeans(display_asset_cur_ev_full)[display_asset_id],
    cur_q05  = apply(display_asset_cur_ev_full, 2, quantile, 0.05)[display_asset_id],
    cur_q50  = apply(display_asset_cur_ev_full, 2, quantile, 0.50)[display_asset_id],
    cur_q95  = apply(display_asset_cur_ev_full, 2, quantile, 0.95)[display_asset_id],
    cur_sd   = apply(display_asset_cur_ev_full, 2, sd)[display_asset_id],
    new_mean = colMeans(display_asset_new_ev_full)[display_asset_id],
    new_q05  = apply(display_asset_new_ev_full, 2, quantile, 0.05)[display_asset_id],
    new_q50  = apply(display_asset_new_ev_full, 2, quantile, 0.50)[display_asset_id],
    new_q95  = apply(display_asset_new_ev_full, 2, quantile, 0.95)[display_asset_id],
    new_sd   = apply(display_asset_new_ev_full, 2, sd)[display_asset_id],
    cur_convey_prob = colMeans(display_convey_cur_full > 0)[display_asset_id],
    new_convey_prob = colMeans(display_convey_new_full > 0)[display_asset_id],
    cur_expected_pick_count = colMeans(display_convey_cur_full)[display_asset_id],
    new_expected_pick_count = colMeans(display_convey_new_full)[display_asset_id],
    delta = new_mean - cur_mean
  )

# Downsample the joint per-sim matrices so the dashboard can compute trade
# deltas and P(team A nets more wins) with proper within-sim correlation,
# without shipping the full 10k-row matrices.
n_keep   <- min(2000, N_SIMS)
keep_idx <- sort(sample(N_SIMS, n_keep))
asset_cur_draws <- asset_cur[keep_idx, , drop = FALSE]
asset_new_draws <- asset_new[keep_idx, , drop = FALSE]
asset_cur_ev_draws <- asset_cur_ev[keep_idx, , drop = FALSE]
asset_new_ev_draws <- asset_new_ev[keep_idx, , drop = FALSE]

# Downsample the slot / raw / curve stores on the SAME kept sims so the Trade
# Machine can recompute hypothetical protections & swaps with correct
# within-sim correlation.
asset_slot_cur_draws    <- asset_slot_cur[keep_idx, , drop = FALSE]
asset_slot_new_draws    <- asset_slot_new[keep_idx, , drop = FALSE]
asset_raw_cur_draws     <- asset_raw_cur[keep_idx, , drop = FALSE]
asset_raw_new_draws     <- asset_raw_new[keep_idx, , drop = FALSE]
asset_ownslot_cur_draws <- asset_ownslot_cur[keep_idx, , drop = FALSE]
asset_ownslot_new_draws <- asset_ownslot_new[keep_idx, , drop = FALSE]
asset_convey_cur_draws  <- asset_convey_cur[keep_idx, , drop = FALSE]
asset_convey_new_draws  <- asset_convey_new[keep_idx, , drop = FALSE]
team_slot_cur_draws     <- team_slot_cur[keep_idx, , , drop = FALSE]
team_slot_new_draws     <- team_slot_new[keep_idx, , , drop = FALSE]
team_slot2_cur_draws    <- team_slot2_cur[keep_idx, , , drop = FALSE]
team_slot2_new_draws    <- team_slot2_new[keep_idx, , , drop = FALSE]
team_rank_worst_draws   <- team_rank_worst[keep_idx, , , drop = FALSE]
sim_curve_par_draws     <- sim_curve_par[keep_idx, , drop = FALSE]

display_asset_cur_draws <- build_display_draw_matrix(asset_cur_draws, pick_display_members, pick_display_assets)
display_asset_new_draws <- build_display_draw_matrix(asset_new_draws, pick_display_members, pick_display_assets)
display_asset_cur_ev_draws <- build_display_draw_matrix(asset_cur_ev_draws, pick_display_members, pick_display_assets)
display_asset_new_ev_draws <- build_display_draw_matrix(asset_new_ev_draws, pick_display_members, pick_display_assets)
display_convey_cur_draws <- build_display_convey_matrix(asset_convey_cur_draws, pick_display_members, pick_display_assets)
display_convey_new_draws <- build_display_convey_matrix(asset_convey_new_draws, pick_display_members, pick_display_assets)

cat(sprintf("Stored %d joint draws per asset for trade analysis\n", n_keep))


## ═════════════════════════════════════════════════════════════════════════════
## 13 - SUMMARIZE + LOTTERY ODDS + EXPORT --------------------------------------
## ═════════════════════════════════════════════════════════════════════════════

tier_map <- current_standings %>%
  transmute(abbr, tier = as.character(tier), wins, losses, overall_rank)

# Team totals per simulation under Expected Pick Value: sum of each team's
# asset EPV columns; best = largest single-asset EPV. Pick counts are the same
# allocations as the outcome draws, so they come from all_res.
team_ev_res <- map_dfr(all_teams, function(tm) {
  cols <- which(pick_assets$owner == tm)
  cur_sub <- asset_cur_ev[, cols, drop = FALSE]
  new_sub <- asset_new_ev[, cols, drop = FALSE]
  tibble(team          = tm,
         sim_id        = seq_len(nrow(cur_sub)),
         current_total = rowSums(cur_sub),
         new_total     = rowSums(new_sub),
         current_best  = if (length(cols) > 0) apply(cur_sub, 1, max, na.rm = TRUE) else NA_real_,
         new_best      = if (length(cols) > 0) apply(new_sub, 1, max, na.rm = TRUE) else NA_real_)
}) %>%
  left_join(all_res %>% select(team, sim_id, current_n, new_n),
            by = c("team", "sim_id"))

# Team summaries for both value modes:
#   - "outcome": sampled player outcomes (all_res)
#   - "ev":      Expected Pick Value (team_ev_res)
team_value_summary <- bind_rows(all_res %>% mutate(value_mode = "outcome"),
                                team_ev_res %>% mutate(value_mode = "ev")) %>%
  group_by(value_mode, team) %>%
  summarise(
    current_mean   = mean(current_total),
    current_median = median(current_total),
    current_sd     = sd(current_total),
    current_q05    = quantile(current_total, 0.05),
    current_q25    = quantile(current_total, 0.25),
    current_q75    = quantile(current_total, 0.75),
    current_q95    = quantile(current_total, 0.95),
    new_mean       = mean(new_total),
    new_median     = median(new_total),
    new_sd         = sd(new_total),
    new_q05        = quantile(new_total, 0.05),
    new_q25        = quantile(new_total, 0.25),
    new_q75        = quantile(new_total, 0.75),
    new_q95        = quantile(new_total, 0.95),
    n_picks_mean   = mean(current_n),
    best_current   = mean(current_best, na.rm = TRUE),
    best_new       = mean(new_best, na.rm = TRUE),
    delta_value    = mean(new_total) - mean(current_total),
    delta_pct      = (mean(new_total) / pmax(mean(current_total), 0.01) - 1) * 100,
    delta_quality  = mean(new_best, na.rm = TRUE) - mean(current_best, na.rm = TRUE),
    sigma_change   = sd(new_total) - sd(current_total),
    .groups        = "drop"
  ) %>%
  left_join(tier_map, by = c("team" = "abbr"))

summary_df <- team_value_summary %>%
  filter(value_mode == "outcome") %>%
  select(-value_mode) %>%
  arrange(desc(delta_value))

summary_ev <- team_value_summary %>%
  filter(value_mode == "ev") %>%
  select(-value_mode) %>%
  arrange(desc(delta_value))

# ---- lottery odds tables (independent of team identities) ----
cat("\n--- Computing Lottery Odds Tables ---\n")
cur_sims <- matrix(0L, N_LOT, 14)
new_sims <- matrix(0L, N_LOT, 16)
for (i in 1:N_LOT) {
  cur_sims[i, ] <- sim_current_lottery()
  new_sims[i, ] <- sim_321_lottery()
}

lottery_seed_tiers <- tibble(
  seed = 1:16,
  lottery_tier = case_when(
    seed <= 3  ~ "relegation",
    seed <= 10 ~ "nonplayin",
    seed <= 14 ~ "playin_seed",
    TRUE       ~ "playin_loser"
  ),
  lottery_tier_label = case_when(
    lottery_tier == "relegation"   ~ "Three worst",
    lottery_tier == "nonplayin"    ~ "4th-10th worst",
    lottery_tier == "playin_seed"  ~ "9/10 Play-In seeds",
    lottery_tier == "playin_loser" ~ "7v8 Play-In losers"
  )
)

# Pick distribution per lottery seed: expected pick, P(#1 / top 3 / top 5 /
# top 10), and Monte Carlo standard errors. Long format: one row per
# (system, seed, lottery draw).
lottery_dist <- bind_rows(tibble(system = "Current",
                                 seed   = rep(1:14, each = N_LOT),
                                 pick   = as.vector(cur_sims)),
                          tibble(system = "Proposed 3-2-1",
                                 seed   = rep(1:16, each = N_LOT),
                                 pick   = as.vector(new_sims))) %>%
  group_by(system, seed) %>%
  summarise(expected_pick    = mean(pick),
            expected_pick_se = sd(pick) / sqrt(n()),
            prob_no1         = mean(pick == 1),
            prob_top3        = mean(pick <= 3),
            prob_top5        = mean(pick <= 5),
            prob_top10       = mean(pick <= 10),
            prob_no1_se      = sqrt(prob_no1 * (1 - prob_no1) / n()),
            prob_top3_se     = sqrt(prob_top3 * (1 - prob_top3) / n()),
            prob_top5_se     = sqrt(prob_top5 * (1 - prob_top5) / n()),
            prob_top10_se    = sqrt(prob_top10 * (1 - prob_top10) / n()),
            .groups          = "drop") %>%
  relocate(seed, .before = system) %>%
  left_join(lottery_seed_tiers, by = "seed")

# Published aggregate 3-2-1 odds table. These are tier-level values because
# teams with the same ball count / floor treatment are symmetric within tier.
official_321_tier_odds <- tribble(
  ~lottery_tier,  ~lottery_tier_label,   ~seed_midpoint, ~official_prob_no1, ~official_prob_top3, ~official_prob_top5, ~official_prob_top10, ~official_expected_pick,
  "relegation",   "Three worst",              2.0,              0.054,              0.16,               0.28,                0.61,                 8.1,
  "nonplayin",    "4th-10th worst",           7.0,              0.081,              0.24,               0.39,                0.73,                 7.4,
  "playin_seed",  "9/10 Play-In seeds",      12.5,              0.054,              0.16,               0.28,                0.59,                 9.1,
  "playin_loser", "7v8 Play-In losers",      15.5,              0.027,              0.08,               0.15,                0.35,                11.7
)

lottery_tier_validation <- lottery_dist %>%
  filter(system == "Proposed 3-2-1") %>%
  group_by(lottery_tier, lottery_tier_label) %>%
  summarise(
    seed_min = min(seed),
    seed_max = max(seed),
    sim_expected_pick = mean(expected_pick),
    sim_prob_no1 = mean(prob_no1),
    sim_prob_top3 = mean(prob_top3),
    sim_prob_top5 = mean(prob_top5),
    sim_prob_top10 = mean(prob_top10),
    sim_expected_pick_mc_se = sqrt(sum(expected_pick_se^2)) / n(),
    sim_prob_no1_mc_se = sqrt(sum(prob_no1_se^2)) / n(),
    sim_prob_top3_mc_se = sqrt(sum(prob_top3_se^2)) / n(),
    sim_prob_top5_mc_se = sqrt(sum(prob_top5_se^2)) / n(),
    sim_prob_top10_mc_se = sqrt(sum(prob_top10_se^2)) / n(),
    .groups = "drop"
  ) %>%
  left_join(official_321_tier_odds, by = c("lottery_tier", "lottery_tier_label")) %>%
  mutate(
    diff_expected_pick = sim_expected_pick - official_expected_pick,
    diff_prob_no1 = sim_prob_no1 - official_prob_no1,
    diff_prob_top3 = sim_prob_top3 - official_prob_top3,
    diff_prob_top5 = sim_prob_top5 - official_prob_top5,
    diff_prob_top10 = sim_prob_top10 - official_prob_top10
  ) %>%
  arrange(seed_min)

cat("\n  [validate] 3-2-1 lottery odds vs published tier table\n")
print(lottery_tier_validation %>%
        transmute(
          tier = lottery_tier_label,
          sim_no1 = round(100 * sim_prob_no1, 2),
          official_no1 = round(100 * official_prob_no1, 1),
          sim_top3 = round(100 * sim_prob_top3, 2),
          official_top3 = round(100 * official_prob_top3, 1),
          sim_top5 = round(100 * sim_prob_top5, 2),
          official_top5 = round(100 * official_prob_top5, 1),
          sim_top10 = round(100 * sim_prob_top10, 2),
          official_top10 = round(100 * official_prob_top10, 1),
          sim_avg_pick = round(sim_expected_pick, 2),
          official_avg_pick = official_expected_pick
        ))

# ---- pick value curve for the dashboard ------------------------------------
# expected_war and ev_q05/ev_q95 show uncertainty around the posterior expected
# value of the pick slot. outcome_q10/outcome_q90 show the asymmetric player-
# outcome distribution for that pick slot. These are intentionally separate:
# EAV uncertainty is a curve-estimation question; outcome quantiles are the
# realized-player risk/upside question.
pick_curve <- bind_rows(
  tibble(
    pick = 1:30,
    round = 1L,
    expected_war = colMeans(pick_mu_draws),
    ev_q05 = apply(pick_mu_draws, 2, quantile, 0.05, na.rm = TRUE),
    ev_q50 = apply(pick_mu_draws, 2, quantile, 0.50, na.rm = TRUE),
    ev_q95 = apply(pick_mu_draws, 2, quantile, 0.95, na.rm = TRUE),
    outcome_q10 = apply(pick_outcome_draws, 2, quantile, 0.10, na.rm = TRUE),
    outcome_q90 = apply(pick_outcome_draws, 2, quantile, 0.90, na.rm = TRUE),
    # Keep war_sd for backward compatibility / diagnostics, but app.R no longer
    # uses it as the curve ribbon.
    war_sd = colMeans(pick_sd_draws),
    p_play = 1,
    p_play_q05 = 1,
    p_play_q50 = 1,
    p_play_q95 = 1
  ),
  tibble(
    pick = 31:60,
    round = 2L,
    expected_war = colMeans(pick2_mu_draws),
    ev_q05 = apply(pick2_mu_draws, 2, quantile, 0.05, na.rm = TRUE),
    ev_q50 = apply(pick2_mu_draws, 2, quantile, 0.50, na.rm = TRUE),
    ev_q95 = apply(pick2_mu_draws, 2, quantile, 0.95, na.rm = TRUE),
    outcome_q10 = apply(pick2_outcome_draws, 2, quantile, 0.10, na.rm = TRUE),
    outcome_q90 = apply(pick2_outcome_draws, 2, quantile, 0.90, na.rm = TRUE),
    # Keep war_sd for backward compatibility / diagnostics, but app.R no longer
    # uses it as the curve ribbon.
    war_sd = colMeans(pick2_sd_draws),
    p_play = colMeans(pick2_p_play_draws),
    p_play_q05 = apply(pick2_p_play_draws, 2, quantile, 0.05, na.rm = TRUE),
    p_play_q50 = apply(pick2_p_play_draws, 2, quantile, 0.50, na.rm = TRUE),
    p_play_q95 = apply(pick2_p_play_draws, 2, quantile, 0.95, na.rm = TRUE)
  )
)

# attach empirical slot means for overlay
pick_curve <- pick_curve %>%
  left_join(
    bind_rows(
      pick_slot_data %>% transmute(pick, emp_mean = war_mean, emp_p_play = 1),
      pick_slot_data_r2 %>% transmute(pick, emp_mean = war_mean, emp_p_play = p_play_emp)
    ),
    by = "pick"
  )

# ---- diagnostics bundle for the dashboard ----
stan_diagnostics <- list(
  pick_model = list(
    alpha       = round(mean(pick_draws$alpha), 2),
    beta        = round(mean(pick_draws$beta), 4),
    gamma       = round(mean(pick_draws$gamma), 2),
    tau_eps          = round(mean(pick_draws$tau_eps), 4),
    tau_sd           = round(mean(pick_draws$tau_sd), 4),
    tau_r            = round(mean(pick_draws$tau_r), 4),
    sigma_pick_1     = round(mean(pick_sd_draws[, 1]), 2),
    sigma_pick_5     = round(mean(pick_sd_draws[, 5]), 2),
    sigma_pick_10    = round(mean(pick_sd_draws[, 10]), 2),
    sigma_pick_30    = round(mean(pick_sd_draws[, 30]), 2),
    n_players   = nrow(pick_fit_data),
    max_rhat    = round(max(pick_diag$rhat, na.rm = TRUE), 4),
    min_ess     = round(min(pick_diag$ess_bulk, na.rm = TRUE)),
    ppc_cover   = round(mean(ppc_tbl$covered), 3),
    ppc_level   = "player rows",
    loo_best_model = pick_loo_compare_tbl$model[1],
    loo_compare = pick_loo_compare_tbl,
    loo_summary = pick_loo_summary,
    curve_type  = "Bayesian ex-Gaussian: strictly decreasing mean curve (power-law gaps with adjacent-pick random-walk departures); outcome SD and skew follow second-order random walks (smooth trends across picks)"
  ),
  pick2_model = list(
    eta_31           = round(mean(pick2_draws$eta_31), 3),
    tau_pi          = round(mean(pick2_draws$tau_pi), 4),
    p_play_31        = round(mean(pick2_p_play_draws[, 1]), 3),
    p_play_45        = round(mean(pick2_p_play_draws[, 15]), 3),
    p_play_60        = round(mean(pick2_p_play_draws[, 30]), 3),
    cond_war_mean_31  = round(mean(pick2_cond_mu_draws[, 1]), 2),
    cond_war_mean_45  = round(mean(pick2_cond_mu_draws[, 15]), 2),
    cond_war_mean_60  = round(mean(pick2_cond_mu_draws[, 30]), 2),
    sigma_pick_31    = round(mean(pick2_sd_draws[, 1]), 2),
    sigma_pick_45    = round(mean(pick2_sd_draws[, 15]), 2),
    sigma_pick_60    = round(mean(pick2_sd_draws[, 30]), 2),
    n_players        = nrow(pick_fit_data_r2),
    played_rate      = round(mean(pick_fit_data_r2$played == 1), 3),
    max_rhat         = round(max(pick2_diag$rhat, na.rm = TRUE), 4),
    min_ess          = round(min(pick2_diag$ess_bulk, na.rm = TRUE)),
    ppc_cover        = round(mean(ppc_tbl_r2$covered), 3),
    ppc_level        = "round-2 hurdle posterior predictive player rows",
    mean_play_31     = round(mean(pick2_cond_mu_draws[, 1]), 2),
    mean_play_60     = round(mean(pick2_cond_mu_draws[, 30]), 2),
    tau_eps          = round(mean(pick2_draws$tau_eps), 4),
    loo_summary      = tibble(
      model = "round2_hurdle_mixture",
      elpd_loo = loo_pick_r2$estimates["elpd_loo", "Estimate"],
      p_loo = loo_pick_r2$estimates["p_loo", "Estimate"],
      looic = loo_pick_r2$estimates["looic", "Estimate"],
      max_pareto_k = max(loo::pareto_k_values(loo_pick_r2), na.rm = TRUE)
    ),
    curve_type = "Bayesian second-round hurdle: adjacent-pick P(play), a strictly decreasing pooled-gap expected-value curve, and fringe/contributor played outcomes (Normal fringe + exponential contributor upside; share, upside and spread follow adjacent-pick random walks)"
  ),
  markov_model = list(
    state_type    = "30-rank smoothed softmax",
    n_transitions = sum(rank_counts_mat),
    n_seasons     = n_distinct(all_standings$season),
    lambda2       = round(mc_diag$lambda2, 3),
    mixing_time   = round(mc_diag$mixing_time, 1),
    tier_lambda2  = round(mc_diag_tier$lambda2, 3),
    tier_mixing_time = round(mc_diag_tier$mixing_time, 1),
    row_sum_error = round(max(abs(rowSums(post_rank_trans) - 1)), 8),
    rank_surface_smoothness = rank_transition_smoothness
  )
)

dashboard_data <- list(
  summary           = summary_df,
  summary_ev        = summary_ev,
  lottery_dist      = lottery_dist,
  lottery_tier_validation = lottery_tier_validation,
  official_321_tier_odds = official_321_tier_odds,
  pick_curve        = pick_curve,
  # Backward-compatible tier transition display, derived from the production
  # 30-rank model. Full rank-state objects are exported below.
  transition_matrix = post_tier_trans,
  transition_counts = tier_counts_mat,
  stationary        = setNames(as.numeric(mc_diag_tier$stationary), TIERS),
  rank_transition_matrix = post_rank_trans,
  rank_transition_counts = rank_counts_mat,
  rank_stationary        = setNames(as.numeric(mc_diag$stationary), RANK_STATE_LABELS),
  team_rank_worst_draws  = team_rank_worst_draws,
  tier_balls        = TIER_BALLS,
  tier_sizes        = TIER_SIZES,
  actual_2026       = bind_rows(actual_2026_order, actual_2026_second_order),
  actual_2026_first = actual_2026_order,
  actual_2026_second = actual_2026_second_order,
  traded_future     = traded_future,
  traded_second     = traded_second,
  complex_future_groups = complex_future_groups,
  complex_future_assets = complex_future_assets,
  complex_second_groups = complex_second_groups,
  complex_second_assets = complex_second_assets,
  owned_future      = owned_future,
  pick_assets       = pick_assets,
  pick_value_summary = pick_value_summary,
  pick_value_ev_summary = pick_value_ev_summary,
  pick_display_assets = pick_display_assets,
  pick_display_members = pick_display_members,
  pick_display_value_summary = pick_display_value_summary,
  pick_display_value_ev_summary = pick_display_value_ev_summary,
  hidden_zero_convey_display_assets = hidden_zero_convey_display_assets,
  asset_cur_draws   = asset_cur_draws,
  asset_new_draws   = asset_new_draws,
  asset_cur_ev_draws = asset_cur_ev_draws,
  asset_new_ev_draws = asset_new_ev_draws,
  display_asset_cur_draws = display_asset_cur_draws,
  display_asset_new_draws = display_asset_new_draws,
  display_asset_cur_ev_draws = display_asset_cur_ev_draws,
  display_asset_new_ev_draws = display_asset_new_ev_draws,
  display_convey_cur_draws = display_convey_cur_draws,
  display_convey_new_draws = display_convey_new_draws,
  asset_slot_cur_draws    = asset_slot_cur_draws,
  asset_slot_new_draws    = asset_slot_new_draws,
  asset_raw_cur_draws     = asset_raw_cur_draws,
  asset_raw_new_draws     = asset_raw_new_draws,
  asset_ownslot_cur_draws = asset_ownslot_cur_draws,
  asset_ownslot_new_draws = asset_ownslot_new_draws,
  asset_convey_cur_draws  = asset_convey_cur_draws,
  asset_convey_new_draws  = asset_convey_new_draws,
  team_slot_cur_draws     = team_slot_cur_draws,
  team_slot_new_draws     = team_slot_new_draws,
  team_slot2_cur_draws    = team_slot2_cur_draws,
  team_slot2_new_draws    = team_slot2_new_draws,
  sim_curve_par_draws     = sim_curve_par_draws,
  proj_years              = proj_years,
  stan_diagnostics  = stan_diagnostics,
  ppc_tbl_r2 = ppc_tbl_r2,
  ppc_play_pick_r2 = ppc_play_pick_r2,
  ppc_play_band_r2 = ppc_play_band_r2,
  metadata = list(
    n_sims      = N_SIMS,
    n_lottery   = N_LOT,
    draft_years = sprintf("2026 actual + %d-%d projected",
                          FIRST_PROJECTED_DRAFT, LAST_PROJECTED_DRAFT),
    system_note = "2026 actual results; 3-2-1 effective 2027-2029",
    model_note  = "Bayesian 30-rank smoothed-softmax Markov chain + round-1 ex-Gaussian with a strictly decreasing pooled-gap mean curve + round-2 hurdle with a strictly decreasing pooled-gap EV curve and fringe/contributor played outcomes",
    value_metric      = "xrapm_war",
    value_metric_label = "xRAPM wins above replacement",
    value_unit_short  = "xRAPM WAR",
    value_window      = sprintf("four rookie-contract seasons after the draft (draft+1 to draft+%d)",
                                VALUE_WINDOW_SEASONS),
    xrapm_replacement = XRAPM_REPLACEMENT,
    points_per_win    = POINTS_PER_WIN,
    fit_draft_years   = range(draft_years),
    xrapm_match       = xrapm_match_summary,
    tiers       = TIERS,
    markov_state_type = "rank_worst_1_to_30",
    n_picks     = nrow(owned_future) + nrow(actual_2026_order) + nrow(actual_2026_second_order),
    round_validation_passed = round_validation_passed,
    timestamp   = Sys.time()
  )
)

saveRDS(dashboard_data, "01_data/dashboard_data.rds")

cat("\n============================================================\n")
cat("  RESULTS EXPORTED -> dashboard_data.rds\n")
cat("  Launch dashboard:  shiny::runApp('app.R')\n")
cat("============================================================\n\n")

# ---- console summary ----
cat(sprintf("%-6s %-13s %5s %9s %10s %8s %7s\n",
            "Team", "Tier", "W-L", "Cur WAR", "3-2-1 WAR", "Delta", "Pct"))
cat(strrep("-", 64), "\n")
for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  cat(sprintf("%-6s %-13s %2d-%-2d %9.1f %10.1f %+8.1f %+6.1f%%\n",
              r$team, r$tier, r$wins, r$losses,
              r$current_mean, r$new_mean, r$delta_value, r$delta_pct))
}

