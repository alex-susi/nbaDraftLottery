# ============================================================================
# SECTION 10: LOTTERY SIMULATORS
# ============================================================================
# Current (pre-2027) system: 14 teams, weighted combos, top-4 drawn.
# Approved 3-2-1 system: 16 teams, 2/3/2/1 balls by tier, ALL 16 drawn, bottom
# three "relegated" cannot land worse than #12.

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

# Seed order for 3-2-1: positions 1..16 are the 16 non-playoff teams, worst to
# best. balls16 holds each seed's lottery-ball count per the approved table:
#   seeds 1-3   relegation    2 balls each  (6)
#   seeds 4-10  non-play-in   3 balls each  (21)
#   seeds 11-14 9/10 seeds    2 balls each  (8)   <- FOUR teams
#   seeds 15-16 7v8 losers    1 ball each   (2)
# Total = 37 balls across 16 seeds.
balls16 <- c(2,2,2, 3,3,3,3,3,3,3, 2,2,2,2, 1,1)

sim_321_lottery <- function() {
  picks <- integer(16)
  drawn <- integer(0)
  
  for (p in 1:16) {
    undrawn <- setdiff(1:16, drawn)
    releg_remaining <- setdiff(1:3, drawn)
    
    # Relegated seeds cannot fall past pick 12. Enforce that constraint while
    # drawing, rather than repairing with a post-hoc swap. If the number of
    # undrawn relegated teams equals the number of remaining floor-safe slots,
    # the next pick must come from the remaining relegated seeds so all can be
    # seated by #12. Everyone else shifts down naturally.
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
# SECTION 11: TIER -> SEEDS, AND THE NEW ANTI-TANK PICK RESTRICTIONS
# ============================================================================
# Within a simulated season, teams are assigned to tiers by the Markov chain.
# We then need a worst-to-best ordering to seed the lottery. We order teams
# first by tier (relegation worst) then randomly within tier (a within-tier
# record proxy), giving the 16 non-playoff seeds and the 14 playoff slots.

order_teams_for_draft <- function(team_tiers) {
  tier_rank <- match(team_tiers, TIERS)            # 1 = relegation = worst
  jitter    <- runif(length(team_tiers))           # within-tier record proxy
  ord <- order(tier_rank, jitter)                  # worst -> best
  all_teams_local <- names(team_tiers)
  all_teams_local[ord]
}

# Apply the approved restrictions to the full ORIGINAL-team slot permutation,
# given each team's recent top-pick history.
#   - cannot receive #1 in consecutive years
#   - cannot receive a top-5 pick three years running
# When a restriction binds, the illegal team is bumped down to the first legal
# slot and every intervening team shifts up one slot. This preserves a valid
# one-team-per-slot draft order.
restricted_slot_floor <- function(orig_team, yr, top_pick_history) {
  hist <- top_pick_history[[orig_team]]
  got_no1_last  <- !is.null(hist$no1)  && (yr - 1) %in% hist$no1
  got_top5_2ago <- !is.null(hist$top5) &&
    all(c(yr - 1, yr - 2) %in% hist$top5)
  
  floor_slot <- 1L
  if (got_no1_last) floor_slot <- max(floor_slot, 2L)
  if (got_top5_2ago) floor_slot <- max(floor_slot, 6L)
  floor_slot
}

restricted_slot_target <- function(slot, orig_team, yr, top_pick_history) {
  max(as.integer(slot), restricted_slot_floor(orig_team, yr, top_pick_history))
}

apply_pick_restrictions <- function(slots, yr, top_pick_history) {
  slots <- as.integer(slots) %>% setNames(names(slots))
  
  if (any(is.na(slots))) {
    warning("Pick restriction input contains NA slots; returning original slots.")
    return(slots)
  }
  
  # Stable constrained reseating. The earlier iterative bump-and-restart version
  # could oscillate when two restricted teams traded the same illegal slot back
  # and forth (e.g., team A cannot pick #1, shifting team B into #1, then team B
  # is also restricted and shifts A back into #1). This version is equivalent to
  # repeatedly shifting the next legal team up, while preserving the original
  # lottery order as much as possible.
  original_order <- names(slots)[order(slots)]
  n <- length(original_order)
  remaining <- original_order
  out <- setNames(rep(NA_integer_, n), names(slots))
  
  min_slot <- setNames(
    vapply(original_order,
           function(tm) restricted_slot_floor(tm, yr, top_pick_history),
           integer(1)),
    original_order
  )
  
  for (slot in seq_len(n)) {
    legal_idx <- which(min_slot[remaining] <= slot)
    
    if (length(legal_idx) == 0L) {
      # Should be practically impossible with these rules, but keep the
      # simulation moving while surfacing the issue if a future rule change or
      # bad history state creates an infeasible assignment.
      warning(sprintf(
        "No legal team available for restricted slot %d in %d; using original order fallback.",
        slot, yr
      ))
      chosen_idx <- 1L
    } else {
      chosen_idx <- legal_idx[1]
    }
    
    chosen <- remaining[chosen_idx]
    out[chosen] <- slot
    remaining <- remaining[-chosen_idx]
  }
  
  if (anyDuplicated(out) || !identical(sort(as.integer(out)), seq_len(n))) {
    warning("Pick restriction reseating produced a non-permutation draft order.")
  }
  
  out
}

# Second-round ordering under each system.
# Current: inverse record for all 30 slots (31 = worst team, 60 = best team).
# 3-2-1: the first 16 second-round slots invert the FINAL first-round lottery
# order; playoff teams then follow inverse record for slots 47-60.
build_second_round_slots_current <- function(ord) {
  out <- setNames(rep(NA_integer_, length(all_teams)), all_teams)
  for (k in seq_along(ord)) out[ord[k]] <- 30L + k
  out
}

build_second_round_slots_321 <- function(ord, first_round_slots) {
  out <- setNames(rep(NA_integer_, length(all_teams)), all_teams)
  lot16 <- names(first_round_slots)[first_round_slots <= 16]
  for (tm in lot16) out[tm] <- 47L - as.integer(first_round_slots[tm])
  nonlot16 <- ord[17:30]
  for (k in seq_along(nonlot16)) out[nonlot16[k]] <- 46L + k
  out
}

# ---- future-pick allocation helpers -----------------------------------------
# These functions resolve original-team pick ownership in each Monte Carlo draw.
# They leave the lottery slot attached to the ORIGINAL team, then separately
# assign that original pick to an owner. That is what allows protections, swap
# returns, retained own picks, and multi-team ranked pools to flow downstream.

rank_teams_by_slot <- function(slots, teams) {
  teams <- teams[teams %in% names(slots)]
  teams[order(as.numeric(slots[teams]), na.last = TRUE)]
}

apply_simple_future_obligations <- function(owner_by_orig, slots, yr) {
  rows <- traded_future %>% filter(year == yr, round == 1L)
  
  # Outright protected/unprotected transfers. If protection does not convey,
  # owner_by_orig stays as the original team, so the own/retained asset receives value.
  outrights <- rows %>% filter(pick_type == "outright")
  if (nrow(outrights) > 0) {
    for (j in seq_len(nrow(outrights))) {
      og <- outrights$original_team[j]
      ow <- outrights$owner[j]
      prot <- outrights$protection[j]
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
  
  owner_by_orig
}

apply_complex_future_obligations <- function(owner_by_orig, slots, yr) {
  # 2027 ----------------------------------------------------------------------
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
    
    # SAN: 1-16 SAC, 17-30 OKC.
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
  
  # 2028 ----------------------------------------------------------------------
  if (yr == 2028L) {
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
    
    # BKN/PHI/PHX/NYK/WAS/MIL/POR nested pool. RealGM's text is deeply nested;
    # this is an explicit approximation: PHI keeps 1-8; among eligible BKN/PHX/
    # NYK/PHI picks, BKN gets the best two when available, NYK the next, PHX the
    # remainder; WAS can then improve by taking the better of WAS and PHX's
    # currently allocated pick; MIL can then improve by taking the better of MIL
    # and WAS's post-swap pick. POR is included only as MIL/WAS swap context.
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
    
    # Approximate WAS/PHX and MIL/WAS swap layers using the PHX-assigned pick.
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
  
  # 2029 ----------------------------------------------------------------------
  if (yr == 2029L) {
    # DAL/HOU/PHX: two best to HOU, other to BKN.
    r <- rank_teams_by_slot(slots, c("DAL", "HOU", "PHX"))
    if (length(r) == 3) {
      owner_by_orig[r[1:2]] <- "HOU"
      owner_by_orig[r[3]] <- "BKN"
    }
    
    # BOS/MIL/POR: best and worst to POR; middle to WAS.
    r <- rank_teams_by_slot(slots, c("BOS", "MIL", "POR"))
    if (length(r) == 3) {
      owner_by_orig[r[c(1, 3)]] <- "POR"
      owner_by_orig[r[2]] <- "WAS"
    }
    
    # CLE/MIN/UTA with MIN top-5 protection.
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
    
    # MEM/ORL: ORL keeps 1-2, otherwise MEM can swap for ORL if ORL is better.
    if (!is.na(slots["ORL"]) && slots["ORL"] > 2 && !is.na(slots["MEM"])) {
      if (slots["ORL"] < slots["MEM"]) {
        owner_by_orig["ORL"] <- "MEM"
        owner_by_orig["MEM"] <- "ORL"
      }
    }
    
    # PHI/LAC: LAC keeps 1-3, otherwise PHI can swap for LAC if LAC is better.
    if (!is.na(slots["LAC"]) && slots["LAC"] > 3 && !is.na(slots["PHI"])) {
      if (slots["LAC"] < slots["PHI"]) {
        owner_by_orig["LAC"] <- "PHI"
        owner_by_orig["PHI"] <- "LAC"
      }
    }
  }
  
  # 2030 ----------------------------------------------------------------------
  if (yr == 2030L) {
    # WAS/PHX/MEM: better WAS/PHX to WAS; better of MEM and worse WAS/PHX to
    # MEM; remaining to PHX.
    wp <- rank_teams_by_slot(slots, c("WAS", "PHX"))
    if (length(wp) == 2) {
      owner_by_orig[wp[1]] <- "WAS"
      rem <- rank_teams_by_slot(slots, c("MEM", wp[2]))
      if (length(rem) == 2) {
        owner_by_orig[rem[1]] <- "MEM"
        owner_by_orig[rem[2]] <- "PHX"
      }
    }
    
    # SAS/DAL/MIN. MIN keeps #1. Otherwise, best of DAL/SAS/MIN to SAS, second
    # to MIN, remaining to DAL. This mirrors the rank language closely enough
    # for valuation without adding another nested state machine.
    if (!is.na(slots["MIN"]) && slots["MIN"] == 1) {
      sd <- rank_teams_by_slot(slots, c("SAS", "DAL"))
      if (length(sd) == 2) {
        owner_by_orig[sd[1]] <- "SAS"
        owner_by_orig[sd[2]] <- "DAL"
      }
      owner_by_orig["MIN"] <- "MIN"
    } else {
      r <- rank_teams_by_slot(slots, c("SAS", "DAL", "MIN"))
      if (length(r) == 3) {
        owner_by_orig[r[1]] <- "SAS"
        owner_by_orig[r[2]] <- "MIN"
        owner_by_orig[r[3]] <- "DAL"
      }
    }
    
    # POR/MIL: POR may take MIL if MIL is better, with MIL receiving POR.
    if (!is.na(slots["MIL"]) && !is.na(slots["POR"]) && slots["MIL"] < slots["POR"]) {
      owner_by_orig["MIL"] <- "POR"
      owner_by_orig["POR"] <- "MIL"
    }
  }
  
  owner_by_orig
}

resolve_pick_owners <- function(slots, yr) {
  owner_by_orig <- setNames(all_teams, all_teams)
  owner_by_orig <- apply_simple_future_obligations(owner_by_orig, slots, yr)
  owner_by_orig <- apply_complex_future_obligations(owner_by_orig, slots, yr)
  owner_by_orig
}

first_round_condition_met <- function(condition_id, first_owner_by_orig) {
  if (is.na(condition_id) || is.null(condition_id)) return(TRUE)
  switch(condition_id,
         LAL_2027_FRP_TO_MEM     = identical(unname(first_owner_by_orig["LAL"]), "MEM"),
         LAL_2027_FRP_NOT_TO_MEM = !identical(unname(first_owner_by_orig["LAL"]), "MEM"),
         DAL_2027_FRP_TO_CHA     = identical(unname(first_owner_by_orig["DAL"]), "CHA"),
         DAL_2027_FRP_NOT_TO_CHA = !identical(unname(first_owner_by_orig["DAL"]), "CHA"),
         TRUE)
}

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

apply_simple_second_obligations <- function(owner_by_orig, slots, yr, first_owner_by_orig) {
  rows <- traded_second %>% filter(year == yr)
  if (nrow(rows) == 0) return(owner_by_orig)
  
  for (j in seq_len(nrow(rows))) {
    og <- rows$original_team[j]
    ow <- rows$owner[j]
    prot <- rows$protection[j]
    cond <- rows$condition_id[j]
    if (is.na(slots[og])) next
    if (!first_round_condition_met(cond, first_owner_by_orig)) next
    if (pick_conveys(slots[og], prot)) owner_by_orig[og] <- ow
  }
  
  owner_by_orig
}

apply_complex_second_obligations <- function(owner_by_orig, slots, yr) {
  if (yr == 2027L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, c("DAL", "BKN"), c("WAS", "DET"))
    
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
    
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, c("ORL", "BOS"), c("UTA", "CHA"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, c("PHX", "GSW"), c("PHI", "WAS"))
  }
  
  if (yr == 2028L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, c("CHA", "LAC"), c("CHA", "DET"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, c("LAL", "WAS"), c("ORL", "WAS"))
  }
  
  if (yr == 2030L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, c("LAC", "UTA"), c("CHA", "UTA"))
  }
  
  if (yr == 2031L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, c("MIN", "GSW"), c("CHI", "DET"))
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, c("BOS", "CLE"), c("UTA", "BOS"))
  }
  
  if (yr == 2032L) {
    owner_by_orig <- assign_ranked_second_pool(owner_by_orig, slots, c("HOU", "PHX"), c("CHI", "PHX"))
  }
  
  owner_by_orig
}

resolve_second_pick_owners <- function(slots, yr, first_owner_by_orig) {
  owner_by_orig <- setNames(all_teams, all_teams)
  owner_by_orig <- apply_simple_second_obligations(owner_by_orig, slots, yr, first_owner_by_orig)
  owner_by_orig <- apply_complex_second_obligations(owner_by_orig, slots, yr)
  owner_by_orig
}

value_allocated_future_assets <- function(sim, yr, draft_round, slots, owner_by_orig, d_pick, d_pick2,
                                          team_value, team_n, team_best,
                                          system = c("cur", "new")) {
  system <- match.arg(system)
  yr_assets <- pick_assets %>%
    filter(.data$year == .env$yr, .data$round == .env$draft_round)
  raw_by_orig <- setNames(rep(0, length(all_teams)), all_teams)
  for (tm in all_teams) {
    raw_by_orig[tm] <- if (!is.na(slots[tm])) sample_pick_value(slots[tm], draw_idx = d_pick, draw_idx_r2 = d_pick2) else 0
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
# For each simulation:
#   * 2026 is FIXED to the actual draft order (both systems identical) so its
#     values are not random — only 2027-2032 are projected.
#   * Draw one Markov transition matrix and one pick-value posterior index.
#   * Initialize each team's tier from 2025-26, then evolve year by year.
#   * Each year, seed BOTH lotteries (current vs 3-2-1), resolve swaps, apply
#     protections and the new pick restrictions, and value every owned pick.
#
# Seeding 2026 baseline tiers from the final 2025-26 standings:
current_tiers0 <- setNames(as.character(current_standings$tier),
                           current_standings$abbr)

# Pre-compute the actual 2026 slot value contribution per owner (sampled each
# sim so 2026 still carries pick-value uncertainty, just not lottery
# uncertainty). Also returns per-asset values keyed by asset_id.
value_2026 <- function(d_pick, d_pick2) {
  v     <- setNames(rep(0, 30), all_teams)
  nbest <- setNames(rep(-Inf, 30), all_teams)
  ct    <- setNames(rep(0L, 30), all_teams)
  asset_val  <- setNames(rep(NA_real_, n_assets), asset_ids)
  asset_slot <- setNames(rep(NA_real_, n_assets), asset_ids)
  a26_assets <- pick_assets %>% filter(year == 2026L)
  for (r in seq_len(nrow(a26_assets))) {
    own <- a26_assets$owner[r]
    sl  <- a26_assets$fixed_slot[r]
    val <- sample_pick_value(sl, draw_idx = d_pick, draw_idx_r2 = d_pick2)
    v[own]     <- v[own] + val
    nbest[own] <- max(nbest[own], val)
    ct[own]    <- ct[own] + 1L
    asset_val[a26_assets$asset_id[r]]  <- val
    asset_slot[a26_assets$asset_id[r]] <- sl
  }
  list(v = v, best = nbest, n = ct, asset_val = asset_val, asset_slot = asset_slot)
}

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
sim_curve_par_cols <- c(
  "alpha", "beta", "gamma", "tau_log_sigma_rw", "nu",
  "logit_play_31_r2", "tau_logit_play_rw_r2",
  paste0("mu_", 1:60),
  paste0("sigma_", 1:60),
  paste0("p_play_", 31:60),
  paste0("r2_cond_played_ws_mean_", 31:60)
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
    pick_draws$tau_log_sigma_rw[d_pick],
    pick_draws$nu[d_pick],
    pick2_draws$logit_play_31[d_pick2],
    pick2_draws$tau_logit_play_rw[d_pick2],
    as.numeric(pick_mu_draws[d_pick, ]),
    as.numeric(pick2_mu_draws[d_pick2, ]),
    as.numeric(pick_sd_draws[d_pick, ]),
    as.numeric(pick2_sd_draws[d_pick2, ]),
    as.numeric(pick2_p_play_draws[d_pick2, ]),
    as.numeric(pick2_cond_mu_draws[d_pick2, ])
  )
  
  # transition matrix for this sim
  P <- t(vapply(1:N_TIERS, function(i) get_theta_row(d_mk, i), numeric(N_TIERS)))
  
  # accumulators
  tv_c <- setNames(rep(0, 30), all_teams); tv_n <- tv_c
  tn_c <- setNames(rep(0L, 30), all_teams); tn_n <- tn_c
  tb_c <- setNames(rep(-Inf, 30), all_teams); tb_n <- tb_c
  
  # 2026 actual (identical under both systems)
  a26 <- value_2026(d_pick, d_pick2)
  tv_c <- tv_c + a26$v;  tv_n <- tv_n + a26$v
  tn_c <- tn_c + a26$n;  tn_n <- tn_n + a26$n
  tb_c <- pmax(tb_c, a26$best); tb_n <- pmax(tb_n, a26$best)
  # store per-asset 2026 values (same under both systems). 2026 is locked, so
  # slot == fixed slot, raw value == realized value, and own-slot == slot.
  a26_ids <- pick_assets$asset_id[pick_assets$year == 2026L]
  asset_cur[sim, a26_ids] <- a26$asset_val[a26_ids]
  asset_new[sim, a26_ids] <- a26$asset_val[a26_ids]
  asset_slot_cur[sim, a26_ids]    <- a26$asset_slot[a26_ids]
  asset_slot_new[sim, a26_ids]    <- a26$asset_slot[a26_ids]
  asset_raw_cur[sim, a26_ids]     <- a26$asset_val[a26_ids]
  asset_raw_new[sim, a26_ids]     <- a26$asset_val[a26_ids]
  asset_ownslot_cur[sim, a26_ids] <- a26$asset_slot[a26_ids]
  asset_ownslot_new[sim, a26_ids] <- a26$asset_slot[a26_ids]
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
  
  team_tiers <- current_tiers0
  
  for (yr in FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT) {
    # evolve tiers one year via the Markov chain
    for (tm in all_teams) {
      i <- match(team_tiers[tm], TIERS)
      team_tiers[tm] <- TIERS[sample(N_TIERS, 1, prob = P[i, ])]
    }
    
    # worst -> best ordering for seeding
    ord <- order_teams_for_draft(team_tiers)
    rank_of <- setNames(seq_along(ord), ord)        # 1 = worst overall
    
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
    # Restrictions look back at the ORIGINAL team's recent top picks. They do
    # not change who owns the pick; ownership is resolved below. Reseating is
    # applied to the full permutation so no two teams can occupy the same slot.
    slot_n <- apply_pick_restrictions(slot_n, yr, top_hist)
    
    # ---- second-round slots -------------------------------------------------
    # Current system keeps inverse-record order. Under 3-2-1, slots 31-46 are
    # the inverse of the final 1-16 lottery order, then playoff teams fill 47-60
    # by inverse record.
    slot2_c <- build_second_round_slots_current(ord)
    slot2_n <- build_second_round_slots_321(ord, slot_n)
    
    # ---- record realized ORIGINAL-team seats this year ----------------------
    yc <- as.character(yr)
    for (tm in all_teams) {
      if (!is.na(slot_c[tm])) team_slot_cur[sim, tm, yc] <- slot_c[tm]
      if (!is.na(slot_n[tm])) team_slot_new[sim, tm, yc] <- slot_n[tm]
      if (!is.na(slot2_c[tm])) team_slot2_cur[sim, tm, yc] <- slot2_c[tm]
      if (!is.na(slot2_n[tm])) team_slot2_new[sim, tm, yc] <- slot2_n[tm]
    }
    
    # ---- resolve all simple + complex ownership obligations -----------------
    owner_by_orig_c <- resolve_pick_owners(slot_c, yr)
    owner_by_orig_n <- resolve_pick_owners(slot_n, yr)
    owner2_by_orig_c <- resolve_second_pick_owners(slot2_c, yr, owner_by_orig_c)
    owner2_by_orig_n <- resolve_second_pick_owners(slot2_n, yr, owner_by_orig_n)
    
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
validate_round_aware_outputs <- function() {
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
  for (yr in as.character(proj_years)) {
    fr <- team_slot_new[, , yr]
    sr <- team_slot2_new[, , yr]
    lot_idx <- which(!is.na(fr) & fr <= 16, arr.ind = TRUE)
    if (nrow(lot_idx) > 0) {
      inv_bad <- inv_bad + sum(sr[lot_idx] != 47L - fr[lot_idx], na.rm = TRUE)
    }
  }
  if (inv_bad > 0) stop("3-2-1 second-round inversion validation failed.", call. = FALSE)
  
  if (any(is.na(pick_assets$round)) || any(!pick_assets$round %in% c(1L, 2L))) {
    stop("pick_assets has missing or invalid round values.", call. = FALSE)
  }
  
  TRUE
}

round_validation_passed <- validate_round_aware_outputs()
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
# player-level Student-t outcome. These are the team / pick values shown when
# the app toggles to "Expected Asset Value" and match the left-side Trade
# Machine interpretation.
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
      # Some entitlements can produce two picks in rare protected/ranked-pool
      # scenarios, so keep the count rather than forcing a 0/1 indicator.
      out[, did] <- rowSums(convey_mat[, ids, drop = FALSE])
    }
  }
  out
}

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

keep_display_cols <- function(mat, ids) {
  ids <- ids[ids %in% colnames(mat)]
  mat[, ids, drop = FALSE]
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

summary_df <- all_res %>%
  group_by(team) %>%
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
  left_join(tier_map, by = c("team" = "abbr")) %>%
  arrange(desc(delta_value))

build_team_summary_from_asset_draws <- function(cur_mat, new_mat, base_summary) {
  purrr::map_dfr(all_teams, function(tm) {
    cols <- which(pick_assets$owner == tm)
    if (length(cols) == 0) {
      cur_total <- rep(0, nrow(cur_mat))
      new_total <- rep(0, nrow(new_mat))
      cur_best <- rep(NA_real_, nrow(cur_mat))
      new_best <- rep(NA_real_, nrow(new_mat))
    } else {
      cur_sub <- cur_mat[, cols, drop = FALSE]
      new_sub <- new_mat[, cols, drop = FALSE]
      cur_total <- rowSums(cur_sub)
      new_total <- rowSums(new_sub)
      cur_best <- apply(cur_sub, 1, max, na.rm = TRUE)
      new_best <- apply(new_sub, 1, max, na.rm = TRUE)
    }
    
    base_row <- base_summary %>% filter(team == tm)
    tibble(
      team = tm,
      current_mean   = mean(cur_total),
      current_median = median(cur_total),
      current_sd     = sd(cur_total),
      current_q05    = quantile(cur_total, 0.05),
      current_q25    = quantile(cur_total, 0.25),
      current_q75    = quantile(cur_total, 0.75),
      current_q95    = quantile(cur_total, 0.95),
      new_mean       = mean(new_total),
      new_median     = median(new_total),
      new_sd         = sd(new_total),
      new_q05        = quantile(new_total, 0.05),
      new_q25        = quantile(new_total, 0.25),
      new_q75        = quantile(new_total, 0.75),
      new_q95        = quantile(new_total, 0.95),
      n_picks_mean   = base_row$n_picks_mean[1],
      best_current   = mean(cur_best, na.rm = TRUE),
      best_new       = mean(new_best, na.rm = TRUE),
      delta_value    = mean(new_total) - mean(cur_total),
      delta_pct      = (mean(new_total) / pmax(mean(cur_total), 0.01) - 1) * 100,
      delta_quality  = mean(new_best, na.rm = TRUE) - mean(cur_best, na.rm = TRUE),
      sigma_change   = sd(new_total) - sd(cur_total)
    )
  }) %>%
    left_join(tier_map, by = c("team" = "abbr")) %>%
    arrange(desc(delta_value))
}

summary_ev <- build_team_summary_from_asset_draws(asset_cur_ev, asset_new_ev, summary_df)

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

summarise_lottery_seed <- function(x) {
  tibble(
    expected_pick = mean(x),
    expected_pick_se = sd(x) / sqrt(length(x)),
    prob_no1  = mean(x == 1),
    prob_top3 = mean(x <= 3),
    prob_top5 = mean(x <= 5),
    prob_top10 = mean(x <= 10),
    prob_no1_se  = sqrt(prob_no1 * (1 - prob_no1) / length(x)),
    prob_top3_se = sqrt(prob_top3 * (1 - prob_top3) / length(x)),
    prob_top5_se = sqrt(prob_top5 * (1 - prob_top5) / length(x)),
    prob_top10_se = sqrt(prob_top10 * (1 - prob_top10) / length(x))
  )
}

lottery_dist <- bind_rows(
  map_dfr(1:14, function(s) {
    summarise_lottery_seed(cur_sims[, s]) %>%
      mutate(seed = s, system = "Current", .before = 1)
  }),
  map_dfr(1:16, function(s) {
    summarise_lottery_seed(new_sims[, s]) %>%
      mutate(seed = s, system = "Proposed 3-2-1", .before = 1)
  })
) %>%
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
    tau_log_sigma_rw = round(mean(pick_draws$tau_log_sigma_rw), 4),
    nu               = round(mean(pick_draws$nu), 2),
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
    curve_type  = "Bayesian Student-t player-level Stan with adjacent-pick sigma smoothing"
  ),
  pick2_model = list(
    logit_play_31    = round(mean(pick2_draws$logit_play_31), 3),
    tau_logit_play_rw = round(mean(pick2_draws$tau_logit_play_rw), 4),
    p_play_31        = round(mean(pick2_p_play_draws[, 1]), 3),
    p_play_45        = round(mean(pick2_p_play_draws[, 15]), 3),
    p_play_60        = round(mean(pick2_p_play_draws[, 30]), 3),
    cond_ws_mean_31  = round(mean(pick2_cond_mu_draws[, 1]), 2),
    cond_ws_mean_45  = round(mean(pick2_cond_mu_draws[, 15]), 2),
    cond_ws_mean_60  = round(mean(pick2_cond_mu_draws[, 30]), 2),
    sigma_pick_31    = round(mean(pick2_sd_draws[, 1]), 2),
    sigma_pick_45    = round(mean(pick2_sd_draws[, 15]), 2),
    sigma_pick_60    = round(mean(pick2_sd_draws[, 30]), 2),
    n_players        = nrow(pick_fit_data_r2),
    played_rate      = round(mean(pick_fit_data_r2$played == 1), 3),
    max_rhat         = round(max(pick2_diag$rhat, na.rm = TRUE), 4),
    min_ess          = round(min(pick2_diag$ess_bulk, na.rm = TRUE)),
    ppc_cover        = round(mean(ppc_tbl_r2$covered), 3),
    ppc_level        = "round-2 right-skew hurdle posterior predictive player rows",
    ws_floor         = round(R2_WS_FLOOR, 2),
    upside_prob_31   = round(mean(pick2_upside_prob_draws[, 1]), 3),
    upside_prob_45   = round(mean(pick2_upside_prob_draws[, 15]), 3),
    upside_prob_60   = round(mean(pick2_upside_prob_draws[, 30]), 3),
    upside_logit_slope = round(mean(pick2_draws$delta_logit_upside), 4),
    upside_multiplier = round(mean(exp(pick2_draws$upside_log_shift)), 2),
    upside_sigma_mult = round(mean(pick2_draws$upside_sigma_mult), 2),
    loo_summary      = tibble(
      model = "round2_right_skew_hurdle_ws",
      elpd_loo = loo_pick_r2$estimates["elpd_loo", "Estimate"],
      p_loo = loo_pick_r2$estimates["p_loo", "Estimate"],
      looic = loo_pick_r2$estimates["looic", "Estimate"],
      max_pareto_k = max(loo::pareto_k_values(loo_pick_r2), na.rm = TRUE)
    ),
    curve_type = "Bayesian second-round right-skew hurdle: adjacent-pick P(play), shifted-lognormal played outcomes, and pick-declining rare-upside component"
  ),
  markov_model = list(
    n_transitions = sum(counts_mat),
    n_seasons     = n_distinct(all_standings$season),
    lambda2       = round(mc_diag$lambda2, 3),
    mixing_time   = round(mc_diag$mixing_time, 1),
    max_abs_diff  = round(max(abs(post_trans - posterior_mean_closed)), 4)
  )
)

dashboard_data <- list(
  summary           = summary_df,
  summary_ev        = summary_ev,
  lottery_dist      = lottery_dist,
  lottery_tier_validation = lottery_tier_validation,
  official_321_tier_odds = official_321_tier_odds,
  pick_curve        = pick_curve,
  transition_matrix = post_trans,
  transition_counts = counts_mat,
  stationary        = setNames(as.numeric(mc_diag$stationary), TIERS),
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
  roster_info       = roster_info,
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
    model_note  = "Bayesian 5-tier Markov chain + round-1 Student-t pick curve + round-2 declining-upside right-skew hurdle pick curve",
    tiers       = TIERS,
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
            "Team", "Tier", "W-L", "Cur WS", "3-2-1 WS", "Delta", "Pct"))
cat(strrep("-", 64), "\n")
for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  cat(sprintf("%-6s %-13s %2d-%-2d %9.1f %10.1f %+8.1f %+6.1f%%\n",
              r$team, r$tier, r$wins, r$losses,
              r$current_mean, r$new_mean, r$delta_value, r$delta_pct))
}


