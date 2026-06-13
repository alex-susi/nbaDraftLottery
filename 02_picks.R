## ═════════════════════════════════════════════════════════════════════════════
## 05 - 2026 ACTUAL DRAFT ORDER ------------------------------------------------
## ═════════════════════════════════════════════════════════════════════════════
# The 2026 lottery is FINAL (Wizards won). We hardcode the actual first-round
# slot of every team's pick from the official post-lottery order so 2026 pick
# values reflect reality instead of being re-simulated. Each row is the owner
# of that slot and the team whose pick it originally was ("via").
#
# Source: NBA.com / ESPN official 2026 first-round order (post-lottery).

actual_2026_order <- tribble(~slot, ~owner, ~original_team,
                             1,     "WAS",  "WAS",
                             2,     "UTA",  "UTA",
                             3,     "MEM",  "MEM",
                             4,     "CHI",  "CHI",
                             5,     "LAC",  "IND",   # Zubac trade
                             6,     "BKN",  "BKN",
                             7,     "SAC",  "SAC",
                             8,     "ATL",  "NOP",   # via New Orleans
                             9,     "DAL",  "DAL",
                             10,    "MIL",  "MIL",
                             11,    "GSW",  "GSW",
                             12,    "OKC",  "LAC",   # OKC's incoming pick via the Clippers chain
                             13,    "MIA",  "MIA",
                             14,    "CHA",  "CHA",
                             15,    "CHI",  "POR",   # via Portland
                             16,    "MEM",  "PHX",   # via Phoenix
                             17,    "OKC",  "PHI",   # via Philadelphia
                             18,    "CHA",  "ORL",   # via Orlando
                             19,    "TOR",  "TOR",
                             20,    "SAS",  "ATL",   # via Atlanta
                             21,    "DET",  "MIN",   # via Minnesota
                             22,    "PHI",  "HOU",   # via Houston
                             23,    "ATL",  "CLE",   # via Cleveland
                             24,    "NYK",  "NYK",
                             25,    "LAL",  "LAL",
                             26,    "DEN",  "DEN",
                             27,    "BOS",  "BOS",
                             28,    "MIN",  "DET",   # via Detroit
                             29,    "CLE",  "SAS",   # via San Antonio
                             30,    "DAL",  "OKC") %>%   # via Oklahoma City
  mutate(round = 1L)

# Actual 2026 second-round order from RealGM yearly summary. Slots are locked
# like the first-round actuals, but valued through the separate round-2 model.
actual_2026_second_order <- tribble(~slot, ~owner, ~original_team,
                                    31,     "NYK", "WAS", 
                                    32,     "MEM", "IND", 
                                    33,     "BKN", "BKN",
                                    34,     "SAC", "SAC", 
                                    35,     "SAS", "UTA", 
                                    36,     "LAC", "MEM",
                                    37,     "OKC", "DAL", 
                                    38,     "CHI", "NOP", 
                                    39,     "HOU", "CHI",
                                    40,     "BOS", "MIL", 
                                    41,     "MIA", "GSW", 
                                    42,     "SAS", "POR",
                                    43,     "BKN", "LAC", 
                                    44,     "SAS", "MIA", 
                                    45,     "SAC", "CHA",
                                    46,     "ORL", "ORL", 
                                    47,     "PHX", "PHI", 
                                    48,     "DAL", "PHX",
                                    49,     "DEN", "ATL", 
                                    50,     "TOR", "TOR", 
                                    51,     "WAS", "MIN",
                                    52,     "LAC", "CLE", 
                                    53,     "HOU", "HOU", 
                                    54,     "GSW", "LAL",
                                    55,     "NYK", "NYK", 
                                    56,     "CHI", "DEN", 
                                    57,     "ATL", "BOS",
                                    58,     "NOP", "DET", 
                                    59,     "MIN", "SAS", 
                                    60,     "WAS", "OKC") %>% 
  mutate(round = 2L)





## ═════════════════════════════════════════════════════════════════════════════
## 06 - FUTURE PICK OWNERSHIP 2027-2032 (RealGM / prosportstransactions) --------
## ═════════════════════════════════════════════════════════════════════════════
# Attempt a live scrape; on failure use the hardcoded table compiled from
# RealGM "future drafts", prosportstransactions, and team beat reporting as of
# the 2026 lottery. Protections are evaluated during simulation; swaps are
# tagged so the owner takes the more favorable slot.
#
# NEW-RULE NOTE: under the approved system, picks may NOT be protected in the
# 12-15 band. None of the encoded protections fall in that band; the helper
# below also hard-blocks any such protection if added later.



# protection evaluator: TRUE if the pick conveys to the new owner at `pos`.
# First-round protections keep the familiar top-N language. Second-round
# protections are usually written as protected ranges like 31-55, so those are
# handled explicitly as well.
pick_conveys <- function(pos, protection) {
  if (length(pos) == 0 || is.na(pos)) return(FALSE)
  if (is.null(protection) || length(protection) == 0 || is.na(protection)) return(TRUE)
  protection <- as.character(protection)
  if (protection == "none") return(TRUE)
  if (protection == "top1")   return(pos > 1)
  if (protection == "top2")   return(pos > 2)
  if (protection == "top3")   return(pos > 3)
  if (protection == "top4")   return(pos > 4)
  if (protection == "top5")   return(pos > 5)
  if (protection == "top6")   return(pos > 6)
  if (protection == "top8")   return(pos > 8)
  if (protection == "top10")  return(pos > 10)
  if (protection == "top16")  return(pos > 16)
  if (protection == "top20")  return(pos > 20)
  if (protection == "lottery") return(pos > 14)
  
  # Second-round protected ranges: e.g. protected31_55 means the pick conveys
  # only if it lands outside 31-55. Since second-round slots are 31-60, that is
  # equivalent to conveying at 56-60.
  m <- stringr::str_match(protection, "^protected(\\d+)_(\\d+)$")
  if (!is.na(m[1, 1])) {
    lo <- as.integer(m[1, 2])
    hi <- as.integer(m[1, 3])
    return(!(pos >= lo && pos <= hi))
  }
  
  # Explicit conveyance range, useful for clauses like "if 56-60".
  m <- stringr::str_match(protection, "^convey(\\d+)_(\\d+)$")
  if (!is.na(m[1, 1])) {
    lo <- as.integer(m[1, 2])
    hi <- as.integer(m[1, 3])
    return(pos >= lo && pos <= hi)
  }
  
  TRUE
}

# Validate the new "no 12-15 protection" rule for any first-round protection we encode.
protection_floor <- function(protection) {
  switch(protection,
         top1 = 1, top2 = 2, top3 = 3, top4 = 4, top5 = 5, top6 = 6,
         top8 = 8, top10 = 10, lottery = 14, top16 = 16, top20 = 20, 0)
}

traded_future <- tribble(
  ~owner, ~original_team, ~year, ~protection, ~pick_type, ~notes,
  
  # ---- 2027: flat / directly representable obligations ----
  "HOU", "BKN", 2027, "none",    "swap",     "HOU may swap with BKN",
  "BKN", "NYK", 2027, "none",    "outright", "NYK to BKN",
  "HOU", "PHX", 2027, "none",    "outright", "PHX to HOU via BKN",
  "MEM", "LAL", 2027, "top4",    "outright", "LAL 5-30 to MEM",
  "CHA", "DAL", 2027, "top2",    "outright", "DAL 3-30 to CHA",
  "CHA", "MIA", 2027, "lottery", "outright", "MIA 15-30 to CHA",
  
  # ---- 2028 ----
  "PHI", "LAC", 2028, "none",    "outright", "LAC to PHI",
  "POR", "ORL", 2028, "none",    "outright", "ORL to POR via MEM",
  "OKC", "DEN", 2028, "top5",    "outright", "DEN 6-30 to OKC if not already settled",
  "OKC", "DAL", 2028, "none",    "swap",     "OKC may swap with DAL",
  
  # ---- 2029 ----
  "BKN", "NYK", 2029, "none",    "outright", "NYK to BKN",
  "LAC", "IND", 2029, "none",    "outright", "IND to LAC",
  "DAL", "LAL", 2029, "none",    "outright", "LAL to DAL",
  "OKC", "DEN", 2029, "top5",    "outright", "DEN 6-30 to OKC if not already settled",
  
  # ---- 2030 ----
  "MEM", "ORL", 2030, "none",    "outright", "ORL to MEM",
  "DAL", "GSW", 2030, "top20",   "outright", "GSW 21-30 to DAL",
  "OKC", "DEN", 2030, "top5",    "outright", "DEN 6-30 to OKC if prior conditions satisfied",
  
  # ---- 2031 ----
  "BKN", "NYK", 2031, "none",    "outright", "NYK to BKN",
  "MEM", "PHX", 2031, "none",    "outright", "PHX to MEM via UTA",
  "SAC", "MIN", 2031, "none",    "outright", "MIN to SAC via SAN",
  "SAS", "SAC", 2031, "none",    "swap",     "SAS may swap with SAC",
  
  # ---- 2032 ----
  "BKN", "DEN", 2032, "none",    "outright", "DEN to BKN") %>%
  mutate(complex_group = NA_character_, round = 1L)

# Complex first-round obligations that require simulation-time ranked-pool
# resolution. Each row in complex_future_assets is a possible owner/original-team
# outcome. Own-pick rows are generated separately, so owner == original_team rows
# are intentionally omitted here.
complex_future_groups <- tribble(~year, ~group_id, ~notes,
                                 2027, "MIL_NOP_ATL",              
                                 "More favorable of MIL/NOP to NOP; other to ATL if 5-30; both to NOP if both 1-4",
                                 2027, "CLE_MIN_UTA_MEM_UTA_PHX",  
                                 "Most favorable CLE/MIN/UTA to MEM; second to UTA; least to PHX",
                                 2027, "SAS_SAC_OKC",              
                                 "SAN 1-16 to SAC; SAN 17-30 to OKC",
                                 2027, "OKC_DEN_LAC",              
                                 "Two most/more favorable of OKC, DEN 6-30, LAC to OKC; other to LAC",
                                 2028, "ATL_CLE_UTA",              
                                 "ATL/CLE/UTA ranked swap pool",
                                 2028, "BOS_SAS",                  
                                 "SAS may swap for BOS if BOS 2-30",
                                 2028, "BKN_PHI_PHX_NYK_WAS_MIL",  
                                 "Nested BKN/PHI/PHX/NYK/WAS/MIL ranked swap pool",
                                 2029, "DAL_HOU_PHX_BKN",          
                                 "Two most favorable of DAL/HOU/PHX to HOU; other to BKN",
                                 2029, "BOS_MIL_POR_WAS",          
                                 "Most and least favorable of BOS/MIL/POR to POR; second to WAS",
                                 2029, "CLE_MIN_UTA_CHA",          
                                 "Most/two most favorable of CLE, MIN 6-30, UTA to UTA; other to CHA",
                                 2029, "MEM_ORL",                  
                                 "MEM may swap for ORL 3-30; ORL keeps 1-2",
                                 2029, "LAC_PHI",                  
                                 "PHI may swap for LAC 4-30; LAC keeps 1-3",
                                 2030, "WAS_PHX_MEM",              
                                 "More favorable WAS/PHX to WAS; MEM gets more favorable of MEM and less favorable WAS/PHX; least to PHX",
                                 2030, "DAL_SAS_MIN",              
                                 "SAS/DAL/MIN ranked swap pool; MIN keeps #1",
                                 2030, "MIL_POR",                  
                                 "POR may swap with MIL")

make_complex_assets <- function(year, group_id, original_teams, possible_owners, notes) {
  tidyr::expand_grid(owner = possible_owners,
                     original_team = original_teams) %>%
    filter(owner != original_team) %>%
    transmute(owner,
              original_team,
              year = as.integer(year),
              protection = "complex",
              pick_type = "complex",
              notes = notes,
              complex_group = group_id,
              round = 1L)
}

complex_future_assets <- bind_rows(
  make_complex_assets(2027, "MIL_NOP_ATL", c("MIL", "NOP"), c("NOP", "ATL"),
                      "MIL/NOP ranked pool: best to NOP; other to ATL unless both top-4"),
  make_complex_assets(2027, "CLE_MIN_UTA_MEM_UTA_PHX", c("CLE", "MIN", "UTA"), c("MEM", "UTA", "PHX"),
                      "CLE/MIN/UTA ranked pool: best MEM, second UTA, least PHX"),
  make_complex_assets(2027, "SAS_SAC_OKC", c("SAS"), c("SAC", "OKC"),
                      "SAN 1-16 to SAC; 17-30 to OKC"),
  make_complex_assets(2027, "OKC_DEN_LAC", c("OKC", "DEN", "LAC"), c("OKC", "LAC"),
                      "OKC/DEN/LAC ranked pool with DEN top-5 protection"),
  
  make_complex_assets(2028, "ATL_CLE_UTA", c("ATL", "CLE", "UTA"), c("ATL", "CLE", "UTA"),
                      "ATL/CLE/UTA ranked swap pool"),
  make_complex_assets(2028, "BOS_SAS", c("BOS", "SAS"), c("BOS", "SAS"),
                      "SAS may swap for BOS if BOS 2-30"),
  make_complex_assets(2028, "BKN_PHI_PHX_NYK_WAS_MIL", c("BKN", "PHI", "PHX", "NYK", "WAS", "MIL", "POR"),
                      c("BKN", "NYK", "WAS", "PHX", "MIL"),
                      "Nested BKN/PHI/PHX/NYK/WAS/MIL/POR ranked-pool approximation"),
  
  make_complex_assets(2029, "DAL_HOU_PHX_BKN", c("DAL", "HOU", "PHX"), c("HOU", "BKN"),
                      "DAL/HOU/PHX pool: two best HOU, other BKN"),
  make_complex_assets(2029, "BOS_MIL_POR_WAS", c("BOS", "MIL", "POR"), c("POR", "WAS"),
                      "BOS/MIL/POR pool: best and worst POR, middle WAS"),
  make_complex_assets(2029, "CLE_MIN_UTA_CHA", c("CLE", "MIN", "UTA"), c("UTA", "CHA"),
                      "CLE/MIN/UTA pool with MIN top-5 protection"),
  make_complex_assets(2029, "MEM_ORL", c("MEM", "ORL"), c("MEM", "ORL"),
                      "MEM may swap for ORL 3-30"),
  make_complex_assets(2029, "LAC_PHI", c("LAC", "PHI"), c("LAC", "PHI"),
                      "PHI may swap for LAC 4-30"),
  
  make_complex_assets(2030, "WAS_PHX_MEM", c("WAS", "PHX", "MEM"), c("WAS", "MEM", "PHX"),
                      "WAS/PHX/MEM ranked pool"),
  make_complex_assets(2030, "DAL_SAS_MIN", c("DAL", "SAS", "MIN"), c("DAL", "SAS", "MIN"),
                      "DAL/SAS/MIN ranked pool with MIN #1 protection"),
  make_complex_assets(2030, "MIL_POR", c("MIL", "POR"), c("MIL", "POR"),
                      "POR may swap with MIL")
)


# ---- SECOND-ROUND OBLIGATIONS ----------------------------------------------
# Round-2 rows use the same owner/original-team allocation design as round 1,
# but protections are range-based and several obligations depend on first-round
# conveyance. These rows are intentionally explicit/hardcoded from RealGM's
# future-pick detail/yearly pages and can be extended by adding rows here.
traded_second <- tribble(
  ~owner, ~original_team, ~year, ~protection, ~pick_type, ~notes, ~condition_id,
  # 2027 flat / directly representable
  "CHI", "CLE", 2027, "none", "outright", "CLE 2027 2nd to CHI", NA_character_,
  "DAL", "CHI", 2027, "none", "outright", "CHI 2027 2nd to DAL", NA_character_,
  "UTA", "DEN", 2027, "none", "outright", "DEN 2027 2nd to UTA", NA_character_,
  "UTA", "LAC", 2027, "none", "outright", "LAC 2027 2nd to UTA", NA_character_,
  "HOU", "MEM", 2027, "none", "outright", "MEM 2027 2nd to HOU", NA_character_,
  "POR", "MIN", 2027, "none", "outright", "MIN 2027 2nd to POR", NA_character_,
  "DET", "MIL", 2027, "none", "outright", "MIL 2027 2nd to DET", NA_character_,
  "NYK", "WAS", 2027, "none", "outright", "WAS 2027 2nd to NYK", NA_character_,
  # LAL 2027 second is conditional on LAL's first-round conveyance to MEM.
  "BKN", "LAL", 2027, "none", "conditional", 
  "LAL 2027 2nd to BKN if LAL conveys 2027 1st to MEM", "LAL_2027_FRP_TO_MEM",
  "MEM", "LAL", 2027, "none", "conditional", 
  "LAL 2027 2nd to MEM if LAL 2027 1st does not convey", "LAL_2027_FRP_NOT_TO_MEM",
  
  # 2028 flat / protected / conditional rows
  "LAC", "DAL", 2028, "none", "outright", "DAL 2028 2nd to LAC", NA_character_,
  "BKN", "MEM", 2028, "none", "outright", "MEM 2028 2nd to BKN", NA_character_,
  "DET", "NYK", 2028, "none", "outright", "NYK 2028 2nd to DET", NA_character_,
  "PHI", "DET", 2028, "protected31_55", "outright", "DET 2028 2nd to PHI protected 31-55", NA_character_,
  "DET", "MIA", 2028, "none", "conditional", 
  "MIA 2028 2nd to DET if DAL 2027 1st conveys to CHA", "DAL_2027_FRP_TO_CHA",
  "CHA", "MIA", 2028, "none", "conditional", 
  "MIA 2028 2nd to CHA if DAL 2027 1st does not convey to CHA", "DAL_2027_FRP_NOT_TO_CHA",
  
  # 2029-2032 flat / protected rows
  "SAS", "LAC", 2029, "none", "outright", "LAC 2029 2nd to SAS", NA_character_,
  "WAS", "LAL", 2029, "none", "outright", "LAL 2029 2nd to WAS", NA_character_,
  "BKN", "MEM", 2029, "none", "outright", "MEM 2029 2nd to BKN", NA_character_,
  "MEM", "POR", 2029, "none", "outright", "POR 2029 2nd to MEM", NA_character_,
  "BOS", "CHA", 2030, "protected31_55", "outright", "CHA 2030 2nd to BOS protected 31-55", NA_character_,
  "BKN", "DAL", 2030, "none", "outright", "DAL 2030 2nd to BKN", NA_character_,
  "BKN", "LAL", 2030, "none", "outright", "LAL 2030 2nd to BKN", NA_character_,
  "CHA", "MIL", 2031, "none", "outright", "MIL 2031 2nd to CHA", NA_character_,
  "CHA", "PHX", 2031, "none", "outright", "PHX 2031 2nd to CHA", NA_character_,
  "BKN", "LAL", 2031, "none", "outright", "LAL 2031 2nd to BKN", NA_character_,
  "BKN", "DEN", 2032, "none", "outright", "DEN 2032 2nd to BKN", NA_character_,
  "BKN", "MIA", 2032, "none", "outright", "MIA 2032 2nd to BKN", NA_character_,
  "CHA", "MIL", 2032, "none", "outright", "MIL 2032 2nd to CHA", NA_character_,
  "UTA", "CLE", 2032, "none", "outright", "CLE 2032 2nd to UTA", NA_character_,
  "ATL", "LAL", 2032, "none", "outright", "LAL 2032 2nd to ATL", NA_character_) %>%
  mutate(round = 2L, complex_group = NA_character_)

make_complex_second_assets <- function(year, group_id, original_teams, possible_owners, notes) {
  tidyr::expand_grid(owner = possible_owners,
                     original_team = original_teams) %>%
    filter(owner != original_team) %>%
    transmute(owner,
              original_team,
              year = as.integer(year),
              protection = "complex",
              pick_type = "complex",
              notes = notes,
              condition_id = NA_character_,
              complex_group = group_id,
              round = 2L)
}

complex_second_groups <- tribble(~year, ~group_id, ~notes,
                                 2027, "DAL_BKN_WAS_DET_2R", 
                                 "More favorable DAL/BKN 2nd to WAS; other to DET",
                                 2027, "HOU_OKC_IND_MIA_SAN_2R", "HOU/OKC/IND/MIA/SAN ranked second-round pool",
                                 2027, "NOP_POR_CHA_POR_HOU_2R", 
                                 "More favorable NOP/POR to CHA; other to POR unless 56-60 to HOU",
                                 2027, "ORL_BOS_UTA_CHA_2R", "More favorable ORL/BOS to UTA; other to CHA",
                                 2027, "PHX_GSW_PHI_WAS_2R", "More favorable PHX/GSW to PHI; other to WAS",
                                 2028, "CHA_LAC_DET_2R", "More favorable CHA/LAC to CHA; less favorable to DET",
                                 2028, "LAL_WAS_ORL_WAS_2R", "More favorable LAL/WAS to ORL; less favorable to WAS",
                                 2030, "LAC_UTA_CHA_UTA_2R", "More favorable LAC/UTA to CHA; less favorable to UTA",
                                 2031, "MIN_GSW_CHI_DET_2R", "More favorable MIN/GSW to CHI; less favorable to DET",
                                 2031, "BOS_CLE_UTA_BOS_2R", "More favorable BOS/CLE to UTA; less favorable to BOS",
                                 2032, "HOU_PHX_CHI_PHX_2R", "More favorable HOU/PHX to CHI; less favorable to PHX")

complex_second_assets <- bind_rows(
  make_complex_second_assets(2027, "DAL_BKN_WAS_DET_2R", c("DAL", "BKN"), c("WAS", "DET"),
                             "DAL/BKN second-round ranked pool"),
  make_complex_second_assets(2027, "HOU_OKC_IND_MIA_SAN_2R", c("HOU", "OKC", "IND", "MIA", "SAS"),
                             c("PHI", "NOP", "NYK", "SAS", "MIA"),
                             "HOU/OKC/IND/MIA/SAN second-round ranked pool"),
  make_complex_second_assets(2027, "NOP_POR_CHA_POR_HOU_2R", c("NOP", "POR"), c("CHA", "POR", "HOU"),
                             "NOP/POR second-round ranked pool with POR 56-60 layer"),
  make_complex_second_assets(2027, "ORL_BOS_UTA_CHA_2R", c("ORL", "BOS"), c("UTA", "CHA"),
                             "ORL/BOS second-round ranked pool"),
  make_complex_second_assets(2027, "PHX_GSW_PHI_WAS_2R", c("PHX", "GSW"), c("PHI", "WAS"),
                             "PHX/GSW second-round ranked pool"),
  make_complex_second_assets(2028, "CHA_LAC_DET_2R", c("CHA", "LAC"), c("CHA", "DET"),
                             "CHA/LAC second-round ranked pool"),
  make_complex_second_assets(2028, "LAL_WAS_ORL_WAS_2R", c("LAL", "WAS"), c("ORL", "WAS"),
                             "LAL/WAS second-round ranked pool"),
  make_complex_second_assets(2030, "LAC_UTA_CHA_UTA_2R", c("LAC", "UTA"), c("CHA", "UTA"),
                             "LAC/UTA second-round ranked pool"),
  make_complex_second_assets(2031, "MIN_GSW_CHI_DET_2R", c("MIN", "GSW"), c("CHI", "DET"),
                             "MIN/GSW second-round ranked pool"),
  make_complex_second_assets(2031, "BOS_CLE_UTA_BOS_2R", c("BOS", "CLE"), c("UTA", "BOS"),
                             "BOS/CLE second-round ranked pool"),
  make_complex_second_assets(2032, "HOU_PHX_CHI_PHX_2R", c("HOU", "PHX"), c("CHI", "PHX"),
                             "HOU/PHX second-round ranked pool")
)

# Add reciprocal contingent rows for simple two-team swaps, so the team losing
# the better pick can receive the swap-holder's original pick in simulations.
swap_return_assets <- traded_future %>%
  filter(pick_type == "swap") %>%
  transmute(owner = original_team,
            original_team = owner,
            year,
            protection = "none",
            pick_type = "swap_return",
            notes = sprintf("Return pick if %s exercises swap with %s", 
                            .data$original_team, .data$owner),
            complex_group = NA_character_,
            round = 1L)

# enforce the 12-15 protection ban
bad_prot <- traded_future %>%
  filter(map_dbl(protection, protection_floor) %in% 12:15)
if (nrow(bad_prot) > 0) {
  warning("Picks with illegal 12-15 protection found; coercing to top10.")
  traded_future <- traded_future %>%
    mutate(protection = ifelse(map_dbl(protection, protection_floor) %in% 12:15,
                               "top10", protection))
}

cat(sprintf("\nFlat future obligations encoded: %d rows (2027-2032)\n",
            nrow(traded_future)))
cat(sprintf("Complex future obligation assets encoded: %d rows across %d groups\n",
            nrow(complex_future_assets), n_distinct(complex_future_assets$complex_group)))


# Build the full set of possible pick assets for 2027-2032. We keep every
# team's own/retained pick row even when the pick is protected or in a swap pool;
# the simulation allocator decides which owner actually receives each original
# pick in each draw. This fixes the previous protected-pick retention bug.
build_owned_picks <- function() {
  own_future_r1 <- tidyr::expand_grid(year = FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT,
                                      original_team = all_teams) %>%
    transmute(owner = original_team,
              original_team,
              year = as.integer(year),
              round = 1L,
              protection = "none",
              pick_type = "own",
              notes = "own / retained first-round pick",
              condition_id = NA_character_,
              complex_group = NA_character_)
  
  own_future_r2 <- tidyr::expand_grid(year = FIRST_PROJECTED_DRAFT:LAST_PROJECTED_DRAFT,
                                      original_team = all_teams) %>%
    transmute(owner = original_team,
              original_team,
              year = as.integer(year),
              round = 2L,
              protection = "none",
              pick_type = "own",
              notes = "own / retained second-round pick",
              condition_id = NA_character_,
              complex_group = NA_character_)
  
  bind_rows(own_future_r1,
            own_future_r2,
            traded_future %>% mutate(condition_id = NA_character_),
            traded_second,
            swap_return_assets %>% mutate(condition_id = NA_character_),
            complex_future_assets %>% mutate(condition_id = NA_character_),
            complex_second_assets) %>%
    distinct(owner, original_team, year, round, pick_type, complex_group, condition_id, 
             .keep_all = TRUE)
}

owned_future <- build_owned_picks()


# PICK-ASSET REGISTRY
# A single master table of every individual pick asset the dashboard can value:
#   * the 30 actual 2026 first-round slots (locked), plus
#   * every owned 2027-2032 pick (traded + own).
# Each row gets a stable asset_id so the Monte Carlo loop can record a value
# distribution per pick under both lottery systems. We also build a readable
# "via" trade-chain string and a human label for the UI.

# 2026 actual assets (slot is fixed; protection/swap not applicable post-result)
assets_2026 <- bind_rows(actual_2026_order, actual_2026_second_order) %>%
  transmute(owner,
            original_team,
            year       = 2026L,
            round      = as.integer(round),
            protection = "none",
            pick_type  = "outright",
            notes      = ifelse(owner == original_team,
                                sprintf("own round-%d pick (2026 actual)", round),
                                sprintf("2026 actual round-%d, via %s", round, original_team)),
            fixed_slot = slot,
            condition_id = NA_character_,
            complex_group = NA_character_)

# 2027-2032 assets (projected)
assets_future <- owned_future %>%
  transmute(owner, original_team, year, round, protection, pick_type, notes,
            fixed_slot = NA_integer_, condition_id = condition_id %||% NA_character_,
            complex_group = complex_group %||% NA_character_)

pick_assets <- bind_rows(assets_2026, assets_future) %>%
  arrange(year, owner, original_team) %>%
  mutate(complex_group = coalesce(complex_group, NA_character_),
         asset_id = ifelse(is.na(complex_group),
                           sprintf("%d_R%d_%s_from_%s_%s", 
                                   year, round, owner, original_team, pick_type),
                           sprintf("%d_R%d_%s_from_%s_%s_%s", 
                                   year, round, owner, original_team, pick_type, complex_group)),
         is_traded = owner != original_team,
         label = case_when(pick_type == "own" ~ sprintf("%d R%d %s own / retained pick", year, round, owner),
                           pick_type == "complex"     ~ sprintf("%d R%d %s pick (contingent to %s, %s)", year, round,
                                                                original_team, owner, complex_group),
                           pick_type == "swap"        ~ sprintf("%d R%d %s pick (swap right held by %s)", year, round,
                                                                original_team, owner),
                           pick_type == "swap_return" ~ sprintf("%d R%d %s pick (return leg to %s)", year, round,
                                                                original_team, owner),
                           is_traded                  ~ sprintf("%d R%d %s pick (via %s)", year, round, 
                                                                original_team, owner),
                           TRUE                       ~ sprintf("%d R%d %s own pick", year, round, owner)),
         # human-readable obligation note
         obligation = case_when(pick_type == "own" ~ sprintf("Own round-%d pick, or retained if outgoing protection/swap does not convey", 
                                                             round),
                                pick_type == "complex"     ~ sprintf("Complex ranked-pool obligation: %s", notes),
                                pick_type == "swap"        ~ sprintf("Swap right held by %s", owner),
                                pick_type == "swap_return" ~ "Return leg if another team exercises a swap",
                                protection != "none"       ~ sprintf("%s-protected", protection),
                                is_traded                  ~ "Unprotected (conveys)",
                                TRUE                       ~ "Own pick, no obligations"))

# de-dupe any exact asset_id collisions (rare: same owner/orig/year/type)
pick_assets <- pick_assets %>%
  group_by(asset_id) %>%
  mutate(dup = row_number()) %>%
  ungroup() %>%
  mutate(asset_id = ifelse(dup > 1, sprintf("%s_%d", asset_id, dup), asset_id)) %>%
  select(-dup)

n_assets   <- nrow(pick_assets)
asset_ids  <- pick_assets$asset_id

cat(sprintf("Pick-asset registry: %d individual assets\n", n_assets))


# USER-FACING PICK ENTITLEMENTS (DISPLAY ASSETS)
# The simulation keeps one internal row per possible owner/original-team outcome
# so allocations can be resolved cleanly. That is too granular for the app:
# swap-return legs and retained own-pick rows are mutually exclusive pieces of
# the SAME user-facing entitlement. For example, BKN's 2027 first should appear
# once as "BKN own or HOU (via HOU swap for BKN)", not as separate retained and
# return-leg rows. The display registry below groups internal assets into the
# RealGM-style pick entitlements users expect to select.

pick_display_assets <- tibble()
pick_display_members <- tibble()

add_display_group <- function(display_asset_id,
                              year,
                              owner,
                              original_teams,
                              label,
                              obligation,
                              notes = obligation,
                              group_type = "grouped",
                              draft_round = 1L,
                              complex_group_filter = NULL,
                              pick_types = c("own", "swap", "swap_return", "complex")) {
  members <- pick_assets %>%
    filter(.data$year == .env$year,
           .data$round == .env$draft_round,
           .data$owner == .env$owner,
           .data$original_team %in% .env$original_teams,
           .data$pick_type %in% .env$pick_types)
  
  if (!is.null(complex_group_filter)) {
    members <- members %>%
      filter(.data$complex_group == .env$complex_group_filter |
               (.data$pick_type == "own" & .data$original_team %in% .env$original_teams))
  }
  
  members <- members %>% distinct(asset_id, .keep_all = TRUE)
  if (nrow(members) == 0) return(invisible(NULL))
  
  pick_display_assets <<- bind_rows(pick_display_assets,
                                    tibble(display_asset_id = display_asset_id,
                                           owner = owner,
                                           year = as.integer(year),
                                           round = as.integer(draft_round),
                                           label = label,
                                           obligation = obligation,
                                           notes = notes,
                                           group_type = group_type,
                                           display_group = complex_group_filter %||% display_asset_id,
                                           member_n = nrow(members),
                                           member_original_teams = paste(sort(unique(members$original_team)),
                                                                         collapse = ", ")))
  
  pick_display_members <<- bind_rows(pick_display_members,
                                     tibble(display_asset_id = display_asset_id,
                                            asset_id = members$asset_id))
  
  invisible(NULL)
}

# Simple two-team swaps: collapse the holder leg, own row, and return leg into
# one selectable entitlement per team.
simple_swaps_for_display <- traded_future %>% filter(pick_type == "swap")
if (nrow(simple_swaps_for_display) > 0) {
  for (i in seq_len(nrow(simple_swaps_for_display))) {
    sw <- simple_swaps_for_display[i, ]
    yr <- sw$year
    holder <- sw$owner
    counter <- sw$original_team
    pool <- c(holder, counter)
    suffix <- sprintf("via %s swap for %s", holder, counter)
    
    add_display_group(display_asset_id = sprintf("display_%d_%s_swap_%s_%s", 
                                                 yr, holder, holder, counter),
                      year = yr,
                      owner = holder,
                      original_teams = pool,
                      label = sprintf("%d %s own or %s (%s)", yr, holder, counter, suffix),
                      obligation = sprintf("%s receives the more favorable of %s and %s; %s receives the other.",
                                           holder, holder, counter, counter),
                      notes = sw$notes,
                      group_type = "simple_swap",
                      pick_types = c("own", "swap"))
    
    add_display_group(display_asset_id = sprintf("display_%d_%s_swap_return_%s_%s", 
                                                 yr, counter, holder, counter),
                      year = yr,
                      owner = counter,
                      original_teams = pool,
                      label = sprintf("%d %s own or %s (%s)", yr, counter, holder, suffix),
                      obligation = sprintf("%s receives the less favorable of %s and %s after %s's swap right.", 
                                           counter, holder, counter, holder),
                      notes = sw$notes,
                      group_type = "simple_swap_return",
                      pick_types = c("own", "swap_return"))
  }
}

# Complex ranked-pool obligations: one RealGM-style selectable entitlement per
# owner, even when the owner could receive different original teams' picks in
# different simulations. The internal member rows still preserve exact simulated
# allocation; the app sees the summed entitlement.
complex_display_specs <- tribble(
  ~year, ~complex_group, ~owner, ~original_teams, ~label, ~obligation,
  
  2027, "MIL_NOP_ATL", "NOP", "MIL,NOP",
  "2027 MIL or NOP (more favorable to NOP; other if both 1-4)",
  "More favorable of MIL and NOP to NOP; if both are 1-4, NOP also retains/receives the other.",
  2027, "MIL_NOP_ATL", "ATL", "MIL,NOP",
  "2027 MIL or NOP (less favorable to ATL if 5-30)",
  "Less favorable of MIL and NOP to ATL if that pick is 5-30.",
  
  2027, "CLE_MIN_UTA_MEM_UTA_PHX", "MEM", "CLE,MIN,UTA",
  "2027 CLE, MIN or UTA (most favorable to MEM)",
  "Most favorable of CLE, MIN and UTA to MEM.",
  2027, "CLE_MIN_UTA_MEM_UTA_PHX", "UTA", "CLE,MIN,UTA",
  "2027 CLE, MIN or UTA (second most favorable to UTA)",
  "Second most favorable of CLE, MIN and UTA to UTA.",
  2027, "CLE_MIN_UTA_MEM_UTA_PHX", "PHX", "CLE,MIN,UTA",
  "2027 CLE, MIN or UTA (least favorable to PHX)",
  "Least favorable of CLE, MIN and UTA to PHX.",
  
  2027, "SAS_SAC_OKC", "SAC", "SAS",
  "2027 SAS 1-16 to SAC",
  "SAS first-round pick to SAC if 1-16.",
  2027, "SAS_SAC_OKC", "OKC", "SAS",
  "2027 SAS 17-30 to OKC",
  "SAS first-round pick to OKC if 17-30.",
  
  2027, "OKC_DEN_LAC", "OKC", "OKC,DEN,LAC",
  "2027 OKC, DEN 6-30 or LAC (two most favorable to OKC)",
  "OKC receives the two most favorable / more favorable picks among OKC, DEN 6-30 and LAC.",
  2027, "OKC_DEN_LAC", "LAC", "OKC,DEN,LAC",
  "2027 OKC, DEN 6-30 or LAC (least / other to LAC)",
  "LAC receives the least favorable / other pick among OKC, DEN 6-30 and LAC.",
  
  2028, "ATL_CLE_UTA", "UTA", "ATL,CLE,UTA",
  "2028 UTA own or CLE (via UTA swap for CLE)",
  "More favorable of CLE and UTA to UTA.",
  2028, "ATL_CLE_UTA", "ATL", "ATL,CLE,UTA",
  "2028 ATL, CLE or UTA (more favorable to ATL)",
  "More favorable of ATL and the less favorable of CLE/UTA to ATL.",
  2028, "ATL_CLE_UTA", "CLE", "ATL,CLE,UTA",
  "2028 ATL, CLE or UTA (least favorable to CLE)",
  "Least favorable of ATL, CLE and UTA to CLE.",
  
  2028, "BOS_SAS", "SAS", "BOS,SAS",
  "2028 SAS own or BOS 2-30 (via SAS swap for BOS)",
  "SAS may swap for BOS if BOS is 2-30.",
  2028, "BOS_SAS", "BOS", "BOS,SAS",
  "2028 BOS own or SAS (via SAS swap for BOS, BOS protected #1)",
  "BOS receives the other pick if SAS exercises the BOS 2-30 swap right; BOS keeps #1.",
  
  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "BKN", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 BRK, PHL 9-30, PHX or NYK (most / two most favorable to BKN)",
  "Nested BKN/PHI/PHX/NYK/WAS/MIL/POR ranked-pool approximation; BKN receives the most / two most favorable eligible pick(s).",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "NYK", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 NYK, BRK, PHX or PHL 9-30 (NYK allocation)",
  "Nested BKN/PHI/PHX/NYK pool allocation to NYK.",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "WAS", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 WAS or least/less favorable BRK, PHL 9-30 and PHX (to WAS)",
  "WAS swap layer on the nested BKN/PHI/PHX pool.",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "PHX", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 PHX or least favorable BRK/PHL 9-30/WAS (to PHX)",
  "PHX receives the least favorable remaining pick in the nested pool.",
  2028, "BKN_PHI_PHX_NYK_WAS_MIL", "MIL", "BKN,PHI,PHX,NYK,WAS,MIL,POR",
  "2028 MIL or WAS/PHX pool pick (via WAS/MIL swap layer)",
  "MIL swap layer on the WAS/PHX/BKN/PHI/NYK/POR pool.",
  
  2029, "DAL_HOU_PHX_BKN", "HOU", "DAL,HOU,PHX",
  "2029 DAL, HOU or PHX (two most favorable to HOU)",
  "Two most favorable of DAL, HOU and PHX to HOU.",
  2029, "DAL_HOU_PHX_BKN", "BKN", "DAL,HOU,PHX",
  "2029 DAL, HOU or PHX (least favorable to BKN)",
  "Least favorable of DAL, HOU and PHX to BKN.",
  
  2029, "BOS_MIL_POR_WAS", "POR", "BOS,MIL,POR",
  "2029 BOS, MIL or POR (most and least favorable to POR)",
  "Most and least favorable of BOS, MIL and POR to POR.",
  2029, "BOS_MIL_POR_WAS", "WAS", "BOS,MIL,POR",
  "2029 BOS, MIL or POR (second most favorable to WAS)",
  "Second most favorable of BOS, MIL and POR to WAS.",
  
  2029, "CLE_MIN_UTA_CHA", "UTA", "CLE,MIN,UTA",
  "2029 CLE, MIN 6-30 or UTA (most / two most favorable to UTA)",
  "Most / two most favorable of CLE, MIN 6-30 and UTA to UTA.",
  2029, "CLE_MIN_UTA_CHA", "CHA", "CLE,MIN,UTA",
  "2029 CLE, MIN 6-30 or UTA (other to CHA)",
  "Other / least favorable eligible pick from CLE, MIN 6-30 and UTA to CHA.",
  
  2029, "MEM_ORL", "MEM", "MEM,ORL",
  "2029 MEM own or ORL 3-30 (via MEM swap for ORL)",
  "MEM may swap for ORL if ORL is 3-30.",
  2029, "MEM_ORL", "ORL", "MEM,ORL",
  "2029 ORL own or MEM (via MEM swap for ORL, ORL protected 1-2)",
  "ORL keeps 1-2; otherwise receives the other pick if MEM exercises the swap.",
  
  2029, "LAC_PHI", "PHI", "LAC,PHI",
  "2029 PHI own or LAC 4-30 (via PHI swap for LAC)",
  "PHI may swap for LAC if LAC is 4-30.",
  2029, "LAC_PHI", "LAC", "LAC,PHI",
  "2029 LAC own or PHI (via PHI swap for LAC, LAC protected 1-3)",
  "LAC keeps 1-3; otherwise receives the other pick if PHI exercises the swap.",
  
  2030, "WAS_PHX_MEM", "WAS", "WAS,PHX,MEM",
  "2030 WAS or PHX (more favorable to WAS)",
  "More favorable of WAS and PHX to WAS.",
  2030, "WAS_PHX_MEM", "MEM", "WAS,PHX,MEM",
  "2030 MEM or less favorable WAS/PHX (more favorable to MEM)",
  "More favorable of MEM and the less favorable of WAS/PHX to MEM.",
  2030, "WAS_PHX_MEM", "PHX", "WAS,PHX,MEM",
  "2030 WAS, PHX or MEM (least favorable to PHX)",
  "Least favorable remaining pick among WAS, PHX and MEM to PHX.",
  
  2030, "DAL_SAS_MIN", "SAS", "DAL,SAS,MIN",
  "2030 DAL, SAS or MIN 2-30 (most favorable to SAS)",
  "Best eligible pick among DAL, SAS and MIN 2-30 to SAS.",
  2030, "DAL_SAS_MIN", "MIN", "DAL,SAS,MIN",
  "2030 DAL, SAS or MIN (second most favorable to MIN; MIN keeps #1)",
  "Second eligible pick to MIN; MIN keeps #1.",
  2030, "DAL_SAS_MIN", "DAL", "DAL,SAS,MIN",
  "2030 DAL, SAS or MIN 2-30 (least / other to DAL)",
  "Remaining eligible pick among DAL, SAS and MIN 2-30 to DAL.",
  
  2030, "MIL_POR", "POR", "MIL,POR",
  "2030 POR own or MIL (via POR swap for MIL)",
  "POR may swap with MIL.",
  2030, "MIL_POR", "MIL", "MIL,POR",
  "2030 MIL own or POR (via POR swap for MIL)",
  "MIL receives the other pick if POR exercises the swap.")

if (nrow(complex_display_specs) > 0) {
  for (i in seq_len(nrow(complex_display_specs))) {
    spec <- complex_display_specs[i, ]
    orig <- str_split(spec$original_teams, "\\s*,\\s*")[[1]]
    add_display_group(display_asset_id = sprintf("display_%d_%s_%s", 
                                                 spec$year, spec$owner, spec$complex_group),
                      year = spec$year,
                      owner = spec$owner,
                      original_teams = orig,
                      label = spec$label,
                      obligation = spec$obligation,
                      notes = spec$obligation,
                      group_type = "complex_entitlement",
                      complex_group_filter = spec$complex_group,
                      pick_types = c("own", "complex"))               
  }
}

# Complex second-round ranked-pool obligations need the same user-facing
# treatment as first-round complex pools. Without these grouped entitlements,
# internal own/retained rows for picks that are fully assigned away can leak
# into the Single Pick / Trade Machine selectors as misleading zero-EV picks.
complex_second_display_specs <- tribble(
  ~year, ~complex_group, ~owner, ~original_teams, ~label, ~obligation,
  
  2027, "DAL_BKN_WAS_DET_2R", "WAS", "DAL,BKN",
  "2027 DAL or BKN 2nd (more favorable to WAS)",
  "More favorable of DAL and BKN second-round picks to WAS.",
  2027, "DAL_BKN_WAS_DET_2R", "DET", "DAL,BKN",
  "2027 DAL or BKN 2nd (less favorable to DET)",
  "Less favorable of DAL and BKN second-round picks to DET.",
  
  2027, "HOU_OKC_IND_MIA_SAN_2R", "PHI", "HOU,OKC,IND,MIA,SAS",
  "2027 HOU, OKC, IND, MIA or SAS 2nd (best eligible to PHI)",
  "Best of the HOU/OKC/IND/MIA second-round pool to PHI.",
  2027, "HOU_OKC_IND_MIA_SAN_2R", "NOP", "HOU,OKC,IND,MIA,SAS",
  "2027 HOU, OKC, IND, MIA or SAS 2nd (second to NOP)",
  "Second-best of the HOU/OKC/IND/MIA second-round pool to NOP.",
  2027, "HOU_OKC_IND_MIA_SAN_2R", "NYK", "HOU,OKC,IND,MIA,SAS",
  "2027 HOU, OKC, IND, MIA or SAS 2nd (third to NYK)",
  "Third-best of the HOU/OKC/IND/MIA second-round pool to NYK.",
  2027, "HOU_OKC_IND_MIA_SAN_2R", "SAS", "HOU,OKC,IND,MIA,SAS",
  "2027 SAS or remaining HOU/OKC/IND/MIA 2nd (more favorable to SAS)",
  "More favorable of SAS and the remaining HOU/OKC/IND/MIA second-round pick to SAS.",
  2027, "HOU_OKC_IND_MIA_SAN_2R", "MIA", "HOU,OKC,IND,MIA,SAS",
  "2027 MIA or SAS/remaining pool 2nd (less favorable to MIA)",
  "Less favorable of SAS and the remaining HOU/OKC/IND/MIA second-round pick to MIA.",
  
  2027, "NOP_POR_CHA_POR_HOU_2R", "CHA", "NOP,POR",
  "2027 NOP or POR 2nd (more favorable to CHA)",
  "More favorable of NOP and POR second-round picks to CHA.",
  2027, "NOP_POR_CHA_POR_HOU_2R", "POR", "NOP,POR",
  "2027 NOP or POR 2nd (less favorable to POR unless 56-60)",
  "Less favorable of NOP and POR second-round picks to POR unless the pick is 56-60.",
  2027, "NOP_POR_CHA_POR_HOU_2R", "HOU", "NOP,POR",
  "2027 NOP or POR 2nd (less favorable to HOU if 56-60)",
  "Less favorable of NOP and POR second-round picks to HOU if that pick lands 56-60.",
  
  2027, "ORL_BOS_UTA_CHA_2R", "UTA", "ORL,BOS",
  "2027 ORL or BOS 2nd (more favorable to UTA)",
  "More favorable of ORL and BOS second-round picks to UTA.",
  2027, "ORL_BOS_UTA_CHA_2R", "CHA", "ORL,BOS",
  "2027 ORL or BOS 2nd (less favorable to CHA)",
  "Less favorable of ORL and BOS second-round picks to CHA.",
  
  2027, "PHX_GSW_PHI_WAS_2R", "PHI", "PHX,GSW",
  "2027 PHX or GSW 2nd (more favorable to PHI)",
  "More favorable of PHX and GSW second-round picks to PHI.",
  2027, "PHX_GSW_PHI_WAS_2R", "WAS", "PHX,GSW",
  "2027 PHX or GSW 2nd (less favorable to WAS)",
  "Less favorable of PHX and GSW second-round picks to WAS.",
  
  2028, "CHA_LAC_DET_2R", "CHA", "CHA,LAC",
  "2028 CHA or LAC 2nd (more favorable to CHA)",
  "More favorable of CHA and LAC second-round picks to CHA.",
  2028, "CHA_LAC_DET_2R", "DET", "CHA,LAC",
  "2028 CHA or LAC 2nd (less favorable to DET)",
  "Less favorable of CHA and LAC second-round picks to DET.",
  
  2028, "LAL_WAS_ORL_WAS_2R", "ORL", "LAL,WAS",
  "2028 LAL or WAS 2nd (more favorable to ORL)",
  "More favorable of LAL and WAS second-round picks to ORL.",
  2028, "LAL_WAS_ORL_WAS_2R", "WAS", "LAL,WAS",
  "2028 LAL or WAS 2nd (less favorable to WAS)",
  "Less favorable of LAL and WAS second-round picks to WAS.",
  
  2030, "LAC_UTA_CHA_UTA_2R", "CHA", "LAC,UTA",
  "2030 LAC or UTA 2nd (more favorable to CHA)",
  "More favorable of LAC and UTA second-round picks to CHA.",
  2030, "LAC_UTA_CHA_UTA_2R", "UTA", "LAC,UTA",
  "2030 LAC or UTA 2nd (less favorable to UTA)",
  "Less favorable of LAC and UTA second-round picks to UTA.",
  
  2031, "MIN_GSW_CHI_DET_2R", "CHI", "MIN,GSW",
  "2031 MIN or GSW 2nd (more favorable to CHI)",
  "More favorable of MIN and GSW second-round picks to CHI.",
  2031, "MIN_GSW_CHI_DET_2R", "DET", "MIN,GSW",
  "2031 MIN or GSW 2nd (less favorable to DET)",
  "Less favorable of MIN and GSW second-round picks to DET.",
  
  2031, "BOS_CLE_UTA_BOS_2R", "UTA", "BOS,CLE",
  "2031 BOS or CLE 2nd (more favorable to UTA)",
  "More favorable of BOS and CLE second-round picks to UTA.",
  2031, "BOS_CLE_UTA_BOS_2R", "BOS", "BOS,CLE",
  "2031 BOS or CLE 2nd (less favorable to BOS)",
  "Less favorable of BOS and CLE second-round picks to BOS.",
  
  2032, "HOU_PHX_CHI_PHX_2R", "CHI", "HOU,PHX",
  "2032 HOU or PHX 2nd (more favorable to CHI)",
  "More favorable of HOU and PHX second-round picks to CHI.",
  2032, "HOU_PHX_CHI_PHX_2R", "PHX", "HOU,PHX",
  "2032 HOU or PHX 2nd (less favorable to PHX)",
  "Less favorable of HOU and PHX second-round picks to PHX."
)

if (nrow(complex_second_display_specs) > 0) {
  for (i in seq_len(nrow(complex_second_display_specs))) {
    spec <- complex_second_display_specs[i, ]
    orig <- str_split(spec$original_teams, "\\s*,\\s*")[[1]]
    add_display_group(display_asset_id = sprintf("display_%d_R2_%s_%s", 
                                                 spec$year, spec$owner, spec$complex_group),
                      year = spec$year,
                      owner = spec$owner,
                      original_teams = orig,
                      label = spec$label,
                      obligation = spec$obligation,
                      notes = spec$obligation,
                      group_type = "complex_second_entitlement",
                      draft_round = 2L,
                      complex_group_filter = spec$complex_group,
                      pick_types = c("own", "complex"))
  }
}

# Any asset not grouped above is already a one-to-one RealGM-style entitlement
# (locked 2026 pick, normal own pick, protected outgoing/retained row, or simple
# unprotected/protected incoming pick).
grouped_internal_asset_ids <- unique(pick_display_members$asset_id)
ungrouped_assets <- pick_assets %>%
  filter(!.data$asset_id %in% grouped_internal_asset_ids)

if (nrow(ungrouped_assets) > 0) {
  one_to_one_display <- ungrouped_assets %>%
    transmute(display_asset_id = paste0("display_", asset_id),
              owner,
              year,
              round,
              label,
              obligation,
              notes,
              group_type = "single_asset",
              display_group = asset_id,
              member_n = 1L,
              member_original_teams = original_team)
  
  pick_display_assets <- bind_rows(pick_display_assets, one_to_one_display)
  pick_display_members <- bind_rows(pick_display_members,
                                    ungrouped_assets %>%
                                      transmute(display_asset_id = paste0("display_", asset_id), asset_id))
}

pick_display_members <- pick_display_members %>% distinct(display_asset_id, asset_id)
pick_display_assets <- pick_display_assets %>%
  distinct(display_asset_id, .keep_all = TRUE) %>%
  left_join(pick_display_members %>% 
              count(display_asset_id, name = "member_n_actual"),
            by = "display_asset_id") %>%
  mutate(member_n = coalesce(member_n_actual, member_n)) %>%
  select(-member_n_actual) %>%
  arrange(year, owner, label)

cat(sprintf("User-facing pick entitlements: %d display rows from %d internal asset rows\n",
            nrow(pick_display_assets), nrow(pick_assets)))
