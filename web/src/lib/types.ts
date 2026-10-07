// Shapes of the files written by 06_web_export.R.

export interface Team {
  abbr: string;
  name: string;
  primary: string;
  secondary: string;
  logo: string | null;
}

export interface DisplayPick {
  id: string;
  owner: string;
  year: number;
  round: number;
  trade_label: string;
  short_label: string;
  group_type: string;
  n_legs: number;
  // Single-leg picks take the protection / swap toggles; grouped entitlements
  // use their display-level draws as-is.
  single_leg: boolean;
  asset_id: string | null;
  fixed_slot: number | null;
  guaranteed: boolean;
  convey_prob_static: number | null;
}

export interface Choice {
  label: string;
  value: string;
}

export interface BlockRef {
  offset: number;
  length: number;
}

export interface TeamFile {
  file: string;
  bytes: number;
  blocks: Record<string, [kind: "f32" | "u8", offset: number]>;
}

export interface Meta {
  generated: string;
  n_draws: number;
  proj_years: number[];
  slot_teams: string[];
  labels: { value_outcome: string; value_unit_long: string };
  protection_choices: Record<"1" | "2", Choice[]>;
  teams: Team[];
  displays: DisplayPick[];
  receiver_raw: Record<string, Record<string, string>>;
  core: { mu: BlockRef; team_slot1: BlockRef; team_slot2: BlockRef };
  team_files: Record<string, TeamFile>;
}

// Draw vectors for one team's picks, keyed like the export's block keys.
export interface TeamBundle {
  f32: Map<string, Float32Array>;
  u8: Map<string, Uint8Array>;
}

export interface Core {
  mu: Float32Array; // [draw][slot - 1], 60 slots per draw
  teamSlot1: Uint8Array; // [year][team][draw], 0 = NA
  teamSlot2: Uint8Array;
}

// Per-pick user controls, keyed by `${side}:${displayId}`.
export interface PickControl {
  protection: string;
  swap: boolean;
}

export type Side = "A" | "B";

// A team logo drawn over a chart at data coordinates.
export interface LogoPoint {
  team: string;
  x: number;
  y: number;
  src: string | null;
  size: number;
}

// summary.json
export interface SummaryTeam extends Team {
  text: string; // readable text color on the primary color
}

export interface PortfolioRow {
  team: string;
  display_pick_count: number;
  cur_total_value: number;
  new_total_value: number;
  delta_total_value: number;
  new_expected_picks: number;
  new_avg_quality: number;
  p_positive: number;
  delta_q10: number | null;
  delta_q90: number | null;
  x_logo: number;
  y_logo: number;
}

export interface PickRow {
  id: string;
  owner: string;
  year: number;
  round: number;
  short_label: string;
  cur_mean: number;
  new_mean: number;
  delta: number;
  impact_bucket: "Own picks" | "Incoming outright picks" | "Swaps / protections";
  cur_expected_pick_count: number;
  new_expected_pick_count: number;
}

export interface Summary {
  gain: { team: string; delta: number; p_positive: number };
  loss: { team: string; delta: number; p_positive: number };
  shifted: number;
  n_moved: number;
  n_future: number;
  top: { owner: string; year: number; short_label: string; delta: number } | null;
  moved_ids: string[];
  mover_ids: string[];
  first_projected_year: number;
  value_metric_desc: string;
  value_outcome: string;
  value_unit: string;
  years: number[];
  teams: SummaryTeam[];
  team_table: { team: string; tier: string; wins: number; losses: number }[];
  portfolio: Record<string, PortfolioRow[]>; // keyed "<year|All>|<round|All>"
  picks: PickRow[];
}

// single/<ABB>.json
export interface ProbSummary {
  prob: number | null;
  q05: number | null;
  q95: number | null;
}

export interface Curve {
  x: number[];
  y: number[];
}

export interface DistStats {
  cur_mean: number;
  cur_q05: number;
  cur_q95: number;
  new_mean: number;
  new_q05: number;
  new_q95: number;
}

export interface SinglePick {
  original_teams: string[];
  fixed_slot_display: string | null;
  obligation: string;
  show_convey: boolean;
  convey: { cur: ProbSummary | null; new: ProbSummary | null };
  swap: { cur: ProbSummary | null; new: ProbSummary | null };
  ev: { stats: DistStats; cur: Curve; new: Curve };
  outcome: { stats: DistStats; cur: Curve; new: Curve };
}

// methodology.json
export interface PickCurveRow {
  pick: number;
  expected_war: number;
  ev_q05: number | null;
  ev_q95: number | null;
  outcome_q10: number | null;
  outcome_q90: number | null;
  emp_mean: number | null;
  p_play: number | null;
  p_play_q05: number | null;
  p_play_q95: number | null;
  emp_p_play: number | null;
}

export interface RankHorizonRow {
  start_rank: number;
  years_ahead: number;
  mean_rank: number;
  qlo: number;
  qhi: number;
  hover: string;
}

export interface LotteryRow {
  system: "Current" | "Proposed 3-2-1";
  seed: number;
  expected_pick: number;
  expected_pick_se: number;
  prob_no1: number;
  prob_no1_se: number;
}

export interface ValidationRow {
  model: string;
  check: string;
  metric: string;
  why: string;
  status: "PASS" | "CHECK" | "INFO" | string;
}

export interface Methodology {
  // Numbers filled into the Value Metric and glossary text in Methodology.svelte.
  value_metric: {
    is_xrapm: boolean;
    draft_years: [number, number];
    replacement: number;
    points_per_win: number;
  };
  value_outcome: string;
  pick_curve: PickCurveRow[];
  tier_matrix: { from: string[]; to: string[]; pct: number[][] };
  rank_matrix_pct: number[][] | null;
  rank_horizon: Record<"40_60" | "25_75" | "10_90", { label: string; rows: RankHorizonRow[] }>;
  rank_colors: string[];
  lottery: LotteryRow[];
  validation: { rows: ValidationRow[]; note: string };
}
