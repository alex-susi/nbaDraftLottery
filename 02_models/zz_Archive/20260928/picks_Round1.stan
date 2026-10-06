// -----------------------------------------------------------------------------
// 1st Round Pick NBA Draft Valuation Model
// -----------------------------------------------------------------------------
//
// Model structure:
//   1. Player outcome
//        Each drafted player has one row. The outcome is first-4-year Win Shares.
//
//        ws4[n] ~ Student_t(nu, mu[pick[n]], sigma_pick[pick[n]])
//
//   2. Pick value curve
//        Expected value declines smoothly from the top of the draft to the end
//        of the first round.
//
//        mu[p] = alpha / p^beta + gamma
//
//      where:
//        alpha controls the overall height of the curve.
//        beta controls how quickly value falls from early to late first.
//        gamma is the lower baseline the curve approaches later in the round.
//
//   3. Pick-specific outcome spread
//        Each pick has its own player outcome spread, but nearby picks are tied
//        together so the model does not overreact to one unusually good or bad
//        historical pick slot.
//
//        log_sigma_pick[1] = log_sigma_1
//        log_sigma_pick[p] = log_sigma_pick[p - 1]
//                            + tau_log_sigma_rw * z_sigma_step[p - 1]
//
//   4. Heavy-tailed player outcomes
//        The Student-t likelihood allows for extreme draft outcomes: 
//        stars, busts, and other unusual player paths. 
//        nu controls how much tail risk the model allows.
//
// -----------------------------------------------------------------------------


data {
  int<lower=1> N;                         // Number of players
  array[N] int<lower=1, upper=30> pick;   // Pick each player was drafted at (1-30)
  vector[N] ws4;                          // First-4-year Win Shares
}



parameters {
  // ---------------------------------------------------------------------------
  // Pick value curve: mu[p] = alpha / p^beta + gamma
  // ---------------------------------------------------------------------------
  real log_alpha; // Height of the first-round value curve (log scale)
  real log_beta;  // Decline rate from early to late first round (log scale)
  real gamma;     // Late-first-round baseline value the curve approaches.


  // ---------------------------------------------------------------------------
  // Player outcome spread by pick
  // ---------------------------------------------------------------------------
  real log_sigma_1;                 // Outcome spread at pick 1 (log scale)
  vector[29] z_sigma_step;          // Pick-by-pick adjustments to the spread curve
  real<lower=0> tau_log_sigma_rw;   // How much the spread curve can vary by pick


  // ---------------------------------------------------------------------------
  // Star / bust tail risk
  // ---------------------------------------------------------------------------
  real<lower=2> nu; // Student-t DoF; lower values allow more extreme draft outcomes
}



transformed parameters {
  real<lower=0> alpha = exp(log_alpha); // Height of value curve
  real<lower=0> beta  = exp(log_beta);  // Decline rate

  vector[30] log_sigma_pick;            // Outcome spread by pick, log scale
  vector<lower=0>[30] sigma_pick;       // Positive outcome spread by pick

  log_sigma_pick[1] = log_sigma_1;      // Start spread curve at pick 1


  // Build spread curve for picks 2-30, nearby picks similar
  for (p in 2:30) {
    // Update outcome spread from previous pick.
    log_sigma_pick[p] = log_sigma_pick[p - 1] +
                        tau_log_sigma_rw * z_sigma_step[p - 1];
  }

  sigma_pick = exp(log_sigma_pick);     // Outcome spread from log scale to positive scale
}



model {
  // Priors
  log_alpha ~ normal(log(20), 0.60);    // High expected value for #1 overall pick
  log_beta  ~ normal(log(0.55), 0.50);  // Smooth decline in value from pick 1-30
  gamma     ~ normal(2, 3);             // Late-first round pick expected value

  log_sigma_1       ~ normal(log(8), 0.50); // #1 pick outcome spread centered around 8 WS
  z_sigma_step      ~ std_normal();         // Pick-by-pick outcome-spread adjustments
  tau_log_sigma_rw  ~ normal(0, 0.15);      // Pick-by-pick outcome-spread curve bend

  nu - 2 ~ exponential(0.20); // Finite variance while allowing stars and busts
  
  
  // Likelihood
  // Loop over historical first-round picks
  for (n in 1:N) {
    // Expected 4-year WS for player's draft slot
    real mu = alpha / pow(pick[n], beta) + gamma;

    ws4[n] ~ student_t(nu, mu, sigma_pick[pick[n]]);
  }
}



generated quantities {
  vector[30] ws4_pred;     // Expected 4-year WS by each first-round pick
  vector[30] ws4_pred_sd;  // Player outcome spread by each first-round pick
  vector[N] log_lik;       // Per-player log likelihood for PSIS-LOO and validation
  vector[N] ws4_rep;       // Simulated 4-year WS for each historical player row


  // Loop over first-round picks
  for (p in 1:30) {
    real mu = alpha / pow(p, beta) + gamma; // Expected 4-year WS for this pick

    ws4_pred[p]    = mu;             // Expected value for this pick
    ws4_pred_sd[p] = sigma_pick[p];  // Player outcome spread for this pick
  }
  
  
  // Loop over historical first-round picks
  for (n in 1:N) {
    // Expected 4-year WS for player's draft slot
    real mu = alpha / pow(pick[n], beta) + gamma;

    log_lik[n] = student_t_lpdf(ws4[n] | nu, mu, sigma_pick[pick[n]]);

    ws4_rep[n] = student_t_rng(nu, mu, sigma_pick[pick[n]]);
  }
}
