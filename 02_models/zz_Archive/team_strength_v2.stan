// team_strength_v2.stan
// Bayesian Markov chain over all 30 league-wide standings / draft-order ranks.
//
// Rank convention used by the R pipeline:
//   rank_worst = 1  -> worst regular-season record / old lottery seed 1
//   rank_worst = 30 -> best regular-season record / pick 30 under inverse record
//
// The model estimates a 30x30 transition surface:
//   next_rank ~ categorical(theta[current_rank, ])
//
// The historical data are sparse at the individual rank-to-rank cell level, so
// the logits are regularized in three ways:
//   1. distance_slope favors nearby future ranks over extreme jumps;
//   2. adjacent starting ranks borrow information through row smoothness;
//   3. adjacent ending ranks borrow information through column smoothness.
//
// This keeps every rank's local identity while sharing strength with neighbors,
// matching the same spirit as adjacent-pick smoothing in the draft value models.

data {
  int<lower=2> K;                       // number of rank states; production K = 30
  array[K, K] int<lower=0> counts;      // observed transition counts: rank t -> rank t+1
}



parameters {
  matrix[K, K] eta_raw;                 // unconstrained transition-logit surface
  real<upper=0> distance_slope;          // penalty per absolute rank jump
  real<lower=0> sigma_eta;               // global shrinkage for raw logits
  real<lower=0> sigma_row;               // smoothness across adjacent starting ranks
  real<lower=0> sigma_col;               // smoothness across adjacent ending ranks
}



transformed parameters {
  matrix[K, K] eta;                      // row-centered logits after distance penalty
  array[K] simplex[K] theta;             // transition probabilities by current rank

  for (i in 1:K) {
    vector[K] row_eta;
    for (j in 1:K) {
      row_eta[j] = eta_raw[i, j] + distance_slope * abs(i - j);
    }
    row_eta = row_eta - mean(row_eta);   // softmax location identification
    for (j in 1:K) eta[i, j] = row_eta[j];
    theta[i] = softmax(row_eta);
  }
}



model {
  // Weakly informative hyperpriors. The half-normal scales keep the 30x30
  // surface smooth unless the transition data strongly support local movement.
  sigma_eta ~ normal(0, 1.0);
  sigma_row ~ normal(0, 0.35);
  sigma_col ~ normal(0, 0.35);
  distance_slope ~ normal(-0.18, 0.12);

  to_vector(eta_raw) ~ normal(0, sigma_eta);

  // Borrow strength across nearby starting ranks: rank 9 and rank 11 should not
  // have unrelated future-rank distributions.
  for (i in 2:K) {
    eta_raw[i, ] - eta_raw[i - 1, ] ~ normal(0, sigma_row);
  }

  // Smooth adjacent destination ranks so the fitted distribution is a surface,
  // not 30 unrelated categorical spikes.
  for (j in 2:K) {
    eta_raw[, j] - eta_raw[, j - 1] ~ normal(0, sigma_col);
  }

  for (i in 1:K) {
    counts[i] ~ multinomial(theta[i]);
  }
}



generated quantities {
  vector[K] row_log_lik;
  vector[K] row_entropy;
  array[K, K] int counts_rep;

  for (i in 1:K) {
    if (sum(counts[i]) > 0)
      row_log_lik[i] = multinomial_lpmf(counts[i] | theta[i]);
    else
      row_log_lik[i] = 0;

    counts_rep[i] = multinomial_rng(theta[i], sum(counts[i]));

    row_entropy[i] = 0;
    for (j in 1:K)
      row_entropy[i] += -theta[i][j] * log(theta[i][j] + 1e-12);
  }
}
