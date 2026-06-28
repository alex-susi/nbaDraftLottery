// team_strength_v3.stan
// Bayesian Markov chain over all 30 league-wide standings / draft-order ranks.
//
// Rank convention used by the R pipeline:
//   rank_worst = 1  -> worst regular-season record / old lottery seed 1
//   rank_worst = 30 -> best regular-season record / pick 30 under inverse record
//
// This version keeps the 30x30 smoothed-softmax transition surface from v2, but
// removes learned smoothing scales. In v2, sigma_eta / sigma_row / sigma_col
// collapsed toward zero and created poor geometry. Here those scales are fixed
// data inputs chosen in R, so the model remains Bayesian but avoids the funnel /
// boundary behavior from estimating weakly identified smoothness hyperparameters.
//
// The transition logits use:
//   1. a global destination-rank baseline;
//   2. a distance penalty favoring nearby future ranks;
//   3. a small local deviation surface smoothed across rows and columns.


data {
  int<lower=2> K;                       // number of rank states; production K = 30
  array[K, K] int<lower=0> counts;      // observed transition counts: rank t -> rank t+1

  // Fixed smoothing constants supplied by the R pipeline. These are deliberately
  // data, not parameters, to avoid the v2 near-zero scale collapse.
  real<lower=0> eta_scale;              // marginal size of local logit deviations
  real<lower=0> row_smooth_scale;       // adjacent starting-rank smoothness
  real<lower=0> col_smooth_scale;       // adjacent ending-rank smoothness
  real<lower=0> dest_scale;             // size of global destination-rank baseline
}



parameters {
  matrix[K, K] eta_raw;                 // local transition-logit deviations
  vector[K] dest_raw;                   // global destination-rank baseline
  real<upper=0> distance_slope;         // penalty per absolute rank jump
}



transformed parameters {
  vector[K] dest_baseline;              // centered destination effect
  matrix[K, K] eta;                     // row-centered logits
  array[K] simplex[K] theta;            // transition probabilities by current rank

  dest_baseline = dest_scale * (dest_raw - mean(dest_raw));

  for (i in 1:K) {
    vector[K] row_eta;
    for (j in 1:K) {
      row_eta[j] = dest_baseline[j] + eta_raw[i, j] + distance_slope * abs(i - j);
    }
    row_eta = row_eta - mean(row_eta);  // softmax location identification
    for (j in 1:K) eta[i, j] = row_eta[j];
    theta[i] = softmax(row_eta);
  }
}



model {
  // Distance is the main interpretable persistence term. The prior allows broad
  // transition rows, but keeps the sign negative unless the data strongly object.
  distance_slope ~ normal(-0.18, 0.08);

  // Global destination-rank baseline and local deviations.
  dest_raw ~ normal(0, 1);
  to_vector(eta_raw) ~ normal(0, eta_scale);

  // Fixed-scale smoothing across adjacent starting ranks.
  for (i in 2:K) {
    eta_raw[i, ] - eta_raw[i - 1, ] ~ normal(0, row_smooth_scale);
  }

  // Fixed-scale smoothing across adjacent ending ranks.
  for (j in 2:K) {
    eta_raw[, j] - eta_raw[, j - 1] ~ normal(0, col_smooth_scale);
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
