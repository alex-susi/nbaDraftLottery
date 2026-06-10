data {
  int<lower=1> K;
  array[K, K] int<lower=0> counts;
  matrix<lower=0>[K, K] alpha;
}
parameters {
  // Use matrix instead of array[K] simplex[K] for standalone GQ.
  // CmdStan CSV output can round simplex rows to sums like 1.000000956,
  // which fails simplex validation when reused as fitted_params.
  matrix<lower=0>[K, K] theta;
}
generated quantities {
  vector[K] log_lik;
  array[K, K] int counts_rep;

  for (i in 1:K) {
    vector[K] theta_i;
    real theta_sum = 0;

    for (j in 1:K) {
      theta_sum += theta[i, j];
    }

    for (j in 1:K) {
      theta_i[j] = theta[i, j] / theta_sum;
    }

    log_lik[i] = multinomial_lpmf(counts[i] | theta_i);
    counts_rep[i] = multinomial_rng(theta_i, sum(counts[i]));
  }
}
