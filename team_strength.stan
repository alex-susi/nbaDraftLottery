// team_strength.stan
// Bayesian Markov chain over the FIVE 3-2-1-lottery tiers.
//
// States (aligned exactly to the approved 2027 lottery structure):
//   1 = relegation   3 worst records          (2 balls each)
//   2 = nonplayin     next 7 non-play-in teams (3 balls each)
//   3 = playin_seed   the four 9/10 seeds       (2 balls each)
//   4 = playin_loser  the two 7v8 play-in losers (1 ball each)
//   5 = playoff       the 14 playoff teams      (0 balls)
//
// The state a team occupies in season t determines how its OWN pick is seeded
// in the lottery. We model how teams move between these tiers year to year as
// a first-order Markov chain. Each row of the 5x5 transition matrix gets a
// Dirichlet prior whose concentrations encode tier adjacency (you are far more
// likely to stay put or move one tier than to leap from relegation to
// playoff). Counts of observed historical transitions update the prior:
//
//   theta[i, ] ~ Dirichlet(alpha[i, ])
//   counts[i, ] ~ Multinomial(theta[i, ])
//
// Dirichlet-Multinomial is conjugate, so the posterior is simply
// Dirichlet(alpha + counts). We nonetheless fit in Stan so we get MCMC
// diagnostics, posterior draws for the full downstream simulation, and a
// single consistent toolchain. (The R script also computes the closed-form
// posterior mean as a cross-check.)

data {
  int<lower=2> K;                       // number of tiers (5)
  array[K, K] int<lower=0> counts;      // observed transition counts i -> j
  matrix<lower=0>[K, K] alpha;          // Dirichlet prior concentrations
}

parameters {
  array[K] simplex[K] theta;            // each row is a transition distribution
}

model {
  for (i in 1:K) {
    theta[i] ~ dirichlet(to_vector(alpha[i, ]));
    counts[i] ~ multinomial(theta[i]);
  }
}

generated quantities {
  // Row-wise log-lik for diagnostics, and the implied stationary-ish
  // one-step entropy of each row (useful for sanity checks).
  vector[K] row_log_lik;
  vector[K] row_entropy;

  for (i in 1:K) {
    if (sum(counts[i]) > 0)
      row_log_lik[i] = multinomial_lpmf(counts[i] | theta[i]);
    else
      row_log_lik[i] = 0;

    row_entropy[i] = 0;
    for (j in 1:K)
      row_entropy[i] += -theta[i][j] * log(theta[i][j] + 1e-12);
  }
}
