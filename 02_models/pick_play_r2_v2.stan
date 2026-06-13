// Round 2 P(play) model for NBA draft pick valuation
// ---------------------------------------------------
// This Stan model estimates only the structural-zero hurdle probability:
//   P(any NBA minutes in first four seasons | pick 31-60).
// Conditional 4-year Win Shares for players who do play are handled in R with
// an empirical/Bayesian bootstrap by broad second-round pick band. That keeps
// the right-tail payoff distribution without forcing Stan to identify an
// unstable latent hit mixture.

data {
  int<lower=1> N;
  array[N] int<lower=31, upper=60> pick;
  array[N] int<lower=0, upper=1> played;
}

parameters {
  real logit_play_31;
  real<lower=0> tau_logit_play_rw;
  vector[29] z_logit_play_step;
}

transformed parameters {
  vector[30] logit_play_pick;
  vector<lower=0, upper=1>[30] p_play_pick;

  logit_play_pick[1] = logit_play_31;
  for (p in 2:30) {
    logit_play_pick[p] = logit_play_pick[p - 1] + tau_logit_play_rw * z_logit_play_step[p - 1];
  }

  p_play_pick = inv_logit(logit_play_pick);
}

model {
  // Pick 31 historically has a high played rate, so center near 90% while
  // allowing the data to move it. The random-walk scale prior controls local
  // slot wiggle; tighten this if exact-pick P(play) looks too jagged.
  logit_play_31 ~ normal(logit(0.90), 0.75);
  tau_logit_play_rw ~ normal(0, 0.20);
  z_logit_play_step ~ normal(0, 1);

  for (n in 1:N) {
    int s = pick[n] - 30;
    played[n] ~ bernoulli(p_play_pick[s]);
  }
}

generated quantities {
  vector[30] p_play;
  array[N] int played_rep;
  vector[N] log_lik;

  for (p in 1:30) {
    p_play[p] = p_play_pick[p];
  }

  for (n in 1:N) {
    int s = pick[n] - 30;
    played_rep[n] = bernoulli_rng(p_play_pick[s]);
    log_lik[n] = bernoulli_lpmf(played[n] | p_play_pick[s]);
  }
}
