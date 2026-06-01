// team_strength_model.stan
// Hierarchical AR(1) team strength projection with covariates
//
// Core model:
//   s_{i,t+1} = mu + phi * (s_{i,t} - mu)
//               + beta_age * (age_{i,t} - mean_age)
//               + beta_cont * (continuity_{i,t} - mean_cont)
//               + epsilon_{i,t}
//
// Covariates:
//   - age: average weighted age of roster (older → faster decline)
//   - continuity: year-over-year roster overlap (higher → more predictable)
//
// Observed data:
//   - Multiple seasons of win% to inform AR(1) dynamics
//   - Current season age and continuity for forward projection

data {
  int<lower=1> T;                          // number of teams
  int<lower=2> S;                          // number of historical seasons
  matrix[T, S] historical_wpct;            // win% history [team, season]
  vector[T] current_age;                   // current avg weighted age
  vector[T] current_continuity;            // current roster continuity (0-1)
  int<lower=1> N_future;                   // seasons to project forward
}

parameters {
  real<lower=0.3, upper=0.7> mu;           // league mean win%
  real<lower=0, upper=0.98> phi;           // AR(1) persistence
  real beta_age;                           // effect of roster age
  real beta_cont;                          // effect of roster continuity
  real<lower=0> sigma_team;                // innovation SD
  real<lower=0> sigma_obs;                 // observation noise
  matrix<lower=0.05, upper=0.95>[T, S] strength; // latent true strengths
}

model {
  // Priors
  mu         ~ beta(50, 50);
  phi        ~ beta(4, 3);
  beta_age   ~ normal(-0.005, 0.01);       // older rosters tend to decline
  beta_cont  ~ normal(0.02, 0.02);         // continuity aids consistency
  sigma_team ~ exponential(10);
  sigma_obs  ~ exponential(10);

  // Initial season prior
  for (i in 1:T) {
    strength[i, 1] ~ normal(mu, 0.15);
  }

  // AR(1) transitions for historical seasons
  for (i in 1:T) {
    for (s in 2:S) {
      real predicted = mu + phi * (strength[i, s - 1] - mu);
      strength[i, s] ~ normal(predicted, sigma_team);
    }
  }

  // Observation model — observed win% is noisy measure of strength
  for (i in 1:T) {
    for (s in 1:S) {
      historical_wpct[i, s] ~ normal(strength[i, s], sigma_obs);
    }
  }
}

generated quantities {
  // Project forward N_future seasons from current strength
  // incorporating age and continuity effects
  matrix[T, N_future] proj_wpct;

  real mean_age  = mean(current_age);
  real mean_cont = mean(current_continuity);

  for (i in 1:T) {
    real s = strength[i, S];  // start from most recent latent strength
    for (yr in 1:N_future) {
      // Age effect decays continuity info over time
      // Continuity effect weakens for farther projections
      real age_effect  = beta_age * (current_age[i] - mean_age);
      real cont_effect = beta_cont * (current_continuity[i] - mean_cont);
      // Covariates fade linearly as we project further out
      real fade = fmax(0.0, 1.0 - 0.15 * (yr - 1));

      s = mu
          + phi * (s - mu)
          + fade * age_effect
          + fade * cont_effect
          + normal_rng(0, sigma_team);
      s = fmin(fmax(s, 0.05), 0.95);

      proj_wpct[i, yr] = s;
    }
  }
}
