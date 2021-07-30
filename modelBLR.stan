
data {
  int<lower = 1> N;                    // number of students
  int<lower = 1> M;                    // number of covariates
  matrix[N, M] x;                      // covariates
  vector[N] y;                         // outcomes
  int<lower=0> N_new;				           // number of students in the prediction data
  matrix[N_new, M] x_new;			         // test data set
  int<lower = 1> np;                   // number of variables with priors
  vector[np] prior_beta_mean;           // prior mean
  vector[np] prior_beta_sd;             // prior sd
}

transformed data {
  real mu_y = mean(y);
}
parameters {
  vector[M] beta;                 // M number of beta_std values, priors for beta - standardized prior
  real<lower = 0> sigma_y;        // sigma lowest value is 0, priros for the sd of the prior
  vector[N_new] y_new;            // predictions
}
model {
  for (i in 2:np) {
  beta[i] ~ normal(prior_beta_mean[i], prior_beta_sd[i]*10);
  }
  for (i in (np+1):M) {
  beta[i] ~ normal(0, 25);  //noninformative part
  }
  sigma_y ~ cauchy(0, 15);        // prior of sd
  y ~ normal(x * beta, sigma_y); // the model for standardized y values
  y_new ~ normal(x_new * beta, sigma_y);  // prediction model
}

