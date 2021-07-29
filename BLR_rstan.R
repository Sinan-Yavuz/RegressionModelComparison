#Author: Sinan Yavuz
#Date: July 28, 2021
#Some predictors have a prior but some does not. We will use informative for what we have on 2018 PISA data.
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(rstan)
library(rstanarm)
library(loo)
options(mc.cores = 4)
#priors
prior_beta <- readRDS("priors.RDS")
#data
pisa.imp <- readRDS("pisa2018.RDS")

y <- pisa.imp$PV1READ
x <- pisa.imp[,-grep("PV1READ", colnames(pisa.imp))]

prior_names <- rownames(prior_beta)

#now the data generation function requires the test set
data.stan <- function(y.train, x.train, x.test, prior_beta) {
  x <- cbind(Intercept = 1, x.train) #get x: intercept and all predictors, -1 stands for the dependent variable. this need to be changed if the 
  y <- y.train
  x_nr <- cbind(x[,prior_names], x[,-which(names(x) %in% prior_names)]) #x new rank
  N <- nrow(x)
  M <- ncol(x)
  x_new <- cbind(Intercept = 1, x.test)
  x_new <- x_new[,names(x_nr)]
  N_new <- nrow(x_new)
  np <- length(prior_names)
  list(N = N, M = M, x = x_nr, y = y, x_new = x_new, N_new = N_new, np = np,
       prior_beta_mean = prior_beta[,1], prior_beta_sd = prior_beta[,2])
}


modelstring <- '
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
  vector[N] y_new;                // predictions
}
model {
  for (i in 2:np) {
  beta[i] ~ normal(prior_beta_mean[i], prior_beta_sd[i]);
  }
  for (i in (np+1):M) {
  beta[i] ~ normal(0, 25);  //noninformative part
  }
  sigma_y ~ cauchy(0, 15);        // prior of sd
  y ~ normal(x * beta, sigma_y); // the model for standardized y values
  y_new ~ normal(x_new * beta, sigma_y);  // prediction model
}
'

my.blr <- function(y.train, x.train, x.test, prior_beta, n.chains = 4, n_iter = 2000) {
writeLines(modelstring, con = 'modelBLR.stan')
data <- data.stan(y.train, x.train, x.test, prior_beta)
fit <- stan('modelBLR.stan', data = data, iter = n_iter, chains = n.chains)
est <- summary(fit)$summary
beta <- est[grep('^beta\\[', rownames(est)), ][1:data$M,c("mean","sd")]
rownames(beta) <- colnames(data$x)
beta <- beta[c("Intercept",colnames(x)),]
y_pred <- est[grep('^y_new\\[', rownames(est)), ][1:nrow(x), "mean"]
return(list(beta = beta, y_pred = y_pred))
}

saveRDS(list(beta=beta, y_pred = y_pred),"beta_2018_blr.RDS")
#readRDS("beta_2018_blr.RDS")

