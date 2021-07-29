var2009 <- c("ST04Q01", "ESCS", "METASUM", "HOMEPOS", "ICTRES", "JOYREAD", "PV1READ")
pisa.2009 <- read.csv("/Users/syavuz/Box/PISA/2009/INT_stui_2009.USA.csv")

pisa.2009.stu <- pisa.2009[,var2009]

head(pisa.2009.stu)

pisa.2009.stu[pisa.2009.stu==9999] <- NA
summary(pisa.2009.stu)
pisa.2009.stu[pisa.2009.stu==9997] <- NA

names(pisa.2009.stu)[1] <- "Gender"

str(pisa.2009.stu)

write.csv(pisa.2009.stu, "/Users/syavuz/Dropbox/FMA vs BMA/R.and.CondorCodes/Analyses_Jul26/PISA2009_forInf.csv", row.names = FALSE)

#RUN BLR for parameters of 2009
#BLR with rstan
pisa.2009.stu <- read.csv("/Users/syavuz/Dropbox/FMA vs BMA/R.and.CondorCodes/Analyses_Jul26/PISA2009_forInf.csv")

library(dplyr)
library(rstan)
library(rstanarm)
library(loo)
options(mc.cores = 4)

data.stan <- function(y.train, x.train) {
  x <- cbind(Intercept = 1, x.train) #get x: intercept and all predictors, -1 stands for the dependent variable. this need to be changed if the 
  y <- y.train
  N <- nrow(x)
  M <- ncol(x)
  list(N = N, M = M, x = x, y = y)
}

modelstring <- '
data {
  int<lower = 1> N;                    // number of students
  int<lower = 1> M;                    // number of covariates
  matrix[N, M] x;                      // covariates
  vector[N] y;                         // outcomes
}

transformed data {
  vector[M] mu_x;                      // mean of the each covariate
  vector[M] sd_x;                      // sd of each covariate 
  matrix[N, M] x_std;                  // 
  
  real mu_y = mean(y);
  real sd_y = sd(y);
  vector[N] y_std = (y - mu_y) / sd_y;  //standardized dependent variable
  
  // x[, 1] is the intercept
  x_std[, 1] = x[, 1];                // set intercept to 1
  for (m in 2:M) {
    mu_x[m] = mean(x[, m]);           // mean of each covariate
    sd_x[m] = sd(x[, m]);             // sd of each covariate
    x_std[, m] = (x[, m] - mu_x[m]) / sd_x[m];  // standardized full set of predictors
  }
}

parameters {
  vector[M] beta_std;                 // M number of beta_std values, priors for beta - standardized prior
  real<lower = 0> sigma_y_std;        // sigma lowest value is 0, priros for the sd of the prior
}

model {
  beta_std ~ normal(0, 25);           // prior
  sigma_y_std ~ cauchy(0, 15);        // prior of sd
  
  y_std ~ normal(x_std * beta_std, sigma_y_std); // the model for standardized y values
}

generated quantities {
  vector[M] beta;                     // M number of beta
  real<lower = 0> sigma_y = sigma_y_std * sd_y;     // estimated sd 
  
  beta[1] = sd_y * beta_std[1] + mu_y;     // non standardized beta
  for (m in 2:M) {
    beta[m] = sd_y / sd_x[m] * beta_std[m];   // non standardized beta
    beta[1] -= beta[m] * mu_x[m];
  }
  
}
'

my.blr <- function(y.train, x.train, n.chains = 4, n_iter = 10000) {
  writeLines(modelstring, con = 'modelBLR.stan')
  data <- data.stan(y.train, x.train)
  fit <- stan('modelBLR.stan', data = data, iter = n_iter, chains = n.chains)
  est <- summary(fit)$summary
  beta <- est[grep('^beta\\[', rownames(est)), ][1:data$M, "mean"]
  return(est)
}

dt <- na.omit(pisa.2009.stu)
y <- dt$PV1READ
x <- dt[,-7]
priorBLR <- my.blr(y, x)

round(priorBLR,3)

beta <- priorBLR[grep('^beta\\[', rownames(priorBLR)), ][1:7, "mean"]




modelstring_nonstd <- '
data {
  int<lower = 1> N;                    // number of students
  int<lower = 1> M;                    // number of covariates
  matrix[N, M] x;                      // covariates
  vector[N] y;                         // outcomes
  int<lower=0> N_new;
  matrix[N_new, M] x_new;
}

transformed data {
  real mu_y = mean(y);
}
parameters {
  vector[M] beta;                 // M number of beta_std values, priors for beta - standardized prior
  real<lower = 0> sigma_y;        // sigma lowest value is 0, priros for the sd of the prior
  vector[N] y_new;                  // predictions
}
model {
  beta[1] ~ normal(mu_y, 25);           // the average score is around 500
  for (m in 2:M) {
  beta[m] ~ normal(0, 25);
  }
  sigma_y ~ cauchy(0, 15);        // prior of sd
  y ~ normal(x * beta, sigma_y); // the model for standardized y values
  y_new ~ normal(x_new * beta, sigma_y);  // prediction model
}
'

data.stan <- function(y.train, x.train, x.test) {
  x <- cbind(Intercept = 1, x.train) #get x: intercept and all predictors, -1 stands for the dependent variable. this need to be changed if the 
  y <- y.train
  N <- nrow(x)
  M <- ncol(x)
  x_new <- cbind(Intercept = 1, x.test)
  N_new <- nrow(x_new)
  list(N = N, M = M, x = x, y = y, x_new = x_new, N_new = N_new)
}


writeLines(modelstring_nonstd, con = 'modelBLR.stan')
data <- data.stan(y, x, x)
fit2 <- stan('modelBLR.stan', data = data, iter = 2000, chains = 4)
est2 <- summary(fit2)$summary
beta <- est2[grep('^beta\\[', rownames(est2)), ][1:7, ]
beta

y_ped <- est2[grep('^y_new\\[', rownames(est2)), ][1:nrow(x), "mean"]

library(Metrics)
rmse(y, y_ped)

x$Gender <- factor(x$Gender)

lm(PV1READ~.,dt)


str(x)
