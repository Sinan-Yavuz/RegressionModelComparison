#Run script
#Date: June 22 2020
#Author: Sinan Yavuz
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#rm(list = ls())
source("Packages.R")
source("BLR_rstan.R")
source("k_fold_cv.R")
source("Measures.R")
source("Pred_cov.R")
source("B_stacking.R")

rep <- as.integer(commandArgs(T)[[1]])
m <- as.integer(commandArgs(T)[[2]])

N <- c(200,1000)[m]

##################### CHTC - Ends ############################# - DON'T RUN ENDS
set.seed(40495068)
seed.number <- round(runif(1000,10000000, 99999999),0)
my.seed <- seed.number[rep]
set.seed(my.seed)
#-------------------------#

#------- data file -------#
pisa.imp <- readRDS("pisa2018.RDS")
#summary(pisa.imp)
#summary(pisa2)
pisa.imp <- sample_n(pisa.imp, N) #dplyr function, row sample
y <- pisa.imp[,1] #dependent variable
x <- pisa.imp[,-1] #set of predictors
#priors
prior_beta <- readRDS("priors.RDS")
#-------------------------#

results.flr <- k.fold.cv(method = "flr")
results.blr <- k.fold.cv(method = "BLR")
results.bma <- k.fold.cv(method = "BMA")
#results.fma <- k.fold.cv(method = "fma") #mentioned that in the paper that 
results.lasso <- k.fold.cv(method = "lasso")
results.ridge <- k.fold.cv(method = "ridge")
results.elastic.net <- k.fold.cv(method = "elastic.net")

results.rf <- k.fold.cv(method = "rf") #no coefficient
results.ssvs <- k.fold.cv(method = "ssvs")
results.stacking <- k.fold.cv(method = "stacking") #no coefficient

results <- as.vector(c(cbind(results.flr[[1]],
                             results.blr[[1]],
                             results.bma[[1]][c(1:2),],
                             results.stacking[[1]],
                             results.lasso[[1]],
                             results.ridge[[1]],
                             results.elastic.net[[1]],
                             results.rf[[1]],
                             results.ssvs[[1]]), results.bma[[1]][3:4,]))

results2 <- as.vector(c(cbind(results.flr[[2]],
                             results.blr[[2]],
                             results.bma[[2]],
                             results.lasso[[2]],
                             results.ridge[[2]],
                             results.elastic.net[[2]],
                             results.ssvs[[2]])))

results3 <-c(results, results2) 

write.table(t(c(rep,results3)), paste0("results.N",N,"_",rep,".csv"), row.names = FALSE, col.names = FALSE)


