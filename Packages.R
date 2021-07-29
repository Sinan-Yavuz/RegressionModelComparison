#--------packages--------#
library(dplyr)
library(BMS) #for BMA with a different priors
#library(MuMIn) #for FMA
library(glmnet) #for Lasso
library(LaplacesDemon) #for KL-divergence
library(BoomSpikeSlab)#for SVSS and ODA
library(randomForest) #for random forest
library(snow) #parallel cores
library(IDPmisc) #NaRV.omit function in predictive loss
library(varhandle)
library(rstan)
library(rstanarm)
library(loo)
 #options(mc.cores = 4)
#parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
#-------------------------#Packages