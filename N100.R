#Run script
#Date: Aug 9 2020
#Author: Sinan Yavuz
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Packages.R")
source("BLR_rstan.R")
source("k_fold_cv.R")
source("Measures.R")
source("PL_TL.R")
source("Pred_cov.R")
source("B_stacking.R")


k <- as.integer(commandArgs(T)[[1]])
m <- as.integer(commandArgs(T)[[3]])
N <- c(200,1000)[k]

rep = m
##################### CHTC - Ends ############################# - DON'T RUN ENDS
set.seed(40495068)
seed.number <- round(runif(1000,10000000, 99999999),0)
my.seed <- seed.number[rep]
set.seed(my.seed)
#-------------------------#

#------- data file -------#
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #remove this code before chtc - this sets your script location - Don't change!
#pisa.imp <- read.csv("pisa.2018.usa.imp.reg.csv") #imputed (by miBMA) 2018 PISA file with determined predictors
pisa.imp <- read.csv("pisa.2018.usa.imp.read.csv")
pisa.imp$Female <- ifelse(pisa.imp$Female=="Female",1,0)
#summary(pisa.imp)
#summary(pisa2)
pisa.imp <- sample_n(pisa.imp, N) #dplyr function, row sample
y <- pisa.imp[,1] #dependent variable
x <- pisa.imp[,-1] #set of predictors
#-------------------------#


undebug(k.fold.cv)

results.flr <- k.fold.cv(method = "flr")
results.blr <- k.fold.cv(method = "BLR")
results.bma <- k.fold.cv(method = "BMA")
#results.fma <- k.fold.cv(method = "fma") #mentioned that in the paper that 
results.lasso <- k.fold.cv(method = "lasso")
results.ridge <- k.fold.cv(method = "ridge")
results.elastic.net <- k.fold.cv(method = "elastic.net")
results.rf <- k.fold.cv(method = "rf")
results.ssvs <- k.fold.cv(method = "ssvs")
results.stacking <- k.fold.cv(method = "stacking")

results <- as.vector(c(cbind(results.flr,
                               results.blr2,
                               results.bma[c(1:11,14),],
                               results.stacking,
                               results.lasso,
                               results.ridge,
                               results.elastic.net,
                               results.rf,
                               results.ssvs), results.bma[12:13,]))

write.table(t(c(rep,results)), paste0("results.N",N,"_",rep,".csv"), row.names = FALSE, col.names = FALSE)

