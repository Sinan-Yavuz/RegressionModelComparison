
#Get descriptives
library(xtable)
library(psych)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #remove this code before chtc

dir()[grep(".csv", dir())]

data <- read.csv("pisa.2018.usa.imp.read.csv")

library(mctest)

mod <- lm(PV1READ ~ ., data = data)
mctest(mod)
imcdiag(mod)

data$Female <- ifelse(data$Female=="Female",1,0)
xtable(describe(data)[,c("mean", "sd", "median", "min", "max", "skew", "kurtosis")])


#results table
dir("./Results/")
N200 <- read.xlsx("./Results/all.results.xlsx", sheetIndex = 1, as.data.frame = TRUE)
N200_res <- N200[1:11,]
rownames(N200_res) <- N200_res[,1]

N200_res$NA. <- NULL
N200_res <- N200_res[c("pred.cov", "bias", "rmspe", "KL", "Pred Error"),]
methods <- colnames(N200_res)
N200_res <- apply(N200_res, 1, as.numeric)

row.names(N200_res) <- methods

xtable(N200_res, digits = 3)


N200_pmp <- N200[13:15,]
colnames(N200_pmp) <- N200_pmp[1,]
N200_pmp <-  N200_pmp[2:3,]
rownames(N200_pmp) <- N200_pmp[,1]
N200_pmp$'NA' <- NULL
N200_pmp <- N200_pmp[,c("mean", "sd", "median", "min", "max")]
N200_pmp <- apply(N200_pmp, 2, as.numeric)
rownames(N200_pmp) <- c("Tot.PMP", "Best.PMP")

#
N1000 <- read.xlsx("./Results/all.results.xlsx", sheetIndex = 2, as.data.frame = TRUE)
N1000_res <- N1000[1:11,]
rownames(N1000_res) <- N1000_res[,1]

N1000_res$NA. <- NULL
N1000_res <- N1000_res[c("pred.cov", "bias", "rmspe", "KL", "Pred Error"),]
methods <- colnames(N1000_res)
N1000_res <- apply(N1000_res, 1, as.numeric)

row.names(N1000_res) <- methods
xtable(N1000_res, digits = 3)



N1000_pmp <- N1000[13:15,]
colnames(N1000_pmp) <- N1000_pmp[1,]
N1000_pmp <-  N1000_pmp[2:3,]
rownames(N1000_pmp) <- N1000_pmp[,1]
N1000_pmp$'NA' <- NULL
N1000_pmp <- N1000_pmp[,c("mean", "sd", "median", "min", "max")]
N1000_pmp <- apply(N1000_pmp, 2, as.numeric)
rownames(N1000_pmp) <- c("Tot.PMP", "Best.PMP")

xtable(rbind(N200_pmp,N1000_pmp))



desc_study1 <- read.xlsx("./../../../../Box/BDB/Data/Copy of descriptive.xlsx", sheetIndex = 2)
xtable(desc_study1)

