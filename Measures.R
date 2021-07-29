#Measures

bias <- function (true, pred) mean(true - pred) #bias, true (or observed) value minus predicted value
mae <- function(true, pred) abs(bias(true, pred)) #mean absolute error or absolute bias
per.bias <- function (true, pred) mean((true - pred) / true) #percent bias
mape <- function(true, pred) abs(per.bias(true, pred)) #mean absolute percentage error  or absolute percent bias
se <- function (true, pred) (true - pred)^2 #squared error
mse <- function (true, pred) mean(se(true, pred)) #mean squared error
rmse <- function (true, pred) sqrt(mse(true, pred)) #root mean squared error
rmspe <- function (true, pred) sqrt(mean(((true - pred)/true)^2)) #root mean squared percent error

