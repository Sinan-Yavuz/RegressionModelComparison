#Predictive coverage

#-------- Predictive Coverage -------#
my.predict <- function(coef, x.data) {
  result <- as.matrix(cbind(1, x.data)) %*% matrix(coef)
  # -1 because, the first variable is y
  return(result) }

pred.cov <- function(xtest = x.test, ytest = y.test, xtrain = x.train, ytrain = y.train, ytrain.tilda = y.train.tilda, ytilda = y.tilda) {
  # prediction should be the vector of the predicted values
  X.matrix <- as.matrix(cbind(1, xtest))  # now it's cbind(1, x.test)
  X.train.matrix <- as.matrix(cbind(1, xtrain))
  # Hanna (04/11): We should have matrices each for x.test and x.train
  
  mse. <- mse(ytrain, ytrain.tilda)
  # residual standard error must be calculated by the data used to calculate coefficients
  # residual standard error (squared) of the train data set
  core.matrix <- solve(t(X.train.matrix) %*% X.train.matrix)
  # solve is an inverse function
  # Core Matrix ((t(X)X)^-1) j6:m9 in the excel example screenshot
  # Hanna: Should be made with data that produced the coefficients
  
  n <- nrow(xtest)
  df <- n - ncol(X.matrix)  # degree of freedom  # n - k (# IV) - 1
  tcrit <- qt((0.975), df)  # two sided t critical value for .95 coverage
  
  # this part is for calculating the prediction interval
  s.e. <- rep(NA, length = n)  # generate standard error matrix for each row
  for(i in 1:n) {
    X_0 <- matrix(X.matrix[i, ])  # X_0 matrix (from x.test) by each row
    s.e.[i] <- sqrt(mse.*(1 + t(X_0) %*% (core.matrix %*% X_0)))
    # prediction interval formula
    # see for excel example
    # http://www.real-statistics.com/multiple-regression/confidence-and-prediction-intervals/
    # Hanna (04/11) below:
    # SQRT(P8*(1+MMULT(TRANSPOSE(O19:O22),MMULT(J6:M9,O19:O22))))
    # P8: MS_Res, mse. in this code.
    # O19:O22: t(X_0), X_0 in this code. The new data to test prediction.
    # J6:M9: (t(X)X)^-1, "core.matrix" in this code, made with data used to create coefficients.
  }
  upr <- ytilda + (tcrit * s.e.)  # prediction interval, upper limit
  lwr <- ytilda - (tcrit * s.e.)  # prediction interval, lower limit
  return(mean(ytest > lwr & ytest < upr))  # predictive coverage (y = y.test)
}