#K-fold.cv

k.fold.cv <- function(X = x, Y = y, method, k = 10, seed = my.seed){
  N <- length(Y) #sample size
  n <- as.integer(N/k) #n is the sample size of the one fold
  D <- cbind(Y, X)[1:(n*k),] #data with residual value 0, dividable to n*k - D is Dataset
  set.seed(seed)
  D <- D[sample(nrow(D)),] #shuffle the dataset - because some consecutive students may come from the same schools and we are not considering the multilevel structure right now
  folds <- split(D, f = as.factor(1:k)) #generate folds
  pb <- txtProgressBar(min = 0, max = k, style = 3) #progress bar
  result <- matrix(nrow = k, ncol = 2) #ncol=11 because I have 11 measures
  predicted.cov <- vector()
  cf_results <- data.frame(matrix(nrow = ncol(D), ncol = k))
  if (is.null(method)) {stop("Select one of the available methods: flr, BLR, BMA, fma, ssvs, lasso, ridge, elastic.net, rf")}
  else if (method == "flr") {
    for(i in 1:k){
      test <- folds[[i]] #test dataset 
      x.test <- test[,colnames(X)] #predictors of the test data set
      y.test <- test[,"Y"] #outcome of the test dataset
      train <- bind_rows(folds[-i]) #bind_row is a dplyr function to bind list into dataframe
      y.train <- train[,"Y"] #this will be used in the pred.cov function to calculate rmse
      x.train <- train[,colnames(X)] #predictors of the training dataset
      fit <- lm(Y ~ ., data = train) #Model
      cf <- fit$coefficients #coefficients of the predicted model
      y.tilda <- my.predict(coef = cf, x.data=x.test) #y.tilda is the predicted scores of the test data set
      p.a <- density(y.test, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      p.s <- density(y.tilda, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      result[i,] <- c(rmse(y.test, y.tilda),
                      KLD(p.s, p.a)$sum.KLD.py.px)
      cf_results[,i] <- cf
      setTxtProgressBar(pb, i) #progress bar
    }
  }
  else if (method == "BMA")  {
    result <- matrix(nrow = k, ncol = 4) #ncol=10 because I have 13 measures for BMA
    for(i in 1:k){
      test <- folds[[i]] #test dataset 
      x.test <- test[,colnames(X)]
      y.test <- test[,"Y"]
      train <- bind_rows(folds[-i]) #bind_row is a dplyr function to bind list into dataframe
      fit <- bms(train, burn = 20000, iter = 100000, nmodel = 500, mcmc = "enumerate", #enumerate gives the same results each time
                 g = "UIP", mprior = "random", mprior.size = NA, user.int = F, 
                 start.value = NA, g.stats = FALSE)
      cf <- coef(fit, order.by.pip = FALSE, include.constant = TRUE, exact = FALSE)[,2]
      cf <- c(cf[length(cf)],cf[-length(cf)]) #take intercept to the beginning
      y.tilda <- predict(fit, newdata = x.test, exact = FALSE)
      totpmp <- colSums(pmp.bma(fit))[2] #total PMP, which BMA uses
      bestpmp <- pmp.bma(fit)[1,2] #best model PMP
      p.a <- density(y.test, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      p.s <- density(y.tilda, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      result[i,] <- c(rmse(y.test, y.tilda),
                      KLD(p.s, p.a)$sum.KLD.py.px,
                      totpmp, bestpmp)
      cf_results[,i] <- cf
      setTxtProgressBar(pb, i)
      #pmp.bma(fit)[1] #I need to think about this
    }
  }
  else if (method == "fma")  {
    print("FMA method needs to be updated to produce the coefficients")
    for(i in 1:k){
      test <- folds[[i]] #test dataset 
      x.test <- test[,colnames(X)]
      y.test <- test[,"Y"]
      train <- bind_rows(folds[-i]) #bind_row is a dplyr function to bind list into dataframe
      y.train <- train[,"Y"]
      x.train <- train[,colnames(X)]
      fit <- lm(Y ~ ., data = train, na.action = na.fail) #Model
      # clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
      # clust <- try(makeCluster(getOption("cl.cores", 4), type = clusterType))
      fma.models <- dredge(fit, rank = "AICc")
      # AIC because the fundamental idea of FMA relies on AIC
      #Because of this, the AICc should be regularly used rather than the AIC, to cover both small- and large-sample situations.Long, J. D. (2012). Longitudinal data analysis for the behavioral sciences using R. Sage.
      mod5 <- get.models(fma.models, subset = delta < 2) # get the best 5 models - because it's the default in BMA, other option is determine cut point delta <= 4 - it was 4 as a default in the package
      mavg5 <- model.avg(mod5, revised.var = FALSE)
      y.train.tilda <- predict(mavg5)
      y.tilda <- predict(mavg5, newdata = test)
      p.a <- density(y.test, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      p.s <- density(y.tilda, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      result[i,] <- c(rmse(y.test, y.tilda),
                      KLD(p.s, p.a)$sum.KLD.py.px)
      setTxtProgressBar(pb, i)
    }
  }
  else if (method == "rf")  {
    for(i in 1:k){
      test <- folds[[i]] #test dataset 
      x.test <- test[,colnames(X)]
      y.test <- test[,"Y"]
      train <- bind_rows(folds[-i]) #bind_row is a dplyr function to bind list into dataframe
      y.train <- train[,"Y"]
      x.train <- train[,colnames(X)]
      fit <- randomForest(Y ~ ., data = train)
      y.tilda <- predict(fit, newdata = test)
      p.a <- density(y.test, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      p.s <- density(y.tilda, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      result[i,] <- c(rmse(y.test, y.tilda),
                      KLD(p.s, p.a)$sum.KLD.py.px)
      setTxtProgressBar(pb, i)
    }
  }
  else if (method == "ssvs")  {
    for(i in 1:k){
      test <- folds[[i]] #test dataset 
      x.test <- test[,colnames(X)]
      y.test <- test[,"Y"]
      train <- bind_rows(folds[-i]) #bind_row is a dplyr function to bind list into dataframe
      y.train <- train[,"Y"]
      x.train <- train[,colnames(X)]
      fit <- lm.spike(Y ~ ., data = train,niter=550000, model.options = SsvsOptions())
      y.tilda <- predict(fit, newdata = test, burn = 50000, mean.only = TRUE)
      p.a <- density(y.test, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      p.s <- density(y.tilda, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      result[i,] <- c(rmse(y.test, y.tilda),
                      KLD(p.s, p.a)$sum.KLD.py.px)
      cf_results[,i] <- colMeans(fit$beta)
      setTxtProgressBar(pb, i)
    }
  }
  else if (method == "BLR")  {
    for(i in 1:k){
      test <- folds[[i]] #test dataset 
      x.test <- test[,colnames(X)]
      y.test <- test[,"Y"]
      train <- bind_rows(folds[-i]) #bind_row is a dplyr function to bind list into dataframe
      y.train <- train[,"Y"]
      x.train <- train[,colnames(X)]
      blr_res <- my.blr(y.train = y.train, x.train = x.train, x.test = x.test, prior_beta = prior_beta)
      y.tilda <- blr_res$y_pred
      p.a <- density(y.test, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      p.s <- density(y.tilda, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      result[i,] <- c(rmse(y.test, y.tilda),
                      KLD(p.s, p.a)$sum.KLD.py.px)
      cf_results[,i] <- blr_res$beta
      setTxtProgressBar(pb, i)
    }
  }
  else if (method == "lasso" | method == "ridge" | method == "elastic.net")  {
    if(method == "lasso") {alpha=1}
    if(method == "ridge") {alpha=0}
    if(method == "elastic.net") {alpha=0.5}
    for(i in 1:k){
      test <- folds[[i]] #test dataset 
      x.test <- test[,colnames(X)]
      y.test <- test[,"Y"]
      train <- bind_rows(folds[-i]) #bind_row is a dplyr function to bind list into dataframe
      y.train <- train[,"Y"]
      x.train <- train[,colnames(X)]
      #next 2 lines are coming from https://stats.stackexchange.com/questions/188753/lasso-regression-for-predicting-continuous-variable-variable-selection
      crossval <-  cv.glmnet(x = as.matrix(x.train), y = y.train, alpha = alpha)
      penalty <- crossval$lambda.min #optimal lambda - #penalty #minimal shrinkage , value of lambda that gives minimum cvm (The mean cross-validated error)
      fit <- glmnet(x = as.matrix(x.train), y = y.train, alpha = alpha) # different values of alpha return different estimators, alpha = 1 is the lasso.
      y.tilda <- predict(fit, newx = as.matrix(x.test), s = penalty)
      cf <- matrix(coef(fit, s = penalty))
      #measurement tools
      p.a <- density(y.test, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      p.s <- density(y.tilda, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
      result[i,] <- c(rmse(y.test, y.tilda),
                      KLD(p.s, p.a)$sum.KLD.py.px)
      cf_results[,i] <- cf
      setTxtProgressBar(pb, i)
    }
  }
    else if (method == "stacking")  {
      for(i in 1:k){
        test <- folds[[i]] #test dataset 
        x.test <- test[,colnames(X)]
        y.test <- test[,"Y"]
        train <- bind_rows(folds[-i]) #bind_row is a dplyr function to bind list into dataframe
        y.train <- train[,"Y"]
        x.train <- train[,colnames(X)]
        fit <- B_stacking(train, test)
        y.tilda <- fit$y.tilda
        p.a <- density(y.test, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
        p.s <- density(y.tilda, 'SJ', from = min(c(y.tilda, y.test)), to = max(c(y.tilda, y.test)))$y
        result[i,] <- c(rmse(y.test, y.tilda),
                        KLD(p.s, p.a)$sum.KLD.py.px)
        setTxtProgressBar(pb, i)
      }
  }
  if(method == "BMA"){
    measures <- list(c("RMSE", "KLD", "TotPMP", "BestPMP"), c(method))
    m.result <- colMeans(result) #mean result: get the average of results
    m.result <- matrix(m.result, dimnames = measures)
    return(list(m.result,rowMeans(cf_results)))
  }
  if(method == "rf"){
    measures <- list(c("RMSE", "KLD"), c(method))
    m.result <- colMeans(result) #mean result: get the average of results
    m.result <- matrix(m.result, dimnames = measures)
    return(list(m.result))
  }
  else{
    measures <- list(c("RMSE", "KLD"), c(method))
    m.result <- colMeans(result) #mean result: get the average of results
    m.result <- matrix(m.result, dimnames = measures)
    return(list(m.result,rowMeans(cf_results)))
  }
}
