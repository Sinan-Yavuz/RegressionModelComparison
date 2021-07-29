B_stacking <- function(data, test){
  attach(data)
  fits <- list()
  fits[[1]] <- stan_glm(Y ~ 1, data = data, seed = 832762) # This is the baseline model
  fits[[2]] <- update(fits[[1]], formula = Y ~ Female + ESCS + HOMEPOS + ICTRES) # model 1 variables
  fits[[3]] <- update(fits[[1]], formula = Y ~ JOYREAD + PISADIFF + SCREADDIFF + SCREADCOMP) # model 2 variables
  fits[[4]] <- update(fits[[1]], formula = Y ~ METASUM + GFOFAIL + MASTGOAL + SWBP + WORKMAST + ADAPTIVITY + COMPETE) # model 3 variables
  fits[[5]] <- update(fits[[1]], formula = Y ~ PERFEED + TEACHINT + BELONG) # model 3 variables
  detach(data)
  loo_list <- lapply(fits, loo)
  wtsStacking <- loo_model_weights(loo_list, method = "stacking")
  n_draws <- nrow(as.matrix(fits[[1]])) 
  ypredStacking <- matrix(NA, nrow = n_draws, ncol = nobs(fits[[1]]))
  for (d in 1:n_draws) {
    k <- sample(1:length(wtsStacking), size = 1, prob = wtsStacking)
    ypredStacking[d, ] <- posterior_predict(fits[[k]], draws = 1)
  }
  y_test_predStacking <- matrix(NA, nrow = n_draws, ncol = nrow(test))
  for (d in 1:n_draws) {
    k <- sample(1:length(wtsStacking), size = 1, prob = wtsStacking)
    y_test_predStacking[d, ] <- posterior_predict(fits[[k]], newdata = test, draws = 1)
  }
  return(list(y.tilda = colMeans(y_test_predStacking), y.train.tilda = colMeans(ypredStacking)))
}
