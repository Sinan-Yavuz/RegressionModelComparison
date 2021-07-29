#### Predictive Loss and Training Loss ####
PL <- function (ytest = y.test, ytrain.tilda = y.train.tilda) #predictive loss - Wang Gelman 2015, Eq 6. first line, right side log p(yi|D ,M). It will be averaged over k later on
{
  n <- length(ytest) #sample size
  probability <- rep(NA, n) #generate a vector called probability
  for(i in 1:n){
    probability[i] <-  mean(ytest[i] > ytrain.tilda) } #p(y_i|D^k, M), ytrain.tilda is the predicted scores of the train data set
  result <- -mean(NaRV.omit(log(probability))) #NaRV.omit removes Inf values, 
  return(result)
}

#TL is independent to k-fold - because this is the lower bound - doesn't have anything with the tested model
#Wang Gelman 2015, Eq 7. second line, right side log p(y|D, M_s)
TL <- function (Y = y) #Y is the all observed outcome values, includes both y.train and y.test 
{
  n <- length(Y) #sample size
  probability <- rep(NA, n)
  for(i in 1:n){
    probability[i] <-  mean(Y[i] > Y) } #p(y|D, M_s), pdf of Y[i] in the distribution of Y. This gives us the lower bound of predictive loss. 
  result <- -mean(NaRV.omit(log(probability))) #NaRV.omit removes Inf values 
  return(result)
}

