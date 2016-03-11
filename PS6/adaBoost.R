####################################################################################################
## 1. Develop your own function for AdaBoost.M1 classifier
## 2. Use classification tree as a weak learner 
## 3. Define for a binary classification problem
## 4. Function must be able to deal with arbitrary # of dimensions
## 5. INPUT:  formula     - object for specifying the dependent variable and predictors, 
#             data        - that formula will operate on, 
#             depth       - integer, max number of nodes in each tree, 
#             noTrees     - number of iterations or trees.
## 6. OUTPUT: predLabels  - vector of predicted labels for the obs 
#####################################################################################################

adaBoost <- function(formula, data, depth, noTrees) {
  
  ##  load reqd library
  
  if (!require("rpart")) install.packages("rpart"); library(rpart)
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)

  
  ##  verify input format
  
  assert_that(not_empty(data))
  assert_that(is.count(depth))
  assert_that(is.count(noTrees));
  
  
  ##  extract true labels 
  
  y <- data[ , all.vars(formula)[1]] 
  y <- ifelse(y==1, 1, -1) # set class as {1,-1}
  
  
  ##  intialization
  
  noObs <-  nrow(data)
  environment(formula) <- environment()
  
  #  for errors and weights
  weights <- rep(1/noObs, noObs)
  alpha <- rep(NA, noTrees)
  err_tree <- rep(NA, noTrees) # tree error
  err_final <- rep(NA, noTrees) # adaBoost error
  
  #  to store fitted values
  datamatrix <- vector(mode = "list", length = noTrees) # store predictions of each tree 
  predLabels <- matrix(NA, nrow = noObs, ncol = noTrees) # store predicted labels
  
  
  ##  recursion
  
  for(i in 1:noTrees) {
    
    #  select a base classifier and train data with equal weights (1/N)
    fit <- rpart(formula = formula, 
                 data = data, 
                 weights = weights, 
                 method = "class",
                 control = list(maxdepth = depth)) #, control = rpart.control(maxdepth = depth))
    
    #  store fitted values from base classifier to our data matrix
    datamatrix[[i]] <- fit
    
    #  predict labels and save
    predLabels[ ,i] <- predict(fit, newdata = data, type="class")
    predLabels[ ,i] <- ifelse(predLabels[,i]==2, 1, -1) # map classes {1,-1}
    
    #  calculate tree error
    err_tree[i] <- sum(weights*(predLabels[,i] != y)) / sum(weights)
    
    #  use the errors as information to reweight each observation
    alpha[i] <- log((1 - err_tree[i])/err_tree[i])
    weights <- weights*exp(alpha[i]*(predLabels[,i] != y))
    
    #  output the sign of a combination of all classifier outputs
    finalPred <- sign(rowSums(t(alpha[1:i] * t(predLabels[ ,1:i])))) 
    
    
    ##  final prediction error
    
    err_final[i] <- 1 - sum(finalPred == y)/length(y) 
    
  }
  
  return(list(predLabels=predLabels, 
              datamatrix=datamatrix, 
              alpha=alpha, 
              error=err_final))
}

