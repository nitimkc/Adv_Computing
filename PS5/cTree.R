
#####################################################################################
## 1. Develop a fnc for classification trees called "cTree"
## 2. INPUT: 
#     - Formula object for specifying the dependent variable and predictors,
#     - dataset that formula will operate on,
#     - depth - integer, max number of nodes in each tree, 
#     - minPoints - min no. of pts in a region
#     - costFnc - used for growing the tree. Options: ME, Gini and Entropy
## 3. OUTPUT: named list with at least these two elements - 
#     - predLabels - vector of predicted labels for the observations in the dataset, 
#     - prob - vector of probabilities for chosen labels
## 4. Use SPAM dataset (provided) 
######################################################################################

## load reqd library
if (!require("plyr")) install.packages("plyr"); library(plyr)
if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
if (!require("assertthat")) install.packages("assertthat"); library(assertthat)

######################################################################################
## 1. Define costFncs 

######################################################################################


MissError <- function(prob) {
  MissError <- 1 - apply(prob,1,max)
  return(MissError)
}


Gini <- function(prob) {
  Gini <- rowSums(prob*(1-prob))
  return(Gini)
}


Entropy <- function(prob) {
  prob[prob == 0] <- 1e-10
  CrossEntropy <- - rowSums(prob*log(prob))
  return(CrossEntropy)
}


######################################################################################
## 2. Develop a fnc that partitions the input space, counts errors for each cut 
#     & chooses the best one
######################################################################################

X <- spambase[, -58]
y <- spambase[, 58]

findThreshold <- function(X, y, costFnc) {
  
  noObs <- nrow(X)
  noFeat <- ncol(X)
  
  cls <- unique(y) 
  k <- length(cls)
  
  # go sequentially over each point and cut between that point & the closest neighbor
  for ( i in 1:(noFeat)) {
    
    for ( j in 1:(noObs-1)) {
      
      # locate potential split
      split <- mean(X[j:(j+1), i])
      
      
      # assign majority predicted classes for each split
      predClass <- rep(NA, noObs)
      
      classLeft <- y[X[,i] < split]
      mostFreqLeft <- as.numeric(names(sort(-table(classLeft)))[1])
      predClass[X[,i] <= split] <- mostFreqLeft 
      
      classRight <- y[X[,i] >= split]
      mostFreqRight <- as.numeric(names(sort(-table(classRight)))[1])
      predClass[X[,i] > split] <- mostFreqRight
      
      # compute class probabilities 
      prob <- matrix(NA, nrow = 2, ncol = k)
      prob[1,] <- table(y[X <= split])/length(classLeft)
      prob[2,] <- table(y[X > split])/length(classRight)
  
      
      # now compute freq of each of the classes in each splits
      #freqLeft <- count(classLeft)$freq/length(classLeft) 
      #freqRight <- count(classRight)$freq/length(classRight) 
      #freqClass <- cbind(freqLeft, freqRight)
      
      # error in this split
      misError <- sum(costFnc(prob))
      
      return(list(error = misError,
                  threshold = split,
                  labels = predClass))
    }
  }
}

    
################################################################################
## 3. Build cTree function now

################################################################################

cTree <- function(formula, data, depth, minPoints, costFnc) {
  
  # verfiy input format
  assert_that(class(formula) == "formula");
  not_empty(data);
  assert_that(is.count(depth)); 
  assert_that(is.count(minPoints));
  assert_that(costFnc %in% c("ME", "Gini", "Entropy"));
  
  # extract data from formula
  y.var <- get.vars(lhs(formula))
  X.vars <- get.vars(rhs(formula))
  
  y <- data[, y.var]
  X <- data[, X.vars]
  
  #find best split
  
  bestSplit <- findThreshold(X, y, costFnc = "Entropy", minPoints = 1)
}















