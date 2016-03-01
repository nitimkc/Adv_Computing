## setwd("~/DATASCIENCE/2_Trimester/15D012ACM/problemSets/PS5")
#spambase <- read.csv("~/DATASCIENCE/2_Trimester/15D012ACM/problemSets/PS5/spambase.data", header=FALSE)
#X <- spambase[,-58]
#y <- spambase[,58]
#x <- spambase[,1]
#costFnc = "Entropy"


cTree <- function(formula, data, depth, minPoints = 1, costFnc = "Entropy") {
  
  ################################################################################  
  ## load reqd library
  ################################################################################
  
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
  if (!require("plyr")) install.packages("plyr"); library(plyr)
  if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
  
  ################################################################################  
  ## define error fncs
  ################################################################################
  
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
  
  ################################################################################
  # develop threshold fnc
  ################################################################################
  
  #findThreshold(x, y, costFnc = "Entropy")
  findThreshold <- function(x, y, costFnc) {
    
    noObs <- length(unique(x))
    y <- as.numeric(y)
    k <- length(unique(y))
    errors <- rep(NA, (noObs -1))
    thresholds <- rep(NA, (noObs -1))
    splitLabels <- matrix(NA, ncol= 2, nrow=(noObs -1))
    splitProbs <- matrix(NA, ncol= 2, nrow=(noObs -1))
    
    # go sequentially over each point and cut between that point & the closest neighbor
    
      for ( i in 1:(noObs-1)) {
        
        # locate potential split
        split <- mean(x[i:(i+1)])
        
        
        # assign majority class in each split as predicted class
        predClass <- rep(NA, noObs)
        
        classLeft <- y[x < split]
        mostFreqLeft <- as.numeric(names(sort(-table(classLeft)))[1])
        predClass[x < split] <- mostFreqLeft 
        
        classRight <- y[x >= split]
        mostFreqRight <- as.numeric(names(sort(-table(classRight)))[1])
        predClass[x >= split] <- mostFreqRight
        
        
        # compute class probabilities based on majority classifier for each split
        probLeft <- sort(-table(classLeft))[1]/length(classLeft)
        probRight <- sort(-table(classRight))[1]/length(classRight)
        prob <- cbind(-probLeft, -probRight)

        
        # errors in this split
        misClsError <-
          if(costFnc == "Entropy"){
            misClsErr <- Entropy(prob)
          } else if(costFnc == "Gini"){
            misClsErr <- Gini(prob)
          } else if(costFnc == "ME"){
            misClsErr <- ME(prob)
          } else {stop("error in cost function argument")}
        
        
        # recording the accuracy, thresholds and labels of the splitted interval
        errors[i] <- sum(misClsError)
        thresholds[i] <- split
        splitLabels[i,] <- c(predClass[x < split][1],
                               predClass[x >= split][1])
        splitProbs[i,] <- prob
      }
    
    
    # next we find the minimum and the best threshold
    minError <- min(errors)
    bestSplit <- thresholds[which(errors == minError)]
  
    # if more than 1 threshold has the same accuracy we choose one randomly
    index <- sample(which(sample(bestSplit, 1) == bestSplit), 1)
    bestSplit <- thresholds[index]
    
    #bestThreshold <- sample(bestThreshold, 1)
    labels <- splitLabels[index, ]
    
    # print(cbind(minError, bestThreshold, labels))
    #return probabilities
    prob <- splitProbs[index, ]
    
    labvec <- c(rep(labels[1], length(which(x < bestSplit))),
                rep(labels[2], length(which(x >= bestSplit))))
    
    probvec <- c(rep(prob[1], length(which(x < bestThreshold))),
                 rep(prob[2], length(which(x >= bestThreshold))))
    
    return(list(thres = bestSplit,
                err = minError,
                labels = labels,
                prob = prob,
                labvec = labvec,
                probvec = probvec))
  }
    
    ################################################################################
    # find best features
    ################################################################################
    #bestFeat(X, y, costFnc = "Entropy")
    bestFeat <- function(X, y, costFnc) {
      
      X <- as.matrix(X)
      costFnc <- costFnc
      errors <- rep(NA, ncol(X))
      labels <- matrix(NA, nrow=ncol(X), ncol=2)
      prob <- matrix(NA, nrow=ncol(X), ncol=2)
      split <- rep(NA, ncol(X))
      labvec <- matrix(NA, nrow=length(y), ncol=ncol(X))
      probvec <- matrix(NA, nrow=length(y), ncol=ncol(X))
      
      
      for(i in 1:ncol(X)){
        errors[i] <- findThreshold(X[,i], y, costFnc)$err
        labels[i,] <- findThreshold(X[,i], y, costFnc)$labels
        prob[i,] <- findThreshold(X[,i], y, costFnc)$prob
        split[i] <- findThreshold(X[,i], y, costFnc)$thres
        labvec[,i] <- findThreshold(X[,i], y, costFnc)$labvec
        probvec[,i] <- findThreshold(X[,i], y, costFnc)$probvec
      }
      
      minError <- min(errors)
      featIndex <- sample(which(errors == minError), 1)
      bestLabels <- labels[featIndex, ]
      bestProb <- prob[featIndex, ]
      bestThres <- potthresholds[featIndex]
      bestLabvec <- labvec[,featindex]
      bestProbvec <- probvec[,featindex]
      
      return(list(labels=bestLabels,
                  prob=bestProb,
                  thres=bestThres,
                  featIndex=featindex,
                  labvec=bestLabvec,
                  probvec=bestProbvec))
    }
    
    ################################################################################
    # Recursion Tree
    ################################################################################
    
    recursion <- function(k, X, y, depth=1, minPoints=minPoints, costFnc) {
      
      k <- K
      X <- as.matrix(X)
      y <- y
      l <- length(y)
      depth <- depth
      mp <- minPoints
      costFnc <- costFnc 
      
      # 
      if(depth <= k) { 
        
        #
        if(l >= minPoints) { 
          
          # if only 1 class then return same labels 
          if(length(unique(y)) == 1) { 
            labels <- rep(y[1], l)
            prob <- rep(1,l)
            return(list(labels=labels,
                        prob=prob))
          } 
          
          # for more than 1 class use bestFeat fnc to get reqd values
          else {
            threshold <- bestFeat(X, y, costFnc)$thres
            labels <- bestFeat(X, y, costFnc)$labvec
            prob <- bestFeat(X, y, costFnc)$probvec
            featIndex <- besFeat(X, y, costFnc)$featIndex
            print(featIndex)
            
            # left
            left <- which(X[ ,featIndex] < threshold)
            leftres <- recursion(K=k, X=X[left, ], y=y[left], depth=(depth+1),
                                     minPoints=mp, costFnc)
            
            # terminate recursion if no split point
            if(!is.null(leftres)){
              labels[left] <- leftres$labels
              prob[left]<- leftres$prob
            }
            
            # right
            right <- which(X[,featindex]>=threshold)
            rightres <- recursion(K=k, X=X[right,], y=y[right], depth=(depth+1),
                                      minPoints=mp, costFnc)
            
            # terminate recursion if no split point
            if(!is.null(rightres)){
              labels[right]<- rightres$labels
              prob[right]<- rightres$prob
            }
            
            return(list(labels=labels,prob=prob))
          }
        }
      }
    }
    
    result <- recursion(K=maxdepth, X=x, Y=y, depth=1, minPoints=minPoints, costFnc)
    predLabels <- result$labels
    prob <- result$prob
    
    return(list(predLabels=predLabels, prob=prob))
  }
