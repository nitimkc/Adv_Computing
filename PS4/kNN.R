
################################################################################
## Create kNN function
 # OUTPUT: vector of predicted labels, vector of prob for predicted label
################################################################################
#features=dataset[,1:2]
#labels=dataset[,3]
#k <- 7
#p <- 2
#type <- "train"

kNN <- function(features, labels, memory = NULL,
                k, p, type="train") {
  
  #load reqd libs
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
  if (!require("plyr")) install.packages("plyr"); library(plyr)
  
  #----------------------------------------------------------------------------# 
  
  # define objects
  n <- nrow(features)
  m <- ncol(features)
  if (n <= k) stop("k cannot be more than n-1")
  
  #----------------------------------------------------------------------------# 
  
  # test the inputs
  not_empty(features); not_empty(labels);
  if (type == "train") {
    assert_that(n == length(labels))
  }
  is.string(type); assert_that(type %in% c("train", "predict"))
  is.count(k);
  assert_that(p %in% c(1, 2, Inf))
  if (type == "predict") {
    assert_that(not_empty(memory) &
                  ncol(memory) == m &
                  nrow(memory) == length(labels))
  }
  
  #----------------------------------------------------------------------------#  
  
  # Compute the distance between each point and all others
  if (type == "train") {
    distMatrix <- matrix(NA, n, n)
    for (i in 1:n) {
      
      # for "train"
      #-------------#
      # getting the probe for the current observation 
      probe <- as.numeric(features[i, ])
      probeExpanded <- matrix(probe, nrow = n, ncol = m,
                              byrow = TRUE)
      
      # computing distances between the probe and exemplars in the training X
      if (p %in% c(1,2)) {
        distMatrix[i, ] <- (rowSums((abs(features -
                                             probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[i, ] <- apply(abs(X - probeExpanded), 1, max)
      }
    }
  } else if (type == "predict") {
    noMemory <rix(NA, n, noMemory)
    for (i in 1:n) {
      
      # for "predict"
      #-------------#
      # getting the probe for the current observation
      probe <- as.numeric(features[i,])
      probeExpanded <- matrix(probe, nrow = noMemory, ncol = m,
                              byrow = TRUE)
      
      # computing distances between the probe and exemplars in the memory
      if (p %in% c(1,2)) {
        distMatrix[i, ] <- (rowSums((abs(memory -
                                             probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[i, ] <- apply(abs(memory - probeExpanded), 1, max)
      }
    }
  }
  
  #----------------------------------------------------------------------------#  
  
  # Sort the distances in increasing numerical order and pick the first k elements
  neighbors <- apply(distMatrix, 1, order)
  
  # pick first k rows from neighbors, get the corresponding label and its freq
  # predict label based on the highest freq
  prob <- rep(NA, n)
  predLabels <- rep(NA, n)
  Mode <- function(x) {
    ux <- unique(as.numeric(x))
    ux[which.max(tabulate(match(x, ux)))]
  }
  for (i in 1:n) {
    x <- as.vector(labels[neighbors[1:k, i]])
    prob[i] <- max(plyr::count(x)$freq)/k
    predLabels[i] <- Mode(x)
  }
  
  #----------------------------------------------------------------------------# 
  
  # examine the performance, available only if training
  if (type == "train") {
    errorCount <- table(predLabels, labels)
    accuracy <- mean(predLabels == labels)
  } else if (type == "predict") {
    errorCount <- NA
    accuracy <- NA
  }
  
  #----------------------------------------------------------------------------# 
  
  # return the results
  return(list(predLabels=predLabels, 
              prob=prob,
              accuracy = accuracy,
              errorCount = errorCount))
}
