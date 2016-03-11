#######################################################################################################
## 1. Use SPAM dataset to (i)   generate training and test data
#                         (ii)  train adaBoost.R and evaluate test performance
#                         (iii) compare performance with gbm package
## 2. Plot the evolution of training and test errors for adaBoost.R and gbm for diff no. of iterations
#######################################################################################################

setwd("~/DATASCIENCE/2_Trimester/15D012ACM/problemSets/PS6")
data <- read.csv("~/DATASCIENCE/2_Trimester/15D012ACM/problemSets/PS5/spambase.data", header=FALSE)
formula <- V58~.
depth = 10 
noTrees = 50

#########################
##  load reqd libraries
#########################

if (!require("caret")) install.packages("caret"); library(caret)
if (!require("gbm")) install.packages("gbm"); library(gbm)
if (!require("ggplot2")) install.packages("ggplot"); library(ggplot2)
if (!require("reshape2")) install.packages("reshape2"); library(reshape2)


#######################
##  partition data
#######################

#  define rows to partition 
part <- createDataPartition(data$V58, p=0.80, list=FALSE) 

#  generate train and test sets
data_train <- data[part, ]
data_test <- data[-part, ]


######################################
##  train and test using own adaBoost
######################################

##  train with the training set
source("adaBoost.R")
result <- adaBoost(formula = formula, data = data_train, depth = 10, noTrees = 50)
ada.trainerror <- result$error

##  evaluate performance on test set

adaBoost.test <- function(result, testData, testLabels) {
  
  #  set classes to {1,-1} 
  y <- ifelse( testLabels==1, 1, -1) 
  
  
  #  initialize
  noObs <- length(y)
  n <- length(result$datamatrix) 
  alpha <- result$alpha
  
  #  to store prediction labels and error
  ada.predLabels <- matrix(NA, nrow = nrow(testData), ncol = n)
  ada.testerror <- rep(NA, n)
  

  for(i in 1:n) {
    
    #  predict labels and store
    ada.predLabels[ ,i] <- predict(result$datamatrix[[i]], testData, type="class")
    ada.predLabels[ ,i] <- ifelse(ada.predLabels[ ,i]==2, 1, -1)
    
    #  output the sign of a combination of all classifier outputs
    ada.finalPred <- sign(rowSums(t(alpha[1:i] * t(ada.predLabels[ ,1:i]))))
    
    #  final prediction error 
    ada.testerror[i] <- 1 - sum(ada.finalPred == y)/noObs #}
  }
  
  return(list(error=ada.testerror))
}


#  call function to retrieve test error
ada.test <- adaBoost.test(result = result, testData = data_test[ ,-58], testLabels = data_test[ ,58])
ada.testerror <- ada.test$error


######################################
##  Compare error with GBM package
######################################

#  set classes in datasets to {0, 1} as reqd by gbm 
gbm.data_train <- data_train
gbm.data_test <- data_test

gbm.data_train$V58 <- ifelse( gbm.data_train$V58==1, 1, 0) 
gbm.data_test$V58 <- ifelse( gbm.data_test$V58==1, 1, 0)

#  call gbm adaboost
gbm <- gbm(formula = formula, 
           distribution = "adaboost",
           data = gbm.data_test,
           n.trees = noTrees,
           interaction.depth = depth, 
           shrinkage = 1, bag.fraction = 1)

#  initialize error vectors
gbm.trainerror <- rep(NA, noTrees)
gbm.testerror <- rep(NA, noTrees)

for (i in 1:noTrees) {
  
gbm.trainerror[i] <- mean((predict(gbm, data_train, n.trees = i) > 0) != gbm.data_train$V58)
gbm.testerror[i] <- mean((predict(gbm, data_test, n.trees = i) > 0) != gbm.data_test$V58)

}

#####################
##  Compare Results
#####################

#  Save all errors into dataframe

errorDf <- cbind(ada.trainerror, ada.testerror, gbm.trainerror, gbm.testerror)
rownames(errorDf) <- 1:50

#  Reshape dataframe 
row <- rownames(errorDf)
new.errorDf = melt(errorDf, id='row')
colnames(new.errorDf) <- c("noTrees", "Error Type", "Error Rate")

#  Plot

pdf("Compare adaBoost.pdf", width=8, height=4.5)
ggplot(data = new.errorDf, aes(x ="noTrees", y ="Error Rate",colors="Error Type")) +
  geom_point() +
  xlab("noTrees") + 
  ylab("Misclassification Error") +
  theme_bw() 
dev.off()
