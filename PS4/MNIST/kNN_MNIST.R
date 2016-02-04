################################################################################
## 1. Use the MNIST_train.csv to train the kNN classifier 
## 2. Make predictions for the hold out sample in the MNIST_test.csv
## 3. Save a csv file named MNIST_predictions.csv with predicted labels column
################################################################################

# Upload and clean data
  setwd("~/DATASCIENCE/2_Trimester/15D012ACM/problemSets/PS4/MNIST")

  MNIST_test <- read.csv("MNIST_test.csv", header=FALSE)
  MNIST_train <- read.csv("MNIST_training.csv", header=FALSE)

  cl <- MNIST_train[, 1]
  train <- MNIST_train[, -1] # select all except the first column
  test <- MNIST_test



# Run knn fnc from r
  if (!require("class")) install.packages("class"); library(class)

  predictions <- knn(train=train, test=test, cl=cl, k =1 , prob = TRUE )



# Save predicted labels
  write.csv(predictions, file = "predictions.csv", row.names=FALSE)
