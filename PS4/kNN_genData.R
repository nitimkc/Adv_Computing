################################################################################
## 1. Test your kNN function with the dataset
## 2. Save predictions with predLabels and prob from kNN fnc and 
## 3. Plot data with decision boundaries
################################################################################

source("Yiqun_genData.R")
source("kNN.R")


myPred <- kNN(features=dataset[,1:2], labels=dataset[,3], memory = NULL,
              k=7, p=2, type="train")



kNN_genData <- function(dataset, 
                        myPred, 
                        saveData = TRUE, 
                        savePlot = TRUE) {
  
  #load reqd library
  if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
  if (!require("akima")) install.packages("akima"); library(akima)
  
  #----------------------------------------------------------------------------# 
  
  # combine dataset and save
  finalData <- cbind(dataset, myPred$predLabels, myPred$prob)
  colnames(finalData) <- c("x1", "x2", "y", "predLabels","prob")
  
  if(saveData){
    write.csv(finalData, file = "predictions.csv", row.names=FALSE)
  }
 
  #----------------------------------------------------------------------------# 
  
  #produce the grid to get the stat_contour
  gd <- interp(as.data.frame(finalData)$x1, as.data.frame(finalData)$x2, 
                 as.data.frame(finalData)$predLabels)
  gd2 <- expand.grid(x=gd$x, y=gd$y)
  gd2$z <- as.vector(gd$z)
  gd2$z[gd2$z > 0.5] <- 1
  
  
  #save the pdf plot
  plot.pdf <- function(finalData){
    pdf("plot.pdf", width=4.5, height=3.5)
    plot <- ggplot(data = finalData, 
                aes(x = x1, y = x2, colour=y, z = y)) + 
      scale_color_manual(values = c("paleturquoise1", "paleturquoise3")) + 
      geom_point() +
      ggtitle(expression(underline("Yiqun's Square"))) +
      xlab(expression(italic("x")[1])) +
      ylab(expression(italic("x")[2])) +
      labs(list(family='serif', cex.lab=1.0)) +
      theme_bw() +
      stat_contour(data=na.omit(gd2), binwidth=1, 
                   colour="midnightblue", aes(x=x, y=y, z=z))
    print(plot)
    dev.off()
  }
  plot.pdf(finalData)
}

################################################################################
## 1. Call function 
## 2. Check execution time for kNN function
################################################################################

kNN_genData(dataset, myPred, saveData = TRUE, savePlot = TRUE)


start.time <- Sys.time()
kNN(features=dataset[,1:2], 
    labels=dataset[,3], memory = NULL,
    k=7, p=2, type="train")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken