
# ----
# Yiqun's square
# ---- 

genSquare <- function(n = 500, 
                      seed = 123, 
                      rangeX = c(0,1), 
                      rangeY = c(0,1), 
                      saveData = FALSE, 
                      savePlot = FALSE){

    if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
    if (!require("Cairo")) install.packages("Cairo"); library(Cairo)

    #perfect Bayes classifier(DGP)
    classify <- function(obs){
        x <- obs[1]
        y <- obs[2]
        
        if((x-1/4)^2+(y-1/4)^2 <= 1/36){
            lab <- 0
        } else if(x + y <=1){
            lab <-1
        } else{
            lab <- 0
        }
        
        return(lab)
    }


    if(!is.na(seed)) set.seed(seed)
    xdraw <- runif(n, min = rangeX[1], max = rangeX[2])
    ydraw <- runif(n, min = rangeY[1], max = rangeY[2])
    finalData <- data.frame(x = xdraw, y = ydraw)
    finalData$class <- factor(apply(finalData, 1, classify))
    names(finalData) <- c("x1", "x2", "y")
    
    if(saveData){
        write.csv(finalData, file = "dataset.csv", row.names=FALSE)
    }
  
    if(savePlot){
        plot <- 
            ggplot(finalData, aes(x = x1, y = x2, col = y)) +
            geom_point() +
            coord_fixed(ratio = 1) +
            theme_bw()
        cairo_pdf("dataPlot.pdf")
        print(plot)
        dev.off()
    }
  
  return(finalData)
}

# ----
# Call Yiqun's square
# ---- 

dataset <- genSquare (n = 2500, 
                      seed = 123, 
                      rangeX = c(0,1), 
                      rangeY = c(0,1), 
                      saveData = FALSE, 
                      savePlot = FALSE)
