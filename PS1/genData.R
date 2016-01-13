################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
################################################################################
# Course : Advanced Computing
# Title  : Problem Set #1
# Date   : 2016.01.14
################################################################################

library(mvtnorm)
library(ggplot2)

#function to save as csv and pdf
plot.pdf <- function(Df){
  pdf("dataPlot.pdf", width=4, height=4.5)
  plot <- ggplot(data = Df, 
                 aes(x = FeatB, y = FeatA, colour=deny, fill=deny)) +
    geom_point() +
    xlab("FeatB") +
    ylab("FeatA") +
    theme_bw() 
  print(plot)
  dev.off()
}

#create wrapper to enforce correlation in the data
varXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  Cov.matrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2),
                     2, 2, byrow = TRUE)
  return(Cov.matrix)
}

#DGP using mvtnorm
genData <- function(n = 1, seed = NA, muXY = c(0,1), varXY = diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = varXY)
  return(rdraws)
}

#Dataframe creation
myData <- function(Categ1, Categ2, muCateg1, muCateg2, sdCateg1,
                     sdCateg2, rhoCateg1, rhoCateg2, seed=777, save= TRUE) {
  varCateg1 <- varXY(rho=rhoCateg1, sdX=sdCateg1[1], sdY=sdCateg1[2])
  varCateg2 <- varXY(rho=rhoCateg2, sdX=sdCateg2[1], sdY=sdCateg2[2])
  CategOne <- rbind(genData(Categ1/2, muCateg1, varCateg1, seed = seed), 
                            genData(Categ1/2, muCateg1 + c(15,0), varCateg1, seed = seed))
  CategTwo <- genData(Categ2, muCateg2, varCateg2, seed = seed+1)
  myDf <- as.data.frame(rbind(CategOne,CategTwo))
  deny <- c(rep("CategOne", Categ1), rep("CategTwo", Categ2))
  target = c(rep(0, Categ1), rep(1, Categ2))
  myDf <- data.frame(myDf, deny, target)
  colnames(myDf) <- c("FeatA", "FeatB", "deny", "target")
  if (save==TRUE) {
    plot.pdf(myDf)
    write.csv(myDf, file ="dataset.csv")
  }
  return(myDf)
}

#call fnc
myDf <- myData(Categ1=50, Categ2=50, c(8, 150), c(14, 150),
                   c(1,20), c(2,30), -0.1, 0.6, 1221)

############################################################################################

