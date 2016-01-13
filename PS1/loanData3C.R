## create small wrapper functions
sigmaXY <- function(rho, sdX, sdY) { 
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2),
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

# creating a function for all of this
loanData <- function(noApproved, noDenied, noUndecided, muApproved, muDenied, muUndecided, sdApproved,
                     sdDenied, sdUndecided, rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+2)
  loanDf <- as.data.frame(rbind(approved,denied, undecided))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided))
  target1 = c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided))
  target2 = c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided))
  target3 = c(rep(0, noApproved), rep(0, noDenied), rep(1, noUndecided))
  loanDf <- data.frame(loanDf, deny, target1, target2, target3)
  colnames(loanDf) <- c("PIratio", "solvency", "Deny", "Target1", "Target2", "Target3")
  return(loanDf)
}

# generating some data
loanDf <- loanData(noApproved=50, noDenied=50, noUndecided=25, c(4, 150), c(10, 100), c(10, 100),
                   c(1,20), c(2,30), c(2,30), -0.1, 0.6, 0.4, 1221)
head(loanDf)

# illustrating the data, note that with ggplot we need to additionally
# specify font family
library(ggplot2)
ggplot(data = loanDf,
       aes(x = solvency, y = PIratio, colour=Deny, fill=Deny)) +
  geom_point() +
  xlab("solvency") +
  ylab("PIratio") +
  theme_bw() +
  theme(text=element_text(family="Arial"))

#now run 3 regressions to obtain boundary lines
reg1 <- lm(Target1 ~ solvency + PIratio + 1, data=loanDf)
reg2 <- lm(Target2 ~ solvency + PIratio + 1, data=loanDf)
reg3 <- lm(Target3 ~ solvency + PIratio + 1, data=loanDf)

#grab coeff
coeff1 <- coef(reg1)[c("solvency", "PIratio")]
coeff2 <- coef(reg2)[c("solvency", "PIratio")]
coeff3 <- coef(reg3)[c("solvency", "PIratio")]
bias1 <- coef(reg1)[1]
bias2 <- coef(reg2)[1]
bias3 <- coef(reg3)[1]

#decision boundary between App and Deny
x <- seq(min(loanDf["solvency"]), max(loanDf["solvency"]), 
         length.out = nrow(loanDf))

y1.2 <- ((coeff2[1] - coeff1[1])/(coeff1[2] - coeff2[2]))* x + ((bias2 - bias1)/(coeff1[2] - coeff2[2]))
  
BDf1.2 <- data.frame(solvency=x, PIratio=y1.2,
                          Deny=rep("Boundary App/Deny", length(x)))

#decision boundary between App and Undecided
y1.3 <- ((coeff3[1] - coeff1[1])/(coeff1[2] - coeff3[2]))* x + ((bias3 - bias1)/(coeff1[2] - coeff3[2]))

BDf1.3 <- data.frame(solvency=x, PIratio=y1.3,
                   Deny=rep("Boundary App/Undcd", length(x)))

#decision boundary between App and Undecided
y2.3 <- ((coeff3[1] - coeff2[1])/(coeff2[2] - coeff3[2]))* x + ((bias3 - bias2)/(coeff2[2] - coeff3[2]))

BDf2.3 <- data.frame(solvency=x, PIratio=y2.3,
                     Deny=rep("Boundary Deny/Undcd", length(x)))

# plotting
ggplot(data = loanDf, aes(x = solvency, y = PIratio,
                          colour=Deny, fill=Deny)) +
  geom_point() +
  xlab("solvency") +
  ylab("PI ratio") +
  theme_bw(base_size = 14, base_family = "Helvetica") +
  geom_line(data=BDf1.2) +
  geom_line(data=BDf1.3) +
  geom_line(data=BDf2.3) 
  #scale_color_manual("deny",
                     values = c("Boundary App/Deny" = "black", "Boundary App/Undcd" = "grey", "Boundary Deny/Undcd" = "magenta"
                                "Target1" = "blue", "Target2" = "red", "Target3" = "orange"))

