ValidationRsquared <- function(validObs, validHat){
 #
 resids <- validHat - validObs
 yBar <- mean(validObs)
 offset <- validObs - yBar
 num <- sum(resids^2)
 denom <- sum(offset^2)
 Rsq <- 1 - num/denom
 return(Rsq)
 }