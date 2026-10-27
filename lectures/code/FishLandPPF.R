# Production possibilities frontier for fish and grain
library(dplyr)
fishProd <- function(e,s,eFT=5,e1=1,b=10,l1=5,l2=2) {
  y <- sapply(e, function(eVal) {
    ret <- 0
    if(eVal > eFT) {
      if(eVal < eFT + e1) {
        ret <- l1 * (eVal - eFT) * s
      } else {
        ret <- (l2 * (eVal - eFT - e1) + l1 * e1) * s
      }
    }
    return(ret)
  })
  y <- pmin(y, b)
  return(y)
}
grainProd <- function(e,eGT=5,g=5,b=10) {
  y <- sapply(e, function(eVal) {
    ret <- 0
    if(eVal > eGT) {
      ret <- g * (eVal - eGT)
    }
    return(ret)
  })
  y <- pmin(y, b)
  return(y)
}
#
# Draw a production possibilities frontier
s <- 0.5
eTot <- 15
eMin <- 5
eVec <-seq(eMin,eTot-eMin,0.1)
dat <- data.frame(e=eVec,f=fishProd(eVec,s),g=grainProd(eTot-eVec,g=0.45))
plot(dat[,2],dat[,3])
