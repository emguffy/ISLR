require(ISLR)
require(boot)
# rm(list=ls())

load("C:\\Users\\emguffy\\Desktop\\5.R.RData")

lm1 <- lm(data=Xy,formula=y~X1+X2)
summary(lm1)
lm1$coef[2]

# inData <- Xy
# index <- sample(dim(Xy)[1],100,replace=TRUE)
beta.fn <- function(inData,index){
  lmOut <- lm(data=inData[index,], formula=y~X1+X2)
  coefficients(lmOut)[['X1']]
}

beta <- numeric(1000)
for(i in 1:1000){
  beta[i] <- beta.fn(Xy,sample(dim(Xy)[1],100,replace=TRUE))
}
mean(beta)
sd(beta)/sqrt(length(beta))


set.seed(1)
boot(data=Xy,beta.fn,R=1000)
