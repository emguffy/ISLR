require(ISLR)
require(boot)
# rm(list=ls())


#### 5.3 - Lab: Cross-validation and the bootstrap
### 5.3.1 - The Validation Set Approach
set.seed(1)
train <- sample(392,196)
lm.fit <- lm(data=Auto, formula=mpg~horsepower,subset=train)
mean((Auto$mpg - predict(lm.fit,newdata=Auto))[-train]**2)

lm.fit2 <- lm(data=Auto,mpg~poly(horsepower,2),subset=train)
mean((Auto$mpg - predict(lm.fit2,newdata=Auto))[-train]**2)

lm.fit3 <- lm(data=Auto,mpg~poly(horsepower,3),subset=train)
mean((Auto$mpg - predict(lm.fit3,newdata=Auto))[-train]**2)


set.seed(2)
train <- sample(392,196)
lm.fit <- lm(data=Auto, formula=mpg~horsepower,subset=train)
mean((Auto$mpg - predict(lm.fit,newdata=Auto))[-train]**2)

lm.fit2 <- lm(data=Auto,mpg~poly(horsepower,2),subset=train)
mean((Auto$mpg - predict(lm.fit2,newdata=Auto))[-train]**2)

lm.fit3 <- lm(data=Auto,mpg~poly(horsepower,3),subset=train)
mean((Auto$mpg - predict(lm.fit3,newdata=Auto))[-train]**2)

### 5.3.2 - Leave-One-Out Cross-Validation
glm.fit <- glm(data=Auto,mpg~horsepower)
cv.err <- cv.glm(data=Auto,glm.fit)
cv.err$delta


#initialized a vector so as to avoid copying
cv.error <- rep(0,5)
for(i in 1:5){
  glm.fit <- glm(data=Auto,formula=mpg~poly(horsepower,i))
  cv.error[i] <- cv.glm(Auto,glm.fit)$delta[1]
}
cv.error


### 5.3.3 - k-Fold Cross-Validation
set.seed(17)
cv.error.10 <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

### 5.3.4 - The Bootstrap
## Estimating the Accuracy of a Statistic of Interest 
alpha.fn <- function(data,index) {
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(data=Portfolio,sample(100,100,replace=TRUE))

boot(data=Portfolio,alpha.fn,R=1000)

## Estimating the Accuracy of a Linear Regression Model
boot.fn <- function(data,index){
  return(coef(lm(data=data,formula=mpg~horsepower,subset=index)))
}
boot.fn(data=Auto,1:392)

set.seed(1)
boot.fn(data=Auto,sample(392,392,replace=TRUE))
boot.fn(data=Auto,sample(392,392,replace=TRUE))

boot(data=Auto,statistic=boot.fn,R=1000)
summary(lm(data=Auto,formula=mpg~horsepower))$coef

boot.fn <- function(data,index){
  coefficients(lm(data=Auto,formula=mpg~horsepower+I(horsepower**2),subset=index))
}
set.seed(1)
boot(data=Auto,statistic=boot.fn,R=1000)


#### 5.R Quiz
load("C:\\Users\\emguffy\\Desktop\\5.R.RData")

### 5.R.R1
summary(lm1 <- lm(data=Xy,formula=y~X1+X2))
summary(lm1)$coefficients[2,2]

### 5.R.R2
matplot(Xy[,c(1,3)],type="l")
head(Xy,100)
?matplot

### 5.R.R3
# inData <- Xy
# index <- sample(dim(Xy)[1],100,replace=TRUE)
beta.fn <- function(inData,index){
  lmOut <- lm(data=inData[index,], formula=y~X1+X2)
  coefficients(lmOut)[['X1']]
}
set.seed(1)
(bootResult <- boot(data=Xy,statistic=beta.fn,R=1000))

set.seed(1)
beta <- numeric(1000)
for(i in 1:1000){
  beta[i] <- beta.fn(Xy,sample(dim(Xy)[1],1000,replace=TRUE))
}
mean(beta)
sd(beta)

### 5.R.R4
set.seed(1)
beta.fn <- function(inData,index){
  lmOut <- lm(data=inData[index,], formula=y~X1+X2)
  coefficients(lmOut)[['X1']]
}
tsboot(Xy,statistic=beta.fn,l=100,sim="fixed",R=1000)

## This is my attempt to to do this manually, without the use of
## the boot{} package and the tsboot() funciton.
set.seed(1)
bootCount <- 1000
bootBetaVector <- numeric(bootCount)
blockCount <- 10
for (i in 1:bootCount){
  # i <- 1
  blockOrder <- sample(blockCount,blockCount,replace=T)
  blockIndex <- integer(1000)
  for(j in 1:10){
    # j <- 2
    start <- (100*(j-1))+1
    end <- j*100
    blockIndex[start:end] <- seq(100*(blockOrder[j]-1)+1,length.out=100)
  }
  new.Xy <- Xy[blockIndex,]
  
  lm.fit <- lm(data=new.Xy, formula=y~X1+X2)
  summary(lm.fit)
  bootBetaVector[i] <- coefficients(lm.fit)[['X1']]
}
mean(bootBetaVector)
sd(bootBetaVector)

# Needless to say, it does not return the same answers, and it 
# does not appear to be an issue with the random seed.  Go figure...

