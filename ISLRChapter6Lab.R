library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# Get rid of na's. 
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

library(leaps)
?regsubsets
regfit.full=regsubsets(data=Hitters, x=Salary~.)
reg.summary=summary(regfit.full)
  
regfit.full=regsubsets(data=Hitters, x=Salary~.,nvmax=19)
reg.summary=summary(regfit.full)

names(reg.summary)
reg.summary$rss

# plot fit statistics
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables", ylab="RSS",type="l")
rss.min <- which.min(reg.summary$rss)
points(rss.min,reg.summary$rss[rss.min],col="red",cex=2,pch=20)

plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted R-squared",type="l")
adjr2.max <- which.max(reg.summary$adjr2)
points(adjr2.max,reg.summary$adjr2[adjr2.max],col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp", type='l')
cp.min <- which.min(reg.summary$cp)
points(cp.min,reg.summary$cp[cp.min],col='red',cex=2,pch=20)

plot(reg.summary$bic,xlab="Number of Variables",ylab="Cp", type='l')
bic.min <- which.min(reg.summary$bic)
points(bic.min,reg.summary$bic[bic.min],col='red',cex=2,pch=20)


plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# Coefficients from the best model (based on BIC)
coef(regfit.full,6)

### Forward and Backward Stepwise Selection
regfit.fwd=regsubsets(x=Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(x=Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)


### Exploring Validation and Cross-validation
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
train
test <- (!train)

regfit.best <- regsubsets(x=Salary~.,data=Hitters[train,],nvmax=19)
str(regfit.best)
summary(regfit.best)
test.mat <- model.matrix(object=Salary~.,data=Hitters[test,])
head(test.mat)

val.errors <- rep(NA,19)
for(i in 1:19){
  coefi <- coef(regfit.best,id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}

val.errors
err.min <- which.min(val.errors)
coef(regfit.best,err.min)

#  Building a predict function for regsubsets
predict.regsubsets <- function(object,newdata,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

extract <- regfit.best$call[[2]]
typeof(extract)

regfit.best <- regsubsets(x=Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
 

k <- 10
set.seed(1)
folds <- sample(1:k,nrow(Hitters),replace=TRUE)

#initializing matrix
cv.errors <- matrix(NA,k,19, dimnames=list(NULL,paste(1:19)))


# loop to perform cross-validation
for(j in 1:k){
  best.fit <- regsubsets(x=Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred <- predict(best.fit, Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

reg.best <- regsubsets(x=Salary~.,data=Hitters,nvmax=19)
coef(reg.best,11)

library(glmnet)
cv.glmnet
grid <- 10^seq(10,-2,length=100)
x <- model.matrix(Salary~.,Hitters)[,-1]
y <- Hitters$Salary
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)
cv.glmnet
cv(ridge.mod)
