require(ISLR)
names(Smarket)
dim(Smarket)
head(Smarket)
cor(Smarket[,-9])

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
dim(summary(glm.fit)$coef)

summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit,type="response")
head(glm.probs)
contrasts(Smarket$Direction)
glm.pred=rep("Down",1250)
head(glm.pred)
glm.pred[glm.probs>.5]<- "Up"
head(glm.pred,100)
mean(glm.pred==Smarket$Direction)
