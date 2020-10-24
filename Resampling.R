##############################################################
########## Resampling sample code
#############################################################

################################### The Validation Set Approach
library(ISLR)
set.seed(1)
train=sample(392,196) # generate 1:392, take a sample of size 196

lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
summary(lm.fit)

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2) # Calculate MSE
# MSE==28.67821
# now try quadratic and cubic regressions

lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset=train)
summary(lm.fit2)

mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# MSE == 19.82

lm.fit3 <- lm(mpg~poly(horsepower,3),data=Auto, subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# MSE == 19.78


################################### Leave-One-Out Cross-Validation

glm.fit=glm(mpg~horsepower, data=Auto) # this is essentially the same with linear

coef(glm.fit)

# cv.glm is in boot library
library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto,glm.fit)
summary(cv.err)

# if we want to compare multiple specifications

cv.err=rep(0,5)
for (i in 1:5) {
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.err[i] <- cv.glm(Auto, glm.fit)$delta[1]
}


################################### k-Fold Cross-Validation

set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]<-cv.glm(Auto,glm.fit, K=10)$delta[1]
}
cv.error.10

################################### the Bootstrap

############# Estimating a sample statistic
alpha.fn = function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(X)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)

alpha.fn(Portfolio, sample(100,100,replace=T))

boot(Portfolio,alpha.fn, R=1000)

############## Estimating the accuracy of linear regression model
boot.fn <- function(data,index){return(coef(lm(mpg~horsepower), data=data, subset=index))}

boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto, sample(392,392,replace=T))

boot(Auto,boot.fn,1000)
