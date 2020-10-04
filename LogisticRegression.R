##############################################################
##########Logistic Regression sample code
#############################################################

################################### Load data
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)

################################### Data exploration
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

################################### Fitting logistic regression
glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)

#summarize
summary(glm.fits)
#coefficient
coef(glm.fits)

################################### Prediction
glm.probs <-predict(glm.fits, type="response")
glm.probs
#convert to 1 and 0
glm.pred <- rep("Down",1250)
glm.pred[glm.probs>0.5]<- "Up"
#compare prediction and actual
table(glm.pred, Direction)  

################################### Manual Cross Validation
train = (Year < 2005)
Smarket.2005 <- Smarket[!train,]
Direction.2005 <- Direction[!train]

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs <-predict(glm.fits, Smarket.2005, type="response")
  
glm.pred <- rep("Down",252)
glm.pred[glm.probs>0.5]<- "Up"

table(glm.pred, Direction.2005)

#Test error rate
mean(glm.pred!=Direction.2005)

################################### Running logistc regressioni without GLM

# Define the log-likelihood function 
# ln L = sum_i [y_i log(p(x_i,beta0,beta1)) + (1-y_i) log(1-p(x_i,beta0,beta1)) ]
loglike <- function(beta0,beta1,x,y) {
  # Define the logistic function 
  logit <- function(x,beta0,beta1) {
    1/(1 + exp(-beta0-beta1*x))
  }
  
  p <- logit(x,beta0,beta1)
  sum( y*log(p) + (1-y)*log(1-p) )
}


# Minimise using Gradient Descient, stochastic gradient descent, ....
