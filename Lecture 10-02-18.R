# Sotck market data
install.packages("ISLR")
library(ISLR)
head(Smarket)
str(Smarket)
table(Smarket$Direction)
contrasts(Smarket$Direction)
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, data=Smarket)
summary(glm.fit)

glm.probs <- predict(glm.fit, type="response")

glm.probs[1:10]

glm.pred <- rep("Down", dim(Smarket)[1])
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred)
table(glm.pred, Smarket$Direction)

mean(glm.pred == Smarket$Direction)

# Does this imply that the logistic regression is working better than random guessing?
# But this is fro the training data
# To test the performance, we need to use different data than the data which was used to train the model

train <- subset(Smarket, Smarket$Year < 2005)

Smarket.2005 <- Smarket[Smarket$Year == 2005,]
dim(Smarket.2005)
dim(train)
  
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, data=train)
glm.probs <- predict(glm.fit, Smarket.2005, type="response")

glm.pred <- rep("Down", dim(Smarket.2005)[1])
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred)
table(glm.pred, Smarket.2005$Direction)

mean(glm.pred == Smarket.2005$Direction)


# Insurance data: The data is on HuskyCT
# create a categorical variable lowcharge which equals 1 if insurance$charges < 7000 and equals 0 otherwise
# run the logit regression of this on age, sex, bmi, smoker, region
# split the data by choosing 1000 observations for training and by using the other observations for testing
# assess the accuracy of this model

setwd("D:/Documents (Louis Booth)/R/Big Data")
insurance = read.csv("insurance.csv", header=TRUE)
set.seed(80085)
insurance$lowcharge <- rep(0, dim(insurance)[1])
insurance$lowcharge[insurance$charges<7000] <- 1
#### insurance$lowcharge <- insurance$charges < 7000

rand <- sample(nrow(insurance), size=1000, replace=F)
#### rand <- sample(1:dim(insurance)[1], size=1000, replace=F)
train <- insurance[rand,]
test <- insurance[-rand,]

glm.fit <- glm(lowcharge~age+sex+bmi+smoker+region, family=binomial, data=train)
glm.probs <- predict(glm.fit, test, type="response")

glm.pred <- rep(0, dim(test)[1])
glm.pred[glm.probs>0.5] <- 1
table(glm.pred)
table(glm.pred, test$lowcharge)

### glm.prob <- predict(glm.fit, test, type="response")
### glm.pred <- rep("low charge", dim(insurance)[1]-1000)
### glm.pred[glm.prob<0.5] <- "high charge"
### table(glm.pred)
### accuracy <- mean(test[, "low charge"] == (glm.prob<0.5))
### accuracy

mean(glm.pred == test$lowcharge)