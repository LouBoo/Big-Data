# Stock Market Data
library(ISLR)
head(Smarket)
str(Smarket)
table(Smarket$Direction) # Show the frequencies with respect to Direction variable.

contrasts(Smarket$Direction)  #Indicates that R has created a dummy variable with a 1 for "Up"

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, data=Smarket)
summary(glm.fit)

glm.probs <- predict(glm.fit, type="response")  # Predict P(Y=1|X)
                                                # default for binomial model is prediction of log-odds.
glm.probs[1:10]

glm.pred <- rep("Down",dim(Smarket)[1])
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred,Smarket$Direction)

mean(glm.pred == Smarket$Direction)  
# Does this imply that the logistic regression is working better than random guessing?
# But this is for the training data.
# To test the performance, we need to use different data than the data that are used to train the model.

train <- subset(Smarket, Smarket$Year < 2005)

Smarket.2005 <- Smarket[Smarket$Year==2005,]
dim(Smarket.2005)

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, data=train)
glm.probs <- predict(glm.fit,Smarket.2005,type="response")

glm.pred <- rep("Down",dim(Smarket.2005)[1])
glm.pred[glm.probs>0.5] = "Up"

table(glm.pred,Smarket.2005$Direction)
mean(glm.pred==Smarket.2005$Direction)  # Worse than random guessing?


# Insurance Data
getwd()
setwd("C:/Users/Min Seong Kim/Dropbox/UConn/BigData")
rm(list=ls())
insurance <- read.csv("insurance.csv", header = TRUE)
str(insurance)
hist(insurance$charges)

insurance$lowcharge <- insurance$charges < 7000
table(insurance$lowcharge)

glm.fit <- glm(lowcharge ~ age + sex + bmi + smoker + region, family=binomial, data=insurance)

summary(glm.fit)

# Randomly sample 1000 observations.
train <- sample(1:dim(insurance)[1], 1000, replace=FALSE) 
train_set <- insurance[train,]
test_set <- insurance[-train,]

glm.fit.train <- glm(lowcharge ~ age + sex + bmi + smoker + region, data=train_set, family=binomial) 
glm.prob <- predict(glm.fit, test_set, type="response")
glm.pred <- rep("low charge",dim(insurance)[1]-1000)
glm.pred[glm.prob<0.5] <- "high charge"
table(glm.pred)

accuracy <- mean(test_set[,"lowcharge"] == (glm.prob>0.5))
accuracy

# Linear Discriminant Analysis
# We fit an LDA model using the lda() function, which is part of the MASS library.
# The syntax for the lda() function is identical to that of lm(), and to that of glm() except for the absence
# of the family option.

library(MASS)
library(ISLR)

str(Smarket)
train <- subset(Smarket, Smarket$Year < 2005)
Smarket.2005 <- Smarket[Smarket$Year==2005,]

lda.fit <- lda(Direction ~Lag1+Lag2, data=train)
lda.fit

plot(lda.fit)   # plots of the linear discriminants by -0.642*Lag1 - 0.514*Lag2

lda.pred <- predict(lda.fit, Smarket.2005)
lda.pred

par(mfrow=c(1,2))
a <- rep("red",dim(Smarket.2005)[1])
a[lda.pred$class=="Up"] <- "blue"
plot(1:length(lda.pred$x),lda.pred$x,col=a)

b <- rep("red",dim(Smarket.2005)[1])
b[Smarket.2005$Direction=="Up"] <- "blue"
plot(1:length(lda.pred$x),lda.pred$x,col=b)

names(lda.pred)
lda.pred$posterior

# You can find prior probabilities (pi_k), group means \mu_k = E(X|Y=k)

lda.class <- lda.pred$class
table(lda.class)
table(lda.class,Smarket.2005$Direction)
mean(lda.class==Smarket.2005$Direction)

# When we have three categories
head(iris)
table(iris$Species)

set.seed(1)
train_index <- sample(1:150, 120, replace=FALSE)
lda.fit1 <- lda(Species ~ ., data=iris[train_index,])
lda.fit1
par(mfrow=c(1,1))
plot(lda.fit1,col=as.numeric(iris$Species[train_index]))   # Linear discriminants are used to separate 
                                                           # the observations among three groups.
# Proportion of trace:
#  LD1   LD2 
# 0.9878 0.0122
# We can see that LD1 is more influential to classfy the observations. We can see this from the plot.

td <- iris[-train_index,]
lda.pred1 <- predict(lda.fit1,td)
lda.pred1

a <- rep(1,dim(td)[1])
a[lda.pred1$class=="versicolor"] <- 15 
a[lda.pred1$class=="virginica"] <- 19 
plot(lda.pred1$x[,1],lda.pred1$x[,2],col=as.numeric(lda.pred1$class),pch=a)  
text(lda.pred1$x[,1]-0.1,lda.pred1$x[,2]-0.1,td$Species)
legend("topright", legend=levels(iris$Species),
       col=c(1,2,3), pch=c(1,15,19),cex=0.8)

table(lda.pred1$class)
table(lda.pred1$class,td$Species)

# multinomial logit model
library(nnet)                                        # Multinomial logit estimation is avaialble in nnet package
ml <- multinom(Species ~ ., data=iris[train_index,])
ml.pred <- predict(ml,iris[-train_index,])

ml.pred
table(ml.pred)
table(ml.pred,td$Species)

# Quadratic Discriminant Analysis: The syntax is the same as lqa
qda.fit <- qda(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=train)
qda.fit
qda.pred <- predict(qda.fit, Smarket.2005)
qda.pred
qda.class <- qda.pred$class
table(qda.class)
table(qda.class,Smarket.2005$Direction)
mean(qda.class==Smarket.2005$Direction)

# knn method for classification
library(class)            # knn() function is a part of the class library.
library(ISLR)
dim(Caravan)              # Includes 85 predictors that measure demographic characteristics for 5822 individuals.
                          # The response variable is "Purchase," which indicates whether or not an individual
                          # purchases a caravan insurance policy.
head(Caravan)


attach(Caravan)
summary(Purchase)

# How to define neighbors when units are different. e.g. salary, age. $1000 is closer than 1 year?
# Normalize each variable to have a zero mean and a variance of 1: scale()

st.X <- scale(Caravan[,-86])
var(Caravan[,1])
mean(Caravan[,1])
var(Caravan[,2])
mean(Caravan[,2])

var(st.X[,1])
mean(st.X[,1])
var(st.X[,2])
mean(st.X[,2])

test <- 1:1000
train.X <- st.X[-test,]
test.X <- st.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
knn.pred <- knn(train.X,test.X,train.Y,k=3)
mean(test.Y != knn.pred)                  # Test error rate: Is this good?
mean(test.Y == "Yes")
table(knn.pred,test.Y)                    # When we predict "purchase", only small percentage actually purchase it.
                                          # 9/74 = 12.2% when k=1, slightly better than 6%.
                                          # IF we just predict no purchase, the error rate will be lower.
                                          # If k=3, 5/25 = 20%. 

# Exercise: Apply the LDA, logit model tp Caravan data and examine accuarcy of their prediction.