rm(list=ls())

library(MASS)
library(ISLR)

str(Smarket)
train <- subset(Smarket, Smarket$Year < 2005)
Smarket.2005 <- Smarket[Smarket$Year == 2005,]

lda.fit <- lda(Direction ~ Lag1 + Lag2, data=train)
lda.fit

plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
lda.pred

par(mfrow=c(1,2))
a <- rep("red", dim(Smarket.2005)[1])
a[lda.pred$class == "Up"] <- "blue"
plot(1:length(lda.pred$x), lda.pred$x, col=a)

b <- rep("red", dim(Smarket.2005)[1])
b[Smarket.2005$Direction == "Up"] <- "blue"
plot(1:length(lda.pred$x), lda.pred$x, col=b)

names(lda.pred)
lda.pred$posterior

# You can find prior probabilities (pi_k), group means mu_k = E(X|Y=k)

lda.class <- lda.pred$class
table(lda.class)
table(lda.class, Smarket.2005$Direction)
mean(lda.class==Smarket.2005$Direction)

# When we have three categories
head(iris)
table(iris$Species)

set.seed(1)
train_index <- sample(1:150, 120, replace=FALSE)
lda.fit1 <- lda(Species~., data=iris[train_index,])
lda.fit1
par(mfrow=c(1,1))
plot(lda.fit1, col=as.numeric(iris$Species[train_index])) # Linear discriminants are used to separate
                                                          # the observations among three categories
# Proportion of trace:
# LD1    Ld2
# 0.9878 0.0122
# we can see that LD1 is more influential to classify the observations. We can see this from the plot.

td <- iris[-train_index,]
lda.pred1 <- predict(lda.fit1, td)
lda.pred1

a <- rep(1, dim(td)[1])
a[lda.pred1$class=="versicolor"] <- 15
a[lda.pred1$class=="virginica"] <- 19
plot(lda.pred1$x[,1], lda.pred1$x[,2], col=as.numeric(lda.pred1$class), pch=a)
text(lda.pred1$x[,1]-0.1, lda.pred1$x[,2]-0.1, td$Species)
legend("topright", legend=levels(iris$Species), col=c(1,2,3), pch=c(1,15,19), cex=0.8)

table(lda.pred1$class)
table(lda.pred1$class, td$Species)

# Multinomial logit model
library(nnet)
ml <- multinom(Species~., data=iris[train_index,])
ml.pred <- predict(ml, iris[-train_index,])

ml.pred
table(ml.pred)
table(ml.pred, td$Species)

# Quadratic discriminant analysis: syntax is the same as lda
qda.fit <- qda(Direction~ Lag1 + Lag2, data=train)
qda.fit
qda.pred <- predict(qda.fit, Smarket.2005)
qda.pred
qda.class <- qda.pred$class
table(qda.class)
table(qda.class, Smarket.2005$Direction)
mean(qda.class==Smarket.2005$Direction)

# knn method for classification
library(class)    # knn() function is a part of the class library
library(ISLR)
dim(Caravan)
head(Caravan)
attach(Caravan)
summary(Purchase)

# How to define neighbors when units are different e.g. salary, age.  $1000 is closer than 1 year?
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
knn.pred <- knn(train.X, test.X, train.Y, k=3)
mean(test.Y != knn.pred)
mean(test.Y == "Yes")
table(knn.pred, test.Y)







install.packages("MASS")
library(MASS)
library(ISLR)
attach(Caravan)
test <- Caravan[1:1000,]
train <- Caravan[1000:5822,]

# LDA
lda.fit2 <- lda(Purchase ~ ., data=train)
# lda.fit2
plot(lda.fit2)
lda.pred2 <- predict(lda.fit2, test)
# lda.pred2
lda.class2 <- lda.pred2$class
table(lda.class2)
table(lda.class2, test$Purchase)
mean(lda.class2==test$Purchase)

# #QDA
# qda.fit2 <- qda(Purchase ~ ., data=train)
# qda.fit2
# qda.pred2 <- predict(qda.fit2, test)
# qda.pred2
# qda.class2 <- qda.pred2$class
# table(qda.class2)
# table(qda.class2, test$Purchase)
# mean(qda.class==test$Purchase)


glm.fit <- glm(Purchase ~ ., family=binomial, data=train)
# summary(glm.fit)
glm.probs <- predict(glm.fit, test, type="response")
glm.pred <- rep("No", dim(test)[1])
glm.pred[glm.probs>0.5] <- "Yes"
table(glm.pred)
table(glm.pred, test$Purchase)
mean(glm.pred == test$Purchase)