rm(list-ls())
library(ISLR)
library(boot)

# Exercise 1.
# Cross-validation can also be used to estimate the test error for a classification problem.
# Run a logit model with the Smarket data. The dependent variable is Direction
# glm.fit <- glm(Direction~Lag1+Lag2, family=binomial, data=Smarket)
# summary(glm.fit)
set.seed(17)

cv.error.1 <- rep(NA, 10)
glm.fit.1 <- glm(Direction~Lag1+Lag2, family=binomial, data=Smarket)
cv.error.1 <- cv.glm(Smarket, glm.fit.1, K=10)$delta[1]
cv.error.1

cv.error.2 <- rep(NA, 10)
glm.fit.2 <- glm(Direction~Lag1+Lag2+Lag3, family=binomial, data=Smarket)
cv.error.2 <- cv.glm(Smarket, glm.fit.2, K=10)$delta[1]
cv.error.2

cv.error.3 <- rep(NA, 10)
glm.fit.3 <- glm(Direction~Lag1+Lag2+Lag3+Lag4, family=binomial, data=Smarket)
cv.error.3 <- cv.glm(Smarket, glm.fit.3, K=10)$delta[1]
cv.error.3

cv.error.4 <- rep(NA, 10)
glm.fit.4 <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, family=binomial, data=Smarket)
cv.error.4 <- cv.glm(Smarket, glm.fit.4, K=10)$delta[1]
cv.error.4

# Compare this model with the following models using K fold cross-validation with K=10.
# Direction~Lag1+Lag2+Lag3, Direction~Lag1+Lag2+Lag3+Lag4, Direction~Lag1+Lag2+Lag3+Lag4+Lag5



# Exercise 2.
# Consider KNN estimation to predict direction using Lag1 and Lag2. To choose the optimal number of neighbors,
# use the K=10, k fold cross validation. Use only 2004 and 2005 year data.

library(class)
set.seed(1)

n <- nrow(Smarket)
ind <- 1:n


for (i in 1:10) {
  testSplit <- sample(ind, size=n/10, replace=FALSE)
  train <- Smarket[-testSplit, ]
  test <- Smarket[testSplit, ]
  train.x <- train[, c("Lag1", "Lag2")]
  test.x <- test[, c("Lag1", "Lag2")]
  train.y <- train$Direction
  test.y <- test$Direction
  
  ind <- ind[-testSplit]
  
  Acc <- rep(NA, 100)
  
  for (j in 1:100) {
    knnPred <- knn(train.x, test.x, train.y, k=j)
    Acc[j] <- mean(test.y == knnPred)
  }
  optKs <- which.max(Acc)
}

optKs
BestK <- mean(optKs)

df <- Smarket
# install.packages("caret")
# install.packages("e1071")
library(caret)
trControl = trainControl(method="cv", number=10)
fit <- train(Direction~Lag1+Lag2, method="knn", tuneGrid=expand.grid(k=1:50), trControl=trControl, metric="Accuracy", data=Smarket)
fit
