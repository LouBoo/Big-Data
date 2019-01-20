library(ISLR)

head(Auto)
n <- dim(Auto)[1]
plot(Auto$horsepower, Auto$mpg)

# Validation set approach

set.seed(1)

train <- sample(x=n, size=n/2, replace=FALSE)
train.data <- Auto[train,]
test.data <- Auto[-train,]

lm.fit1 <- lm(mpg~horsepower, data=train.data)
lm.pred1 <- predict(lm.fit1, test.data)
mse1 <- mean((test.data$mpg-lm.pred1)^2)

lm.fit2 <- lm(mpg~poly(horsepower, 2), data=train.data)
lm.pred2 <- predict(lm.fit2, test.data)
mse2 <- mean((test.data$mpg-lm.pred2)^2)

lm.fit3 <- lm(mpg~poly(horsepower, 3), data=train.data)
lm.pred3 <- predict(lm.fit3, test.data)
mse3 <- mean((test.data$mpg-lm.pred3)^2)

lm.fit10 <- lm(mpg~poly(horsepower, 10), data=train.data)
lm.pred10 <- predict(lm.fit10, test.data)
mse10 <- mean((test.data$mpg-lm.pred10)^2)

plot(c(1, 2, 3, 10), c(mse1, mse2, mse3, mse10), pch=5, type="b", ylim=c(15, 30))

lm.fit1.1 <- lm(mpg~horsepower, data=train.data)
lm.pred1.1 <- predict(lm.fit1.1, train.data)
mse1.1 <- mean((train.data$mpg-lm.pred1.1)^2)

lm.fit2.1 <- lm(mpg~poly(horsepower, 2), data=train.data)
lm.pred2.1 <- predict(lm.fit2.1, train.data)
mse2.1 <- mean((train.data$mpg-lm.pred2.1)^2)

lm.fit3.1 <- lm(mpg~poly(horsepower, 3), data=train.data)
lm.pred3.1 <- predict(lm.fit3.1, train.data)
mse3.1 <- mean((train.data$mpg-lm.pred3.1)^2)

lm.fit10.1 <- lm(mpg~poly(horsepower, 10), data=train.data)
lm.pred10.1 <- predict(lm.fit10.1, train.data)
mse10.1 <- mean((train.data$mpg-lm.pred10.1)^2)

plot(c(1, 2, 3, 10), c(mse1.1, mse2.1, mse3.1, mse10.1), pch=19, type="b", col="red")

# Leave-one-out Cross-validation (LOOCV)

# The LOOCV can be automatically computed with glm() and cv.glm()

glm.fit <- glm(mpg~horsepower, data=Auto) # The result is equivalent to the one with lm().
                                          # glm() can be used together with cv.glm(), and this is part of the

library(boot)
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
names(cv.err)                             # K: The value of K used for the K-fold cross validation
                                          # 
                                          #


cv.error <- rep(NA, 5)
for (i in 1:5) {
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

# K-fold Cross-validation

set.seed(17)

cv.error.10 <- rep(NA, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# Exercise 1.
# Cross-validation can also be used to estimate the test error for a classification problem.
# Run a logit model with the Smarket data. The dependent variable is Direction


# Compare this model with the following models using K fold cross-validation with K=10.
# Direction~Lag1+Lag2+Lag3, Direction~Lag1+Lag2+Lag3+Lag4, Direction~Lag1+Lag2+Lag3+Lag4+Lag5



# Exercise 2.
# Consider KNN estimation to predict direction using Lag1 and Lag2. To choose the optimal number of neighbors,
# use the K=10, k fold cross validation. Use only 2004 and 2005 year data.