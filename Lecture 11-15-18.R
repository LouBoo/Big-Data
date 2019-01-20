# Subset Selection
setwd("D:/Documents (Louis Booth)/R/Big Data")
rm(list=ls())

library(ISLR)
head(Hitters)
dim(Hitters)

sum(is.na(Hitters))

Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# Best subset selection

# install.packages("leaps")
library(leaps)
regfit.full <- regsubsets(Salary~., data=Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary.full <- summary(regfit.full)
names(reg.summary.full)
reg.summary.full$rsq
reg.summary.full$adjr2

par(mfrow=c(2,2))
plot(reg.summary.full$rsq, xlab="Number of regressors", ylab="R-square", type="l")

plot(reg.summary.full$adjr2, xlab="Number of regressors", ylab="Adjusted R-square", type="l")
a <- which.max(reg.summary.full$adjr2)
points(a, reg.summary.full$adjr2[a], col="red", cex=2, pch=20)

plot(reg.summary.full$cp, xlab="Number of regressors", ylab="Cp", type="l")
a1 <- which.min(reg.summary.full$cp)
points(a1, reg.summary.full$cp[a1], col="red", cex=2, pch=20)

plot(reg.summary.full$bic, xlab="Number of regressors", ylab="BIC", type="l")
a2 <- which.min(reg.summary.full$bic)
points(a2, reg.summary.full$bic[a2], col="red", cex=2, pch=20)

plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

coef(regfit.full, 6)

# Forward and Backward Stepwise selection
regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
reg.summary.fwd <- summary(regfit.fwd)
reg.summary.fwd

regfit.bwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
reg.summary.bwd <- summary(regfit.bwd)
reg.summary.bwd

plot(reg.summary.full$bic, xlab="Number of regressors", ylab="BIC", type="l", main="Best Subset Selection")
a2 <- which.min(reg.summary.full$bic)
points(a2, reg.fit.full$bic[a2], col="red", cex=2, pch=20)

plot(reg.summary.fwd$bic, xlab="Number of regressors", ylab="BIC", type="l", main="Forward Stepwise Selection")
a2 <- which.min(reg.summary.fwd$bic)
points(a2, reg.fit.fwd$bic[a2], col="red", cex=2, pch=20)

plot(reg.summary.bwd$bic, xlab="Number of regressors", ylab="BIC", type="l", main="Backward Stepwise Selection")
a2 <- which.min(reg.summary.bwd$bic)
points(a2, reg.fit.bwd$bic[a2], col="red", cex=2, pch=20)

plot(regfit.full, scale="bic", main="Best Subset Selection")
plot(regfit.fwd, scale="bic", main="Forward Stepwise Selection")
plot(regfit.bwd, scale="bic", main="Backward Stepwise Selection")

# Shrinkage Method: Ridge and Lasso
# Objective function is SSR + penalty term

# Ridge Regression

# "glmnet" package needs to be installed for ridge regression and the lasso
install.packages("glmnet")
library(glmnet)
rm(list=ls())
Hitters <- na.omit(Hitters)
x.temp <- model.matrix(Salary~., Hitters)

head(x.temp)
x <- x.temp[,-1]
y <- Hitters$Salary

grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod, s=50, type="coefficients")[1:20,]

# Cross validation to choose lambda
set.seed(1)
train <- sample(1:nrow(x), round(nrow(x)/2))
y.train <- y[train]
x.train <- x[train,]
y.test <- y[-train]
x.test <- x[-train,]

ridge.mod <- glmnet(x.train, y.train, alpha=0, lambda=grid, thresh=1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx=x.test)
mean((ridge.pred - y.test)^2)

# we can see whether ridge regression with lambda=4 performs better than the LS

ridge.pred <- predict(ridge.mod, s=0, newx=x.test, exact=TRUE, x=x.train, y=y.train)
mean((ridge.pred - y.test)^2)

train.data <- data.frame(y.train, x.train)
linear.fit <- lm(y.train~., data=train.data)
newx <- data.frame(x.test)
linear.pred <- predict(object=linear.fit, newdata=newx)
mean((linear.pred - y.test)^2)

set.seed(1)
cv.out <- cv.glmnet(x.train, y.train, alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s=bestlam, newx=x.test)
mean((ridge.pred - y.test)^2)

# Finally we refit our ridge regression model on the full data set using lambda chosen by cross validation
out <- glmnet(x, y, alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20,]

# Lasso
lasso.mod <- glmnet(x.train, y.train, alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x.train, y.train, alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x.test)
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:20,]
lasso.coef

### Dimensional Reduction Methods

library(ISLR)
head(Hitters)
dim(Hitters)

sum(is.na(Hitters))

Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

set.seed(1)
train <- sample(1:nrow(Hitters), round(nrow(Hitters)/2))
train.set <- Hitters[train,]
test.set <- Hitters[-train,]
x.test <- test.set[,-19]
y.test <- test.set[,19]

# Principal Component Regression
install.packages("pls")
library(pls)
pcr.fit <- pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")

summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

pcr.fit <- pcr(Salary~., data=train.set, scale=TRUE, ncomp=6)
pcr.pred <- predict(pcr.fit, x.test, ncomp=6)

mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(Salary~., data=Hitters, scale=TRUE)
summary(pcr.fit)

# Partial Least Squares (supervised method) : use plsr() function from pls library
set.seed(10)
pls.fit <- plsr(Salary~., data=Hitters, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

pls.fit <- plsr(Salary~., data=train.set, scale=TRUE, ncomp=12)
pls.pred <- predict(pls.fit, x.test, ncomp=12)

mean((pls.pred-y.test)^2)

pls.fit <- plsr(Salary~., data=Hitters, scale=TRUE, ncomp=12)
summary(pls.fit)

# Exercise: we want to predict the number of applications using the other variables
# in the College data set.  You can find this set in ISLR library.
# Try subset selection, shrinkage methods, and dimensional reduction methods and examine
# which method is working best based on training set and test set split.




