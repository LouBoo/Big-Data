library(ISLR)
College <- College

set.seed(1)
train <- sample(1:nrow(College), round(nrow(College))/2)
train.set <- College[train,]
test.set <- College[-train,]
x.test <- test.set[,-2]
y.test <- test.set[,2]
x.train <- train.set[,-2]
y.train <- train.set[,2]

### Subset Selection
library(leaps)
regfit.full <- regsubsets(Apps~., data=College)
summary(regfit.full)

regfit.full <- regsubsets(Apps~., data=College, nvmax=19)
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

coef(regfit.full, 12)

reg.fit <- lm(Apps~Private+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+PhD+Expend+Grad.Rate, data=College)
reg.pred <- predict(reg.fit, x.test)

sub.msep <- mean((reg.pred-y.test)^2)


### Shrinkage Method: Ridge
library(glmnet)
x.temp <- model.matrix(Apps~., College)

head(x.temp)
x <- x.temp[,-2]
y <- College$Apps

grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)

dim(coef(ridge.mod))

# Cross validation to choose lambda
train <- sample(1:nrow(x), round(nrow(x)/2))
y.train1 <- y[train]
x.train1 <- x[train,]
y.test1 <- y[-train]
x.test1 <- x[-train,]

ridge.mod <- glmnet(x.train1, y.train1, alpha=0, lambda=grid, thresh=1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx=x.test1)
ridge.msep <- mean((ridge.pred - y.test)^2)

ridge.pred <- predict(ridge.mod, s=0, newx=x.test1, exact=TRUE, x=x.train1, y=y.train1)
mean((ridge.pred - y.test1)^2)

cv.out <- cv.glmnet(x.train1, y.train1, alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s=bestlam, newx=x.test1)
ridge.msep <- mean((ridge.pred - y.test1)^2)



### Dimensional Reduction
library(pls)
pcr.fit <- pcr(Apps~., data=College, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")
pcr.fit <- pcr(Apps~., data=train.set, scale=TRUE, ncomp=5)
pcr.pred <- predict(pcr.fit, x.test, ncomps=5)
pcr.msep <- mean((pcr.pred-y.test)^2)

pcr.fit <- pcr(Apps~., data=College, scale=TRUE)
summary(pcr.fit)



pls.fit <- plsr(Apps~., data=College, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

pls.fit <- plsr(Apps~., data=train.set, scale=TRUE, ncomp=5)
pls.pred <- predict(pls.fit, x.test, ncomp=5)

pls.msep <- mean((pls.pred-y.test)^2)

pls.fit <- plsr(Apps~., data=College, scale=TRUE, ncomp=5)
summary(pls.fit)

msep <- list(sub.msep, ridge.msep, pls.msep, pcr.msep)
best.method <- which.min(msep)
best.method
# 1 corresponds to best subset selection method