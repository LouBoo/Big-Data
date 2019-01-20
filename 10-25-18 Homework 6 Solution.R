library(ISLR)
library(boot)

head(Smarket)

# Exercise 1

set.seed(1)

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

cv.error <- rep(NA, 4)
for (i in 3:6) {
  Sdata <- Smarket[,c(2:i,9)]
  glm.fit <- glm(Direction~., family=binomial, data=Sdata)
  cv.error[i-2] <- cv.glm(Sdata, glm.fit, cost = cost, K=10)$delta[1]
}
cv.error
cv.error[which.min(cv.error)]

# Exercise 2

library(class)

set.seed(1)
aa <- rep(NA, 100)
cv.error <- matrix(aa, 10, 10)

Sdata1.temp <- Smarket[, c(2,3,9)]
n <- nrow(Sdata1.temp)
ni <- n/10
a <- sample(1:n, n, replace=FALSE)
Sdata1 <- Sdata1.temp[a, ]
Sdata1$Lag1 <- scale(Sdata1$Lag1)
Sdata1$Lag2 <- scale(Sdata1$Lag2)
# cost <- function(r, pi ) {
#   weight1 <- 1
#   weight0 <- 1
#   c1 <- (r==1)&(pi<0.5)
#   c0 <- (r==0)&(pi>=0.5)
#   return(mean(weight1 * c1 + weight0 * c0))
# }

for (j in 1:10) {
  for (i in 1:10) {
    test.index <- (ni * (i-1) + 1) : (ni * i)
    test.data <- Sdata1[test.index, ]
    train.data <- Sdata1[-test.index, ]
    yhat <- knn(train.data[,1:2], test.data[,1:2], train.data[,3], k=j+30)
    cv.error[i,j] <- mean(yhat != test.data[,3])
  }
}

cv.error.means <- colMeans(cv.error)
cv.error.means[which.min(cv.error.means)]
