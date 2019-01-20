# Exercise Download "Auto.csv" from http://www-bcf.usc.edu/~gareth/ISL/data.html
# and estimate the regression of mpg on weight using the KNN method.
# Draw the regression line on the scatterplot and compare it with linear regression line.
rm(list=ls())

setwd("D:/Documents (Louis Booth)/R/Big Data")
Auto = read.table("Auto.data", header=TRUE, na.strings="?")

y <- Auto$mpg
x <- Auto$weight

knn = function(x0, X, Y, k) {
  x0 <- matrix(rep(x0, length(y)), byrow=TRUE)
  X <- matrix(x)
  distance <- rowSums((x0-X)^2)
  rank <- order(distance)
  y_k <- Y[rank][1:k]
  mean(y_k)
}

plot(Auto$weight, Auto$mpg)
linear.fit <- lm(mpg~weight, data=Auto)
abline(linear.fit, lwd=3)
a <- 70
K <- 50
x_vec <- seq(min(Auto$weight)+100, max(Auto$weight)-100, length=a)
fhat <- rep(NA, a)
for (i in 1:a) {
  fhat[i] <- knn(x_vec[i], Auto$weight, Auto$mpg, K)
}
lines(x_vec, fhat, lwd=3, col="red")