#  Exercise 3
#  Simulate time series data and see that conventional tests may not work in the time series. 
#  e.g. GDP growth, stock price, a firm's revenue.... Marketing
rm(list=ls())
install.packages("sandwich")
library(sandwich)
set.seed(102)
reject <- 0
A <- 1000
rej <- rep(NA,A)
rej2 <- rep(NA,A)

for (i in 1:A) {
  Sales<- rep(NA,100)
  Online <- rep(NA,100)
  e <- rep(NA,100)
  
  Online[1] <- 2*rnorm(1)
  e[1] <- rnorm(1)
  b0 <- 1
  b1 <- 0
  Sales[1] <- b0 + b1*Online[1] + e[1]
  rho1 <- 0.7
  rho2 <- 0.7
  
  for (t in 2:100) {
    Online[t] <- rho1*Online[t-1] + rnorm(1)
    e[t] <- rho2*e[t-1] + rnorm(1)
    Sales[t] <- b0 + b1*Online[t] + e[t]
  }
  
  linear.fit <- lm(Sales~Online)
  rej[i] <- (0<confint(linear.fit)[2,1]) + (0>confint(linear.fit)[2,2])
  se2 <- sqrt(NeweyWest(linear.fit)[2,2])
  ci2 <- c(coefficients(linear.fit)[2]-1.96*se2, coefficients(linear.fit)[2]+1.96*se2)
  rej2[i] <- (0<ci2[1]) + (0>ci2[2])
}
sum(rej)/A
sum(rej2)/A