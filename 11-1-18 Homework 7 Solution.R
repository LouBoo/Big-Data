library(ISLR)
lm.fit <- lm(mpg~horsepower+weight, data=Auto)
t <- summary(lm.fit)$coefficients[2,3]

tstar <- NA

for (r in 1:1000) {
  ustar <- sample(lm.fit$residuals, nrow(Auto), replace=TRUE)
  ystar <- lm.fit$coefficients[1] + lm.fit$coefficients[2]*Auto$horsepower + lm.fit$coefficients[3]*Auto$weight + ustar
  lm.fit.star <- lm(ystar~Auto$horsepower+Auto$weight)
  tstar[r] <- (lm.fit.star$coefficients[2] - lm.fit$coefficients[2])/summary(lm.fit.star)$coefficients[2,2]
}

tstar.ordered <- sort(tstar, decreasing=FALSE)
cv.star <- c(tstar.ordered[25], tstar.ordered[975])
t
cv.star