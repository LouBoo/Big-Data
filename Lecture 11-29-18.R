library(ISLR)

## Polynomial Regression

head(Wage)
fit <- lm(wage~poly(age,4), data=Wage)
summary(fit)

a <- c(1,2,3)
poly(a,2)
poly(a,2,raw=TRUE)

fit2 <- lm(wage~poly(age,4,raw=TRUE), data=Wage)
summary(fit2)
fit2a <- lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
fit2b <- lm(wage~cbind(age,age^2,age^3,age^4), data=Wage)

agelims <- range(Wage$age)
age.grid <- seq(from=agelims[1], to=agelims[2])

preds <- predict(fit,newdata=list(age=age.grid),se=TRUE)
se.band <- cbind(preds$fit - 2*preds$se.fit, preds$fit + 2*preds$se.fit)

preds.1 <- predict(fit,newdata=list(age=age.grid),interval="confidence")
summary(preds.1)
conf.band <- preds.1[,2:3]

plot(Wage$age, Wage$wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Degree-4 Polynomial")
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.band, lwd=2, col="red", lty=3)

preds2 <- predict(fit2, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit-preds2$fit))

# Decision of the degree of the polynomial: anova (test about the coefficients) 
