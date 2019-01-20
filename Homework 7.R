library(boot)
library(ISLR)

set.seed(1)

df <- Auto[, c(1, 4, 5)]

X1 <- df[, 2]
X2 <- df[, 3]
y <- df[, 1]

lm.fit <- lm(y ~ X1 + X2)

beta0.hat <- lm.fit$coefficients[1]
beta1.hat <- lm.fit$coefficients[2]
beta2.hat <- lm.fit$coefficients[3]

n <- nrow(df)

beta1.star <- rep(NA, 1000)
beta2.star <- rep(NA, 1000)
t.star <- rep(NA, 1000)

for (r in 1:1000) {
  ind <- sample(1:n, size = n, replace = TRUE)
  u.hat <- lm.fit$residuals[ind]
  y.hat <- beta0.hat + beta1.hat * X1 + beta2.hat * X2 + u.hat
  lm.fit.star <- lm(y.hat ~ X1 + X2)
  beta1.star[r] <- lm.fit.star$coefficients[2]
  t.star[r] <- (beta1.star[r] - beta1.hat) / summary(lm.fit.star)$coefficients[2,2]
}

t.star <- sort(t.star)
crit.t.star <- c(t.star[25], t.star[975])


pdf(file="Homework 7.pdf")
hist(t.star, breaks = 30, probability = TRUE, col= "grey", 
     main = "Distribution of t* Under Residual Bootstrap")
lines(density(t.star), col = "red", lwd = 3)
abline(v = c(crit.t.star, beta1.hat), col = c("blue"), lty = c(2, 2, 1), lwd = 3)
cat(paste("The 95% critical values are", round(crit.t.star[1], 3),
          "and", round(crit.t.star[2], 3), "."))
dev.off()