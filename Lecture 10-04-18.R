library(MASS)
library(ISLR)
train <- subset(Smarket, Smarket$Year < 2005)
test <- subset(Smarket, Smarket$Year >= 2005)
lda.fit <- lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=train)
lda.fit

plot(lda.fit)

lda.pred <- predict(lda.fit, test)
plot(lda.pred$x)
names(lda.pred)


lda.class <- lda.pred$class
table(lda.class)
table(lda.class, test$Direction)
mean(lda.class==test$Direction)
