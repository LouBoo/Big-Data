# Example 1: Wage data

install.packages("ISLR")
library("ISLR")
data()

# Example 1: Wage data

str(Wage)
head(Wage)

x1 <- Wage$age
x2 <- Wage$year
x3 <- Wage$education

y <- Wage$wage

plot(x1,y)
linear <- lm(y~x1)
abline(linear, col="blue", lwd=3)

data1_temp <- data.frame(x1,y)
data1 <- data1_temp[order(data1_temp$x1),]
head(data1)
tail(data1)

plot(data1$x1,data1$y, xlab="Age", ylab="Wage")
lpr <- loess(data1$y~data1$x1)
lines(data1$x1,lpr$fitted, col="red", lty=1, lwd=3)

plot(x2,y, xlab="Year", ylab="Wage")
linear2 <- lm(y~x2)
abline(linear2, col="blue", lwd=3)
summary(linear2)

str(Wage$education)
summary(Wage$education)
x3 <- as.numeric(x3)
boxplot(y~x3, col=2:6, xlab="Education Level", ylab="Wage")

# Example 2: Stock Market Data
# See the association between today change and previous day's change
rm(list=ls())

head(Smarket)
tail(Smarket)
par(mfrow=c(1,3))
boxplot(Lag1~Direction, data=Smarket, col=c("blue", "red"), main="Yesterday", xlab="Today's Direction", ylab="% change in S&P")
boxplot(Lag2~Direction, data=Smarket, col=c("blue", "red"), main="One Day Previous", xlab="Today's Direction", ylab="% change in S&P")
boxplot(Lag3~Direction, data=Smarket, col=c("blue", "red"), main="Two Day's Previous", xlab="Today's Direction", ylab="% change in S&P")

# Example 3: Gene Expression Data
# consists of 64 cancer cell lines. Each cancer cell line has 6830 gene expression measurements

rm(list=ls())
par(mfrow=c(1,2))
head(NCI60)
summary(NCI60)

cc <- as.factor(NCI60$labs)
ccn <- as.numeric(cc)

pca.data <- prcomp(NCI60$data, scale.=TRUE)
plot(pca.data$x[,1], pca.data$x[,2], pch=20, lwd=5)
plot(pca.data$x[,1], pca.data$x[,2], col=2*ccn, pch=20, lwd=5)

