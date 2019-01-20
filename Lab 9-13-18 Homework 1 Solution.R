# Exercise 1.
# Plot the fitted y with the prediction interval based on the quadratic regression model: lm.fit3
library(MASS)
library(ISLR)
Boston$lstat2 <- (Boston$lstat)^2
lm.fit3 <- lm(medv~lstat+lstat2, data=Boston)
x1 <- seq(min(Boston$lstat),max(Boston$lstat),length=30)
x2 <- x1^2
newX <- data.frame(lstat=x1, lstat2=x2)
yhat <- predict(object=lm.fit3, newdata=newX, interval="prediction")
plot(Boston$lstat,Boston$medv, pch=20)
lines(x1, yhat[,1], lwd=3)
lines(x1, yhat[,2], col="red", lwd=3)
lines(x1, yhat[,3], col="red", lwd=3)

#  Exercise 2 
# (1) Download the housing dataset from https://www.kaggle.com/harlfoxem/housesalesprediction
#     and run a regression to predict housing prices. 


# (2) Build a model to predict the housing price given characteristics of a house in the dataset.
#     Consider the following predictors:
#    
#     season, sqft_living, yr_built, interaction of sqft_living yr_built, and waterfront

#     Create a variable "season" which equals 
#     "Winter" if a house was sold in Jan, Feb, Mar, Dec.
#     "Spring" if it was sold in Apr, May, Jun
#     "Summer" if it was sold in Jul, Aug
#     "Fall" if it was sold in Sep, Oct, Nov.
#     Do you find any seasonality in housing price?


# (3) Do you find any nonlinearity or heteroskedasticity? 
#     What is the problem if the error term is heteroskedastic?
#     How can you address these problems (if you have here)?


# (4) Conduct an F test for the following hypotheses.
#     H0: there is no seasonality on the housing price. H1: H0 is not true.


# (5) Predict the housing price when season = spring, sqft_living=2500, yr_built=2000, waterfront=0.


### Exercise 2
### (1) & (2)
setwd("D:/Documents (Louis Booth)/R/Big Data/kc_house_data.csv")
house <- read.csv("kc_house_data.csv", header=TRUE)
head(house)
summary(house)
str(house)

house$date <- as.character(house$date)
house$date <- strsplit(house$date, 'T')
for (i in 1:nrow(house)) {
  house$date[[i]] <- house$date[[i]][1]
}
house$date <- as.Date(as.character(house$date), "%Y%m%d")


house$season[format.Date(house$date, "%m") == "01" | format.Date(house$date, "%m") == "02" | format.Date(house$date, "%m") == "03" | format.Date(house$date, "%m") == "12"] <- "Winter"
house$season[format.Date(house$date, "%m") == "04" | format.Date(house$date, "%m") == "05" | format.Date(house$date, "%m") == "06"] <- "Spring"
house$season[format.Date(house$date, "%m") == "07" | format.Date(house$date, "%m") == "08"] <- "Summer"
house$season[format.Date(house$date, "%m") == "09" | format.Date(house$date, "%m") == "10" | format.Date(house$date, "%m") == "11"] <- "Fall"

lm.fit1 <- lm(price~season+sqft_living+yr_built+sqft_living*yr_built+waterfront, data=house)
summary(lm.fit1)

# Yes, based on the significance of the seasonSpring coefficient,
# there is likely some seasonality in housing prices

### (3)
par(mfrow=c(2,2))
plot(lm.fit1)

# Yes there is heteroscedasticity present, based on the Residuals vs Fitted and Scale-Location plots
# The problem with heteroscedasticty is that the variance of the error terms varies based on Xi
# I would likely use robust standard errors in the model to correct for this

### (4)
lm.fit_u <- lm(price~season+sqft_living+yr_built+sqft_living*yr_built+waterfront, data=house)
lm.fit_r <- lm(price~sqft_living+yr_built+sqft_living*yr_built+waterfront, data=house)
ssr_u <- sum((fitted(lm.fit_u) - house$price)^2)
ssr_r <- sum((fitted(lm.fit_r) - house$price)^2)
f <- ((ssr_r - ssr_u)/3)/(ssr_u/(21613-5-1))
# Reject Ho

### (5)
predict(object=lm.fit1,newdata=data.frame(season="Spring", sqft_living=2500, yr_built=2000, waterfront=0), interval="prediction")
predict(object=lm.fit1,newdata=data.frame(season="Spring", sqft_living=2500, yr_built=2000, waterfront=0), interval="confidence")
predict(object=lm.fit1,newdata=data.frame(season="Spring", sqft_living=2500, yr_built=2000, waterfront=0))



###########################################SOLUTION################################################



setwd("D:/Documents (Louis Booth)/R/Big Data/kc_house_data.csv")
house <- read.csv("kc_house_data.csv", header=TRUE)
str(house)
house$date <- as.character(house$date)
head(house)
house$year <- substr(house$date, start=1, stop=4)
house$month <- substr(house$date, start=5, stop=6)
fix(house)

house$year <- as.factor(house$year)

house$month <- as.numeric(house$month)
house$season <- NA
house$season[house$month %in% c(1,2,3,12)] <- "Winter"
house$season[house$month %in% c(4,5,6)] <- "Spring"
house$season[house$month %in% c(7,8)] <- "Summer"
house$season[house$month %in% c(9,10,11)] <- "Fall"

class(house$season)
house$season <- as.factor(house$season)

linear.fit <- lm(price~season+sqft_living*yr_built+waterfront, data=house)
summary(linear.fit)
par(mfrow=c(2,2))
plot(linear.fit)

house$lprice <- log(house$price)

pairs(~lprice+year+season+sqft_living+waterfront+yr_built, data=house)
linear.fit <- lm(lprice~season+sqft_living*yr_built+waterfront, data=house)
summary(linear.fit)
plot(linear.fit)

linear.fit_Ho <- lm(lprice~sqft_living*yr_built+waterfront, data=house)

SSR <- sum((linear.fit$residuals)^2)
SSRo <- sum((linear.fit_Ho$residuals)^2)

Fstat <- ((SSRo-SSR)/3)/(SSR/(dim(house)[1]-7-1))
Fstat
qf(0.95,df1=1,df2=dim(house)[1]-7-1)
