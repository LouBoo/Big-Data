# Basics in R

c <- c(1,6,2)
length(x)

y <- c(1,4,3)

x+y

ls()
rm(list=ls())

x <- matrix(data=c(1,2,3,4), nrow=2, ncol=2)
matrix(c(1,2,3,4),2,2, byrow=TRUE)

x <- rnorm(50)
y <- x + rnorm(50, mean=50, sd=0.1)
cor(x,y)

set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# Graphics
ls()
rm(list=ls())

x <- rnorm(100)
y <- rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x axis", ylab="this is the y axis", main="Plot of X vs Y")
pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()

getwd()

x <- seq(1,10)
x
x <- 10:1.5
x
x <- seq(-pi, pi, length=50)

y <- x
f1 <- outer(x,y,"+")
contour(x,y,f1)
f2 <- outer(x,y, function(x,y)cos(y)/(1+x^2))
contour(x, y, f2)
contour(x, y, f2, nlevels=45, add=T)

fa <- (f2 - t(f2))/2
contour(x, y, fa, nlevels=15)

image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta=30)
persp(x, y, fa, theta=30, phi=20)
persp(x, y, fa, theta=30, phi=70)

# Indexing data

A <- matrix(1:16, nrow=4, ncol=4)
A

a[2,3]    # [Row,Column]
A[2,]
A[1:2,4]
A[c(1,3),c(2,4)]
A[-c(1,2),]
dim(A)

# Loading data
setwd("D:/Documents (Louis Booth)/R/Big Data")
Auto = read.table("Auto.data")
str(Auto)
head(Auto)
tail(Auto)
fix(Auto)
Auto <- read.table("Auto.data", header=TRUE)
fix(Auto)
Auto <- read.table("Auto.data", header=TRUE, na.strings="?")    # Convert "?" to "NA"
fix(Auto)
dim(Auto)

summary(Auto$weight)
Auto$size[Auto$weight < 2500] <- "small"
Auto$size[Auto$weight >= 2500 & Auto$weight < 3500] <- "medium"
Auto$size[Auto$weight >= 3500] <- "large"

rank <- order(Auto$name)
Auto[rank,]

names(Auto)
head(Auto)

Auto <- na.omit(Auto)   # remove the rows that contain missing values
dim(Auto)

Auto <- read.csv("Auto.csv", header=TRUE, na.strings="?")
Auto$id <- 1:dim(Auto)[1]

Auto1 <- Auto[,c("id", "mpg", "cylinders", "name")]
Auto2 <- Auto[sample(1:dim(Auto)[1], dim(Auto)[1], replace=FALSE), c("id", "mpg", "cylinders", "name")]
Auto2 <- Auto2[-(1:50),]
Auto3 <- merge(Auto1, Auto2, by="id", all=TRUE)
fix(Auto3)

# Additional graphical and numerical summaries

plot(Auto$cylinders, Auto$mpg)
Auto$cylinders <- as.factor(Auto$cylinders)
str(Auto)
plot(Auto$cylinders, Auto$mpg, col=2:6, xlab="cylinders", ylab="mpg")
plot(Auto$cylinders, Auto$mpg, col=2:6, varwidth=TRUE, horizontal=TRUE)
table(Auto$cylinders)

hist(Auto$mpg)
hist(Auto$mpg, col="blue", breaks=15)

pairs(Auto)
pairs(~mpg + displacement + horespower + weight, data=Auto)