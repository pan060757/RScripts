cars
plot(cars$dist,type="l")
plot(cars$dist,type="h")
hist(cars$dist)

library(lattice)
num <- sample(1:3,size=50,replace=T)
num
barchart(table(num))

qqmath(rnorm(100))

stripplot(~Sepal.Length|Species,data=iris,layout=c(1,3))
densityplot(~Sepal.Length,groups=Species,data=iris,plot.points=FALSE)

bwplot(Species~Sepal.Length,data=iris)
xyplot(Sepal.Width~Sepal.Length,groups=Species,data=iris)
splom(iris[1:4])

w <- c(75,64,47.4,66.9,62.2,58.7,63.5,66.6,64.0,57.0,69.0,56.9,50.0,72.0)
w
x <- 44:76
hist(w,freq=FALSE)
x
lines(density(w),col="blue")
lines(x,dnorm(x,mean(w),sd(w)),col="yellow")

ecdf(x)
plot(ecdf(w),verticals = TRUE,do.p=FALSE)

w <- c(75,64,47.4,66.9,62.2,66.2,58.7,63.5,66.6,64.0,57.0,69.0,56.9,50.0,72.0)
w
qqnorm(w)
qqline(w)

x <- c(25,45,50,54,55,61,64,68,72,75,75,78,79,81,83,84,84,84,85,86,86,86,87,89,89,89,90,91,91,92,100)
x
stem(x)


w <- c(75.0,64,47.4,66.9,62.2,62.2,58.7,63.5,66.6,64,57,69,56.9,50,72)
w
shapiro.test(x)

x <- c(1,2,3,5)
y <- c(2,3,6,8)
plot(x,y)
title(main="x~yÉ¢µãÍ¼")
lm.sol <- lm(y~x)
abline(lm.sol)


ore <- data.frame(x=c(67,54,72,64,39,22,58,43,46,34),y=c(24,15,23,19,16,11,20,16,17,13))
ore
attach(ore)
cor.test(x,y)




e <- read.table("clipboard", header = T)
e
colMeans(e)
cov(e)
cor(e)
cor.test(~x1+x2,data=e)
cor.test(~x1+x3,data=e)
cor.test(~x2+x3,data=e)

x <-seq(0.10,0.23,0.01)
x <- c(0.10,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.20,0.21,0.23)
x
y <- c(42.0,43.5,45.0,45.5,45.0,47.5,49.0,53.0,50.0,55.0,55.0,60.0)
y
lm.sol <- lm(y~x+1)
summary(lm.sol)
