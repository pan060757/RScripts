setwd("G:/研一/研一下学期/学习资料/R语言学习/R-script")
getwd()
source("beta.int.R")
source("try.R")
x <- c(0.10,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.20,0.21,0.23)
x
y <- c(42.0,43.5,45.0,45.5,45.0,47.5,49.0,53.0,50.0,55.0,55.0,60.0)
y
lm.sol <- lm(y~x+1)
beta.int(lm.sol)
summary(lm.sol)
new <- data.frame(x=0.16)
lm.pred <- predict(lm.sol,new,interval = "prediction",level=0.95)
lm.pred
anova(lm.sol)
coef(lm.sol)
deviance(lm.sol)
formula(lm.sol)
plot(lm.sol)
residuals(lm.sol)
step(lm.sol)
summary(lm.sol)



blood <- data.frame(x1=c(76.0,91.5,85.5,82.5,79.0,80.5,74.5,79.0,85.0,76.5,82.0,95.0,92.5),
                    x2=c(50,20,20,30,30,50,60,50,40,55,40,40,20),
                    Y=c(120,141,124,126,117,125,123,125,132,123,132,155,147))
blood
lm.sol <- lm(Y~x1+x2,data=blood)
summary(lm.sol)


beta.int(lm.sol)

new <- data.frame(x1=80,x2=40)
lm.pred <- predict(lm.sol,new,interval = "prediction",level=0.95)
lm.pred




toothpaste<-data.frame(
  X1=c(-0.05, 0.25,0.60,0, 0.25,0.20, 0.15,0.05,-0.15, 0.15,
       0.20, 0.10,0.40,0.45,0.35,0.30, 0.50,0.50, 0.40,-0.05,
       -0.05,-0.10,0.20,0.10,0.50,0.60,-0.05,0, 0.05, 0.55),
  X2=c( 5.50,6.75,7.25,5.50,7.00,6.50,6.75,5.25,5.25,6.00,
        6.50,6.25,7.00,6.90,6.80,6.80,7.10,7.00,6.80,6.50,
        6.25,6.00,6.50,7.00,6.80,6.80,6.50,5.75,5.80,6.80),
  Y =c( 7.38,8.51,9.52,7.50,9.33,8.28,8.75,7.87,7.10,8.00,
        7.89,8.15,9.10,8.86,8.90,8.87,9.26,9.00,8.75,7.95,
        7.65,7.27,8.00,8.50,8.75,9.21,8.27,7.67,7.93,9.26)
)
toothpaste
lm.sol <-lm(Y~X1+X2,data=toothpaste)
summary(lm.sol)

attach(toothpaste)
plot(Y~X1)
abline(lm(Y~X1))

plot(Y~X2)
lm2.sol <- lm(Y~X2+I(X2^2))
x <- seq(min(X2),max(X2),len=200)
y <- predict(lm2.sol,data.frame(X2=x))
plot(Y~X2);lines(x,y)

lm.new <- update(lm.sol,.~.+I(X2^2))
summary(lm.new)

beta.int(lm.new)

lm2.new <- update(lm.new,.~.-X2)
summary(lm2.new)

lm3.new <- update(lm.new,.~.+X1*X2)
summary(lm3.new)

beta.int(lm3.new)
