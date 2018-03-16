cement<-data.frame(
  X1=c( 7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10),
  X2=c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68),
  X3=c( 6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8),
  X4=c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12),
  Y =c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5,
       93.1,115.9, 83.8, 113.3, 109.4)
)
cement
lm.sol <- lm(Y~X1+X2+X3+X4,data=cement)
summary(lm.sol)

#用step函数作逐步回归
lm.step <- step(lm.sol)
summary(lm.step)

drop1(lm.step)

lm.opt <- lm(Y~X1+X2,data=cement)
summary(lm.opt)


blood<-data.frame(
  X1=c(76.0, 91.5, 85.5, 82.5, 79.0, 80.5, 74.5,
       79.0, 85.0, 76.5, 82.0, 95.0, 92.5),
  X2=c(50, 20, 20, 30, 30, 50, 60, 50, 40, 55,
       40, 40, 20),
  Y= c(120, 141, 124, 126, 117, 125, 123, 125,132, 123, 132, 155, 147)
)
blood
lm.sol <- lm(Y~X1+X2,data=blood)
summary(lm.sol)
y.res <- resid(lm.sol)
y.fit <- predict(lm.sol)
plot(y.res~y.fit)
plot(blood$Y~y.fit)
abline(lm(blood$Y~y.fit))

y.rst <- rstandard(lm.sol)
plot(y.rst~y.fit)

