source("计算增长率.R")
income <-data.frame(
     t=c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015),
     y=c(17598,22992,32778,36613,41376,51208,63223,86016,103075,122984))
income
cost <- data.frame(
  t=c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015),
  y=c(12679,19805,25829,35186,48708,60951,72716,90335,96118,116651))
cost
incomeratio <- getratio(income$y)
incomeratio
costratio <- getratio(cost$y)
costratio
t <- income$t[-1]
plot(t,incomeratio,type='o',main="增长率对比图",col='blue',ylim=c(0,0.6))
legend("topright",legend =c("红色","蓝色"),col=c("blue","red"),pch=c(5,24),lty=1)
lines(t,costratio,type='o',main="增长率对比图",col='red')


