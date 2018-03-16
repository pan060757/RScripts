###利用2006-2013年的收入
income <- c(28138,32647,43342,49445,57940,67845,71924,114512)
incomeseries <- ts(income)
incomeseries
plot.ts(incomeseries)      ######绘制时间序列图

#########平滑法：比如计算时间序列的简单移动平均

#########在R的“TTR”包中的SMA()函数可以用简单的移动平均来平滑时间序列数据。
install.packages("TTR")
library("TTR")
####使用跨度为3的简单移动平均平滑时间序列
incomeSMA <- SMA(incomeseries,n=3)
plot.ts(incomeSMA)

##########使用指数平滑法来进行预测
incomepredict <- HoltWinters(incomeseries,beta=FALSE,gamma=FALSE)
incomepredict
##########指数平滑法采取的预测存储在一个元素名为"fitted"的列表变量里
incomepredict$fitted
plot(incomepredict)
###########可以计算样本内预测误差的误差平方之和，即原始时间序列覆盖的时间内的预测误差
incomepredict$SSE
###########HoltWinters()的默认仅仅是预测时期即覆盖原始数据的时期
###########下列安装包的过程并不成功
install.packages("forecast")
library("forecast")



#############Arima模型
##########一阶差分如下所示
logincomeseries <- log(incomeseries)
incomediff1 <- diff(logincomeseries,differences = 1)
plot.ts(incomediff1)
##二阶差分
incomediff2 <- diff(logincomeseries,differences = 2)
plot.ts(incomediff2)
#三阶差分
incomediff3<- diff(logincomeseries,differences=3)
plot.ts(incomediff3)



####auto.arima()
incomeArima <- auto.arima(incomeseries,ic="bic")
incomeforecast <- forecast.Arima(incomeArima,h=2)  #####对未来两年的医疗费用收入进行预测
incomeforecast



##########对2006-2013年的医疗支出进行预测
cost <- c(12679,19805,25829,35186,48708,60951,72716,90335)
costseries <- ts(cost)
#一阶差分
costdiff1 <- diff(costseries,differences = 1)
plot.ts(costdiff1)
##二阶差分
costdiff2 <- diff(costseries,differences = 2)
plot.ts(costdiff2)
#三阶差分
costdiff3<- diff(costseries,differences=3)
plot.ts(costdiff3)
####auto.arima()
costArima <- auto.arima(costseries,ic="bic")
costforecast <- forecast.Arima(costArima,h=2)  #####对未来两年的费用支出进行预测
costforecast
















