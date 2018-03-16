####对统筹收入和支出按照月份进行
library("fUnitRoots")
library("forecast")
source("E:/毕业设计/project/RScripts/gm11.R")
source("E:/毕业设计/project/RScripts/指数平滑法建模.R")
data <- read.csv("E:/毕业设计/建模结果/income_costByMonth.csv",header = T)
colnames(data) <- c('month','income','cost')
data

#######################################################################################
######用2006-2013年数据，预测2014-2015年度数据情况
######检查是否具有季节性趋势
costseries <- ts(data$cost,frequency=12,start=c(2006,1))
ts.plot(costseries)
decomposeRt<-decompose(costseries)
plot(decomposeRt)
training <- data$cost[1:96]
fit <- ets(training, model = "AAA")
summary(fit)
######误差检验
acf(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box") 
######残差qq图
qqnorm(fit$residuals)
qqline(fit$residuals)

pred <- forecast(fit,24)
plot(pred, xlab="Year", ylab="统筹基金收入",
      main="数据拟合情况")

training <- data$cost[1:108]
fit <- ets(training, model = "AAA")
summary(fit)
######误差检验
acf(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box") 
######残差qq图
qqnorm(fit$residuals)
qqline(fit$residuals)

training <- data$cost
fit <- ets(training, model = "AAA")
summary(fit)
######误差检验
acf(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box") 
######残差qq图
qqnorm(fit$residuals)
qqline(fit$residuals)

pred <- forecast(fit,24)
plot(pred, xlab="Year", ylab="统筹基金收入",
     main="数据拟合情况")






costseries <- ts(data$cost[1:96],frequency=12 )
costseries
plot.ts(costseries)
fit<- HoltWinters(costseries)
residuals <-costseries -fit$fitted 
residuals 
######误差检验
acf(residuals)
Box.test(residuals,type="Ljung-Box") 
######残差qq图
qqnorm(residuals)
qqline(residuals)
plot(costmodel)
errorate <-(costmodel$fitted-data$cost)/data$cost
errorate
# ###########预测未来期的值
# cost_predict <- forecast.HoltWinters(costmodel, h=24)
# cost_predict
# cost_predict$fitted   #####
# predict <- data.frame(cost_predict)  ####取第二列预测值
# write.table(predict[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
# plot.forecast(cost_predict)
####检验残差是否自相关
acf(cost_predict$residuals, lag.max=20)  
Box.test(cost_predict$residuals, lag=20, type="Ljung-Box")  
plot.ts(cost_predict$residuals)  


#####对统筹支出进行建模
cost <- ts(data$cost[1:96],frequency=12,start=c(2006,1))
plot.ts(cost)
#一阶差分
costdiff1 <- diff(cost,differences = 1)
adfTest(costdiff1)
plot.ts(costdiff1)
auto.arima(costdiff1,trace=T)   #####模型自动选择
x_arima <- arima(cost,order=c(0,1,1),seasonal=list(order=c(0,1,2),period=12),method="ML")
accuracy(x_arima)
######误差检验
acf(x_arima$residuals)
Box.test(x_arima$residuals,type="Ljung-Box") 
tsdiag(x_arima)
######残差qq图
qqnorm(x_arima$residuals)
qqline(x_arima$residuals)
tsdiag(x_arima)
xforecasts<-forecast.Arima(x_arima,h=24,level=c(99.5))
xforecasts
plot.forecast(xforecasts)
write.table(xforecasts[4],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)



#######################################################################################
######用2006-2014年数据，预测2015数据情况
costseries <- ts(data$cost[1:108],frequency=12 )
costseries
plot.ts(costseries)
costmodel<- HoltWinters(costseries)
costmodel
plot(costmodel)
errorate <-(costmodel$fitted-data$cost)/data$cost
errorate
###########预测未来期的值
cost_predict <- forecast.HoltWinters(costmodel, h=12)
cost_predict
accuracy(cost_predict)      #####得到准确性度量
cost_predict$fitted   #####
predict <- data.frame(cost_predict)  ####取第二列预测值
write.table(predict[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
plot.forecast(cost_predict)
####检验残差是否自相关
acf(cost_predict$residuals, lag.max=20)  
Box.test(cost_predict$residuals, lag=20, type="Ljung-Box")  
plot.ts(cost_predict$residuals)  


#####对统筹支出进行建模
cost <- ts(data$cost[1:108],frequency=12)
plot.ts(cost)
#一阶差分
costdiff1 <- diff(cost,differences = 1)
adfTest(costdiff1)
plot.ts(costdiff1)
auto.arima(costdiff1,trace=T)   #####模型自动选择
x_arima <- arima(cost,order=c(0,1,1),seasonal=list(order=c(0,1,2),period=12),method="ML")
accuracy(x_arima)
######误差检验
acf(x_arima$residuals)
Box.test(x_arima$residuals,type="Ljung-Box") 
######残差qq图
qqnorm(x_arima$residuals)
qqline(x_arima$residuals)
tsdiag(x_arima)

xforecasts<-forecast.Arima(x_arima,h=12,level=c(99.5))
xforecasts
plot.forecast(xforecasts)
write.table(xforecasts[4],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)



#######################################################################################
######用2006-2015年度数据，预测未来统筹费用支出走向
######指数平滑法模型
costseries <- ts(data$cost,frequency=12 )
costseries
plot.ts(costseries)
costmodel<- HoltWinters(costseries)
costmodel
plot(costmodel)
errorate <-(costmodel$fitted-data$cost)/data$cost
errorate
###########预测未来期的值
cost_predict <- forecast.HoltWinters(costmodel,h=60)
cost_predict
cost_predict$fitted   #####
predict <- data.frame(cost_predict)  ####取第二列预测值
write.table(predict[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
plot.forecast(cost_predict)
####检验残差是否自相关
acf(na.omit(cost_predict$residuals), lag.max=12)  
Box.test(cost_predict$residuals, lag=12, type="Ljung-Box")  
plot.ts(cost_predict$residuals)  

######ARIMA模型
#####确定模型差分数
costseries <- ts(data$cost,frequency=12,start=c(2006,1))
ndiffs(costseries)
plot.ts(costseries)
#一阶差分
costdiff1 <- diff(costseries,differences = 1)
adfTest(costdiff1)
plot.ts(costdiff1)
acf(costdiff1) 
pacf(costdiff1)
auto.arima(costdiff1,trace=T)   #####模型自动选择
x_arima <- arima(costseries,order=c(0,1,1),seasonal=list(order=c(0,1,2),period=12),method="ML")
x_arima
accuracy(x_arima)
######误差检验
acf(x_arima$residuals)
Box.test(x_arima$residuals,type="Ljung-Box") 
######残差qq图
qqnorm(x_arima$residuals)
qqline(x_arima$residuals)
tsdiag(x_arima)

xforecasts<-forecast.Arima(x_arima,h=60,level=c(99.5))
xforecasts
plot.forecast(xforecasts)
write.table(xforecasts[4],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
