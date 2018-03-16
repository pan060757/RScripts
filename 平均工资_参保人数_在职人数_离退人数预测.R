#####################################################################
#####平均工资建模(gm11效果更好)
library("forecast")
source("E:/毕业设计/project/RScripts/gm11.R")
source("E:/毕业设计/project/RScripts/指数平滑法建模.R")
data <- read.csv("E:/毕业设计/毕业论文/论文/pdf/平均工资_在职人数_退休人数.csv")
colnames(data) <- c("year","avg_wage","working_number","retired_number")
#####平均工资预测
X0 <- data$avg_wage
####???2014年平均工资进行预???(使用2006-2013年的数据)
predict<- gm(X0[1:8],2)
errorRate <- (predict-X0[9:10])/X0[9:10]*100
errorRate

####???2015年平均工资进行预???(使用2006-2014年的数据)
predict <- gm(X0[1:9],1)
errorRate <- (predict-X0[10])/X0[10]*100
errorRate

####对未???5年平均工资进行预???(使用2006-2015年的数据)
predict <- gm(X0[1:10],5)
predict

####指数平滑法
X_fitted<- HoltWinters(X0[1:8],gamma=FALSE)
X_fitted
X_fitted$fitted[,1]
plot(X_fitted)             ##拟合值和预测值的对比???
###########预测未来期
forecast <- forecast.HoltWinters(X_fitted,h=2)
print(forecast)
predict <- data.frame(forecast)  ####取第二列预测???
print(predict[,1])
plot.forecast(incomeseriesforecast2)


X_fitted<- HoltWinters(X0[1:9],gamma=FALSE)
X_fitted
X_fitted$fitted[,1]
plot(X_fitted)             ##拟合值和预测值的对比???
###########预测未来期
forecast <- forecast.HoltWinters(X_fitted,h=1)
print(forecast)
predict <- data.frame(forecast)  ####取第二列预测???
print(predict[,1])
plot.forecast(incomeseriesforecast2)


X_fitted<- HoltWinters(X0[1:10],gamma=FALSE)
X_fitted
X_fitted$fitted[,1]
plot(X_fitted)             ##拟合值和预测值的对比???
###########预测未来期
forecast <- forecast.HoltWinters(X_fitted,h=5)
print(forecast)
predict <- data.frame(forecast)  ####取第二列预测???
print(predict[,1])
plot.forecast(incomeseriesforecast2)

#####################################################################################
####在职人数预测
X0 <- data$working_number
####???2014年在职人数进行预???(使用2006-2013年的数据)
predict<- gm(X0[1:8],2)
errorRate <- (predict-X0[9:10])/X0[9:10]*100
errorRate

####???2015年在职人数进行预???(使用2006-2014年的数据)
predict <- gm(X0[1:9],1)
errorRate <- (predict-X0[10])/X0[10]*100
errorRate

####对未???5年在职人数进行预???(使用2006-2015年的数据)
predict <- gm(X0[1:10],5)
predict

####指数平滑法
X0 <- data$working_number
X_fitted<- HoltWinters(X0[1:8],gamma=FALSE)
X_fitted
X_fitted$fitted[,1]
plot(X_fitted)             ##拟合值和预测值的对比???
write.table(X_fitted$fitted[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
###########预测未来期
forecast <- forecast.HoltWinters(X_fitted,h=2)
print(forecast)
predict <- data.frame(forecast)  ####取第二列预测???
print(predict[,1])
plot.forecast(incomeseriesforecast2)
write.table(predict[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)

X_fitted<- HoltWinters(X0[1:9],gamma=FALSE)
X_fitted
X_fitted$fitted[,1]
plot(X_fitted)             ##拟合值和预测值的对比???
write.table(X_fitted$fitted[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
###########预测未来期
forecast <- forecast.HoltWinters(X_fitted,h=1)
forecast
predict <- data.frame(forecast)  ####取第二列预测???
print(predict[,1])
plot.forecast(incomeseriesforecast2)
write.table(predict[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)


X_fitted<- HoltWinters(X0[1:10],gamma=FALSE)
X_fitted
X_fitted$fitted[,1]
plot(X_fitted)             ##拟合值和预测值的对比???
write.table(X_fitted$fitted[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
###########预测未来期
forecast <- forecast.HoltWinters(X_fitted,h=5)
print(forecast)
predict <- data.frame(forecast)  ####取第二列预测???
print(predict[,1])
plot.forecast(forecast)
write.table(predict[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
##########################################################################################
####离退人数预测
X0 <- data$retired_number
####???2014年离退人数进行预???(使用2006-2013年的数据)
predict<- gm(X0[1:8],2)
errorRate <- (predict-X0[9:10])/X0[9:10]*100
errorRate

####???2015年离退人数进行预???(使用2006-2014年的数据)
predict <- gm(X0[1:9],1)
errorRate <- (predict-X0[10])/X0[10]*100
errorRate

####对未???5年离退人数数进行预???(使用2006-2015年的数据)
predict <- gm(X0[1:10],5)
predict

####指数平滑法
X0 <- data$retired_number
X_fitted<- HoltWinters(X0[1:8],gamma=FALSE)
X_fitted
X_fitted$fitted[,1]
plot(X_fitted)             ##拟合值和预测值的对比???
write.table(X_fitted$fitted[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
###########预测未来期
forecast <- forecast.HoltWinters(X_fitted,h=2)
print(forecast)
predict <- data.frame(forecast)  ####取第二列预测???
print(predict[,1])
plot.forecast(incomeseriesforecast2)
write.table(predict[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)



X_fitted<- HoltWinters(X0[1:9],gamma=FALSE)
X_fitted
X_fitted$fitted[,1]
plot(X_fitted)             ##拟合值和预测值的对比???
write.table(X_fitted$fitted[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
###########预测未来期
forecast <- forecast.HoltWinters(X_fitted,h=1)
print(forecast)
predict <- data.frame(forecast)  ####取第二列预测???
print(predict[,1])
plot.forecast(incomeseriesforecast2)
write.table(predict[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)



X_fitted<- HoltWinters(X0[1:10],alpha=1,gamma=FALSE)
X_fitted
X_fitted$fitted[,1]
plot(X_fitted)             ##拟合值和预测值的对比???
write.table(X_fitted$fitted[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
###########预测未来期
forecast <- forecast.HoltWinters(X_fitted,h=5)
print(forecast)
predict <- data.frame(forecast)  ####取第二列预测???
print(predict[,1])
plot.forecast(incomeseriesforecast2)
write.table(predict[,1],file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)


