Holt <- function(data,nums)
{
  dataseries <- ts(data,start=c(2006))
  plot.ts(dataseries)
  forecast<- HoltWinters(dataseries,beta=FALSE,gamma=FALSE)
  plot(forecast)             ##拟合值和预测值的对比图
  fit <- forecast$fitted[,1]  ####取第一列拟合值
  print(fit)
  write.table(fit,file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
  ratio <- (data[3:length(data)]-forecast$fitted[,1])/data[3:length(data)]*100
  print(ratio)
  write.table(ratio,file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
  ###########预测未来5期的值
  forecast2<- forecast.HoltWinters(forecast,h=nums)
  print(forecast2)
  predict <- data.frame(forecast2)  ####取第二列预测值
  print(predict[,1])
  write.table(predict,file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
  plot.forecast(forecast2)
}


Holt1 <- function(data,nums)
{
  library("forecast")
  dataseries <- ts(data)
  plot.ts(dataseries)
  forecast<- HoltWinters(dataseries,beta=FALSE,gamma=FALSE)
  plot(forecast)             ##拟合值和预测值的对比图
  fit <- forecast$fitted[,1]  ####取第一列拟合值
  print(fit)
  write.table(fit,file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
  ratio <- (data[3:length(data)]-forecast$fitted[,1])/data[3:length(data)]*100
  print(ratio)
  write.table(ratio,file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
  ###########预测未来5期的值
  forecast2<- predict.HoltWinters(forecast,h=nums)
  print(forecast2)
  predict <- data.frame(forecast2)  ####取第二列预测值
  print(predict[,1])
  return (predict[,1])
  write.table(predict,file="E:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
  plot.forecast(forecast2)
}

plotForecastErrors <- function(forecasterrors)
  
{
  
  # make a red histogram of the forecast errors:
  
  mysd <- sd(forecasterrors)
  
  hist(forecasterrors, col='red', freq=FALSE)
  
  # freq=FALSE ensures the area under the histogram = 1
  
  # generate normally distributed data with mean 0 and standard deviation mysd
  
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  
  myhist <- hist(mynorm, plot=FALSE)
  
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  
  points(myhist$mids, myhist$density, type='l', col='blue', lwd=2)
  
}



