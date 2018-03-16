######预测国王去世年龄
library("fUnitRoots")
setwd("E:/毕业设计/project/RScripts")
source('指数平滑法建模.R')
kings <-c(60,43,67,50,56,42,50,65,68,43,65,34,47,34,
          49,41,13,35,53,56,16,43,69,59,48,59,86,55,68,
          51,33,49,67,77,81,67,71,81,68,70,77,56)
kings
kingsseries <- ts(kings)
plot.ts(kingsseries)
install.packages("TTR")
library("TTR")
######指数平滑法
kingEMA <- EMA(kingsseries,n=5,ratio=0.3)
kingEMA
plot.ts(kingEMA)

#####K期移动平均法
kingSMA <- SMA(kingsseries,n=5)
kingSMA
plot.ts(kingSMA)



#########伦敦1813年到1912年全部的每年每英尺降雨量数据
rain <- c(23.56,26.07,21.86,31.24,23.65,23.88,
          26.41,22.67,31.69,23.86,24.11,32.43,
          23.26,22.57,23.00,27.88,25.32,25.08,
          27.76,19.82,24.78,20.12,24.34,27.42,
          19.44,21.63,27.49,19.43,31.13,23.09,
          25.85,22.65,22.75,26.36,17.70,29.81,
          22.93,19.22,20.63,35.34,25.89,18.65,
          23.06,22.21,22.18,18.77,28.21,32.24,
          22.27,27.57,21.59,16.93,29.48,31.60,
          26.25,23.40,25.42,21.32,25.02,33.86,
          22.67,18.82,28.44,26.16,28.17,34.08,
          33.82,30.28,27.92,27.14,24.40,20.35,
          26.64,27.01,19.21,27.74,23.85,21.23,
          28.15,22.61,19.80,27.94,21.47,23.52,
          22.86,17.69,22.54,23.28,22.17,20.84,
          38.10,20.65,22.97,24.26,23.01,23.67,
          26.75,25.36,24.79,27.88)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)
####从该图中可以看出整个曲线处于大致不变的水平，
#####且随机变动在整个时间序列范围内也可以认为是大致不变的，所以该序列可以大致被描述为一个相加模型

#####使用简单指数平滑法进行预测
rainseriesforecast <- HoltWinters(rainseries,beta=FALSE,gamma=FALSE)
rainseriesforecast
#####预测值放在rainseriesforecast$fitted中
plot(rainseriesforecast)      ####给出预测值和实际值
####可见我们预测的过于平滑，R提供了样本预测误差平方和（SSE）来衡量预测效果

######我们可以预测未来时期的值,我们对未来8年的下雨量进行预测
library("forecast")
rainseriesforecast2 <- forecast.HoltWinters(rainseriesforecast,h=2)
rainseriesforecast2
plot.forecast(rainseriesforecast2)         ######给出了预测区间

acf(rainseriesforecast2$residuals,lag.max=20)  #可以使用lag.max 指定我们想要看到的最大阶数

####观看上图可以发现自相关系数在第3期的时候达到了置信界限。
######为了验证在滞后1-20阶时非0自相关属性是否显著，可以借助R语言的（Box.test()）Ljung-Box检验。
Box.test(rainseriesforecast2$residuals,lag=20,type="Ljung-Box")

######验证预测误差是正太分布，并且均值为0，方差不变
plot.ts(rainseriesforecast2$residuals)
######观察上图，我们可以归纳出整个时间区间内预测误差属于大致不变的正态分布，且均值接近0




######非恒定水平即有增长或者降低趋势的，没有季节性可相加模型的时间序列预测算法
#####---霍尔特指数平滑法（Holt）
#####以1866到1911年每年女士裙子直径为案例
skirts <- c(608,617,625,636,657,691,728,784,816,876,949,1027,1047,1049,1018,
            1021,1012,1018,991,962,921,871,829,822,820,802,821,819,791,746,
            726,661,620,588,568,542,551,541,557,556,534,528,529,523,531)

skirtseries <- ts(skirts,start=c(1866))
plot.ts(skirtseries)
#####使用R中的HoltWinters()进行霍尔特指数平滑预测（gamma=FALSE）
skirtsseriesforecast<- HoltWinters(skirtseries, gamma=FALSE)
skirtsseriesforecast
plot(skirtsseriesforecast)
#####总体看来，预测的效果还不错（黑色为原始序列，红色为预测值）
skirtsseriesforecast<- HoltWinters(skirtseries, gamma=FALSE,l.start=608,b.start=9)
skirtsseriesforecast
plot(skirtsseriesforecast)
#####对未来2期的数据进行预测
skirtsseriesforecast2 <- forecast.HoltWinters(skirtsseriesforecast, h=2)
plot.forecast(skirtsseriesforecast2)




########Holt-Winters算法中提供了alpha、beta和gamma 来分别相应当前点的水平、趋势部分和季节部分。參数的去执法范围都是0-1之间，
########而且參数接近0时。最近的观測值的影响权重就越小。
#######以澳大利亚昆士兰州海滨纪念商品的月度销售
souvenir <- c(1664.81,2397.53,2840.71,3547.29,3752.96,3714.74,4349.61,
              3566.34,5021.82,6423.48,7600.60,19756.21,2499.81,5198.24,
              7225.14,4806.03,5900.88,4951.34,6179.12,4752.15,5496.43,
              5835.10,12600.08,28541.72,4717.02,5702.63,9957.58,5304.78,
              6492.43,6630.80,7349.62,8176.62,8573.17,9690.50,15151.84,
              34061.01,5921.10,5814.58,12421.25,6369.77,7609.12,7224.75,
              8121.22,7979.25,8093.06,8476.70,17914.66,30114.41,4826.64,
              6470.23,9638.77,8821.17,8722.37,10209.48,11276.55,12552.22,
              11637.39,13606.89,21822.11,45060.69,7615.03,9849.69,14558.40,
              11587.33,9332.56,13082.09,16732.78,19888.61,23933.38,25391.35,
              36024.80,80721.71,10243.24,11266.88,21826.84,17357.33,15997.79,
              18601.53,26155.15,28586.52,30505.41,30821.33,46634.38,104660.67)
souvenirseries <- ts(souvenir,frequency=12 )
logsouvenirseries <- log(souvenirseries)
plot.ts(logsouvenirseries)
souvenirtimeseriesforecast <- HoltWinters(logsouvenirseries)
souvenirtimeseriesforecast
plot(souvenirtimeseriesforecast)
###########预测未来期的值
souvenirtimeseriesforecast2 <- forecast.HoltWinters(souvenirtimeseriesforecast, h=2)
plot.forecast(souvenirtimeseriesforecast2)









########Holt-Winters算法中提供了alpha、beta和gamma 来分别相应当前点的水平、趋势部分和季节部分。參数的去执法范围都是0-1之间，
########而且參数接近0时。最近的观測值的影响权重就越小。
#######以澳大利亚昆士兰州海滨纪念商品的月度销售
xtime <- c(1664.81,2397.53,2840.71,3547.29,3752.96,3714.74,4349.61,
              3566.34,5021.82,6423.48,7600.60,19756.21,2499.81,5198.24,
              7225.14,4806.03,5900.88,4951.34,6179.12,4752.15,5496.43,
              5835.10,12600.08,28541.72,4717.02,5702.63,9957.58,5304.78,
              6492.43,6630.80,7349.62,8176.62,8573.17,9690.50,15151.84,
              34061.01,5921.10,5814.58,12421.25,6369.77,7609.12,7224.75,
              8121.22,7979.25,8093.06,8476.70,17914.66,30114.41,4826.64,
              6470.23,9638.77,8821.17,8722.37,10209.48,11276.55,12552.22,
              11637.39,13606.89,21822.11,45060.69,7615.03,9849.69,14558.40,
              11587.33,9332.56,13082.09,16732.78,19888.61,23933.38,25391.35,
              36024.80,80721.71,10243.24,11266.88,21826.84,17357.33,15997.79,
              18601.53,26155.15,28586.52,30505.41,30821.33,46634.38,104660.67)
xtimeseries <- ts(xtime,frequency = 12)
#取对数可以减少极值带来的影响，消除方差不齐。
logxtimeseries<-log(xtimeseries)
xtimeseriesforecasts<-HoltWinters(logxtimeseries)
xtimeseriesforecasts
#用黑线画出原始数据的时间曲线图，用红线在上面画出预测值的时间曲线图：
plot(logxtimeseries)
plot(xtimeseriesforecasts)
xtimeseriesforecasts2<-forecast.HoltWinters(xtimeseriesforecasts,h=19)
plot.forecast(xtimeseriesforecasts2)
acf(xtimeseriesforecasts2$residuals,lag.max=20)
Box.test(xtimeseriesforecasts2$residuals,lag=20,type="Ljung-Box")

########检测误差是否服从正态分布
plot.ts(xtimeseriesforecasts2$residuals) # make a time plot
plotForecastErrors(xtimeseriesforecasts2$residuals) # make a histogram



xseriesdiff1<-diff(logxtimeseries,differences=1)
plot.ts(xseriesdiff1)
adfTest(xseriesdiff1)
acf(xseriesdiff1,lag.max=20) #plot a correlogram
acf(xseriesdiff1,lag.max=20,plot=FALSE) # get the autocorrelation values
pacf(xseriesdiff1,lag.max=20) #plot a partial correlogram
auto.arima(logxtimeseries,trace=T)   #####模型自动选择

xtimeseriesarima <- arima(logxtimeseries,order=c(2,0,0),seasonal=list(order=c(0,1,1),period=12),method="ML")
summary(xtimeseriesarima)
tsdiag(xtimeseriesarima)
xtimeseriesforecasts<-forecast.Arima(xtimeseriesarima,h=24,level=c(99.5))

xtimeseriesforecasts

plot.forecast(xtimeseriesforecasts)


######误差检验
acf(xtimeseriesforecasts$residuals,lag.max=20)
Box.test(xtimeseriesforecasts$residuals,lag=20,type="Ljung-Box") 
######残差qq图
qqnorm(xtimeseriesforecasts $residuals)
qqline(xtimeseriesforecasts $residuals)


########用2006-2013年当期收入数据来进行拟合
income <- c(28138,32647,43342,49445,57940,67845,71924,114512)
incomeseries <- ts(income,start=c(2006))
plot.ts(incomeseries)
incomeseriesforecast<- HoltWinters(incomeseries,gamma=FALSE)
incomeseriesforecast$fitted
plot(incomeseriesforecast)             ##拟合值和预测值的对比图

###########预测未来期的值
incomeseriesforecast2 <- forecast.HoltWinters(incomeseriesforecast,h=2)
incomeseriesforecast2$fitted
plot.forecast(incomeseriesforecast2)



########用2006-2013年当期费用支出数据来进行拟合
cost<- c(12679,19805,25829,35186,48708,60951,72716,90335)
costseries <- ts(cost,start=c(2006))
plot.ts(costseries)
costseriesforecast<- HoltWinters(costseries,gamma=FALSE)
costseriesforecast
plot(costseriesforecast)             ##拟合值和预测值的对比图
costseriesforecast$fitted[,1]         ####访问第一列

###########预测未来期的值
costseriesforecast2<- forecast.HoltWinters(costseriesforecast,h=2)
costseriesforecast2
plot.forecast(costseriesforecast2)





########对2006-2015年各项指标进行预测
getwd()
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
source("指数平滑法建模.R")
data <- read.table(("C:/Users/song/Desktop/generalData.txt"))
data
####对于平均工资建立gm11模型如下
X0 <- data$wage
Holt(X0,5)

####对单位在职参保人数进行建模
X0 <- data$cmp.worker
Holt(X0,5)

####对单位退休参保人数进行建模
X0 <- data$cmp.retire
Holt(X0,5)

####对个体在职参保人数进行建模
X0 <- data$single.worker
Holt(X0,5)

####对个体退休参保人数进行建模
X0 <- data$single.retire          ######并不满足光滑性检验
Holt(X0,5)

####对当期收入进行建模
X0 <- data$income.current         
Holt(X0,5)

####对个人账户进行建模 
X0 <- data$menzhen.single         #####(当大于4时开始满足平滑性检验)
Holt(X0,5)

####对门诊统筹支付进行建模 
X0 <- data$menzhen.group         #####(当大于4时开始满足平滑性检验)
Holt(X0,5)

####对住院统筹支付进行建模 
X0 <- data$hospital.group         #####(当大于4时开始满足平滑性检验)
Holt(X0,5)

####对医疗费用总支出进行建模
X0 <- c(12769,19805,25829,35186,48708,60951,72716,90335,96118,116651)
Holt(X0,5)       



source("指数平滑法建模.R")
data <- read.table(("C:/Users/song/Desktop/generalData.txt"))
data
####对于平均工资建立gm11模型如下
X0 <- data$wage[0:8]
Holt(X0,2)


####对单位在职参保人数进行建模
X0 <- data$cmp.worker[0:8]
Holt(X0,2)

####对单位退休参保人数进行建模
X0 <- data$cmp.retire[0:8]
Holt(X0,2)

####对个体在职参保人数进行建模
X0 <- data$single.worker[0:8]
Holt(X0,2)

####对个体退休参保人数进行建模
X0 <- data$single.retire[0:8]          ######并不满足光滑性检验
Holt(X0,2)

####对当期收入进行建模
X0 <- data$income.current[0:8]         
Holt(X0,2)

####对个人账户进行建模 
X0 <- data$menzhen.single[0:8]        #####(当大于4时开始满足平滑性检验)
Holt(X0,2)

####对门诊统筹支付进行建模 
X0 <- data$menzhen.group[0:8]         #####(当大于4时开始满足平滑性检验)
Holt(X0,2)  ####效果比较差

####对住院统筹支付进行建模 
X0 <- data$hospital.group[0:8]         #####(当大于4时开始满足平滑性检验)
Holt(X0,2)

####对医疗费用总支出进行建模
X0 <- c(12769,19805,25829,35186,48708,60951,72716,90335)
Holt(X0,2)  



####
source("指数平滑法建模.R")
data <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx\\local\\dataset\\income.txt")
data
X0 <- data$V1[0:35]
X0
Holt1(X0,12)


#####指数平滑法
source("指数平滑法建模.R")
X0 <- c(127023573.24,155063561.62,195288199.21,243807352.12,276653981.62,361953989.94,660724259.14,738122452.74)
Holt(X0,5) 


X0 <- c(114762910.66,154414299.63,175802278.55,225978270.97,318585403.79,304577844.94,335881349.70,383704670.13)
Holt(X0,5)

X0 <- c(241786483.90,309477861.25,371090477.76,469785623.09,595239385.41,666531834.88,996605608.84,1121827516.42)
Holt(X0,5)


getwd()
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
source("指数平滑法建模.R")
####总的参保人数
X0 <- c(47561,65932,90523,103395,120223,216555,249252,267985,296255,338161,363992,388116,423138,446656)
Holt(X0,5)

####在职参保人数
X0 <- c(32961,45278,58879,68175,80070,140421,161375,172469,194148,222644,236792,256941,287454,302809,305966)
Holt(X0,5)


####退休参保人数
X0 <- c(14600,20654,31644,35220,40153,76134,87877,95516,102107,115517,127200,131175,135684,143847)
Holt(X0,5)



####二级甲类费用
X0<- c(610334.04,1247941.03,2942431.91,4488150.2,11863593.58,14536871.17,17425320.2,19206800.02)
Holt(X0,5)

####二级乙类费用
X0<- c(6767752,8429849.1,10646146.8,11412668.71,8508299.3,10524780.6,15380410.1,19306617.3)
Holt(X0,5)

####
X0 <- c(2768,3299,4157,4555,5085,5673,6697,7524)
Holt(X0,5)


####三级甲类费用
X0 <- c(2206975.03,3357867.36,17308658.72,24841468.62,33585463.26,40207372.2)
Holt(X0,5)

####三级费用
X0 <- c(24814300.8,30543772.1,27017452.2,38455074.5,59073404.37,72040663.78)
Holt(X0,5)

X0 <- c(3728,4172,4863,6250,8661,9583)
Holt(X0,5)

getwd()
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
source("指数平滑法建模.R")
library("TTR")
library('forecast')
fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample20.txt",header=TRUE)
fees
X0 <- fees$jlfy
X0
Holt(X0,5)

###乙类费用
X0 <- fees$ylfy
X0
Holt(X0,5)

###住院人次
X0 <- fees$zyrc[0:12]
X0
Holt(X0,5)

####甲类药品费
X0 <- fees$jlypf[0:12]
X0
Holt(X0,5)

####乙类药品费
X0 <- fees$ylypf[0:12]
X0
Holt(X0,5)

###计算一下误差率(600,0.85)
errorrate <- ((fees$jlfy+fees$ylfy*0.9-fees$zyrc*600)*0.85-fees$tczhzf)/fees$tczhzf
errorrate




####(3,0)
fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample30.txt",header=TRUE)
fees
X0 <- fees$jlfy
X0
Holt(X0,5)

###乙类费用
X0 <- fees$ylfy
X0
Holt(X0,5)

###住院人次
X0 <- fees$zyrc[0:12]
X0
Holt(X0,5)

####甲类药品费
X0 <- fees$jlypf[0:12]
X0
Holt(X0,5)


####乙类药品费
X0 <- fees$ylypf[0:12]
X0
Holt(X0,5)

###计算一下误差率(700,0.8)
errorrate <- ((fees$jlfy+fees$ylfy*0.9-fees$zyrc*700)*0.8-fees$tczhzf)/fees$tczhzf
errorrate



fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample21.txt",header=TRUE)
fees
X0 <- fees$jlfy
X0
Holt(X0,5)

###乙类费用
X0 <- fees$ylfy
X0
Holt(X0,5)

###住院人次
X0 <- fees$zyrc[0:12]
X0
Holt(X0,5)

####甲类药品费
X0 <- fees$jlypf[0:12]
X0
Holt(X0,5)


####乙类药品费
X0 <- fees$ylypf[0:12]
X0
Holt(X0,5)

###计算一下误差率(500,0.92)
errorrate <- ((fees$jlfy+fees$ylfy*0.9-fees$zyrc*500)*0.92-fees$tczhzf)/fees$tczhzf
errorrate



fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample31.txt",header=TRUE)
fees
X0 <- fees$jlfy
X0
Holt(X0,5)

###乙类费用
X0 <- fees$ylfy
X0
Holt(X0,5)

###住院人次
X0 <- fees$zyrc[0:12]
X0
Holt(X0,5)

####甲类药品费
X0 <- fees$jlypf[0:12]
X0
Holt(X0,5)


####乙类药品费
X0 <- fees$ylypf[0:12]
X0
Holt(X0,5)

###计算一下误差率(600,0.88)
errorrate <- ((fees$jlfy+fees$ylfy*0.9-fees$zyrc*600)*0.88-fees$tczhzf)/fees$tczhzf
errorrate


####指数平滑法计算在职和离退人数
number <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\ageModel\\stateCount.txt",header=TRUE)
number
####
X0 <-number$zzrs[0:14]
X0
Holt(X0,5)
####
X0 <-number$ltrs[0:17]
X0
Holt(X0,5)



#####平均工资预测
X0 <- c(13643,15406,18183,21350,23313,25649,27053,31340,37648,41121)
X0
Holt(X0,3)

#####参保人数预测
X0 <- c(248936,267714,295811,337634,361136,387794,421040,445978,456318,459241)
X0
Holt(X0,3)

#####
x0 <- c(13878,15504.96,17526,19503)
x0
Holt(x0,3)

####平均工资预测
######读入Totaltrend数据
totalTrend<- read.csv("G:\\研二\\泸州市医保基金项目\\下\\建模结果\\totalTrend.txt",sep="\t")
colnames(totalTrend) <- c("year", "avgWage", "floor","ceil","zzrs","txrs","total","ratio","income","cost","group")
totalTrend



##########1949-1960年每个月国际航班的乘客量。
library(forecast)
library(xts)
data("AirPassengers")
ts <- as.xts(AirPassengers)
plot(ts, main = "Average Monthly Passengers from 1949 to 1960")

training <- ts[1:130]
testing <- ts[131:144]

fit1 <- ets(training, model = "AAN", damped = FALSE)
fit2 <- ets(training, model = "ZZZ", damped = NULL)

summary(fit1)

summary(fit2)


pred1 <- forecast(fit1, h = length(testing))
pred2 <- forecast(fit2, h = length(testing))

perf1 <- accuracy(f = as.numeric(pred1$mean), x = as.numeric(testing))
perf2 <- accuracy(f = as.numeric(pred2$mean), x = as.numeric(testing))

results <- rbind(perf1, perf2)
rownames(results) <- c("model1", "model2")
results




#############拟合模型
#拟合模型
fit <- ets(nhtemp, model = "ANN")
fit
forecast(fit,1)
plot(forecast(fit, 1), xlab="Year",ylab=expression(paste("Temperature (", degree*F,")")),
      main="New Haven Annual Mean Temperature")
#得到准确性度量
accuracy(fit)
#三指数模型拟合
fit <- ets(log(AirPassengers),model = "AAA")
fit
#查看准确性度量
accuracy(fit)
#未来值预测
pred <- forecast(fit,5)
#预测值查看
pred

#图形绘制
plot(pred, main="Forecast for Air Travel",ylab="Log(AirPassengers)", xlab="Time")
#用原始值预测
pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
p <- cbind(pred$mean,pred$lower,pred$upper)
dimnames(p)[[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
p

##########自动选取最优模型
fit <- ets(JohnsonJohnson)
fit
#绘制原数据
plot(Nile)
#确定差分次数
ndiffs(Nile)

plot(forecast(fit), main="Johnson & Johnson Forecasts",
       ylab="Quarterly Earnings (Dollars)", xlab="Time", flty=2)


#拟合模型
fit <- arima(Nile, order = c(0,1,1))
fit

#查看拟合准确性
accuracy(fit)

#绘制Q-Q正态图
qqnorm(fit$residuals)
#拟合Q-Q正太线
qqline(fit$residuals)
#Box.test检测
Box.test(fit$residuals, type = "Ljung-Box")

library(forecast)
#自动拟合模型
fit <- auto.arima(sunspots,trace=T)
#模型查看
fit

