######利用Arima对于原始数据各列进行建模
install.packages("fUnitRoots")
getwd()
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
data <- read.table(("C:/Users/song/Desktop/generalData.txt"))
data
#####对平均工资进行建模
library(fUnitRoots)
wage <- ts(data$wage)
plot.ts(wage)
#一阶差分
wagediff1 <- diff(wage,differences=1)
adfTest(wagediff1)
plot.ts(wagediff1)
#二阶差分
wagediff2 <- diff(wage,differences =2)
plot.ts(wagediff2)
adfTest(wagediff2)   #2阶平稳（10个数据时）

#自相关函数
acf(wagediff2,lag.max=8)        #自相关图
acf(wagediff2,lag.max=8,plot=FALSE)        #自相关图
pacf(wagediff2,lag.max=8)       #偏自相关图
pacf(wagediff2,lag.max=8,plot=FALSE)
auto.arima(wage,trace="T")    ##(0,1,0)
###得到模型的参数
wagearima <- arima(wage,order=c(0,2,1))       ###(0,2,2)
wagearima
######对未来几年的数据进行预测
library("forecast")
wageforcast <- forecast.Arima(wagearima,h=5)
wageforcast
write.table(wageforcast[4],file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)

#####
cmp_worker <- ts(data$cmp.worker)
plot.ts(cmp_worker)
#一阶差分
cmp_workerdiff1 <- diff(cmp_worker,differences = 1)
adfTest(cmp_workerdiff1)
plot.ts(cmp_workerdiff1)
#二阶差分
cmp_workerdiff2 <- diff(cmp_worker,differences = 2)
plot.ts(cmp_workerdiff2)
adfTest(cmp_workerdiff2)   #2阶平稳(10个数据时)
#自相关函数
acf(cmp_workerdiff2,lag.max=7)        #自相关图
acf(cmp_workerdiff2,lag.max=7,plot=FALSE)        #自相关图
pacf(cmp_workerdiff2,lag.max=7)       #偏自相关图
pacf(cmp_workerdiff2,lag.max=7,plot=FALSE)
auto.arima(cmp_worker,trace="T")    ##(0,1,0)
###得到模型的参数
arima <- arima(cmp_worker,order=c(1,2,1))   ###(2,2,2)
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=5)
forcast
write.table(forcast[4],file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)

#####
cmp_retire <- ts(data$cmp.retire)
plot.ts(cmp_retire)
#一阶差分
cmp_retirediff1 <- diff(cmp_retire,differences = 1)
adfTest(cmp_retirediff1)
plot.ts(cmp_retirediff1)
#自相关函数
acf(cmp_retirediff1,lag.max=9)        #自相关图
acf(cmp_retirediff1,lag.max=9,plot=FALSE)        #自相关图
pacf(cmp_retirediff1,lag.max=9)       #偏自相关图
pacf(cmp_retirediff1,lag.max=9,plot=FALSE)
auto.arima(cmp_retire,ic="bic")    ##(0,1,0)
###得到模型的参数
arima <- arima(cmp_retire,order=c(2,1,3))   ####(0,1,3)
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=5)
forcast
write.table(forcast[4],file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)


#####
single_worker <- ts(data$single.worker)
plot.ts(single_worker)
#一阶差分
single_workerdiff1 <- diff(single_worker,differences = 1)
adfTest(single_workerdiff1)
plot.ts(single_workerdiff1)
#二阶差分
single_workerdiff2 <- diff(single_worker,differences = 2)
plot.ts(single_workerdiff2)
adfTest(single_workerdiff2)   #2阶平稳
#自相关函数
acf(single_workerdiff2,lag.max=8)        #自相关图
acf(single_workerdiff2,lag.max=8,plot=FALSE)        #自相关图
pacf(single_workerdiff2,lag.max=8)       #偏自相关图
pacf(single_workerdiff2,lag.max=8,plot=FALSE)
auto.arima(single_worker,ic="bic")    ##(1,1,0)
arima <- arima(single_worker,order=c(1,2,0))   ###(2,2,0)
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=5)
forcast
write.table(forcast[4],file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)


#####
single_retire <- ts(data$single.retire)
plot.ts(single_retire)
#一阶差分
single_retirediff1 <- diff(single_retire,differences = 1)
adfTest(single_retirediff1)
plot.ts(single_retirediff1)
##二阶差分
single_retirediff2 <- diff(single_retire,differences =2)
adfTest(single_retirediff2)
plot.ts(single_retirediff2)
##三阶差分
single_retirediff3 <- diff(single_retire,differences =3)
adfTest(single_retirediff3)
plot.ts(single_retirediff3)
#自相关函数
acf(single_retirediff3,lag.max=7)        #自相关图
acf(single_retirediff3,lag.max=7,plot=FALSE)        #自相关图
pacf(single_retirediff3,lag.max=7)       #偏自相关图
pacf(single_retirediff3,lag.max=7,plot=FALSE)
auto.arima(single_retire,ic="bic")    ##(0,1,0)
arima <- arima(single_retire,order=c(1,1,1))          ####(3,1,0)
arima 
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=5)
forcast
write.table(forcast[4],file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)


#####对当期收入进行建模
income <- ts(data$income.current)
plot.ts(income)
#一阶差分
incomediff1 <- diff(income,differences = 1)
adfTest(incomediff1)
plot.ts(incomediff1)
incomediff2 <- diff(income,differences = 2)
plot.ts(incomediff2)
adfTest(incomediff2)
incomediff3 <- diff(income,differences = 3)
plot.ts(incomediff3)
adfTest(incomediff3)          ####3阶平稳
#自相关函数
acf(incomediff3,lag.max=7)        #自相关图
acf(incomediff3,lag.max=7,plot=FALSE)        #自相关图
pacf(incomediff3,lag.max=7)       #偏自相关图
pacf(incomediff3,lag.max=7,plot=FALSE)
auto.arima(income,ic="bic")    ##(0,1,0)
arima <- arima(income,order=c(0,3,1))   ####(1,3,1)
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=5)
forcast
write.table(forcast[4],file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)




#####
menzhen_single <- ts(data$menzhen.single)
plot.ts(menzhen_single)
#一阶差分
menzhen_singlediff1 <- diff(menzhen_single,differences = 1)
adfTest(menzhen_singlediff1)
plot.ts(menzhen_singlediff1)
#二阶差分
menzhen_singlediff2 <- diff(menzhen_single,differences =2)
adfTest(menzhen_singlediff2)
plot.ts(menzhen_singlediff2)
#三阶差分
menzhen_singlediff3 <- diff(menzhen_single,differences =3)
adfTest(menzhen_singlediff3)
plot.ts(menzhen_singlediff3)
#自相关函数
acf(menzhen_singlediff3,lag.max=7)        #自相关图
acf(menzhen_singlediff3,lag.max=7,plot=FALSE)        #自相关图
pacf(menzhen_singlediff3,lag.max=7)       #偏自相关图
pacf(menzhen_singlediff3,lag.max=7,plot=FALSE)
auto.arima(menzhen_single,ic="bic")    ##(0,1,0)
arima <- arima(menzhen_single,order=c(1,3,0))      ###(1,3,1),CSS里有非平穩的AR部分
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=5)
forcast
write.table(forcast[4],file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)



#####
menzhen_group <- ts(data$menzhen.group)
plot.ts(menzhen_group)
#一阶差分
menzhen_groupdiff1 <- diff(menzhen_group,differences = 1)
adfTest(menzhen_groupdiff1)
plot.ts(menzhen_groupdiff1)
#二阶差分
menzhen_groupdiff2 <- diff(menzhen_group,differences =2)
adfTest(menzhen_groupdiff2)
plot.ts(menzhen_groupdiff2)
#自相关函数
acf(menzhen_groupdiff2,lag.max=8)        #自相关图
acf(menzhen_groupdiff2,lag.max=8,plot=FALSE)        #自相关图
pacf(menzhen_groupdiff2,lag.max=8)       #偏自相关图
pacf(menzhen_groupdiff2,lag.max=8,plot=FALSE)
auto.arima(menzhen_group,ic="bic")    ##(0,1,1)
arima <- arima(menzhen_group,order=c(0,2,1))   #####(0,2,4)
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=5)
forcast
write.table(forcast[4],file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)




#####
hospital.group <- ts(data$hospital.group)
plot.ts(hospital.group)
#一阶差分
hospital.groupdiff1 <- diff(hospital.group,differences = 1)
adfTest(hospital.groupdiff1)
plot.ts(hospital.groupdiff1)
#二阶差分
hospital.groupdiff2 <- diff(hospital.group,differences =2)
adfTest(hospital.groupdiff2)
plot.ts(hospital.groupdiff2)
#自相关函数
acf(hospital.groupdiff2,lag.max=8)        #自相关图
acf(hospital.groupdiff2,lag.max=8,plot=FALSE)        #自相关图
pacf(hospital.groupdiff2,lag.max=8)       #偏自相关图
pacf(hospital.groupdiff2,lag.max=8,plot=FALSE)
auto.arima(hospital.group,ic="bic")    ##(0,1,0)
arima <- arima(hospital.group,order=c(1,2,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=5)
forcast


####对医疗费用总支出进行建模
X0<- c(12769,19805,25829,35186,48708,60951,72716,90335,96118,116651)
cost <- ts(X0)
plot.ts(cost)
#一阶差分
costdiff1 <- diff(cost,differences = 1)
adfTest(costdiff1)
plot.ts(costdiff1)
#二阶差分
costdiff2<- diff(cost,differences =2)
adfTest(costdiff2)
plot.ts(costdiff2)
#三阶差分
costdiff3<- diff(cost,differences =3)
adfTest(costdiff3)
plot.ts(costdiff3)
#自相关函数
acf(costdiff3,lag.max=8)        #自相关图
acf(costdiff3,lag.max=8,plot=FALSE)        #自相关图
pacf(costdiff3,lag.max=8)       #偏自相关图
pacf(costdiff3,lag.max=8,plot=FALSE)
arima <- arima(cost,order=c(0,3,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=5)
forcast








#####对平均工资进行建模
library(fUnitRoots)
wage <- ts(data$wage[0:8])
plot.ts(wage)
#一阶差分
wagediff1 <- diff(wage,differences=1)
adfTest(wagediff1)
plot.ts(wagediff1)
#二阶差分
wagediff2 <- diff(wage,differences =2)
plot.ts(wagediff2)
adfTest(wagediff2)   
#自相关函数
acf(wagediff2,lag.max=8)        #自相关图
acf(wagediff2,lag.max=8,plot=FALSE)        #自相关图
pacf(wagediff2,lag.max=8)       #偏自相关图
pacf(wagediff2,lag.max=8,plot=FALSE)
auto.arima(wage,trace="T")    ##(0,1,0)
###得到模型的参数
wagearima <- arima(wage,order=c(1,2,0))
wagearima
######对未来几年的数据进行预测
library("forecast")
wageforcast <- forecast.Arima(wagearima,h=2)
wageforcast
predict <- wageforcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)


#####
cmp_worker <- ts(data$cmp.worker[0:8])
plot.ts(cmp_worker)
#一阶差分
cmp_workerdiff1 <- diff(cmp_worker,differences = 1)
adfTest(cmp_workerdiff1)
plot.ts(cmp_workerdiff1)
#自相关函数
acf(cmp_workerdiff1,lag.max=7)        #自相关图
acf(cmp_workerdiff1,lag.max=7,plot=FALSE)        #自相关图
pacf(cmp_workerdiff1,lag.max=7)       #偏自相关图
pacf(cmp_workerdiff1,lag.max=7,plot=FALSE)
auto.arima(cmp_worker,trace="T")    ##(0,1,0)
###得到模型的参数
arima <- arima(cmp_worker,order=c(1,1,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=2)
forcast
predict <- forcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)

#####
cmp_retire <- ts(data$cmp.retire[0:8])
plot.ts(cmp_retire)
#一阶差分
cmp_retirediff1 <- diff(cmp_retire,differences = 1)
adfTest(cmp_retirediff1)
plot.ts(cmp_retirediff1)
#自相关函数
acf(cmp_retirediff1,lag.max=9)        #自相关图
acf(cmp_retirediff1,lag.max=9,plot=FALSE)        #自相关图
pacf(cmp_retirediff1,lag.max=9)       #偏自相关图
pacf(cmp_retirediff1,lag.max=9,plot=FALSE)
auto.arima(cmp_retire,ic="bic")    ##(0,1,0)
###得到模型的参数
arima <- arima(cmp_retire,order=c(1,1,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=2)
forcast
predict <- forcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)


#####
single_worker <- ts(data$single.worker[0:8])
plot.ts(single_worker)
#一阶差分
single_workerdiff1 <- diff(single_worker,differences = 1)
adfTest(single_workerdiff1)
plot.ts(single_workerdiff1)
#二阶差分
single_workerdiff2 <- diff(single_worker,differences = 2)
plot.ts(single_workerdiff2)
adfTest(single_workerdiff2)   #2阶平稳
#自相关函数
acf(single_workerdiff2,lag.max=8)        #自相关图
acf(single_workerdiff2,lag.max=8,plot=FALSE)        #自相关图
pacf(single_workerdiff2,lag.max=8)       #偏自相关图
pacf(single_workerdiff2,lag.max=8,plot=FALSE)
auto.arima(single_worker,ic="bic")    ##(1,1,0)
arima <- arima(single_worker,order=c(1,2,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=2)
forcast
predict <- forcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)


#####
single_retire <- ts(data$single.retire[0:8])
plot.ts(single_retire)
#一阶差分
single_retirediff1 <- diff(single_retire,differences = 1)
adfTest(single_retirediff1)
plot.ts(single_retirediff1)
##二阶差分
single_retirediff2 <- diff(single_retire,differences =2)
adfTest(single_retirediff2)
plot.ts(single_retirediff2)
##三阶差分
single_retirediff3 <- diff(single_retire,differences =3)
adfTest(single_retirediff3)
plot.ts(single_retirediff3)
#自相关函数
acf(single_retirediff3,lag.max=7)        #自相关图
acf(single_retirediff3,lag.max=7,plot=FALSE)        #自相关图
pacf(single_retirediff3,lag.max=7)       #偏自相关图
pacf(single_retirediff3,lag.max=7,plot=FALSE)
auto.arima(single_retire,ic="bic")    ##(0,1,0)
arima <- arima(single_retire,order=c(1,3,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=2)
forcast
predict <- forcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)



#####对当期收入进行建模(8个数据时3阶无法平稳)
income <- ts(data$income.current[0:8])
plot.ts(income)
#一阶差分
incomediff1 <- diff(income,differences = 1)
adfTest(incomediff1)
plot.ts(incomediff1)
incomediff2 <- diff(income,differences = 2)
plot.ts(incomediff2)
adfTest(incomediff2)
incomediff3 <- diff(income,differences = 3)
plot.ts(incomediff3)
adfTest(incomediff3)          ####3阶平稳
####3阶平稳
#自相关函数
acf(incomediff3,lag.max=7)        #自相关图
acf(incomediff3,lag.max=7,plot=FALSE)        #自相关图
pacf(incomediff3,lag.max=7)       #偏自相关图
pacf(incomediff3,lag.max=7,plot=FALSE)
auto.arima(income,ic="bic")    ##(0,1,0)
arima <- arima(income,order=c(1,3,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=2)
forcast
predict <- forcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)





#####8个数据时无法达到平稳
menzhen_single <- ts(data$menzhen.single[0:8])
plot.ts(menzhen_single)
#一阶差分
menzhen_singlediff1 <- diff(menzhen_single,differences = 1)
adfTest(menzhen_singlediff1)
plot.ts(menzhen_singlediff1)
#二阶差分
menzhen_singlediff2 <- diff(menzhen_single,differences =2)
adfTest(menzhen_singlediff2)
plot.ts(menzhen_singlediff2)
#三阶差分
menzhen_singlediff3 <- diff(menzhen_single,differences =3)
adfTest(menzhen_singlediff3)
plot.ts(menzhen_singlediff3)
#自相关函数
acf(menzhen_singlediff3,lag.max=7)        #自相关图
acf(menzhen_singlediff3,lag.max=7,plot=FALSE)        #自相关图
pacf(menzhen_singlediff3,lag.max=7)       #偏自相关图
pacf(menzhen_singlediff3,lag.max=7,plot=FALSE)
auto.arima(menzhen_single,ic="bic")    ##(0,1,0)
arima <- arima(menzhen_single,order=c(3,3,0))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=2)
forcast
predict <- forcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)



#####
menzhen_group <- ts(data$menzhen.group[0:8])
plot.ts(menzhen_group)
#一阶差分
menzhen_groupdiff1 <- diff(menzhen_group,differences = 1)
adfTest(menzhen_groupdiff1)
plot.ts(menzhen_groupdiff1)
#二阶差分
menzhen_groupdiff2 <- diff(menzhen_group,differences =2)
adfTest(menzhen_groupdiff2)
plot.ts(menzhen_groupdiff2)
#三阶差分
menzhen_groupdiff3 <- diff(menzhen_group,differences =3)
adfTest(menzhen_groupdiff3)
plot.ts(menzhen_groupdiff3)
#自相关函数
acf(menzhen_groupdiff3,lag.max=8)        #自相关图
acf(menzhen_groupdiff3,lag.max=8,plot=FALSE)        #自相关图
pacf(menzhen_groupdiff3,lag.max=8)       #偏自相关图
pacf(menzhen_groupdiff3,lag.max=8,plot=FALSE)
auto.arima(menzhen_group,ic="bic")    ##(0,1,1)
arima <- arima(menzhen_group,order=c(1,3,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=2)
forcast
predict <- forcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)




#####
hospital.group <- ts(data$hospital.group[0:8])
plot.ts(hospital.group)
#一阶差分
hospital.groupdiff1 <- diff(hospital.group,differences = 1)
adfTest(hospital.groupdiff1)
plot.ts(hospital.groupdiff1)
#二阶差分
hospital.groupdiff2 <- diff(hospital.group,differences =2)
adfTest(hospital.groupdiff2)
plot.ts(hospital.groupdiff2)
#三阶差分
hospital.groupdiff3 <- diff(hospital.group,differences =3)
adfTest(hospital.groupdiff3)
plot.ts(hospital.groupdiff3)
#自相关函数
acf(hospital.groupdiff3,lag.max=8)        #自相关图
acf(hospital.groupdiff3,lag.max=8,plot=FALSE)        #自相关图
pacf(hospital.groupdiff3,lag.max=8)       #偏自相关图
pacf(hospital.groupdiff3,lag.max=8,plot=FALSE)
auto.arima(hospital.group,ic="bic")    ##(0,1,0)
arima <- arima(hospital.group,order=c(0,3,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=2)
forcast
predict <- forcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)


####对医疗费用总支出进行建模
X0<- c(12769,19805,25829,35186,48708,60951,72716,90335)
cost <- ts(X0)
plot.ts(cost)
#一阶差分
costdiff1 <- diff(cost,differences = 1)
adfTest(costdiff1)
plot.ts(costdiff1)
#二阶差分
costdiff2<- diff(cost,differences =2)
adfTest(costdiff2)
plot.ts(costdiff2)
#三阶差分
costdiff3<- diff(cost,differences =3)
adfTest(costdiff3)
plot.ts(costdiff3)
#自相关函数
acf(costdiff3,lag.max=8)        #自相关图
acf(costdiff3,lag.max=8,plot=FALSE)        #自相关图
pacf(costdiff3,lag.max=8)       #偏自相关图
pacf(costdiff3,lag.max=8,plot=FALSE)
arima <- arima(cost,order=c(0,3,1))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=2)
forcast
predict <- forcast[4]    ####单独提取预测值
write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)




####
data <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx\\local\\dataset\\income.txt")
data
X0 <- data$V1[0:35]
X0
cost <- ts(X0)
plot.ts(cost)
#一阶差分
costdiff1 <- diff(cost,differences = 1)
adfTest(costdiff1)
plot.ts(costdiff1)
#自相关函数
acf(costdiff1,lag.max=36)        #自相关图
acf(costdiff1,lag.max=36,plot=FALSE)        #自相关图
pacf(costdiff1,lag.max=36)       #偏自相关图
pacf(costdiff1,lag.max=36,plot=FALSE)
arima <- arima(cost,order=c(10,1,10))
arima
######对未来几年的数据进行预测
library("forecast")
forcast <- forecast.Arima(arima,h=12)
forcast





X0<-c(67921503.86,68015300.54,67935558.4,69861359.99,69927642.06,70036233.56,72886218.39,73494464.7,73805659.47,74073572.87,74343148.69,74194349.46)
X1 <- c(68584322,70380415,71355630,70936283,71679442,72131999,71436662,74412463,75043040,75376663,76922522,76685530)
ratio <- (X1-X0)/X0
ratio
X2 <- c(69269242,69929460,70589677,71249894,71910112,72570329,73230547,73890764,74550982,75211199,75871416,76531634)
ratio <- (X2-X0)/X0
ratio       





