#####################################################################
#####平均工资建模(gm11效果更好)
library("forecast")
source("E:/毕业设计/project/RScripts/gm11.R")
source("E:/毕业设计/project/RScripts/指数平滑法建模.R")
X0 <- c(13643,15406,18183,21350,23313,25649,27053,31340,37648,41121)
####对2014年平均工资进行预测(使用2006-2013年的数据)
predict<- gm(X0[1:8],1)
errorRate <- (predict-X0[9])/X0[9]*100
errorRate

####对2015年平均工资进行预测(使用2006-2014年的数据)
predict <- gm(X0[1:9],1)
errorRate <- (predict-X0[10])/X0[10]*100
errorRate

####对未来5年平均工资进行预测(使用2006-2015年的数据)
predict <- gm(X0[1:10],5)
predict

####指数平滑法
####对2014年平均工资进行预测(使用2006-2013年的数据)
predict <- Holt1(X0[1:8],1)
predict
errorRate <- (predict-X0[9])/X0[9]*100
errorRate

####对2015年平均工资进行预测(使用2006-2014年的数据)
predict <- Holt1(X0[1:9],1)
predict
errorRate <- (predict-X0[10])/X0[10]*100
errorRate

####对未来5年进行预测(使用2006-2015年的数据)
predict <- Holt1(X0[1:10],5)
predict


#########################################################################
############参保人数预测#################################################
number <- read.csv("E:/毕业设计/建模结果/charge_cost/working_retired.csv",header = FALSE)
colnames(number) <- c('year','number','working','working_number','retired','retired_number')  ###重命名
number$number
####指数平滑法
####对2014年平均工资进行预测(使用2006-2013年的数据)
predict <- Holt1(number$number[1:8],1)
predict
errorRate <- (predict-number$number[9])/number$number[9]*100
errorRate






