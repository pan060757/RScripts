#按月预测统筹账户收入以及支出情况
###gm(1,1)模型
setwd("E:/毕业设计/project/RScripts/")
source("gm11.R")
source("指数平滑法.R")
####ARIMA建模
library("forecast")
library("fUnitRoots")
income <- read.csv("E:\\毕业设计\\project\\mif_python\\收支预测模型\\output\\workerChargeBymonth.csv",header = FALSE)
colnames(income) <- c('month','totalIncome','groupIncome','singleIncome','number')
income
cost<- read.csv("E:\\毕业设计\\project\\mif_python\\收支预测模型\\output\\workerCostBymonth.csv",header = FALSE)
colnames(cost) <- c('month','totalFees','groupFees','h_fees','h_groupFees','m_fees','m_groupFees','h_count','m_count','avg_hFees','avg_mFees')
cost

dataseries <- ts(income$totalIncome,frequency=12)
plot.ts(dataseries)

dataseries <- ts(cost$totalFees)
plot.ts(dataseries)

month <- season(ts(income$totalIncome,frequency=12))
model <- lm(income$totalIncome~month)
summary(model)

month <- season(ts(cost$totalFees,frequency=12))
model <- lm(cost$totalFees~month)
summary(model)

