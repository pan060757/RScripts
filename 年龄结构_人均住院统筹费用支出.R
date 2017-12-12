#年龄结构对人均住院统筹费用支出的影响
data <- read.csv("E:/毕业设计/建模结果/age_AvghgroupFees.csv")
data
###(年份,所属年龄组,人均住院统筹费用支出,参保人数)
colnames(data) <- c("year","age_group","avg_hgroupFees","number","hgroupFees")      ##列重命名
data
####不考虑人口年龄结构时，风险因素
year_fees <- tapply(data$avg_hgroupFees,data$year,sum)    ####按照年份进行统计
###考虑人口年龄结构时
age_fees <- tapply(data$hgroupFees,data$year,sum)    ####按照年份进行统计
result1 <- c()
result2<- c()
for(i in 1:nrow(data)){            ###遍历每一行
   print(data[i,]$year)            ####对应年份
   print(data[i,]$age_group)       ####对应年龄组
   ####不考虑年龄结构风险系数计算
   avg_yearFees <- data[i,]$avg_hgroupFees         ####该年,各个年龄组对应的人均统筹费用支出
   all_yearFees <- year_fees[as.character(data[i,]$year)]  ###该年，总共人均统筹费用支出
   year_risk <- avg_yearFees/all_yearFees     ####不考虑年龄结构风险系数
   result1 <- c(result1,year_risk)
   cat("不考虑年龄结构各年龄段风险系数：",year_risk,"\n")
   ####考虑年龄结构风险系数计算
   avg_ageFees <- data[i,]$hgroupFees      ####该年,各个年龄组对应的人均统筹费用支出
   all_ageFees <- age_fees[as.character(data[i,]$year)]  ###该年，总共人均统筹费用支出
   age_risk <-avg_ageFees /all_ageFees  ####考虑年龄结构风险系数
   result2 <- c(result2,age_risk)
   cat("考虑人口结构各年龄段风险系数：",age_risk,"\n")
}
result1
result2

