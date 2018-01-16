#
setwd("E:/毕业设计/建模结果/fpm/")
getwd()


library(arules)
###将一般的数据转换为事务类型的数据
drug <- read.transactions("E:/毕业设计/建模结果/fpm/freqDrugOfNGS.csv",format="basket",sep="\t",rm.duplicates=TRUE)
drug
summary(drug)
rules <- apriori(drug,parameter = list(support=0.001,confidence=0.5))
rules
inspect(head(sort(rules,by='lift'),3))



trement <- read.transactions("E:/毕业设计/建模结果/fpm/freqTrementOfNGS.csv",format="basket",sep="\t",rm.duplicates=TRUE)
trement
summary(trement)
rules <- apriori(trement,parameter = list(support=0.001,confidence=0.5))
rules
inspect(head(sort(rules,by='lift'),3))