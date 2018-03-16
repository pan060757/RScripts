install.packages("arules")
library(arules)
data("Groceries")
Groceries
summary("Groceries")
rules <- apriori(Groceries,parameter = list(support=0.001,confidence=0.5))
rules
inspect(head(sort(rules,by='lift'),3))

#####仅有2015年的数据
data <- read.csv("C:\\Users\\song\\Desktop\\fpm_data.txt",sep=",",header=TRUE)
colnames(data) <- c("number",  "identity","dw","age","sex","income","grade","disease")
data
options(max.print = 1000)
rules <- apriori(data[,3:7],parameter = list(support=0.001,confidence=0.6))
rules
inspect(head(sort(rules,by='lift'),1000))
write.table(inspect(sort(rules,by='lift')),file="F:/test.txt",col.names=FALSE,row.names = FALSE,append=TRUE)

####所有的数据
data <- read.csv("C:\\Users\\song\\Desktop\\fpm_all.txt",sep=",",header=TRUE)
colnames(data) <- c("number",  "identity","dw","age","sex","income","grade","days","month","disease","risk")
data
rules <- apriori(data[,3:11],parameter = list(support=0.001,confidence=0.8))
rules
inspect(head(sort(rules,by='lift'),1000))
write.table(inspect(sort(rules,by='lift')),file="F:/test.txt",col.names=FALSE,row.names = FALSE,append=TRUE)
orderRules <- sort(rules,by='lift')
######查看指定的规则
rules <- subset(orderRules, rhs %pin% c("grade="))
rules
inspect(rules)
write.table(inspect(rules),file="F:/grade.txt",col.names=FALSE,row.names = FALSE,append=TRUE)

#####
rules <- subset(orderRules, lhs %pin% c("disease=")& lift>3)
rules
inspect(rules)
write.table(inspect(rules),file="F:/disease.txt",col.names=FALSE,row.names = FALSE,append=TRUE)


library(arules)
###将一般的数据转换为事务类型的数据
test <- read.transactions("C:/Users/songsong/Desktop/test.txt",format="basket",sep=",",rm.duplicates=TRUE)
test
summary(test)
rules <- apriori(test,parameter = list(support=0.001,confidence=0.5))
rules
inspect(head(sort(rules,by='lift'),3))





