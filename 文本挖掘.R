install.packages("tm")
install.packages("stemming")
install.packages("RWeka")
install.packages("Rwordseg")
raw_doc <- c("hello worls from tm")
corpus <- Corpus(VectorSource(raw_doc))


#apriori算法
install.packages("arules")
library(arules)
data <- paste("item1,item2","item1","item2,item3", sep="\n")
data
data("Adult")
Adult
rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target ="rules"))


#k-means算法，R在其默认载入的stats包中就包含了这个函数
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2), matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
cl <- kmeans(x, 2)
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex=2)
#代码第1行生成两组两维的正态分布的数据，第一组均值为0，第二组均值为1，两组数据方差都为0.3。第2行对该数据进行聚类，第3和第4行把聚类结果画出来。

#svm算法，需要你已经安装了包e1071
install.packages("e1071")
library(e1071)
data(iris)
x <- subset(iris, select = -Species)
x
y <- iris$Species
model <- svm(x, y)
summary(model)
pred <- predict(model, x)
table(pred, y)
 #第5行代码调用svm函数，计算由x作为特征y作为类别标签的分类器模型，第7行把模型应用于原数据进行预测。

------------------------------------------------------------------------------------------
#gbdt算法
install.packages("gbm")
install.packages("mlbench")
install.packages("caret")

library(gbm)
data(PimaIndiansDiabetes2,package='mlbench')
data <- PimaIndiansDiabetes2
head(data)
#将响应变量转为0-1格式
data$diabetes <- as.numeric(data$diabetes)
data <- transform(data,diabetes=diabetes-1)
data
# 使用gbm函数建模
model <- gbm(diabetes~.,data=data,shrinkage=0.01,
             distribution='bernoulli',cv.folds=5,
             n.trees=3000,verbose=F)
summary(model)    #可以看出各个变量的relative influence

#distribution :损失函数的形式，分类问题一般选择bernoulli分布，回归问题一般选gaussian分布
#:n.trees:迭代次数，在3000-10000之间
#shrinkage:学习速率，参数在0.01-0.001之间
#bag.fraction:再抽样比率
#interaction.depth:决策树的深度

# 用交叉检验确定最佳迭代次数
best.iter <- gbm.perf(model,method='cv')

# 观察各解释变量的重要程度
summary(model,best.iter)

# 变量的边际效应
plot.gbm(model,1,best.iter)       #其中的1表示的是第几个变量，如1代表的是pregnant变量

# 用caret包观察预测精度
install.packages("caret")
install.packages("survival")
library(ggplot2)
library(caret)
library(gbm)
library(plyr)
data <- PimaIndiansDiabetes2
fitControl <- trainControl(method = "cv", number = 5,returnResamp = "all")
model2 <- train(diabetes~., 
                data=data,
                method='gbm',
                distribution='bernoulli',
                trControl = fitControl,
                verbose=F,
                tuneGrid = data.frame(.n.trees=best.iter,.shrinkage=0.01,.interaction.depth=1,.n.minobsinnode = 10))
model2
