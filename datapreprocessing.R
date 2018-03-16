library(rpart)

model<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
             data=iris,method="class",control=5,
             parms=list(prior=c(0.3,0.4,0.3), split = "information"))



plot(model,margin=0.2)
text(model,use.n=T,all=T,cex=0.9)

# Regression Tree Example
library(rpart)

# grow tree 
cu.summary
fit <- rpart(Mileage~Price + Country + Reliability + Type, 
             method="anova", data=cu.summary)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
     title = "Regression Tree for Mileage ")


library("party")#�������ݰ�
iris_ctree<-ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")



####
mif <- read.csv("C:\\Users\\song\\Desktop\\diabetesN.txt",header=TRUE,sep=",")
mif
formular=totalFees~type+grade+line+ratio+days+age+drugFees
fit=rpart(formular,method='anova',data=mif) 
fit
draw.tree(fit)
printcp(fit)  #####���ȹ۲�ģ�͵��������� 
plotcp(fit) #����CP��complexity parameter����xerror�����ͼ
pfit=prune(fit,cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])  #��prune�������ģ�ͽ����޼�
pfit
printcp(pfit)
draw.tree(pfit)
test <- read.csv("C:\\Users\\song\\Desktop\\testdata.csv",header=TRUE,sep=",")
test
ndata=data.frame(type=test$type,drugFees=test$drugFees,grade=test$grade,line=test$line,ratio=test$ratio,days=test$days,age=test$age) 
ndata
predictValue <- predict(fit,newdata=ndata) 
predictValue
errorRate=(predictValue-test$totalFees)/test$totalFees
errorRate
RMSE <- sqrt(sum((predictValue-test$totalFees)^2)/length(predictValue))
RMSE


#####���ɭ��ʵ��
library("randomForest")
formular=totalFees~type+grade+line+ratio+days+age+drugFees
randomForest=randomForest(formular,data=mif,importance=TRUE,na.action = na.omit) 
randomForest
importance(randomForest,type=1)  #��Ҫ������
importance(randomForest,type=2)  #Giniָ��
varImpPlot(randomForest)         #��Ҫ��������ӻ�
ndata=data.frame(type=test$type,grade=test$grade,drugFees=test$drugFees,line=test$line,ratio=test$ratio,days=test$days,age=test$age) 
ndata
predictValue <- predict(randomForest,newdata=ndata) 
predictValue
errorRate=(predictValue-test$totalFees)/test$totalFees
errorRate
RMSE <- sqrt(sum((predictValue-test$totalFees)^2)/length(predictValue))
RMSE

######gbdt�㷨
library(gbm)
formular=totalFees~type+grade+line+ratio+days+age+drugFees
gbmModel <- gbm(formular,data=mif,distribution='gaussian',cv.folds = 5,shrinkage=0.005,n.trees=3000)
gbmModel
ndata=data.frame(type=test$type,grade=test$grade,drugFees=test$drugFees,line=test$line,ratio=test$ratio,days=test$days,age=test$age) 
ndata
predictValue <- predict.gbm(gbmModel,newdata=ndata,n.trees=100) 
predictValue
errorRate <- (predictValue-test$totalFees)/test$totalFees
errorRate
RMSE <- sqrt(sum((predictValue-test$totalFees)^2)/length(predictValue))
RMSE


library("DMwR")
data <- read.csv("C:\\Users\\song\\Desktop\\NGSFees.txt",header = FALSE)
data
data1 <- data[,2:6]
data1
outlier.scores <- lofactor(data1,k=20)
plot(density(outlier.scores),col="green",main="the cost of different diseases", xlab="the score of the LOF",  ylab="Density",ylim=c(0,20))
outlier<- order(outlier.scores,decreasing=T)[1:5]
print (data1[outlier,])
legend("topright",legend=paste(rep(c("�Թ���","���Ĳ�","��Ѫѹ")),sep=", ")
       ,col=c('green','red','blue'), pch=1:3)


data <- read.csv("C:\\Users\\song\\Desktop\\GXBFees.txt",header = FALSE)
data
data1 <- data[,2:6]
data1
outlier.scores <- lofactor(data1,k=20)
lines(density(outlier.scores),col="red")

data <- read.csv("C:\\Users\\song\\Desktop\\YZJPTCFees.txt",header = FALSE)
data
data1 <- data[,2:6]
data1
outlier.scores <- lofactor(data1,k=20)
outlier<- order(outlier.scores,decreasing=T)[1:5]
print (data1[outlier,])
lines(density(outlier.scores),col="grey")

data <- read.csv("C:\\Users\\song\\Desktop\\GXYFees.txt",header = FALSE)
data
data1 <- data[,2:6]
data1
outlier.scores <- lofactor(data1,k=20)
lines(density(outlier.scores),col="blue")


##ͬһ�ȼ�ҽԺ����֧������Ա�
data <- read.csv("C:\\Users\\song\\Desktop\\grade1.txt",header = FALSE)
data
data1 <- data[,3]
data1
outlier.scores <- lofactor(data1,k=10)
plot(density(outlier.scores),col="green",main="cost of different hospitals", xlab="the score of the LOF",  ylab="Density",ylim=c(0,20))
outlier<- order(outlier.scores,decreasing=T)[1:5]
print (data[outlier,])
legend("topright",legend=paste(rep(c("һ��","����","����")),sep=", ")
       ,col=c('green','red','blue'), pch=1:3)


data <- read.csv("C:\\Users\\song\\Desktop\\grade2.txt",header = FALSE)
data
data1 <- data[,3]
data1
outlier.scores <- lofactor(data1,k=10)
lines(density(outlier.scores),col="red")


data <- read.csv("C:\\Users\\song\\Desktop\\grade3.txt",header = FALSE)
data
data1 <- data[,3]
data1
outlier.scores <- lofactor(data1,k=10)
lines(density(outlier.scores),col="blue")

