timestart<-Sys.time();
####这块写你要运行的程序(所有数据)
data <- read.csv("C:\\Users\\song\\Desktop\\cancer.txt",header = FALSE)
data
data <- data[,2:10]                   ####获取2到10列
data
outlier.scores <- lof(data,k=56)  ####k为100时，准确率为71%
outlier.scores
outlier<- order(outlier.scores,decreasing=T)
outlier
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
write.table(outlier,file="F:/lof_label1.txt",col.names=FALSE,row.names = FALSE,append=TRUE)


getwd()
setwd("F:\\泸州市医保基金项目\\会议\\Apweb_demo\\实验数据\\")
timestart<-Sys.time();
####这块写你要运行的程序
data <- read.csv("C:\\Users\\song\\Desktop\\cancer.txt",header = FALSE)
data <- data[,2:10]                   ####获取2到10列
candidate <- read.csv("C:\\Users\\song\\Desktop\\candidate.txt",header = FALSE)
candidate <- candidate[,2:10]                   ####获取2到10列
outlier.scores <- improve_lof(candidate,data,k=5)  ####k为100时，准确率为71%
outlier<- order(outlier.scores,decreasing=T)
outlier
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
write.table(outlier,file="F:/lof_label2.txt",col.names=FALSE,row.names = FALSE,append=TRUE)



timestart<-Sys.time();
####这块写你要运行的程序(所有数据)
data <- read.csv("C:\\Users\\song\\Desktop\\test.csv",header = FALSE)
data
data <- data[,9]                   ####获取2到10列
data
outlier.scores <- lof(data,k=1)  ####k为100时，准确率为71%
outlier.scores
outlier<- order(outlier.scores,decreasing=T)
outlier
write.table(outlier,file="F:/lof_label.txt",col.names=FALSE,row.names = FALSE,append=TRUE)

