timestart<-Sys.time();
####���д��Ҫ���еĳ���(��������)
data <- read.csv("C:\\Users\\song\\Desktop\\cancer.txt",header = FALSE)
data
data <- data[,2:10]                   ####��ȡ2��10��
data
outlier.scores <- lof(data,k=56)  ####kΪ100ʱ��׼ȷ��Ϊ71%
outlier.scores
outlier<- order(outlier.scores,decreasing=T)
outlier
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
write.table(outlier,file="F:/lof_label1.txt",col.names=FALSE,row.names = FALSE,append=TRUE)


getwd()
setwd("F:\\������ҽ��������Ŀ\\����\\Apweb_demo\\ʵ������\\")
timestart<-Sys.time();
####���д��Ҫ���еĳ���
data <- read.csv("C:\\Users\\song\\Desktop\\cancer.txt",header = FALSE)
data <- data[,2:10]                   ####��ȡ2��10��
candidate <- read.csv("C:\\Users\\song\\Desktop\\candidate.txt",header = FALSE)
candidate <- candidate[,2:10]                   ####��ȡ2��10��
outlier.scores <- improve_lof(candidate,data,k=5)  ####kΪ100ʱ��׼ȷ��Ϊ71%
outlier<- order(outlier.scores,decreasing=T)
outlier
timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
write.table(outlier,file="F:/lof_label2.txt",col.names=FALSE,row.names = FALSE,append=TRUE)



timestart<-Sys.time();
####���д��Ҫ���еĳ���(��������)
data <- read.csv("C:\\Users\\song\\Desktop\\test.csv",header = FALSE)
data
data <- data[,9]                   ####��ȡ2��10��
data
outlier.scores <- lof(data,k=1)  ####kΪ100ʱ��׼ȷ��Ϊ71%
outlier.scores
outlier<- order(outlier.scores,decreasing=T)
outlier
write.table(outlier,file="F:/lof_label.txt",col.names=FALSE,row.names = FALSE,append=TRUE)
