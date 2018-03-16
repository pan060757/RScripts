####医疗项目数据分析
age <- read.csv("G:\\研二\\泸州市医保基金项目\\spark运行结果\\ageModel\\ageDivided310.txt")
age
barplot(data.matrix(age), beside=TRUE,col=c('lightblue','mistyrose','lightcyan','lavender','cornsilk'),
      ylim=c(0,100))


x <-c(3293584.68,4006475.98,4892159.47,5953233.42,7182093.02,8813695.09,13178963.22,16438881.02,19354772.76,23701028.49,20208210.75)
y <- 1:11
plot(y,x)


####药品费用前10的药品名
color <- c('lightcoral','lightblue','lawngreen','mistyrose','orchid4','lightcyan','lavender','cornsilk','snow','steelblue')
fees <- c(7379637.26,5262819.61,5154149.73,5037114.07,4902152.2,3459960.87,3356849.13,3251523.59,3008248.77,2967488.09)
barplot(data.matrix(fees), beside=TRUE,col=color,
        legend=c('丹红注射液','注射用红花黄色素','脑苷肌肽注射液','前列地尔注射液','注射用血栓通(冻干)','氯化钠注射液','注射用左卡尼汀','注射用骨肽','注射用奥美拉唑钠','注射用血塞通(冻干)'))



####不同等级医院费用变化情况
sample310 <- read.csv("G:\\研二\\泸州市医保基金项目\\spark运行结果\\data_new\\Medical\\sample310.txt")
sample <-as.matrix(sample310)
sample
colnames(sample310)=c('无等级','一级','二级','三级','社区')             ####修改列名
labels <- c('2010','2011','2012','2013','2014','2015')
legs <- c('无等级','一级','二级','三级','社区')   
cols <- c('lightcoral','lightblue','lawngreen','mistyrose','orchid4','lightcyan')
barplot(sample, offset = 0, axis.lty = 1, names.arg = legs, col = cols, beside = TRUE)  
legend("topright", fill = cols, box.col = "transparent",legend=labels) 


####不同等级医院费用变化情况
sample390 <- read.csv("G:\\研二\\泸州市医保基金项目\\spark运行结果\\data_new\\Medical\\sample390.txt")
sample <-as.matrix(sample390)
sample
colnames(sample390)=c('无等级','一级','二级','三级','社区')             ####修改列名
labels <- c('2010','2011','2012','2013','2014','2015')
legs <- c('无等级','一级','二级','三级','社区')   
cols <- c('lightcoral','lightblue','lawngreen','mistyrose','orchid4','lightcyan')
barplot(sample, offset = 0, axis.lty = 1, names.arg = legs, col = cols, beside = TRUE)  
legend("topright", fill = cols, box.col = "transparent",legend=labels) 



####不同等级医院费用变化情况
sample <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Medical_Institution_Payment_Model\\differentlevel.txt",header = TRUE)
sample <-as.matrix(sample)
sample
colnames(sample)=c('三级','二级','一级','无等级','社区')             ####修改列名
labels <- c('2010','2011','2012','2013','2014','2015')
legs <- c('三级','二级','一级','无等级','社区')   
cols <- c('lightcoral','lightblue','lawngreen','mistyrose','orchid4','lightcyan')
barplot(sample, offset = 0, axis.lty = 1, names.arg = legs, col = cols, beside = TRUE)  
legend("topright", fill = cols, box.col = "transparent",legend=labels) 





#####相同医院等级费用分布情况
same310 <- read.csv("G:\\研二\\泸州市医保基金项目\\spark运行结果\\data_new\\Medical\\samelevel310.txt")
summary(same310)
plot(density(same310$fees))
m <- seq(1217,10098078,100000)
table(cut(same310$fees,m))
barplot(table(cut(same310$fees,m)),col=c('lightgreen'))#作图
title(xlab = "fees", ylab = "frequency")


same390 <- read.csv("G:\\研二\\泸州市医保基金项目\\spark运行结果\\data_new\\Medical\\samelevel390.txt")
summary(same390)
plot(density(same390$fees))
m <- seq(8793,41448126,1000000)
table(cut(same390$fees,m))
barplot(table(cut(same390$fees,m)),col=c('lightgreen'))#作图

####二级
qfx1 <- 600
qfx2 <- 500
ratio1<- 0.85
ratio2 <- 0.92
fees<- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\test1.txt",header=TRUE)
fees2 <- (fees$ejzzjlfy+fees$ejzzylfy*0.9-qfx1*fees$zyrc20)*ratio1+
  (fees$ejtxjlfy+fees$ejtxylfy*0.9-qfx2*fees$zyrc21)*ratio2
fees2


####三级
qfx1 <- 800
qfx2 <- 600
ratio1<- 0.8
ratio2 <- 0.88
fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\test2.txt",header=TRUE)
fees3 <- (fees$sjzzjlfy+fees$sjzzylfy*0.9-qfx1*fees$zyrc30)*ratio1+
  (fees$sjtxjlfy+fees$sjtxylfy*0.9-qfx2*fees$zyrc31)*ratio2
fees3

####社区医院
qfx1 <- 500
qfx2 <- 400
ratio1<- 0.9
ratio2 <- 0.96
fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\test3.txt",header=TRUE)
fees
fees4 <- (fees$sqzzjlfy+fees$sqzzylfy*0.9-qfx1*fees$zyrc40)*ratio1+
  (fees$sqtxjlfy+fees$sqtxylfy*0.9-qfx2*fees$zyrc41)*ratio2
fees4

####一级
qfx1 <- 500
qfx2 <- 400
ratio1<- 0.9
ratio2 <- 0.96
fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\test4.txt",header=TRUE)
fees
fees1 <- (fees$yjzzjlfy+fees$yjzzylfy*0.9-qfx1*fees$zyrc10)*ratio1+
  (fees$yjtxjlfy+fees$yjtxylfy*0.9-qfx2*fees$zyrc11)*ratio2
fees1

totalfees <- fees1+fees2+fees3+fees4
totalfees






####城镇职工无等级(500,0.9),(400,0.96)
####城镇职工异地(1000,0.75),(1000,0.75)
####城镇居民无等级(200,0.8)
####城镇居民异地(900,0.4)



####2015年药品费用前10的药品名
color <- c('lightcoral','lightblue','lawngreen','mistyrose','orchid4','lightcyan','lavender','cornsilk','snow','steelblue')
fees <- c(7379637.26,5262819.61,5154149.73,5037114.07,4902152.2,3459960.87,3356849.13,3251523.59,3008248.77,2967488.09)
barplot(data.matrix(fees), beside=TRUE,col=color,
        legend=c('丹红注射液','注射用红花黄色素','脑苷肌肽注射液','前列地尔注射液','注射用血栓通(冻干)','氯化钠注射液','注射用左卡尼汀','注射用骨肽','注射用奥美拉唑钠','注射用血塞通(冻干)'))



x0 <- c(13643,15406,18183,21350,23313,25649,27053,31340,37648,41121)
x1 <- x0/12
x1
)