########用2006-2013年数据建立多元回归模型
inc<-data.frame(
  zlb=c(1.67,1.83,1.79, 1.78, 1.75, 1.76, 1.73, 1.74),
  zzrs=c(156370,183789,202044,212367,219954,232399,237813,245695),
  cbrs=c(249855,284421,315049,331460,345635,364248,374620,386565),
  txrs=c(93485,100632,113005,119093,125681,131849,136807,140870),
  pjgz=c(13643,15406,18183,21350,23313,25649,27053,31340),
  income=c(17958,22992,32778,36613,41376,51208,63223,122984))
inc
plot(inc$cbrs,inc$income)
plot(inc$txrs,inc$income)
plot(inc$pjgz,inc$income)
lm.sol <- lm(income~zzrs+txrs+pjgz,data=inc)
summary(lm.sol)
##########对已有的时间序列数据进行拟合
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/income)
errorrate

##########对未来的数据(2014-2015)进行拟合
newdata <- data.frame(zlb=log(c(1.83,1.86)),
                      zzrs=log(c(262376,273233)),
                      cbrs=log(c(405684,419892)),
                      pjgz=log(c(37648,41121)),
                      income=log(c(117924,139027)))
y.fit <- predict(lm.sol,newdata)
y.fit
errorrate <- c((y.fit-newdata$income)/newdata$income)
errorrate

fees <-data.frame(
  jlypf=c(578924.21,835456.99,1214730.79,1439446.17,
          2723349.09,3640105.66,4294828.51,4567603.11),
  zyrc=c(2768,3299,4157,4555,5085,5673,6697,7524),
  jlfy=c(610334.04,1247941.03,2942431.91,4488150.20,11863593.58,14536871.17,
          17425320.20,19206800.02))

fees
lm.sol <- lm(jlfy~zyrc+jlypf,data=fees)
summary(lm.sol)
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/fees$jlfy)
errorrate
#####对新数据进行预测
fees <- data.frame(
  jlypf=c(5553566.80,6835328.01),
  zyrc=c(8853,12043),
  jlfy=c(25206536.85,34941224.49)
)
y.fit <- predict(lm.sol,fees)
y.fit
errorrate <- c((y.fit-fees$jlfy)/fees$jlfy)
errorrate



fees <- read.csv("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample1.txt")
fees
lm.sol <- lm(jlfy[0:12]~zyrc[0:12]+jlypf[0:12],data=fees)
summary(lm.sol)
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/fees$jlfy[0:12])
errorrate
####predict new data
newdata<- data.frame(fees$jlypf[13:14],
                   fees$zyrc[13:14],
                   fees$jlfy[13:14])
colnames(newdata)=c('jlypf','zyrc','jlfy')
newdata
y.fit <- predict(lm.sol,newdata)
y.fit
errorrate <- c((y.fit-newdata$jlfy)/newdata$jlfy)
errorrate



fees <- read.csv("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample2.txt")
fees
lm.sol <- lm(jlfy[0:8]~zyrc[0:8]+jlypf[0:8],data=fees)
summary(lm.sol)
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/fees$jlfy[0:8])
errorrate
####predict new data
newdata<- data.frame(fees$jlfy[9:11],
                     fees$zyrc[9:11],
                     fees$jlypf[9:11])
colnames(newdata)=c('jlfy','zyrc','jlypf')
newdata
y.fit <- predict(lm.sol,newdata)
y.fit
errorrate <- c((y.fit-newdata$jlfy)/newdata$jlfy)
errorrate


fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample40.txt",header = TRUE)
fees
lm.sol <- lm(jlfy[0:11]~zyrc[0:11]+jlypf[0:11],data=fees)
summary(lm.sol)
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/fees$jlfy[0:11])
errorrate

####predict new data
newdata <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample401.txt",header = TRUE)
newdata
data <- data.frame(newdata$zyrc,newdata$jlypf)
data
colnames(data)=c('zyrc','jlypf')
y.fit <- predict(lm.sol,data)
y.fit

lm.sol <- lm(jlfy[0:11]~zyrc[0:11]+ylypf[0:11],data=fees)
summary(lm.sol)
newdata <- data.frame(newdata$zyrc,newdata$ylypf)
newdata
colnames(newdata)=c('zyrc','ylypf')
y.fit <- predict(lm.sol,newdata)
y.fit


fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample41.txt",header = TRUE)
fees
lm.sol <- lm(jlfy[0:11]~zyrc[0:11]+jlypf[0:11],data=fees)
summary(lm.sol)
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/fees$jlfy[0:11])
errorrate

####predict new data
newdata <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample411.txt",header = TRUE)
newdata
data <- data.frame(newdata$zyrc,newdata$jlypf)
data
colnames(data)=c('zyrc','jlypf')
y.fit <- predict(lm.sol,data)
y.fit

lm.sol <- lm(jlfy[0:11]~zyrc[0:11]+ylypf[0:11],data=fees)
summary(lm.sol)
newdata <- data.frame(newdata$zyrc,newdata$ylypf)
newdata
colnames(newdata)=c('zyrc','ylypf')
y.fit <- predict(lm.sol,newdata)
y.fit

fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample10.txt",header = TRUE)
fees
lm.sol <- lm(jlfy[0:13]~zyrc[0:13]+jlypf[0:13],data=fees)
summary(lm.sol)
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/fees$jlfy[0:13])
errorrate

####predict new data
newdata <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample101.txt",header = TRUE)
newdata
data <- data.frame(newdata$zyrc,newdata$jlypf)
data
colnames(data)=c('zyrc','jlypf')
y.fit <- predict(lm.sol,data)
y.fit

lm.sol <- lm(jlfy[0:13]~zyrc[0:13]+ylypf[0:13],data=fees)
summary(lm.sol)
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/fees$jlfy[0:13])
errorrate
newdata <- data.frame(newdata$zyrc,newdata$ylypf)
newdata
colnames(newdata)=c('zyrc','ylypf')
y.fit <- predict(lm.sol,newdata)
y.fit





fees <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample11.txt",header = TRUE)
fees
lm.sol <- lm(jlfy[0:11]~zyrc[0:11]+jlypf[0:11],data=fees)
summary(lm.sol)
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/fees$jlfy[0:11])
errorrate

####predict new data
newdata <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx_new\\result\\Hospitalization_Payment_Model\\sample111.txt",header = TRUE)
newdata
data <- data.frame(newdata$zyrc,newdata$jlypf)
data
colnames(data)=c('zyrc','jlypf')
y.fit <- predict(lm.sol,data)
y.fit

lm.sol <- lm(jlfy[0:11]~zyrc[0:11]+ylypf[0:11],data=fees)
summary(lm.sol)
y.fit <- predict(lm.sol)
y.fit
y.res <- resid(lm.sol)
y.res
errorrate <- (y.res/fees$jlfy[0:11])
errorrate
newdata <- data.frame(newdata$zyrc,newdata$ylypf)
newdata
colnames(newdata)=c('zyrc','ylypf')
y.fit <- predict(lm.sol,newdata)
y.fit







