########GM(1,1)实现医疗保险数据
getwd()
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
source("gm11.R")
X0 <- c(173,201,262,317,476,607)
########生成X1时间序列
X1 <- generate(X0)
X1
####进行光滑性检验
test(X0,X1)
etest(X0)


Y <-matrix(getY(X0),length(getY(X0)),1)
Y
b <- c(getB(X1),rep(1,length(getB(X1))))
b
B <- matrix(b,length(getB(X1)),2)
B
result <- solve(t(B)%*%B)%*%(t(B))%*%Y
result
a <- result[1,1]
a
u <- result[2,1]
u
#对原始序列X0进行预测
fit <- getPredict(length(X0),X0,a,u)
fit
plot(X0[-1]~fit)
abline(lm(X0[-1]~fit))  
res <- c(abs(X0[-1]-fit))
res
plot(res~X0[-1])
rato <- res/X0[-1]
rato
######对模型精度进行检验
res <- c(0)
res <- c(res,abs(X0[-1]-fit))     ###计算残差
res
avg_res <- sum(res)/length(X0)   ###计算平均残差
avg_res 
avg_x <- sum(X0)/length(X0)
avg_x
S1 <- sqrt((sum((X0-avg_x)^2))/length(X0))
S1
#####计算小误差的概率
P <- getprob(res,avg_res,S1)
P
S2 <- sqrt((sum((res-avg_res)^2))/length(X0))
S2
C <- S2/S1
C

#####计算支出
X0 <- c(143,274,313,364,421,574)
########生成X1时间序列
X1 <- generate(X0)
X1
####进行光滑性检验
test(X0,X1)
gm(X0,2)
#####进行级比检验
etest(X0)


Y <-matrix(getY(X0),length(getY(X0)),1)
Y
b <- c(getB(X1),rep(1,length(getB(X1))))
b
B <- matrix(b,length(getB(X1)),2)
B
result <- solve(t(B)%*%B)%*%(t(B))%*%Y
result
a <- result[1,1]
a
u <- result[2,1]
u


#对原始序列X0进行预测
fit <- getPredict(length(X0),X0,a,u)
fit
plot(X0[-1]~fit)
abline(lm(X0[-1]~fit))  
res <- c(abs(X0[-1]-fit))
res
plot(res~X0[-1])
rato <- res/X0[-1]
rato
######对模型精度进行检验
res <- c(0)
res <- c(res,abs(X0[-1]-fit))     ###计算残差
res
avg_res <- sum(res)/length(X0)   ###计算平均残差
avg_res 
avg_x <- sum(X0)/length(X0)
avg_x
S1 <- sqrt((sum((X0-avg_x)^2))/length(X0))
S1
#####计算小误差的概率
P <- getprob(res,avg_res,S1)
P
S2 <- sqrt((sum((res-avg_res)^2))/length(X0))
S2
C <- S2/S1
C




k <- 6
#对其它数据进行预测
x6 <- (1-exp(a))*(X0[1]-u/a)*exp(-a*k)
x6   #797.4665
x.res <- (abs(731-x6)/731)
x.res

#####GM(1,1)实现北京市某城市1986-1992年交通噪声平均声级数据
getwd()
setwd("G:/研一/研一下学期/学习资料/R语言学习/R-script")
source("gm11.R")
X0 <- c(71.1,72.4,72.4,72.1,71.4,72.0,71.6)
X1 <- generate(X0)
X1
####进行光滑性检验
test(X0,X1)
etest(X0)
Y <-matrix(getY(X0),length(getY(X0)),1)
Y
b <- c(getB(X1),rep(1,length(getB(X1))))
b
B <- matrix(b,length(getB(X1)),2)
B
result <- solve(t(B)%*%B)%*%(t(B))%*%Y
result
#a,u为所求的参数
a <- result[1,1]
a
u <- result[2,1]
u
#对原始序列进行预测
fit <- getPredict(length(X0),X0,a,u)
fit
plot(X0[-1]~fit)
abline(lm(X0[-1]~fit))  
res <-c(X0[-1]-fit)
res


##用现有的医疗数据对原始时间序列建模
#用2006-2013年的基金收入数据数据进行建模
getwd()
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
source("gm11.R")
X0 <- c(17598,22992,32778,36613,41376,51208,63223,86016)
incomeseries <- ts(X0[-1],start=c(2006))
plot.ts(incomeseries)
X1 <- generate(X0)
X1
####进行光滑性检验
test(X0,X1)

Y <-matrix(getY(X0),length(getY(X0)),1)
Y
b <- c(getB(X1),rep(1,length(getB(X1))))
b
B <- matrix(b,length(getB(X1)),2)
B
result <- solve(t(B)%*%B)%*%(t(B))%*%Y
result
#a,u为所求的参数
a <- result[1,1]
a
u <- result[2,1]
u
#对原始序列画出拟合残差图
fit <- c(X0[1],getPredict(length(X0),X0,a,u))
fit    ####给出的是除x0以外的，其它序列的预测方法
fitseries <- ts(fit,start=c(2006))
lines (fitseries,col='red')
ratio <-c((X0-fit)/X0)
ratio
######对模型精度进行检验
res <- c(abs(X0-fit))     ###计算残差
res
avg_res <- sum(res)/length(X0)   ###计算平均残差
avg_res 
avg_x <- sum(X0)/length(X0)
avg_x
S1 <- sqrt((sum(abs(X0-avg_x))^2)/length(X0))
S1
#####计算小误差的概率
P <- getprob(res,avg_res,S1)
P
S2 <- sqrt((sum(abs(res-avg_res))^2)/length(X0))
S2
C <- S2/S1
C



###对2014和2015的数据进行预测
X9 <- c(103075)
fit <- getUnPredict(9,X0,a,u)      ###对2014年的数据进行预测
fit
ratio <- c((X9-fit)/X9)
ratio
X10 <- c(122984)
fit <- getUnPredict(10,X0,a,u)      ###对2015年的数据进行预测
fit
ratio <- c((X10-fit)/X10)
ratio



##用现有的医疗数据对原始时间序列建模
#用2006-2013年的费用支出数据数据进行建模
getwd()
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
source("gm11.R")
X0 <- c(12679,19805,25829,35186,48708,60951,72716,90335)
X1 <- generate(X0)
X1
####进行光滑性检验
test(X0,X1)          ######检验失败，并不满足光滑性检验，所以不宜采用gm(1,1)模型

Y <-matrix(getY(X0),length(getY(X0)),1)
Y
b <- c(getB(X1),rep(1,length(getB(X1))))
b
B <- matrix(b,length(getB(X1)),2)
B
result <- solve(t(B)%*%B)%*%(t(B))%*%Y
result
#a,u为所求的参数
a <- result[1,1]
a
u <- result[2,1]
u
#对原始序列画出拟合残差图
fit <- c(X0[1],getPredict(length(X0),X0,a,u))
fit    ####给出的是除x0以外的，其它序列的预测方法
plot(X0~fit)
abline(lm(X0~fit))  
ratio <-c((X0-fit)/X0)
ratio
######对模型精度进行检验
res <- c(abs(X0-fit))     ###计算残差
res
avg_res <- sum(res)/length(X0)   ###计算平均残差
avg_res 
avg_x <- sum(X0)/length(X0)
avg_x
S1 <- sqrt((sum(abs(X0-avg_x))^2)/length(X0))
S1
#####计算小误差的概率
P <- getprob(res,avg_res,S1)
P
S2 <- sqrt((sum(abs(res-avg_res))^2)/length(X0))
S2
C <- S2/S1
C

###对2014和2015的数据进行预测
X9 <- c(96118)
fit <- getUnPredict(9,X0,a,u)      ###对2014年的数据进行预测
ratio <- c((X9-fit)/X9)
ratio
X10 <- c(116651)
fit <- getUnPredict(10,X0,a,u)      ###对2015年的数据进行预测
ratio <- c((X10-fit)/X10)
ratio





#####读入2006-2015的医疗保险的数据(gm11建模)
getwd()
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
source("gm11.R")
data <- read.table(("C:/Users/song/Desktop/generalData.txt"))
data
####对于平均工资建立gm11模型如下
X0 <- data$wage
gm(X0,5)

####对单位在职参保人数进行建模
X0 <- data$cmp.worker
gm(X0,5)

####对单位退休参保人数进行建模
X0 <- data$cmp.retire
gm(X0,5)

####对个体在职参保人数进行建模
X0 <- data$single.worker
gm(X0,5)

####对个体退休参保人数进行建模
X0 <- data$single.retire          ######并不满足光滑性检验
gm(X0,5)

####对当期收入进行建模
X0 <- data$income.current         
gm(X0,5)
etest(X0)

####对个人账户进行建模 
X0 <- data$menzhen.single         #####(当大于4时开始满足平滑性检验)
gm(X0,5)



####对门诊统筹支付进行建模 
X0 <- data$menzhen.group         #####
gm(X0,5)

####对住院统筹支付进行建模 
X0 <- data$hospital.group         #####
gm(X0,5)

####对医疗费用总支出进行建模
X0 <- c(12769,19805,25829,35186,48708,60951,72716,90335,96118,116651)
gm(X0,5)       #####不满足光滑性检验
etest(X0)




####对于平均工资建立gm11模型如下
X0 <- data$wage[0:8]
gm(X0,2)


####对单位在职参保人数进行建模
X0 <- data$cmp.worker[0:8]
gm(X0,2)

####对单位退休参保人数进行建模
X0 <- data$cmp.retire[0:8]
gm(X0,2)

####对个体在职参保人数进行建模
X0 <- data$single.worker[0:8]
gm(X0,2)

####对个体退休参保人数进行建模
X0 <- data$single.retire          ######并不满足光滑性检验
gm(X0,2)

####对当期收入进行建模
X0 <- data$income.current[0:8]         
gm(X0,2)

####对个人账户进行建模 
X0 <- data$menzhen.single[0:8]         #####(当大于4时开始满足平滑性检验)
gm(X0,2)

####对门诊统筹支付进行建模 
X0 <- data$menzhen.group[0:8]         #####
gm(X0,2)

####对住院统筹支付进行建模 
X0 <- data$hospital.group[0:8]         #####不满足光滑性检验
gm(X0,2)

####对医疗费用总支出进行建模
X0 <-c(12769,19805,25829,35186,48708,60951,72716,90335)
X0
gm(X0,2)       #####不满足光滑性检验
###对原始数据进行对数变换
X0 <-ln(c(12769,19805,25829,35186,48708,60951,72716,90335))
X0
gm(X0,2)      ####经过对数变换后满足光滑性检验
exp(11.76225)
exp(12.04589)
X0 <-log(c(12769,19805,25829,35186,48708,60951,72716,90335,96118,116651))
X0

####对总的参保人数进行建模
X0 <- c(249855,284421,315049,331460,345635,364248,374620,386565)
gm(X0,2)




####2012-2015年读入相应的月份数据
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
source("gm11.R")
data <- read.table("G:\\python_learning\\data_set\\data\\python_programs\\ylbx\\local\\dataset\\income.txt")
data
X0 <- data$V1[0:5]
X0
gm(X0,2)



X0 <- c(2360,2873,3188,3656,4605)
gm(X0,2)


X0 <- c(1.0,1.1052,1.2214,1.3499,1.4918,1.6487)
gm(X0,2)



######20161128
X0 <- c(127023573.24,155063561.62,195288199.21,243807352.12,276653981.62,361953989.94,660724259.14,738122452.74)
gm(X0,5)


####基金总收入情况
X0 <- c(241786483.90,309477861.25,371090477.76,469785623.09,595239385.41,666531834.88,996605608.84,1121827516.42,1293622449.34,1415911182.47)




