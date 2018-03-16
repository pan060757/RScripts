getwd()
setwd("G:\\研一\\研一下学期\\学习资料\\R语言学习\\R-script")
source("新陈代谢GM(1,1)模型.R")
#####使用等维信息GM(1,1)论文中的数据进行分析
X0 <- c(65565,108845,148677,192740,226928,261060,332473,403912,414545,484936,657336,800100)
para <- getParameters(X0)         #####获得微分方程的两个参数a,u
para
#####利用传统的gm11模型对于原始序列的数据进行拟合
fit <- getPredict(length(X0),X0)
res <- c(abs(X0[-1]-fit))     ###计算残差
res
ratio <- res/X0[-1]
ratio

#####对于未来7年的基金收入进行预测
result <- gm(X0,7)
result


#####使用等维信息GM(1,1)论文中的数据进行分析
X0 <- c(37340,68937,94772,125474,137941,163647,190148,269820,375484,503542,583009,665698)
para <- getParameters(X0)         #####获得微分方程的两个参数a,u
para
#####利用传统的gm11模型对于原始序列的数据进行拟合
fit <- getPredict(length(X0),X0)
res <- c(abs(X0[-1]-fit))     ###计算残差
res
ratio <- res/X0[-1]
ratio

#####对于未来7年的基金收入进行预测
result <- gm(X0,7)
result



#####用2006-2013年的医疗收入数据进行模型拟合
X0 <- c(17958,22992,32778,36613,41376,51208,63223,86016)
X <- c(103075,122984)
#####利用传统的gm11模型对于原始序列的数据进行拟合
fit <- c(X0[1],getPredict(length(X0),X0))
fit    ####给出的是除x0以外的，其它序列的预测方法
#######模型拟合的误差率
ratio <-c((X0-fit)/X0)
ratio
#####对于未来7年的基金收入进行预测
result <- gm(X0,2)
result
result[(length(X0)-1):(length(X0))]
ratio <- (result[length(X0)-1]-X[1])/X[1]
ratio
ratio <- (result[length(X0)]-X[2])/X[2]
ratio



#####用2006-2013年的医疗费用数据进行模型拟合
X0 <- c(12679,19805,25829,35186,48708,60951,72716,90335)
X <- c(96118,116651)
#####利用传统的gm11模型对于原始序列的数据进行拟合
fit <- c(X0[1],getPredict(length(X0),X0))
fit    ####给出的是除x0以外的，其它序列的预测方法
#######模型拟合的误差率
ratio <-c((X0-fit)/X0)
ratio
#####对于未来7年的基金收入进行预测
result <- gm(X0,2)
result
result[(length(X0)-1):(length(X0))]
ratio <- (result[length(X0)-1]-X[1])/X[1]
ratio
ratio <- (result[length(X0)]-X[2])/X[2]
ratio




#####尝试用X(1,n)代替X(1,0)来解决问题

