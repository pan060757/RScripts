###计算起付线、封顶线、自付比例对均次医疗费用支出的影响
##测试样例一：
a <- 1500           ##起付线
c <- 280000          ##封顶线
k <- 0.15           ##自付比例
EX0 <- 138018.40      ##均次住院医疗费用
EY0 <- 9522.90      ##均次住院报销费用

# ##测试样例二:
# a <- 850          ##起付线
# c <- 250000          ##封顶线
# k <- 0.15           ##自付比例
# EX0 <- 6718.87      ##均次住院医疗费用
# EY0 <- 4793.83      ##均次住院报销费用

###定义方程函数
f1 <- function(sig,a,c,k,EX0,EY0) {
  b <- c/(1-k)+a      ##医保均次总花费最高金额
  n <- 10000
  u <- seq(0,1,1/n)   ###将(0,1)区间等分成n份
  ksi <- a+(b-a)*u
  eta <- b+(10000000-b)*u
  sigma_1 <- (sqrt(2*pi)*sig)^(-1)
  sigma_2 <- 0.5*sig^2
  sigma_3 <- 2*sig^2
  sum((b-a)*(1-k)*(ksi-a)*sigma_1*exp(-(log(ksi)-log(EX0)+sigma_2)^2/sigma_3)/(n*ksi)+(10000000-b)*c*sigma_1*exp(-(log(eta)-log(EX0)+sigma_2)^2/sigma_3)/(n*eta))-EY0
}
###求解对应的sigma的值
result<- uniroot(f1,c(0.5,10),a=a,c=c,k=k,EX0=EX0,EY0=EY0)
result$root

###改变起付线的值
sig <- 3.795
for (a in seq(1550,1700,50)){
  b <- c/(1-k)+a      ##医保均次总花费最高金额
  n <- 10000
  u <- seq(0,1,1/n)   ###将(0,1)区间等分成n份
  ksi <- a+(b-a)*u
  eta <- b+(10000000-b)*u
  sigma_1 <- (sqrt(2*pi)*sig)^(-1)
  sigma_2 <- 0.5*sig^2
  sigma_3 <- 2*sig^2
  EY0 <- sum((b-a)*(1-k)*(ksi-a)*sigma_1*exp(-(log(ksi)-log(EX0)+sigma_2)^2/sigma_3)/(n*ksi)+(10000000-b)*c*sigma_1*exp(-(log(eta)-log(EX0)+sigma_2)^2/sigma_3)/(n*eta))
  print(EY0)
}


###改变报销比例的值,对均次医疗费用的影响
sig <- 3.795
a <- 1500
for (k in seq(0.04,0.14,0.01)){
  b <- c/(1-k)+a      ##医保均次总花费最高金额
  n <- 10000
  u <- seq(0,1,1/n)   ###将(0,1)区间等分成n份
  ksi <- a+(b-a)*u
  eta <- b+(10000000-b)*u
  sigma_1 <- (sqrt(2*pi)*sig)^(-1)
  sigma_2 <- 0.5*sig^2
  sigma_3 <- 2*sig^2
  EY0 <- sum((b-a)*(1-k)*(ksi-a)*sigma_1*exp(-(log(ksi)-log(EX0)+sigma_2)^2/sigma_3)/(n*ksi)+(10000000-b)*c*sigma_1*exp(-(log(eta)-log(EX0)+sigma_2)^2/sigma_3)/(n*eta))
  print(EY0)
}

###改变封顶线的值，对均次医疗费用的影响
sig <- 3.795
k<- 0.15
for (c in seq(290000,390000,10000)){
  b <- c/(1-k)+a      ##医保均次总花费最高金额
  n <- 10000
  u <- seq(0,1,1/n)   ###将(0,1)区间等分成n份
  ksi <- a+(b-a)*u
  eta <- b+(10000000-b)*u
  sigma_1 <- (sqrt(2*pi)*sig)^(-1)
  sigma_2 <- 0.5*sig^2
  sigma_3 <- 2*sig^2
  EY0 <- sum((b-a)*(1-k)*(ksi-a)*sigma_1*exp(-(log(ksi)-log(EX0)+sigma_2)^2/sigma_3)/(n*ksi)+(10000000-b)*c*sigma_1*exp(-(log(eta)-log(EX0)+sigma_2)^2/sigma_3)/(n*eta))
  print(EY0)
}

###############################################################################################################################
###############################################################################################################################
##上述建模过程应用在泸州市医疗保险数据上结果如下：
##按行处理文件
data <- read.table("E:/毕业设计/建模结果/hospitalizationFeesBygrade.csv",sep=",",header=F,encoding="UTF-8")
data













