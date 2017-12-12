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
###############################################################################################################################
##上述建模过程应用在泸州市医疗保险数据上结果如下：
##按行处理文件
data <- read.table("E:/毕业设计/建模结果/hospitalizationFeesBygroup.csv",sep=",",header=F,encoding="UTF-8")
colnames(data) <- c("year","grade","state","hfees","hgroupfees","line","ratio")      ##列重命名
# data

###对2015年的情况进行分析
for (i in 51:nrow(data)){              ####遍历每一行
  print("********************************************************************************")
  print(data[i,]$grade)
  print(data[i,]$state)
  c <- 240000   ##封顶线
  a <- data[i,]$line   ##起付线
  k <- data[i,]$ratio  ##自付比例
  EX0 <- data[i,]$hfees  ##均次住院费用
  EY0 <- data[i,]$hgroupfees ##均次统筹支付
  result<- uniroot(f1,c(0.1,5),a=a,c=c,k=k,EX0=EX0,EY0=EY0)
  sig <- result$root    ##求得参数的值
  
  ###改变起付线的值
  print('改变起付线的值,对均次医疗费用的影响')
  line <- a
  for (a in seq(400,900,50)){
    b <- c/(1-k)+a      ##医保均次总花费最高金额
    n <- 10000
    u <- seq(0,1,1/n)   ###将(0,1)区间等分成n份
    ksi <- a+(b-a)*u
    eta <- b+(10000000-b)*u
    sigma_1 <- (sqrt(2*pi)*sig)^(-1)
    sigma_2 <- 0.5*sig^2
    sigma_3 <- 2*sig^2
    EY0_new <- sum((b-a)*(1-k)*(ksi-a)*sigma_1*exp(-(log(ksi)-log(EX0)+sigma_2)^2/sigma_3)/(n*ksi)+(10000000-b)*c*sigma_1*exp(-(log(eta)-log(EX0)+sigma_2)^2/sigma_3)/(n*eta))
    ratio <- (EY0_new-EY0)*line/((a-line)*EY0)
    cat("起付线对均次统筹支付的弹性系数为:",ratio,"\n")
   }
  
  ###改变报销比例的值,对均次医疗费用的影响
  print('改变报销比例的值,对均次医疗费用的影响')
  a <- data[i,]$line   ##起付线
  r <- k         ##保存自付比
  for (k in seq(0.04,0.20,0.01)){
    b <- c/(1-k)+a      ##医保均次总花费最高金额
    n <- 10000
    u <- seq(0,1,1/n)   ###将(0,1)区间等分成n份
    ksi <- a+(b-a)*u
    eta <- b+(10000000-b)*u
    sigma_1 <- (sqrt(2*pi)*sig)^(-1)
    sigma_2 <- 0.5*sig^2
    sigma_3 <- 2*sig^2
    EY0_new <- sum((b-a)*(1-k)*(ksi-a)*sigma_1*exp(-(log(ksi)-log(EX0)+sigma_2)^2/sigma_3)/(n*ksi)+(10000000-b)*c*sigma_1*exp(-(log(eta)-log(EX0)+sigma_2)^2/sigma_3)/(n*eta))
    ratio <- (EY0_new-EY0)*r/((k-r)*EY0)
    cat("自付比对均次统筹支付的弹性系数为:",ratio,"\n")
  }
  
  ###改变封顶线的值，对均次医疗费用的影响
  print('改变封顶线对均次医疗费用的影响')
  k <- data[i,]$ratio  ##自付比例
  m <- 240000
  for (c in seq(240000,300000,10000)){
    b <- c/(1-k)+a      ##医保均次总花费最高金额
    n <- 10000
    u <- seq(0,1,1/n)   ###将(0,1)区间等分成n份
    ksi <- a+(b-a)*u
    eta <- b+(10000000-b)*u
    sigma_1 <- (sqrt(2*pi)*sig)^(-1)
    sigma_2 <- 0.5*sig^2
    sigma_3 <- 2*sig^2
    EY0_new<- sum((b-a)*(1-k)*(ksi-a)*sigma_1*exp(-(log(ksi)-log(EX0)+sigma_2)^2/sigma_3)/(n*ksi)+(10000000-b)*c*sigma_1*exp(-(log(eta)-log(EX0)+sigma_2)^2/sigma_3)/(n*eta))
    ratio <- (EY0_new-EY0)*m/((c-m)*EY0)
    cat("封顶线对均次统筹支付的弹性系数为:",ratio,"\n")
  }
}