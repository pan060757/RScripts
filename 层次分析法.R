##层次分析法
##输入：judgeMatrix 判断矩阵；round 结果约分位数
##输出：权重
weight <- function (judgeMatrix, round=3) {
  n = ncol(judgeMatrix)
  cumProd <- vector(length=n)
  cumProd <- apply(judgeMatrix, 1, prod)  ##求每行连乘积
  weight <- cumProd^(1/n)  ##开n次方(特征向量)
  weight <- weight/sum(weight) ##求权重
  round(weight, round)
}

###一致性检验
###注：CRtest调用了weight函数
###输入：judgeMatrix
###输出：CI, CR
CRtest <- function (judgeMatrix, round=3){
  RI <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49, 1.51) #随机一致性指标
  Wi <- weight(judgeMatrix)  ##计算权重
  n <- length(Wi)
  if(n > 11){
    cat("判断矩阵过大,请少于11个指标 \n")
  }
  if (n > 2) {
    W <- matrix(Wi, ncol = 1) 
    judgeW <- judgeMatrix %*% W 
    JudgeW <- as.vector(judgeW)
    la_max <- sum(JudgeW/Wi)/n
    CI = (la_max - n)/(n - 1)
    CR = CI/RI[n]
    cat("\n CI=", round(CI, round), "\n")
    cat("\n CR=", round(CR, round), "\n")
    if (CR <= 0.1) {
      cat(" 通过一致性检验 \n")
      cat("\n Wi: ", round(Wi, round), "\n")
    }
    else {
      cat(" 请调整判断矩阵,使CR<0.1 \n")
      Wi = NULL
    }
  }
  else if (n <= 2) {
    return(Wi)
  }
  consequence <- c(round(CI, round), round(CR, round))
  names(consequence) <- c("CI", "CR")
  consequence
}


#####输入案例进行测试
b <- c(1,1/2,1/3,1/5,2,1,1/4,2/9,3,4,1,1/3,5,9/2,3,1)
(judgeMatix <- matrix(b, ncol=4))

##计算权重
weight(judgeMatix)
##判断矩阵一致性检验
CRtest(judgeMatix)

b <- c(1,1/2,1/7,2,1,1/3,7,3,1)
(judgeMatix <- matrix(b, ncol=3))

##计算权重
weight(judgeMatix)
##判断矩阵一致性检验
CRtest(judgeMatix)

b <- c(1,2/3,1/2,2/3,1,2/3,2,3/2,1)
(judgeMatix <- matrix(b, ncol=3))
##计算权重
weight(judgeMatix)
##判断矩阵一致性检验
CRtest(judgeMatix)


a <- c(0.0214,0.0427,0.0214,0.0427,0.0563,0.0499,0.0405,0.0717,0.0610,0.0643,0.0613,0.0646,0.0646,0.0474,
       0.0395,0.0223,0.0360,0.0372,0.0158,0.0233,0.0233,0.0465,0.0466)
b <- c(-1.205,0.316,0.005,0.311,0.085,0.003,0.078,0.65,0.103,0.084,-0.977,0.141,0.156,0.026,0.015,
       0.135,0.114,0.259,0.016,1.438,0.781,0.087,0.006)
c <- c(-0.826,0.328,0.023,0.346,0.078,0.003,0.041,0.66,0.114,0.075,-0.101,0.421,0.178,0.034,0.026,0.126,
       0.201,0.243,0.02,0.997,0.713,0.084,0.006)
d <- sum(a*c)
d
