
getY <- function(X0)
{
  y <- c(X0[2:length(X0)])
  return (y)
}
getY(X0)


getB <- function(X1)
{
  i <- 0
  b <- c()
  for(i in 1:(length(X1)-1))
  {
    bi <- -1/2*(X1[i]+X1[i+1])
    b <- c(b,bi)
  }
  return (b)
  
}

#对原始序列进行预测
getPredict <- function(k,X0)
{
  
  para <- getParameters(X0)         #####获得微分方程的两个参数a,u
  a <- para[1]
  u <- para[2]
  result <- c()
  for(i in 1:(length(X0)-1))
  {
    xi<- (1-exp(a))*(X0[1]-u/a)*exp(-a*i)
    result <- c(result,xi)
  }
  return (result)
}



#对未知序列进行预测
getUnPredict <- function(k,X0)
{
  para <- getParameters(X0)         #####获得微分方程的两个参数a,u
  a <- para[1]
  u <- para[2]
  result<- (1-exp(a))*(X0[1]-u/a)*exp(-a*(k-1))
  return (result)
}


#####根据原始序列生成新的序列
generate<- function(X0)
{
  i <- 0
  j <- 0
  X1 <- c()
  for(i in 1:length(X0)){
    s <- 0
    for(j in 1:i)
    {
      s <- s+X0[j]
    }
    X1 <- c(X1,s)
  }
  return (X1)
}

#光滑性检验
test <- function(X0,X1)
{
  for(i in 1:(length(X0)-1))
  {
    if(i>1)
    {
      r <- c()
      r <- c(r,i)
      r <- c(r,X0[i])
      r <- c(r,X1[i-1])
      print(r)
      print(X0[i]/X1[i-1])
    }
    
    if(i>3&&(X0[i]/X1[i-1])<0.5)
    {
      print("满足光滑性检验的要求")
    }
    
  }
}

######获得微分方程的相关参数
getParameters <- function(X0)
{
  X1 <- generate(X0)
  ####进行光滑性检验
  test(X0,X1)          ######检验失败，并不满足光滑性检验，所以不宜采用gm(1,1)模型
  Y <-matrix(getY(X0),length(getY(X0)),1)
  b <- c(getB(X1),rep(1,length(getB(X1))))
  B <- matrix(b,length(getB(X1)),2)
  result <- solve(t(B)%*%B)%*%(t(B))%*%Y
  #a,u为所求的参数
  a <- result[1,1]
  u <- result[2,1]
  para <- c(a,u)
}


#####新陈代谢gm11模型
gm <- function(X0,n)
{
  print("当前序列为：")
  print(X0)
  para <- getParameters(X0)         #####获得微分方程的两个参数a,u
  a <- para[1]
  print("参数a:")
  print(a)
  print("参数u:")
  u <- para[2]
  print(u)
  while(n>0){
    predict <- getUnPredict(length(X0)+1,X0)      ###对新的数据进行预测
    X0 <- c(X0[-1],predict)
    n <- (n-1)
    gm(X0,n)
  }
  return (X0)
}


