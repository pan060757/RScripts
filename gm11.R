
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

X0 <- c(173,201,262,317,476,607)
X1 <- generate(X0)


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

getB(X1)

#��ԭʼ���н���Ԥ��
getPredict <- function(k,X0,a,u)
{
  result <- c()
  for(i in 1:(length(X0)-1))
  {
    xi<- (1-exp(a))*(X0[1]-u/a)*exp(-a*i)
    result <- c(result,xi)
  }
  return (result)
}



#��δ֪���н���Ԥ��
getUnPredict <- function(k,X0,a,u)
{
    result<- (1-exp(a))*(X0[1]-u/a)*exp(-a*(k-1))
    return (result)
}


#�⻬�Լ���
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
       print("����⻬�Լ����Ҫ��")
     }
    
  }
}



####���ȼ���
etest <- function(X0)
{
  for(i in 1:length(X0))
  {
    ratio <- X0[i]/X0[i-1]
    print(ratio)
    if(i>3&&(ratio>exp(-2/(length(X0)+1)))&&(ratio<exp(2/(length(X0)+1))))
    {
      print(i)
      print("���㼶�ȼ���")
    }
  }
}






####С�����ʵļ���
getprob <- function(e,avg_error,S1)
{
    count <- 0
    for(i in 1:(length(e)))
    {
      print(e[i])
      if(abs(e[i]-avg_error)<0.6745*S1)
      {
        count <- count+1
      }
    }
    return (count/length(e))
}






#####���彨ģ
gm <- function(X0,nums){
  X1 <- generate(X0)
  print(X1)
  ####���й⻬�Լ��� 
  test(X0,X1)          #####����⻬�Լ���
  Y <-matrix(getY(X0),length(getY(X0)),1)
  b <- c(getB(X1),rep(1,length(getB(X1))))
  B <- matrix(b,length(getB(X1)),2)
  result <- solve(t(B)%*%B)%*%(t(B))%*%Y
  print('*****')
  #a,uΪ����Ĳ���
  a <- result[1,1]
  u <- result[2,1]
  print(a)
  print(u)
  #��ԭʼ���л�����ϲв�ͼ
  fit <- c(X0[1],getPredict(length(X0),X0,a,u))
  print("���ֵ��")
  print(fit)    ####�������ǳ�x0����ģ��������е�Ԥ�ⷽ��
  write.table(fit,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
  print("����ʣ�")
  ratio <-c((X0-fit)/X0)*100
  print(ratio)
  write.table(ratio,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
  
  ######��ģ�;��Ƚ��м���
  res <- c(abs(X0-fit))     ###����в�
  avg_res <- sum(res)/length(X0)   ###����ƽ���в�
  avg_x <- sum(X0)/length(X0)      ###����ԭʼ���е�ƽ��ֵ
  S1 <- sqrt((sum(abs(X0-avg_x))^2)/length(X0))
  #####����С���ĸ���
  P <- getprob(res,avg_res,S1)
  print(P)
  S2 <- sqrt((sum(abs(res-avg_res))^2)/length(X0))
  C <- S2/S1
  print(C)
  
  ###��2016-2018�����ݽ���Ԥ��
  predict <- c()
  for(i in 1:nums)
  {
    predict <- c(predict,getUnPredict(length(X0)+i,X0,a,u))     ###��2014������ݽ���Ԥ��
  }
  print("Ԥ��ֵ:")
  print(predict)
  write.table(predict,file="F:/data.xlsx",col.names=FALSE,row.names = FALSE,append=TRUE)
  return (predict)
}


















