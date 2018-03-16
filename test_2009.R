####自己尝试
####计算各一级指标下，二级指标的熵
computeEntropy <- function (data,i,j,m){
  ####先计算第i个一级指标下第j个指标的熵
  ####先求出总和
  s <- 0
  for(index in 1:m){
    s <- s+1+data[index,j]  ###为了防止结果出现0的情况
  }
  
  tij<-(1+data[i,j])/s
  return (tij)
}


####数据读入
data<- read.csv("E:/毕业设计/建模结果/test.csv",header=F)
data
####进行数据标准化操作(最小-最大规范化)
data_scatter <- matrix(NA,5,0)
data_scatter
for(i in 1:19){
  bi=(data[,i]-min(data[,i]))/(max(data[,i])-min(data[,i]))
  data_scatter <-cbind(data_scatter,bi)
}

data_scatter


####确定各指标的熵
m <- 5
temp_ij<- c()
for(i in 1:5){
  for(j in 1:19){
    temp_ij <- c(temp_ij,computeEntropy(data_scatter,i,j,m))
  }
}
fij <- matrix(temp_ij,5,19)
fij

hj <- c()
for(j in 1:19){ 
  s <- 0
  for(i in 1:m){
    if(fij[i,j]>0){
    s <- s+fij[i,j]*log(fij[i,j])
    }
    else
      s=s+0
  }
  temp<- -s/log(m)
  hj <- c(hj,temp)
}
hj

hj <- c(0.795,0.841,0.801,0.795,0.820,0.807,0.814,0.814,0.811,0.807,0.795,0.820,0.795,0.820,0.807,0.806,0.823,0.807,0.795)
####确定二级各指标的权重
n <- 19
a <- (1-hj)/(n-sum(hj))
a <- a/sum(a)
a

####确定一级各指标的权重
s <- 0
for(j in 1:9)
  s <- s+(1-hj[j])

w1 <- (3-hj[1]-hj[2]-hj[3])/s
w1

w2 <- (3-hj[4]-hj[5]-hj[6])/s
w2

w3<- (3-hj[7]-hj[8]-hj[9])/s
w3

w <- cbind(w1,w2,w3)
w

####给定主观权重的值
k <- c(0.4,0.3,0.3)  ###一级指标主观权重

b <- c(0.3,0.4,0.3,0.4,0.4,0.2,0.34,0.33,0.33)   ###二级指标主观权重


###二级指标综合权重计算
s1 <- 0
for(i in 1:3){
  s1<-s1+a[i]*b[i]
}

labda <- c()
for(i in 1:3)
  labda<-c(labda,a[i]*b[i]/s1)

s2 <- 0
for(i in 3:6){
  s2<- s2+a[i]*b[i]
}

for(i in 4:6)
  labda<-c(labda,a[i]*b[i]/s2)

s3 <- 0
for(i in 7:9){
  s3<- s3+a[i]*b[i]
}

for(i in 7:9)
  labda<-c(labda,a[i]*b[i]/s3)

labda 

###一级指标综合权重
s1 <- 0
for(i in 1:3){
  s1 <- s1+w[i]*k[i]
}

theta <- c()
for(i in 1:3)
  theta <- c(theta,w[i]*k[i]/s1)

theta


















