###熵权模糊综合评价法实现代码
#数据读入
library(forecast)
library(XLConnect)
sourui <- read.csv("E:/毕业设计/建模结果/train.csv",header = T)
##索引列删除
sourui$案例 <- NULL
###第一步：归一化处理。
min.max.norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}

max.min.norm <- function(x){
  (max(x)-x)/(max(x)-min(x))
}

sourui_1 <- apply(sourui[,-c(7,11)],2,min.max.norm)  #正向
sourui_2 <- apply(sourui[,c(7,11)],2,max.min.norm)   #负向

sourui_t <- cbind(sourui_1,sourui_2)

#####第二步：求出所有样本对指标Xj的贡献总量

first1 <- function(data)
{
  x <- c(data)
  for(i in 1:length(data))
    x[i] = data[i]/sum(data[])
  return(x)
}
dataframe <- apply(sourui_t,2,first1)

####第三步：将上步生成的矩阵每个元素变成每个元素与该ln（元素）的积并计算信息熵。

first2 <- function(data)
{
  x <- c(data)
  for(i in 1:length(data)){
    if(data[i] == 0){
      x[i] = 0
    }else{
      x[i] = data[i] * log(data[i])
    }
  }
  return(x)
}
dataframe1 <- apply(dataframe,2,first2)

k <- 1/log(length(dataframe1[,1]))
d <- -k * colSums(dataframe1)

###第四步：计算冗余度。
d <- 1-d

###第五步：计算各项指标的权重。
w <- d/sum(d)
w




