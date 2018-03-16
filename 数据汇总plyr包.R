library(plyr)
head(tips)
aggregate(x=tips$tip,by=list(tips$sex),FUN=mean)

ddply(.data=tips,.variables='sex',.fun=function(x){mean(x$tip)})

ratio_fun <- function(x){
  sum(x$tip)/sum(x$total_bill)
}

ddply(tips,.(sex),function(x){mean(x$tip)})
ddply(tips,.(sex),ratio_fun)

iris[,-5]
data <- as.matrix(iris[,-5])
result4 <- adply(
  .data=data,
  .margins=2,       #其中1表示按行进行分组，2表示按照列进行分组
  .fun=function(x){
    max <- max(x)
    min <- min(x)
    median <- median(x)   中位数
    sd <- round(sd(x),2)  标准差
    return (c(max,min,median,sd))
  }
)
result4


colwise(mean,is.numeric)(iris)

data()
