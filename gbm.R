N <- 1000
X1 <- runif(N)   #产生1000个均匀分布随机数
X1
X2 <- 2*runif(N)
X2 
letters           #26个字母
X3 <- ordered(sample(letters[1:4],N,replace=TRUE),levels=letters[4:1])
#函数ordered()可以创建有序因子
# ordered(province)
# 四川 湖南 江苏 四川 四川 四川 湖南 江苏 湖南 江苏
#Levels: 湖南 < 江苏 < 四川
X3
X4 <- factor(sample(letters[1:6],N,replace=TRUE))
X4
X5 <- factor(sample(letters[1:3],N,replace=TRUE))
X5
X6 <- 3*runif(N)
X6
mu <- c(-1,0,1,2)[as.numeric(X3)]
mu

SNR <- 10 # signal-to-noise ratio
Y <- X1**1.5 + 2 * (X2**.5) + mu
sigma <- sqrt(var(Y)/SNR)
Y <- Y + rnorm(N,0,sigma)
# introduce some missing values
X1[sample(1:N,size=500)] <- NA
X4[sample(1:N,size=300)] <- NA
data <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)
head(data)


# fit initial model
library(gbm)
gbm1 <-
  gbm(Y~X1+X2+X3+X4+X5+X6, # formula
      data=data, # dataset
      var.monotone=c(0,0,0,0,0,0), # -1: monotone(单调) decrease,
      # +1: monotone increase,
      # 0: no monotone restrictions
      distribution="gaussian",          # see the help for other choices
      n.trees=1000,                      # number of trees
      shrinkage=0.05,                    # shrinkage or learning rate,
                                         # 0.001 to 0.1 usually work
      interaction.depth=3,               # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5,                # subsampling fraction, 0.5 is probably best
      train.fraction = 0.5,              # fraction of data for training,
                                         # first train.fraction*N used for training
      n.minobsinnode = 10,               # minimum total weight needed in each node
      cv.folds = 3,                      # do 3-fold cross-validation
      keep.data=TRUE,                    # keep a copy of the dataset with the object
      verbose=FALSE,                     # don't print out progress
      n.cores=1)                         # use only a single core (detecting #cores is
                                         # error-prone, so avoided here)
summary(gbm1)
# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)

# check performance using a 50% heldout test set
best.iter <- gbm.perf(gbm1,method="test")
print(best.iter)

# check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)

# plot the performance # plot variable influence
summary(gbm1,n.trees=1) # based on the first tree
summary(gbm1,n.trees=best.iter) # based on the estimated best number of trees

# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(gbm1,1))
print(pretty.gbm.tree(gbm1,gbm1$n.trees))

# make some new data
N <- 1000
X1 <- runif(N)
X2 <- 2*runif(N)
X3 <- ordered(sample(letters[1:4],N,replace=TRUE))
X4 <- factor(sample(letters[1:6],N,replace=TRUE))
X5 <- factor(sample(letters[1:3],N,replace=TRUE))
X6 <- 3*runif(N)
mu <- c(-1,0,1,2)[as.numeric(X3)]
Y <- X1**1.5 + 2 * (X2**.5) + mu + rnorm(N,0,sigma)
data2 <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)
head(data2)

# predict on the new data using "best" number of trees
# f.predict generally will be on the canonical scale (logit,log,etc.)
f.predict <- predict(gbm1,data2,best.iter)
head(f.predict)
# least squares error
print(sum((data2$Y-f.predict)^2))

# create marginal plots
# plot variable X1,X2,X3 after "best" iterations
par(mfrow=c(1,3))
plot(gbm1,1,best.iter)
plot(gbm1,2,best.iter)
plot(gbm1,3,best.iter)

par(mfrow=c(1,1))
# contour plot of variables 1 and 2 after "best" iterations
plot(gbm1,1:2,best.iter)
# lattice plot of variables 2 and 3
plot(gbm1,2:3,best.iter)
# lattice plot of variables 3 and 4
plot(gbm1,3:4,best.iter)

# 3-way plots
plot(gbm1,c(1,2,6),best.iter,cont=20)
plot(gbm1,1:3,best.iter)
plot(gbm1,2:4,best.iter)
plot(gbm1,3:5,best.iter)
# do another 100 iterations
gbm2 <- gbm.more(gbm1,100,
                 verbose=FALSE)    
#stop printing detailed progress




#####gbm实现医疗保险数据
gbm.sol <-data.frame(total=c(2065,3787,7286,9400,10902,12404,13783,15732,18020,19996,21937,23735),
                    zznum=c(1509,2863,5471,6926,7975,9045,10022,11580,13420,14988,16410,17791),
                    avgincome=c(8346,9371,10870,12422,14040,16024,18364,21001,24932,29229,32736,37147),
                    ratio=c(2.71,3.10,3.01,2.80,2.72,2.69,2.66,2.79,2.92,2.99,2.97,2.99),
                    Y1=c(89.9,170,384,607.5,890,1141,1405,1747,2257,3040,3672,4309))
gbm.sol
gbm1 <-
  gbm(Y1~total+zznum+avgincome+ratio, # formula
      data=data, # dataset
      var.monotone=c(0,0,0,0,0,0), # -1: monotone decrease,
      # +1: monotone increase,
      # 0: no monotone restrictions
      distribution="gaussian",          # see the help for other choices
      n.trees=3000,                      # number of trees
      shrinkage=0.05,                    # shrinkage or learning rate,
      # 0.001 to 0.1 usually work
      interaction.depth=3,               # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5,                # subsampling fraction, 0.5 is probably best
      train.fraction = 0.25,              # fraction of data for training,
      # first train.fraction*N used for training
      n.minobsinnode = 1,               # minimum total weight needed in each node
      cv.folds = 3,                      # do 3-fold cross-validation
      keep.data=TRUE,                    # keep a copy of the dataset with the object
      verbose=FALSE,                     # don't print out progress
      n.cores=1)                         # use only a single core (detecting #cores is
# error-prone, so avoided here)
summary(gbm1)