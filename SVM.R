#SVM算法
getwd()
setwd("G:/研一/研一下学期/学习资料/R语言学习/R-dataset")
getwd()
df <- read.csv("df.csv")
df
logit.fit <- glm(Label ~ X + Y,
                 family = binomial(link = 'logit'),
                 data = df)
logit.predictions <- ifelse(predict(logit.fit) > 0, 1, 0)
mean(with(df, logit.predictions == Label))

svm.fit <- svm(Label ~ X + Y, data = df)
svm.predictions <- ifelse(predict(svm.fit) > 0, 1, 0)
mean(with(df,svm.predictions == Label))
?with
