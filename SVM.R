#SVM�㷨
getwd()
setwd("G:/��һ/��һ��ѧ��/ѧϰ����/R����ѧϰ/R-dataset")
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