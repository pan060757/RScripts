##参保人员和医院聚类分析
insured_person <- read.csv("E:\\毕业设计\\建模结果\\manage\\ins_processingS.csv")
insured_person
gower_dist <- daisy(insured_person[, -1],
                    metric = "gower",
                    type = list(logratio = 3))
# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"
summary(gower_dist)


gower_mat <- as.matrix(gower_dist)

# Output most similar pair
college_clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
college_clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


