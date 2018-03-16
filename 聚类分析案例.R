##
set.seed(1680) # 设置随机种子，使得本文结果具有可重现???
library(dplyr)
library(ISLR)
library(cluster)
library(Rtsne)
library(ggplot2)
College
###构建聚类模型之前，我们需要做一些数据清洗工作：

###录取率等于录取人数除以总申请人数
###判断某个学校是否为高水平院校，需要根据该学校的所有新生中毕业于排名前 10% 高中的新生数量占比是否大于 50% 来决定
college_clean <- College %>%
  mutate(name = row.names(.),
         accept_rate = Accept/Apps,
         isElite = cut(Top10perc,
                       breaks = c(0, 50, 100),
                       labels = c("Not Elite", "Elite"),
                       include.lowest = TRUE)) %>%
  mutate(isElite = factor(isElite)) %>%
  select(name, accept_rate, Outstate, Enroll,
         Grad.Rate, Private, isElite)

glimpse(college_clean)

###距离计算
###聚类分析的第一步是定义样本之间距离的度量方法，最常用的距离度量方法是欧式距离。
###然而欧氏距离只适用于连续型变量，所以本文将采用另外一种距离度量方法—— Gower 距离。

# Remove college name before clustering
gower_dist <- daisy(college_clean[, -1],
                    metric = "gower",
                    type = list(logratio = 3))
# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"
summary(gower_dist)

###此外，我们可以通过观察最相似和最不相似的样本来判断该度量方法的合理性。
####本案例中，圣托马斯大学和约翰卡罗尔大学最相似，而俄克拉荷马科技和艺术大学和哈佛大学差异最大。
gower_mat <- as.matrix(gower_dist)

# Output most similar pair
college_clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
college_clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

###聚类算法的选择
###现在我们已经计算好样本间的距离矩阵，接下来需要选择一个合适的聚类算法，
###本文采用 PAM（partioniong around medoids）算法来构建模型：

###聚类个数的选择
###我们将利用轮廓系数来确定最佳的聚类个数，轮廓系数是一个用于衡量聚类离散度的内部指标，该指标的取值范围是[-1,1]，其数值越大越好。
###通过比较不同聚类个数下轮廓系数的大小，我们可以看出当聚类个数为 3 时，聚类效果最好。
# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:10, sil_width)


###聚类结果解释
###描述统计量
###聚类完毕后，我们可以调用 summary 函数来查看每个簇的汇总信息。
###从这些汇总信息中我们可以看出：簇1主要是中等学费且学生规模较小的私立非顶尖院校，簇2主要是高收费、低录取率且高毕业率的私立顶尖院校，而簇3则是低学费、低毕业率且学生规模较大的公立非顶尖院校。
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

pam_results <- college_clean %>%
  dplyr::select(-name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

####查看聚类结果
pam_fit$clustering
print(pam_results$the_summary)

#####PAM 算法的另一个优点是各个簇的中心点是实际的样本点。
###从聚类结果中我们可以看出，圣弗朗西斯大学是簇1 的中心点，巴朗德学院是簇2 的中心点，而密歇根州州立大学河谷大学是簇3 的中心点。
college_clean[pam_fit$medoids, ]


###可视化方法
###t-SNE 是一种降维方法，它可以在保留聚类结构的前提下，将多维信息压缩到二维或三维空间中。借助t-SNE我们可以将 PAM 算法的聚类结果绘制出来，
###有趣的是私立顶尖院校和公立非顶尖院校这两个簇中间存在一个小聚类簇。
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = college_clean$name)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

# 进一步探究可以发现，这一小簇主要包含一些竞争力较强的公立院校，比如弗吉尼亚大学和加州大学伯克利分校。
# 虽然无法通过轮廓系数指标来证明多分一类是合理的，但是这 13 所院校的确显著不同于其他三个簇的院校。У??
tsne_data %>%
  filter(X > 15 & X < 25,
         Y > -15 & Y < -10) %>%
  left_join(college_clean, by = "name") %>%
  collect %>%
  .[["name"]]
