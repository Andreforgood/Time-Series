HW1
#Q1:
set.seed(250)
n <- 200
a <- rep(0,n)

a[1:(n/2)] <- rnorm(n/2,0,1)
a[(n/2+1):n] <- rnorm(n/2,1,1)
head(a)
plot(a)

#Q2:
install.packages('fitdistrplus')
library(fitdistrplus)
model1<- fitdistrplus::fitdist(a, 'norm')
model1$bic
model1

#Q3
#Fix X with a mixture of normal when you know there are two mixture components 
install.packages('mclust')
library(mclust)
mix_fit <- Mclust(a, G = 2)
mix_fit$parameters

#Q4 What would you do if you don't know the number of mixture components?
my_mix_fit <- Mclust(a)
my_mix_fit$G
my_mix_fit$parameters
my_mix_fit$bic

for (i in 1:5){
  my_mix_fit <- Mclust(a,G = i)
  print(my_mix_fit$bic)
}
#根据BIC准则，最小值为对应的G = 1的情形

#Q5  Now, suppose you know that the data actually comes from two different model, 
#but you don't know the cutting point 100. How can you fit the model?
#Hint. Treat the cutting point as a parameter.
#we can loop over all possible cutting points, and compare the bic of each.
best_cutting <- 0
best_bic <- Inf

for (cutting in 2:(n-1)){
  x1 <- a[1:cutting]
  x2 <- a[cutting:n]
  
  my_model1 <- fitdistrplus::fitdist(x1, 'norm')
  my_model2 <- fitdistrplus::fitdist(x2, 'norm')
  
  sum_bic <- my_model1$bic + my_model2$bic
  
  if (sum_bic < best_bic){
    best_bic <- sum_bic
    best_cutting <- cutting
  }
}
print(c(best_bic,best_cutting))



#Q6(Bonus)
# 直接进行K均值聚类分析
# 使用肘部法则来确定最佳聚类数（k值）
wss <- numeric(10)
for (i in 1:10) {
  wss[i] <- sum(kmeans(a, centers=i)$withinss)
}

plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# 基于图形选择一个肘点，2和3看起来均可
k <- 2  
km_result <- kmeans(a, centers=k)

# 查看聚类结果
km_result$cluster  
km_result$centers  

# 可视化聚类结果
plot(a, col=km_result$cluster, pch=19, xlab="Index", ylab="Value")
points(km_result$centers, col=1:k, pch=3, cex=2, lwd=2)

group_data <- split(a, km_result$cluster)

# 输出每个聚类的数据
for (i in 1:k) {
  cat("Group", i, ":", group_data[[i]], "\n")
}

