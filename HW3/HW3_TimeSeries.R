HW3
install.packages('TSA')
library(forecast)
install.packages('tseries')
#1、
#Generate the first 1000 data points from the following 
#ARMA(1,2). R(t)=0.3+0.7 R(t-1)+ a(t)-0.8 a(t-1)+ 0.1 a(t-2)

#make one difference, the equation becomes
#ARMA(2,3),R(t) = 1.7R(t-1) - 0.7R(t-2) + a(t) - 1.8a(t-1) - 0.7a(t-2) - 0.1a(t-3)

#以下为手动模拟函数过程
set.seed(12)
n <- 1000
a <- rnorm(n)
R <- rep(0,n) #初始化数据

R[1] <- 1  # 可以假设初始值，或者根据实际情况调整
R[2] <- 0.3 + 0.7 * R[1] + a[2] - 0.8 * a[1]  # 计算第二个值

for (i in 3:n){
  R[i] <- 0.3 + 0.7 * R[i-1] + a[i] - 0.8 * a[i-1] + 0.1 * a[i-2] 
}

#print(R)
head(R)
plot(x = c(1:n),y = R)

#2、
library(TSA)
eacf(R)
#This graph is quite hard to tell, if we know ARMA is to be used, 
#then ARMA(1,2) should be our choice

#3、
coef_arma <- arima(R,order = c(1,0,2))
print(coef_arma)

#4、
# 对ARMA(1,2)模型生成的数据进行一次差分
diff_R<- diff(R)

# 显示差分后的前几个数据点
head(diff_R)

#5、 Do the augmented Dickey-Fuller test on your simulated data.
library(tseries)
# 执行ADF检验
adf_test <- adf.test(diff_R)

# 显示ADF检验的结果
adf_test
