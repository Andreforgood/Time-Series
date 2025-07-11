install.packages('zoo')
library(forecast)
library(zoo)
#1、
#Generate the first 500 data points from the following 
#AR(3).R(t)=0.01+0.1 R(t-1)-0.1 R(t-3)+ a(t)

n <- 500 

#下面是白噪声的构造
miu_a <- 0
sigma_a <- 1

#numeric()函数是R语言中的一个函数，用于创建一个指定长度的空数值型向量
my_vector <- numeric(n)

#然后构造3个初始data值
#由于有期望与方差，所以用rnorm()用于生成服从正态分布的随机数
my_vector[1:3] <- rnorm(3,mean = 0,sd = 1)
#再利用迭代公式计算剩余
for (i in 4:n) {
  my_vector[i] <- 0.01+0.1*my_vector[i-1]-0.1*my_vector[i-3]+rnorm(1,mean=0,sd=1)
}

plot(x=c(1:n),y=my_vector,main='AR(3)',type='l',xlab='Time',ylab='R(t)')

#2、
acf(my_vector,main = 'Autocorrelation of AR3',xlab='Lag',ylab='Autocorrelation')
#3、
pacf_result <- pacf(my_vector)

# 绘制PACF图
plot(pacf_result, main = "Partial Autocorrelation Function (PACF)")

# 根据PACF图确定阶数,以0.05为判别条件
order <- which(abs(pacf_result$acf) < 0.05)[1] - 1
cat("Estimated order of the autoregressive model:", order, "\n")

#4、Now we calculate AIC for the model
n <- 10
my_model <- ar(my_vector, aic = TRUE, order.max = n,order.method = "ols")
my_aic <- (my_model$aic)

for (i in 0:10){
  names(my_aic)[i+1] <- paste0('poly',i) 
}

print(my_aic)
cat(min(my_aic))
which.min(my_aic)
#所以返回的是三阶的AR3，索引为4
#美元符号（$）用于提取对象中的成员或属性。
#当对象是列表或数据框时，美元符号允许访问其中的特定列或变量。
cat('The best order is', which.min(my_aic)-1)
cat('The min AIC is', min(my_aic))

#5、
order <- 3

# 使用ar函数进行参数估计
my_ar_model <- ar(my_vector, method = "ols", order.max = order)
# 输出参数估计结果
print(my_ar_model)

#6、
my_residual <- residuals(my_ar_model)
class(my_residual)

my_residual <- na.fill(my_residual,fill=0)
print(my_residual)

acf_resid <- acf(my_residual,main='ACF of my residual')

Box.test(my_residual, type="Ljung-Box")
#根据输出，无法拒绝原假设，所以残差序列符合白噪声。