
# ---------------------------------------------
# Homework 3
# ---------------------------------------------

# n <- 200
n <- 10^3

# Homework 3.1 (a)
  # WRONG SIMULATION
  x<-arima.sim(n=n, list(ar=c(0.7), ma=c(-0.8,0.1))) + 0.3
  plot(x,main="X")

  # Correct SIMULATION
  x<-arima.sim(n=n, list(ar=c(0.7), ma=c(-0.8,0.1))) + 1
  plot(x,main="X")
  
# Homework 3.1 (b)
require(TSA)
eacf(x)

# Homework 3.1 (c)
require(tseries)
summary(r.arma1 <- arma(x, order = c(1, 2)),include.intercept = TRUE)
require(forecast)
Arima(x, order=c(1,0,2), include.drift = TRUE)

# Homework 3.3 (a)
z<-arima.sim(n=n+1, list(order = c(1,1,2) ,ar=c(0.7), ma=c(-0.8,0.1)))

# Homework 3.3 (b)
require(urca)
df2=ur.df(z,type="none",selectlags = "AIC")
summary(df2)
