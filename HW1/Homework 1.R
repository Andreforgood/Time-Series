# Homework 1.1
a <- rnorm(100, mean = 0, sd = 1)
b <- rnorm(100, mean = 1, sd = 1)
x = c(a,b)

# Homework 1.2
require(MASS)
fit <- fitdistr(x, densfun="normal") 
fit
hist(x, pch=20, breaks=25, prob=TRUE, main="")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)

# Homework 1.3
install.packages('mixtools')
require(mixtools)
fit = normalmixEM(x,k=2)
summary(fit)
plot(fit,which=2)
lines(density(x), lty=2, lwd=2)

# Homework 1.4
require(mixtools)
fit = normalmixEM(x)
summary(fit)
plot(fit,which=2)
lines(density(x), lty=2, lwd=2)

# Homework 1.5
install.packages('StepSignalMargiLike')
require(StepSignalMargiLike)
prior <- prior.norm.B(x)
index.ChPT <- est.changepoints(data.x=x, model="normal", prior=prior, max.segs=2)
est.mean <- est.mean.norm(x, index.ChPT, prior)
PlotChangePoints(x, 1:length(x), index.ChPT, est.mean)

# Homework 1.6
require(StepSignalMargiLike)
prior <- prior.norm.B(x)
index.ChPT <- est.changepoints(data.x=x, model="normal", prior=prior)
est.mean <- est.mean.norm(x, index.ChPT, prior)
PlotChangePoints(x, 1:length(x), index.ChPT, est.mean)
