rm(list=ls())
gc()

library(rethinking)
y <- c(1,0,1,1,1,0,1,0)
y_sum <- cumsum(y)
p_grid <- seq(from=0, to=1, length.out=1000)
prob_p <- rep(1,1000)
prob_data <- dbinom(1, size=1, prob=p_grid)
posterior <- prob_data*prob_p
#posterior <- posterior/sum(posterior)
par(mfrow=c(1,1))
plot(prob_p, ylim=c(0,1))
for(i in 1:length(y_sum)){
  prob_data <- dbinom(y_sum[i], size=i, prob=p_grid)  
  posterior <- prob_data*prob_p
  points(posterior, col=rgb(1,0,0,1/(10-i)), type='l')
}

par(mfrow=c(3,3), mar=c(2,1,1,1))
plot(prob_p, ylim=c(0,1.2), type='l', col='red')
for(i in 1:length(y_sum)){
  prob_data <- dbinom(y_sum[i], size=i, prob=p_grid)  
  posterior <- prob_data*prob_p
  plot(posterior, ylim=c(0,1.2), col='red', type='l', main=paste('n',i,sep=' = '))
}




## 3.2
p_grid <- seq(from=0, to=1, length.out=1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, size=9, prob=p_grid)
posterior <- prob_data*prob_p
posterior <- posterior/sum(posterior)
par(mfrow=c(1,3))
plot(prob_p)
plot(prob_data)
plot(posterior)

## 3.3 - 3.5
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
par(mfrow=c(1,2))
plot(samples)
rethinking::dens(samples)

## 3.6 - 3.8
sum(posterior[p_grid < 0.5])
sum(samples < 0.5)/1e4
sum(samples > 0.5 & samples < 0.75)/1e4

## 3.9 and 3.10
quantile(samples, 0.8)
quantile(samples, c(0.1, 0.9))

## 3.11 and 3.13
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size=3, prob=p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)
par(mfrow=c(1,3))
plot(prior)
plot(likelihood)
plot(posterior)

rethinking::PI(samples, prob=0.5)
rethinking::HPDI(samples, prob=0.5)

## 
rm(list=ls())
gc()
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)
plot(posterior, type='l')
abline(v = 1000*seq(0.1,.9,.1))
par(mfrow=c(1,9), mar=c(4,2,1,3))
for( i in seq(0.1, 0.9, .1)) rethinking::simplehist(rbinom(1e4, size=9, prob=i), xlab = i, ylab = NA, ylim=c(0,4000))

par(mfrow = c(1,1), mar=c(4,4,4,4))
w <- rbinom(1e4, size=9, prob=samples)
rethinking::simplehist(w)
