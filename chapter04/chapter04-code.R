rm(list=ls())
gc()

library(rethinking)
library(ggplot2)

## 4.1.1 Normal by addition
steps <- c(0, runif(16, -1, 1))
hist(steps)
pos <- sum(steps)
plot(cumsum(steps), type='l', xlim=c(1,16), ylim=c(-8,8))
position.list <- c()
for(i in 1:1000){
  steps <- c(0, runif(16,-1,1))
  pos <- sum(steps)
  position.list <- c(position.list, pos)
  points(cumsum(steps), type='l', col=rgb(0,0,1,.1))
}
hist(position.list)

## 4.1
position.list <- replicate(1000, sum(runif(16,-1,1)))
hist(position.list)

## 4.14 - 4.15
n <- 1e4
sample_mu <- rnorm(n, 178, 20)
sample_sigma <- runif(n, 0, 50)
prior_h <- rnorm(n, sample_mu, sample_sigma)
rethinking::dens(prior_h)

par(mfrow=c(2,2))
rethinking::dens(sample_mu)
rethinking::dens(sample_sigma)
rethinking::dens(prior_h)

sample_mu <- rnorm(n, 178, 100)
prior_h <- rnorm(n, sample_mu, sample_sigma)
rethinking::dens(prior_h)

# 4.3 Gaussian model of height
par(mfrow=c(1,1))
## 4.26
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

plot(d$height ~ d$weight)
## 4.27 - 4.29
flist <- alist(height ~ dnorm(mu, sigma),
               mu ~ dnorm(178,20),
               sigma ~ dunif(0,50))
m4.1 <- rethinking::quap(flist, data=d2)
rethinking::precis(m4.1)

## 4.32
vcov(m4.1)

## 4.33
diag(vcov(m4.1))
cov2cor(vcov(m4.1))

## 4.34 - 4.35
post <- rethinking::extract.samples(m4.1, n=1e4)
head(post)
rethinking::precis(post)
plot(post, col=rgb(0,0,1,.33))

## 4.36
library(MASS)
post2 <- MASS::mvrnorm(n=1e4, mu=coef(m4.1), Sigma=vcov(m4.1))
plot(post2, col=rgb(0,0,1,.33))

# 4.4 Linear Prediction
## 4.38 - 4.41
set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)
par(mfrow=c(1,2))
plot(NULL, xlim=range(d2$weight), ylim=c(-100,400), xlab="weight", ylab="height")
abline(h=0, lty=2)
abline(h=272, lty=1, lwd=.5)
mtext("b ~ dnorm(0,10)")
xbar <- mean(d2$weight)
for(i in 1:N){
  curve(a[i] +b[i]*(x-xbar), from=min(d2$weight), to=max(d2$weight), add=TRUE, col=col.alpha("black",0.2))
}
b <- rlnorm(N, 0, 1)
plot(NULL, xlim=range(d2$weight), ylim=c(-100,400), xlab="weight", ylab="height")
abline(h=0, lty=2)
abline(h=272, lty=1, lwd=.5)
mtext("log(b) ~ dnorm(0,10)")
xbar <- mean(d2$weight)
for(i in 1:N){
  curve(a[i] +b[i]*(x-xbar), from=min(d2$weight), to=max(d2$weight), add=TRUE, col=col.alpha("black",0.2))
}

rethinking::dens(b, xlim=c(0,5), adj=0.1)

## 4.42, 4.44
xbar <- mean(d2$weight)
m4.3 <- rethinking::quap(alist(height ~ dnorm(mu, sigma),
                               mu <- a + b*(weight - xbar),
                               a ~ dnorm(178, 20),
                               b ~ dlnorm(0, 1),
                               sigma ~ dunif(0, 50)),
                         data = d2)
precis(m4.3)
## 4.45
round(vcov(m4.3), 3)

## 4.46
par(mfrow=c(1,1))
plot(height ~ weight, data=d2, col=rangi2)
post <- rethinking::extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map*(x - xbar), add=TRUE)

## 4.47
post <- rethinking::extract.samples(m4.3)
post[1:5,]

## 4.48
N <- 10
dN <- d2[1:N, ]
mN <- rethinking::quap(alist(height ~ dnorm(mu, sigma),
                 mu <- a + b*(weight - mean(weight)),
                 a ~ dnorm(178,20),
                 b ~ dlnorm(0,1),
                 sigma ~ dunif(0,50)),
           data=dN)

## 4.49
post <- rethinking::extract.samples(mN, n=20)
plot(dN$weight, dN$height,
     xlim=range(d2$weight),
     ylim=range(d2$height),
     col=rangi2, xlab="weight", ylab="height")
mtext(concat("N = ", N))
for(i in 1:20){
  curve(post$a[i] + post$b[i]*(x - mean(dN$weight)),
        col=col.alpha("black",0.3), add=TRUE)
}

estimate.and.plot <- function(N){
  dN <- d2[1:N, ]
  mN <- rethinking::quap(alist(height ~ dnorm(mu, sigma),
                               mu <- a + b*(weight - mean(weight)),
                               a ~ dnorm(178,20),
                               b ~ dlnorm(0,1),
                               sigma ~ dunif(0,50)),
                         data=dN)
  
  ## 4.49
  post <- rethinking::extract.samples(mN, n=20)
  plot(dN$weight, dN$height,
       xlim=range(d2$weight),
       ylim=range(d2$height),
       col=rangi2, xlab="weight", ylab="height")
  mtext(concat("N = ", N))
  for(i in 1:20){
    curve(post$a[i] + post$b[i]*(x - mean(dN$weight)),
          col=col.alpha("black",0.3), add=TRUE)
  }
}
## Figure 4.7
par(mfrow=c(2,2))
estimate.and.plot(10)
estimate.and.plot(50)
estimate.and.plot(150)
estimate.and.plot(352)

par(mfrow=c(1,1))

## 4.50 - 4.51
post <- rethinking::extract.samples(m4.3)
mu_at_50 <- post$a + post$b*(50 - xbar)
rethinking::dens(mu_at_50, col=rangi2, lwd=2, xlab="mu|weight=50")

## 4.52
rethinking::PI(mu_at_50, prob=0.89)

## 4.53
mu <- rethinking::link(m4.3)
str(mu)

## 4.54
weight.seq <- seq(from=25, to=70, by=1)
mu <- rethinking::link(m4.3, data=data.frame(weight=weight.seq))
str(mu)

par(mfrow=c(1,2))
## 4.55
plot(height ~ weight, d2, type="n")
for(i in 1:100) points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2, 0.1))

## 4.56
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)

## 4.57
plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
rethinking::shade(mu.PI, weight.seq)

par(mfrow=c(1,1))
## 4.59
sim.height <- rethinking::sim(m4.3, data=list(weight=weight.seq))
str(sim.height)

## 4.60
height.PI <- apply(sim.height, 2, PI, prob=0.89)

## 4.61
plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
mu.HPDI <- apply(mu, 2, rethinking::HPDI)
rethinking::shade(mu.HPDI, weight.seq)
rethinking::shade(height.PI, weight.seq)

# 4.5 Curves from lines