rm(list=ls())
gc()

library(ggplot2)
library(rethinking)
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

sum(samples < .2)/length(samples)

dens <- density(samples)
dd <- with(dens, data.frame(x,y))
qplot(x,y,data=dd,geom="line") + geom_ribbon(data=subset(dd, x < .2), aes(ymax=y),ymin=0,
              fill="blue",colour=NA,alpha=0.5)

## 3E4
quantile(samples, .2)

## 3E6
rethinking::HPDI(samples, prob=.66)

## 3E7
q17 <- quantile(samples, probs=.17)
q83 <- quantile(samples, probs=.83)
sum(samples > q17 & samples < q83)/length(samples)
rethinking::PI(samples, prob=.66)

## 3M1
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size=15, prob=p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
ggplot(NULL, aes(x=samples)) + geom_density()


birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)