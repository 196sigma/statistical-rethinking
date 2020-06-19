rm(list+ls())
gc()

library(rethinking)

data("reedfrogs")
d <- reedfrogs

## 13.2
d$tank <- 1:nrow(d)
dat <- list(S = d$surv,
            N = d$density,
            tank = d$tank)

m13.1 <- rethinking::ulam(alist(S ~ dbinom(N, p),
                                logit(p) <- a[tank],
                                a[tank] ~ dnorm(0, 1.5)),
                          data=dat, chains=4, log_lik=TRUE)
rethinking::precis(m13.1, depth=2)

plot(d$propsurv)
abline(v=c(1+16,1+32))
abline(h=mean(d$propsurv), lty=3)
