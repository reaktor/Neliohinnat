library(dplyr)
library(rstan)

d <-
  read.table("sane-ashi.txt", sep=" ", header=T, colClasses=list(pnro="factor")) %>%
  tbl_df() %>%
  mutate(level1 = as.factor(substr(pnro, 1, 3)),
         level2 = as.factor(substr(pnro, 1, 2)),
         level3 = as.factor(substr(pnro, 1, 1)), 
         yr = (year-2013)/100,
         lprice = log(price)-6)


wtf <- function (d, cl, cu) { d1 <- unique(data.frame(l=as.numeric(d[,cl][[1]]), u=as.numeric(d[,cu][[1]]))); d1[order(d1$l),]$u }

#m <- lmer(log(price) ~ yr  + (1|pnro) + (1+yr|level1) + (1+yr|level2) + (1|level3), data=d, weights=d$n)
m <- stan_model(file="m1.stan")
s <- sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), M3=nlevels(level3),
                                   lprice=lprice, count=n, yr=yr, 
                                   pnro=as.numeric(pnro), 
                                   l1=wtf(d, "pnro", "level1"), 
                                   l2=wtf(d, "level1", "level2"), 
                                   l3=wtf(d, "level2", "level3"))),
              iter=500, warmup=200, thin=1, init="random", chains=1, refresh=1)

saveRDS(s, "s.rds")

# Note that it would be better to compute several chains.
# You should monitor the convergence here somehow.
s
traceplot(s, "Omega", inc_warmup=F)
traceplot(s, "Omega3", inc_warmup=F)
traceplot(s, "tau1", inc_warmup=F)
traceplot(s, "tau3", inc_warmup=F)

beta <- apply(extract(s, "beta")[[1]], c(2, 3), mean)
hintataso <- exp(beta[,2]+6)
trendi <- beta[,1]
hist(hintataso, n=100)
hist(trendi, n=100)
plot(beta[,2], trendi, pch=".")

# For NA pnro's, look for upper level in the hierarchy and take beta1
load("pnro_spatial_wgs84.RData")
pnro.sp$pnro

write.table(data.frame(pnro=levels(d$pnro), hintataso, trendi), "pnro-hinnat.txt", row.names=F, quote=F)
