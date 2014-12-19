library(dplyr)
library(rstan)

d <- readRDS("data/statfi_ashi_pnro_processed_2005-2014_20141219.rds") %>% 
  tbl_df() %>% 
  filter(!is.na(price)) %>%
  mutate(pnro=as.factor(as.character(pnro)), year=as.numeric(as.character(year))) %>%
  mutate(level1 = as.factor(substr(pnro, 1, 3)),
         level2 = as.factor(substr(pnro, 1, 2)),
         level3 = as.factor(substr(pnro, 1, 1)), 
         yr = (year-mean(year))/10,
         lprice = log(price)-6)


wtf <- function (d, cl, cu) { d1 <- unique(data.frame(l=as.numeric(d[,cl][[1]]), u=as.numeric(d[,cu][[1]]))); d1[order(d1$l),]$u }

#m <- lmer(log(price) ~ yr  + (1|pnro) + (1+yr|level1) + (1+yr|level2) + (1|level3), data=d, weights=d$n)
m <- stan_model(file="source/m2.stan")
s <- sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), M3=nlevels(level3),
                                   lprice=lprice, count=n, yr=yr, 
                                   pnro=as.numeric(pnro), 
                                   l1=wtf(d, "pnro", "level1"), 
                                   l2=wtf(d, "level1", "level2"), 
                                   l3=wtf(d, "level2", "level3"))),
              iter=2000, warmup=1000, thin=5, init=0, chains=1, refresh=1)

saveRDS(s, "s.rds")
s <- readRDS("s.rds")

# Note that it would be better to compute several chains.
# You should monitor the convergence here somehow.
s
traceplot(s, "Omega", inc_warmup=F, ask=T)
traceplot(s, "Omega3", inc_warmup=F, ask=T)
traceplot(s, "tau1", inc_warmup=F)
traceplot(s, "tau3", inc_warmup=F)

beta <- apply(extract(s, "beta")[[1]], c(2, 3), mean)
beta1 <- apply(extract(s, "beta1")[[1]], c(2, 3), mean)
beta2 <- apply(extract(s, "beta2")[[1]], c(2, 3), mean)
beta3 <- apply(extract(s, "beta3")[[1]], c(2, 3), mean)
lhinta <- beta[,1]+6
trendi <- beta[,2]/10
quad <- beta[,3]
lhinta1 <- beta1[,1]+6
trendi1 <- beta1[,2]/10
quad1 <- beta1[,3]
lhinta2 <- beta2[,1]+6
trendi2 <- beta2[,2]/10
quad2 <- beta2[,3]
lhinta3 <- beta3[,1]+6
trendi3 <- beta3[,2]/10
quad3 <- beta3[,3]
# hist(trendi, n=100)
# plot(beta[,1], trendi)

first.nna2 <- function (c1, c2)  ifelse(!is.na(c1), c1, c2)
first.nna4 <- function (c1, c2, c3, c4) first.nna2(c1, first.nna2(c2, first.nna2(c3, c4)))

# For NA pnro's, look for upper level in the hierarchy and take beta1
load("data/pnro_spatial_wgs84.RData")
pnro <- pnro.sp$pnro
pnro.pars <- data.frame(pnro=levels(d$pnro), lhinta, trendi, quad)
level1.pars <- data.frame(level1=levels(d$level1), lhinta1, trendi1, quad1)
level2.pars <- data.frame(level2=levels(d$level2), lhinta2, trendi2, quad2)
level3.pars <- data.frame(level3=levels(d$level3), lhinta3, trendi3, quad3)
res <- data.frame(pnro, 
                  level1 = as.factor(substr(pnro, 1, 3)), 
                  level2 = as.factor(substr(pnro, 1, 2)), 
                  level3 = as.factor(substr(pnro, 1, 1))) %>% 
  left_join(pnro.pars, by="pnro") %>% 
  left_join(level1.pars, by="level1") %>% 
  left_join(level2.pars, by="level2") %>% 
  left_join(level3.pars, by="level3") %>% 
  transmute(pnro=pnro, 
            lhinta=first.nna4(lhinta, lhinta1, lhinta2, lhinta3), 
            trendi=first.nna4(trendi, trendi1, trendi2, trendi3), 
            quad=first.nna4(quad, quad1, quad2, quad3)) %>%
  mutate(hintataso=exp(lhinta))

write.table(res,  "data/pnro-hinnat.txt", row.names=F, quote=F)
saveRDS(res, "data/pnro-hinnat.rds")
