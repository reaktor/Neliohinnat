library(dplyr)
library(rstan)

# FIXME: Reduce()
first.nna2 <- function (c1, c2)  ifelse(!is.na(c1), c1, c2)
first.nna4 <- function (c1, c2, c3, c4) first.nna2(c1, first.nna2(c2, first.nna2(c3, c4)))

prefix.factor <- function (pnro, n) as.factor(substr(pnro, 1, n))
l3 <- { . %>% prefix.factor(., 1) }
l2 <- { . %>% prefix.factor(., 2) }
l1 <- { . %>% prefix.factor(., 3) }

d <- readRDS("data/statfi_ashi_pnro_processed_2005-2014_20141219.rds") %>% 
  tbl_df() %>% 
  filter(!is.na(price)) %>%
  mutate(pnro=factor(pnro), year=as.numeric(as.character(year))) %>% # pnro has extra levels
  mutate(level1 = l1(pnro),
         level2 = l2(pnro),
         level3 = l3(pnro), 
         yr = (year-mean(year))/10,
         lprice = log(price)-6)


wtf <- function (d, cl, cu) data.frame(l=as.numeric(d[[cl]]), u=as.numeric(d[[cu]])) %>% unique %>% { .[order(.$l),]$u }

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
# Those tau's are notoriously well autocorrelated.
s
traceplot(s, "Omega", inc_warmup=F, ask=T)
traceplot(s, "Omega3", inc_warmup=F, ask=T)
traceplot(s, "tau1", inc_warmup=F)
traceplot(s, "tau3", inc_warmup=F)
# etc.

beta.prm.mean <- function (v) apply(extract(s, v)[[1]], c(2, 3), mean)
# For debugging
beta <- beta.prm.mean("beta")
lhinta <- beta[,1]+6
trendi <- beta[,2]/10
quad <- beta[,3]

par.tbl <- function(d, v.name, b.name, name.postfix) 
  data.frame(levels(d[[v.name]]), beta.prm.mean(b.name)) %>% 
  setNames(c(v.name, paste(c("lhinta", "trendi", "quad"), name.postfix, sep=""))) %>%
  tbl_df()


# For NA pnro's, look for upper level in the hierarchy and take beta1
load("data/pnro_spatial_wgs84.RData")
pnro <- pnro.sp$pnro
pnro.pars <- par.tbl(d, "pnro", "beta", "")
level1.pars <- par.tbl(d, "level1", "beta1", "1")
level2.pars <- par.tbl(d, "level2", "beta2", "2")
level3.pars <- par.tbl(d, "level3", "beta3", "3")
res <- data.frame(pnro, 
                  level1 = l1(pnro), 
                  level2 = l2(pnro), 
                  level3 = l3(pnro)) %>% 
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
