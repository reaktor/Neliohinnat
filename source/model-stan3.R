library(dplyr)
library(rstan)

first.nna2 <- function (c1, c2)  ifelse(!is.na(c1), c1, c2)
first.nna <- function (...) Reduce(first.nna2, list(...))
  
prefix.factor <- function (pnro, n) as.factor(substr(pnro, 1, n))
l3 <- { . %>% prefix.factor(., 1) }
l2 <- { . %>% prefix.factor(., 2) }
l1 <- { . %>% prefix.factor(., 3) }

# Span the space on pnro's form the spatial data.
pnro.area <- local({
  load("data/pnro_spatial_epsg2393.RData") # And pnro.sp magically appears (FIXME: rds...)
  data.frame(pnro=pnro.sp.alt$pnro, log.area=log(pnro.sp.alt@data$area.m2)-18) }) 

d <- readRDS("data/statfi_ashi_pnro_processed_2005-2014_20141219.rds") %>% 
  left_join(pnro.area %>% select(pnro, log.area), by="pnro") %>%
  filter(!is.na(log.area)) %>%
  tbl_df() %>% 
  filter(!is.na(price)) %>%
  mutate(pnro=factor(pnro), year=as.numeric(as.character(year))) %>% # pnro has extra levels
  mutate(level1 = l1(pnro),
         level2 = l2(pnro),
         level3 = l3(pnro), 
         yr = (year-mean(year))/10,
         lprice = log(price)-6)



wtf <- function (d, cl, cu) data.frame(l=as.numeric(d[[cl]]), u=as.numeric(d[[cu]])) %>% unique %>% { .[order(.$l),]$u }

m <- stan_model(file="source/m3.stan")
s <- sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), M3=nlevels(level3),
                                   lprice=lprice, count=n, yr=yr, z=log.area,
                                   pnro=as.numeric(pnro), 
                                   l1=wtf(d, "pnro", "level1"), 
                                   l2=wtf(d, "level1", "level2"), 
                                   l3=wtf(d, "level2", "level3"))),
              iter=6000, warmup=1000, thin=25, init=0, chains=1, refresh=1, seed=4)

saveRDS(s, "s3.rds")
s <- readRDS("s3.rds")


# ******** Postprocessing is to be written ***********

# Note that it would be better to compute several chains.
# You should monitor the convergence here somehow.
# Those tau's are notoriously well autocorrelated.
if (F) {
  s
  traceplot(s, "LOmega", inc_warmup=F, ask=T)
  traceplot(s, "LOmega3", inc_warmup=F, ask=T)
  traceplot(s, "tau1", inc_warmup=F)
  traceplot(s, "tau3", inc_warmup=F)
  # etc.
}

# Low-level correlation matrix over price level, trend, etc.
# Is of general interest
LOmega <- apply(extract(s, "LOmega")[[1]], c(2, 3), mean)
Omega <- LOmega %*% t(LOmega) # Not quite, we'd need a tensor product
saveRDS(Omega, "data/Omega.rds")
beta.prm.mean <- function (v) apply(extract(s, v)[[1]], c(2, 3), mean)
# For debugging 
if (F) {
  beta <- beta.prm.mean("beta")
  lhinta <- beta[,1]+6
  trendi <- beta[,2]/10
  quad <- beta[,3]
  hist(beta[,4], n=100)
  hist(beta[,5], n=100)
  hist(beta[,6], n=100)
}

par.tbl <- function(d, v.name, b.name, name.postfix) 
  data.frame(levels(d[[v.name]]), beta.prm.mean(b.name)) %>% 
  setNames(c(v.name, paste(c("lhinta", "trendi", "quad", "k.lhinta", "k.trendi", "k.quad"), name.postfix, sep=""))) %>%
  tbl_df()

pnro <- pnro.area$pnro
# For NA pnro's in the model, look for upper level in the hierarchy and take beta1 etc.
res <- data.frame(pnro.area, level1 = l1(pnro), level2 = l2(pnro), level3 = l3(pnro)) %>% 
  left_join(par.tbl(d, "pnro", "beta", ""), by="pnro") %>% 
  left_join(par.tbl(d, "level1", "beta1", "1"), by="level1") %>% 
  left_join(par.tbl(d, "level2", "beta2", "2"), by="level2") %>% 
  left_join(par.tbl(d, "level3", "beta3", "3"), by="level3") %>% 
  transmute(pnro=pnro, 
            lhinta=first.nna(lhinta, lhinta1, lhinta2, lhinta3) + 
                   first.nna(k.lhinta, k.lhinta1, k.lhinta2, k.lhinta3) * log.area, 
            trendi=first.nna(trendi, trendi1, trendi2, trendi3) +
                   first.nna(k.trendi, k.trendi1, k.trendi2, k.trendi3) * log.area, 
            quad=first.nna(quad, quad1, quad2, quad3) +
                   first.nna(k.quad, k.quad1, k.quad2, k.quad3) * log.area
            ) %>%
  mutate(hintataso=exp(lhinta))


write.table(res,  "data/pnro-hinnat.txt", row.names=F, quote=F)
saveRDS(res, "data/pnro-hinnat.rds")
