library(rstan)
library(dplyr)

s <- readRDS("s3.rds")


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
  hist(beta[,1], n=100)
  hist(beta[,2], n=100)
  hist(beta[,3], n=100)
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
              first.nna(k.lhinta, k.lhinta1, k.lhinta2, k.lhinta3) * log.density, 
            trendi=first.nna(trendi, trendi1, trendi2, trendi3) +
              first.nna(k.trendi, k.trendi1, k.trendi2, k.trendi3) * log.density, 
            quad=first.nna(quad, quad1, quad2, quad3) +
              first.nna(k.quad, k.quad1, k.quad2, k.quad3) * log.density
  ) %>%
  mutate(hintataso=exp(lhinta))


write.table(res,  "data/pnro-hinnat.txt", row.names=F, quote=F)
saveRDS(res, "data/pnro-hinnat.rds")

