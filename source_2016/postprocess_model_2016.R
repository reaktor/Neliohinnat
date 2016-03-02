library("rstan")
# library("MASS")
library("dplyr")
library("RJSONIO")
library("ggplot2")
theme_set(theme_bw())
library("sp")

source("source_2016/common.R")

# Model from 2015
# s <- sflist2stanfit(readRDS("data_2015/s6list.rds")[1:8]) #; traceplot(s, "tau", inc_warmup=F)

# Model from 2016
s <- readRDS("data_2016/model_samples_debug_8chains_1000+1000t20_20160219.rds")

d <- readRDS("data_2016/d.rds")

if (F) {
  s
  traceplot(s, "LOmega", inc_warmup=F)
  traceplot(s, "LOmega2", inc_warmup=F)
  traceplot(s, "tau", inc_warmup=F)
  traceplot(s, "tau1", inc_warmup=F)
  traceplot(s, "tau2", inc_warmup=F)
  traceplot(s, "mean_beta", inc_warmup=F)
  traceplot(s, "df", inc_warmup=F)
  traceplot(s, "sigma", inc_warmup=F)
  traceplot(s, "ysigma", inc_warmup=F)
  #traceplot(s, "beta", inc_warmup=F)
  # etc.
}

# Low-level correlation matrix over price level, trend, etc.
# Is of general interest
Omega <- matrix(apply(apply(extract(s, "LOmega")[[1]], 1, function (m) m %*% t(m)), 1, mean), c(3, 3))
saveRDS(Omega, "data_2016/Omega.rds")
Omega1 <- matrix(apply(apply(extract(s, "LOmega1")[[1]], 1, function (m) m %*% t(m)), 1, mean), c(6, 6))
if (F) saveRDS(Omega1, "data_2016/Omega1.rds")

raise6 <- function (a) {
  dim.a <- dim(a); N <- length(dim.a)
  apply(a, 1:(N-1), function (v) c(v, rep(0, 6-dim.a[N]))) %>%
  aperm(c(2:N, 1))
} 
  
beta.prm.mean <- function (v) apply(extract(s, v)[[1]], c(2, 3), mean) %>% raise6
beta.prm <- function (v) extract(s, v)[[1]] %>% raise6

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

beta.names <- c("lprice", "trend", "quad", "k.lprice", "k.trend", "k.quad")

par.tbl <- function(d, v.name, b.name, name.postfix) 
  data.frame(levels(d[[v.name]]), beta.prm.mean(b.name)) %>% 
  setNames(c(v.name, 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df()

par.tbl.long <- function(d, v.name, b.name, name.postfix) {
  samples <- beta.prm(b.name)
  data.frame(expand.grid(1:dim(samples)[[1]], levels(d[[v.name]])), 
             array(samples, c(dim(samples)[[1]]*dim(samples)[[2]], dim(samples)[[3]]))) %>% 
  setNames(c("sample", v.name, 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df() }

mean.tbl.long <- function (name.postfix="4") 
  extract(s, "mean_beta")[[1]] %>% { 
    data.frame(sample=1:dim(.)[[1]], .) } %>% 
  setNames(c("sample", 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df() 

load("data_2016/pnro_data_20160215.RData")
pnro.area <- pnro.dat %>% transmute(pnro=pnro, log.density = -log(density_per_km2)/10) # FIXME: this is in two places
pnro <- pnro.area$pnro
n.samples <- length(extract(s, "lp__")[[1]])
# For NA pnro's in the model, look for upper level in the hierarchy and take beta1 etc.
res.long <- data.frame(pnro.area, level1 = l1(pnro), level2 = l2(pnro), level3 = l3(pnro)) %>% 
  merge(data.frame(sample=1:n.samples)) %>% 
  filter(is.finite(log.density)) %>%
  left_join(par.tbl.long(d, "pnro",   "beta",  ""),  by=c("pnro",   "sample")) %>%
  left_join(par.tbl.long(d, "level1", "beta1", "1"), by=c("level1", "sample")) %>% 
  left_join(par.tbl.long(d, "level2", "beta2", "2"), by=c("level2", "sample")) %>% 
  left_join(mean.tbl.long(                     "4"), by=c(          "sample")) %>% 
  mutate(pnro=pnro, 
            log.density = log.density,
            lprice=sum.0na(lprice, lprice1, lprice2, lprice4) + 
              sum.0na(k.lprice, k.lprice1, k.lprice2,  k.lprice4) * log.density, 
            trend=sum.0na(trend, trend1, trend2,  trend4) +
              sum.0na(k.trend, k.trend1, k.trend2,  k.trend4) * log.density, 
            quad=sum.0na(quad, quad1, quad2,  quad4) +
              sum.0na(k.quad, k.quad1, k.quad2,  k.quad4) * log.density
  ) %>%
  # Original unit is decade, for vars below it is year. 
  # d/d.yr lprice = trend + 2*quad*yr
  # d/(10*d.yr) lprice = trend/10 + 2*quad*yr/10
  # d^2/(10*d.yr)^2 lprice = 2*quad/100
  # trendi is as percentage / 100.
  # trendimuutos is as percentage units / 100 / year.
  mutate(hinta = exp(6 + lprice), trendi = trend/10, trendimuutos = 2*quad/100, 
         hinta2017 = exp(6 + lprice + trend*year2yr(2017) + quad*year2yr(2017)**2),
         trendi2017 = (trend + 2*quad*year2yr(2017))/10) %>%
  tbl_df()

res <- res.long %>% group_by(pnro, log.density) %>% 
 summarise(lprice = mean(lprice), 
           hinta2017=mean(hinta2017), trendi2017=mean(trendi2017), trendimuutos=mean(trendimuutos)) %>%
  ungroup()
saveRDS(res, "data_2016/pnro-hinnat_2016.rds")

res2080 <- res.long %>% group_by(pnro, log.density) %>% 
  summarise(lprice = mean(lprice), 
            hinta2017.20 = quantile(hinta2017, .2), 
            trendi2017.20 = quantile(trendi2017, .2), 
            trendimuutos.20 = quantile(trendimuutos, .2), 
            hinta2017.80 = quantile(hinta2017, .8), 
            trendi2017.80 = quantile(trendi2017, .8), 
            trendimuutos.80 = quantile(trendimuutos, .8) 
            ) %>%
  ungroup()


# was:
# write.table(res %>% select(-log.density),  "data_2016/pnro-hinnat.txt", row.names=F, quote=F)
write.table(res2080,  "data_2016/pnro-hinnat_20-80_2016.txt", row.names=F, quote=F)
saveRDS(res2080, "data_2016/pnro-hinnat_20-80_2016.rds")

# FIXME: exp(6 + lprice + trend*year2yr(2016) + quad*year2yr(2016)**2) in two places, 
# make a function.

## COMPUTE PREDICTIONS ########

years = 2005:2017
predictions <- 
  expand.grid(sample=unique(res.long$sample), 
              year=years, 
              pnro=unique(res.long$pnro)) %>% tbl_df %>% #head(10000) %>%
  left_join(res.long %>% select(pnro, sample, lprice, trend, quad), by=c("sample", "pnro")) %>%
  mutate(hinta = exp(6 + lprice + trend*year2yr(year) + quad*year2yr(year)**2)) %>%
  group_by(pnro, year) %>% 
  do(data.frame(hinta = mean(.$hinta), #hinta_sd = sd(.$hinta), 
                hinta10 = quantile(.$hinta, .1), 
                hinta25 = quantile(.$hinta, .25), 
                hinta50 = quantile(.$hinta, .5), 
                hinta75 = quantile(.$hinta, .75), 
                hinta90 = quantile(.$hinta, .9))) %>%
  ungroup() %>%
  left_join(d %>% select(pnro, year, obs_hinta=price, n_kaupat=n), by=c("year", "pnro"))

saveRDS(predictions, "data_2016/predictions_2016.rds")

# Compare predictions to those from year 2015
predictions %>%
  select(pnro, year, hinta) %>%
  left_join(readRDS("data_2015/predictions.rds") %>%
              select(pnro, year, hinta),
            by = c("pnro", "year")) %>%
  ggplot(aes(x=hinta.x, y=hinta.y)) + geom_point(aes(colour=factor(year)))
# Looks similar enough


## JSONs #############

res %>% plyr::dlply("pnro", function (i) list(hinta2017=i$hinta2017, 
                                              trendi2017=i$trendi2017, 
                                              trendimuutos=i$trendimuutos)) %>% 
  toJSON %>% writeLines("json_2016/trends.json")

predictions %>% group_by(pnro) %>% # filter(pnro %in% c("02940", "00100")) %>%
  plyr::d_ply("pnro", function (i) list(year=i$year, 
                                        hinta10=i$hinta10, 
                                        hinta25=i$hinta25, 
                                        hinta50=i$hinta50, 
                                        hinta75=i$hinta75, 
                                        hinta90=i$hinta90, 
                                        obs_hinta=i$obs_hinta, 
                                        n_kaupat=i$n_kaupat) %>% toJSON %>%
                writeLines(., paste("json_2016/predictions/", i$pnro[[1]], ".json",  sep=""))
  )

d %>% select(pnro, year, n) %>%
  tidyr::spread(year, n, fill=0) %>% 
  tidyr::gather(year, n, -pnro) %>% 
  { .[order(.$n),]} %>% 
  mutate(i=row_number()) %>% 
  ggplot(aes(x=i, y=n)) + 
  geom_line() + 
  scale_y_continuous(trans = "log1p", breaks=c(0, 6, 10, 100, 1000))

d %>% select(pnro, year, n)  %>% 
  group_by(pnro) %>% 
  summarise(n=sum(n))  %>% 
  { .[order(.$n),]} %>% 
  mutate(i=row_number()) %>% 
  ggplot(aes(x=i, y=n)) + 
  geom_line() + 
  scale_y_continuous(trans = "log1p", breaks=c(0, 6, 10, 100, 1000))



