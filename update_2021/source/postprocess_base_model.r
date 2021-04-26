library("rstan")
library("dplyr")
library("RJSONIO")
library("ggplot2")
library("sp")

UPDATE_VERSION = 'update_2021'
BASE_PATH = paste0('./', UPDATE_VERSION)

source(paste0(BASE_PATH, '/source/common.R'))

STAN_INPUT = paste0(BASE_PATH, '/data/d_20210128.rds')
d <- readRDS(STAN_INPUT)

SAMPLES = paste0(BASE_PATH, '/data/long_model_samples_8chains_5000+5000t100_20210129.rds')
s = readRDS(SAMPLES)

F = TRUE

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
}

# Low-level correlation matrix over price level, trend, etc.
# Is of general interest
Omega <- matrix(apply(apply(extract(s, "LOmega")[[1]], 1, function (m) m %*% t(m)), 1, mean), c(3, 3))
saveRDS(Omega, paste0(BASE_PATH, "/data/Omega.rds"))
Omega1 <- matrix(apply(apply(extract(s, "LOmega1")[[1]], 1, function (m) m %*% t(m)), 1, mean), c(6, 6))
if (F) saveRDS(Omega, paste0(BASE_PATH, "/data/Omega1.rds"))

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
  trendi <- beta[,2]/YEAR_SCALE
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


# This is used only to get pnro density info
PNRO_DATA = paste0(BASE_PATH, '/data/pnro_data_20210128.RData')
load(PNRO_DATA)
rm(pnro.sp, pnro.ashi.dat)
pnro.area <- pnro.population %>% transmute(pnro=pnro, log.density = -log(density_per_km2)/10) # FIXME: this is in two places
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
  mutate(hinta = exp(6 + lprice), trendi = trend/YEAR_SCALE, trendimuutos = 2*quad/YEAR_SCALE/YEAR_SCALE, 
         hinta2020 = exp(6 + lprice + trend*year2yr(2020) + quad*year2yr(2020)**2),
         trendi2020 = (trend + 2*quad*year2yr(2020))/YEAR_SCALE) %>%
  tbl_df()

RES_LONG = paste0(BASE_PATH, '/data/pnro_res_long_2021.rds')
saveRDS(res.long, RES_LONG)
res.long = readRDS(RES_LONG)

res <- res.long %>% 
  group_by(pnro, log.density) %>% 
  dplyr::summarise(lprice = mean(lprice),
            trendi2020_q25 = quantile(trendi2020, 0.25),
            trendi2020_q75 = quantile(trendi2020, 0.75), 
            hinta2020_q25 = quantile(hinta2020, 0.25),
            hinta2020_q75 = quantile(hinta2020, 0.75),
            hinta2020 = mean(hinta2020),
            trendi2020 = mean(trendi2020),
            trendimuutos = mean(trendimuutos)) %>%
  ungroup() %>%
  mutate(trendi2020_luotettava = (trendi2020_q25*trendi2020_q75 > 0))


RES = paste0(BASE_PATH, '/data/pnro-hinnat_2021.rds')
saveRDS(res, RES)
res <- readRDS(RES)


## COMPUTE PREDICTIONS ########
years <- 2010:2020

predictions <- 
  expand.grid(sample=unique(res.long$sample), 
              year=years, 
              pnro=unique(res.long$pnro)) %>% tbl_df %>% #head(10000) %>%
  left_join(res.long %>% dplyr::select(pnro, sample, lprice, trend, quad), by=c("sample", "pnro")) %>%
  mutate(hinta = exp(6 + lprice + trend*year2yr(year) + quad*year2yr(year)**2)) %>%
  group_by(pnro, year) %>% 
  do(data.frame(hinta = mean(.$hinta), #hinta_sd = sd(.$hinta), 
                hinta10 = quantile(.$hinta, .1), 
                hinta25 = quantile(.$hinta, .25), 
                hinta50 = quantile(.$hinta, .5), 
                hinta75 = quantile(.$hinta, .75), 
                hinta90 = quantile(.$hinta, .9))) %>%
  ungroup() %>%
  left_join(d %>% dplyr::select(pnro, year, obs_hinta=price, n_kaupat=n), by=c("year", "pnro"))

PRED = paste0(BASE_PATH, '/data/predictions_2020.rds')
saveRDS(predictions, PRED)
predictions = readRDS(PRED)

## VALIDATION ########

# Tällä kannattaa tarkistella että prediktion osuvat yhteen datan kanssa. 
# Postinumeroita: parikkala 59130, haaga 00320, espoo lippajärvi 02940, pieksämäki 76100, tapiola 02100
single_pnro = "00640"
preds_tmp = predictions %>% filter(pnro==single_pnro) %>% tidyr::gather(q, y, -pnro, -year,  -n_kaupat) 
ggplot() + 
  geom_line(data=preds_tmp[preds_tmp$q!='obs_hinta', ],  aes(x=year, y=y, color=q)) + 
  geom_point(data=preds_tmp[preds_tmp$q=='obs_hinta', ], aes(x=year, y=y, color=q, size=n_kaupat)) +
  ggtitle(single_pnro)
 
# Compare predictions to those from year 2018
predictions %>%
  dplyr::select(pnro, year, hinta_2021 = hinta) %>%
  inner_join(readRDS("data_2018/predictions_2018.rds") %>%
               dplyr::select(pnro, year, hinta_2018 = hinta),
             by = c("pnro", "year")) %>%
  ggplot(aes(x=hinta_2021, y=hinta_2018)) + geom_point(aes(colour=factor(year)), alpha=0.5) 
# ggplot(aes(x=hinta_2021-hinta_2018)) + geom_histogram()


# For urbanisation analysis
res.long.narrow <- res.long %>% dplyr::select(pnro, lprice, trend, quad, sample)

yearly.trends <- 
  expand.grid(sample=unique(res.long.narrow$sample), 
              year=years, 
              pnro=unique(res.long.narrow$pnro)) %>% tbl_df %>%
  left_join(res.long.narrow) %>%
  mutate(trend.y = (trend + 2*quad*year2yr(year))/YEAR_SCALE) %>%
  group_by(pnro, year) %>%
  summarise(trend.y.mean=mean(trend.y), trend.y.median=median(trend.y))


## JSONs #############
library('plyr')

res %>% plyr::dlply("pnro", function (i) list(hinta2020=i$hinta2020, 
                                              trendi2020=i$trendi2020,
                                              trendi2020_min=unname(i$trendi2020_q25),
                                              trendi2020_max=unname(i$trendi2020_q75))) %>% 
  toJSON %>% writeLines(paste0(BASE_PATH, "/json/trends.json"))

predictions %>% group_by(pnro) %>%
  plyr::d_ply("pnro", function (i) list(year=i$year, 
                                        hinta10=i$hinta10, 
                                        hinta25=i$hinta25, 
                                        hinta50=i$hinta50, 
                                        hinta75=i$hinta75, 
                                        hinta90=i$hinta90, 
                                        obs_hinta=i$obs_hinta, 
                                        n_kaupat=i$n_kaupat) %>% toJSON %>%
                writeLines(., paste(BASE_PATH, "/json/predictions/", i$pnro[[1]], ".json",  sep=""))
  )

######
# Plots to show data scarcity
d %>% dplyr::select(pnro, year, n) %>%
  tidyr::spread(year, n, fill=0) %>% 
  tidyr::gather(year, n, -pnro) %>% 
  { .[order(.$n),]} %>% 
  dplyr::mutate(i=row_number()) %>% 
  ggplot(aes(x=i, y=n)) + 
  geom_line() + 
  xlab('Postal code - years') +
  ylab('Transactions')+
  scale_y_continuous(trans = "log1p", breaks=c(0, 6, 10, 100, 1000))

d %>% dplyr::select(pnro, year, n)  %>% 
  group_by(pnro) %>% 
  dplyr::summarize(n=sum(n))  %>% 
  { .[order(.$n),]} %>% 
  dplyr::mutate(i=row_number()) %>% 
  ggplot(aes(x=i, y=n)) + 
  geom_line() + 
  xlab('Postal codes') +
  ylab('Transactions')
  scale_y_continuous(trans = "log1p", breaks=c(0, 6, 10, 100, 1000))



  






