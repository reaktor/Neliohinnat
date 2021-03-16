library("rstan")
library("dplyr")
library("RJSONIO")
library("ggplot2")
library("sp")
library('bayesplot')

UPDATE_VERSION = 'update_2021'
BASE_PATH = paste0('./', UPDATE_VERSION)

source(paste0(BASE_PATH, '/source/common.R'))

STAN_INPUT = paste0(BASE_PATH, '/data/d_20210304.rds')
d <- readRDS(STAN_INPUT)

SAMPLES = paste0(BASE_PATH, '/data/debug_factorial_model_samples.rds')
s = readRDS(SAMPLES)


raise6 <- function (a, inc.covs) {
  if (inc.covs) {cols = 6}
  else {cols = 3}
  dim.a <- dim(a); N <- length(dim.a)
  apply(a, 1:(N-1), function (v) c(v, rep(0, cols-dim.a[N]))) %>%
    aperm(c(2:N, 1))
} 

beta.prm.mean <- function (v) apply(rstan::extract(s, v)[[1]], c(2, 3), mean) %>% raise6(inc.covs)
beta.prm <- function (v, inc.covs) rstan::extract(s, v)[[1]] %>% raise6(inc.covs)

beta.names <- c("lprice", "trend", "quad", "k.lprice", "k.trend", "k.quad")


par.tbl <- function(d, v.name, b.name, name.postfix) 
  data.frame(levels(d[[v.name]]), beta.prm.mean(b.name)) %>% 
  setNames(c(v.name, 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df()

par.tbl.long <- function(d, v.name, b.name, name.postfix, inc.covs=F) {
  samples <- beta.prm(b.name, inc.covs)
  if (!inc.covs) {beta.names = beta.names[1:3]}
  data.frame(expand.grid(1:dim(samples)[[1]], levels(d[[v.name]])), 
             array(samples, c(dim(samples)[[1]]*dim(samples)[[2]], dim(samples)[[3]]))) %>% 
    setNames(c("sample", v.name, 
               paste(beta.names, name.postfix, sep=""))) %>%
    tbl_df() }

mean.tbl.long <- function (name.postfix="4") 
  rstan::extract(s, "mean_beta")[[1]] %>%
  { data.frame(sample=1:dim(.)[[1]], .) } %>% 
  setNames(c("sample", 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df() 

get.cov.samples <- function(covs, s, b_cov_name = 'beta_cov', z_name = 'factorial_cov') {
  cov_mat = as.matrix( select(covs, starts_with('c_')) )
  beta_cov_mat = as.matrix(rstan::extract(s, b_cov_name)[[1]])
  z_mat = cbind(pnro, as.data.frame(cov_mat %*% t(beta_cov_mat) ) ) %>%
    pivot_longer(cols = starts_with('V'),
                 names_to = 'sample', names_prefix = 'V',
                 values_to = z_name)
  z_mat$sample = as.integer(z_mat$sample)
  return(z_mat)
}

# Fetch covariates for all postal areas
PNRO_DATA = paste0(BASE_PATH, '/data/pnro_data_20210304.RData')
load(PNRO_DATA)
rm(pnro.sp, pnro.ashi.dat)

covs = pnro.population %>%
  get_covariates() %>%
  select(pnro, starts_with('c_'))
pnro = covs$pnro

##### temp temp
covs_53 = covs %>% filter(pnro == '00530')

z_pnro = d %>% filter(!is.na(price)) %>% select(pnro)

z_mat = as.data.frame(t(rstan::extract(s, 'z')[[1]])) %>%
  mutate(pnro = z_pnro$pnro) %>% distinct() 


x = pivot_longer(z_mat, starts_with('V'), names_to = 'sample', names_prefix = 'V',
                 values_to = 'factorial_cov') %>% mutate(sample = as.numeric(sample))
z_dif = x %>% left_join(get.cov.samples(covs, s, 'beta_cov', 'factorial_cov'), by=c('pnro', 'sample')) %>%
  mutate(diff = factorial_cov.x - factorial_cov.y) %>% filter(pnro == '00530')
hist(z_dif$factorial_cov.y)

z_dif %>% filter(pnro == '00530') %>% ggplot() + geom_histogram(aes(x = diff))


# For NA pnro's in the model, look for upper level in the hierarchy and take beta1 etc.
n.samples <- length(rstan::extract(s, "lp__")[[1]])
res.long <- data.frame(pnro,  level1 = l1(pnro)) %>% 
  merge(data.frame(sample=1:n.samples)) %>% 
  left_join(par.tbl.long(d, "pnro",   "beta",  ""),  by=c("pnro",   "sample")) %>%
  left_join(par.tbl.long(d, "level1", "beta1", "1"), by=c("level1", "sample")) %>% 
  #left_join(par.tbl.long(d, "level2", "beta2", "2"), by=c("level2", "sample")) %>% 
  left_join(mean.tbl.long(                     "4"), by=c(          "sample")) %>% 
  left_join(get.cov.samples(covs, s, 'beta_cov', 'factorial_cov'), by=c('pnro', 'sample')) %>%
  left_join(get.cov.samples(covs, s, 'beta_cov_yr', 'factorial_cov_yr'), by=c('pnro', 'sample')) %>%
  left_join(get.cov.samples(covs, s, 'beta_cov_yr2', 'factorial_cov_yr2'), by=c('pnro', 'sample')) %>%
  mutate(pnro=pnro,
         lprice=sum.0na(lprice,  lprice1, lprice4) + 
           sum.0na(k.lprice4) * factorial_cov, 
         trend=sum.0na(trend,  trend1,  trend4) +
           sum.0na(k.trend4) * factorial_cov_yr, 
         quad=sum.0na(quad,  quad1,  quad4) +
           sum.0na(k.quad4) * factorial_cov_yr2
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

#######################
RES_LONG = paste0(BASE_PATH, '/data/pnro_res_long_2021.rds')
saveRDS(res.long, RES_LONG)
res.long = readRDS(RES_LONG)


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



## VALIDATION ########

# Tällä kannattaa tarkistella että prediktion osuvat yhteen datan kanssa. 
# Postinumeroita: parikkala 59130, haaga 00320, espoo lippajärvi 02940, pieksämäki 76100, tapiola 02100
single_pnro = "00320"
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
