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

#mcmc_intervals(s_long, pars=vars(starts_with('beta_')))

raise6 <- function (a) {
  dim.a <- dim(a); N <- length(dim.a)
  apply(a, 1:(N-1), function (v) c(v, rep(0, 6-dim.a[N]))) %>%
    aperm(c(2:N, 1))
} 

beta.prm.mean <- function (v) apply(rstan::extract(s, v)[[1]], c(2, 3), mean) %>% raise6
beta.prm <- function (v) rstan::extract(s, v)[[1]] %>% raise6

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
  rstan::extract(s, "mean_beta")[[1]] %>% { 
    data.frame(sample=1:dim(.)[[1]], .) } %>% 
  setNames(c("sample", 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df() 

get.cov.samples <- function(covs, s) {
  cov_mat = as.matrix( select(covs, starts_with('c_')) )
  beta_cov_mat = as.matrix(rstan::extract(s, 'beta_cov')[[1]])
  z_mat = cbind(pnro, as.data.frame(cov_mat %*% t(beta_cov_mat) ) ) %>%
    pivot_longer(cols = starts_with('V'),
                 names_to = 'sample', names_prefix = 'V',
                 values_to = 'factorial_cov')
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


# For NA pnro's in the model, look for upper level in the hierarchy and take beta1 etc.
n.samples <- length(rstan::extract(s, "lp__")[[1]])
res.long <- data.frame(pnro, level1 = l1(pnro), level2 = l2(pnro), level3 = l3(pnro)) %>% 
  merge(data.frame(sample=1:n.samples)) %>% 
  left_join(par.tbl.long(d, "pnro",   "beta",  ""),  by=c("pnro",   "sample")) %>%
  left_join(par.tbl.long(d, "level1", "beta1", "1"), by=c("level1", "sample")) %>% 
  left_join(par.tbl.long(d, "level2", "beta2", "2"), by=c("level2", "sample")) %>% 
  left_join(mean.tbl.long(                     "4"), by=c(          "sample")) %>% 
  left_join(get.cov.samples(covs, s), by=c('pnro', 'sample')) %>%
  mutate(pnro=pnro,
         lprice=sum.0na(lprice, lprice1, lprice2, lprice4) + 
           sum.0na(k.lprice, k.lprice1, k.lprice2,  k.lprice4) * factorial_cov, 
         trend=sum.0na(trend, trend1, trend2,  trend4) +
           sum.0na(k.trend, k.trend1, k.trend2,  k.trend4) * factorial_cov, 
         quad=sum.0na(quad, quad1, quad2,  quad4) +
           sum.0na(k.quad, k.quad1, k.quad2,  k.quad4) * factorial_cov
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