library("dplyr")
library("rstan")
library("parallel")
library('tictoc')
rstan_options(auto_write = TRUE)
options(mc.cores = detectCores() - 1)

UPDATE_VERSION = 'update_2021'
BASE_PATH = paste0('./', UPDATE_VERSION)

source(paste0(BASE_PATH, '/source/common.R'))

## PREPARE DATA FOR STAN ##########

load(paste0(BASE_PATH, '/data/pnro_data_20210304.RData'))
d_covs = pnro.population %>%
  get_covariates(impute = T, include_intercept = T, level3_dummies = F) %>%
  full_join(data.frame(year = unique(pnro.ashi.dat$year)), by=character())
  
d = d_covs %>% left_join(pnro.ashi.dat, by = c('pnro', 'year', 'population')) %>%
  mutate_history_vars() %>% mutate(pnro = as.factor(pnro))
rm(pnro.ashi.dat, pnro.population, pnro.sp)

STAN_INPUT = paste0(BASE_PATH, '/data/d_20210304.rds')
saveRDS(d, STAN_INPUT)

## RUN STAN #########

d_pred <- readRDS(STAN_INPUT) %>% mutate(pnro_yr_ix = as.factor(paste0(pnro, year)))
d = d_pred %>% filter(!is.na(price))

covs = as.matrix(dplyr::select(d, starts_with('c_')))
covs_pred = as.matrix(dplyr::select(d_pred, starts_with('c_')))
n_covs = dim(covs)[2]
#NOMINAL_MODEL = paste0(BASE_PATH, '/source/nominal_model.stan')
NOMINAL_EMP_MODEL = paste0(BASE_PATH, '/source/models/nominal_emp_model.stan')
m <- stan_model(file=NOMINAL_EMP_MODEL)

s.f <- function (nchains=1, iter=2500, warmup=1000, thin=25, refresh=-1)
  sampling(m, data=c(with(d,
                    list(N=nrow(d), M=nlevels(pnro),
                        M1=nlevels(level1),
                        M2=nlevels(level2), 
                        lprice=lprice, count=n,
                        n_covs=n_covs,
                        n_years = max(year) - min(year) + 1,
                        pnro_yr_ix = as.numeric(pnro_yr_ix)
                        )),
                    with(d_pred,
                    list(N_pred=nrow(d_pred),
                         yr_pred=yr,
                         year_pred = year_ix,
                         pnro_pred=as.numeric(pnro), 
                         l1_pred=as.numeric(level1),
                         l2_pred=as.numeric(level2),
                         covs_pred=covs_pred
                         ))), 
           iter=iter, warmup=warmup, thin=thin, init=0,
           chains=nchains, cores= nchains, refresh=refresh)

# Run single short chain for debugging. Note "numerical problems" are ok
s <- s.f(1, iter=10, warmup=5, thin=1, refresh=1)

# Run four short chains in parallel for debugging
s <- s.f(4, iter=50, warmup=5, thin=1, refresh=1)

# # Run as long as sensible on a laptop to get first results
tic()
s <- s.f(nchains=4, iter=100, warmup=50, thin=1, refresh=1) #30min on my laptop
toc()

saveRDS(s, paste0(BASE_PATH, '/data/debug_nominal_empirical_model_samples.rds'))

# # Run eight long chains for final results
# s <- s.f(nchains=8, iter=2000, warmup=1000, thin=20, refresh=50)
# saveRDS(s, "data_2017/model_samples_debug_8chains_1000+1000t20_20170406.rds")

# Run eight long chains for final results
#s_long <- s.f(nchains=8, iter=10000, warmup=5000, thin=100, refresh=50)
#saveRDS(s_long, "data_2017/LONG_model_samples_8chains_5000+5000t100_20170412.rds")


