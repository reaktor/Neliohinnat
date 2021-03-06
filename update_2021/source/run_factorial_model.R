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
  get_covariates(impute = T)
d = pnro.ashi.dat %>% mutate_history_vars() %>% left_join(d_covs) %>%
  mutate(pnro = as.factor(pnro))

STAN_INPUT = paste0(BASE_PATH, '/data/d_20210304.rds')
saveRDS(d, STAN_INPUT)

## RUN STAN #########

d <- readRDS(STAN_INPUT) %>% filter(!is.na(price)) %>% droplevels()

covs = as.matrix(select(d, starts_with('c_')))
n_covs = dim(covs)[2]
FACTORIAL_MODEL = paste0(BASE_PATH, '/source/factorial_model.stan')
m <- stan_model(file=FACTORIAL_MODEL)

s.f <- function (nchains=1, iter=2500, warmup=1000, thin=25, refresh=-1)
  sampling(m, data=with(d,
                    list(N=nrow(d), M=nlevels(pnro),
                        M1=nlevels(level1),
                        M2=nlevels(level2), 
                        lprice=lprice, count=n, yr=yr,
                        pnro=as.numeric(pnro), 
                        l1=as.numeric(level1),
                        l2=as.numeric(level2),
                        ncovs=n_covs,
                        covs=covs
                        )), 
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

traceplot(s, 'beta_cov_yr', inc_warmup=F)

saveRDS(s, paste0(BASE_PATH, '/data/debug_2_factorial_model_samples.rds'))

# # Run eight long chains for final results
# s <- s.f(nchains=8, iter=2000, warmup=1000, thin=20, refresh=50)
# saveRDS(s, "data_2017/model_samples_debug_8chains_1000+1000t20_20170406.rds")

# Run eight long chains for final results
#s_long <- s.f(nchains=8, iter=10000, warmup=5000, thin=100, refresh=50)
#saveRDS(s_long, "data_2017/LONG_model_samples_8chains_5000+5000t100_20170412.rds")


