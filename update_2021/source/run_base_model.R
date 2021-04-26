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

load(paste0(BASE_PATH, '/data/pnro_data_20210128.RData'))
d <- pnro.ashi.dat %>%
  # Compute NEGATIVE log.density because Janne
  mutate(log.density = -log(density_per_km2)/10) %>%
  filter(!is.na(log.density) & !is.na(price)) %>%
  mutate(pnro=factor(pnro),
        year=as.numeric(as.character(year))) %>% # pnro has extra levels
  mutate(level1 = l1(pnro),
         level2 = l2(pnro),
         level3 = l3(pnro), 
         yr = year2yr(year),
         lprice = log(price)-6)

STAN_INPUT = paste0(BASE_PATH, '/data/d_20210128.rds')
saveRDS(d, STAN_INPUT)

## RUN STAN #########

d <- readRDS(STAN_INPUT)

BASE_MODEL = paste0(BASE_PATH, '/source/base_model.stan')
m <- stan_model(file=BASE_MODEL)
s.f <- function (nchains=1, iter=2500, warmup=1000, thin=25, refresh=-1)
  sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), 
                                lprice=lprice, count=n, yr=yr, z=log.density,
                                pnro=as.numeric(pnro), 
                                l1=as.numeric(level1),
                                l2=as.numeric(level2))), 
           iter=iter, warmup=warmup, thin=thin, init=0, chains=nchains, cores= nchains, refresh=refresh)

# Run single short chain for debugging. Note "numerical problems" are ok
s <- s.f(1, iter=10, warmup=5, thin=1, refresh=1)

# Run four short chains in parallel for debugging
s <- s.f(4, iter=50, warmup=5, thin=1, refresh=1)

# # Run as long as sensible on a laptop to get first results
tic()
s <- s.f(nchains=4, iter=100, warmup=20, thin=1, refresh=1) #30min on my laptop
toc()

saveRDS(s, paste0(BASE_PATH, '/data/model_samples_debug_4chains_100+20t1_20210129.rds'))



# # Run eight long chains for final results
# s <- s.f(nchains=8, iter=2000, warmup=1000, thin=20, refresh=50)
# saveRDS(s, "data_2017/model_samples_debug_8chains_1000+1000t20_20170406.rds")

# Run eight long chains for final results
#s_long <- s.f(nchains=8, iter=10000, warmup=5000, thin=100, refresh=50)
#saveRDS(s_long, "data_2017/LONG_model_samples_8chains_5000+5000t100_20170412.rds")
