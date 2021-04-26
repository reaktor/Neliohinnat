library("dplyr")
library("rstan")
library("parallel")
rstan_options(auto_write = TRUE)
options(mc.cores = 4)
source("source_2017/common_2017.R")

## PREPARE DATA FOR STAN ##########

# FIXME: log.density has wrong sign. 

# New data 'd' by Juuso
load("data_2017/pnro_data_20170405.RData")
d <- pnro.ashi.dat %>%
  # Compute NEGATIVE log.density because Janne
  mutate(log.density = -log(density_per_km2)/10) %>%
  #  mutate(log.density = (log(density_per_km2)-14)/10) # This would be close to d$log.density
  filter(!is.na(log.density) & !is.na(price)) %>%
  mutate(pnro=factor(pnro), year=as.numeric(as.character(year))) %>% # pnro has extra levels
  mutate(level1 = l1(pnro),
         level2 = l2(pnro),
         level3 = l3(pnro), 
         yr = year2yr(year),
         lprice = log(price)-6)

saveRDS(d, "data_2017/d_20170412.rds")

## RUN STAN #########

d <- readRDS("data_2017/d_20170412.rds")
# wtf <- function (d, cl, cu) data.frame(l=as.numeric(d[[cl]]), u=as.numeric(d[[cu]])) %>% unique %>% { .[order(.$l),]$u }

m <- stan_model(file="source_2017/model_2017.stan")
s.f = function (nchains=1, iter=2500, warmup=1000, thin=25, refresh=-1)
  sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), 
                                lprice=lprice, count=n, yr=yr, z=log.density,
                                pnro=as.numeric(pnro), 
                                l1=as.numeric(level1),
                                l2=as.numeric(level2))), 
           iter=iter, warmup=warmup, thin=thin, init=0, chains=nchains, cores= nchains, refresh=refresh, seed=4)

# Run single short chain for debugging
s <- s.f(1, iter=10, warmup=5, thin=1, refresh=1)

# Run four sbort chains in parallel for debugging
s <- s.f(4, iter=50, warmup=5, thin=1, refresh=1)

# Note "numerical problems" are ok

# # Run as long as sensible on a laptop to get first results
# s <- s.f(nchains=4, iter=200, warmup=100, thin=2, refresh=5)
# saveRDS(s, "data_2017/model_samples_debug_4chains_100+100t2_20170405.rds")

## Run in AWS ############

# # Run eight long chains for final results
# s <- s.f(nchains=8, iter=2000, warmup=1000, thin=20, refresh=50)
# saveRDS(s, "data_2017/model_samples_debug_8chains_1000+1000t20_20170406.rds")

# Run eight long chains for final results
s_long <- s.f(nchains=8, iter=10000, warmup=5000, thin=100, refresh=50)
saveRDS(s_long, "data_2017/LONG_model_samples_8chains_5000+5000t100_20170412.rds")

## Run shorter model for years 2010-2016 shorter years ################

d_short <- d %>% filter(year >= 2010)
s.f_short <- function (nchains=1, iter=2500, warmup=1000, thin=25, refresh=-1)
  sampling(m, data=with(d_short, list(N=nrow(d_short), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), 
                                lprice=lprice, count=n, yr=yr, z=log.density,
                                pnro=as.numeric(pnro), 
                                l1=as.numeric(level1),
                                l2=as.numeric(level2))), 
           iter=iter, warmup=warmup, thin=thin, init=0, chains=nchains, cores= nchains, refresh=refresh, seed=4)

s_short <- s.f_short(4, iter=50, warmup=5, thin=1, refresh=1)

# Run eight long chains for final results
s_short <- s.f_short(nchains=8, iter=10000, warmup=5000, thin=100, refresh=50)
saveRDS(s_short, "data_2017/SHORT_model_samples_8chains_5000+5000t100_20170412.rds")


# ## Alternative, run with years 2010-2016 ###########
# 
# load("data_2017/pnro_data_20170405.RData")
# d <- pnro.ashi.dat %>%
#   filter(year >= 2010) %>%
#   # Compute NEGATIVE log.density because Janne
#   mutate(log.density = -log(density_per_km2)/10) %>%
#   #  mutate(log.density = (log(density_per_km2)-14)/10) # This would be close to d$log.density
#   filter(!is.na(log.density) & !is.na(price)) %>%
#   mutate(pnro=factor(pnro), year=as.numeric(as.character(year))) %>% # pnro has extra levels
#   mutate(level1 = l1(pnro),
#          level2 = l2(pnro),
#          level3 = l3(pnro), 
#          yr = year2yr(year),
#          lprice = log(price)-6)
# saveRDS(d, "data_2017/d_2010-2016.rds")
# d <- readRDS("data_2017/d_2010-2016.rds")
# m <- stan_model(file="source_2017/model_2017.stan")
# s.f = function (nchains=1, iter=2500, warmup=1000, thin=25, refresh=-1)
#   sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), 
#                                 lprice=lprice, count=n, yr=yr, z=log.density,
#                                 pnro=as.numeric(pnro), 
#                                 l1=as.numeric(level1),
#                                 l2=as.numeric(level2))), 
#            iter=iter, warmup=warmup, thin=thin, init=0, chains=nchains, cores= nchains, refresh=refresh, seed=4)
# 
# # Run eight long chains for final results
# s <- s.f(nchains=8, iter=2000, warmup=1000, thin=20, refresh=50)
# saveRDS(s, "data_2017/model_2010-2016_samples_debug_8chains_1000+1000t20_20170407.rds")
