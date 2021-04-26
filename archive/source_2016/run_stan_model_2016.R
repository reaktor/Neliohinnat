library("dplyr")
library("rstan")
library("parallel")
rstan_options(auto_write = TRUE)
options(mc.cores = 4)
source("source/common.R")

## PREPARE DATA FOR STAN ##########

# FIXME: log.density has wrong sign. 

# New data 'd' by Juuso
load("data_2016/pnro_data_20160215.RData")
d <- pnro.ashi.dat %>%
  # Compute NEGATIVE log.density because Janne
  mutate(log.density = -log(density_per_km2)/10) %>%
  #  mutate(log.density = (log(density_per_km2)-14)/10) # This would be close to d$log.density
  filter(!is.na(log.density) & !is.na(price)) %>%
  mutate(pnro=factor(pnro), year=as.numeric(as.character(year))) %>% # pnro has extra levels
  mutate(level1 = l1(pnro),
         level2 = l2(pnro),
         level3 = l3(pnro), 
         yr = year2yr(year), #(year - mean(unique(year)))/10,
         lprice = log(price)-6)

saveRDS(d, "data_2016/d.rds")

## RUN STAN #########

d <- readRDS("data_2016/d.rds")
# wtf <- function (d, cl, cu) data.frame(l=as.numeric(d[[cl]]), u=as.numeric(d[[cu]])) %>% unique %>% { .[order(.$l),]$u }

m <- stan_model(file="source/model_2016.stan")
s.f <- function (nchains=1, iter=2500, warmup=1000, thin=25, refresh=-1)
  sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), 
                                lprice=lprice, count=n, yr=yr, z=log.density,
                                pnro=as.numeric(pnro), 
                                l1=as.numeric(level1),
                                l2=as.numeric(level2))), 
           iter=iter, warmup=warmup, thin=thin, init=0, chains=nchains, cores= nchains, refresh=refresh, seed=4)

# Run single short chain for debugging
s <- s.f(1, iter=10, warmup=5, thin=1, refresh=1)

# Run four sbort chains in parallel for debugging
s <- s.f(4, iter=10, warmup=5, thin=1, refresh=1)

# Run eight long chains for final results
s <- s.f(nchains=8, iter=2000, warmup=1000, thin=20, refresh=50)
saveRDS(s, "data_2016/model_samples_debug_8chains_1000+1000t20_20160219.rds")
