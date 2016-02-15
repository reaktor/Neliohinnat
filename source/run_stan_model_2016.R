library("dplyr")
library("rstan")
library("parallel")

# Auxiliary helper functions
# source("source/common3.R")
prefix.factor <- function (pnro, n) as.factor(substr(pnro, 1, n))
l3 <- { . %>% prefix.factor(., 1) }
l2 <- { . %>% prefix.factor(., 2) }
l1 <- { . %>% prefix.factor(., 3) }
# year2yr <- function (year) (year-2009.406)/10

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
         yr = year - mean(unique(year)), #year2yr(year),
         lprice = log(price)-6)

saveRDS(d, "data_2016/d.rds")

# wtf <- function (d, cl, cu) data.frame(l=as.numeric(d[[cl]]), u=as.numeric(d[[cu]])) %>% unique %>% { .[order(.$l),]$u }

m <- stan_model(file="source/model_2016.stan")
s.f <- function (i, iter=2500, warmup=1000, thin=25, refresh=-1)
  sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), 
                                lprice=lprice, count=n, yr=yr, z=log.density,
                                pnro=as.numeric(pnro), 
                                l1=as.numeric(level1),
                                l2=as.numeric(level2))), 
           iter=iter, warmup=warmup, thin=thin, init=0, chains=1, refresh=refresh, seed=4, chain_id=i)

if (F) {
  message("No parallel long chains, only the debug chain.")
  s <- s.f(0, iter=10, warmup=5, thin=1, refresh=1)
  s <- s.f(0, iter=500, warmup=250, thin=1, refresh=1)
} else {
  s.list <- mclapply(1:8, mc.cores = 8, s.f, iter=2000, warmup=1000, thin=20)
  if (F) s <- sflist2stanfit(s.list) 
}


saveRDS(s.list, "data_2016/model_samples_list.rds")
