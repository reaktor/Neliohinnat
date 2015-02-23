library(dplyr)
library(rstan)
library(parallel)

source("source/common3.R")

# FIXME: log.density has wrong sign. 

d <- readRDS("data/statfi_ashi_pnro_processed_2005-2014_20141219.rds") %>% 
  left_join(pnro.area, by="pnro") %>%
  filter(!is.na(log.density) & !is.na(price)) %>%
  tbl_df() %>% 
  mutate(pnro=factor(pnro), year=as.numeric(as.character(year))) %>% # pnro has extra levels
  mutate(level1 = l1(pnro),
         level2 = l2(pnro),
         level3 = l3(pnro), 
         yr = year2yr(year),
         lprice = log(price)-6)

saveRDS(d, "data/d.rds")

wtf <- function (d, cl, cu) data.frame(l=as.numeric(d[[cl]]), u=as.numeric(d[[cu]])) %>% unique %>% { .[order(.$l),]$u }

m <- stan_model(file="source/m3i.stan")
s.f <- 
  function (i, iter=2500, warmup=1000, thin=25, refresh=-1)
  sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), M3=nlevels(level3),
                                   lprice=lprice, count=n, yr=yr, z=log.density,
                                   pnro=as.numeric(pnro), 
                                   l1=wtf(d, "pnro", "level1"), 
                                   l2=wtf(d, "level1", "level2"), 
                                   l3=wtf(d, "level2", "level3"))),
              iter=iter, warmup=warmup, thin=thin, init=0, chains=1, refresh=refresh, seed=4, chain_id=i)

if (T)
  message("No parallel long chains, only the debug chain.")
  s <- s.f(0, iter=500, warmup=250, thin=1, refresh=1)
else {
  s.list <- mclapply(1:4, mc.cores = 4, s.f)
  s <- sflist2stanfit(s.list) 
}


saveRDS(s, "s4.rds")
