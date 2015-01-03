library(dplyr)
library(rstan)
library(parallel)

first.nna2 <- function (c1, c2)  ifelse(!is.na(c1), c1, c2)
first.nna <- function (...) Reduce(first.nna2, list(...))
  
prefix.factor <- function (pnro, n) as.factor(substr(pnro, 1, n))
l3 <- { . %>% prefix.factor(., 1) }
l2 <- { . %>% prefix.factor(., 2) }
l1 <- { . %>% prefix.factor(., 3) }

population <- read.csv("data/vakiluku_posnro_2012_proper.csv", 
                       sep=";", header=T, colClasses="character") %>%
  transmute(pnro=Postinumero, log.population=log(as.numeric(vakiluku)))

# Span the space on pnro's form the spatial data.
pnro.area <- local({
  load("data/pnro_spatial_epsg2393.RData") # And pnro.sp magically appears (FIXME: rds...)
  data.frame(pnro=pnro.sp.alt$pnro, log.area=log(pnro.sp.alt@data$area.m2)-18) }) %>%
  inner_join(population, by="pnro") %>% 
  mutate(log.density = (log.area - log.population)/10)

d <- readRDS("data/statfi_ashi_pnro_processed_2005-2014_20141219.rds") %>% 
  left_join(pnro.area, by="pnro") %>%
  filter(!is.na(log.density) & !is.na(price)) %>%
  tbl_df() %>% 
  mutate(pnro=factor(pnro), year=as.numeric(as.character(year))) %>% # pnro has extra levels
  mutate(level1 = l1(pnro),
         level2 = l2(pnro),
         level3 = l3(pnro), 
         yr = (year-mean(year))/10,
         lprice = log(price)-6)

saveRDS(d, "data/d.rds")

wtf <- function (d, cl, cu) data.frame(l=as.numeric(d[[cl]]), u=as.numeric(d[[cu]])) %>% unique %>% { .[order(.$l),]$u }

m <- stan_model(file="source/m3.stan")
s.f <- 
  function (i)
  sampling(m, data=with(d, list(N=nrow(d), M=nlevels(pnro), M1=nlevels(level1), M2=nlevels(level2), M3=nlevels(level3),
                                   lprice=lprice, count=n, yr=yr, z=log.density,
                                   pnro=as.numeric(pnro), 
                                   l1=wtf(d, "pnro", "level1"), 
                                   l2=wtf(d, "level1", "level2"), 
                                   l3=wtf(d, "level2", "level3"))),
              iter=2500, warmup=1000, thin=25, init=0, chains=1, refresh=-1, seed=4, chain_id=i)

s.list <- mclapply(1:4, mc.cores = 4, s.f)
s <- sflist2stanfit(s.list)          

saveRDS(s, "s3.rds")
