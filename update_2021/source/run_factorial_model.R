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
d <- pnro.ashi.dat %>%
  # Compute NEGATIVE log.density because Janne
  mutate(log.density = -log(density_per_km2)/10) %>%
  mutate(pnro=factor(pnro),
         year=as.numeric(as.character(year))) %>% # pnro has extra levels
  mutate(level1 = l1(pnro),
         level2 = l2(pnro),
         level3 = l3(pnro), 
         yr = year2yr(year),
         lprice = log(price)-6) %>%
  mutate(c_male_share = men %>% nlogit(population),
         c_employed_share = employed %>% nlogit(population),
         c_unemployed_share = unemployed %>% nlogit(population),
         c_emploed_per_unemployed = regshare(employed, unemployed),
         c_refinement_jobs_per_jobs = refinement_jobs %>% nlogit(jobs),
         c_service_jobs_per_jobs = service_jobs %>% nlogit(jobs),
         c_primary_prod_jobs_per_jobs = primary_prod_jobs %>% nlogit(jobs),
         c_jobs_per_capita = regshare(jobs, population),
         c_educated_share = educated %>% nlogit(population),
         c_high_school_share = highschool_grads %>% nlogit(population),
         c_vocational_share = vocational_grads %>% nlogit(population),
         c_bachelor_share = bachelor_degrees %>% nlogit(population),
         c_master_share = master_degrees %>% nlogit(population),
         c_other_prop_ratio = other_properties %>% nlogit(properties),
         c_living_prop_ratio = living_properties %>% nlogit(properties),
         c_house_ratio = small_houses %>% nlogit(apartments),
         c_cottage_ratio = nlogit(cottages, cottages + properties),
         c_mean_income = stdna(mean_income),
         c_median_income = stdna(median_income),
         c_low_income_share = low_income %>% nlogit(population),
         c_mid_income_share = mid_income %>% nlogit(population),
         c_hi_income_share = hi_income %>% nlogit(population),
         c_intercept = 1
         )

STAN_INPUT = paste0(BASE_PATH, '/data/d_20210304.rds')
saveRDS(d, STAN_INPUT)

## RUN STAN #########

d <- readRDS(STAN_INPUT) %>% filter(!is.na(price))

covs = as.matrix(select(d, starts_with('c_')))
n_covs = dim(covs)[2]
FACTORIAL_MODEL = paste0(BASE_PATH, '/source/factorial_model.stan')
m <- stan_model(file=FACTORIAL_MODEL)
s.f <- function (nchains=1, iter=2500, warmup=1000, thin=25, refresh=-1)
  sampling(m, data=with(d,
                    list(N=nrow(d), M=nlevels(pnro),
                        M1=nlevels(level1), M2=nlevels(level2), 
                        lprice=lprice, count=n, yr=yr, z=log.density,
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
s <- s.f(nchains=4, iter=100, warmup=20, thin=1, refresh=1) #30min on my laptop
toc()

#saveRDS(s, paste0(BASE_PATH, '/data/model_samples_debug_4chains_100+20t1_20210129.rds'))



