library("dplyr")
library("rstan")
library("parallel")
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

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
s <- s.f(8, iter=2000, warmup=1000, thin=20, refresh=10)

saveRDS(s, "data_2016/model_samples_debug_8chains_1000+1000t20_20160218.rds")

## DEBUGGING ########

s <- readRDS("data_2016/model_samples_debug_500+500t10_20160217.rds")



# The following numerical problems occured the indicated number of times after warmup on chain 0
# count
# Exception thrown at line 54: student_t_log: Scale parameter[1] is inf, but must be finite!               2
# Exception thrown at line 54: student_t_log: Degrees of freedom parameter is inf, but must be finite!     1
# When a numerical problem occurs, the Metropolis proposal gets rejected.
# However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
# Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
# Warning messages:
#   1: There were 5 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. 
# 2: Examine the pairs() plot to diagnose sampling problems

# 
# 
# # The final run
# if (F) {
#   message("No parallel long chains, only the debug chain.")
#   s <- s.f(0, iter=10, warmup=5, thin=1, refresh=1)
#   s <- s.f(0, iter=500, warmup=250, thin=1, refresh=1)
# } else {
#   s.list <- mclapply(1:8, mc.cores = 8, s.f, iter=2000, warmup=1000, thin=20)
#   if (F) s <- sflist2stanfit(s.list) 
# }
# 
# 
# saveRDS(s.list, "data_2016/model_samples_list.rds")
