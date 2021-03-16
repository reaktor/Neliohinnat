library('tidyr')

# Center & scale for modeling 
YEAR_SCALE = 10
year2yr <- function (year) (year-2010)/YEAR_SCALE

# Prefixes for postal codes
prefix.factor <- function (pnro, n) as.factor(substr(pnro, 1, n))
l3 <- { . %>% prefix.factor(., 1) }
l2 <- { . %>% prefix.factor(., 2) }
l1 <- { . %>% prefix.factor(., 3) }

first.nna2 <- function (c1, c2)  ifelse(!is.na(c1), c1, c2)
first.nna <- function (...) Reduce(first.nna2, list(...))

sum.0na2 <- function (c1, c2)  ifelse(is.na(c1), 0, c1) + ifelse(is.na(c2), 0, c2)
sum.0na <- function (...) Reduce(sum.0na2, list(...))


std <- function (x) (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
nlogit <- function (n, N, prior=20) { 
  n = replace_na(n, 0)
  N = replace_na(N, 0)
  p <- (n + prior*mean(n, na.rm=T)/mean(N, na.rm=T))/(N + prior)
  std(log(p / (1-p)))
}
regshare <- function(x, y, prior=20) {
  x = replace_na(x, 0)
  y = replace_na(y, 0)
  p <- (x + prior*mean(x, na.rm=T)/mean(y, na.rm=T))/(y + prior)
  std(p)
}
stdna <- function (x) {
  x = replace_na(x, 0)
  (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
}

mutate_history_vars <- function(df){
    d = df %>%
      mutate(year=as.numeric(as.character(year)), 
             yr = year2yr(year),
             lprice = log(price)-6
      )
  return (d)
}

get_covariates <- function(df){
  d <- df %>%
    mutate(pnro=factor(pnro)) %>% 
    mutate(level1 = l1(pnro),
           level2 = l2(pnro),
           level3 = l3(pnro)) %>%
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
           c_hi_income_share = hi_income %>% nlogit(population)
    )
  return(d)
}