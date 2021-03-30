library("rstan")
library("dplyr")
library("RJSONIO")
library("ggplot2")
library("sp")
library('bayesplot')

UPDATE_VERSION = 'update_2021'
BASE_PATH = paste0('./', UPDATE_VERSION)

source(paste0(BASE_PATH, '/source/common.R'))

STAN_INPUT = paste0(BASE_PATH, '/data/d_20210304.rds')
d <- readRDS(STAN_INPUT) 

SAMPLES = paste0(BASE_PATH, '/data/debug_nominal_empirical_model_samples.rds')
s = readRDS(SAMPLES)

mcmc_intervals(s, pars = vars(starts_with('ysigma')))

res.long = cbind(pnro = d$pnro, year = d$year, obs_price = d$price,
                 population = d$population,
                 n = d$n,
                 as.data.frame(t(rstan::extract(s, 'pred_mean')[[1]]))) %>%
  filter(population > 0) %>%
  pivot_longer(cols = starts_with('V'),names_to = 'sample', names_prefix = 'V',
               values_to = 'pred') %>%
  mutate(pred = exp(6 + pred)) %>%
  group_by(pnro, sample) %>%
  mutate(delta = pred - lag(pred, default = first(pred), order_by = sample)) %>%
  ungroup() %>%
  mutate(trend = delta/(pred - delta) ) %>% filter(pnro != '70210')

res = res.long %>% filter(year == '2020') %>% group_by(pnro) %>%
  dplyr::summarise(trend2020_q25 = quantile(trend, 0.25),
                   trend2020_q75 = quantile(trend, 0.75),
                   price2020_q25 = quantile(pred, 0.25),
                   price2020_q75 = quantile(pred, 0.75),
                   price2020 = mean(pred),
                   trend2020 = mean(trend),
                   population = first(population)) %>%
  ungroup() %>%
  mutate(trend2020_reliable = (trend2020_q25*trend2020_q75 > 0))

predictions = res.long %>%
  group_by(pnro, year) %>% 
  do(data.frame(price = mean(.$pred),
                price10 = quantile(.$pred, .1), 
                price25 = quantile(.$pred, .25), 
                price50 = quantile(.$pred, .5), 
                price75 = quantile(.$pred, .75), 
                price90 = quantile(.$pred, .9))) %>%
  ungroup() %>%
  left_join(d %>% dplyr::select(pnro, year, obs_price=price, n_obs=n), by=c("year", "pnro"))

RES = paste0(BASE_PATH, '/data/pnro-hinnat_nominal_debug_2021.rds')
saveRDS(res, RES)
res <- readRDS(RES)

### COVARIATES #####
get.cov.betas <- function(d, s) {
  covs = d %>% dplyr::select(starts_with('c_'))
  tmp = rstan::extract(s, pars="beta_year")[[1]] %>% reshape2::melt() %>% setNames(c("i", "year", "var", "beta")) %>% 
   mutate(var=colnames(covs)[var]) %>% as_tibble() %>%
   ggplot(aes(x=year, y=beta, group=i)) + 
   geom_line(alpha=.2) + geom_hline(yintercept=0, color=I("red"), alpha=I(.5)) + facet_wrap(~ var, scales = 'free_y') +
   theme_bw(12)
  tmp
}


get.cov.betas(d, s)

## VALIDATION ########
mcmc_intervals(s, pars = vars(starts_with('df')))
mcmc_intervals(s, pars = vars(starts_with('sigma'), starts_with('ysigma')))

# Tällä kannattaa tarkistella että prediktion osuvat yhteen datan kanssa. 
# Postinumeroita: parikkala 59130, haaga 00320, espoo lippajärvi 02940, pieksämäki 76100, tapiola 02100
single_pnro = "00530"
preds_tmp = predictions %>% filter(pnro==single_pnro) %>% tidyr::gather(q, y, -pnro, -year,  -n_obs) 
ggplot() + 
  geom_line(data=preds_tmp[preds_tmp$q!='obs_price', ],  aes(x=year, y=y, color=q)) + 
  geom_point(data=preds_tmp[preds_tmp$q=='obs_price', ], aes(x=year, y=y, color=q, size=n_obs)) +
  ggtitle(single_pnro)

# Compare predictions to those from year 2018
tmp = predictions %>%
  dplyr::select(pnro, year, update_2021 = price) %>%
  inner_join(readRDS("archive/data_2018/predictions_2018.rds") %>%
               dplyr::select(pnro, year, update_2018 = hinta),
             by = c("pnro", "year")) %>%
  mutate(diff = update_2021 - update_2018) %>%
  ggplot(aes(x=update_2021, y=update_2018)) + 
  geom_point(aes(colour=factor(year)), alpha=0.5) + ggtitle('Prediction updates compared')
tmp
# ggplot(aes(x=hinta_2021-hinta_2018)) + geom_histogram()



## JSONs #############
library('plyr')

res %>% plyr::dlply("pnro", function (i) list(hinta2020=i$price2020, 
                                              trendi2020=i$trend2020,
                                              trendi2020_min=unname(i$trend2020_q25),
                                              trendi2020_max=unname(i$trend2020_q75))) %>% 
  toJSON %>% writeLines(paste0(BASE_PATH, "/json/trends.json"))

predictions %>% group_by(pnro) %>%
  plyr::d_ply("pnro", function (i) list(year=i$year, 
                                        hinta10=i$price10, 
                                        hinta25=i$price25, 
                                        hinta50=i$price50, 
                                        hinta75=i$price75, 
                                        hinta90=i$price90, 
                                        obs_hinta=i$obs_price, 
                                        n_kaupat=i$n_obs) %>% toJSON %>%
                writeLines(., paste(BASE_PATH, "/json/predictions/", i$pnro[[1]], ".json",  sep=""))
  )

######
# Plots to show data scarcity
d %>% dplyr::select(pnro, year, n) %>%
  tidyr::spread(year, n, fill=0) %>% 
  tidyr::gather(year, n, -pnro) %>% 
  { .[order(.$n),]} %>% 
  dplyr::mutate(i=row_number()) %>% 
  ggplot(aes(x=i, y=n)) + 
  geom_line() + 
  xlab('Postal code - years') +
  ylab('Transactions')+
  scale_y_continuous(trans = "log1p", breaks=c(0, 6, 10, 100, 1000))

d %>% dplyr::select(pnro, year, n)  %>% 
  group_by(pnro) %>% 
  dplyr::summarize(n=sum(n))  %>% 
  { .[order(.$n),]} %>% 
  dplyr::mutate(i=row_number()) %>% 
  ggplot(aes(x=i, y=n)) + 
  geom_line() + 
  xlab('Postal codes') +
  ylab('Transactions')
scale_y_continuous(trans = "log1p", breaks=c(0, 6, 10, 100, 1000))
