library(dplyr)
library(ggplot2)
library(rstan)


UPDATE_VERSION = 'update_2021'
BASE_PATH = paste0('../../', UPDATE_VERSION)

source(paste0(BASE_PATH, '/source/common.R'))

## PREPARE DATA FOR STAN ##########

load(paste0(BASE_PATH, '/data/pnro_data_20210304.RData'))
d_covs = pnro.population %>%
  get_covariates(impute = T, include_intercept = T, level3_dummies = F) %>%
  full_join(data.frame(year = unique(pnro.ashi.dat$year)), by=character())

fit <- readRDS("models/nom_emp_samples_8chains_1000+1000t20_20210330.rds")

STAN_INPUT = paste0(BASE_PATH, '/data/d_20210304.rds')
d <- readRDS(STAN_INPUT) 

d2 <- cbind(pnro = d$pnro, year = d$year, 
            as.data.frame(t(rstan::extract(fit, 'pred_mean')[[1]]))) %>% as_tibble

sort((summary(fit)$summary)[,"Rhat"], decr=T)[1:10]
sort((summary(fit)$summary)[,"n_eff"], decr=F)[1:10]


d_covs_pure <- d_covs %>% select(matches("^c_"))

rstan::extract(fit, pars="beta_year")[[1]] %>% reshape2::melt() %>% setNames(c("i", "year", "var", "beta")) %>% 
  mutate(var=colnames(d_covs_pure)[var]) %>% 
  filter(var != "c_intercept") %>%
  as_tibble() %>% filter(i%%10==0) %>% 
  ggplot(aes(x=year, y=beta, group=i)) + 
  geom_line(alpha=.2) + geom_hline(yintercept=0, color=I("red"), alpha=I(.5)) + facet_wrap(~ var) 

rstan::extract(fit, pars="beta_year")[[1]] %>% reshape2::melt() %>% setNames(c("i", "year", "var", "beta")) %>% 
  mutate(var=colnames(d_covs_pure)[var]) %>% 
  as_tibble() %>% filter(year %in% c(10, 11)) %>% #filter(i%%10==0) %>% 
  tidyr::pivot_wider(names_from="year", values_from="beta", names_prefix="y") %>%
  mutate(change_19_20=y11-y10) %>%
  ggplot(aes(y=var, x=change_19_20)) + geom_point(alpha=.02) + geom_vline(xintercept = 0, color="red")

rstan::extract(fit, pars="beta_year")[[1]] %>% reshape2::melt() %>% setNames(c("i", "year", "var", "beta")) %>% 
  mutate(var=colnames(d_covs_pure)[var]) %>% 
  filter(var != "c_intercept") %>%
  as_tibble() %>% filter(year %in% c(10, 11)) %>% #filter(i%%10==0) %>% 
  tidyr::pivot_wider(names_from="year", values_from="beta", names_prefix="y") %>%
  mutate(change_19_20=y11-y10) %>%
  #group_by(var) %>%
  #mutate(z_change = abs(qnorm(mean(change_19_20>0)))) %>%
  #mutate(z_effect = .01*(1-abs(mean(y11>0)))) %>%
  ggplot(aes(y=change_19_20, x=y11,  alpha=I(.1))) +  geom_point(size=.1) +
  geom_vline(xintercept = 0, color="red", alpha=.5) +
  geom_hline(yintercept = 0, color="red", alpha=.5) +
  facet_wrap(~ var) + #+ scale_color_gradient(low="black", high="red")
  theme_minimal()


# SVD for covariates x years

cov_sol <- rstan::extract(fit, pars="beta_year")[[1]] %>% 
  apply(c(2, 3), mean) %>% 
  { svd(scale(.[,-23], scale=F)) }

d_cov_loadings <- cov_sol$u[,c(1, 2)] %>% data.frame() %>% mutate(year=2010:2020) 

rot1 <- coef(lm(year ~ X1 + X2, data=d_cov_loadings))[-1] %>% {./sqrt(sum(. * .))}
rot2 <- c(-rot1[2], rot1[1]) 
rot12 <- rbind(rot1, rot2)

d_cov_loadings2 <- cov_sol$u[,c(1, 2)] %*% solve(rot12) %>% scale %>% 
  data.frame() %>% setNames(c("X1", "X2")) %>% mutate(year=2010:2020) 

d_cov_loadings2 %>% ggplot(aes(x=X1, y=-X2, label=year)) + geom_path() + geom_label() + 
  xlab("PC1: \"Urbanisation/centralisation index\"") + ylab("PC2: \"Suburb index\"") + theme_classic()

#> plot((cov_sol$v[,c(1, 2)] %*% rot12)[,1], type="l")
#> plot((cov_sol$v[,c(1, 2)] %*% rot12)[,2], type="l")

pnro_scores <- as.matrix(d_covs_pure)[,-23] %*% (cov_sol$v[,c(1, 2)] %*% rot12) 
d4 <- d2 %>% mutate(X1=pnro_scores[,1], X2=pnro_scores[,2]) %>%
  filter(year==2020) 

clipclip <- function (x) pmax(-2, pmin(x, 2))

pnro_sf %>% left_join(d4) %>% mutate(X1 = clipclip(X1), X2=clipclip(X2)) %>%
  #filter(substr(pnro, 1, 2) %in% c("00", "01", "02")) %>%
  #filter(substr(pnro, 1, 1) %in% c("0", "2", "3")) %>%
  ggplot(aes(fill=X2)) + geom_sf(size=.1) + scale_fill_viridis_c(na.value="#00000000")

# We need to do the same for samples, to get confidence intervals.

cov_sols <- apply(rstan::extract(fit, pars="beta_year")[[1]], 1, function (x) svd(scale(x[,-23], scale=F)))

sol2df <- function (cov_sol, target_2 = d_cov_loadings2$X2) { 
  res <- cov_sol$u[,c(1, 2)] %*% solve(rot12) %>% scale %>% 
         data.frame() %>% setNames(c("X1", "X2")) %>% 
         mutate(year=2010:2020) 
  if (sum(target_2*res$X2)<0) res$X2 <- -res$X2
  res }

lapply(cov_sols, sol2df) %>%  bind_rows %>% 
  ggplot(aes(x=X1, y=X2)) + geom_point(alpha=.2, size=.2) + facet_wrap(~ year)

lapply(seq_along(cov_sols), 
       function (i) sol2df(cov_sols[[i]]) %>% mutate(sample=i)) %>%  bind_rows %>% 
  ggplot(aes(x=year, y=X2, group=sample)) + 
  geom_line(alpha=.05) + geom_point(alpha=.05)

lapply(seq_along(cov_sols), 
       function (i) sol2df(cov_sols[[i]]) %>% mutate(sample=i)) %>%  bind_rows %>% group_by(sample) %>% arrange(X2) %>% summarise(min_year=year[1]) %>% { table(.$min_year) %>% prop.table}

# The following is for SVD'ing pnro x year directly, without covariates.

d3 <- d2 %>% 
  pivot_longer(cols=starts_with("V"), names_to="sample", values_to="lprice") %>% 
  pivot_wider(names_from="year", values_from="lprice") %>% 
  group_by(pnro) %>% select(-sample) %>%
  summarise_each(mean)

# sol <- princomp(d3 %>% select(-pnro))
sol <- svd(d3 %>% select(-pnro) %>% scale(center=T, scale=F)) 
d_loadings <- data.frame(X1=sol$v[,1], X2=sol$v[,2]) %>% mutate(year=as.numeric(colnames(d3)[-1]))
rot1 <- coef(lm(year ~ X1 + X2, data=d_loadings))[-1] %>% {./sqrt(sum(. * .))}
rot2 <- c(-rot1[2], rot1[1]) 
rot12 <- rbind(rot1, rot2)

scores_rotated <- sol$u[,c(1, 2)] %*% rot12 %>% scale
d4 <- d3 %>% mutate(X1 = scores_rotated[,1], X2 = scores_rotated[,2])

pnro_sf <- sf::st_as_sf(pnro.sp)  %>% 
  sf::st_transform("+proj=laea +y_0=0 +lon_0=25 +lat_0=62 +ellps=WGS84 +no_defs") 

clipclip <- function (x) pmax(-3, pmin(x, 3))

pnro_sf %>% left_join(d4) %>% mutate(X1 = clipclip(X1), X2=clipclip(X2)) %>%
  filter(substr(pnro, 1, 2) %in% c("00", "01", "02")) %>%
  #filter(substr(pnro, 1, 1) %in% c("0", "2", "3")) %>%
  ggplot(aes(fill=X2)) + geom_sf(size=.1) + scale_fill_viridis_c(na.value="#00000000")

