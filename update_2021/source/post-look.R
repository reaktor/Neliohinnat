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

pnro_sf <- sf::st_as_sf(pnro.sp)  %>% 
  sf::st_transform("+proj=laea +y_0=0 +lon_0=25 +lat_0=62 +ellps=WGS84 +no_defs") 

d_covs_pure <- d_covs %>% select(matches("^c_"))

# Covariate coef samples as function of year (alpha lines)
rstan::extract(fit, pars="beta_year")[[1]] %>% reshape2::melt() %>% setNames(c("i", "year", "var", "beta")) %>% 
  mutate(var=colnames(d_covs_pure)[var]) %>% 
  filter(var != "c_intercept") %>%
  as_tibble() %>% filter(i%%10==0) %>% 
  ggplot(aes(x=year, y=beta, group=i)) + 
  geom_line(alpha=.2) + geom_hline(yintercept=0, color=I("red"), alpha=I(.5)) + facet_wrap(~ var, scales=NULL) 

# Covariate coef change 2019...2020
rstan::extract(fit, pars="beta_year")[[1]] %>% reshape2::melt() %>% setNames(c("i", "year", "var", "beta")) %>% 
  mutate(var=colnames(d_covs_pure)[var]) %>% 
  as_tibble() %>% filter(year %in% c(10, 11)) %>% #filter(i%%10==0) %>% 
  tidyr::pivot_wider(names_from="year", values_from="beta", names_prefix="y") %>%
  mutate(change_19_20=y11-y10) %>%
  ggplot(aes(y=var, x=change_19_20)) + geom_point(alpha=.02) + 
  xlim(-.2, .1) +
  geom_vline(xintercept = 0, color="red")

# Covariate effect vs. change 2019...2020, per covariate, samples
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


### SVD for covariates x years
cov_year_samples <- rstan::extract(fit, pars="beta_year")[[1]]

m2sol <- function (m) svd(scale(m[,-23], scale=F))

sol2df <- function (cov_sol, target_2 = NULL) { 
  res <- cov_sol$u[,c(1, 2)] %>% #%*% solve(rot12) %>% scale %>% 
    data.frame() %>% setNames(c("X1", "X2")) %>% 
    mutate(year=2010:2020) 
  if (!is.null(target_2) &  sum(target_2*res$X2)<0) res$X2 <- -res$X2
  res }

# This is needed for the sample-wise solution, because of occasional flippage of X2.
post_mean_loadings <- cov_year_samples %>% apply(c(2, 3), mean) %>% m2sol %>% sol2df

# rot1 <- coef(lm(year ~ X1 + X2, data=d_cov_loadings))[-1] %>% {./sqrt(sum(. * .))}
# rot2 <- c(-rot1[2], rot1[1]) 
# rot12 <- rbind(rot1, rot2)
# 
# d_cov_loadings2 <<- cov_sol$u[,c(1, 2)] %*% solve(rot12) %>% scale %>% 
#   data.frame() %>% setNames(c("X1", "X2")) %>% mutate(year=2010:2020)
#
# plot((cov_sol$v[,c(1, 2)] %*% rot12)[,1], type="l")
# plot((cov_sol$v[,c(1, 2)] %*% rot12)[,2], type="l")

post_mean_loadings %>% ggplot(aes(x=X1, y=-X2, label=year)) + geom_path() + geom_label() + 
  xlab("PC1: \"Urbanisation/centralisation index\"") + ylab("PC2: \"Suburb index\"") + theme_classic()
  
pnro_scores <- as.matrix(d_covs_pure)[,-23] %*% (cov_sol$v[,c(1, 2)] %*% rot12) 
d4 <- d2 %>% mutate(X1=pnro_scores[,1], X2=pnro_scores[,2]) %>%
  filter(year==2020) 

clipclip <- function (x) pmax(-2, pmin(x, 2))

# This is more informative with the svd done over pnro x year
pnro_sf %>% left_join(d4) %>% mutate(X1 = clipclip(X1), X2=clipclip(X2)) %>%
  filter(substr(pnro, 1, 2) %in% c("00", "01", "02")) %>%
  # filter(substr(pnro, 1, 1) %in% c("0", "2", "3")) %>%
  ggplot(aes(fill=X2)) + geom_sf(size=.1) + scale_fill_viridis_c(na.value="#00000000")

# We need to do the same for samples, to get confidence intervals.

cov_sols <- apply(rstan::extract(fit, pars="beta_year")[[1]], 1, m2sol)
cov_sols_df <- lapply(seq_along(cov_sols), 
                      function (i) sol2df(cov_sols[[i]], 
                                          target_2=post_mean_loadings$X2) %>% 
                                          mutate(sample=i)) %>%  bind_rows

#cov_sols_df %>% ggplot(aes(x=X1, y=X2)) + geom_point(alpha=.2, size=.2) + facet_wrap(~ year)

cov_sols_df %>% ggplot(aes(x=year, y=X2, group=sample)) + geom_line(alpha=.05) + geom_point(alpha=.05)

# This is markedly different from d_cov_loadings
cov_sols_mean <- cov_sols_df %>% group_by(year) %>% summarise(X1=mean(X1), X2=mean(X2))

cov_sols_df %>%
  ggplot(aes(x=X1, y=X2, group=as.factor(year))) + 
  stat_ellipse(level=.8, color="white", fill="#00508015", geom="polygon") + 
  geom_line(aes(group=NULL), data=cov_sols_mean, color="#c06040", size=1) +
  geom_label(aes(label=year), data=cov_sols_mean) +
  xlab("PC1: \"Urbanisation/centralisation index\"") + ylab("PC2: \"Suburb index\"") +
  theme_classic(14) +
  theme(axis.title.x=element_text())
ggsave("../figs/princomps-2020.png")

# 2020 anomalous?
cov_sols_df %>% group_by(sample) %>% arrange(X2) %>% summarise(min_year=year[1]) %>% 
  { table(.$min_year) %>% prop.table}

### The following is for SVD'ing pnro x year directly, without covariates.

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


clipclip <- function (x) pmax(-3, pmin(x, 3))

pnro_sf %>% left_join(d4) %>% mutate(X1 = clipclip(X1), X2=clipclip(X2)) %>%
  filter(substr(pnro, 1, 2) %in% c("00", "01", "02")) %>%
  #filter(substr(pnro, 1, 1) %in% c("0", "2", "3")) %>%
  ggplot(aes(fill=X2)) + geom_sf(size=.1) + scale_fill_viridis_c(na.value="#00000000")

