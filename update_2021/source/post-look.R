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
cov_names <- colnames(d_covs_pure)
ICEPT_IDX <- match("c_intercept", cov_names)
years <- sort(unique(d2$year))

cov_year_samples <- rstan::extract(fit, pars="beta_year")[[1]]

beta_samples_df <- cov_year_samples %>% reshape2::melt() %>% 
  setNames(c("sample", "year", "var", "beta")) %>% 
  mutate(var=cov_names[var], year=years[year]) %>% 
  filter(var != "c_intercept") %>%
  as_tibble()

# Covariate coef samples as function of year (alpha lines)
p1 <- beta_samples_df %>% filter(sample %% 10 == 0) %>% 
  filter(var %in% c("c_living_space", "c_mean_income", "c_high_school_share",  
                    "c_area", "c_population", "c_log_density")) %>%
  ggplot(aes(x=year, y=beta, group=sample)) + 
  geom_line(alpha=.2) + geom_hline(yintercept=0, color=I("red"), alpha=I(.5)) +
  scale_x_continuous(breaks=c(2013, 2016, 2019)) + 
  ylab("coefficient for a standardized covariate") +
  theme_minimal(14) 
  
p1 + facet_wrap(~ var, scales=NULL) 
p1 + facet_wrap(~ var, scales="free_y")

# Covariate coef change 2019...2020
# NOTE that this depends on year literals. 
beta_samples_df %>% filter(year %in% c(2019, 2020)) %>% 
  tidyr::pivot_wider(names_from="year", values_from="beta", names_prefix="y") %>%
  mutate(change_19_20=y2020-y2019) %>%
  ggplot(aes(y=var, x=change_19_20)) + geom_point(alpha=.05) + 
  xlim(-.15, .1) +
  geom_vline(xintercept = 0, color="red") + theme_minimal(14)
ggsave("../../figs/")


### SVD for covariates x years

m2sol <- function (m) svd(scale(m[,-ICEPT_IDX], scale=F))

sol2df <- function (cov_sol, take_V=FALSE, names=years, target_1 = NULL,  target_2 = NULL) { 
  res <- { if (take_V) cov_sol$v else cov_sol$u }[,c(1, 2)] %>%  
    data.frame() %>% setNames(c("X1", "X2")) %>% 
    mutate(name=names) 
  if (!is.null(target_1) &  sum(target_1*res$X1)<0) res$X1 <- -res$X1
  if (!is.null(target_2) &  sum(target_2*res$X2)<0) res$X2 <- -res$X2
  res }

clipclip <- function (x, xmin=-2, xmax=2) pmax(xmin, pmin(x, xmax))

# This is needed for the sample-wise solution, because of occasional flippage of X2.
post_mean_sol <- cov_year_samples %>% apply(c(2, 3), mean) %>% m2sol
post_mean_u <-  post_mean_sol %>% sol2df

# rot1 <- coef(lm(year ~ X1 + X2, data=d_cov_loadings))[-1] %>% {./sqrt(sum(. * .))}
# rot2 <- c(-rot1[2], rot1[1]) 
# rot12 <- rbind(rot1, rot2)
# 
# d_cov_loadings2 <<- cov_sol$u[,c(1, 2)] %*% solve(rot12) %>% scale %>% 
#   data.frame() %>% setNames(c("X1", "X2")) %>% mutate(year=2010:2020)
#
# plot((cov_sol$v[,c(1, 2)] %*% rot12)[,1], type="l")
# plot((cov_sol$v[,c(1, 2)] %*% rot12)[,2], type="l")

post_mean_u %>% ggplot(aes(x=X1, y=-X2, label=name)) + geom_path() + geom_label() + 
  xlab("PC1: \"Urbanisation/centralisation index\"") + ylab("PC2: \"Suburb index\"") + theme_classic()
  
# This is more informative with the svd done over pnro x year
pnro_scores <- as.matrix(d_covs_pure)[,-ICEPT_IDX] %*% (post_mean_sol$v[,c(1, 2)]) # %*% rot12) 
d4 <- d2 %>% mutate(X1=pnro_scores[,1], X2=pnro_scores[,2]) %>% filter(year==2020) 

pnro_sf %>% left_join(d4) %>% mutate(X1 = clipclip(X1), X2=clipclip(X2)) %>%
  filter(substr(pnro, 1, 2) %in% c("00", "01", "02")) %>%
  # filter(substr(pnro, 1, 1) %in% c("0", "2", "3")) %>%
  ggplot(aes(fill=X2)) + geom_sf(size=.1) + scale_fill_viridis_c(na.value="#00000000")


# We need to do the same for samples, to get confidence intervals.

cov_sols <- apply(cov_year_samples, 1, m2sol)
us_df <- lapply(seq_along(cov_sols), 
                      function (i) sol2df(cov_sols[[i]], 
                                          target_2=post_mean_u$X2) %>% 
                                          mutate(sample=i)) %>%  bind_rows

#us_df %>% ggplot(aes(x=X1, y=X2)) + geom_point(alpha=.2, size=.2) + facet_wrap(~ year)

us_df %>% ggplot(aes(x=name, y=X2, group=sample)) + geom_line(alpha=.05) + geom_point(alpha=.05)

# This is markedly different from post_mean_u
post_svd_mean_u <- us_df %>% group_by(name) %>% summarise(X1=mean(X1), X2=mean(X2))

us_df %>%
  ggplot(aes(x=X1, y=-X2, group=as.factor(name))) + 
  stat_ellipse(level=.8, color="white", fill="#00508015", geom="polygon") + 
  geom_line(aes(group=NULL), data=post_svd_mean_u, color="#c06040", size=1) +
  geom_label(aes(label=name), data=post_svd_mean_u) +
  xlab("PC1: \"Urbanisation/centralisation index\"") + ylab("PC2: \"Suburb index\"") +
  theme_classic(14) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
ggsave("../../figs/princomps-2020.png")

# 2020 anomalous?
us_df %>% mutate(year=name) %>% group_by(sample) %>% arrange(X2) %>% summarise(min_year=year[1]) %>% 
  { table(.$min_year) %>% prop.table}


vs_df <- lapply(seq_along(cov_sols), 
                      function (i) sol2df(cov_sols[[i]], 
                                          take_V=T, 
                                          target_2={post_mean_sol %>% sol2df(take_V=T, names=NA)}$X2,
                                          names=cov_names[-ICEPT_IDX]) %>% 
                        mutate(sample=i)) %>%  bind_rows


vs_df %>% ggplot(aes(x=X1, y=name)) + geom_point(alpha=.05) + 
  geom_vline(xintercept = 0, color="red", alpha=.5) + 
  xlab("Covariate coeffs on PC1 (centralization)") + ylab("") + theme_minimal(14)
ggsave("../../figs/")

vs_df %>% ggplot(aes(x=X2, y=name)) + geom_point(alpha=.05) + 
  geom_vline(xintercept = 0, color="red", alpha=.5) + 
  xlab("Covariate coeffs on PC2") + ylab("") + theme_minimal(14)
ggsave("../../figs/")

### The following is for SVD'ing pnro x year directly, without covariates.

d3 <- d2 %>% 
  pivot_longer(cols=starts_with("V"), names_to="sample", values_to="lprice") %>% 
  pivot_wider(names_from="year", values_from="lprice") %>% 
  group_by(pnro) %>% select(-sample) %>%
  summarise_each(mean)

# sol <- princomp(d3 %>% select(-pnro))
sol <- svd(d3 %>% select(-pnro) %>% scale(center=T, scale=F)) 
d_loadings <- data.frame(X1=sol$v[,1], X2=sol$v[,2]) %>% mutate(year=as.numeric(colnames(d3)[-1]))
# Again, the rotation is basically identity and can be skipped.
if (F) {
  rot1 <- coef(lm(year ~ X1 + X2, data=d_loadings))[-1] %>% {./sqrt(sum(. * .))}
  rot2 <- c(-rot1[2], rot1[1]) 
  rot12 <- rbind(rot1, rot2) 
  } else {
  rot12 <- diag(2)
}
scores_rotated <- sol$u[,c(1, 2)] %*% rot12 %>% scale
d4 <- d3 %>% mutate(X1 = scores_rotated[,1], X2 = scores_rotated[,2])

X2_pnro_plot <- function (d, zip_pattern, xmin=-2, xmax=2) 
  pnro_sf %>% left_join(d) %>% mutate(X1 = clipclip(X1, xmin, xmax), X2=clipclip(X2, xmin, xmax)) %>%
    filter(grepl(zip_pattern, pnro)) %>%
    ggplot(aes(fill=-X2)) + geom_sf(size=.1) + scale_fill_viridis_c(na.value="#00000000")

X2_pnro_plot(d4, "^", -2, 2); ggsave("../../figs/map-Finland-pnro-price-princomp2.png")
X2_pnro_plot(d4, "^00", -2, 2); ggsave("../../figs/map-Hki-pnro-price-princomp2.png")
X2_pnro_plot(d4, "^0[012]", -2, 2); ggsave("../../figs/map-capital-pnro-price-princomp2.png")
