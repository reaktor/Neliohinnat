library(dplyr)
library(ggplot2)

# Lots of things needed here. 

source("source/common.R")

load("data/pnro_data_20220426.RData") # pnro.ashi.dat, pnro.population, pnro.sp

d_covs0 <- get_covariates(pnro.population, impute = T, include_intercept = T, level3_dummies = F)
d_covs <- crossing(d_covs0, distinct(pnro.ashi.dat, year))
d <- readRDS("data/d_20220426.rds") 

fit <- readRDS("data/samples_5000+5000_20220426.rds")

# Slot statistics
n_slots <- d %>% select(pnro, year, n) %>% mutate(n=ifelse(is.na(n), 0, n)) 

n_slots %>% arrange(-n) %>% 
  mutate(pcn=cumsum(n)/sum(n), p=(1:n())/n()) %>% 
  ggplot(aes(x=p, y=pcn)) + geom_line() + xlim(0, .3) + scale_y_continuous(breaks=.9)

n_slots %>% summarise(mean(n<100))


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
pnros <- d_covs0$pnro

cov_year_samples <- rstan::extract(fit, pars="beta_year")[[1]]

beta_samples_df <- cov_year_samples %>% reshape2::melt() %>% 
  setNames(c("sample", "year", "var", "beta")) %>% 
  mutate(var=cov_names[var], year=years[year]) %>% 
  filter(var != "c_intercept") %>%
  as_tibble()

# Covariate coef samples as function of year (alpha lines)
p1 <- beta_samples_df %>% filter(sample %% 20 == 0) %>% 
  filter(var %in% c("c_living_space", "c_mean_income", "c_high_school_share",  
                    "c_employed_share", "c_male_share", "c_log_density")) %>%
  ggplot(aes(x=year, y=beta, group=sample)) + 
  geom_line(alpha=.3, size=.6) + geom_hline(yintercept=0, color=I("red"), alpha=I(.5)) +
  scale_x_continuous(breaks=c(2013, 2016, 2019)) + 
  ylab("coefficient for standardized covariate") +
  theme_minimal(14) 
  
p1 + facet_wrap(~ var, scales=NULL); ggsave("figs/cov-timeseries.png")
p1 + facet_wrap(~ var, scales="free_y"); ggsave("figs/cov-timeseries-varscale.png")

# Variances of slot re
rstan::extract(fit, pars="ysigma")[[1]] %>% 
  data.frame %>% setNames(years) %>% pivot_longer(matches("^20")) %>% 
  ggplot(aes(x=value, y=name)) + geom_point(alpha=.04, size=2) + 
  theme_minimal(14) + ylab("year") + xlab("ysigma")
ggsave("figs/stds-of-slot-re.png")

# A general look at covariates
d_covs %>% filter(year==2021) %>% 
  select(matches("^c_"), pnro, population) %>% 
  pivot_longer(matches("^c_"), names_to="cov", values_to="y") %>% 
  ggplot(aes(x=log(population), y=y)) + facet_wrap(~ cov) + geom_point(alpha=.5, size=.1)


# Covariate coef change 2020...2021
# NOTE that this depends on year literals. 
beta_samples_df %>% filter(year %in% c(2020, 2021)) %>% 
  tidyr::pivot_wider(names_from="year", values_from="beta", names_prefix="y") %>%
  mutate(change_20_21=y2021-y2020) %>%
  ggplot(aes(y=var, x=change_20_21)) + geom_point(alpha=.05) + 
  xlim(-.15, .1) +
  xlab("change of coefficients from 2020 to 2021") + ylab("") +
  geom_vline(xintercept = 0, color="red") + 
  theme_minimal(14)
ggsave("figs/cov-change-2021.png")


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
d4 <- d2 %>% mutate(X1=pnro_scores[,1], X2=pnro_scores[,2]) %>% filter(year==2021) 

pnro_sf %>% left_join(d4) %>% #mutate(X1 = clipclip(X1), X2=clipclip(X2)) %>%
  filter(substr(pnro, 1, 1) %in% c("0", "1", "2", "3")) %>%
  # filter(substr(pnro, 1, 1) %in% c("0", "2", "3")) %>%
  ggplot(aes(fill=clipclip(X2-.4*X1, xmax=1))) + geom_sf(size=.1) + scale_fill_viridis_c(na.value="#00000000")


# We need to do the same for samples, to get confidence intervals.

cov_sols <- apply(cov_year_samples, 1, m2sol)
us_df <- lapply(seq_along(cov_sols), 
                      function (i) sol2df(cov_sols[[i]], 
                                          target_2=post_mean_u$X2) %>% 
                                          mutate(sample=i)) %>%  bind_rows

#us_df %>% ggplot(aes(x=X1, y=X2)) + geom_point(alpha=.2, size=.2) + facet_wrap(~ year)

us_df %>% ggplot(aes(x=name, y=X2, group=sample)) + geom_path(alpha=.05) + geom_point(alpha=.05)
us_df %>% ggplot(aes(x=X1, y=X2, group=sample, color=name)) + geom_path(alpha=.1) 

# This is markedly different from post_mean_u
post_svd_mean_u <- us_df %>% group_by(name) %>% summarise(X1=mean(X1), X2=mean(X2))

us_df %>%
  ggplot(aes(x=X1, y=-X2, group=as.factor(name))) + 
  stat_ellipse(level=.8, color="white", fill="#00508015", geom="polygon") + 
  geom_path(aes(group=NULL), data=post_svd_mean_u, color="#c06040", size=1) +
  geom_label(aes(label=name), data=post_svd_mean_u) +
  xlab("PC1: \"Urbanisation/centralisation index\"") + ylab("PC2: \"Suburb index\"") +
  theme_classic(14) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) #+ coord_equal()
ggsave("figs/princomps-2021.png")

# 2021 anomalous?
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
ggsave("figs/PC1-coeffs.png")

vs_df %>% ggplot(aes(x=X2, y=name)) + geom_point(alpha=.05) + 
  geom_vline(xintercept = 0, color="red", alpha=.5) + 
  xlab("Covariate coeffs on PC2") + ylab("") + theme_minimal(14)
ggsave("figs/PC2-coeffs.png")

### The following is for SVD'ing pnro x year directly, without covariates.

d3 <- d2 %>% 
  pivot_longer(cols=starts_with("V"), names_to="sample", values_to="lprice") %>% 
  pivot_wider(names_from="year", values_from="lprice") %>% 
  group_by(pnro) %>% select(-sample) %>%
  summarise_each(mean)

# sol <- princomp(d3 %>% select(-pnro))
sol <- svd(scale(d3 %>% select(-pnro) %>% apply(., 1, \(x) x-mean(x)) %>% t, center=T, scale=F))

#sol <- svd(d3 %>% select(-pnro) %>% scale(center=T, scale=F)) 
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

southp <- function(geometry) sf::st_coordinates(sf::st_centroid(geometry))[,2] < 4e5

plot_scalar <- function (d, var, pred) {
  var <- enquo(var); pred <- enquo(pred)
  pnro_sf %>% left_join(d) %>%
  filter(!!pred) %>%
  ggplot(aes(fill = !!var)) + geom_sf(size=.1) +
  scale_fill_viridis_c() }

pnro_sf %>% left_join(d4) %>% ggplot(aes(x=X1, y=log(population))) + geom_point()
pnro_sf %>% left_join(d4) %>% ggplot(aes(x=X2, y=log(population))) + geom_point()

good_pnro1 <- quote(pnro != "70210")
good_pnro2 <- quote(pnro != "70210" & X2<1.0)

plot_scalar(d4, -X1, grepl("^", pnro) & !!good_pnro1); ggsave("figs/map-Finland-pnro-price-princomp1.png")
plot_scalar(d4, X2, grepl("^", pnro) & !!good_pnro2); ggsave("figs/map-Finland-pnro-price-princomp2.png")

plot_scalar(d4, -X1, southp(geometry) & !!good_pnro1); ggsave("figs/map-south-pnro-price-princomp1.png")
plot_scalar(d4, X2, southp(geometry) & !!good_pnro2); ggsave("figs/map-south-pnro-price-princomp2.png")

plot_scalar(d4, -X1, grepl("^00", pnro) & !!good_pnro2); ggsave("figs/map-Hki-pnro-price-princomp1.png")
plot_scalar(d4, X2, grepl("^00", pnro) & !!good_pnro2); ggsave("figs/map-Hki-pnro-price-princomp2.png")

plot_scalar(d4, -X1, grepl("^0[012]", pnro) & !!good_pnro2); ggsave("figs/map-capital-pnro-price-princomp1.png")
plot_scalar(d4, X2, grepl("^0[012]", pnro) & !!good_pnro2); ggsave("figs/map-capital-pnro-price-princomp2.png")

d_change <- d2 %>% 
  select(pnro, year, matches("^V[0-9]+")) %>% 
  pivot_longer(matches("^V[0-9]"), names_to="sample", values_to="lprice") %>% 
  filter(year %in% c(2020, 2021)) %>% 
  pivot_wider(names_from=year, values_from=lprice, names_prefix="y") %>% 
  mutate(change_21_20=y2021-y2020) %>% select(pnro, change_21_20) %>%
  group_by(pnro) %>% 
  summarise_all(function (x) clipclip(median(x), -.1, .10)) %>%
  left_join(d %>% select(pnro, population) %>% distinct)

pnro_sf %>% left_join(d_change) %>% 
  filter(southp(geometry)) %>%
  mutate(change_21_20 = ifelse(population<46, NA, change_21_20)) %>%
  ggplot(aes(fill=change_21_20)) + 
  geom_sf(size=.0)+ scale_fill_viridis_c(na.value="#000000") + theme_void(14)
ggsave("figs/map-south-21-20-change.png")

# SVD to the predictions over zip_codes x years, but for each sample separately. 

d3l <- cbind(pnro = d$pnro, year = d$year, 
            as.data.frame(t(rstan::extract(fit, 'pred_mean')[[1]]))) %>% 
  pivot_longer(cols=starts_with("V"), names_to="sample", values_to="lprice") %>% 
  pivot_wider(names_from="year", values_from="lprice") %>% 
  group_by(sample) %>% group_split

pnro_svd <- function (d3) { 
  # Note the implicit intercept term 11^T, that is, centered over years and zip codes. 
  select(d3, -pnro, -sample) %>% apply(., 1, \(x) x-mean(x)) %>% t %>% scale(center=T, scale=F) %>% svd() }

sols <- lapply(d3l, pnro_svd) # takes a while (a couple of minutes or something)

# This tries to fix polarity ambiquity in sample-wise component matrices
est_flippage <- function(loadings_matrix) { # matrix with rows: samples; cols: zips or years
  for (i in 1:10) {
    loadings_mean <- apply(loadings_matrix, 2, mean)
    loadings_matrix <- outer(sign(loadings_matrix %*% loadings_mean)[,1], 
                             rep(1, ncol(loadings_matrix))) * loadings_matrix }
  loadings_matrix }

loadings_col <- function (i_col, col_name=NULL, target="V") {
  unit_names <- if (target=="U") pnros else years
  unit_var <- if (target=="U") "pnro" else "year"
  if (is.null(col_name)) col_name <- paste("X", i_col, sep="")
  sapply(sols, \(s) {if (target=="U") s$u else s$v}[,i_col]) %>% t %>% 
    est_flippage %>% data.frame() %>% as_tibble() %>% 
    setNames(unit_names) %>% mutate(sample=1:n()) %>% 
    # regex below should match both years (in case of V) and zip codes (in case of U)
    pivot_longer(matches("^[0-9]+$"), names_to=unit_var, values_to=col_name) }

d_loadings <- loadings_col(1) %>% left_join(loadings_col(2)) #%>% left_join(loadings_col(3))
d_loadings_mean <- d_loadings %>% group_by(year) %>% summarise(across(matches("^X"), mean))

d_loadings %>%
  ggplot(aes(x=-X1, y=X2, group=as.factor(year))) + 
    stat_ellipse(level=.8, color="white", fill="#00508015", geom="polygon") + 
    geom_path(aes(group=NULL), data=d_loadings_mean, color="#c06040", size=1) +
    geom_label(aes(label=year), data=d_loadings_mean) +
    xlab("PC1: \"Urbanisation/centralisation index\"") + ylab("PC2: \"Suburb index\"") +
    theme_classic(14) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank()) #+ coord_equal()
ggsave("figs/princomps-2021.png")

d_scores <- loadings_col(1, target="U") %>% 
  left_join(loadings_col(2, target="U")) %>% 
  left_join(loadings_col(3, target="U"))
d_scores_mean <- d_scores %>% group_by(pnro) %>% summarise(across(matches("^X"), mean))

d5 <- d_scores_mean

pnro_sf %>% left_join(d5) %>% ggplot(aes(x=X1, y=log(population))) + geom_point()
pnro_sf %>% left_join(d5) %>% ggplot(aes(x=X2, y=log(population))) + geom_point()
pnro_sf %>% left_join(d5) %>% ggplot(aes(x=X3, y=log(population))) + geom_point()

good_pnro1 <- quote(pnro != "70210")
good_pnro2 <- quote(pnro != "70210" & X2<0.012)

plot_scalar(d5, -X1, grepl("^", pnro) & !!good_pnro1); ggsave("figs/map-Finland-pnro-price-princomp1.png")
plot_scalar(d5, X2, grepl("^", pnro) & !!good_pnro2); ggsave("figs/map-Finland-pnro-price-princomp2.png")

plot_scalar(d5, -X1, southp(geometry) & !!good_pnro1); ggsave("figs/map-south-pnro-price-princomp1.png")
plot_scalar(d5, X2, southp(geometry) & !!good_pnro2); ggsave("figs/map-south-pnro-price-princomp2.png")

plot_scalar(d5, -X1, grepl("^00", pnro) & !!good_pnro2); ggsave("figs/map-Hki-pnro-price-princomp1.png")
plot_scalar(d5, X2, grepl("^00", pnro) & !!good_pnro2); ggsave("figs/map-Hki-pnro-price-princomp2.png")

plot_scalar(d5, -X1, grepl("^0[012]", pnro) & !!good_pnro2); ggsave("figs/map-capital-pnro-price-princomp1.png")
plot_scalar(d5, X2, grepl("^0[012]", pnro) & !!good_pnro2); ggsave("figs/map-capital-pnro-price-princomp2.png")


