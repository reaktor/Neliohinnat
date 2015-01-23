library(rstan)
library(dplyr)

source("source/common3.R")


s <- readRDS("s3.rds")
d <- readRDS("data/d.rds")

if (F) {
  s
  traceplot(s, "LOmega", inc_warmup=F, ask=T)
  traceplot(s, "LOmega3", inc_warmup=F, ask=T)
  traceplot(s, "tau", inc_warmup=F)
  traceplot(s, "tau3", inc_warmup=F)
  traceplot(s, "mean_beta", inc_warmup=F)
  traceplot(s, "beta", inc_warmup=F)
  # etc.
}

# Low-level correlation matrix over price level, trend, etc.
# Is of general interest
Omega <- matrix(apply(apply(extract(s, "LOmega")[[1]], 1, function (m) m %*% t(m)), 1, mean), c(6, 6))
saveRDS(Omega, "data/Omega.rds")
Omega1 <- matrix(apply(apply(extract(s, "LOmega1")[[1]], 1, function (m) m %*% t(m)), 1, mean), c(6, 6))
saveRDS(Omega1, "data/Omega1.rds")
beta.prm.mean <- function (v) apply(extract(s, v)[[1]], c(2, 3), mean)
# For debugging 
if (F) {
  beta <- beta.prm.mean("beta")
  lhinta <- beta[,1]+6
  trendi <- beta[,2]/10
  quad <- beta[,3]
  hist(beta[,1], n=100)
  hist(beta[,2], n=100)
  hist(beta[,3], n=100)
  hist(beta[,4], n=100)
  hist(beta[,5], n=100)
  hist(beta[,6], n=100)
}

par.tbl <- function(d, v.name, b.name, name.postfix) 
  data.frame(levels(d[[v.name]]), beta.prm.mean(b.name)) %>% 
  setNames(c(v.name, 
             paste(c("lprice", "trend", "quad", "k.lprice", "k.trend", "k.quad"), name.postfix, sep=""))) %>%
  tbl_df()

pnro <- pnro.area$pnro
# For NA pnro's in the model, look for upper level in the hierarchy and take beta1 etc.
res <- data.frame(pnro.area, level1 = l1(pnro), level2 = l2(pnro), level3 = l3(pnro)) %>% 
  mutate(log.density=ifelse(is.finite(log.density), 
                            log.density, 
                            mean(log.density[is.finite(log.density)]))) %>% # OBS! a bit dangerous
  left_join(par.tbl(d, "pnro", "beta", ""), by="pnro") %>% 
  left_join(par.tbl(d, "level1", "beta1", "1"), by="level1") %>% 
  left_join(par.tbl(d, "level2", "beta2", "2"), by="level2") %>% 
  left_join(par.tbl(d, "level3", "beta3", "3"), by="level3") %>% 
  mutate(pnro=pnro, 
            log.density = log.density,
            lprice=first.nna(lprice, lprice1, lprice2, lprice3) + 
              first.nna(k.lprice, k.lprice1, k.lprice2, k.lprice3) * log.density, 
            trend=first.nna(trend, trend1, trend2, trend3) +
              first.nna(k.trend, k.trend1, k.trend2, k.trend3) * log.density, 
            quad=first.nna(quad, quad1, quad2, quad3) +
              first.nna(k.quad, k.quad1, k.quad2, k.quad3) * log.density
  ) %>%
  # Original unit is decade, for vars below it is year. 
  # d/d.yr lprice = trend + 2*quad*yr
  # d/(10*d.yr) lprice = trend/10 + 2*quad*yr/10
  # d^2/(10*d.yr)^2 lprice = 2*quad/100
  # trendi is as percentage / 100.
  # trendimuutos is as percentage units / 100 / year.
  mutate(hinta = exp(lprice+6), trendi = trend/10, trendimuutos = 2*quad/100, 
         trendi2015 = (trend + 2*quad*year2yr(2015))/10)

res.shrinked <- res %>% select(pnro, hinta, lprice, trendi, trendimuutos)
write.table(res.shrinked,  "data/pnro-hinnat.txt", row.names=F, quote=F)
saveRDS(res.shrinked, "data/pnro-hinnat.rds")

# TODO:
# - res.big, with all samples

library(ggplot2)

ggplot(res, aes(x=-log.density, y=lprice, color=l3(pnro))) + geom_point(alpha=.5, size=3) + 
  xlab("log.density") + ylab("log price - 6") + theme_minimal(15)
ggsave("figs/density-lprice.png")
ggplot(res, aes(x=-log.density, y=trendi, color=l3(pnro))) + geom_point(alpha=.5, size=3) +
  xlab("log.density") + ylab("trend per year") + theme_minimal(15)
ggsave("figs/density-trend.png")
ggplot(res, aes(x=-log.density, y=trendimuutos, color=l3(pnro))) + geom_point(alpha=.5, size=3) +
  xlab("log.density") + ylab("trend change per year") + theme_minimal(15)
ggsave("figs/density-trendchange.png")

ggplot(res, aes(x=lprice, fill=l3(pnro))) + geom_histogram(binwidth=.04) +
  xlab("log price - 6") + ylab("") + theme_minimal(15)
ggsave("figs/lprice-histogram.png")
ggplot(res, aes(x=trendi, fill=l3(pnro))) + geom_histogram(binwidth=.002) +
  xlab("trend") + ylab("") + theme_minimal(15)
ggsave("figs/trendi-histogram.png")
ggplot(res, aes(x=trendimuutos, fill=l3(pnro))) + geom_histogram(binwidth=.0004) +
  xlab("trend change") + ylab("") + theme_minimal(15)
ggsave("figs/trendimuutos-histogram.png")
ggplot(res, aes(x=trendi2015, fill=l3(pnro))) + geom_histogram(binwidth=.002) +
  xlab("trend 2015") + ylab("") + theme_minimal(15)
ggsave("figs/trendi-2015.png")


## Plot with alternative coordinates
load("data/pnro_spatial_epsg2393.RData") # pnro.sp.alt appears here
pnro.hinnat.sp <- merge(pnro.sp.alt, res)
pdf("figs/pnro_prices.pdf")
spplot(pnro.hinnat.sp, zcol="lprice", lwd=0.00, col="transparent", main="Log price")
spplot(pnro.hinnat.sp, zcol="trendi", lwd=0.00, col="transparent", main="Trend")
spplot(pnro.hinnat.sp, zcol="trendimuutos", lwd=0.00, col="transparent", main="Trend change per year")
spplot(pnro.hinnat.sp, zcol="trendi2015", lwd=0.00, col="transparent", main="Trend 2015")
dev.off()

pk.sp <- pnro.hinnat.sp[substr(pnro.hinnat.sp$pnro, 1, 2) %in% c("00", "01", "02"),]
pdf("figs/pk-pnro_prices.pdf")
spplot(pk.sp, zcol="lprice", lwd=0.00, col="transparent", main="Log price")
spplot(pk.sp, zcol="trendi", lwd=0.00, col="transparent", main="Trend")
spplot(pk.sp, zcol="trendimuutos", lwd=0.00, col="transparent", main="Trend change per year")
spplot(pk.sp, zcol="trendi2015", lwd=0.00, col="transparent", main="Trend 2015")
dev.off()


pnro.hinnat.sp.raw<- merge(pnro.sp.alt, 
                           d %>% group_by(pnro) %>% 
                             do(data.frame(lprice=mean(.$lprice), trend=coef(lm(lprice ~ year + 1, 
                                                                                weights = .$n,
                                                                                data=.))[["year"]])) %>%
                             data.frame)
pk.sp.raw <- pnro.hinnat.sp.raw[substr(pnro.hinnat.sp.raw$pnro, 1, 2) %in% c("00", "01", "02"),]

pdf("figs/without-hierarhy.pdf")
spplot(pnro.hinnat.sp.raw, zcol="lprice", lwd=0.00, col="transparent", main="log.price (raw)")
spplot(pnro.hinnat.sp.raw, zcol="trend", lwd=0.00, col="transparent", main="trend (raw)")
spplot(pk.sp.raw, zcol="lprice", lwd=0.00, col="transparent", main="log.price (raw)")
spplot(pk.sp.raw, zcol="trend", lwd=0.00, col="transparent", main="trend (raw)")
dev.off()






