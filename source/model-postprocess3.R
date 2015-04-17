library(rstan)
library(dplyr)
library(RJSONIO)

source("source/common3.R")


s <- sflist2stanfit(readRDS("s6list.rds")[1:8]); traceplot(s, "tau", inc_warmup=F)
d <- readRDS("data/d.rds")

if (F) {
  s
  traceplot(s, "LOmega", inc_warmup=F, ask=T)
  traceplot(s, "LOmega2", inc_warmup=F, ask=T)
  traceplot(s, "tau", inc_warmup=F)
  traceplot(s, "tau1", inc_warmup=F)
  traceplot(s, "tau2", inc_warmup=F)
  traceplot(s, "mean_beta", inc_warmup=F)
  traceplot(s, "df", inc_warmup=F)
  traceplot(s, "sigma", inc_warmup=F)
  traceplot(s, "ysigma", inc_warmup=F)
  #traceplot(s, "beta", inc_warmup=F)
  # etc.
}

# Low-level correlation matrix over price level, trend, etc.
# Is of general interest
Omega <- matrix(apply(apply(extract(s, "LOmega")[[1]], 1, function (m) m %*% t(m)), 1, mean), c(3, 3))
saveRDS(Omega, "data/Omega.rds")
Omega1 <- matrix(apply(apply(extract(s, "LOmega1")[[1]], 1, function (m) m %*% t(m)), 1, mean), c(6, 6))
if (F) saveRDS(Omega1, "data/Omega1.rds")

raise6 <- function (a) {
  dim.a <- dim(a); N <- length(dim.a)
  apply(a, 1:(N-1), function (v) c(v, rep(0, 6-dim.a[N]))) %>%
  aperm(c(2:N, 1))
} 
  
beta.prm.mean <- function (v) apply(extract(s, v)[[1]], c(2, 3), mean) %>% raise6
beta.prm <- function (v) extract(s, v)[[1]] %>% raise6

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

beta.names <- c("lprice", "trend", "quad", "k.lprice", "k.trend", "k.quad")

par.tbl <- function(d, v.name, b.name, name.postfix) 
  data.frame(levels(d[[v.name]]), beta.prm.mean(b.name)) %>% 
  setNames(c(v.name, 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df()

par.tbl.long <- function(d, v.name, b.name, name.postfix) {
  samples <- beta.prm(b.name)
  data.frame(expand.grid(1:dim(samples)[[1]], levels(d[[v.name]])), 
             array(samples, c(dim(samples)[[1]]*dim(samples)[[2]], dim(samples)[[3]]))) %>% 
  setNames(c("sample", v.name, 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df() }

mean.tbl.long <- function (name.postfix="4") 
  extract(s, "mean_beta")[[1]] %>% { 
    data.frame(sample=1:dim(.)[[1]], .) } %>% 
  setNames(c("sample", 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df() 

load("data/pnro_data_20150318.RData")
pnro.area <- pnro.dat %>% transmute(pnro=pnro, log.density = -log(density_per_km2)/10) # FIXME: this is in two places
pnro <- pnro.area$pnro
n.samples <- length(extract(s, "lp__")[[1]])
# For NA pnro's in the model, look for upper level in the hierarchy and take beta1 etc.
res.long <- data.frame(pnro.area, level1 = l1(pnro), level2 = l2(pnro), level3 = l3(pnro)) %>% 
  merge(data.frame(sample=1:n.samples)) %>% 
  filter(is.finite(log.density)) %>%
  left_join(par.tbl.long(d, "pnro",   "beta",  ""),  by=c("pnro",   "sample")) %>%
  left_join(par.tbl.long(d, "level1", "beta1", "1"), by=c("level1", "sample")) %>% 
  left_join(par.tbl.long(d, "level2", "beta2", "2"), by=c("level2", "sample")) %>% 
  left_join(mean.tbl.long(                     "4"), by=c(          "sample")) %>% 
  mutate(pnro=pnro, 
            log.density = log.density,
            lprice=sum.0na(lprice, lprice1, lprice2, lprice4) + 
              sum.0na(k.lprice, k.lprice1, k.lprice2,  k.lprice4) * log.density, 
            trend=sum.0na(trend, trend1, trend2,  trend4) +
              sum.0na(k.trend, k.trend1, k.trend2,  k.trend4) * log.density, 
            quad=sum.0na(quad, quad1, quad2,  quad4) +
              sum.0na(k.quad, k.quad1, k.quad2,  k.quad4) * log.density
  ) %>%
  # Original unit is decade, for vars below it is year. 
  # d/d.yr lprice = trend + 2*quad*yr
  # d/(10*d.yr) lprice = trend/10 + 2*quad*yr/10
  # d^2/(10*d.yr)^2 lprice = 2*quad/100
  # trendi is as percentage / 100.
  # trendimuutos is as percentage units / 100 / year.
  mutate(hinta = exp(6 + lprice), trendi = trend/10, trendimuutos = 2*quad/100, 
         hinta2016 = exp(6 + lprice + trend*year2yr(2016) + quad*year2yr(2016)**2),
         trendi2016 = (trend + 2*quad*year2yr(2016))/10) %>%
  tbl_df()

res <- res.long %>% group_by(pnro, log.density) %>% 
 summarise(lprice = mean(lprice), 
           hinta2016=mean(hinta2016), trendi2016=mean(trendi2016), trendimuutos=mean(trendimuutos)) %>%
  ungroup()
   
write.table(res %>% select(-log.density),  "data/pnro-hinnat.txt", row.names=F, quote=F)
saveRDS(res, "data/pnro-hinnat.rds")

# FIXME: exp(6 + lprice + trend*year2yr(2016) + quad*year2yr(2016)**2) in two places, 
# make a function.

years = 2005:2016
predictions <- 
  expand.grid(sample=unique(res.long$sample), 
              year=years, 
              pnro=unique(res.long$pnro)) %>% tbl_df %>% #head(10000) %>%
  left_join(res.long %>% select(pnro, sample, lprice, trend, quad), by=c("sample", "pnro")) %>%
  mutate(hinta = exp(6 + lprice + trend*year2yr(year) + quad*year2yr(year)**2)) %>%
  group_by(pnro, year) %>% 
  do(data.frame(hinta = mean(.$hinta), #hinta_sd = sd(.$hinta), 
                hinta10 = quantile(.$hinta, .1), 
                hinta25 = quantile(.$hinta, .25), 
                hinta50 = quantile(.$hinta, .5), 
                hinta75 = quantile(.$hinta, .75), 
                hinta90 = quantile(.$hinta, .9))) %>%
  ungroup() %>%
  left_join(d %>% select(pnro, year, obs_hinta=price, n_kaupat=n), by=c("year", "pnro"))

saveRDS(predictions, "predictions.rds")

pnro.plot <- function (ipnro) {
  d.pnro <- predictions %>% filter(pnro %in% ipnro) 
  p <- ggplot(d.pnro, aes(x=year, y=hinta)) + 
    geom_line() +
    geom_ribbon(aes(ymin=hinta25, ymax=hinta75), alpha=.2) +
    geom_ribbon(aes(ymin=hinta10, ymax=hinta90), alpha=.2) 
  d2.pnro <- d.pnro %>% filter(!is.na(n_kaupat))
  if (nrow(d2.pnro)>0) 
    p <- p + geom_point(data=d2.pnro, aes(x=year, y=obs_hinta, size=n_kaupat)) +
    scale_size_continuous(name="Kauppojen\nmäärä")
  p + ggtitle("Neliöhintoja 2005-2016, 50% ja 80% luottamusvälit") + theme_bw(15) + 
    scale_x_continuous(breaks=c(2006, 2010, 2014), labels=c("-06", "-10", "-14")) + 
    #scale_y_log10(breaks=c(200, 500, 700, 1000, 2000, 5000, 7000, 10000)) +
    facet_wrap(~ pnro) + xlab("Vuosi")
}

library(ggplot2)

pnro.plot(c("02620", "02940", "02210", "00320", "59130", "00100", "16230", "33100", "09120"))
predictions$pnro %>% (function (i) i[grep("^02", i)]) %>% unique %>% pnro.plot(.)
predictions$pnro %>% (function (i) i[grep("^56", i)]) %>% unique %>% pnro.plot(.)
predictions$pnro %>% (function (i) i[grep("^59", i)]) %>% unique %>% pnro.plot(.)
predictions$pnro %>% (function (i) i[grep("^90", i)]) %>% unique %>% pnro.plot(.)

ggplot(res, aes(x=-log.density, y=lprice, color=l3(pnro))) + geom_point(alpha=.5, size=3) + 
  xlab("log.density") + ylab("log price - 6") + theme_minimal(15) #+ 
  #geom_smooth(method="gam", formula = y ~ s(x), se=F)
ggsave("figs/density-lprice.png")
ggplot(res, aes(x=-log.density, y=trendi2016, color=l3(pnro))) + geom_point(alpha=.5, size=3) +
  xlab("log.density") + ylab("trend per year") + theme_minimal(15) #+ 
  #geom_smooth(method="gam", formula = y ~ s(x), se=F)
ggsave("figs/density-trend.png")
ggplot(res, aes(x=-log.density, y=trendimuutos, color=l3(pnro))) + geom_point(alpha=.5, size=3) +
  xlab("log.density") + ylab("trend change per year") + theme_minimal(15) #+ 
  #geom_smooth(method="gam", formula = y ~ s(x), se=F)
ggsave("figs/density-trendchange.png")

ggplot(res, aes(x=lprice, fill=l3(pnro))) + geom_histogram(binwidth=.04) +
  xlab("log price - 6") + ylab("") + theme_minimal(15)
ggsave("figs/lprice-histogram.png")
ggplot(res, aes(x=trendi2016, fill=l3(pnro))) + geom_histogram(binwidth=.002) +
  xlab("trend") + ylab("") + theme_minimal(15)
ggsave("figs/trendi-histogram.png")
ggplot(res, aes(x=trendimuutos, fill=l3(pnro))) + geom_histogram(binwidth=.0004) +
  xlab("trend change") + ylab("") + theme_minimal(15)
ggsave("figs/trendimuutos-histogram.png")



## Plot with alternative coordinates
load("data/pnro_spatial_epsg2393.RData") # pnro.sp.alt appears here
pnro.hinnat.sp <- merge(pnro.sp.alt, data.frame(res))
pdf("figs/pnro_prices.pdf")
spplot(pnro.hinnat.sp, zcol="lprice", lwd=0.00, col="transparent", main="Log price")
spplot(pnro.hinnat.sp, zcol="trendi2016", lwd=0.00, col="transparent", main="Trend 2016")
spplot(pnro.hinnat.sp, zcol="trendimuutos", lwd=0.00, col="transparent", main="Trend change per year")
dev.off()

pk.sp <- pnro.hinnat.sp[substr(pnro.hinnat.sp$pnro, 1, 2) %in% c("00", "01", "02"),]
pdf("figs/pk-pnro_prices.pdf")
spplot(pk.sp, zcol="lprice", lwd=0.00, col="transparent", main="Log price")
spplot(pk.sp, zcol="trendi2016", lwd=0.00, col="transparent", main="Trend 2016")
spplot(pk.sp, zcol="trendimuutos", lwd=0.00, col="transparent", main="Trend change per year")
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

# Plots for blogs

predictions$pnro %>% (function (i) i[grep("^02[01234]", i)]) %>% unique %>% pnro.plot(.)
ggsave("figs/espoota.png", dpi=150)

png("figs/raw-vs-model.png", width=1024, height=1024)
p1 <- spplot(pnro.hinnat.sp.raw, zcol="lprice", lwd=0.00, at=seq(-0.5, 3, .1), 
             col="transparent", main="Keskihinta raakana (log)")
p2 <- spplot(pnro.hinnat.sp, zcol="lprice", lwd=0.00, at=seq(-0.5, 3, .1),
             col="transparent", main="Keskihinta mallista (log)")
print(p1, split=c(1, 1, 2, 1), more=T)
print(p2, split=c(2, 1, 2, 1), more=F)
dev.off()

p1 <-
d %>% select(pnro, year, n)  %>% tidyr::spread(year, n, fill=NA) %>% sample_n(70) %>% 
  tidyr::gather(year, n, -pnro) %>%
  ggplot(aes(x=year, y=pnro, fill=n)) + 
  geom_tile() + xlab("Vuodet") + ylab("Postinumero") + 
  theme_minimal(18) +
  scale_x_discrete(labels=NULL) +
  #theme(legend.justification=c(1,0), legend.position=c(1,0),
  #      legend.background = element_rect(fill="#f0f0f0d0", size=0)) +
  theme(axis.text.y = element_text(size=10), axis.ticks.x=element_blank()) +
  scale_fill_gradient(low = "#f7fcf5", high = "#005a32", na.value="red", trans="sqrt",
                      breaks=c(6, 30, 100, 300, 1000), name="Kauppojen\nmäärä") 
#ggsave("figs/raakadata.png", dpi=150)

p2 <- 
d %>% select(pnro, year, n)  %>% tidyr::spread(year, n, fill=0) %>% 
  tidyr::gather(year, n, -pnro) %>% { .[order(.$n),]} %>% 
  mutate(i=row_number()/n()*100) %>% 
  ggplot(aes(x=i, y=n)) + geom_line() + 
  scale_y_continuous(trans = "log1p", breaks=c(0, 6, 10, 30, 100, 1000)) + 
  scale_x_continuous(breaks=c(0, 17.5, 50, 100)) +
  ylab("Kauppojen määrä") + xlab("Postinumero-vuosi-yhdistelmät (%)") +
  theme_minimal(18)

png("figs/harvuus.png", width=1024, height=512)
gridExtra::grid.arrange(p1, p2, ncol=2, widths=c(.5, 1), heights=c(1, 1))
dev.off()

p1 <- 
ggplot(res, aes(x=-log.density, y=lprice, color=l3(pnro))) + geom_point(alpha=.55, size=2.5) + 
  guides(color=F) + 
  scale_color_hue(l=55) +
  xlab("Asukastiheys (log)") + ylab("Hinta (log) ") + theme_minimal(18) #+ 
p2 <- 
ggplot(res, aes(x=-log.density, y=trendi2016, color=l3(pnro))) + geom_point(alpha=.55, size=2.5) +
  guides(color=F) +
  scale_fill_hue(l=55) +
  xlab("") + ylab("Trendi") + theme_minimal(18) #+ 
p3 <- 
ggplot(res, aes(x=-log.density, y=trendimuutos, color=l3(pnro))) + geom_point(alpha=.55, size=2.5) +
  scale_color_hue(l=55, name="Alue\n(postinumeron\n1. numero)") +
  xlab("") + ylab("Trendin muutos") + theme_minimal(18) #+ 

png("figs/tiheys-korrelaatiot.png", width=1024, height=1024/3.5)
gridExtra::grid.arrange(p1, p2, p3, ncol=3, widths=c(1, 1, 1.5), heights=c(1, 1, 1))
dev.off()



# JSONs

res %>% plyr::dlply("pnro", function (i) list(hinta2016=i$hinta2016, 
                                              trendi2016=i$trendi2016, 
                                              trendimuutos=i$trendimuutos)) %>% 
  toJSON %>% writeLines("json/trends.json")

predictions %>% group_by(pnro) %>% # filter(pnro %in% c("02940", "00100")) %>%
  plyr::d_ply("pnro", function (i) list(year=i$year, 
                                        hinta10=i$hinta10, 
                                        hinta25=i$hinta25, 
                                        hinta50=i$hinta50, 
                                        hinta75=i$hinta75, 
                                        hinta90=i$hinta90, 
                                        obs_hinta=i$obs_hinta, 
                                        n_kaupat=i$n_kaupat) %>% toJSON %>%
                      writeLines(., paste("json/predictions/", i$pnro[[1]], ".json",  sep=""))
              )

d %>% select(pnro, year, n)  %>% tidyr::spread(year, n, fill=0) %>% tidyr::gather(year, n, -pnro) %>% { .[order(.$n),]} %>% mutate(i=row_number()) %>% ggplot(aes(x=i, y=n)) + geom_line() + scale_y_continuous(trans = "log1p", breaks=c(0, 6, 10, 100, 1000))

d %>% select(pnro, year, n)  %>% group_by(pnro) %>% summarise(n=sum(n))  %>% { .[order(.$n),]} %>% mutate(i=row_number()) %>% ggplot(aes(x=i, y=n)) + geom_line() + scale_y_continuous(trans = "log1p", breaks=c(0, 6, 10, 100, 1000))


