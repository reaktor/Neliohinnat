# Plot pnro hinnat on a map

# Read hinta data
pnro.hinnat <- readRDS("data/pnro-hinnat.rds")
pnro.hinnat$current.trend <- with(pnro.hinnat, trendi + (2015-2009.4)/10*quad)

## Plot with alternative coordinates
load("data/pnro_spatial_epsg2393.RData") # pnro.sp.alt appears here
pnro.hinnat.sp <- merge(pnro.sp.alt, pnro.hinnat)
pdf("figs/pnro_prices.pdf")
spplot(pnro.hinnat.sp, zcol="lhinta", lwd=0.00, col="transparent", main="log.price")
spplot(pnro.hinnat.sp, zcol="trendi", lwd=0.00, col="transparent", main="trend")
spplot(pnro.hinnat.sp, zcol="quad", lwd=0.00, col="transparent", main="quadratic")
spplot(pnro.hinnat.sp, zcol="current.trend", lwd=0.00, col="transparent", main="Trend 2015")
dev.off()

pk.sp <- pnro.hinnat.sp[substr(pnro.hinnat.sp$pnro, 1, 2) %in% c("00", "01", "02"),]
pdf("figs/pk-pnro_prices.pdf")
spplot(pk.sp, zcol="lhinta", lwd=0.00, col="transparent", main="log.price")
spplot(pk.sp, zcol="trendi", lwd=0.00, col="transparent", main="trend")
spplot(pk.sp, zcol="quad", lwd=0.00, col="transparent", main="quadratic")
spplot(pk.sp, zcol="current.trend", lwd=0.00, col="transparent", main="Trend 2015")
dev.off()

d <- readRDS("data/d.rds")


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



