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
