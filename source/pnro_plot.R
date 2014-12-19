# Plot pnro hinnat on a map

# Read hinta data
pnro.hinnat <- readRDS("data/pnro-hinnat.rds")

## Plot with alternative coordinates
load("data/pnro_spatial_epsg2393.RData") # pnro.sp.alt appears here
pnro.hinnat.sp <- merge(pnro.sp.alt, pnro.hinnat)
pdf("figs/pnro_prices.pdf")
spplot(pnro.hinnat.sp, zcol="lhinta", lwd=0.00, col="transparent", main="log.price")
spplot(pnro.hinnat.sp, zcol="trendi", lwd=0.00, col="transparent", main="trend")
spplot(pnro.hinnat.sp, zcol="quad", lwd=0.00, col="transparent", main="quadratic")
dev.off()

