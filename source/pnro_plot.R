# Plot pnro hinnat on a map

# Read hinta data
pnro.hinnat <- read.table("pnro-hinnat.txt", sep=" ", header=TRUE, colClasses="character")
pnro.hinnat$hintataso <- as.numeric(pnro.hinnat$hintataso)
pnro.hinnat$trendi <- as.numeric(pnro.hinnat$trendi)

# Load pnro spatial data processed in 'get_pnro_areas.R'
load("pnro_spatial_wgs84.RData")

# Merge data
pnro.hinnat.sp <- merge(pnro.sp, pnro.hinnat)

# Plot
png("pnro_trendi1.png", width=2000, height=3000)
spplot(pnro.hinnat.sp, zcol="trendi")
dev.off()


## Plot with alternative coordinates
load("pnro_spatial_epsg2393.RData")
pnro.hinnat.sp <- merge(pnro.sp.alt, pnro.hinnat)
png("pnro_trendi2.png", width=2000, height=3000)
spplot(pnro.hinnat.sp, zcol="trendi")
dev.off()
