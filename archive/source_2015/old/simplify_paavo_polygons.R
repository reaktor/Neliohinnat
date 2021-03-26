# Script for exploring options to simplify the Paavo postal code area polygons

library("rgdal")
library("dplyr")


# Paavo data Data retrieved in get_data.R
load("data/pno_data_20150316.RData")

# Earlier data retrieved in source/old/get_pnro_areas.R
load("data/pnro_spatial_wgs84.RData")

## SIMPLIFY DATA #########

pno.sp@data <- pno.sp@data %>%
  mutate(area_km2 = round(area / 1e6, d=2)) %>%
#         population = round(population / 1e3, d=1)) %>% # use population as is
  select(pno, name, municipality, area_km2, population)
  
## SIMPLIFY POLYGONS #######

# Process data a bit
pno.sp <- pno.sp[order(pno.sp@data$pno),]
pnro.sp@data$rand <- runif(nrow(pnro.sp@data))

# Plot HKI region with original data
pdf("temp/duukkis_pks.pdf", width=10, height=10)
spplot(pnro.sp[1:200,], zcol="rand")
dev.off()

pdf("temp/paavo_pks.pdf", width=10, height=10)
spplot(pno.sp[1:200,], zcol="population")
dev.off()

# Write geojson
rgdal::writeOGR(obj=pnro.sp, dsn="temp/geojson_duukkis", layer="pnro", driver="GeoJSON")
rgdal::writeOGR(obj=pno.sp, dsn="temp/geojson_paavo", layer="pnro", driver="GeoJSON")


## TODO
# get data smaller
# maybe have to apply different simplifying to different sizes of polygons?
# also include rounding?
# try maptools::thinnedSpatialPoly()
# REMOVE ISLANDS!!!

# Huge difference in object sizes:
object.size(pno.sp)
# 29478512 bytes
object.size(pnro.sp)
# 10510816 bytes

# Check lengths / resolution
len.new <- sapply(pno.sp@polygons, function(p) nrow(p@Polygons[[1]]@coords))
c(mean(len.new), sum(len.new))
#  147.4648 447703.0000

len.old <- sapply(pnro.sp@polygons, function(p) nrow(p@Polygons[[1]]@coords))
c(mean(len.old), sum(len.old))
#   24.06351 73129.00000
# => pno.sp defines polygons in much more detailed level

## Check number of polygons per postal code
n.new <- sapply(pno.sp@polygons, function(x) length(x@Polygons))
length(which(n.new > 1)) # 323
# spplot(pno.sp[n.new > 1, ], zcol="population")
table(n.new)

n.old <- sapply(pnro.sp@polygons, function(x) length(x@Polygons))
length(which(n.old > 1)) # 44
# spplot(pnro.sp[n.old > 1, ], zcol="population")
table(n.old)
#    1    2    3    6 
# 2995   39    4    1


## 1. ROUNDING ####

# Change to WGS84
pno.sp <- spTransform(pno.sp, CRS("+proj=longlat +ellps=WGS84"))
rgdal::writeOGR(obj=pno.sp, dsn="temp/geojson_paavo_wgs84", layer="pnro", driver="GeoJSON")

# Check
head(pnro.sp@polygons[[1]]@Polygons[[1]]@coords)

# Round
# need to lapply to multi polygons!!! :
pno.sp.rounded <- pno.sp
pno.sp.rounded@polygons <- lapply(pno.sp.rounded@polygons, function(p) {res=p;
                                                                        for (pi in 1:length(res@Polygons))
                                                                          res@Polygons[[pi]]@coords=round(res@Polygons[[pi]]@coords, d=3);
                                                                        res})
# CHECK
lapply(pno.sp.rounded@polygons[[4]]@Polygons, function(x) head(x@coords))
rgdal::writeOGR(obj=pno.sp.rounded, dsn="temp/geojson_paavo_wgs84_rd3", layer="pnro", driver="GeoJSON")

len.new <- sapply(pno.sp.rounded@polygons, function(p) nrow(p@Polygons[[1]]@coords))
c(mean(len.new), sum(len.new))
# 147.4648 447703.0000


## SIMPLIFY #######

# Simplify polygons using rgeos::gSimplify() or maptools::thinnedSpatialPoly
library("rgeos")
library("maptools")

## Using rgeos::gSimplify

temp <- rgeos::gSimplify(pno.sp.rounded, tol=0.001, topologyPreserve = FALSE)
pno.sp.simple <- SpatialPolygonsDataFrame(Sr=temp, data=pno.sp@data)
len.new <- sapply(pno.sp.simple@polygons, function(p) nrow(p@Polygons[[1]]@coords))
c(mean(len.new), sum(len.new))
# 38.58498 117144.00000 with tol=0.001

# Compare rounded polygons on Helsinki region
pdf("temp/paavo_wgs84_rd3_tol0.001F.pdf", width=10, height=10)
spplot(pno.sp.simple[1:200,], zcol="population")
dev.off()
rgdal::writeOGR(obj=pno.sp.simple, dsn="temp/geojson_paavo_wgs84_rd3_tol0.001F", layer="pnro", driver="GeoJSON")


## Use maptools::thinnedSpatialPoly instead, filters small extra polygons aways

# Get areas
pno.areas <- lapply(pno.sp@polygons, function(x) sapply(x@Polygons, function(y) y@area))
hist(log10(unlist(pno.areas)))

# Focus on multi-polygon codes
pno.areas.multi <- pno.areas[sapply(pno.areas, length) > 1]
hist(log10(unlist(pno.areas.multi)))
area.th <- 1e-3
table(sapply(pno.areas.multi, function(x) any(x > area.th)))

# Try removing these with thinnedSpatialPoly
pno.sp.simple <- maptools::thinnedSpatialPoly(SP=pno.sp.rounded, tolerance = 0.002, minarea = 1e-3, topologyPreserve = FALSE, avoidGEOS = TRUE)
len.new <- sapply(pno.sp.simple@polygons, function(p) nrow(p@Polygons[[1]]@coords))
c(mean(len.new), sum(len.new))
# 40.583 123210.000 with tol=0.001 & minarea = 1e-3
# 29.60639 89885.00000 with tol=0.002 & minarea = 1e-3
pdf("temp/paavo_wgs84_rd3_tol0.002F_R_min1e-3.pdf", width=10, height=10)
spplot(pno.sp.simple[1:200,], zcol="population")
dev.off()
rgdal::writeOGR(obj=pno.sp.simple, dsn="temp/geojson_paavo_wgs84_rd3_tol0.002F_R_min1e-3", layer="pnro", driver="GeoJSON")


# How many multi polygons left?
pno.simple.areas <- lapply(pno.sp.simple@polygons, function(x) sapply(x@Polygons, function(y) y@area))
table(sapply(pno.simple.areas, length))
#    1    2    3    4    5    7    8 
# 2959   63    9    1    2    1    1 with tol=0.001 & minarea = 1e-3


# pno.simple.areas.multi <- pno.simple.areas[sapply(pno.simple.areas, length) > 1]
# hist(log10(unlist(pno.simple.areas.multi)))



## OLD STUFF #######


# ## Even larger tol -values, removes some pno
# temp <- rgeos::gSimplify(pno.sp, tol=500, topologyPreserve = FALSE)
# pno.sp.simple <- SpatialPolygonsDataFrame(Sr=temp, data=pno.sp@data[sapply(temp@polygons, function(x) x@ID),])
# mean(sapply(pno.sp.simple@polygons, function(p) nrow(p@Polygons[[1]]@coords)))


# 

# This is the same as above
pno.sp.simple <- maptools::thinnedSpatialPoly(SP=pno.sp, tolerance = 100, minarea = 0, topologyPreserve = FALSE, avoidGEOS = FALSE)
len.new <- sapply(pno.sp.simple@polygons, function(p) nrow(p@Polygons[[1]]@coords))
c(mean(len.new), sum(len.new))

# Run without rgeos
pno.sp.simple <- maptools::thinnedSpatialPoly(SP=pno.sp, tolerance = 200, minarea = 0, topologyPreserve = FALSE, avoidGEOS = TRUE)
len.new <- sapply(pno.sp.simple@polygons, function(p) nrow(p@Polygons[[1]]@coords))
c(mean(len.new), sum(len.new))
# 100F: 35.71443 108429.00000
# 200F: 26.00395 78948.00000

pdf("temp/pnro_pks_simple_temp_tol200F_R.pdf", width=10, height=10)
spplot(pno.sp.simple[1:200,], zcol="population")
dev.off()
rgdal::writeOGR(obj=pno.sp.simple, dsn="temp/pno_simple_geojson", layer="pnro", driver="GeoJSON")


## Check number of polygons per postal code
temp = sapply(pno.sp@polygons, function(x) length(x@Polygons))
length(which(temp > 1))
spplot(pno.sp[temp > 1, ], zcol="population")



# 
pno.sp.simple <- maptools::thinnedSpatialPoly(SP=pno.sp, tolerance = 100, minarea = 300000, topologyPreserve = FALSE, avoidGEOS = TRUE)
len.new <- sapply(pno.sp.simple@polygons, function(p) nrow(p@Polygons[[1]]@coords))
c(mean(len.new), sum(len.new))
pdf("temp/pnro_pks_simple_temp_tol100F_R_min3e5.pdf", width=10, height=10)
spplot(pno.sp.simple[1:200,], zcol="population")
dev.off()

rgdal::writeOGR(obj=pno.sp.simple, dsn="temp/pno_simple_geojson", layer="pnro", driver="GeoJSON")

pno.areas <- lapply(pno.sp.simple@polygons, function(x) sapply(x@Polygons, function(y) y@area))
hist(log10(unlist(pno.areas)))
