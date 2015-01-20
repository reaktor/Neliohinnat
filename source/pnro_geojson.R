# Script for making a geojson of the pnro data

library("rgdal")

# Read price and spatial data and merge
pnro.hinnat <- readRDS("data/pnro-hinnat.rds")
load("data/pnro_spatial_wgs84.RData") # pnro.sp.alt appears here
pnro.hinnat.sp <- merge(pnro.sp, pnro.hinnat)

# Write only polygons as GeoJSON (can not specify file type for some reason, rename afterwards)
rgdal::writeOGR(obj=pnro.sp, dsn="pnro_areas_geojson", layer="postinumerot 20140217", driver="GeoJSON")
file.rename("pnro_areas_geojson", "pnro_areas.geojson")


# Remove -Inf values
infs <- unique(unlist(lapply(pnro.hinnat.sp@data, function(x) which(x==Inf))))
pnro.hinnat.sp <- pnro.hinnat.sp[-infs, ]

# Write as GeoJSON (can not specify file type for some reason, rename afterwards)
rgdal::writeOGR(obj=pnro.hinnat.sp, dsn="pnro_prices_geojson", layer="postinumerot 20140217", driver="GeoJSON")
file.rename("pnro_prices_geojson", "pnro_prices.geojson")
# validate with http://geojsonlint.com/

# Take only Helsinki as an example
pnro.hinnat.pks.sp <- pnro.hinnat.sp[substr(pnro.hinnat.sp$pnro, 1, 2) %in% c("00", "01", "02"), ]

rgdal::writeOGR(obj=pnro.hinnat.pks.sp, dsn="pnro_pks_prices_geojson", layer="postinumerot 20140217", driver="GeoJSON")
file.rename("pnro_pks_prices_geojson", "pnro_pks_prices.geojson")


# Try as gist
library("devtools")
install_github("ropensci/rgbif")
library("rgbif")
?gist("")

