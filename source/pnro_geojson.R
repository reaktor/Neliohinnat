# Script for making a geojson of the pnro data

library("rgdal")

# Read price and spatial data and merge
pnro.hinnat <- readRDS("data/pnro-hinnat.rds")
load("data/pnro_spatial_wgs84.RData") # pnro.sp.alt appears here
pnro.hinnat.sp <- merge(pnro.sp, pnro.hinnat)

# Write as GeoJSON (can not specify file type for some reason, rename afterwards)
rgdal::writeOGR(obj=pnro.sp, dsn="pnro_prices_geojson", layer="postinumerot 20140217", driver="GeoJSON")
file.rename("pnro_prices_geojson", "pnro_prices.geojson")
# validate with http://geojsonlint.com/