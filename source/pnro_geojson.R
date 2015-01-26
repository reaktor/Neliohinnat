# Script for making a geojson of the pnro data

library("rgdal")

# Read price and spatial data and merge
pnro.hinnat <- readRDS("data/pnro-hinnat.rds")
load("data/pnro_spatial_wgs84.RData") # pnro.sp.alt appears here
pnro.hinnat.sp <- merge(pnro.sp, pnro.hinnat)


# Remove -Inf values and NAs
# Update: No more -Inf or Inf
# infs <- unique(unlist(lapply(pnro.hinnat.sp@data, function(x) which(x == -Inf | x == Inf))))
# pnro.hinnat.sp <- pnro.hinnat.sp[-infs, ]
nas <-  unique(unlist(lapply(pnro.hinnat.sp@data, function(x) which(is.na(x)))))
pnro.hinnat.sp <- pnro.hinnat.sp[-nas, ]

# Add fill
col.scale <- rainbow(100)
cuts <- cut(pnro.hinnat.sp$trendi, 100)
cols <- col.scale[match(cuts, levels(cuts))]
# remove alpha
cols <- tolower(substr(cols, 1, 7))
pnro.hinnat.sp$fill <- cols
# pnro.hinnat.sp[["stroke"]] <- "#ffffff"
# pnro.hinnat.sp[["stroke-opacity"]] <- 0
pnro.hinnat.sp[["stroke-width"]] <- 0

# Write as GeoJSON (can not specify file type for some reason, rename afterwards)
rgdal::writeOGR(obj=pnro.hinnat.sp, dsn="pnro_prices_geojson", layer="postinumerot 20140217", driver="GeoJSON")
file.rename("pnro_prices_geojson", "pnro_prices.geojson")
# validate with http://geojsonlint.com/



# Take only Helsinki as an example
pnro.hinnat.pks.sp <- pnro.hinnat.sp[substr(pnro.hinnat.sp$pnro, 1, 2) %in% c("00", "01", "02"), ]

rgdal::writeOGR(obj=pnro.hinnat.pks.sp, dsn="pnro_pks_prices_geojson", layer="postinumerot 20140217", driver="GeoJSON")
file.rename("pnro_pks_prices_geojson", "pnro_pks_prices.geojson")



# Write only polygons as GeoJSON (can not specify file type for some reason, rename afterwards)
rgdal::writeOGR(obj=pnro.sp, dsn="pnro_areas_geojson", layer="postinumerot 20140217", driver="GeoJSON")
file.rename("pnro_areas_geojson", "pnro_areas.geojson")

