# Script for retrieving Finnish postal code areas

# To install rgdal and rgeos on OSX, follow these instructions
# http://tlocoh.r-forge.r-project.org/mac_rgeos_rgdal.html


# Download the kml data from http://www.palomaki.info/apps/pnro/
# Go to https://www.google.com/fusiontables/DataSource?docid=14HKp89uGtLzO0PEeZszVcBfFwzhIYpY5M-s33hk#map:id=3
# and File -> Download -> kml
# This didn't work
# destfile <- "pnro.kml"
# url <- "https://www.google.com/fusiontables/exporttable?query=select+col2+from+14HKp89uGtLzO0PEeZszVcBfFwzhIYpY5M-s33hk&o=kml&g=col2&styleId=2&templateId=2"
# download.file(url, destfile)

# Read kml
# library("rgdal")
# filename <- "postinumerot 20140217.kml"
# ogrListLayers(filename)
# pnro.sp <- readOGR(dsn=filename, layer="postinumerot 20140217")
# # Clean the data (can remove everything else than the name)
# pnro.sp@data <- pnro.sp@data[1]
# names(pnro.sp@data) <- "pnro"
# pnro.sp@data$pnro <- as.character(pnro.sp@data$pnro)
# # Save
# save(pnro.sp, file="pnro_spatial_wgs84.RData")

library("devtools")
install_github("ropengov/gisfin")
library("gisfin")
pnro.sp <- get_postalcode_areas()
save(pnro.sp, file="data/pnro_spatial_wgs84.RData")

# Add random value and plot it
pnro.sp@data$rand <- runif(nrow(pnro.sp@data))
png("pnro_temp.png", width=1000, height=1000)
spplot(pnro.sp, zcol="rand")
dev.off()

# Save also in epsg:2393 coordinates (better for northern Finland)
pnro.sp.alt <- spTransform(pnro.sp, CRS("+init=epsg:2393"))

# Add also area
library("rgeos")
pnro.sp.alt@data$area.m2 <- rgeos::gArea(pnro.sp.alt, byid=TRUE)
save(pnro.sp.alt, file="data/pnro_spatial_epsg2393.RData")

# Write only polygons as GeoJSON (can not specify file type for some reason, rename afterwards)
rgdal::writeOGR(obj=pnro.sp, dsn="pnro_areas_geojson", layer="pnro", driver="GeoJSON")
file.rename("pnro_areas_geojson", "json/pnro_areas.geojson")

# Compute neighbours
library("spdep")
pnro.nb.list <- spdep::poly2nb(pnro.sp, row.names=pnro.sp@data$pnro)
# Check neighbours of "00100"
attributes(pnro.nb.list)$region.id[pnro.nb.list[[1]]]
# "00120" "00130" "00170" "00180" "00250" "00260" "00530"

# Write as JSON
pnro.nb.list2 <- lapply(pnro.nb.list, function(x) x = attributes(pnro.nb.list)$region.id[x])
names(pnro.nb.list2) <- attributes(pnro.nb.list)$region.id

library("rjson")
pnro.nb.json <- toJSON(pnro.nb.list2)
writeLines(pnro.nb.json, con="json/pnro_neighbors.json")
# The tidy the format (i.e. add ends of lines) using http://jsonlint.com/