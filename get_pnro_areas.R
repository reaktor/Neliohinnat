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
library("rgdal")
filename <- "postinumerot 20140217.kml"
ogrListLayers(filename)
pnro.sp <- readOGR(dsn=filename, layer="postinumerot 20140217")
# Clean the data (can remove everything else than the name)
pnro.sp@data <- pnro.sp@data[1]
names(pnro.sp@data) <- "pnro"
pnro.sp@data$pnro <- as.character(pnro.sp@data$pnro)
# Save
save(pnro.sp, file="pnro_spatial_wgs84.RData")


# Add random value and plot it
pnro.sp@data$rand <- runif(nrow(pnro.sp@data))
png("pnro_temp.png", width=1000, height=1000)
spplot(temp, zcol="rand")
dev.off()

# Save also in epsg:2393 coordinates (better for northern Finland)
pnro.sp.alt <- spTransform(pnro.sp, CRS("+init=epsg:2393"))
save(pnro.sp.alt, file="pnro_spatial_epsg2393.RData")
