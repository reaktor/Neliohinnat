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
load("data/pnro_spatial_wgs84.RData")

# Add random value and plot it
pnro.sp@data$rand <- runif(nrow(pnro.sp@data))
pdf("temp/pnro_temp.pdf", width=10, height=20)
spplot(pnro.sp, zcol="rand")
dev.off()

# Try rounding polygon coordinates
pnro.sp.rounded <- pnro.sp
pnro.sp.rounded@polygons <- lapply(pnro.sp.rounded@polygons, function(p) {res=p;
                                                                          res@Polygons[[1]]@coords=round(res@Polygons[[1]]@coords, d=3);
                                                                          res})

# Compare rounded polygons on Helsinki region
pdf("temp/pnro_pks_temp.pdf", width=10, height=10)
spplot(pnro.sp[1:200,], zcol="rand")
dev.off()
pdf("temp/pnro_pks_rounded_temp_d3.pdf", width=10, height=10)
spplot(pnro.sp.rounded[1:200,], zcol="rand")
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

# Write also rounded polygons
rgdal::writeOGR(obj=pnro.sp.rounded, dsn="pnro_areas_round-d3_geojson", layer="pnro", driver="GeoJSON")
file.rename("pnro_areas_round-d3_geojson", "json/pnro_areas_round-d3.geojson")

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
message("Then tidy the format (i.e. add ends of lines) using http://jsonlint.com/")

# # Get postal code info using sorvi
# library("devtools")
# install_github("ropengov/sorvi")
# library("sorvi")
# postal.code.table <- get_postal_code_info()
# # Process into json
# pnros <- as.vector(pnro.sp@data$pnro)
# names(pnros) <- pnros
# pnro.info.list <- lapply(pnros, function(p) postal.code.table$municipality[match(p, postal.code.table$postal.code)])
# pnro.info.json <- toJSON(pnro.info.list)
# writeLines(pnro.info.json, con="json/pnro_info.json")
# message("Then tidy the format (i.e. add ends of lines) using http://jsonlint.com/")

# Alternatively from Paavo
library("devtools")
install_github("ropengov/pxweb")
library("pxweb")
d <- interactive_pxweb(api = "statfi")
myDataSetName <- get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/2015/paavo_9_koko_2015.px",
                 dims = list(Postinumeroalue = c('*'), Tiedot = c('*')), clean = TRUE)
# Extract names and municipalities for each pnro
pnro.raw <- levels(myDataSetName$Postinumeroalue)
# Remove 'KOKO MAA'
pnro.raw <- pnro.raw[-grep("KOKO MAA", pnro.raw)]
# Process a bit
pnro.raw <- gsub("  \\(", "|", pnro.raw)
pnro.raw <- gsub("\\)", "", pnro.raw)
pnro.number <- sapply(strsplit(pnro.raw, split=" "), "[", 1)
pnro.raw2 <- substr(pnro.raw, 7, 100)
pnro.name <- sapply(strsplit(pnro.raw2, split="\\|"), "[", 1)
pnro.municipality <- sapply(strsplit(pnro.raw2, split="\\|"), "[", 2)

# Put into list
pnro.info.list <- vector("list", length(pnro.number))
names(pnro.info.list) <- pnro.number
for (pi in seq(pnro.number))
  pnro.info.list[[pi]] <- c("name"=pnro.name[pi], "municipality"=pnro.municipality[pi])

# Write json
library("rjson")
pnro.info.json <- toJSON(pnro.info.list)
writeLines(pnro.info.json, con="json/pnro_info.json")
message("Then tidy the format (i.e. add ends of lines) using http://jsonlint.com/")


## Check some intersections
vakiluku <- read.table("data/vakiluku_posnro_2012_proper.csv", sep=";", header=TRUE, colClasses = c("Postinumero"="character"))
intersect1 = intersect(pnro.sp@data$pnro, vakiluku$Postinumero) # 3028
intersect2 = intersect(pnro.sp@data$pnro, pnro.number) # 3028
intersect3 = intersect(vakiluku$Postinumero, pnro.number) # 3036
intersect4 = intersect(intersect1, pnro.number) # 3028
all(intersect1 == intersect2) # TRUE

setdiff(pnro.sp@data$pnro, pnro.number)
# "02390" "05440" "16940" "42120" "57150" "60110" "87140" "87600" "90600" "98999" "99970"