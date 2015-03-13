# Get Paavo data

# Get Paavo polygons with gisfin
library("devtools")
install_github("ropengov/gisfin")
library("gisfin")
request <- gisfin::GeoStatFiWFSRequest$new()$getPostalCodeAreaLayers()
client <- gisfin::GeoStatFiWFSClient$new(request)
layers <- client$listLayers()
layers
library(raster)
request$getPostalCodeArea(layers[1])
client <- gisfin::GeoStatFiWFSClient$new(request)
pno <- client$getLayer(layers[1])
spplot(pno, zcol="pinta_ala")


