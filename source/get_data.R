# Script for retrieving data the apartment house price prediction
# (c) 2015 Juuso Parkkinen

# Necessary data
# - apartment house prices by postal code areas from statfi
# - postal code area map data from statfi Paavo
# - postal code area population from statfi Paavo

## Packages needed:
# pxweb from ropengov: https://github.com/ropengov/pxweb
# install.packages("pxweb")
library("pxweb")

# gisfin from ropengov: https://github.com/ropengov/gisfin
# Need to install the development version from GitHub:
# library("devtools")
# install_github("ropengov/gisfin")
library("gisfin")

# Other necessary packages
library("raster")
library("dplyr")
library("tidyr")

## Get apartment house price data #######

# Data source: http://pxweb2.stat.fi/database/StatFin/databasetree_en.asp


# Use this interactive interface to browse the data
# d <- interactive_pxweb(api = "statfi")

# This is the table we want:
# [004_ashi_tau_109.px]    Vanhojen vapaarahoitteisten asuntojen hinnat postinumeroalueittain ja rakennusvuosittain
pno.ashi.raw <- pxweb::get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/ashi/004_ashi_tau_109.px",
                                      dims = list(Vuosi = c('*'), Neljännes = c('0'), Talotyyppi = c('6'),
                                                  Rakennusvuosi = c('8'), Postinumero = c('*'), Tiedot = c('*')),
                                      clean = TRUE)
pno.ashi.raw <- tbl_df(pno.ashi.raw)


## Get postal code map data from Paavo ########

# The data is described here: http://www.tilastokeskus.fi/tup/rajapintapalvelut/paavo_en.html

# Browse the available data sets
request <- gisfin::GeoStatFiWFSRequest$new()$getPostalCodeAreaLayers()
client <- gisfin::GeoStatFiWFSClient$new(request)
client$listLayers()

# Retrieve the Latest Paavo Map Data
pno.layer <- "postialue:pno"
request$getPostalCodeArea(pno.layer)
client <- gisfin::GeoStatFiWFSClient$new(request)
pno.sp <- client$getLayer(pno.layer)


## Get postal code population data from Paavo #######

# Use the pxweb package as above to locate the data
# d <- interactive_pxweb(api = "statfi")

# Now we want this table from 
# [paavo_1_he_2015.px]
pno.population.raw <- get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/2015/paavo_1_he_2015.px",
                                 dims = list(Postinumeroalue = c('*'),
                                             Tiedot = c('He_vakiy')),
                                 clean = TRUE)
pno.population.raw <- tbl_df(pno.population.raw)


## Process data #########

# Process population data
pno.population <- pno.population.raw %>%
  filter(Postinumeroalue != "KOKO MAA") %>%
  mutate(Postinumeroalue = as.character(Postinumeroalue),
         pno = substr(Postinumeroalue, 1, 5),
         municipality = gsub("\\)", "", sapply(strsplit(Postinumeroalue, split="\\("), "[", 2))) %>%
  rename(population = values) %>%
  select(pno, municipality, population)

# Process pno.sp data and compute densities
pno.sp@data <- pno.sp@data %>%
  rename(pno = posti_alue,
         name = nimi,
         area = pinta_ala) %>%
  inner_join(pno.population) %>%
  select(pno, name, municipality, area, population) %>%
  mutate(density = population / area)

# Process dwelling price data, include density
pno.ashi.dat <- pno.ashi.raw %>%
  mutate(Postinumero = as.character(Postinumero),
         Vuosi = as.numeric(as.character(Vuosi))) %>%
  select(Postinumero, Vuosi, Tiedot, values) %>%
  spread(Tiedot, values) %>%
  rename(pno = Postinumero,
         year = Vuosi,
         price = Keskiarvo,
         n = Lukumäärä) %>%
  inner_join(pno.sp@data %>%
               select(pno, density))

# Save data
save(pno.sp, pno.ashi.dat, file="data/pno_data_20150316.RData")


## Write json files ######


# # Write only polygons as GeoJSON (can not specify file type for some reason, rename afterwards)
# rgdal::writeOGR(obj=pnro.sp, dsn="pnro_areas_geojson", layer="pnro", driver="GeoJSON")
# file.rename("pnro_areas_geojson", "json/pnro_areas.geojson")

# library("rjson")

