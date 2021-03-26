# Script for retrieving data the apartment house price prediction
# (c) 2017 Juuso Parkkinen / Reaktor

# Necessary data
# - apartment house prices for years 2005-2015 by postal code areas from statfi
# - postal code area map data from Duukkis
# - postal code area population from statfi Paavo

## Packages needed:
# pxweb from ropengov: https://github.com/ropengov/pxweb
# install.packages("pxweb")
library("pxweb")

# gisfin from ropengov: https://github.com/ropengov/gisfin
# Need to install the development version 0.9.22 from GitHub 
# library("devtools")
# devtools::install_github("ropengov/gisfin")
library("gisfin")

# Other necessary packages
library("raster")
library("dplyr")
library("tidyr")
library("stringr")


## Get apartment house price data #######

# Data source: http://pxweb2.stat.fi/database/StatFin/databasetree_en.asp


# Use this interactive interface to browse the data
# d <- interactive_pxweb(api = "statfi")

# This is the table we want:
# [040_ashi_tau_104.px]    Vanhojen vapaarahoitteisten asuntojen hinnat postinumeroalueittain ja rakennusvuosittain
pnro.ashi.raw <- pxweb::get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/ashi/040_ashi_tau_104.px",
                                       dims = list(Vuosi = c('*'), Neljännes = c('0'), Talotyyppi = c('6'),
                                                   Postinumero = c('*'), Rakennusvuosi = c('8'), Tiedot = c('*')),
                                       clean = TRUE) %>% tbl_df() %>%
  # Have to add leading zeros to Postinumero
  mutate(Postinumero = sprintf("%05s", as.character(Postinumero)))
n_distinct(pnro.ashi.raw$Postinumero)
# [1] 1672 (2016: 1575)
stopifnot(all(nchar(as.character(pnro.ashi.raw$Postinumero))==5))



## Get postal code map data from Duukkis ########

# The data is described here: http://www.palomaki.info/apps/pnro/

pnro.sp.duukkis <- gisfin::get_postalcode_areas()
n_distinct(pnro.sp.duukkis@data$pnro)
# [1] 3039

## Get postal code area data from Paavo #####

# This is used only to get accurate area values
# Note! This data also contains polygons, but they are very detailed and hence too large for web visualization purposes.
request <- gisfin::GeoStatFiWFSRequest$new()$getPostalCodeAreaLayers()
client <- gisfin::GeoStatFiWFSClient$new(request)
client$listLayers()
pno.layer <- "postialue:pno"
request$getPostalCodeArea(pno.layer)
client <- gisfin::GeoStatFiWFSClient$new(request)
pnro.sp.paavo <- client$getLayer(pno.layer)
n_distinct(pnro.sp.paavo@data$posti_alue)
# [1] 3036

# PROBLEM 5.4.2017: The above code does not work for some reason!
# => Use data from last year, update only price data

## Get postal code population data from Paavo #######

# Use the pxweb package as above to locate the data
# d <- interactive_pxweb(api = "statfi")

# Now we want this table from 
# [2017/paavo_9_koko_2017.px]
pnro.population.raw <- pxweb::get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/2017/paavo_9_koko_2017.px",
                                             dims = list(Postinumeroalue = c('*'),
                                                         Tiedot = c('He_vakiy')),
                                             clean = TRUE) %>% tbl_df()
n_distinct(pnro.population.raw$Postinumeroalue)
# [1] 3037

## Process data sets #########

# 5.4.2017: SKIP THE FOLLOWING, USE DATA FROM LAST YEAR INSTEAD
load("~/Documents/workspace/reaktor/Neliohinnat/data_2016/pnro_data_20160303.RData")
# Use pnro.dat and pnro.sp as they were, update only pnro.ashi.dat
rm(pnro.ashi.dat)

# # Process population data (extract pnro, municipality and name from Postinumeroalue)
# pnro.population <- pnro.population.raw %>%
#   filter(Postinumeroalue != "KOKO MAA") %>%
#   mutate(temp = gsub("  \\(", "|", as.character(Postinumeroalue)),
#          pnro = substr(temp, 1, 5),
#          name = sapply(strsplit(substr(temp, 7, 100), split="\\|"), "[", 1),
#          municipality = str_trim(gsub("\\)", "", sapply(strsplit(temp, split="\\|"), "[", 2)))) %>%
#   rename(population = values) %>%
#   select(pnro, name, municipality, population)
# 
# # Combine all necessary data together (results in 3028 rows)
# pnro.dat <- pnro.population %>%
#   inner_join(pnro.sp.duukkis@data) %>% # include only those pnro in the polygon data
#   select(-name) %>% # do NOT use name from Paavo PX-Web data
#   inner_join(pnro.sp.paavo@data %>%
#                rename(pnro = posti_alue,
#                       name = nimi) %>% # use name from Paavo map data
#                mutate(area_km2 = pinta_ala / 1e6)) %>%
#   select(pnro, name, municipality, population, area_km2) %>%  
#   mutate(density_per_km2 = round(population / area_km2, d=2),
#          area_km2 = round(area_km2, d=2))
# n_distinct(pnro.dat$pnro)
# # [1] 3028
# 
# # Create a new spatial df with necessary data
# pnro.sp <- subset(pnro.sp.duukkis, pnro.sp.duukkis@data$pnro %in% pnro.dat$pnro)
# pnro.sp@data <- pnro.sp@data %>%
#   inner_join(pnro.dat) %>%
#   select(pnro, name, municipality, population, area_km2)

# Process dwelling price data, include density
pnro.ashi.dat <- pnro.ashi.raw %>%
  mutate(Postinumero = as.character(Postinumero),
         Vuosi = as.numeric(as.character(Vuosi))) %>%
  select(Postinumero, Vuosi, Tiedot, values) %>%
  spread(Tiedot, values) %>%
  rename(pnro = Postinumero,
         year = Vuosi,
         price = Keskiarvo,
         n = Lukumäärä) %>%
  inner_join(pnro.dat %>%
               select(pnro, density_per_km2))
n_distinct(pnro.ashi.dat$pnro)
# [1] 1671 (2016: 1571)

n_distinct(pnro.dat$pnro) # 3028
n_distinct(pnro.sp@data$pnro) # 3028
all(pnro.ashi.dat$pnro %in% pnro.dat$pnro) # TRUE

# Save all data
save(pnro.dat, pnro.sp, pnro.ashi.dat, file="data_2017/pnro_data_20170405.RData")


## Write spatial data for web plots ######


load("data_2017/pnro_data_20170405.RData")

# Write spatial polygons as geojson or topojson

# 2017: write first as shapefile and then convert to topojson
rgdal::writeOGR(pnro.sp, "temp_pnro_shape_2017", "pnro-rgdal", driver="ESRI Shapefile")
# Use https://github.com/mbostock/topojson/ to convert
system("topojson -p pnro -o json_2017/pnro.topojson temp_pnro_shape_2017/pnro-rgdal.shp")

# the geojson/topojson does not include the data, so write it separately as json
# Put into list
pnro.info.list <- vector("list", length(pnro.sp@data$pnro))
names(pnro.info.list) <- pnro.sp@data$pnro
for (pi in seq(pnro.info.list))
  pnro.info.list[[pi]] <- pnro.sp@data[pi,]

# Write json
library("jsonlite")

# Write in non-pretty format
pnro.info.list %>%
  toJSON(pretty=FALSE) %>%
  writeLines(con="json_2017/pnro_info_nonpretty.json")