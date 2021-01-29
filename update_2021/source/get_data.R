# Script for retrieving data the apartment house price prediction
# (c) 2017 Juuso Parkkinen / Reaktor
# Update 2020 Henrik Aalto / Reaktor

# Necessary data
# - apartment house prices for years 2010-2020 by postal code areas from statfi
# - postal code area map data from Duukkis (http://www.palomaki.info/apps/pnro/)
# - postal code area population from statfi Paavo

## Packages needed:
# pxweb from ropengov: https://github.com/ropengov/pxweb
# install.packages("pxweb")
library("pxweb")

# gisfin from ropengov: https://github.com/ropengov/gisfin
# Need to install the development version 0.9.22 from GitHub 
 #library("devtools")
 #devtools::install_github("ropengov/gisfin")
#library("gisfin")
 
#remotes::install_github("ropengov/geofi")
 library("geofi")
 
# Other necessary packages
library("raster")
library("dplyr")
library("tidyr")
library("stringr")


## Get apartment house price data #######

# Data source: http://pxweb2.stat.fi/database/StatFin/databasetree_en.asp


# Use this interactive interface to browse the data
#d <- pxweb_interactive()

# This is the table we want:
# [statfin_ashi_pxt_112q.px] 
# Vanhojen osakeasuntojen keskihinnat ja kauppojen lukumäärät postinumeroalueittain ja rakennusvuosittain, talotyypit ja rakennusvuodet yhteensä
px_data <- 
  pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/ashi/vv/statfin_ashi_pxt_112q.px",
            query = "./update_2021/source/psno_statfi_query.json" )
px_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Pick only postal code, drop area name and preliminary data asterisk from year
pnro.ashi.raw <- px_data %>%
  mutate(Postinumero = str_sub(Postinumero, 1, 5)) %>%
  mutate(Vuosi = str_sub(Vuosi, 1, 4))

n_distinct(pnro.ashi.raw$Postinumero)
# [1] 1673 (2019: 1663, 2018: 1683, 2017: 1672, 2016: 1575)
stopifnot(all(nchar(as.character(pnro.ashi.raw$Postinumero))==5))


## Get postal code population data from Paavo #######

# Use the pxweb package as above to locate the data
# d <- pxweb_interactive()

# Now we want this table from 
# [2021/paavo_pxt_12f7.px]
px_data <- 
  pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/2021/paavo_pxt_12f7.px",
            query = "./update_2021/source/paavo_population_query.json")
pnro.population.raw <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

pnro.population = pnro.population.raw %>%
  filter(Postinumeroalue != "KOKO MAA") %>%
  mutate(temp = gsub(" (", "|", as.character(Postinumeroalue), fixed=TRUE),
         pnro = substr(temp, 1, 5),
         name = sapply(strsplit(substr(temp, 7, 100), split="\\|"), "[", 1),
         municipality = str_trim(gsub("\\)", "", sapply(strsplit(temp, split="\\|"), "[", 2))),
         area_km2 = `Postinumeroalueen pinta-ala`/1000000) %>%
  dplyr::select(-Postinumeroalue, -temp, -`Postinumeroalueen pinta-ala`, -starts_with('Asukkaat yhteensä, 2018')) %>%
  rename(population = starts_with('Asukkaat yhteensä, 2019'),
         bachelor_degrees = starts_with('Alemman kork'),
         master_degrees = starts_with('Ylemmän kork'),
         mean_income = starts_with('Asukkaiden keski'),
         median_income = starts_with('Asukkaiden mediaa'),
         owner_occupied_households = starts_with('Omistusasunnoissa'),
         total_households = starts_with('Taloudet yht'),
         unemployed = starts_with('Työttömät')) %>%
  mutate(density_per_km2 = round(population / area_km2, d=2),
         area_km2 = round(area_km2, d=2))

# Append population and area data to Duukkis postal area polygons
load(file="./update_2021/data/pnro_polygons_raw.RData")
pnro.sp = subset(pnro.sp.raw, pnro.sp.raw@data$pnro %in% pnro.population$pnro)
pnro.sp@data = pnro.sp@data %>%
  inner_join(pnro.population) %>%
  dplyr::select(pnro, name, municipality, population, area_km2)

# Process price data
pnro.ashi.dat = pnro.ashi.raw %>%
  mutate(Vuosi = as.numeric(as.character(Vuosi))) %>%
  rename(pnro = Postinumero,
         year = Vuosi,
         price = starts_with('Neliöhinta'),
         n = starts_with('Kauppojen')) %>%
  dplyr::select(pnro, year, price, n) %>%
  inner_join(pnro.population) %>%
  filter(pnro %in% pnro.sp@data$pnro)

all(pnro.ashi.dat$pnro %in% pnro.sp@data$pnro) # TRUE

save(pnro.population, pnro.sp, pnro.ashi.dat, file="./update_2021/data/pnro_data_20210128.RData")


## Write spatial data for web plots ######

load("./update_2021/data/pnro_data_20210128.RData")

# Write first as shapefile and then convert to topojson
require('rgdal')
rgdal::writeOGR(pnro.sp, "temp_pnro_shape_2021", "pnro-rgdal", driver="ESRI Shapefile")

# Use https://github.com/mbostock/topojson/ to convert
# run in terminal:
# shp2json temp_pnro_shape_2021/pnro-rgdal.shp > update_2021/json/pnro.geojson
# geo2topo update_2021/json/pnro.geojson > update_2021/json/pnro.topojson

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
  writeLines(con="./update_2021/json/pnro_info_nonpretty.json")

