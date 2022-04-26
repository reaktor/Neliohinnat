# Script for retrieving data the apartment house price prediction
# (c) 2017 Juuso Parkkinen / Reaktor
# Update 2021 Henrik Aalto / Reaktor
# Update (hack) 2022 Janne Sinkkonen / Reaktor

# Necessary data
# - apartment house prices for years 2010-2020 by postal code areas from statfi
# - postal code area map data from statfi Paavo
# - postal code area population from statfi Paavo

## Packages needed (but more packages later in the code)
# pxweb from ropengov: https://github.com/ropengov/pxweb
# install.packages("pxweb")
library("pxweb")

#remotes::install_github("ropengov/geofi")
 library("geofi")
 
# Other necessary packages
library("raster")
library("dplyr")
library("tidyr")
library("stringr")


## Get apartment house price data #######

# Data source: http://pxweb2.stat.fi/database/StatFin/databasetree_en.asp

# The problem with saved queries seems to be that they break between years when the zip codes for 
# example change, as they do. 
# Use this interactive interface to browse the data
#  d <- pxweb_interactive()
# Vanhojen osakeasuntojen keskihinnat ja kauppojen lukumäärät postinumeroalueittain ja rakennusvuosittain, 
#   112q at some point
#   talotyypit ja rakennusvuodet yhteensä
# Then
#  saveRDS(d, "source/queries/pnro-statfi-query-result-raw.rds")
if (F) {
  px_data <- 
    pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/ashi/vv/statfin_ashi_pxt_112q.px",
              query = "source/queries/psno_statfi_query.json" )
  px_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") }

# The query is in d_pxq_obj$query, as an R list structure
d_pxq_obj <- readRDS("source/queries/pnro-statfi-query-result-raw.rds")

# Pick only postal code, drop area name and preliminary data asterisk from year
pnro.ashi.raw <- d_pxq_obj$data %>%
  mutate(Postinumero = str_sub(Postinumero, 1, 5)) %>%
  mutate(Vuosi = str_sub(Vuosi, 1, 4))

n_distinct(pnro.ashi.raw$Postinumero)
# [1] 1687 (2020: 1673, 2019: 1663, 2018: 1683, 2017: 1672, 2016: 1575)
stopifnot(all(nchar(as.character(pnro.ashi.raw$Postinumero))==5))


## Get postal code population data from Paavo #######
# Again, in principle 
#   d <- pxweb_interactive()
# but Paavo I couldn't find. 
# The old query works, except for zip codes that need to be updated (maybe). 
# (One could also load the whole table from the web interface, but this 
#  selects the right variables and is maybe a little bit safer, format-wise.)

zip_raw = get_zipcodes(year=2022)
pq <- pxweb_query("source/queries/paavo_population_query.json")
pq["query"][[1]][[1]]$selection$values <- pnros

# Now we want this table from 
# [2021/paavo_pxt_12f7.px]
url = "https://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/2021/paavo_pxt_12f7.px"
pxm = pxweb_get(url)
pxm$variables[[1]]$elimination <- TRUE
px_data <- pxweb_advanced_get(url, query = "source/queries/paavo_population_query.json", pxmdo = pxm)
pnro.population.raw <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

pnro.population = pnro.population.raw %>%
  filter(Postinumeroalue != "KOKO MAA") %>%
  mutate(temp = gsub(" (", "|", as.character(Postinumeroalue), fixed=TRUE),
         pnro = substr(temp, 1, 5),
         name = sapply(strsplit(substr(temp, 7, 100), split="\\|"), "[", 1),
         municipality = str_trim(gsub("\\)", "", sapply(strsplit(temp, split="\\|"), "[", 2))),
         area_km2 = `Postinumeroalueen pinta-ala`/1000000) %>%
  dplyr::select(-Postinumeroalue, -temp, -`Postinumeroalueen pinta-ala`,
                -starts_with('Asukkaat yhteensä, 2018')) %>%
  dplyr::rename(population = starts_with('Asukkaat yhteensä'),
         bachelor_degrees = starts_with('Alemman kork'),
         master_degrees = starts_with('Ylemmän kork'),
         vocational_grads = starts_with('Ammatillisen tut'),
         highschool_grads = starts_with('Ylioppilastutkinnon'),
         educated = starts_with('Koulutetut yht'),
         mean_income = starts_with('Asukkaiden keskitul'),
         median_income = starts_with('Asukkaiden mediaa'),
         households = starts_with('Taloudet yhteensä, 2019 (TE)'),
         unemployed = starts_with('Työttömät'),
         employed = starts_with('Työlliset'),
         men = starts_with('Miehet'),
         low_income = starts_with('Alimpaan tuloluokkaan kuuluvat asukkaat'),
         mid_income = starts_with('Keskimmäiseen tuloluokkaan kuuluvat as'),
         hi_income = starts_with('Ylimpään tuloluokkaan kuuluvat asu'),
         refinement_jobs = starts_with('Jalostuksen'),
         primary_prod_jobs = starts_with('Alkutuotannon'),
         service_jobs = starts_with('Palveluiden'),
         jobs = starts_with('Työpaikat'),
         cottages = starts_with('Kesämökit'),
         properties = starts_with('Rakennukset'),
         living_properties = starts_with('Asuinrakennukset'),
         other_properties = starts_with('Muut rakennukset'),
         apartments = starts_with('Asunnot'),
         small_houses = starts_with('Pientalo'),
         living_space = starts_with('Asumisv')
         ) %>%
  mutate(density_per_km2 = round(population / area_km2, d=2),
         area_km2 = round(area_km2, d=2))


# Fetch postal areas and convert to polygons
library('rmapshaper')
library('cleangeo')


Q = rmapshaper::ms_filter_islands(dplyr::select(zip_raw, pnro=posti_alue, geom), min_area = 90000)
Q2 = rmapshaper::ms_simplify(Q, keep = 0.15)
P = as(Q2, 'Spatial')
P = cleangeo::clgeo_Clean(P)

P_longlat = spTransform(P, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

# Append population and area data to postal area polygons
pnro.sp = subset(P_longlat, P@data$pnro %in% pnro.population$pnro)
pnro.sp@data = pnro.sp@data %>%
  inner_join(pnro.population) %>%
  dplyr::select(pnro, name, municipality, population, area_km2)

# Process price data
pnro.ashi.dat = pnro.ashi.raw %>%
  mutate(Vuosi = as.numeric(as.character(Vuosi))) %>%
  dplyr::rename(pnro = Postinumero,
         year = Vuosi,
         price = starts_with('Neliöhinta'),
         n = starts_with('Kauppojen')) %>%
  dplyr::select(pnro, year, price, n) %>%
  inner_join(pnro.population) %>%
  filter(pnro %in% pnro.sp@data$pnro)

all(pnro.ashi.dat$pnro %in% pnro.sp@data$pnro) # TRUE

save(pnro.population, pnro.sp, pnro.ashi.dat, file="./data/pnro_data_20220426.RData")


## Write spatial data for web plots ######

load("./data/pnro_data_20220426.RData")

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
  writeLines(con="./json/pnro_info_nonpretty.json")

