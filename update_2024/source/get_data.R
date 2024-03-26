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
# remotes::install_github("ropengov/pxweb")
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
  d <- pxweb_interactive()
# Vanhojen osakeasuntojen neliöhinnat ja kauppojen lukumäärät postinumeroalueittain, vuosittain, 
#   13mu at some point
#   talotyypit ja rakennusvuodet yhteensä
# Then
saveRDS(d, "pnro-statfi-query-result-raw.rds")

# The query is in d_pxq_obj$query, as an R list structure
d_pxq_obj <- readRDS("source/queries/pnro-statfi-query-result-raw.rds")

# Pick only postal code, drop area name and preliminary data asterisk from year
pnro.ashi.raw <- d_pxq_obj$data %>%
  mutate(Postinumero = str_sub(Postinumero, 1, 5)) %>%
  mutate(Vuosi = str_sub(Vuosi, 1, 4)) %>%
  filter(Vuosi >= 2013)

# Calculate weighted means as average over all apartment types is no longer available
pnro.ashi.raw.v2 <- pnro.ashi.raw %>%
  rename(N = `Kauppojen lukumäärä, varainsiirtoverotiedot vuodesta 2020 alkaen`) %>%
  filter(!is.na(`Neliöhinta (EUR/m2)`) & !is.na(N)) %>%  # Remove rows with NA values in price
  group_by(Vuosi, Postinumero) %>%
  summarise(`Neliöhinta` = sum(`Neliöhinta (EUR/m2)` * N, na.rm = TRUE) / sum(N, na.rm = TRUE),
            `Kauppojen_määrä` = sum(N, na.rm=TRUE))

n_distinct(pnro.ashi.raw.v2$Postinumero)
# [1] 1687 (2020: 1673, 2019: 1663, 2018: 1683, 2017: 1672, 2016: 1575)
stopifnot(all(nchar(as.character(pnro.ashi.raw.v2$Postinumero))==5))


## Get postal code population data from Paavo #######
# Again, in principle 
#   d <- pxweb_interactive()

# Now we want this table from 
# [2022/paavo_pxt_12f7.px]
px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/uusin/paavo_pxt_12f7.px",
            query = "source/queries/paavo_population_query.json")
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
  dplyr::rename(population = starts_with('Asukkaat yhteensä (HE)'),
         bachelor_degrees = starts_with('Alimman kork'),
         master_degrees = starts_with('Ylemmän kork'),
         vocational_grads = starts_with('Ammatillisen tut'),
         highschool_grads = starts_with('Ylioppilastutkinnon'),
         educated = starts_with('Koulutetut yht'),
         mean_income = starts_with('Asukkaiden keskitul'),
         median_income = starts_with('Asukkaiden mediaa'),
         households = starts_with('Taloudet yhteensä'),
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
         living_space = starts_with('Asumisv'),
         year = Vuosi
         ) %>%
  mutate(density_per_km2 = round(population / area_km2, d=2),
         area_km2 = round(area_km2, d=2),
         year = as.numeric(as.character(year)))
# Add 2022 as 2023 to population data as it is not yet available
# (Assumes that demographics have stayed as is)
pnro.population = pnro.population %>% filter(year == 2022) %>%
  mutate(year = 2023) %>%
  rbind(pnro.population)

# Fetch postal areas and convert to polygons
library('rmapshaper')
library('sf')

zip_raw = get_zipcodes(year=2023)
Q = rmapshaper::ms_filter_islands(dplyr::select(zip_raw, pnro=posti_alue, geom), min_area = 90000)
Q2 = rmapshaper::ms_simplify(Q, keep = 0.15)
pnro.sf = st_as_sf(Q2)
pnro.sf = st_transform(pnro.sf, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

# Append population and area data to postal area polygons
pnro.sf = subset(pnro.sf, pnro.sf$pnro %in% pnro.population$pnro)
pnro.sf = pnro.sf %>%
  inner_join(pnro.population, by=c('pnro'), multiple="last") %>%
  dplyr::select(pnro, name, municipality, population, area_km2)

# Process price data
pnro.ashi.dat = pnro.ashi.raw.v2 %>%
  mutate(Vuosi = as.numeric(as.character(Vuosi))) %>%
  dplyr::rename(pnro = Postinumero,
         year = Vuosi,
         price = starts_with('Neliöhinta'),
         n = starts_with('Kauppojen')) %>%
  dplyr::select(pnro, year, price, n) %>%
  inner_join(pnro.population, by=c('pnro', 'year')) %>%
  filter(pnro %in% pnro.sf$pnro)

all(pnro.ashi.dat$pnro %in% pnro.sf$pnro) # TRUE

save(pnro.population, pnro.sf, pnro.ashi.dat, file="./data/pnro_data_20240127.RData")


## Write spatial data for web plots ######

load("./update_2024/data/pnro_data_20240127.RData")

# Write first as geojson and then convert to topojson
require('sf')
sf::st_write(pnro.sf, "./json/pnro.geojson")

# Use https://github.com/mbostock/topojson/ to convert
# run in terminal:
# geo2topo update_2021/json/pnro.geojson > update_2021/json/pnro.topojson

# the geojson/topojson does not include the data, so write it separately as json
# Put into list
pnro.sf.data = pnro.sf %>% st_drop_geometry()
pnro.info.list <- vector("list", length(pnro.sf$pnro))
names(pnro.info.list) <- pnro.sf$pnro
for (pi in seq(pnro.info.list))
  pnro.info.list[[pi]] <- pnro.sf.data[pi,]

# Write json
library("jsonlite")

# Write in non-pretty format
pnro.info.list %>%
  toJSON(pretty=FALSE) %>%
  writeLines(con="./json/pnro_info_nonpretty.json")

