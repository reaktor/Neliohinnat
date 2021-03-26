# Script for analysing the results by provinces

library("dplyr")
library('stringr')

load("update_2021/data/pnro_data_20210304.RData")
pnro.hinnat <- readRDS("update_2021/data/pnro-hinnat_nominal_2021.rds")

# predictions <- readRDS("predictions.rds")

## PRODUCE TOP/BOTTOM LISTS FOR MEDIA ########

# Get municipality to province -mapping
# FIX: https://github.com/avoindata/mml/blob/master/rdata/Yleiskartta-1000/HallintoAlue_DataFrame.RData
load("update_2021/data/HallintoAlue_DataFrame.RData")
df <- df[, -grep("Enklaavi", colnames(df))]
df <- df[!duplicated(df), ]
rownames(df) <- convert_municipality_names(df$Kunta.FI)
df <- df[sort(rownames(df)), ]
m2p <- as.character(df$Maakunta.FI)
names(m2p) <- as.character(df$Kunta.FI)

# Add province info to data
provinces <- pnro.population %>%
  mutate(municipality.alternative = str_replace_all(municipality, c('Pedersören kunta' = 'Pedersöre',
                                                                                    'Koski Tl' = 'Koski.Tl',
                                                                                    'Maarianhamina - Mariehamn' = 'Maarianhamina'))) %>%
  mutate(municipality.alternative = stringr::str_trim(municipality.alternative),
         province = iconv(m2p[municipality.alternative], from="UTF8", to ="ISO-8859-1")) %>%
  select(pnro, name, municipality, province, population)

# Produce results
pnro.province <- pnro.hinnat %>%
  inner_join(provinces) %>%
  mutate(hinta2020keski = round(price2020, d=-1),
         hinta2020min = round(price2020_q25, d=-1),
         hinta2020max = round(price2020_q75, d=-1),
         muutos2020keski = round(100*trend2020, d=1),
         muutos2020min = round(100*trend2020_q25, d=1),
         muutos2020max = round(100*trend2020_q75, d=1)) %>%
  select(pnro, name, population, municipality, province, hinta2020keski,
         hinta2020min, hinta2020max, muutos2020keski, muutos2020min, muutos2020max) %>%
  dplyr::rename(nimi = name,
         kunta = municipality,
         maakunta = province,
         asukasluku = population)

pnro.province %>%
  arrange(maakunta, desc(muutos2020keski)) %>%
  mutate(pnro = as.character(pnro)) %>%
  write.csv("update_2021/data/asuntohinnat_kvantiilit_2020_kokoSuomi.csv", quote=T, row.names=F, fileEncoding="utf8")

# Share of postal areas and population with price increases in 2020
pos_change = pnro.hinnat %>% 
  filter(trend2020_reliable == TRUE & trend2020 > 0) 
nrow(pos_change) # --> 656 / 3008 = 21.8 % pnro with increase in 2020
sum(pos_change$population) # 2246885 / 5455832 ≈ 41.1%
sum(pnro.hinnat$population)

# Write top 100 
pnro.province %>%
  mutate(pnro = as.character(pnro)) %>%
  arrange(desc(muutos2020min)) %>%
  head(100) %>%
  write.csv("update_2021/data/asuntohinnat_kvantiilit_2020_top100.csv", quote=T, row.names=F, fileEncoding="utf8")

# Write top 100 and bottom 100
pnro.province %>%
  mutate(pnro = as.character(pnro)) %>%
  arrange((muutos2020max)) %>%
  head(100) %>%
  write.csv("update_2021/data/asuntohinnat_kvantiilit_2020_bottom100.csv", quote=T, row.names=F, fileEncoding="utf8")
