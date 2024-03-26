# Script for analysing the results by provinces
library("sorvi")

library("dplyr")
library('stringr')

load("update_2024/data/pnro_data_20240127.RData")
pnro.hinnat <- readRDS("update_2024/data/pnro-hinnat_nominal_2023.rds")

## PRODUCE TOP/BOTTOM LISTS FOR MEDIA ########

# Get municipality to province -mapping
# FIX: https://github.com/avoindata/mml/blob/master/rdata/Yleiskartta-1000/HallintoAlue_DataFrame.RData
load("update_2024/data/HallintoAlue_DataFrame.RData")
df <- df[, -grep("Enklaavi", colnames(df))]
df <- df[!duplicated(df), ]
rownames(df) <- df$Kunta.FI
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
  dplyr::distinct(pnro, .keep_all = T) %>%
  mutate(hinta2023keski = round(price2023, d=-1),
         hinta2023min = round(price2023_q25, d=-1),
         hinta2023max = round(price2023_q75, d=-1),
         muutos2023keski = round(100*trend2023, d=1),
         muutos2023min = round(100*trend2023_q25, d=1),
         muutos2023max = round(100*trend2023_q75, d=1)) %>%
  select(pnro, name, population, municipality, province, hinta2023keski,
         hinta2023min, hinta2023max, muutos2023keski, muutos2023min, muutos2023max) %>%
  dplyr::rename(nimi = name,
         kunta = municipality,
         maakunta = province,
         asukasluku = population) %>%
  filter(asukasluku >= 1000) # filter population >= 1000 due to reliability

pnro.province %>%
  arrange(maakunta, desc(muutos2023keski)) %>%
  mutate(pnro = as.character(pnro)) %>%
  write.csv("update_2024/data/asuntohinnat_kvantiilit_2023_kokoSuomi.csv", quote=T, row.names=F, fileEncoding="utf8")

# Share of postal areas and population with price increases in 2023
pos_change = pnro.hinnat %>% 
  filter(trend2023_reliable == TRUE & trend2023 > 0) 
nrow(pos_change) # --> 43 / 3007 = 1.4 % pnro with increase in 2023
sum(pos_change$population) # 24727 / 5494699 ≈ 0.4%
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
