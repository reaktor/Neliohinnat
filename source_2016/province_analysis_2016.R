# Script for analysing the results by provinces

library("sorvi")

library("dplyr")

load("data_2016/pnro_data_20160215.RData")
pnro.hinnat <- readRDS("data_2016/pnro-hinnat_2016.rds")

# predictions <- readRDS("predictions.rds")

# Get municipality to province -mapping
m2p <- municipality_to_province() 

# Add province info to data
pnro.dat <- pnro.dat %>%
  mutate(municipality.alternative = stringr::str_trim(municipality),
         municipality.alternative = ifelse(municipality.alternative  == "Pedersören kunta", "Pedersöre", municipality.alternative ),
         municipality.alternative = ifelse(municipality.alternative  == "Koski Tl", "Koski.Tl", municipality.alternative ),
         province = iconv(m2p[municipality.alternative], from="UTF8", to ="ISO-8859-1"))

# Produce for each province
# table
# - top10 ja bottom10 alueet, hintatason

pnro.province <- pnro.hinnat %>%
  inner_join(pnro.dat) %>%
  mutate(hinta2017 = round(hinta2017),
         trendi2017 = round(100*trendi2017, d=1)) %>%
  select(pnro, name, municipality, province, hinta2017, trendi2017) %>%
  rename(nimi = name,
         kunta = municipality,
         maakunta = province)

# pnro.province %>%
#   filter(province == "Uusimaa") %>%
#   arrange(desc(hinta2016)) %>%
#   head(10) %>%
#   write.csv("province_analysis/Uusimaa_top10_temp.csv")

pnro.province %>%
#  filter(province == "Uusimaa") %>%
  arrange(maakunta, desc(hinta2017)) %>%
  mutate(pnro = as.character(pnro)) %>%
  write.csv("data_2016/asuntohinnat_2017_kokoSuomi_20160302.csv", sep="\t", quote=T, row.names=F, fileEncoding="ISO-8859-1")
#  write.csv("province_analysis/koko_suomi_20150414.csv")
  
## With updated data on 7.5.2015 #####

pnro.hinnat2 <- readRDS("data_2016/pnro-hinnat_20-80_2016.rds")

# pnro.hinnat.comb <- merge(pnro.hinnat, pnro.hinnat2)
pnro.province <- pnro.hinnat2 %>%
  inner_join(pnro.hinnat) %>%
  inner_join(pnro.dat) %>%
  mutate(hinta2017keski = round(hinta2017, d=-1),
         hinta2017min = round(hinta2017.20, d=-1),
         hinta2017max = round(hinta2017.80, d=-1),
         trendi2017keski = round(100*trendi2017, d=1),
         trendi2017min = round(100*trendi2017.20, d=1),
         trendi2017max = round(100*trendi2017.80, d=1)) %>%
  select(pnro, name, municipality, province, hinta2017keski, hinta2017min, hinta2017max, trendi2017keski, trendi2017min, trendi2017max) %>%
  rename(nimi = name,
         kunta = municipality,
         maakunta = province)

pnro.province %>%
  arrange(maakunta, desc(hinta2017min)) %>%
  mutate(pnro = as.character(pnro)) %>%
  write.csv("data_2016/asuntohinnat_kvantiilit_2017_kokoSuomi_20160302.csv", quote=T, row.names=F, fileEncoding="ISO-8859-1")

# Write top 100 and bottom 100
pnro.province %>%
  mutate(pnro = as.character(pnro)) %>%
  arrange(desc(trendi2017keski)) %>%
  head(100) %>%
  write.csv("data_2016/asuntohinnat_kvantiilit_2017_top100_20160302.csv", quote=T, row.names=F, fileEncoding="ISO-8859-1")

# Write top 100 and bottom 100
pnro.province %>%
  mutate(pnro = as.character(pnro)) %>%
  arrange((trendi2017keski)) %>%
  head(100) %>%
  write.csv("data_2016/asuntohinnat_kvantiilit_2017_bottom100_20160302.csv", quote=T, row.names=F, fileEncoding="ISO-8859-1")

