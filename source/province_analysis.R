# Script for analysing the results by provinces

library("sorvi")

library("dplyr")

load("data/pnro_data_20150318.RData")
pnro.hinnat <- readRDS("data/pnro-hinnat.rds")

# predictions <- readRDS("predictions.rds")

# Get municipality to province -mapping
m2p <- municipality_to_province() 

# Add province info to data
pnro.dat <- pnro.dat %>%
  mutate(municipality.alternative = municipality,
         municipality.alternative = ifelse(municipality.alternative  == "Pedersören kunta", "Pedersöre", municipality.alternative ),
         municipality.alternative = ifelse(municipality.alternative  == "Koski Tl", "Koski.Tl", municipality.alternative ),
         province = iconv(m2p[municipality.alternative], from="UTF8", to ="ISO-8859-1"))

# Produce for each province
# table
# - top10 ja bottom10 alueet, hintatason

pnro.province <- pnro.hinnat %>%
  inner_join(pnro.dat) %>%
  mutate(hinta2016 = round(hinta2016),
         trendi2016 = round(100*trendi2016, d=1)) %>%
  select(pnro, name, municipality, province, hinta2016, trendi2016) %>%
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
  arrange(maakunta, desc(hinta2016)) %>%
  mutate(pnro = as.character(pnro)) %>%
  write.csv("data/asuntohinnat_2016_kokoSuomi_20150507.csv", sep="\t", quote=T, row.names=F, fileEncoding="ISO-8859-1")
#  write.csv("province_analysis/koko_suomi_20150414.csv")
  
## With updated data on 7.5.2015 #####

pnro.hinnat2 <- readRDS("data/pnro-hinnat2080.rds")

# pnro.hinnat.comb <- merge(pnro.hinnat, pnro.hinnat2)
pnro.province <- pnro.hinnat2 %>%
  inner_join(pnro.hinnat) %>%
  inner_join(pnro.dat) %>%
  mutate(hinta2016keski = round(hinta2016, d=-1),
         hinta2016min = round(hinta2016.20, d=-1),
         hinta2016max = round(hinta2016.80, d=-1),
         trendi2016keski = round(100*trendi2016, d=1),
         trendi2016min = round(100*trendi2016.20, d=1),
         trendi2016max = round(100*trendi2016.80, d=1)) %>%
  select(pnro, name, municipality, province, hinta2016keski, hinta2016min, hinta2016max, trendi2016keski, trendi2016min, trendi2016max) %>%
  rename(nimi = name,
         kunta = municipality,
         maakunta = province)

pnro.province %>%
  arrange(maakunta, desc(hinta2016min)) %>%
  mutate(pnro = as.character(pnro)) %>%
  write.csv("data/asuntohinnat_kvantiilit_2016_kokoSuomi_20150507.csv", quote=T, row.names=F, fileEncoding="ISO-8859-1")
