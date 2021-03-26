# Script for analysing the results by provinces

library("sorvi")

library("dplyr")

load("data_2017/pnro_data_20170405.RData")
pnro.hinnat <- readRDS("data_2017/SHORT_pnro-hinnat_2017.rds")

# predictions <- readRDS("predictions.rds")

## PRODUCE TOP/BOTTOM LISTS FOR MEDIA ########

# Get municipality to province -mapping
# m2p <- municipality_to_province() 
# FIX: https://github.com/avoindata/mml/blob/master/rdata/Yleiskartta-1000/HallintoAlue_DataFrame.RData
# Do 'municipality_to_province' and 'get_municipality_info_mml' manualyl:
load("data_2017/HallintoAlue_DataFrame.RData")
df <- df[, -grep("Enklaavi", colnames(df))]
df <- df[!duplicated(df), ]
rownames(df) <- convert_municipality_names(df$Kunta.FI)
df <- df[sort(rownames(df)), ]
m2p <- as.character(df$Maakunta.FI)
names(m2p) <- as.character(df$Kunta.FI)
# if (!is.null(municipalities)) {
#   m2p <- m2p[as.character(municipalities)]
# }
# m2p


# Add province info to data
pnro.dat <- pnro.dat %>%
  mutate(municipality.alternative = stringr::str_trim(municipality),
         municipality.alternative = ifelse(municipality.alternative  == "Pedersören kunta", "Pedersöre", municipality.alternative ),
         municipality.alternative = ifelse(municipality.alternative  == "Koski Tl", "Koski.Tl", municipality.alternative ),
         province = iconv(m2p[municipality.alternative], from="UTF8", to ="ISO-8859-1"))

## Use quantiles #####

# UPDATE 25.4.2017: Use 25-75 quantiles everywhere, instead of 20-80. 

# pnro.hinnat2 <- readRDS("data_2017/SHORT_pnro-hinnat_20-80_2017.rds")

# pnro.hinnat.comb <- merge(pnro.hinnat, pnro.hinnat2)
# pnro.province <- pnro.hinnat2 %>%
#   inner_join(pnro.hinnat) %>%
pnro.province <- pnro.hinnat %>%
  inner_join(pnro.dat) %>%
  mutate(hinta2018keski = round(hinta2018, d=-1),
         hinta2018min = round(hinta2018_q25, d=-1),
         hinta2018max = round(hinta2018_q75, d=-1),
         trendi2018keski = round(100*trendi2018, d=1),
         trendi2018min = round(100*trendi2018_q25, d=1),
         trendi2018max = round(100*trendi2018_q75, d=1)) %>%
  select(pnro, name, municipality, province, hinta2018keski, hinta2018min, hinta2018max, trendi2018keski, trendi2018min, trendi2018max) %>%
  rename(nimi = name,
         kunta = municipality,
         maakunta = province)

pnro.province %>%
  arrange(maakunta, desc(trendi2018keski)) %>%
  mutate(pnro = as.character(pnro)) %>%
  write.csv("data_2017/asuntohinnat_kvantiilit_2018_kokoSuomi_20170425.csv", quote=T, row.names=F, fileEncoding="ISO-8859-1")


# Write top 100 
pnro.province %>%
  mutate(pnro = as.character(pnro)) %>%
  arrange(desc(trendi2018min)) %>%
  head(100) %>%
  write.csv("data_2017/asuntohinnat_kvantiilit_2018_top100_20170425.csv", quote=T, row.names=F, fileEncoding="ISO-8859-1")

# Write top 100 and bottom 100
pnro.province %>%
  mutate(pnro = as.character(pnro)) %>%
  arrange((trendi2018max)) %>%
  head(100) %>%
  write.csv("data_2017/asuntohinnat_kvantiilit_2018_bottom100_20170425.csv", quote=T, row.names=F, fileEncoding="ISO-8859-1")


# Produce for each province
# table
# - top10 ja bottom10 alueet, hintatason

# pnro.province <- pnro.hinnat %>%
#   inner_join(pnro.dat) %>%
#   mutate(hinta2018 = round(hinta2018),
#          trendi2018 = round(100*trendi2018, d=1)) %>%
#   select(pnro, name, municipality, province, hinta2018, trendi2018) %>%
#   rename(nimi = name,
#          kunta = municipality,
#          maakunta = province)

# pnro.province %>%
#   filter(province == "Uusimaa") %>%
#   arrange(desc(hinta2016)) %>%
#   head(10) %>%
#   write.csv("province_analysis/Uusimaa_top10_temp.csv")

# pnro.province %>%
# #  filter(province == "Uusimaa") %>%
#   arrange(maakunta, desc(hinta2018)) %>%
#   mutate(pnro = as.character(pnro)) %>%
#   write.csv("data_2016/asuntohinnat_2017_kokoSuomi_20160302.csv", sep="\t", quote=T, row.names=F, fileEncoding="ISO-8859-1")
# #  write.csv("province_analysis/koko_suomi_20150414.csv")
  


# ## COMPARISON ANALYSIS ##########
# 
# # pnro_hinnat_2016 <- readRDS("data_2015/pnro-hinnat.rds") %>%
# #   inner_join(pnro.dat)
# # pnro_hinnat_2017 <- readRDS("data_2016/pnro-hinnat_2016.rds") %>%
# #   inner_join(pnro.dat)
# pnro_hinnat_combined <- readRDS("data_2015/pnro-hinnat.rds") %>%
#   inner_join(readRDS("data_2016/pnro-hinnat_2016.rds"),
#              by ="pnro") %>%
#   left_join(pnro.dat)
# 
# library("ggplot2")
# theme_set(theme_bw(24))
# # compare prices
# ggplot(pnro_hinnat_combined, aes(x=hinta2016, y=hinta2018)) + 
#   geom_point(size=2.5, alpha=0.5) +
#   geom_abline(slope=1, size=1.5) +
#   ggtitle("Hintataso 2016 vs 2017")
# ggsave("figs_2016/Hintataso_2016-vs-2017.png", width=10, height=10)
# 
# # compare trends
# ggplot(pnro_hinnat_combined, aes(x=100*trendi2016, y=100*trendi2018)) + 
#   geom_point(size=2.5, alpha=0.5) +
#   geom_abline(slope=1, size=1.5) +
#   geom_vline(xintercept=0, linetype="dashed") +
#   geom_hline(yintercept=0, linetype="dashed") +
#   ggtitle("Trendi 2016 vs 2017")
# ggsave("figs_2016/Trendi_2016-vs-2017.png", width=10, height=10)
# 
# ggplot(pnro_hinnat_combined, aes(x=trendimuutos.x, y=trendimuutos.y)) + geom_point(alpha=0.5) +
#   geom_abline(slope=1) +
#   geom_vline(xintercept=0, linetype="dashed") +
#   geom_hline(yintercept=0, linetype="dashed") +
#   labs(x="Trendimuutos 2016", y="Trendimuutos 2017") +
#   ggtitle("Trendimuutos 2016 vs 2017")
# ggsave("figs_2016/Trendimuutos_2016-vs-2017.png", width=10, height=10)
# 
# # Density
# ggplot(pnro_hinnat_combined, aes(x=density_per_km2, y=trendi2016)) +
#   geom_point() + 
#   scale_x_log10() +
#   geom_smooth(method="lm")
# 
# ggplot(pnro_hinnat_combined, aes(x=density_per_km2, y=trendi2018)) +
#   geom_point() + 
#   scale_x_log10() +
#   geom_smooth(method="lm")
# 
# 
# m2016 <- lm(trendi2016 ~ log10(density_per_km2),
#             data=pnro_hinnat_combined)
# m2017 <- lm(trendi2018 ~ log10(density_per_km2),
#             data=pnro_hinnat_combined)
# 
# # ## Urbanisaatio #######
# 
# trends <- readRDS("data_2016/yearly-trends_2016.rds") %>%
#   left_join(pnro.dat, by = "pnro") 
# 
# 
# #ggplot(transform(filter(trends,year<2015), year=factor(year)), aes(log(density_per_km2),trend.y.mean),) + 
# ggplot(trends, aes(density_per_km2,trend.y.mean)) + 
#   geom_point(size=1) + #aes(color=trend.y.mean>0) 
#   geom_smooth(method="lm") + 
#   facet_wrap(~year, ncol=3) + 
#   xlab('Tiheys (as / km^2)') + ylab("Trendi (% / vuosi)") + 
#   scale_x_log10()
# #   scale_x_continuous(breaks = log(norm.vals), labels=norm.vals) + scale_colour_discrete(name = "Hinta" ,labels= c("laskee","nousee")) 
# 
