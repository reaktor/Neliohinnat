# Script for getting house price data from statfi

# library("devtools")
# install_github("ropengov/pxweb")
install.packages("pxweb")
library("pxweb")

## PRICES BY POSTAL CODE #######

d <- interactive_pxweb(api = "statfi")
# [004_ashi_tau_109.px]    Vanhojen vapaarahoitteisten asuntojen hinnat postinumeroalueittain ja rakennusvuosittain

ashi.pnro.raw <- get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/ashi/004_ashi_tau_109.px",
                                dims = list(Vuosi = c('*'), Neljännes = c('0'), Talotyyppi = c('6'),
                                            Rakennusvuosi = c('8'), Postinumero = c('*'), Tiedot = c('*')),
                                clean = TRUE)
saveRDS(ashi.pnro.raw, file="data/statfi_ashi_pnro_raw_2005-2014_20150303.rds")
# saveRDS(ashi.pnro.raw, file="data/statfi_ashi_pnro_raw_2005-2014_20141219.rds")


## Filter data

library("dplyr")
library("tidyr")
ashi.pnro.raw <- readRDS("data/statfi_ashi_pnro_raw_2005-2014_20150303.rds")

# Reformat into year, pnro, price, n
ashi.pnro.dat <- ashi.pnro.raw %>%
  select(Vuosi, Postinumero, Tiedot, values) %>%
  spread(Tiedot, values) %>%
  rename(year = Vuosi,
         pnro = Postinumero,
         price = Keskiarvo,
         n = Lukumäärä)

saveRDS(ashi.pnro.dat, file="data/statfi_ashi_pnro_processed_2005-2014_20150303.rds")


## PRICES BY MUNICIPALITY (NOT NEEDED NOW) #######

# d <- interactive_pxweb(api = "statfi")
# 
# # We want these
# # 8. [008_ashi_tau_111_fi.px] Asunto-osakehuoneistojen keskimääräiset kauppahinnat(€/m²) ja kauppojen lukumäärät 2013
# # 9.  [008_ashi_tau_114_fi.px] Asunto-osakehuoneistojen keskimääräiset kauppahinnat(€/m²) ja kauppojen lukumäärät 2012
# # 10. [009_ashi_tau_115_fi.px] Asunto-osakehuoneistojen keskimääräiset kauppahinnat(€/m²) ja kauppojen lukumäärät 2011
# # 12. [010_ashi_tau_119_fi.px] Asunto-osakehuoneistojen keskimääräiset kauppahinnat(€/m²) ja kauppojen lukumäärät 2010
# # 13. [014_ashi_tau_116_fi.px] Asunto-osakehuoneistojen keskimääräiset kauppahinnat(€/m²) ja kauppojen lukumäärät 2009
# # 14. [014_ashi_tau_118_fi.px] Asunto-osakehuoneistojen keskimääräiset kauppahinnat(€/m²) ja kauppojen lukumäärät 2008
# tables <- c("008_ashi_tau_111_fi.px", "008_ashi_tau_114_fi.px", "009_ashi_tau_115_fi.px",
#             "010_ashi_tau_119_fi.px", "014_ashi_tau_116_fi.px", "014_ashi_tau_118_fi.px")
# names(tables) <- 2013:2008
# ashi.dat <- c()
# for (ti in seq(tables)) {
#   ti.dat <- get_pxweb_data(url = paste0("http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/ashi/", tables[ti]),
#                            dims = list(Alue = c('*'), Talotyyppi = c('*'), Tiedot = c('*')), clean = TRUE)
#   ashi.dat <- rbind(ashi.dat, cbind(ti.dat, vuosi=names(tables)[ti]))
# }
# save(ashi.dat, file="data/statfi_ashi_municipality_2008-2013.RData")
# 
# # Write small example data
# # write.csv(ashi.dat[1:1000,], file="data/statfi_ashi_municipality_example.csv")
# 
# # And omakotitalot
# # [kihi] does not have the same data as [ashi] :(



