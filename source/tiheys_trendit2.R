# Script for analysing the relation between population density and price trends
library("ggplot2")
library("sorvi")
library("mgcv")
library("MASS")

postal.code.table <- get_postal_code_info()
m2p <- municipality_to_province()

load("data/pnro_spatial_epsg2393.RData")

population <- read.csv("data/vakiluku_posnro_2012_proper.csv", sep=";", header=T, colClasses="character",row.names=1)

# Read hinta data
pnro.hinnat <- read.table("data/pnro-hinnat.txt", sep=" ", header=TRUE, colClasses="character")
pnro.hinnat$hintataso <- as.numeric(pnro.hinnat$hinta)
pnro.hinnat$trendi <- as.numeric(pnro.hinnat$trendi)

isot = c("Helsinki","Espoo","Tampere","Vantaa","Oulu","Turku","Jyväskylä","Kuopio","Lahti","Kouvola")
pk = c("Helsinki")
pks = c("Helsinki","Espoo","Vantaa","Kauniainen")
pksiso = c("Helsinki","Espoo","Vantaa","Kauniainen","Kirkkonummi","Siuntio","Vihti","Nurmijärvi","Kerava","Sipoo","Pornainen","Mäntsälä","Tuusula","Järvenpää")

selection <- pk
population$vakiluku = as.numeric(population$vakiluku)
population$temp = as.integer(as.integer(row.names(population)) / 100)
population$area = NaN
population$logtiheys = NaN
population$maakunta = ""
population$alue =  iconv(population$alue, from="ISO-8859-1", to="UTF-8")
m2p = iconv(m2p, from="ISO-8859-1", to="UTF-8")
rows <- row.names(population)
# add population density
for (index in seq(length(pnro.sp.alt@data$pnro))) {
  if (pnro.sp.alt@data$pnro[index] %in% rows){
    kunta = postal.code.table[postal.code.table$postal.code==pnro.sp.alt@data$pnro[index],]$municipality
    if (length(kunta) > 0){
      if (population[pnro.sp.alt@data$pnro[index],]$vakiluku > 0 & kunta %in% selection) {
        population[pnro.sp.alt@data$pnro[index],]$logtiheys = log(population[pnro.sp.alt@data$pnro[index],]$vakiluku / pnro.sp.alt@data$area.m2[index] * 10^6)
        temp = m2p[kunta]
        #names(temp) = NULL
        if (!is.na(temp[1])){
          if (kunta %in% selection){
            population[pnro.sp.alt@data$pnro[index],]$maakunta = kunta
            if (population[pnro.sp.alt@data$pnro[index],]$logtiheys > -12){
            population[pnro.sp.alt@data$pnro[index],]$area = toString(population[pnro.sp.alt@data$pnro[index],]$temp)
            } else  {
              #population[pnro.sp.alt@data$pnro[index],]$area = population[pnro.sp.alt@data$pnro[index],]$alue
              population[pnro.sp.alt@data$pnro[index],]$logtiheys = NaN
            }
          } else{
            #population[pnro.sp.alt@data$pnro[index],]$maakunta = iconv(temp[1], from="ISO-8859-1", to="UTF-8")
          }
      }}
    }}}

population$trendi = NaN

# add population trendi
for (index in seq(length(pnro.sp.alt@data$pnro))) {
  population[pnro.sp.alt@data$pnro[index],]$trendi = pnro.hinnat$trendi[index]
}

#png("trendi_tiheys.png", width=2000, height=3000)
#ggplot(population, aes(x=logtiheys,y=trendi)) + geom_point(shape=1,aes(color=maakunta)) + geom_smooth(method="gam", formula = y ~ s(x))
#ggsave("trendi_tiheys.png")
#dev.off()

#ggplot(population, aes(x=logtiheys,y=trendi)) + geom_point(shape=1,aes(color=maakunta)) + geom_smooth(method="gam", formula = y ~ s(x))
ggplot(population, aes(x=logtiheys,y=trendi)) + geom_point(shape=1,aes(color=maakunta)) + geom_smooth(method="rlm")
ggsave("figs/trendi_tiheys_Hki.png")
dev.off()

population_hki = population[!is.nan(population$logtiheys),]
population_hki[order(population_hki$logtiheys),]

