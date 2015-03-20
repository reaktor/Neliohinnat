# script for making a data frame with combined information
library("sorvi")

postal.code.table <- get_postal_code_info()
m2p <- municipality_to_province()

load("data/pnro_spatial_epsg2393.RData")

population <- read.csv("data/vakiluku_posnro_2012_proper.csv", sep=";", header=T, colClasses="character",row.names=1)

# Read hinta data
pnro.hinnat <- read.table("data/pnro-hinnat.txt", sep=" ", header=TRUE, colClasses="character")
pnro.hinnat$hintataso <- as.numeric(pnro.hinnat$hinta)
pnro.hinnat$trendi <- as.numeric(pnro.hinnat$trendi)

m2p = iconv(m2p, from="ISO-8859-1", to="UTF-8")

population$vakiluku = as.numeric(population$vakiluku)
population$temp = as.integer(as.integer(row.names(population)) / 100)
population$area = NaN
population$logtiheys = NaN
population$kunta = ""
population$alue =  iconv(population$alue, from="ISO-8859-1", to="UTF-8")
population$hintataso = NaN
population$trendi = NaN
population$trendimuutos = NaN
population$pinta_ala = NaN
rows <- row.names(population)
# add population density
for (index in seq(length(pnro.sp.alt@data$pnro))) {
  if (pnro.sp.alt@data$pnro[index] %in% rows){
    kunta = postal.code.table[postal.code.table$postal.code==pnro.sp.alt@data$pnro[index],]$municipality
    if (length(kunta) > 0){
      if (population[pnro.sp.alt@data$pnro[index],]$vakiluku > 0) {
        population[pnro.sp.alt@data$pnro[index],]$pinta_ala = pnro.sp.alt@data$area.m2[index] / 10^6
        population[pnro.sp.alt@data$pnro[index],]$logtiheys = log10(population[pnro.sp.alt@data$pnro[index],]$vakiluku / pnro.sp.alt@data$area.m2[index] * 10^6)
        temp = m2p[kunta]
        #names(temp) = NULL
        if (!is.na(temp[1])){
          population[pnro.sp.alt@data$pnro[index],]$kunta = kunta[1]
          population[pnro.sp.alt@data$pnro[index],]$area = substr(pnro.sp.alt@data$pnro[index],3,3)
        }
      }
    }
  }
}

# add estimates
for (index in seq(length(pnro.sp.alt@data$pnro))) {
  population[pnro.sp.alt@data$pnro[index],]$hintataso = pnro.hinnat$hintataso[index]
  population[pnro.sp.alt@data$pnro[index],]$trendi = pnro.hinnat$trendi[index]
  population[pnro.sp.alt@data$pnro[index],]$trendimuutos = pnro.hinnat$trendimuutos  [index]
}

write.csv(population,"data/pnro-full-stack.csv")



