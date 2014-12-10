# Script for analysing the relation between population density and price trends
library("ggplot2")
library("sorvi")

postal.code.table <- get_postal_code_info()
m2p <- municipality_to_province()

load("pnro_spatial_epsg2393.RData")

population <- read.csv("vakiluku_posnro_2012_proper.csv", sep=";", header=T, colClasses="character",row.names=1)

# Read hinta data
pnro.hinnat <- read.table("pnro-hinnat.txt", sep=" ", header=TRUE, colClasses="character")
pnro.hinnat$hintataso <- as.numeric(pnro.hinnat$hintataso)
pnro.hinnat$trendi <- as.numeric(pnro.hinnat$trendi)


population$vakiluku = as.numeric(population$vakiluku)

population$logtiheys = NaN
population$maakunta = ""
rows <- row.names(population)
# add population density
for (index in seq(length(pnro.sp.alt@data$pnro))) {
  if (pnro.sp.alt@data$pnro[index] %in% rows){
    if (population[pnro.sp.alt@data$pnro[index],]$vakiluku > 0) {
        population[pnro.sp.alt@data$pnro[index],]$logtiheys = log(population[pnro.sp.alt@data$pnro[index],]$vakiluku / pnro.sp.alt@data$area.m2[index])
        temp = m2p[postal.code.table[postal.code.table$postal.code==pnro.sp.alt@data$pnro[index],]$municipality]
        names(temp) = NULL
        population[pnro.sp.alt@data$pnro[index],]$maakunta = temp
      }}}

population$trendi = NaN

# add population trendi
for (index in seq(length(pnro.sp.alt@data$pnro))) {
  population[pnro.sp.alt@data$pnro[index],]$trendi = pnro.hinnat$trendi[index]
}

#png("trendi_tiheys.png", width=2000, height=3000)
ggplot(population, aes(x=logtiheys,y=trendi)) + geom_point(shape=1) + geom_smooth(method="gam", formula = y ~ s(x))
ggsave("trendi_tiheys.png")
dev.off()
