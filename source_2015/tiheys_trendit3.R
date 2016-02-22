# Script for analysing the relation between population density and price trends
library("ggplot2")
library("sorvi")
library("mgcv")
library("MASS")

population <- read.csv("data/pnro-full-stack.csv", sep=",", header=T, colClasses="character",row.names=1)
population$vakiluku <- as.numeric(population$vakiluku)
population$logtiheys <- as.numeric(population$logtiheys)
population$hintataso <- as.numeric(population$hintataso)
population$trendi <- as.numeric(population$trendi)
population$trendimuutos <- as.numeric(population$trendimuutos)
population$pinta_ala <- as.numeric(population$pinta_ala)

isot = c("Helsinki","Espoo","Tampere","Vantaa","Oulu","Turku","Jyväskylä","Kuopio","Lahti","Kouvola")
pk = c("Helsinki")
pks = c("Helsinki","Espoo","Vantaa","Kauniainen")
pksiso = c("Helsinki","Espoo","Vantaa","Kauniainen","Kirkkonummi","Siuntio","Vihti","Nurmijärvi","Kerava","Sipoo","Pornainen","Mäntsälä","Tuusula","Järvenpää")

#selection <- isot
for (selection in isot){ 
  selected = population[population$kunta == selection,]
  ggplot(selected, aes(x=logtiheys,y=trendi)) + geom_point(shape=1) + geom_smooth(method="rlm") + ggtitle(selection)
  ggsave(paste("figs/trendi_tiheys_",selection,".png",sep = ""))
    dev.off()

  ggplot(selected, aes(x=logtiheys,y=trendimuutos)) + geom_point(shape=1) + geom_smooth(method="rlm") + ggtitle(selection)
  ggsave(paste("figs/trendimuutos_tiheys_",selection,".png",sep = ""))
  dev.off()
  
}
selection <- pks
selected = population[population$kunta %in% selection,]
ggplot(selected, aes(x=logtiheys,y=trendimuutos)) + geom_point(shape=1,aes(color=kunta)) + geom_smooth(method="rlm") + ggtitle("Pääkaupunkiseutu")
ggsave("figs/trendimuutos_tiheys_pks.png")
dev.off()

selection <- pksiso
selected = population[population$kunta %in% selection,]
ggplot(selected, aes(x=logtiheys,y=trendimuutos)) + geom_point(shape=1,aes(color=kunta)) + geom_smooth(method="rlm") + ggtitle("Pääkaupnkiseutu, laaja")
ggsave("figs/trendimuutos_tiheys_pksiso.png")
dev.off()

ggplot(population, aes(x=logtiheys,y=trendimuutos)) + geom_point(shape=1,aes(color=area)) + geom_smooth(method="rlm") + ggtitle("Koko Suomi")
ggsave("figs/trendimuutos_tiheys_Suomi.png")
dev.off()

