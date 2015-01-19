first.nna2 <- function (c1, c2)  ifelse(!is.na(c1), c1, c2)
first.nna <- function (...) Reduce(first.nna2, list(...))

prefix.factor <- function (pnro, n) as.factor(substr(pnro, 1, n))
l3 <- { . %>% prefix.factor(., 1) }
l2 <- { . %>% prefix.factor(., 2) }
l1 <- { . %>% prefix.factor(., 3) }

population <- read.csv("data/vakiluku_posnro_2012_proper.csv", 
                       sep=";", header=T, colClasses="character") %>%
  transmute(pnro=Postinumero, log.population=log(as.numeric(vakiluku)))

# Span the space on pnro's form the spatial data.
pnro.area <- local({
  load("data/pnro_spatial_epsg2393.RData") # And pnro.sp magically appears (FIXME: rds...)
  data.frame(pnro=pnro.sp.alt$pnro, log.area=log(pnro.sp.alt@data$area.m2)-18) }) %>%
  inner_join(population, by="pnro") %>% 
  mutate(log.density = (log.area - log.population)/10)

