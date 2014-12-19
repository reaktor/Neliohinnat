# Load libraries
library(RMySQL)
library(brew)
library(ggplot2)
library(Hmisc)
library(gbm)
library(maptools)
library(sp)
library(RColorBrewer)
library(RgoogleMaps)
library(rgdal)

# load parameters
source("/var/www/sandbox/aprt_price/lib/aprtPriceParams.r")

# load functions
source("/var/www/sandbox/aprt_price/lib/aprtPriceFunctions.r")

# Read all required data ========

# load price data and model
if(!exists("price.data") | !exists("price.model")){
  channel <-  open.db.connection(connection.info) 
  price.data <- read.price.data(channel = channel,
                                connection.info = connection.info)
  
  price.model <- load.price.model(connection.info$model.table,channel, debug=FALSE)
  price.model <- price.model[[1]]
  dbDisconnect(channel)
  rm(channel)
}

# Read in boundaries of ZIP code regions and cities
map.data.dir <- "~/wrkCrypt/Data/Geographics/Orig_200802/"
#kunnat <- read.shape(paste(data.dir,"Kuntarajat/ESRI/Kunta200.shp",sep=""))
#posti <- read.shape(paste(data.dir,"Postinumeroaluerajat/ESRI/Posti_region.shp",sep=""))

zipCodeAreas <-
  readShapeSpatial(paste(map.data.dir,"Postinumeroaluerajat/ESRI/Posti_region.shp",sep=""))

#change encoding and save as character (not factor)
for (cn in  c("KUNTA","TOIMIP","NIMI"))
  zipCodeAreas[[cn]] <- iconv(zipCodeAreas[[cn]], from="latin1", to="UTF-8")
# Add numeric zip code
zipCodeAreas[["zip"]] <- as.numeric(as.character(zipCodeAreas[["PNRO"]]))


# Read zip area polygons in lat-lon coordinates
fname <- "/home/jouni/sandbox/maps/zipCodeCoordsWSG.txt"
zipPolys <- read.table(fname,sep=";")
colnames(zipPolys) <- c("lat","lon","zip")

# Definitions of helper functions ========

to.bins <- function(v,n){
  rr <- range(v)
 
  le <- floor(rr[1]/1000)*1000
  ue <- ceil(rr[2]/1000)*1000
  
  bin <- round( (ue-le) / n,digits=-3)

  br <- vector("numeric",n+1)
  br.mid <- vector("numeric",n)
  
  br[1] <- le
  for(i in 2:(length(br)-1))   br[i] <- br[i-1]+bin
  br[length(br)] <- ue

  aa <- cut(t.df$col.var, breaks=br)
  levels(aa) <-  br[1:(length(br)-1) ] + bin/2
  return(aa)
}

# Stats by zip code
zip.table <- function(){
  # Table of aggregates
  aggFunc <- function(df){
    n <- nrow(df)
    meanPrice <- mean(df$price)
    meanPricePerSqM <- mean(df$pricePerSqM)
    meanSize <- mean(df$size)
    rooms.1 <- sum(df$nrOfRooms=="1")
    rooms.2 <- sum(df$nrOfRooms=="2")
    rooms.3 <- sum(df$nrOfRooms=="3")
    rooms.4 <- sum(df$nrOfRooms=="4")
    
    return(c(n,meanPrice,meanPricePerSqM,meanSize, rooms.1,
             rooms.2, rooms.3, rooms.4)) 
}
  pd.agg <- ddply(price.data, c("zipCode"), aggFunc)
  colnames(pd.agg)[-1] <- c( "n","meanPrice", "meanPricePerSqM",
                            "meanSize",  "rooms.1",  "rooms.2",  "rooms.3",
                            "rooms.4")
  return(pd.agg)
}

# extract the polygons from the spatial object
getpoly <- function(pol){
  z <- pol@Polygons
  return(z[[1]]@coords)
}

# Re-define the function to plot polygons -added "density" option to the
# polygon function
PlotPolysOnStaticMap.JKa <- function (MyMap, polys, col, verbose = 1,
                                      density=NULL, lwd=0.5, ...) {
  Rcoords <- LatLon2XY.centered(MyMap, lat = polys[, "Y"], 
                                lon = polys[, "X"])
  polys.XY <- polys
  polys.XY[, "X"] <- Rcoords$newX
  polys.XY[, "Y"] <- Rcoords$newY
  if (!missing(col)) 
    polys.XY[, "col"] <- col
  if (!("col" %in% colnames(polys.XY))) 
    polys.XY[, "col"] <- rgb(0.1, 0.1, 0.1, 0.05)
  op <- par("lwd"=lwd)
  tmp <- PlotOnStaticMap(MyMap, verbose = 0, ...)
  tmp <- by(polys.XY[, c("X", "Y", "col")], polys.XY[, "PID"],
            density=density, border=grey(0.8), 
            mypolygon)
  par(op)
}

# Interactive part starts here ========

# Calculate some summary stats by zip codes from the sales data 
pd.agg <- zip.table()

# join stats to the spatial polygons data
t.df <- as.data.frame(zipCodeAreas)
t.1 <-  merge(t.df, pd.agg,  by.x = "zip", by.y = "zipCode",
            all.x = TRUE, all.y = FALSE,
            sort = FALSE)

t.2 <- t.1[order(t.1$zip),]
rownames(t.2) <- rownames(as.data.frame(zipCodeAreas))
zipCodeAreas@data <- t.2

# Choose zip codes to use
ci <- c("HELSINKI","ESPOO","VANTAA","KAUNIAINEN")#,"SIPOO","KERAVA")
t.df <- as.data.frame(zipCodeAreas)
use.zip <- t.df[t.df$KUNTA %in% ci,"zip"]

# Take a subset from the spatial polygons data
use.areas <- zipCodeAreas[zipCodeAreas$zip %in% use.zip,]
names(use.areas)
summary(use.areas)

# Take a subset from the zip area polygons
use.zipPolys <-  zipPolys[zipPolys$zip %in% use.areas$zip,] 

# Plot the zip code polygons on google map tile ==

# get a map centered in helsinki
#helsinki.center <- c(60.172701, 24.934044)
helsinki.center <- c(60.2228335,24.9141885)
fname <- "Helsinki.png"
zoom <- 11
helsinki.map <- GetMap.bbox(center=helsinki.center, destfile=fname, maptyp="mobile",
                            sensor=FALSE, zoom=zoom)

# Plot mean price / sq meter ==
# define color for each zip polygon
col.var <- cut2(use.areas$meanPricePerSqM,
                g=8, levels.mean=TRUE, digits=1)

myPalette<-brewer.pal(length(levels(col.var)),"PuRd")
myPalette<- rev(brewer.pal(length(levels(col.var)),"Spectral"))


#myPalette<-brewer.pal(length(levels(col.var)),"Spectral")

t.df <- data.frame(zip=use.areas$zip, color=myPalette[col.var],
                   color.var=use.areas$meanPricePerSqM,
                    stringsAsFactors = FALSE)

t.df[ is.na(t.df$color), "color"] <- rgb(1,1,1)

# join the color to the zip  polygon data frame
plot.polys <- merge(use.zipPolys,t.df,by="zip",all.x=TRUE,all.y=FALSE)
# rename columns, plot function requires these names
colnames(plot.polys) <- c("PID","Y","X","col","col.var")

# the polygons seem to be just a bit off
# now here's a kludge:
plot.polys$X <- plot.polys$X-0.002

PlotPolysOnStaticMap.JKa(helsinki.map, plot.polys, density=75,lwd=0.75)


# define polygon to be plotted
lon <- c(24.9, 25.0, 25.0, 24.9)
lat <-c(60.20, 60.20, 60.30, 60.30)
df.poly <- data.frame(X=lon,Y=lat,col=4,PID=1) 


# Plot the effect of zip code in tge gbm model

cn <- "zipCode"
t.df <- plot(price.model,
            i.var=which(price.model$var.names==cn),
            continuous.resolution = 1000,
            return.grid=TRUE)
#plot(t.df[,1], exp(t.df[,2]), type="l") 

t.df$zip <- round(t.df$zipCode,-1)
t.df$col.var <- exp(t.df$y)

t.df <- unique(t.df)
# define color for each zip polygon
#col.var <- cut(t.df$col.var, breaks=8)
col.var <- to.bins(t.df$col.var, n=8)

myPalette<-brewer.pal(length(levels(col.var)),"PuRd")
myPalette<- rev(brewer.pal(length(levels(col.var)),"Spectral"))

t.df$color <- myPalette[col.var]
t.df[ is.na(t.df$color), "color"] <- rgb(1,1,1)
t.df$zipCode <- NULL
t.df$col.var <- NULL
t.df$y <- NULL


# join the color to the zip  polygon data frame
v <- t.df$color
names(v) <- as.character(t.df$zip)
cl <- v[as.character(use.zipPolys$zip)]

plot.polys <-data.frame(X=use.zipPolys$lon,
                        Y=use.zipPolys$lat,
                        PID=use.zipPolys$zip,
                        col=cl,
                        stringsAsFactors = FALSE)


# the polygons seem to be just a bit off
# now here's a kludge:
plot.polys$X <- plot.polys$X-0.002

to.file <- FALSE

if(to.file){
  png(filename = "HelsinkiZipPrices.png",
      width = 640, height = 640, units = "px",
      pointsize = 12, bg = "white",  res = NA,
      type ="cairo")
}
PlotPolysOnStaticMap.JKa(helsinki.map, plot.polys, density=75,lwd=0.65)

if(to.file) dev.off()


xp <- c(0.2,0.5)
yp <- c(0.1,0.1)
plot((1:10)/10,(1:10)/10,type="n", xlab=NULL, ylab=NULL)
n <- 8
for(i in 1:n){
  lines(xp,yp,lwd=10, col=myPalette[i])
  text((xp[2]+0.05), yp[1], levels(col.var)[i], col=1, adj=c(-.1,-.1))
  yp <- yp+0.1
}
  

   ## Setup up coordinate system (with x==y aspect ratio):
     plot(c(-2,3), c(-1,5), type = "n", xlab="x", ylab="y", asp = 1)
     ## the x- and y-axis, and an integer grid
     abline(h=0, v=0, col = "gray60")
     text(1,0, "abline( h = 0 )", col = "gray60", adj = c(0, -.1))
     abline(h = -1:5, v = -2:3, col = "lightgray", lty=3)
     abline(a=1, b=2, col = 2)
     text(1,3, "abline( 1, 2 )", col=2, adj=c(-.1,-.1))



levels(col.var)

bb <- to.bins(t.df$col.var,8)
summary(bb)


summary(t.df$col.var)


t.df <- data.frame(zip=use.areas$zip, color=myPalette[col.var],
                   color.var=use.areas$meanPricePerSqM,
                    stringsAsFactors = FALSE)



cn <- "zipCode"
tmp <- plot(price.model,
            i.var=which(price.model$var.names==cn),
            continuous.resolution = 1000,
            return.grid=TRUE)
plot(tmp[,1], exp(tmp[,2]), type="l") 


plot(price.model, i.var=c(1,2,6))




#qn <- quantile(t.1$meanPricePerSqM,probs=seq(0,1,0.2))
#br <- signif(quantile(t.1$meanPricePerSqM,probs=seq(0,1,0.2)),3)
#t <- cut(t.1$meanPricePerSqM, breaks=br)
#levels(t) <- as.character(br)

# plotting
spplot(areas, "col.var", col=grey(.9), col.regions=myPalette,
       main="XYZ")

areas.poly.list <- lapply(areas@polygons, getpoly)

areas.colors <- myPalette[areas$col.var]

# bounding box
mx.x <- max(unlist(lapply(areas.poly.list, function(x) max(x[,1]) )))
mx.y <- max(unlist(lapply(areas.poly.list, function(x) max(x[,2]) )))

mn.x <- min(unlist(lapply(areas.poly.list, function(x) min(x[,1]) )))
mn.y <- min(unlist(lapply(areas.poly.list, function(x) min(x[,2]) )))

plot(1:10, xlim=c(mn.x, mx.x) , ylim=c(mn.y, mx.y) , type="n")
#polygon(areas.poly.list[[1]], density=49,col="red")
mapply( function(a,b) polygon(a, col=b, density=70),  areas.poly.list, areas.colors)


load("~/Downloads/FIN_adm4.RData")
class(gadm)
names(gadm)

#change encoding and save as character (not factor)
for (cn in  c("NAME_3","NAME_4"))
  gadm[[cn]] <- toupper(iconv(gadm[[cn]], from="latin1", to="UTF-8"))

as.data.frame(gadm)[1:10,"NAME_4"]

ci <- c("HELSINKI","ESPOO","VANTAA","KAUNIAINEN")#,"SIPOO","KERAVA")
t.df <- gadm[gadm$NAME_4 %in% ci,]
#x <- spplot(t.df, "ID_4",col.regions="red")
#print(x)

cit <- lapply(t.df@polygons, getpoly) 

plot(1:10, xlim=c(mn.x, mx.x) , ylim=c(mn.y, mx.y) , type="n")
lapply(cit, polygon, border="black")


x <- 1



# Get map from google maps ==


#lon <- c(24.6, 25.0)
#lat <-c(60.2, 60.356949)
#fname <- "GraterHelsinki.png"
#marker <- "60.150000,24.900000,blue"

#helsinki.map <- GetMap.bbox(lonR=lon,latR=lat,destfile=fname,maptyp="mobile",
#                            sensor=FALSE,marker=marker)


helsinki.center <- c(60.172701, 24.934044)
fname <- "Helsinki.png"
zoom <- 11

helsinki.map <- GetMap.bbox(center=helsinki.center, destfile=fname, maptyp="roadmap",
                            sensor=FALSE, zoom=zoom)

# define polygon to be plotted
lon <- c(24.9, 25.0, 25.0, 24.9)
lat <-c(60.20, 60.20, 60.30, 60.30)

df.poly <- data.frame(X=lon,Y=lat,col=4,PID=1) 

df.poly <- zipPolys[zipPolys$zip %in% zip,]



PlotPolysOnStaticMap.JKa(helsinki.map,df.poly,density=75)



# GARBAGE



x <- areas[1:10,]
y <- x@polygons
(z <- y[[3]]@Polygons)
(p <- z[[1]]@coords)

p <- areas.poly.list[[1]]

plot(p[,1], p[,2], type="n")
polygon(p[,1],p[,2],density=4,col="red")


# extract the polygons from the spatial object
getpoly <- function(pol){
  z <- pol@Polygons
  return(z[[1]]@coords)
}
allpolys <- lapply(zipCodeAreas@polygons, getpoly)



# Write zip area polygons to file
zz <- mapply(function(a,b) cbind(a,b) , allpolys, zipCodeAreas$zip)
zz <- unlist(zz)

tm <- matrix(nrow=1000000,ncol=3)
i <- 1
for(el in zz){
  ll <- nrow(el)
  tm[i:(i+ll-1),] <- el
  i <- i+ll
}


tm <- tm[1:(i-1),]
dim(tm)

write.table(tm, file = "zipCodeCoordsKKJ.txt", sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"))
