rm(list=ls())
# library(httpRequest)
library(XML)
library(RMySQL)

# Set parameters ===

# common parameters
source("/var/www/sandbox/aprt_price/lib/aprtPriceParams.r")

# this file specific parameters
writeToDB <- TRUE
debug <- FALSE
writeToCSV <- debug

max.web.data.rows <- 50000

# Load functions
source(paste(app.dir,"/lib/aprtPriceFunctions.r",sep=""))


# Open database connection ===

channel <- open.db.connection(connection.info, debug=debug)


# Start script ===

setwd(work.dir)

#all.p <- seq(100,10010, by=10)

 all.p <- c(100, 120, 130, 140, 150, 160, 170, 180, 200, 210, 240, 250, 260, 270, 280,
            300,310, 320, 330, 340, 350, 360, 370, 380, 390, 400, 410, 420, 440, 500, 510,
            520, 530, 550, 560, 570, 600, 610, 620, 630, 640, 650, 660, 670, 680, 690,
            700, 710, 720, 730, 740, 750, 760, 770, 780, 790, 800, 810, 820, 830, 840,
            870, 900, 910, 920, 930, 940, 950, 960, 970, 980,  990, 1150, 1200, 1230, 1260,
            1280, 1300, 1350, 1360, 1370, 1380, 1390, 1400, 1450, 1480, 1490, 1510, 1520, 1600, 1610,
            1620, 1630, 1640, 1650, 1660, 1670, 1690, 1710, 1730, 1800, 1820, 1900, 2100, 2110, 2120,
            2130, 2140, 2160, 2170, 2180, 2200, 2210, 2230, 2240, 2260, 2270, 2280, 2300, 2320, 2330,
            2340, 2360, 2400, 2410, 2430, 2450, 2460, 2600, 2610, 2620, 2630, 2650, 2660, 2700, 2710,
            2720, 2730, 2740, 2750, 2760, 2770, 2780, 2810, 2880, 2920, 2940, 2970, 3100, 3250, 3400, 3600,
            3850, 4130, 4200, 4220, 4230, 4250, 4260, 4300, 4310, 4320, 4330, 4340, 4400, 4410, 4420,
            4430, 4440, 4460, 4500, 4600, 5100, 5200, 5400, 5460, 5800, 5810, 5820, 5830, 5840, 5880,
            5900, 6100, 6150, 6400, 6450, 7500, 7900, 8100, 8200, 8350, 8500, 8680, 8700, 9120)

if(debug) all.p <- all.p[1]

web.site.data <- matrix(nrow=max.web.data.rows,ncol=14) 
i <- 1

for(p in all.p){
for(r in c(1,2,3,4)){
for(h in c(1,2)){
  print(paste(p,r,h)) 

  use.url <- paste("http://asuntojen.hintatiedot.fi/haku/?p=",
                   p,"&r=",r,"&h=",h,sep="")

  # Parse the html document as a XML-tree
  # if url cannot be retrieved write it to file and move on
  htmldoc <- NULL 
  # write the annoying htmlParseEntityRef: expecting ';' notification to a tmp file 
  t <- tempfile()
  sink(file = t)
  try(htmldoc<-htmlParse(file=use.url, isURL = TRUE,
                         encoding="UTF-8"), TRUE)
  sink()
  unlink(t)
  
  if(!class(htmldoc)[1]=="HTMLInternalDocument"){
    cat(use.url, file="FailedUrls.txt", append=TRUE)
    next
  }
  
  
  # Find the nodes that comprise of the data table
  tableNodes <- getNodeSet(htmldoc, "//table[@id='mainTable']/tbody[3]//tr")

  # Pick the table elements and convert to a data frame 
  elem <- sapply(tableNodes,xmlChildren)
  elem <- unlist(elem[unlist(sapply(elem,length)) == 18])
  elem <- elem[names(elem) == "td"]
  elem <- sapply(elem,xmlValue)

  if(length(elem) ==0) next

  rows <- length(elem)/9 
  web.site.data[i:(i+rows-1),1:9] <- matrix(as.vector(elem),ncol=9,byrow=TRUE)

  web.site.data[i:(i+rows-1),10] <- p
  web.site.data[i:(i+rows-1),11] <- r
  web.site.data[i:(i+rows-1),12] <- h

  i <- i+rows

}}}

web.site.data <- web.site.data[1:(i-1),]

dimnames(web.site.data) <- list(NULL,c("areaName", "appartmentDescription",
                                       "size", "price", "pricePerSqM",
                                       "constructionYear","floor", "elevator", "appartmentCondition",
                                       "zipCode","nrOfRooms","buildingType",
                                       "buildingHeight","queryDate"))

web.site.data[,"buildingType"] <- ifelse(web.site.data[,12] == "1", "kerrostalo", "rivitalo" )

web.site.data[,"size"] <- sub(",",".",web.site.data[,"size"])
web.site.data[,"price"] <- sub(",",".",web.site.data[,"price"])

x <- strsplit(web.site.data[,"floor"],"/")

web.site.data[,"floor"] <- unlist(lapply(x, function(x){x[[1]]}))
tmpfun <- function(x){ if( length(x) > 1) x[[2]] else NA }
web.site.data[,"buildingHeight"] <- unlist(lapply(x, tmpfun))

web.site.data[,"queryDate"] <- as.character(Sys.Date())

Encoding(web.site.data) <- "UTF-8"


if(writeToCSV){
  # Write data csv file
  out.file <- paste(data.dir,"/asuntojen.hinnat.",
                    Sys.Date(),".txt",sep="")
  of <- file(description = out.file, open = "w", blocking = TRUE,
             encoding = "UTF-8")
  write.table(web.site.data, file = of, append = FALSE, quote = FALSE, sep = "|",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
  close(of)
}


if(writeToDB){
  
  tbl.name <- connection.info$data.table

  to.db <- as.data.frame(web.site.data,stringsAsFactors=FALSE)

  dbWriteTable(channel,tbl.name, to.db,
               row.names=FALSE, append=TRUE)

}



