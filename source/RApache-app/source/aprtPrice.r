# This file is not needed for the web app!!!

setContentType('text/html')

# create environment if neccessary
#if(!exists("aprt.env")) aprt.env <- new.env()
#if( "aprt.env" %in% search() ){
#  env <- as.environment( which("aprt.env"==search()) )
#} else  env <- attach(NULL, name="aprt.env")

# Load libraries if neccessary
if(FALSE){
  if( !("package:RMySQL" %in% search()) ) library(RMySQL)
  if( !("package:brew" %in% search()) ) library(brew)
  if( !("package:ggplot2" %in% search()) ) library(ggplot2)
  if( !("package:Hmisc" %in% search()) ) library(Hmisc)
  if( !("package:gbm" %in% search()) ) library(gbm)
}

# load parameters
source("/var/www/sandbox/aprt_price/lib/aprtPriceParams.r")

# load functions
source("/var/www/sandbox/aprt_price/lib/aprtPriceFunctions.r")


# Set working directory
setwd(app.dir)

# Open DB connection
if(!exists("channel"))
  channel <-  open.db.connection(connection.info)

# Load price data
price.data <- read.price.data(channel = channel,
                                      connection.info = connection.info) 


brew(file=paste(app.dir,"/brew/aprtPriceIndex.html",sep=""))

# Save a working copy of the data
#sink(file= file("/dev/null","w"))
#save(list=ls(env),file=paste(app.dir,".work.RData",sep="/"))
#sink()


