# read data if not available
if(!exists("channel")){
  channel <-  open.db.connection(connection.info) }
price.model <- load.price.model(connection.info$model.table, channel)
dbDisconnect(channel)
rm(channel)

  
#print(POST)

input.values <- list()
input.values$size <-
  ifelse( is.null(POST$size),  NA, as.numeric(POST$size)) 
input.values$constructionYear <-
  ifelse(is.null(POST$constructionYear), NA, as.numeric(POST$constructionYear))
input.values$floor <-
  ifelse(is.null(POST$floor), NA, as.numeric(POST$floor))
input.values$elevator <-
  ifelse(is.null(POST$elevator), NA, POST$elevator)
input.values$appartmentCondition <-
  ifelse(is.null(POST$appartmentCondition), NA,  POST$appartmentCondition)
input.values$zipCode <-
  ifelse(is.null(POST$zipCode), NA, as.numeric(POST$zipCode))
input.values$nrOfRooms <-
  ifelse(is.null(POST$nrOfRooms), NA, as.numeric(POST$nrOfRooms))
input.values$buildingType <-
  ifelse(is.null(POST$buildingType), NA, POST$buildingType)
input.values$buildingHeight <-
  ifelse(is.null(POST$buildingHeight), NA,  as.numeric(POST$buildingHeight))

#print(input.values)
#print(price.model$best.iter)

best.iter <- as.numeric(price.model$best.iter)
# if the number of trees to use is not defined, use all
if(is.na(best.iter)) best.iter <- price.model$model$n.trees
   
price.est <- apply.apparment.prices.model(price.model$model, input.values, best.iter)

str <- "<p> Price estimate for the appartment: <emph>"
cat( paste(str, format(price.est,digits=6)," Eur. </emph> <p/>", sep=""))

if(FALSE){
input.values$size <- 0
input.values$constructionYear <-1926
input.values$floor <- 2
input.values$elevator <- "ei"
input.values$appartmentCondition <- NA
input.values$zipCode <- 120
input.values$nrOfRooms <- 3
input.values$buildingType <- "kerrostalo"
input.values$buildingHeight <- 4

input.values$nrOfRooms <- NULL

}
