# This file contains fucntions
# used in handling the appartment prices data

open.db.connection <- function(connection.info, debug=FALSE){
  # The database default charset has to be set to utf8. 
  # Otherwise characters will be converted to latin1 (default charset of database)

  # import functions form the package namespaces
  # I REALLY don't understand why this is neccessary
  i <- which("package:DBI"==search())
  dbConnect <- as.environment(i)$dbConnect
  dbDriver <- as.environment(i)$dbDriver
  dbGetQuery <- as.environment(i)$dbGetQuery
  
  channel <- dbConnect(dbDriver(connection.info$db.type),
                       group=connection.info$db.group,
                       default.file=connection.info$client.opts)
  
  dbGetQuery(channel,"set names utf8;")
  dbGetQuery(channel,"set character_set_database=utf8;")
  if(debug) print(dbGetQuery(channel,"show variables;")[11:25,])

  return(channel)
}

read.price.data <- function( channel=NULL,  data.source="ReadDB", connection.info=NULL,
                            data.dir, fname){
  # Read in the price data. Dafault source is a MySQL database 

  i <- which("package:DBI"==search())
  dbGetQuery <- as.environment(i)$dbGetQuery
    
  if( is.null(channel) & data.source=="ReadDB" ){
    print("Database connection required!")
    return(FALSE)
  }
  
  if(data.source =="ReadDB"){    
    sqlstr <- paste("SELECT * FROM ", connection.info$data.table, 
                    "\nWHERE queryDate=(select max(queryDate) from ",
                    connection.info$data.table, ");\n",sep="")
    price.data <- dbGetQuery(channel, sqlstr)
  }
  
  if(data.source == "ReadRdata"){
    # Depreciated
    in.file <- paste(data.dir,fname,sep="")
    load(in.file)
  }
  
  if(data.source == "ReadCsv"){
    # Depreciated
    in.file <- paste(data.dir, fname,sep="")
    price.data <- read.table(in.file, header = TRUE, sep = "|", quote = "\"'",
                             dec = ".",
                             as.is = TRUE, colClasses = "character", nrows = -1,
                             skip = 0, stringsAsFactors = FALSE,
                             fileEncoding = "UTF-8")
  }

  num.cols <- c("size","price","pricePerSqM","constructionYear",
                "floor","zipCode","nrOfRooms","buildingHeight")
  
  for(col in num.cols){
    price.data[[col]] <- as.numeric(price.data[[col]])
  }

  fact.cols <- c("elevator","appartmentCondition","buildingType")
  for(col in fact.cols){
    price.data[[col]] <- factor(price.data[[col]])
  }

  price.data$queryDate <- as.Date(price.data$queryDate,
                                  format = "%Y-%m-%d")
  return(price.data)
    
}


explore.gbm.parameters <- function(frm, price.data){

  gb.models <- list()
  
  for(i.d in seq(2,6,1)){
    
    idx <- paste("i.d=",i.d,sep="")
    gb.models[[idx]] <- gbm(frm,
                            data=price.data,             
                            distribution="laplace",     
                            n.trees=3000,                
                            shrinkage=0.01,       
                            interaction.depth=i.d,  
                            bag.fraction = 0.5,   
                            train.fraction = 0.5,   
                            n.minobsinnode = 10,  
                            cv.folds = 1,
                            keep.data=FALSE,
                            verbose=TRUE) 
  }

  for(shrink in seq(0.002,0.01,0.002)){
    
    idx <- paste("shrink=",shrink,sep="")
    gb.models[[idx]] <- gbm(frm,
                            data=price.data,             
                            distribution="laplace",     
                            n.trees=3000,                
                            shrinkage=shrink,       
                            interaction.depth=6,  
                            bag.fraction = 0.5,   
                            train.fraction = 0.5,   
                            n.minobsinnode = 10,  
                            cv.folds = 1,
                            keep.data=FALSE,
                            verbose=TRUE) 
  }


  n.of.models<-length(gb.models)
  n.trees <- gb.models[[1]]$n.trees

  df.v.errors <- matrix(nrow=n.trees*n.of.models, ncol=4) 

  for(i in 1:n.of.models){
    incr <- (i-1)*(n.trees-1)
    print((i+incr))
    df.v.errors[(i+incr):(i+n.trees+incr-1),1] <- gb.models[[i]]$valid.error
    df.v.errors[(i+incr):(i+n.trees+incr-1),2] <- gb.models[[i]]$interaction.depth
    df.v.errors[(i+incr):(i+n.trees+incr-1),3] <- gb.models[[i]]$shrinkage
    df.v.errors[(i+incr):(i+n.trees+incr-1),4] <- 1:3000
  }


  df.v.errors <- as.data.frame(df.v.errors)

  df.v.errors$V2 <- factor(df.v.errors$V2)
  df.v.errors$V3 <- factor(df.v.errors$V3)
  
  colnames(df.v.errors) <- c("validation.error","tree.size","shrinkage","index")

  return(list(models=gb.models, error.df = df.v.errors))
         
}


save.price.model <- function(tbl.name, channel, model.dir, my.model,debug=FALSE, ...){
  # Save the model as a binary file and
  # store information about it to the DB
  # input:
  #  tbl.name = name of the table containg model info in DB
  #  channel = the database connection object
  #  model.dir = directory in FS where models are saved
  # my.model = the model to be saved
  
  sqlstr <- paste("select coalesce(max(modelid),0)  as  modelid \n from ",tbl.name, " ; \n",sep="")
  if(debug) cat(sqlstr)
  
  model.id <- dbGetQuery(channel, sqlstr)[1,1] +1

  fname <- paste(model.dir,"/mymodel_", as.character(Sys.Date()),"_", model.id,"_",
                 ".Rdata",sep="")
  
  model.info <- list(
                     filename=fname,
                     fit_date = as.character(Sys.Date()),
                     data_date = as.character(max(price.data$queryDate)),
                     source_data = "asuntodata.hintadata",
                     best_iteration = best.iter
                     )

  modelProperty <- names(model.info)
  propertyValue <- unlist(model.info)

  model.info.df <- data.frame(modelid=model.id, modelProperty,
                              propertyValue, stringsAsFactors =FALSE )
  
  dbWriteTable(channel,tbl.name, model.info.df,
               row.names=FALSE, append=TRUE)
  save(my.model, file=fname)

  return(TRUE)

}


load.price.model <- function(tbl.name, channel, model.id=NULL, debug=FALSE, ...){
  # Save the model as a binary file and
  # store information about it to the DB
  # input:
  #  tbl.name = name of the table containg model info in DB
  #  channel = the database connection object
  # model.id = id of the model to be loaded
  
  sqlstr <- paste("select coalesce(max(modelid),0)  as  modelid \n from ",tbl.name, " ; \n",sep="")
  if(debug) cat(sqlstr)

  if( is.null(model.id) ) model.id <- dbGetQuery(channel, sqlstr)[1,1]

  sqlstr <- paste("select  modelid, modelproperty, propertyvalue \n from ", tbl.name,
                  "\n where modelid=",model.id, " ; \n",sep="")
  if(debug) cat(sqlstr)
  
  model.info.df <- dbGetQuery(channel, sqlstr)

  fname <- model.info.df[model.info.df$modelproperty=="filename","propertyvalue"]

  best.iter <- NA
  if("best_iteration" %in% model.info.df$modelproperty)
    best.iter <- model.info.df[model.info.df$modelproperty=="best_iteration","propertyvalue"]
  
  model.name <- load(file=fname)

  return( list(model= get(model.name), best.iter=best.iter))

}


format.new.data <- function(values, gbmodel){
  # Formats the values in list "values" in the
  # correct form for applying the model "gbmodel"
  # (using function gbm.predict() )
  # retunrs the fromatted data in a data.frame
  #
  # Crusial thiung here is to set the levels correct for
  # variables of type "factor". The gbm.model object "gbmodel"
  # contains neccessary information for this purpose
  
  
  df <- list()
    
  x <- attr(gbmodel$Terms, "dataClasses")
  num.cols <- names(x[x=="numeric"])
  
  for(i in num.cols){
    if( !is.numeric(values[[i]]) ) df[[i]] <- NA
    else df[[i]] <- values[[i]]
  }

  x <- attr(gbmodel$Terms, "dataClasses")
  fac.cols <- names(x[x=="factor"])

  for(i in fac.cols){
    idx <- which( gbmodel$var.names == i)
    df[[i]] <- factor(values[[i]], levels = gbmodel$var.levels[[idx]])
  }

  return( as.data.frame(df[ gbmodel$var.names ]) )

}

apply.apparment.prices.model <- function(model, input.values=NULL, best.iter){
  # Estimates the price of an apparment by applying the input model 
  # to the input.values
  # The information about the appartment is written to list 
  # "input.values" list is formatted to correct form for 
  # the prediction algorithm by auxiliary function format.new.data()

  if(is.null(input.values)){
    # default values for testing
    input.values <- list()
    input.values$size = 80
    input.values$constructionYear = 1926
    input.values$floor = 2
    input.values$elevator = "ei"
    input.values$appartmentCondition = "hyvä"
    input.values$zipCode = 120
    input.values$nrOfRooms = 3
    input.values$buildingType = "kerrostalo"
    input.values$buildingHeight = 4
  }

  input.data <- format.new.data(input.values, model)
  # FIX ME: it is assumed that the model gives log of price
  log.price  <- predict(model, input.data, best.iter)
  est.price  <- exp(log.price)

  return(est.price)
}

write.model.to.db <- function(model.id, model.object){
  # This approach does not work.
  # Don't know how top write a binary object to DB
  to.db <- data.frame(modelid=model.id,
                      modelobject=serialize(model.object,
                        connection= NULL,
                        ascii=TRUE)
                      )

  drv <- dbDriver("MySQL")
  channel <- dbConnect(drv, dbname="Asuntodata")

  tbl_name<-"hintamallit"

  system.time(dbWriteTable(channel,tbl_name, to.db,
                           row.names=FALSE, append=TRUE))

}
