rm(list=ls())
library(ggplot2)
library(gbm)
library(DBI)
library(RMySQL)


# Set parameters ===
# common parameters
source("/var/www/sandbox/aprt_price/lib/aprtPriceParams.r")

# this file specific parameters
explore.parameters <- FALSE
debug <- TRUE

# Load functions
source(paste(app.dir,"/lib/aprtPriceFunctions.r",sep=""))


# Open database connection ===
channel <- open.db.connection(connection.info, debug=debug)

# Start script ===

setwd(work.dir)

price.data <- read.price.data(channel=channel, connection.info= connection.info)


# Remove scandic characters from variables as they cause problems
# set missing values to NA
tmp <-  as.character(price.data$appartmentCondition)
tmp <- gsub("ä","a",tmp)
tmp[!(tmp %in% c("huono","hyva","tyydyttava"))] <- NA
price.data$appartmentCondition <- as.factor(tmp)

tmp <-  as.character(price.data$buildingType)
tmp[!(tmp %in% c("kerrostalo","rivitalo"))] <- NA
price.data$buildingType <- as.factor(tmp)

tmp <-  as.character(price.data$elevator)
tmp[!(tmp %in% c("on","ei"))] <- NA
price.data$elevator <- as.factor(tmp)


if(debug){
  print(head(price.data))
  print(summary(price.data))
  print(str(price.data))
}


# fit a model for price using gradient boosting
# fitting formula = frm
frm <- formula(log(price) ~ size + constructionYear + floor + elevator + appartmentCondition +
               zipCode + nrOfRooms + buildingType + buildingHeight)

# take only helsinki-espoo-vantaa-kauniainen
price.data <- price.data[price.data$zipCode %in% use.zip, ] 

if(explore.parameters){
  # Explore the parameters "shrinkage" and "tree.size" 
  # used in the GB -algorithm
  param.expl <- explore.gbm.parameters(frm, price.data)

  # Plot the validation error for various parameters
  pl <- qplot(index, validation.error,
              geom="line", colour=tree.size,
              data=param.expl$error.df, facets = shrinkage ~. )
  
  pl <- qplot(index, validation.error,
              geom="line", colour=shrinkage,
              data=param.expl$error.df, facets = tree.size ~. )

  pl <- qplot(index, validation.error,
              geom="line", 
              data=param.expl$error.df, facets = shrinkage ~ tree.size )

  print(pl)
}

# Fit the actual usable model
# Chosen: shrinkage = 0.005, tree.size=3
my.model <-  gbm(frm,
                 data=price.data,             
                 distribution="laplace",     
                 n.trees=5000,                
                 shrinkage=0.005,       
                 interaction.depth=3,  
                 bag.fraction = 0.5,   
                 train.fraction = 0.5,   
                 n.minobsinnode = 10,  
                 cv.folds = 1,
                 keep.data=FALSE,
                 verbose=TRUE)

best.iter <- gbm.perf(my.model,method="test")

print(best.iter)

save.price.model(connection.info$model.table,
                 channel, model.dir, my.model,debug=TRUE,
                 best.iter=best.iter)


# Example how to load and apply saved model
if(FALSE{
  price.model <- load.price.model(connection.info$model.table, channel)
  price.est <- apply.apparment.prices.model(price.model$model)
}
