# This file contains parameters that are common
# for all different parts of the project

# Directories
work.dir <- "/var/www/sandbox/aprt_price"
data.dir <- "None"
model.dir <- "/var/www/sandbox/aprt_price/data"
app.dir <- "/var/www/sandbox/aprt_price"
app.loc <- "/sandbox/aprt_price"

img.dir <- paste(app.dir,"/images",sep="")
img.loc <- paste(app.loc,"/images",sep="")




# Parameters for the DB connection
connection.info <-  list()
connection.info$model.table <- "price_models"
connection.info$data.table <- "sales_data"
connection.info$db.group <- "appartment_price_data"
connection.info$db.type <- "MySQL"
connection.info$client.opts <- "/home/jouni/.my.cnf"


