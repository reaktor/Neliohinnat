# read data if not available
if(!exists("price.data")){
  channel <-  open.db.connection(connection.info) 
  price.data <- read.price.data(channel = channel,
                                connection.info = connection.info) 
  dbDisconnect(channel)
  rm(channel)
}

# increase the fonr size in figures
theme_set(theme_grey(base_size=50))

bar.color="blue"
price.data$nrOfRooms <- as.factor(price.data$nrOfRooms)

price.vs.size <-  function(){
  # Price vs. appartment size
  pl <- ggplot(price.data,
               aes_string(x = "size", y="price")) 
  #pl <- pl + geom_point(alpha= I(1/2))
  pl <- pl + geom_smooth(span=1, alpha=I(0.6),size=1,
                         method="lm", formula= y~poly(x,3))
  # method="lm", formula= y~poly(x,3))
  pl <- pl + coord_cartesian(xlim=c(0,175), ylim=c(0,550000))
  pl <- pl + geom_point(aes(colour=nrOfRooms), alpha=I(1/2) )
  
  pl <- pl + scale_y_continuous(breaks=seq(0,550000,50000),
                                name= expression(Hinta~(Eur)))
  pl <- pl + scale_x_continuous(breaks=seq(0,175,25),
                                name= expression(Asunnon~koko~(m^2)))
  pl <- pl + scale_fill_identity(name="Huoneiden\nlukumäärä")

  fname  <- paste(img.dir,"/",POST$id,".png",sep="")
  imgloc <- paste(img.loc,"/",POST$id,".png",sep="")
  
  return(list(pl=pl, fname=fname, imgloc=imgloc))
  
}

price.dist <- function(){
   # Price distribution
  pl <- ggplot(price.data,
               aes_string(x="price"))
  #pl <- pl+geom_histogram(binwidth=0.01) + scale_x_log10()
  pl <- pl + geom_histogram(binwidth=10000, colour=bar.color,fill=bar.color)
  pl <- pl + coord_cartesian(xlim=c(0,1e6))
  pl <- pl + scale_x_continuous(breaks=seq(0,1e6,1e5))

  fname  <- paste(img.dir,"/",POST$id,".png",sep="")
  imgloc <- paste(img.loc,"/",POST$id,".png",sep="")
  
  return(list(pl=pl, fname=fname, imgloc=imgloc))

}

size.dist <- function(){
   # Price distribution
  var <- "size"
  pl <- ggplot(price.data,  aes_string(x=var))
  pl <- pl + geom_histogram(binwidth=5, colour=bar.color,fill=bar.color)
  pl <- pl + coord_cartesian(xlim=c(0,250))
  pl <- pl + scale_x_continuous(breaks=seq(0,250,10))

  fname  <- paste(img.dir,"/",POST$id,".png",sep="")
  imgloc <- paste(img.loc,"/",POST$id,".png",sep="")
  
  return(list(pl=pl, fname=fname, imgloc=imgloc))

}

nrOfRooms.dist <- function(){
  var <- "nrOfRooms"
  pl <- ggplot(price.data,  aes_string(x=var))
  pl <- pl + geom_bar(colour=bar.color,fill=bar.color)
  pl <- pl +  theme_set(theme_grey(base_size=50))

  fname  <- paste(img.dir,"/",POST$id,".png",sep="")
  imgloc <- paste(img.loc,"/",POST$id,".png",sep="")
  
  return(list(pl=pl, fname=fname, imgloc=imgloc))

}

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


if(POST$id == "price.vs.size") img.info <- price.vs.size()
if(POST$id == "price.dist") img.info <- price.dist()
if(POST$id == "size.dist") img.info <- size.dist()
if(POST$id == "nrOfRooms.dist") img.info <- nrOfRooms.dist()
if(POST$id == "zip.table") tbl <- zip.table()

if(exists("img.info")){
  CairoPNG(filename = img.info$fname, width = 300, height = 200,
           pointsize = 15, bg = "white")

  # increase the fonr size in figures
  img.info$pl <- img.info$pl + theme_set(theme_grey(base_size=50))

  print(img.info$pl)
  dev.off()

  cat( paste('<img src=\"', img.info$imgloc,'\" height="350px", width="500px" />',sep=""))
}

if(exists("tbl")){
tbl.str <-'
  <table>
  <tr>
    <th>Zip Code</th>
    <th>Sold apartments</th>
    <th> Avg. Price</th>
    <th> Avg. Price/ m<sup>2</sup></th>
    <th> Avg apartment size </th>
    <th> 1 room apartments</th>
    <th> 2 room apartments</th>
    <th> 3 room apartments</th>
    <th> 4+room apartments</th>
  </tr>
    <% for(i in 1:nrow(tbl)){ %>
  <tr>
    <td> <%= tbl[i,"zipCode"] %> </td>
    <td> <%= tbl[i,"n"] %> </td>
    <td> <%= format(tbl[i,"meanPrice"],digits=6)  %> </td>
    <td> <%= format(tbl[i,"meanPricePerSqM"],digits=4)  %> </td>
    <td> <%= format(tbl[i,"meanSize"],digits=3) %> </td>
    <td> <%= tbl[i,"rooms.1"] %> </td>
    <td> <%= tbl[i,"rooms.2"] %> </td>
    <td> <%= tbl[i,"rooms.3"] %> </td>
    <td> <%= tbl[i,"rooms.4"] %> </td>
  </tr> 
   <%}%>
 </table>'
  brew(text=tbl.str)
 }


DONE


# CairoX11()
#setContentType("image/png")

#t <- tempfile()
#sendBin(readBin(t, "raw", n=file.info(t)$size))
#unlink(t)

#cat(search())
#cat(ls(as.environment(2)))
