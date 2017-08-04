##########################################################################################
#                                                                                        #
# Set of custom functions for use in Real Estate in the Information Age Book             #
#                                                                                        #
##########################################################################################

##########################################################################################
###  DATA INTEGRATION and CLEANING FUNCTIONS: Chapters 5 and 6 ---------------------------

### Convert CSV into SQLite data ---------------------------------------------------------

convertCSVtoSQLite <- function(dataPathCurrent,               # path to .csv Files
                               dataPathNew=dataPathCurrent,   # path to export .db file
                               newFileName,                   # name of .db file
                               fileNames=NULL,                # names of files (if not taking all .csvs)
                               tableNames=NULL,               # names of tables (if not .csv file name)
                               overWrite=FALSE,               # overwrite existing tables?
                               verbose=FALSE,                 # provide progress updates?
                               writeRowNames=FALSE            # write out row names?
){
  ## Set libraries
  
  require(RSQLite)
  require(RODBC)
  
  # Determine File Names to convert
  
  if(!is.null(fileNames)){
    fileList <- fileNames
    if(is.null(tableNames)) tableNames <- gsub('.csv', '', fileNames)
  } else {
    fileList <- list.files(dataPathCurrent)
    fileList <- fileList[grep('.csv', fileList)]
    if(is.null(tableNames)) tableNames <- gsub('.csv', '', fileList)
  }
  
  if(length(fileList) != length(tableNames)){
    cat('ERROR: File names and table names not same length')
    return('File and table names not the same length')
  }
  
  # Set up connections
  slConn <- dbConnect(dbDriver("SQLite"), dbname=paste0(dataPathNew, '/', newFileName))
  if(verbose) cat("  Converting to SQLite file: ",
                  paste0(dataPathNew, '/', newFileName), '\n')
  
  # Read in and convert each file
  
  for(fL in 1:length(fileList)){
    if(verbose) cat(paste0('Converting file number: ', fL, ' of ', 
                           length(fileList), '\n'))
    
    # Read in Tables
    xTable <- read.csv(paste0(dataPathCurrent, '/', fileList[fL]))
    
    # Check to see if they table already exists
    tExists <- dbExistsTable(slConn, tableNames[fL])
    
    # If exists and overwrite=TRUE then remove existing
    if(overWrite & tExists) {
      dbRemoveTable(slConn, tableNames[fL])
      if(verbose) cat(paste0("    Removing existing table: ",
                             tableNames[fL], '\n'))
    }
    
    # Write the table to the database
    dbWriteTable(slConn, tableNames[fL], xTable, 
                 row.names=writeRowNames)
    if(verbose) cat(paste0("    Writing table: ", tableNames[fL],
                           '\n'))
  }
  
  # Close
  dbDisconnect(slConn)
  
  # Return List of tables written
  
  return(fileList)  
}


### Create funtional Property Identification #s for King Data ----------------------------

buildPinx <- function(X,                     # Dataset to use, including Major, Minor
                      condoComp = FALSE      # Is this a condoComp dataset?
){
  
  # If condoComp fix minor to 0000
  if(condoComp) {
    X <- as.data.frame(X)
    X$Minor <- '0000'
  }
  
  # convert names    
  oldNames <- colnames(X)
  newNames <- tolower(oldNames)
  colnames(X) <- newNames
  
  # ensure value is numeric
  X$major <- as.numeric(X$major)
  if(!condoComp) X$minor <- as.numeric(X$minor)
  
  # remove NA columns
  X <- X[!is.na(X$major),]
  if(!condoComp) X <- X[!is.na(X$minor),]
  
  # Remove those with invalid values
  X <- X[X$major < 1000000, ]
  if(!condoComp) X <- X[X$minor < 10000, ]
  
  # Add leading to major
  X$major <- addLeadZeros(X$major, maxChar=6)
  
  # Add leading to minor
  if(!condoComp) X$minor <- addLeadZeros(X$minor, maxChar=4)
  
  # Combine  
  X$pinx <- paste0("..", X$major, X$minor)
  
  # Reorder
  X <- X[ ,c("pinx", newNames)]
  colnames(X)[2:ncol(X)] <- oldNames
  
  # Return X
  return(X)
}

### Add leading zeros to character fields currently stored as numeric --------------------

addLeadZeros <- function(xNbr,       # Numbers to add 0s to
                         maxChar = 6 # Desired total length
                         )
{
  missZero <- maxChar - nchar(xNbr)
  xNbr <- paste0(unlist(as.character(lapply(missZero, leadZeroBuilder))),
                 xNbr)
  return(xNbr)
}

### Subordinate function (to addLeadZeros()) that creates set of leading 0s --------------

leadZeroBuilder <- function(x)
{
  if(length(x) == 0){
    return(0)
  } else {
    gsub('x', '0', paste0(unlist(rep("x", x)), collapse=''))
  }
}

### Create a count of the number of times a various property has been sold ---------------

buildTransCount <- function(x.sales,                    # Sales dataFrame
                            trans.limit                 # Maximum allowed
                            ){
  
  # Compute trans number
  pinx.table <- as.data.frame(table(x.sales$pinx))
  colnames(pinx.table) <- c("pinx", "tn.total")
  
  # Merge back to sales
  x.sales$tn.total <- pinx.table$tn.total[match(x.sales$pinx, pinx.table$pinx)] 
  
  # Order Properly and remove those with too many transactions
  x.sales <- x.sales[order(x.sales$pinx, 
                          x.sales$sales.year,
                          x.sales$ExciseTaxNbr), ]  
  
  x.sales <- x.sales[x.sales$tn.total < trans.limit, ]
  
  # Assign a transNumber
  x.sales$trans.nbr <- 1
  for(i in 2:max(x.sales$tn.total)){ 
    idX <- which(x.sales$tn.total == i)
    idL <- length(idX) / i
    repNbrs <- rep(1:i, idL)
    x.sales$trans.nbr[idX] <- repNbrs
  }
  
  return(x.sales)
}

### Offer options on how to handle duplicates in a given field ---------------------------

idDup <- function(x.data, 
                  x.field, 
                  new.field='new.field', 
                  idd.type='toRemove',
                  bin.nonuq=FALSE){
  
  if(idd.type == 'toRemove'){
    x.data <- x.data[!duplicated(x.data[x.field]), ]
  }
  if(idd.type == 'labelDup'){
    x.data[ ,ncol(x.data) + 1] <- duplicated(x.data[x.field])
    colnames(x.data)[ncol(x.data)] <- new.field   
  }
  if(idd.type == 'labelNonUnique'){
    cntTable <- as.data.frame(table(x.data[x.field]))
    x.data[ ,ncol(x.data) + 1] <- cntTable$Freq[match(x.data[, x.field],
                                                    cntTable$Var1)]
    colnames(x.data)[ncol(x.data)] <- new.field   
    if(bin.nonuq){
      x.data[ ,ncol(x.data)] <- ifelse(x.data[ ,ncol(x.data)] > 1, 1, 0)  
    }
  }
  
  return(x.data)
}

### Function that adds Unique IDs for sales ----------------------------------------

buildSaleUIDs <- function(xSales                       # Sales dataframes
                              ){
  
  # Add Unique IDs for each Record and Each Sales
  years <- rownames(table(xSales$sales.year))
  xSales$rec.ID <- ' '
  xSales$sale.ID <- ' '
  
  # Loop through the years
  for(i in 1:length(years)){
    idX <- which(xSales$sales.year == as.numeric(years[i]))
    xSales$rec.ID[idX] <- paste0(years[i], '..', 1:length(idX))
    xSales$sale.ID[idX] <- paste0(years[i], '..', 
                                 as.numeric(as.factor(xSales$ExciseTaxNbr[idX])))
  }
  
  # Reconfigure file so that IDs are in the beginning
  cNbr <- ncol(xSales)
  xSales <- xSales[ ,c(1 ,cNbr - 1, cNbr, 3:(cNbr - 2))]
  return(xSales)
}

### Function to trim a dataset based on a field with multiple values ---------------------

trimByField <- function(x.data,
                        x.field, 
                        trim.list, 
                        inclusive=FALSE){
  
  # Identify those in list
  
  if(!inclusive){
    x.data$valid <- 1
    
    for(i in 1:length(trim.list)){
      x.data$valid <- ifelse(x.data[x.field] == trim.list[i], 0 , x.data$valid)
    }
  } else {
    x.data$valid <- 0
    
    for(i in 1:length(trim.list)){
      x.data$valid <- ifelse(x.data[x.field] == trim.list[i], 1 , x.data$valid)
    }
    
  }
  
  # Trim data
  
  x.data <- x.data[x.data$valid == 1, ]
  x.data$valid <- NULL
  
  # Return data
  
  return(x.data)
}

### Function to read in king data for a given year ---------------------------------------

readData <- function(dbName,
                     dataYear,                         # Current year
                     verbose=FALSE                     # Show progress
                         ){
  
  # Connect to database
  if(verbose) cat ('\n Connecting to ', dbName, '\n')
  dyConn <- dbConnect(dbDriver('SQLite'), dbname=dbName)
  
  # Parcel Data    
  if(verbose) cat ('\n Reading in Parcel ', dataYear, ' data \n')
  assign("parcel.data", 
         buildPinx(dbGetQuery(dyConn, 
                              paste0('SELECT Major, Minor, PresentUse FROM ',
                                     'Parcel'))),
         envir=.GlobalEnv)
  
  # Resbldg Data
  if(verbose) cat ('\n Reading in ResBldg ', dataYear, ' data \n')
  assign('resbldg.data', 
         buildPinx(dbGetQuery(dyConn, 
                              paste0('SELECT Major, Minor, BldgNbr FROM ',
                                     'ResBldg'))),
         envir=.GlobalEnv)

  # Close
  dbDisconnect(dyConn) 
}

### Point to surface converter, with optional clip ------------------------------

point2Surface <- function(points, 
                          surf.data, 
                          res, 
                          idp.val=2,
                          clip=NULL,
                          verbose=0){
  
  # ARGUMENTS
  #
  # points: SpatialPointsDataFrame of observations
  # surf.data: vector of values at the points used to create surface
  # res: resolultion or size of the grid cells
  # ipd.val: inverse distance paramter (1 to 4??)
  # clip: SpatialPolygons to clip the results to  
  
  require(sp)
  require(gstat)
  require(rgeos)
  
  ## Check points
  
  if(class(points) != "SpatialPointsDataFrame" & 
     class(points) != "SpatialPoints"){ 
    return('Points data must be SpatialPointsDataFrame or SpatialPoints object')
  }
  
  
  ## Check surface data  
  
  if(length(surf.data) != nrow(points@data)){ 
    return('Data to interpolate not same length as point data')
  }
  
  ## Extract bounding coordinates
  
  xy.box <- bbox(points)
  
  # expand out resolution
  xy.box[,1] <- xy.box[,1] - res 
  xy.box[,2] <- xy.box[,2] + res 
  
  ## Build grid of points
  
  xs <- seq(xy.box[1, 1], xy.box[1,2], res)
  ys <- seq(xy.box[2, 1], xy.box[2,2], res)
  xy.grid <- as.data.frame(expand.grid(xs, ys))
  colnames(xy.grid) <- c("x", "y")
  
  ## Create a SPDF and Pixel DF  
  
  xy.grid <- SpatialPointsDataFrame(cbind(xy.grid$x, xy.grid$y), data=xy.grid,
                                    proj4string=CRS(proj4string(points)))
  xy.pix <- as(xy.grid, "SpatialPixelsDataFrame")
  
  
  ## Create idw image  
  
  image.out <- gstat::idw(surf.data ~ 1, points, xy.pix, 
                          idp=idp.val, debug.level=verbose)
  
  ## If trim
  
  if(!is.null(clip)){
    
    # Check format
    if(class(clip) != "SpatialPolygonsDataFrame" & 
       class(clip) != "SpatialPolygons"){ 
      return(paste0('Points data must be SpatialPolygonsDataFrame',
                    'or SpatialPolygons object'))
    }
    
    # Make clip
    in.xy <- gContains(clip, xy.grid, byid=T) 
    image.out <- image.out[which(in.xy), ] 
  }
  
  ## Return 
  
  return(image.out)
  
}







################################################################################  
# Great a grid covering a set of points ----------------------------------------

createGridPoints <- function(X, gscale, mask=F){
  
  require(sp)
  
  # Set bounding range  
  BBox <- X@bbox
  xmin <- round(BBox[1,1] - gscale, -round(log(gscale,10),0))
  xmax <- round(BBox[1,2] - gscale, -round(log(gscale,10),0))
  ymin <- round(BBox[2,1] - gscale, -round(log(gscale,10),0))
  ymax <- round(BBox[2,2] - gscale, -round(log(gscale,10),0))
  xrange <- xmax - xmin
  yrange <- ymax - ymin
  
  # set up capture object
  grid.coords <- matrix(nrow=(yrange/gscale) * (xrange/gscale), ncol = 2)
  x.nums <- (xmin/gscale):(xmax/gscale)
  y.nums <- (ymin/gscale):(ymax/gscale)
  
  # Assign Points 
  for(x in 1:(xrange/gscale)){
    for(y in 1:(yrange/gscale)){
      grid.coords[((x-1)*(yrange/gscale))+y,] <- c(x.nums[x]*gscale,
                                                   y.nums[y]*gscale)
    }
  }
  
  grid.coords <- grid.coords[!is.na(grid.coords[,1]),]
  
  # Create Full Grid
  grid.sp <- SpatialPointsDataFrame(grid.coords, 
                                    data=as.data.frame(1:dim(grid.coords)[1]),
                                    proj4string=X@proj4string)
  
  # Mask
  if(mask){  
    #gi <- which(gIntersects(X, grid.sp, byid=T))
    #grid.sp <- grid.sp[which(gi),]
    gi <- over(grid.sp, X)
    giX <- which(!is.na(gi[,1]))
    grid.sp <- grid.sp[giX, ]
  }
  
  # Return grip
  
  return(grid.sp)
  
}


















### Tool to source directly from Github ------------------------------------------------------------------

sourceHttps <- function(u,                          # Name of location (raw Github address)
                         unlink.tmp.certs = FALSE    # Security certification handling
                         ) {
  # load package
  require(RCurl)
  
  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}

# Place multiple ggplots into a configuration ----------------------------------

ggMultiPlots <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



fullSummary <- function(x){
  
  Min <- min(x)
  
  Pctl.01 <- quantile(x, .01)
  Pctl.05 <- quantile(x, .05)
  Pctl.25 <- quantile(x, .25)
  
  Pctl.50 <- quantile(x, .50)
  Mean <- mean(x)
  
  Pctl.75 <- quantile(x, .75)
  Pctl.95 <- quantile(x, .95)
  Pctl.99 <- quantile(x, .99)
  
  Max <- max(x)
  
  StDev <- sd(x)
  
  return(c(Min=Min, 
           Pctl.01=Pctl.01, Pctl.05=Pctl.05, Pctl.25=Pctl.25, 
           Median=Pctl.50, Mean=Mean,
           Pctl.75=Pctl.75, Pctl.95=Pctl.95, Pctl.99=Pctl.99, 
           Max=Max,
           StDev=StDev))
}



### Sale Cleaning Master function --------------------------------------------------------

# cleanSales <- function(saleYears = c(2011, 2016),      # Sales years to use
#                        transLimit = 5,                # Max number of sales per prop
#                        salesDB,
#                        trimList=list(SaleReason=2:19,  
#                                      SaleInstrument=c(0, 1, 4:28),
#                                      SaleWarning=paste0(" ", c(1:2, 5:9, 11:14,
#                                                                18:23, 25, 27,
#                                                                31:33, 37, 39,
#                                                                43, 46, 48, 49,
#                                                                50:53, 59, 61,
#                                                                63, 64, 66),
#                                                           " ")),
#                        overWrite=TRUE,                 # Overwrite existing files
#                        verbose=FALSE                   # Give progress
# ){
#   
#   # libraries
#   require(RODBC)
#   require(RSQLite)
#   require(stringr)
#   
#   # read in Sales File
#   if(verbose) cat('Reading in raw sales\n')
#   salesConn <- dbConnect(dbDriver('SQLite'), salesDB)
#   rawSales <- dbReadTable(salesConn, 'AllSales')
#   
#   # base clean
#   if(verbose) cat('Removing sales with missing PIN or Price\n')
#   cleanSales <- rawSales[rawSales$Major > 0, ]
#   cleanSales <- cleanSales[cleanSales$SalePrice > 0, ]
#   
#   # build sales data
#   if(verbose) cat('Building sales date\n')
#   cleanSales$docDate <- paste(substr(cleanSales$DocumentDate, 4, 5)
#                               ,substr(cleanSales$DocumentDate, 1, 2),
#                               substr(cleanSales$DocumentDate, 7, 10), sep="")
#   cleanSales$salesDate <- as.POSIXct(strptime(cleanSales$docDate, "%d%m%Y"))
#   cleanSales$salesYear <- as.numeric(format(cleanSales$salesDate, "%Y"))
#   cleanSales <- cleanSales[!is.na(cleanSales$salesDate), ]
#   
#   # eliminate Transactions prior to Sales Year Limit
#   if(verbose) cat('Limiting to selected time frame\n')
#   cleanSales <- cleanSales[cleanSales$salesYear >= saleYears[1] & 
#                              cleanSales$salesYear <= saleYears[2], ]
#   
#   # add PINX
#   if(verbose) cat('Adding PINx\n')
#   cleanSales <- buildPinx(cleanSales)
#   
#   # add trans count and limit by paramter
#   if(verbose) cat('Adding Trans count\n')
#   cleanSales <- buildTransCount(cleanSales, transLimit=transLimit)
#   
#   # add MultiParcel sale designation
#   if(verbose) cat('Labeling Multiple parcel sales\n')
#   cleanSales <- idDup(cleanSales, 'ExciseTaxNbr', new.field = 'multiParcel',
#                       iddType='labelNonUnique', binNonUq=TRUE)
#   
#   # Add unique IDs
#   if(verbose) cat('Adding Unique IDs\n')
#   trimSales <- buildSaleUIDs(cleanSales)
#   
#   # Trim sales by Insturment, reason and warning
#   if(verbose) cat('Removing bad Reasons Instruments and Warnings\n')
#   
#   # Fix the "Warning" Field.  Add a leading/trailing space for the grep()
#   trimSales$SaleWarning <- paste(" ", trimSales$SaleWarning, " ", sep="")
#   
#   for(tL in 1:length(trimList)){
#     trimSales <- trimByField(trimSales, names(trimList)[tL],
#                              trimList = unlist(trimList[tL]))
#   }
#   
#   # Write out
#   if(verbose) cat('Writing out data \n')
#   tExists <- dbExistsTable(salesConn, 'trimmedSales')
#   
#   if(overWrite & tExists) {
#     dbRemoveTable(salesConn, 'trimmedSales')
#     if(verbose) cat(paste0('    Removing existing table: trimmedSales\n'))
#   }
#   dbWriteTable(salesConn, 'trimmedSales', trimSales, row.names=FALSE)
#   
#   # Close
#   dbDisconnect(salesConn)
# }
# ### Function that applies labels to king sales -------------------------------------------
# 
# labelSales <- function(salesDB,
#                        dataYear,
#                        overWrite=TRUE,                  # Should overwrite? 
#                        verbose=FALSE                    # See progress?
#                        ){
#   
#   require(plyr)
#   
#   # Set null values
#   labeledSales <- list()
#   oldYears <- NULL
#   
#   # Read in sales Data
#   if(verbose) cat('Reading in sales data \n')
#   salesConn <- dbConnect(dbDriver('SQLite'), salesDB)
#   pSales <- dbReadTable(salesConn, 'trimmedSales')
#   dbDisconnect(salesConn)
#   
#   # Read in Data
#   readData(dbName=salesDB,
#            dataYear)
#     
#   # Add Present Uses
#   pSales$present.use <- parcel.data$PresentUse[match(pSales$pinx,
#                                                   parcel.data$pinx)]
# 
#   # Add the record type
#   pSales$res.record <- resbldg.data$BldgNbr[match(pSales$pinx,
#                                               resbldg.data$pinx)]
#   pSales$res.record <- ifelse(is.na(pSales$res.record), 0, pSales$res.record)
#   
#   # Remove those with non-residential record type or with more than one dwelling on it  
#   pSales <- pSales[pSales$res.record == 1, ]
#     
#   # Remove those not with SFR or Townhome use category
#   pSales <- pSales[pSales$present.use == 2 |
#                        pSales$present.use == 29, ]
#     
#   # Write out
#   if(verbose) cat('Writing out data \n')
#   salesConn <- dbConnect(dbDriver('SQLite'), salesDB)
#   tExists <- dbExistsTable(salesConn, 'labeledSales')
#   
#   if(overWrite & tExists) {
#     dbRemoveTable(salesConn, 'labeledSales')
#     if(verbose) cat(paste0('    Removing existing table: labeledSales\n'))
#   }
#   dbWriteTable(salesConn, 'labeledSales', pSales, row.names=FALSE)
#   
#   # Close
#   dbDisconnect(salesConn)
#   
#   #Clean up
#   for(delX in c('parcel.data','resbldg.data')){
#     rm(list=ls(pattern=glob2rx(paste0(delX,"*"))))
#   }
#   gc()  
#   
# } #ends function

