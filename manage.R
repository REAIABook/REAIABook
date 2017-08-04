##########################################################################################
#                                                                                        #            
#  Code for Chapter 6 (Manage) of Real Estate Analysis in the Information Age            #   
#                                                                                        #                  
##########################################################################################

### Preliminary Commands -----------------------------------------------------------------

 ## Load Libraries

  library(RSQLite)
  library(RODBC)
  library(tidyverse)
  library(maptools)
  library(sf)
  library(sp)

 ## Set data and code directory

  data.dir <- 'c:/temp/' 
  code.dir <- 'c:/code/REAIABook/'

 ## Load custom source files

  source(file.path(code.dir, 'custom_functions.R'))  

 ## Set the database path and name  

  data.db <- file.path(data.dir, 'seattleCaseStudy.db')

### Arrange assessor data into single db file --------------------------------------------

 ## Convert CSVs to SQLite

  if (!file.exists(data.db)){
  
   convertCSVtoSQLite(dataPathCurrent=file.path(data.dir, 'assessor'),
                      dataPathNew=data.dir,
                      newFileName='seattleCaseStudy.db',
                      fileNames=c('EXTR_RPSale.csv',
                                  'EXTR_Parcel.csv',
                                  'EXTR_Resbldg.csv'),
                      tableNames=c('Sales',
                                   'Parcel',
                                   'ResBldg'),
                      overWrite=TRUE,
                      verbose=TRUE,
                      writeRowNames=FALSE)
  }

## Fix Unique IDS for the Assessor Data --------------------------------------------------
  
 ## Open up connection to database
  
  db.conn <- dbConnect(dbDriver('SQLite'), data.db)
  
 ## Sales
  
  # Read in data
  sales.data <- dbReadTable(db.conn, 'Sales')

  # Make new UID from ExciseTaxNbr
  sales.data <- dplyr::mutate(sales.data, UID=ExciseTaxNbr)
  
  # Place Excise Tax Number on far left
  sales.data <- dplyr::select(sales.data, UID, everything())

  # Order by that field
  sales.data <- dplyr::arrange(sales.data, UID)

  # Create fully unique field
  
  # Get a data.frame of those UID that are duplicated
  uid.mult <- sales.data %>% 
    dplyr::group_by(UID) %>%
      dplyr::summarize(rec.count=n()) %>%
       dplyr::filter(rec.count > 1)
  
  # Develop a list full of proper suffixes
  uid.suffix <- lapply(split(uid.mult, uid.mult$UID), 
                       function(x) 1:(x$rec.count))
  names(uid.suffix) <- NULL
  
  # Apply suffix to sales.data
  sales.data$uid.suffix <- 0
  sales.data$uid.suffix[sales.data$UID %in% uid.mult$UID] <- unlist(uid.suffix)
  
  # Concatenate to make an unique ID
  sales.data$UID <- ifelse(sales.data$uid.suffix == 0,
                           sales.data$UID,
                           paste0(sales.data$UID, '..', sales.data$uid.suffix))
  
  # Remove uid.suffix field
  sales.data$uid.suffix <- NULL

  # Write out
  dbRemoveTable(db.conn, 'Sales')
  dbWriteTable(db.conn, 'Sales', sales.data, row.names=FALSE)
  
 ## Parcel Data
  
  # Read in data
  parcel.data <- dbReadTable(db.conn, 'Parcel')
  
  # Add unique pinx field
  parcel.data <- buildPinx(parcel.data)
  
  # Write out
  dbRemoveTable(db.conn, 'Parcel')
  dbWriteTable(db.conn, 'Parcel', parcel.data, row.names=FALSE)
  
## ResBldg Data
  
  # Read in data
  resbldg.data <- dbReadTable(db.conn, 'ResBldg')
  
  # Add unique pinx field
  resbldg.data <- buildPinx(resbldg.data)
  
  # Add the bldgnbr to complete unique id
  resbldg.data$pinx <- paste0(resbldg.data$pinx, '.', resbldg.data$BldgNbr)
  names(resbldg.data)[1] <- 'BldgID'
  
  # Write out
  dbRemoveTable(db.conn, 'ResBldg')
  dbWriteTable(db.conn, 'ResBldg', resbldg.data, row.names=FALSE)
  
 ## Clean up
  
  rm(resbldg.data); rm(parcel.data); rm(sales.data)
  gc()
  
### Convert the Parcel Cadastral data into R objects for faster loading, etc. later ------
  
 ## Convert the parcel file to centroids
  
  # Load in parcel file
  parcels <- st_read(file.path(data.dir, 'geographic', 'parcel', 'parcel.shp'), 
                     quiet=TRUE)

  # Convert to appropriate CRS
  parcels <- st_transform(parcels, crs=4326)
  
  # Extract centroid Lat longs
  parcel.centroids <- st_centroid(parcels)
  
  # Convert to a simple features data.frame
  parcel.centroids <- st_sf(parcel.centroids)
  
  # Add PIN values
  parcel.centroids$PIN <- parcels$PIN
  
  # Save as an R object for loading later
  save(parcel.centroids, file=file.path(data.dir, 'geographic', 'parcelcentroids.Rdata'))

### Add Crime Beat data to database ------------------------------------------------------
  
  # Read in data
  crime.data <- read.csv(file.path(data.dir, 'crime', 'seattle_crime.csv'))
  
  # Convert names
  names(crime.data) <- tolower(names(crime.data))
  names(crime.data)[1] <- 'uid'
  
  # Write out to database
  if (dbExistsTable(db.conn, 'Crime')){
    dbRemoveTable(db.conn, 'Crime')
  }
  dbWriteTable(db.conn, 'Crime', crime.data, row.names=FALSE)
  
### Add Twitter sentiment data to database -----------------------------------------------   
  
 # Read in tweet sentiment data  
  tweet.sent <- read.csv(file.path(data.dir, 'tweets', 'sentimenttweets.csv'),
                         header=TRUE)
  
  # Remove if exists 
  if (dbExistsTable(db.conn, 'SentimentTweets')){
    dbRemoveTable(db.conn, 'SentimentTweets')
  }
  
  # Write to database
  dbWriteTable(db.conn, 'SentimentTweets', tweet.sent, row.names=FALSE)
  
### Convert the Beats data into an R object ----------------------------------------------  
  
  # Read in the Police Beats Data
  beats <- st_read(file.path(data.dir, 'beats', 'SPD_BEATS_WGS84.shp'), 
                   quiet=TRUE)
  
  # Transform the Coordinate Reference System  
  beats <- st_transform(beats, crs=4326)
  
  # Convert beats shapefile from 'sf' to 'sp'
  beats.sp <- as(beats, 'Spatial')
  
  # Add id and beat numbers
  beats.sp@data$id <- paste0("ID", 1:nrow(beats.sp@data))
  
  # Convert to a fortified object (data.frame)
  beats.spf <- broom::tidy(beats.sp)
  beats.spf$beat <- beats.sp@data$beat[match(beats.spf$id, 
                                             beats.sp@data$id)]
  
  # Save all beats shapefiles as an R object for loading later
  save(beats, beats.sp, beats.spf, 
       file= file.path(data.dir, 'geographic', 'beats.Rdata'))

### End session --------------------------------------------------------------------------  
  
 ## Close connection to database
  
  dbDisconnect(db.conn)
  
##########################################################################################
##########################################################################################
  
  
  
  
  