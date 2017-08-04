##########################################################################################
#                                                                                        #            
#  Code for Chapter 7 (Preparation) of Real Estate Analysis in the Information Age       #   
#                                                                                        #                  
##########################################################################################

### Preliminary Commands -----------------------------------------------------------------

 ## Load Libraries

  library(sf)
  library(sp)
  library(tidyverse)
  library(RSQLite)
  library(RODBC)
  library(geosphere)
  library(rgeos)
  
 ## Set data and code directory
 
  #data.dir <- 'c:/dropbox/research/bigdatabook/data/'
  data.dir <- 'c:/temp/'
  code.dir <- 'c:/code/research/REAIA_book/'
  
 ## Load custom source files
  
  source(paste0(code.dir, 'custom_functions.R'))  
  
 ## Set the database path and name  
  
  data.db <- file.path(data.dir, 'seattleCaseStudy.db')

### Initial clean of sales to eliminate non-relevant observations ------------------------ 

  # Read in Sales File
  db.conn <- dbConnect(dbDriver('SQLite'), data.db)
  sales.data <- dbReadTable(db.conn, 'Sales')
  
 ## Transform and Filtering Activities for Sales (Interspersed) 
  
  # Filter those missing PIN numbers or Price
  sales.data <- dplyr::filter(sales.data, Major > 0)
  sales.data <- dplyr::filter(sales.data, SalePrice > 0)
  
  # Mutate new document date field
  sales.data <- dplyr::mutate(.data=sales.data,
                              doc.date = paste0(substr(DocumentDate, 4, 5),
                                                 substr(DocumentDate, 1, 2),
                                                 substr(DocumentDate, 7, 10)))
  # Mutate new sale date field
  sales.data <- dplyr::mutate(.data=sales.data,
                              sales.date = as.Date(doc.date, "%d%m%Y"))
  
  # Mutate new sales year field
  sales.data <- dplyr::mutate(.data=sales.data,
                              sales.year = as.numeric(format(sales.date, "%Y")))
  
  # Remove document date fields
  sales.data <- dplyr::mutate(.data=sales.data,
                              doc.date = NULL)
  sales.data <- dplyr::mutate(.data=sales.data,
                              DocumentDate = NULL)
  
  # Filter those missing a sales date
  sales.data <- dplyr::filter(.data=sales.data,
                              !is.na(sales.date))
  
  # Filter sales based on study relevancy (time period)
  sales.data <- dplyr::filter(.data=sales.data,
                              sales.year == 2016)
  
  # Mutate: Add qtr and month of sale
  sales.data$month <- as.numeric(substr(sales.data$sales.date, 6, 7))
  sales.data$qtr <- ((sales.data$month - 1) %/% 3) + 1
  
  # Mutate Major and Minor into a PIN identifier (custom function)
  sales.data <- buildPinx(X=sales.data)
  
  # Mutate & Filter: Add trans count and limit by parameter (custom function)
  sales.data <- buildTransCount(x.sales=sales.data, 
                                trans.limit=5)
  
  # Mutate: Add MultiParcel sale designation (custom function)
  sales.data <- idDup(x.data=sales.data, 
                      x.field='ExciseTaxNbr', 
                      new.field = 'multi.parcel',
                      idd.type='labelNonUnique', 
                      bin.nonuq=TRUE)
  
  # Filter those with multiparcel
  sales.data <- dplyr::filter(.data=sales.data,
                              multi.parcel == 0)
  
  # Mutate a new sale id after filters (custom function)
  sales.data <- buildSaleUIDs(sales.data) 
  
  # Filter by Sale Insturment, Reason and Warning
  
  # Mutate the "Warning" Field.  
  #Add a leading/trailing space for the grep()
  
  sales.data <- dplyr::mutate(sales.data,
                              SaleWarning = paste0(" ", SaleWarning, " "))

  # Create a list of factors to eliminate  
  trim.list <- list(SaleReason=2:19,  
                    SaleInstrument=c(0, 1, 4:28),
                    SaleWarning=paste0(" ", c(1:2, 5:9, 11:14, 18:23, 25, 27,
                                              31:33, 37, 39, 43, 46, 48, 49,
                                              50:53, 59, 61, 63, 64, 66), " "))
  
  # Loop through each factor type and filter accordingly (custom function)
  for(tL in 1:length(trim.list)){
    sales.data <- trimByField(x.data=sales.data, 
                              x.field=names(trim.list)[tL],
                              trim.list = unlist(trim.list[tL]))
  }
  
  # Transform:  Limit field names
  sales.data <- dplyr::select(.data=sales.data, pinx, rec.ID, sale.ID, SalePrice, 
                              sales.date, sales.year, month, qtr)
  
  
  sales.data <- dplyr::rename(.data=sales.data, sale.price=SalePrice)
  
### Integrate Sale Record and Use Type------------------------------------------------------
  
 ## Read in Data
  
  # Parcel Data
  parcel.data <- dbReadTable(db.conn, 'parcel')
  
  # Mutate Pinx number
  parcel.data <- buildPinx(parcel.data)
  
  # Res Building data
  resbldg.data <- dbReadTable(db.conn, 'resbldg')
  
  # Mutate pinx number
  resbldg.data <- buildPinx(resbldg.data)
  
  # Integrate the record type (Whether or not Residential) (Labeling)
  sales.data$res.record <- resbldg.data$BldgNbr[match(sales.data$pinx,
                                                      resbldg.data$pinx)]
  
  # Mutate the integrated field to binary
  sales.data$res.record <- ifelse(is.na(sales.data$res.record), 
                                  0, 
                                  sales.data$res.record)
  
  # Filter those with non-residential record type or with more than one dwelling on it  
  sales.data <- dplyr::filter(.data=sales.data,
                              res.record == 1)
  
  # Mutate:  Remove Res.record field
  sales.data <- dplyr::mutate(.data=sales.data, res.record = NULL)
  
  
  # Integrate property use field
  sales.data$present.use <- parcel.data$PresentUse[match(sales.data$pinx,
                                                   parcel.data$pinx)]
  
  # Filter those with no present use
  sales.data <- dplyr::filter(.data=sales.data,
                              !is.na(present.use))
  
  # Filter those not with SFR or Townhome use category
  sales.data <- dplyr::filter(.data=sales.data,
                              present.use == 2 | present.use == 29)
  
### Integrate Sales data with assessor data ----------------------------------------------  

 ## Transform the assessor data
  
  # Parcel Data
  
  # Transform:  Remove unneccessary fields
  parcel.data <- parcel.data[,c('pinx', 'CurrentZoning', 'SqFtLot', 
                                'Topography', 'RestrictiveSzShape', 
                                'MtRainier', 'Olympics', 'Cascades', 
                                'Territorial', 'SeattleSkyline', 'PugetSound',
                                'LakeWashington', 'LakeSammamish', 
                                'SmallLakeRiverCreek', 'OtherView',
                                'WfntLocation', 'WfntFootage', 'WfntBank',
                                'TrafficNoise')]
  
  # Transform: Change field names
  names(parcel.data) <- c('pinx', 'zoning', 'lot.size', 'topo', 'restr.szshp',
                          'view.rainier', 'view.olympics', 'view.cascades',
                          'view.terr', 'view.city', 'view.puget', 
                          'view.lkwash', 'view.lksamm', 'view.smwater',
                          'view.other', 'wfnt', 'wfnt.ftg', 'wfnt.bank',
                          'traffic.noise')
  
  # Res Building Data
  
  # Transform: Remove unnecessary fields 
  resbldg.data <- resbldg.data[,c('pinx', 'NbrLivingUnits', 'BldgNbr', 
                                  'Stories', 'BldgGrade', 'SqFtTotLiving',
                                  'SqFtFinBasement',
                                  'SqFtGarageBasement', 'SqFtGarageAttached',
                                  'SqFtDeck', 'Bedrooms', 'BathHalfCount', 
                                  'Bath3qtrCount', 'BathFullCount', 
                                  'YrBuilt',
                                  'YrRenovated', 'Condition')]
  
  # Transform: Change field names
  names(resbldg.data) <- c('pinx', 'nbr.lu', 'bldg.nbr', 'stories',
                           'bldg.grade', 'tot.sf', 'bsmt.sf',
                           'gar.bsmt.sf', 'gar.att.sf', 'deck.sf',
                           'beds', 'bath.half', 'bath.75', 'bath.full',
                           'yr.built', 'yr.ren', 'condition')
  
  # Filter: remove multi-structure sites from resbldg
  
  resbldg.data <- dplyr::arrange(.data=resbldg.data,
                                bldg.nbr)
  resbldg.data <- dplyr::filter(.data=resbldg.data,
                                !duplicated(pinx))
  resbldg.data <- dplyr::filter(.data=resbldg.data,
                                bldg.nbr == 1)
  
  # Remove building number field
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                 bldg.nbr = NULL)
  
  # Mutate: Create total bathrooms
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                baths = bath.full + bath.75 * .75 + bath.half * .5)
  
  # Remove partial bath fields
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                bath.full = NULL)
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                bath.75 = NULL)
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                bath.half = NULL)
  
 ## Integrate Assessor data and sales data
  
  # Join parcel data to sales (inner join)
  sales.data <- dplyr::inner_join(x=sales.data, 
                                  y=parcel.data, 
                                  by='pinx')
 
  # Join res bldg data to sales (inner join)
  sales.data <- dplyr::inner_join(x=sales.data, 
                                  y=resbldg.data, 
                                  by='pinx')

  # Mutate: Create Age variable
  sales.data <- dplyr::mutate(.data=sales.data,
                              age=sales.year - yr.built)
  
  # Filter: Remove those built after the sale
  sales.data <- dplyr::filter(.data=sales.data,
                              age >= 0)
  
  # Remove Yr.Built field
  sales.data <- dplyr::mutate(.data=sales.data,
                              yr.built=NULL)
  
  # Create years since renovation
  sales.data <- dplyr::mutate(.data=sales.data,
                              age.renov = sales.year - yr.ren)
  
  # Filter: Remove those renovated after the sale
  sales.data <- dplyr::filter(.data=sales.data,
                              age.renov >= 0)
  
  # Mutate: Create effective age field
  sales.data$eff.age <- ifelse(sales.data$age.renov < sales.data$age,
                               sales.data$age.renov,
                               sales.data$age)

  # Remove Age.renov field
  sales.data <- dplyr::mutate(.data=sales.data,
                              age.renov=NULL)
  
  # Remove Yr.Ren field
  sales.data <- dplyr::mutate(.data=sales.data,
                              yr.ren=NULL)
  
  # Filter: Remove those with more than one land use
  sales.data <- dplyr::filter(.data=sales.data,
                              nbr.lu == 1)
  
  # Remove Nbr.lu field
  sales.data <- dplyr::mutate(.data=sales.data,
                              nbr.lu=NULL)
  
### Integrate the geospatial data (parcel and beat) with the sales -----------------------  
  
 ## Prepare Centroids Data  
  
  # Load Data
  load(file= file.path(data.dir, 'geographic/parcelcentroids.Rdata'))
  
  # Mutate:  Add pinx field for joining
  parcel.centroids$pinx <- paste0('..', parcel.centroids$PIN)
  
  # Remove unnecessary fields
  parcel.centroids$PIN <- NULL
  
  # Filter: Limit to those parcels in the sale dataset
  parcel.centroids <- dplyr::filter(parcel.centroids, pinx %in% sales.data$pinx)

 ## Prepare the Police Beats file

 # Load Data
  load(file=file.path(data.dir, 'geographic/beats.Rdata'))
  
 # Filter: Remove water precincts  
  beats <- dplyr::filter(beats, first_prec != '')
  
  # Add geographic area to the beats data
  
  # Extract area measurement from the shapefile
  beat.polys <- beats.sp@polygons
  poly.area <- unlist(lapply(beat.polys, function(x) x@area))
  
  # Convert to square miles
  poly.area <- poly.area * (68.99 ^ 2)
  
  # Add to data
  beats.sp@data$size <- poly.area
  
  # Save all beats shapefiles as an R object for loading later
  save(beats, beats.sp, beats.spf, 
       file= file.path(data.dir, 'geographic/beats.Rdata'))
  
## Integrate the Beat Identification to the sales
  
  # Transform:  Set up null field
  parcel.centroids$beat <- 'NONE'
  
  # Integrate: Perform geospatial intersection (spatial join)
  beats.overlay <- st_intersects(beats, parcel.centroids)
  
  # Transform: Extract intersection and add to parcel centroids
  for(i in 1:length(beats.overlay)){
    
    ov.id <- beats.overlay[[i]]
    parcel.centroids$beat[ov.id] <- as.character(beats$beat[i])

  }
  
  # Filter: Remove sales not in Beat Precincts
  parcel.centroids <- dplyr::filter(parcel.centroids, beat != "NONE")
  
 ## Add location data to sales  
  
  # Transform: Create Separate Lat/long Columns 
  parcel.centroids$longitude <- unlist(lapply(parcel.centroids$parcel.centroids, function(x) x[1]))
  parcel.centroids$latitude <- unlist(lapply(parcel.centroids$parcel.centroids, function(x) x[2]))
  
  # Save Data
  save(parcel.centroids, 
       file= file.path(data.dir, 'geographic/parcelcentroids_sales.Rdata'))
  
  
  # Integrate: Join data
  sales.data <- merge(sales.data, 
                      parcel.centroids[ , c('pinx', 'beat', 'longitude', 'latitude')],
                      by='pinx')
  
  # Transform:  Reclassify the zoning variable
  sales.data$zoning[grep('LR1', sales.data$zoning)] <- 'LR1'
  sales.data$zoning[grep('LR2', sales.data$zoning)] <- 'LR2'
  sales.data$zoning[grep('LR3', sales.data$zoning)] <- 'LR3'
  sales.data$zoning[grep('NC', sales.data$zoning)] <- 'Comm'
  sales.data$zoning[grep('C1', sales.data$zoning)] <- 'Comm'
  sales.data$zoning[grep('MR', sales.data$zoning)] <- 'Other'
  sales.data$zoning[grep('RSL', sales.data$zoning)] <- 'Other'
  sales.data$zoning[grep('NR', sales.data$zoning)] <- 'Other'
  
### Crime Data ---------------------------------------------------------------------------  
  
 ## Read in Data
  
  crime.data <- dbReadTable(db.conn, 'Crime')
  
 ## Transform date and time fields  
  
  # Extract date only from time/date field
  crime.data <- dplyr::mutate(.data=crime.data,
                              crime.date = as.Date(substr(
                                occurred.date.or.date.range.start, 1, 10), '%m/%d/%Y'))
                              
  # Extract time and convert to 24 hour scale                            
  crime.data <- dplyr::mutate(.data=crime.data,
                              crime.time = substr(
                                occurred.date.or.date.range.start, 12, 20))
  
  crime.data$crime.time <- as.numeric(substr(crime.data$crime.time,1,2)) + 
                         as.numeric(substr(crime.data$crime.time,4,5))/60
  
  crime.data$crime.time <- ifelse(substr(crime.data$occurred.date.or.date.range.start, 
                                   21, 21) == 'A', 
                            crime.data$crime.time,
                            crime.data$crime.time + 12)
  
  crime.data$crime.time <- ifelse(crime.data$crime.time >=24, 
                            crime.data$crime.time - 24, 
                            crime.data$crime.time)
  
 ## Recategorize crime
  
  # Create Crime Groups
  violent <- c('ASSAULT', 'HOMICIDE', 'ROBBERY', 'WEAPON')
  property <- c('BIKE THEFT', 'BURGLARY', 'CAR PROWL', 'ILLEGAL DUMPING', 'MAIL THEFT',
                'OTHER PROPERTY', 'PICKPOCKET', 'PROPERTY DAMAGE', 'PURSE SNATCH', 
                'SHOPLIFTING', 'STOLEN PROPERTY', 'THEFT OF SERVICES', 'VEHICLE THEFT')
  behavior <- c('DISPUTE', 'DISORDERLY CONDUCT', 'DISTURBANCE', 'LIQUOR VIOLATION', 
                'LOITERING', 'NARCOTICS', 'PROSTITUTION', 'PUBLIC NUISANCE', 'THREATS',
                'TRESPASS')
  traffic <- c('TRAFFIC', 'DUI')
  
  # Assign new groups
  crime.data$crime.type <- 'other'
  crime.data$crime.type[crime.data$summarized.offense.description %in% 
                          violent] <- 'violent'
  crime.data$crime.type[crime.data$summarized.offense.description %in% 
                          property] <- 'property'
  crime.data$crime.type[crime.data$summarized.offense.description %in% 
                          behavior] <- 'behavior'
  crime.data$crime.type[crime.data$summarized.offense.description %in% 
                          traffic] <- 'traffic'
 
  # Convert sales.date to character to write to database
  crime.data <- dplyr::mutate(.data=crime.data,
                              crime.date = as.character(crime.date))  
  
  
 ## Integrate crime and sales data -------------------------------------------------------  
  
  # Fix the date field
  crime.data$crime.date <- as.Date(crime.data$crime.date)
  
  # Limite crime data to 2015 or later  
  crime.data <- crime.data[crime.data$year >= 2015, ]
  
  # Set distance threshold in Meters
  dist.thres <- 400
  
  # Set blank values
  sales.data$crime.violent <- 0
  sales.data$crime.property <- 0
  sales.data$crime.traffic <- 0
  sales.data$crime.behavior <- 0
  sales.data$crime.other <- 0
  
  # Loop through each and calculate local crime counts
  for(j in 1:nrow(sales.data)){
    
    # Extract sales data
    j.data <- sales.data[j,]
    
    # Calculate time difference
    x.days <- j.data$sales.date - crime.data$crime.date
    
    # Limit crime data to time window
    c.data <- crime.data[x.days > 0 & x.days < 365, ]
    
    # Calculate distances
    j.dist <- distHaversine(j.data[,c('longitude', 'latitude')],
                            c.data[,c('longitude', 'latitude')])
    
    # Limit data to those within threshold
    cx.data <- c.data[j.dist < dist.thres, ]
    
    # Add count to the sales data
    sales.data$crime.violent[j] <- length(which(cx.data$crime.type == 'violent'))
    sales.data$crime.property[j] <- length(which(cx.data$crime.type == 'property'))
    sales.data$crime.behavior[j] <- length(which(cx.data$crime.type == 'behavior'))
    sales.data$crime.traffic[j] <- length(which(cx.data$crime.type == 'traffic'))
    sales.data$crime.other[j] <- length(which(cx.data$crime.type == 'other'))
    
    # Report on progress
    if(j %% 100 == 0){
      cat('record number ', j, '\n')
    }
  }
  
### Prepare sentiment analyses
 
  tweet.data <- dbReadTable(db.conn, 'SentimentTweets')
  
 ## Limit to Tweets in the city
  
  # Create city boundary
  seattle.bound <- gUnaryUnion(beats.sp)
  
  # Convert tweet to spatial poing data frame
  tweet.sp <- SpatialPointsDataFrame(cbind(tweet.data$longitude, tweet.data$latitude),
                                     data=tweet.data)
  proj4string(tweet.sp) <- CRS(proj4string(beats.sp))
  
  # Clip by city boundary
  in.seattle <- gIntersects(tweet.sp, seattle.bound, byid=T)
  tweet.sp <- tweet.sp[which(in.seattle), ]
  
  ## Create a dataset with only tweet with a non-zero sentiment
  
  # Make dataset
  twsent.sp <- tweet.sp[tweet.sp@data$SentimentScore != 0, ]
  
  # Convert all tweets to -1 or 1
  twsent.sp@data$SS <- ifelse(twsent.sp@data$SentimentScore < 0, -1, 1)
  
  # Create a simple DF
  twsent <- twsent.sp@data
  
##########################################################################################  
  
  # Convert sales.date to character to write to database
  sales.data <- dplyr::mutate(.data=sales.data,
                              sales.date = as.character(sales.date))  
  
  # Convert sales.date to character to write to database
  crime.data <- dplyr::mutate(.data=crime.data,
                              crime.date = as.character(crime.date))  
  
  # Write to the database
  dbWriteTable(db.conn, 'prepSales', sales.data, row.names=FALSE, overwrite=TRUE)
  
  # Write out to database
  if(dbExistsTable(db.conn, 'Crime')){
    dbRemoveTable(db.conn, 'Crime')
  }
  dbWriteTable(db.conn, 'Crime', crime.data, row.names=FALSE)
  
  # Write out to database
  if(dbExistsTable(db.conn, 'SentimentTweets')){
    dbRemoveTable(db.conn, 'SentimentTweets')
  }
  dbWriteTable(db.conn, 'SentimentTweets', twsent, row.names=FALSE)
  
  # Close
  dbDisconnect(db.conn)
  
##########################################################################################  
  
  
  
  
  
  
  
  
##########################################################################################
##########################################################################################