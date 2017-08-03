Overview
--------

The examples and the case studies presented in this book are conducted in the R computing language.[1] All data and code necessary to reproduce the cast studies are provided on this website or are downloadable free of charge from the internet. The code for each chapter (found at <https://github.com/REAIABook/REAIABook>) can be run to reproduce the examples and analyses that are found throughout the book.

Before you start, you need to do two things.

1.  Download R from <https://cran.r-project.org/bin/windows/base/> and install.
2.  Download RStudio from <https://www.rstudio.com/products/rstudio/download/.> and install.
    Note that RStudio is not necessary but it is a helpful interactive development environment (IDE) that will make working with the provided R code easier. The code below was built in 64-bit R, version 3.4.0.

Once you have opened this can you move on to the code from the individual chapters. Do note that in many cases, code from one chapter depends on code and analyses performed in a previous chapter. You should also set aside a directory on your computer where you can save the raw and intermediate data as well as the outputs from the graphing and modeling exercises. You will need to manually input this directory into the code for each chapter starting in chapter 5. The code is written into modules by chapter, with the intent that each can be executed independently and as a result preliminary or setup commands are entered at the beginning of each chapter's code.

Finally, please note that the code below has been written with the aim of readability and exposure to different approaches to solving the same tasks and not with the sole aim of efficiency. Please do issue pull requests for errors (which are likely) but not for improvements to code efficiency.

Introduction
------------

How to get the code and data from this site:

There are two primary ways to get the code and data from this site.

1.  If you don't have a Github account you can click on the green "Clone or Download" button and download a .ZIP file and extract the files on your computer.

2.  If you have a Github account, download and install the Github desktop application. Make sure you are logged in in the application. Then click the green "Clone or Download" button and select "Open in Desktop" and follow the instructions to clone (copy) the files to your computer.

Also you should allocate a directory for the data that will be downloaded in the code that has at least 2 GB of open space. This should be used as the data dirctory (**data.dir**) in the code.

Chapter 1
---------

No code

Chapter 2
---------

No code

Chapter 3
---------

In chapter 3, we install the necessary packages that we will use throughout this book. You can either copy the code below directly into the 'console' window of RStudio or you can open the *software.r* file and execute it in its entirety with **cntl-enter**. If asked to choose a mirror for downloading the packages, any will do but the one closest to you is likely to be fastest.

``` r
  install.packages('tidyverse')
  install.packages('sf')
  install.packages('sp')
  install.packages('maptools')
  install.packages('rgeos')
  install.packages('spgwr')
  install.packages('spdep')
  install.packages('geosphere')
  install.packages('OpenStreetMap')
  install.packages('gstat')
  install.packages('RODBC')
  install.packages('RSQLite')
  install.packages('BMA')
  install.packages('MASS')
  install.packages('car')
  install.packages('lmtest')
  install.packages('plm')
  install.packages('scales')
```

After you have installed the above libraries you can move on to the code for chapter 5.

Chapter 4
---------

No Code

Chapter 5
---------

In chapter 5 we gather data from a variety of sources for the two case studies that will run in parallel to this book. The code below will download and unzip the necessary data to reproduce the case studies used in this book. This data will also be used to create the example plots and tables throughout the book. The user will have to specify a particular directory in which the data will be located. A code directory (the directory in which you downloaded the code from the Introduction chapter) will also need to be specified.

Please note that, depending on your internet speed, the initial downloading of the files may take some time. Note that once the raw files are downloaded, the code will recognize their existance and not download them again if you run the code a second time, so long as you do not move the files from their downloaded location. Also, you will need at least 2.5 GB of space[2] in the directory that you indicate in the **data.dir** parameter below.

### Gathering the Data

Before executing the code file *gather.R*, the user must specify the directory into which the downloaded data will be stored, unzipped, etc. In the example below we have used **c:/temp/** as the directory. You should delete this and use a directory of your choice.

``` r
  data.dir <- 'c:/temp/'
```

Next, we check to see if a sub-directory to store the raw zip files exists. If not, we create one called *raw\_zip\_files*.

``` r
  if (!dir.exists(file.path(data.dir, 'raw_zip_files'))){
    dir.create(file.path(data.dir, 'raw_zip_files'))
  }
```

We now check to see if the GIS shapefile of the Seattle Police Department Beat Districts has already been downloaded. If not, we download it from the internet.

``` r
  # Check if file exists
  if (!file.exists(file.path(data.dir, 'raw_zip_files', 'beats.zip'))){
  
    # If it doesn't, then download
    download.file(url=paste0('https://data.seattle.gov/views/nnxn-434b/files/',
                         '96d998d4-ae20-4ea8-b912-436e68982a0d.zip'), 
                  destfile=file.path(data.dir, 'raw_zip_files', 'beats.zip'))
  }  
```

We then check to see if a separate directory for the beats data exists. If it doesn't we create it. We then unzip the **beats.zip** file into the new *beats* directory.

``` r
  # Create a directory for beats data if one doesn't exist
  if (!dir.exists(file.path(data.dir, 'beats'))){
    dir.create(file.path(data.dir, 'beats'))
  }
  
  # Unzip the files
  unzip(zipfile=file.path(data.dir, 'raw_zip_files', 'beats.zip'),
        exdir=file.path(data.dir, 'beats'))
```

Next, we download the property parcel (cadastre) shapefile (if not done so in the past). We then create a separate sub-directory for the geographic data (if not already present) and then and unzip it into the *geographic* sub-directory.

``` r
  # Check if file exists
  if(!file.exists(file.path(data.dir, 'raw_zip_files', 'parcel_shapefile.zip'))){
    
    # If it doesn't, then download
    download.file(url=paste0('ftp://ftp.kingcounty.gov/gis-web/web/',
                             'GISData/parcel_SHP.zip'), 
                  destfile=file.path(data.dir, 'raw_zip_files', 'parcel_shapefile.zip'))
  }

  # Create a directory if one doesn't exist
  if (!dir.exists(file.path(data.dir, 'geographic'))){
    dir.create(file.path(data.dir, 'geographic'))
  }
  
  # Unzip the files
  unzip(zipfile=file.path(data.dir, 'raw_zip_files', 'parcel_shapefile.zip'), 
        exdir=file.path(data.dir, 'geographic'))
```

Finally, we gather the county assessor data. This includes sales transactions, information about the land parcel and information about the residential buildings. Before we download any data we create a sub-directory called *assessor* if one doesn't already exist.

``` r
  # Create a directory if one doesn't exist
  if(!dir.exists(file.path(data.dir, 'assessor'))){
    dir.create(file.path(data.dir, 'assessor'))
  }
```

We start by downloading the sales transaction file, if it does not already exist. Once downloaded, it is unzipped into the *assessor* sub-directory.

``` r
  # Check if file exists
  if (!file.exists(file.path(data.dir, 'raw_zip_files', 'sales.zip'))){
    
    # If it doesn't, then download
    download.file(url=paste0('http://your.kingcounty.gov/extranet/assessor/',
                             'Real Property Sales.zip'), 
                  destfile=file.path(data.dir, 'raw_zip_files', 'sales.zip'))
  }
  
  # Unzip
  unzip(zipfile=file.path(data.dir, 'raw_zip_files', 'sales.zip'), 
        exdir=file.path(data.dir, 'assessor'))
```

We then do the same for the parcel and the residential building (resbldg) information.

``` r
 ## Parcel Tabular File

  # Check if file exists
  if (!file.exists(file.path(data.dir, 'raw_zip.files', 'parcel.zip'))){
    
    # If it doesn't, then download
    download.file(url='http://your.kingcounty.gov/extranet/assessor/Parcel.zip', 
                  destfile=file.path(data.dir, 'raw_zip_files', 'parcel.zip'))
  }
  
  # Unzip
  unzip(zipfile=file.path(data.dir, 'raw_zip_files', 'parcel.zip'), 
        exdir=file.path(data.dir, 'assessor'))
```

``` r
 ## Residential Building File  
  
  # Check if file exists
  if (!file.exists(file.path(data.dir, 'raw_zip_files', 'resbldg.zip'))){
    
    # If it doesn't, then download
    download.file(url=paste0('http://your.kingcounty.gov/extranet/assessor/',
                             'Residential Building.zip'), 
                  destfile=file.path(data.dir, 'raw_zip_files', 'resbldg.zip'))
  }
    
  # Unzip
  unzip(zipfile=file.path(data.dir, 'raw_zip_files', 'resbldg.zip'), 
        exdir=file.path(data.dir, 'assessor'))
```

We then add the crime report statisitics from the City of Seattle, downloading them into the 'crime' directory.

``` r
  # Create a directory for assessor data if one doesn't exist
  if (!dir.exists(file.path(data.dir, 'crime'))){
    dir.create(file.path(data.dir, 'crime'))
  }
  
  # Download .csv from the City of Seattle
  download.file(url=paste0('https://data.seattle.gov/api/views/7ais-f98f/rows.csv?',
                           'accessType=DOWNLOAD'), 
                destfile=file.path(data.dir, 'crime', 'seattle_crime.csv' ))
```

And finally, we download the pre-analyzed twitter sentiment data from the REAIABook Github site and place in the 'tweets' directory.

``` r
  # Create a directory for assessor data if one doesn't exist
  if (!dir.exists(file.path(data.dir, 'tweets'))){
    dir.create(file.path(data.dir, 'tweets'))
  }
  
  # Download .csv from the City of Seattle
  download.file(url=paste0('http://raw.githubusercontent.com/REAIABook/',
                           'REAIABook/master/tweetSentiment.csv'), 
                destfile=file.path(data.dir, 'tweets', 'sentimenttweets.csv' ))
```

Chapter 6
---------

Chapter 6 discusses data management. In the code below we move the raw, downloaded tabular data into a database format, adding unique identifiers in the process. For the geo-spatial data we convert the shapefiles to an R data objects and re-project the coordinates to match a standard coordinate reference system format.

We begin the process by loading a number of packages or libraries that will be needed to complete this process. These packages were installed on your machine with the code from Chapter 3 above. These packages are developed by third-party users to augment the R language and represent one of the great benefits of using R.

``` r
  library(RSQLite)
  library(RODBC)
  library(tidyverse)
  library(maptools)
  library(sf)
  library(sp)
```

Next, the user must enter the directory where the data is stored, **data.dir** and the directory where the code has been downloaded, **code.dir**. Both directories should be identical to the directories entered in Chapter 5.

``` r
  data.dir <- 'c:/temp/'             # For Example
  code.dir <- 'c:/code/REAIA_Book/'  # For Example
```

We load in a set of custom functions that have been developed to help with the data integration and cleaning process.

``` r
 source(file.path(code.dir, 'custom_functions.R'))
```

We also create a name and path for the database into which we'll integrate all of the data.

``` r
  data.db <- file.path(data.dir, 'seattleCaseStudy.db')
```

### Manage Assessors data

To better manage the various assessor's data files -- the sales, parcel and residential building information -- we combine them into a single SQLite database. This also allows for greater portability of the files. This same database will contain the various iterations of the cleaned sales data as well. The **convertCSVtoSQLite()** function is a custom function that we have developed for the task of combining various CSV files into a single SQLite database. If you don't want to see the progress of the process you can change the *verbose* argument to FALSE.

``` r
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
```

#### Sales data unique identifiers

In this section we create a set of unique identifiers for the sales data.

We start by opening a connection to the SQL database that we created above and reading in the raw sales information.

``` r
  # Set connection
  db.conn <- dbConnect(dbDriver('SQLite'), data.db)
  
  # Read in sales data
  sales.data <- dbReadTable(db.conn, 'Sales')
```

We then use a multiple step process to create a unique identifer field, based on the excise tax affidavit number. We begin by creating a new field called **UID** that contains the excise tax number. We then re-order the columns to put this value on the far left and then sort the data by the **UID** field.

``` r
  # Make new UID from ExciseTaxNbr
  sales.data <- dplyr::mutate(sales.data, UID=ExciseTaxNbr)
  
  # Place Excise Tax Number on far left
  sales.data <- dplyr::select(sales.data, UID, everything())

  # Order by that field
  sales.data <- dplyr::arrange(sales.data, UID)
```

As some of these numbers are duplicates due to multiple parcel (or property) sales, we must do a little more work to make this a unique identifier. First, we create a separate data frame of those excise tax numbers that appear more than once.

``` r
  uid.mult <- sales.data %>% 
    dplyr::group_by(UID) %>%
      dplyr::summarize(rec.count=n()) %>%
       dplyr::filter(rec.count > 1)
```

Second, we create a vector showing the count or number of times each of the duplicated UIDs appear.

``` r
  # Develop a list full of proper suffixes
  uid.suffix <- lapply(split(uid.mult, uid.mult$UID), 
                       function(x) 1:(x$rec.count))
  names(uid.suffix) <- NULL
```

We then add these counts (suffix) to the sales data. Those with a UID that is unique receive a 0. Finally, we add the suffix value to the original UID to make an actual unique identifier. The suffix field is removed.

``` r
  # Apply suffix to sales.data
  sales.data$uid.suffix <- 0
  sales.data$uid.suffix[sales.data$UID %in% uid.mult$UID] <- unlist(uid.suffix)
  
  # Concatenate to make an unique ID
  sales.data$UID <- ifelse(sales.data$uid.suffix == 0,
                           sales.data$UID,
                           paste0(sales.data$UID, '..', sales.data$uid.suffix))
  
  # Remove uid.suffix field
  sales.data$uid.suffix <- NULL
```

We write this data back to the database, removing the old table first.

``` r
  dbRemoveTable(db.conn, 'Sales')
  dbWriteTable(db.conn, 'Sales', sales.data, row.names=FALSE)
```

#### Parcel and res bldg data unique identifiers

Here we add unique identifiers to the King County Assessors parcel and residential building tabular data. The unique identifiers (**pinx**) are simply an extension of the County's parcel identification numbers (PINs) where we add two leading '.'s in order to avoid dropped leading zeros in future uses. A custom function call **buildPinx()** is used to accomplish this. Full code for the **buildPinx()** function can be found in the *custom\_functions.R* file.

We start by reading the raw parcel tabular data, creating the **pinx** unique identifier and then writing it back to the database.

``` r
  # Read in data
  parcel.data <- dbReadTable(db.conn, 'Parcel')
  
  # Add unique pinx field
  parcel.data <- buildPinx(parcel.data)
  
  # Write out
  dbRemoveTable(db.conn, 'Parcel')
  dbWriteTable(db.conn, 'Parcel', parcel.data, row.names=FALSE)
```

We then do the same with the residential building tabular data. As some properties have more than one residential building a suffix is added here to indicate which building it is.

``` r
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
```

As we have loaded a number of large datasets, we clean our our working RAM here.

``` r
  rm(resbldg.data); rm(parcel.data); rm(sales.data)
  gc()
```

#### Parcel Shapefile conversion

We now load in the parcel shapefile and convert it from a polygon to a point coverage. We also change the coordinate reference system.

We start by loading the parcel shapefile from the King County GIS department into R as a simple features object (sf).

``` r
  parcels <- st_read(file.path(data.dir, 'geographic', 'parcel', 'parcel.shp'), 
                     quiet=TRUE)
```

We then transform the coordinate reference system to match the other spatial data we will use later.

``` r
  parcels <- st_transform(parcels, 4326)
```

We then extract the centroid of each parcel polygon and conver this to a simple features data frame. We add the parcel identification number to each point as the unique identifier.

``` r
  # Extract centroid Lat longs
  parcel.centroids <- st_centroid(parcels)
  
  # Convert to a simple features data.frame
  parcel.centroids <- st_sf(parcel.centroids)
  
  # Add PIN values
  parcel.centroids$PIN <- parcels$PIN
```

Finally, we save this object as an .Rdata file for faster loading in the future.

``` r
  save(parcel.centroids, file=file.path(data.dir, 'geographic', 'parcelcentroids.Rdata'))
```

#### Add in Crime Beat Data

Now we load in the crime data and add it to the database.

First we load the crime data from the .csv file downloaded from the City of Seattle.

``` r
  crime.data <- read.csv(file.path(data.dir, 'crime', 'seattle_crime.csv'))
```

We then convert all of the field names to lower case and rename the unique identifier field to a simpler name, uid.

``` r
  # Convert to lower case
  names(crime.data) <- tolower(names(crime.data))
  
  # Rename unique identifier
  names(crime.data)[1] <- 'uid'
```

We then write these data to the database, removing the existing table if necessary.

``` r
  # Check if exists and remove
  if(dbExistsTable(db.conn, 'Crime')){
    dbRemoveTable(db.conn, 'Crime')
  }

  # Write to database
  dbWriteTable(db.conn, 'Crime', crime.data, row.names=FALSE)
```

#### Add Twitter sentiment data to database

We now add the twitter sentiment data (available from the REAIA Github site) to the database.

First we load in the sentiment scored tweets directly from the Github site.

``` r
 # Read in tweet sentiment data  
  tweet.sent <- read.csv(file.path(data.dir, 'tweets', 'sentimenttweets.csv'),
                         header=TRUE)
  
  # Remove if exists 
  if (dbExistsTable(db.conn, 'SentimentTweets')){
    dbRemoveTable(db.conn, 'SentimentTweets')
  }
  
  # Write to database
  dbWriteTable(db.conn, 'SentimentTweets', tweet.sent, row.names=FALSE)
```

#### Police Beat Spatial Data

Finally, we manage the Seattle Police Beat spatial data.

We begin by reading in the police beat boundaries as a simple features polygon object.

``` r
  beats <- st_read(file.path(data.dir, 'beats', 'SPD_BEATS_WGS84.shp'), 
                   quiet=TRUE)
```

We then transform the coordinate reference system.

``` r
  beats <- st_transform(beats, 4326)
```

As we will work with other spatial packages in later analyses, we also convert simple features (sf) object to a an *sp* object (an earlier form of spatial handling in R). To this, we add the beat numbers as a unique identifier.

``` r
  # Convert to sp object
  beats.sp <- as(beats, 'Spatial')

  # Add id and beat numbers
  beats.sp@data$id <- paste0("ID", 1:nrow(beats.sp@data))
```

We then also convert the police beat coverage into a simple data frame format (fortified) that can be plotted in ggplot. This includes adding the beat identification code to the data frame.

``` r
  # Convert to a fortified object (data.frame)
  beats.spf <- broom::tidy(beats.sp)
  
  # Add beat id
  beats.spf$beat <- beats.sp@data$beat[match(beats.spf$id, 
                                             beats.sp@data$id)]
```

Finally, we save all three object types of the beat polygon data to an r workspace for easier loading later on.

``` r
  save(beats, beats.sp, beats.spf, 
       file= file.path(data.dir, 'geographic', 'beats.Rdata'))
```

We finish the manage stage of our analysis by closing the connection to the database.

``` r
  dbDisconnect(db.conn)
```

Chapter 7
---------

In chapter 7 we discuss data preparation. In it we transform, integrate and filter our data.

We begin with a set of prelimnary commands. Specifically, we load the necessary libraries, set our directory paths and load our custom functions.

``` r
 ## Load Libraries

  library(sf)
  library(sp)
  library(tidyverse)
  library(RSQLite)
  library(RODBC)
  library(geosphere)
  library(rgeos)
  
 ## Set data and code directory
 
  data.dir <- 'c:/temp/'                      # For example
  code.dir <- 'c:/code/research/REAIA_book/'  # For example
  
 ## Load custom source files
  
  source(paste0(code.dir, 'custom_functions.R'))  
```

We then set the path to our active database and establish a connection to it.

``` r
  # Specify path to database
  data.db <- file.path(data.dir, 'seattleCaseStudy.db')

  # Establish connection to database
  db.conn <- dbConnect(dbDriver('SQLite'), data.db)
```

#### Preparing Sales Data

We start the process by preparing the sales data. First, we load the data from the database

``` r
  sales.data <- dbReadTable(db.conn, 'Sales')
```

First we filter out observations that are missing critical information; parcel identification numbers and sale prices. Those with missing sale price or prices of 0 are usually non arm's length transactions which are not relevant to our analysis.

``` r
  # Filter those missing PIN numbers
  sales.data <- dplyr::filter(sales.data, Major > 0)
  
  # Filter those with sale price of 0
  sales.data <- dplyr::filter(sales.data, SalePrice > 0)
```

Next we perform a number of options of options to transform and then filter on the sale date. We begin by re-ordering the sales date into a standard month/date/year format and then convert this to a 'date' data type within R.

``` r
  # Re-order date field
  sales.data <- dplyr::mutate(.data=sales.data,
                              doc.date = paste0(substr(DocumentDate, 4, 5),
                                                 substr(DocumentDate, 1, 2),
                                                 substr(DocumentDate, 7, 10)))
  # Mutate new sale date field into R date format
  sales.data <- dplyr::mutate(.data=sales.data,
                              sales.date = as.Date(doc.date, "%d%m%Y"))
```

Then we create a sales year field

``` r
  sales.data <- dplyr::mutate(.data=sales.data,
                              sales.year = as.numeric(format(sales.date, "%Y")))
```

Next, the extra date field that are no longer needed are removed.

``` r
  sales.data <- dplyr::mutate(.data=sales.data,
                              doc.date = NULL)
  sales.data <- dplyr::mutate(.data=sales.data,
                              DocumentDate = NULL)
```

Sales which are missing sales dates are then filtered out.

``` r
  sales.data <- dplyr::filter(.data=sales.data,
                              !is.na(sales.date))
```

Next, we limit the data to sales from the year 2016

``` r
  sales.data <- dplyr::filter(.data=sales.data,
                              sales.year == 2016)
```

We then add fields denoting the quarter of sales (Q1 to Q4) and the month of sale (1 to 12)

``` r
  # Add qtr field 
  sales.data$month <- as.numeric(substr(sales.data$sales.date, 6, 7))
  
  # Add month field
  sales.data$qtr <- ((sales.data$month - 1) %/% 3) + 1
```

We now turn to other transformation and filters.

First, we convert the two-piece parcel identification number (PIN) into a single value, **pinX** that will link to the ID created for the geographical shapefile.

``` r
  sales.data <- buildPinx(X=sales.data)
```

Some property show an abnormally high number of transactions. We filter out any property that has transacted more than five time during the study period. (For all custom functions see the *custom\_functions.R* file.)

``` r
  sales.data <- buildTransCount(x.sales=sales.data, 
                                trans.limit=5)
```

We then label and remove all transactions that include more than one parcel.

``` r
  # Mutate: Add MultiParcel sale designation (custom function)
  sales.data <- idDup(x.data=sales.data, 
                      x.field='ExciseTaxNbr', 
                      new.field = 'multi.parcel',
                      idd.type='labelNonUnique', 
                      bin.nonuq=TRUE)
  
  # Filter those with multiparcel
  sales.data <- dplyr::filter(.data=sales.data,
                              multi.parcel == 0)
```

Next, we create a new unique identifier that is transaction (and not property) related. This step occurs here instead of in the 'manage' section due to its dependency on the previous filter and transform activities.

``` r
  sales.data <- buildSaleUIDs(sales.data) 
```

The King County Assessor provides three labels for each transaction, one indicating the type of instrument (or document) used to transfer title, one listing the stated reason for the sale and one with a set of warning codes generated by the assessor. We will filter the sales data by each of these fields.

First, we add a set of leading and trailing spaces around the sale warning field to make parsing of the codes easier.

``` r
  sales.data <- dplyr::mutate(sales.data,
                              SaleWarning = paste0(" ", SaleWarning, " "))
```

Next, we build a list containing the codes or values from each field that are to be filtered out. We then loop through this list and remove the observations that match the filtered values in each field (using the custom **trimByField()** function).

``` r
  # Create a list of factors to eliminate  
  trim.list <- list(SaleReason=2:19,  
                    SaleInstrument=c(0, 1, 4:28),
                    SaleWarning=paste0(" ", c(1:2, 5:9, 11:14, 18:23, 25, 27,
                                              31:33, 37, 39, 43, 46, 48, 49,
                                              50:53, 59, 61, 63, 64, 66), " "))
  
  # Loop through each factor type and filter accordingly (custom function)
  for (tL in 1:length(trim.list)){
    sales.data <- trimByField(x.data=sales.data, 
                              x.field=names(trim.list)[tL],
                              trim.list = unlist(trim.list[tL]))
  }
```

Finally, we remove all of the unnecessary fields and rename the sale price field.

``` r
  # Transform:  Limit field names
  sales.data <- dplyr::select(.data=sales.data, pinx, rec.ID, sale.ID, SalePrice, 
                              sales.date, sales.year, month, qtr)
  
  
  # Rename Sales Price field
  sales.data <- dplyr::rename(.data=sales.data, sale.price=SalePrice)
```

#### Prepare Parcel and Res Bldg Data

We now turn to preparing the parcel and residential building tabular data.

We begin by loading the parcel data from the database and then creating an identifier that will match with the sales and the geographic data, **pinx**.

``` r
  # Load Parcel Data
  parcel.data <- dbReadTable(db.conn, 'parcel')
  
  # Create Pinx Field
  parcel.data <- buildPinx(parcel.data)
```

We then do the same with the residential building data.

``` r
  # Load Res Building data
  resbldg.data <- dbReadTable(db.conn, 'resbldg')
  
  # Create Pinx Field
  resbldg.data <- buildPinx(resbldg.data)
```

Next, we merge (integrate) the record type (property type) from the residential building data to the sales data. We then convert this to a binary indicator, 1 if a residential sale, 0 if not.

``` r
  # Integrate the record type (Whether or not Residential) (Labeling)
  sales.data$res.record <- resbldg.data$BldgNbr[match(sales.data$pinx,
                                                      resbldg.data$pinx)]
  
  # Mutate the integrated field to binary
  sales.data$res.record <- ifelse(is.na(sales.data$res.record), 
                                  0, 
                                  sales.data$res.record)
```

We then remove all sales that are not residential and then remove the binary field just created.

``` r
  # Filter those with non-residential record type or with more than one dwelling on it  
  sales.data <- dplyr::filter(.data=sales.data,
                              res.record == 1)
  
  # Mutate:  Remove Res.record field
  sales.data <- dplyr::mutate(.data=sales.data, res.record = NULL)
```

The specific property use is then merged to the sale data from the parcel data file.

``` r
  sales.data$present.use <- parcel.data$PresentUse[match(sales.data$pinx,
                                                   parcel.data$pinx)]
```

We remove all sales that have no present use code and those that are not detached single family (2) or townhouse (29).

``` r
  # Filter those with no present use
  sales.data <- dplyr::filter(.data=sales.data,
                              !is.na(present.use))
  
  # Filter those not with SFR or Townhome use category
  sales.data <- dplyr::filter(.data=sales.data,
                              present.use == 2 | present.use == 29)
```

We start by limiting the parcel data to only those fields which we need and then renaming the fields.

``` r
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
```

We then do the same for the residential building data

``` r
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
```

Next, we perform a number of preparation exercises on the residential building data.

First, we sort the data by the individual building number, remove any properties with more than one building and then remove any remains observations that do not indicate the primary building (data error). We finish by removing the buildling number field.

``` r
  # Sort by building number
  resbldg.data <- dplyr::arrange(.data=resbldg.data,
                                bldg.nbr)

  # Remove those with more than one building 
  resbldg.data <- dplyr::filter(.data=resbldg.data,
                                !duplicated(pinx))
  
  # Remove remaining observations with buliding number not 1
  resbldg.data <- dplyr::filter(.data=resbldg.data,
                                bldg.nbr == 1)
  
  # Remove building number field
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                 bldg.nbr = NULL)
```

Next, we create a new field the totals up the three bathrooms fields. We then remove the partial bath fields.

``` r
  # Create total baths
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                baths = bath.full + bath.75 * .75 + bath.half * .5)
  
  # Remove partial bath fields
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                bath.full = NULL)
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                bath.75 = NULL)
  resbldg.data <- dplyr::mutate(.data=resbldg.data,
                                bath.half = NULL)
```

#### Integrate Sales with Parcel and ResBldg data

First we join the parcel data to the sales data based on the pinx field.

``` r
  # Join parcel data to sales (inner join)
  sales.data <- dplyr::inner_join(x=sales.data, 
                                  y=parcel.data, 
                                  by='pinx')
```

Then we join the residential building data to the sales data based on the pinx field.

``` r
  # Join res bldg data to sales (inner join)
  sales.data <- dplyr::inner_join(x=sales.data, 
                                  y=resbldg.data, 
                                  by='pinx')
```

After the integration we continue with transformation and filtering.

First we create a home age variable by subtracting the year built from the sale year. We then filter out all homes built after the year of sale (land sales) and then remove the year built field.

``` r
  # Mutate: Create Age variable
  sales.data <- dplyr::mutate(.data=sales.data,
                              age=sales.year - yr.built)
  
  # Filter: Remove those built after the sale
  sales.data <- dplyr::filter(.data=sales.data,
                              age >= 0)
  
  # Remove Yr.Built field
  sales.data <- dplyr::mutate(.data=sales.data,
                              yr.built=NULL)
```

Similarly, we create a renovated age field by subtracting the year of renovation from the year of sale and remove any sale with a major renovation after the sale date.

``` r
  # Create years since renovation
  sales.data <- dplyr::mutate(.data=sales.data,
                              age.renov = sales.year - yr.ren)
  
  # Filter: Remove those renovated after the sale
  sales.data <- dplyr::filter(.data=sales.data,
                              age.renov >= 0)
```

We then take the lesser of two, the regular age and the renovated age to create the effective age of the dwelling. After doing this we remove the now unnecessary renovated age and renovation year field.

``` r
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
```

Finally, we filter out all sales with more than one living unit (duplexes, triplexes, etc.) and then remove the field giving the count of living units.

``` r
  # Filter: Remove those with more than one land use
  sales.data <- dplyr::filter(.data=sales.data,
                              nbr.lu == 1)
  
  # Remove Nbr.lu field
  sales.data <- dplyr::mutate(.data=sales.data,
                              nbr.lu=NULL)
```

### Integrate Sales and Geospatial Data

Before integration, we perform a few basic transformation and filtering steps on the geospatial data.

We start by loading the parcel centroids object we created in the manage step.

``` r
  load(file= file.path(data.dir, 'geographic/parcelcentroids.Rdata'))
```

We then create a **pinx** field to join with the sales data. After doing so we remove the unnecessary **PIN** field.

``` r
  # Mutate:  Add pinx field for joining
  parcel.centroids$pinx <- paste0('..', parcel.centroids$PIN)
  
  # Remove unnecessary fields
  parcel.centroids$PIN <- NULL
```

Next, we filter the parcel centroid file to include only those parcels that match with a sale.

``` r
  parcel.centroids <- dplyr::filter(parcel.centroids, pinx %in% sales.data$pinx)
```

Now we prepare the police beat data.

First we load the workspace with the three different police beat objects.

``` r
  load(file=file.path(data.dir, 'geographic/beats.Rdata'))
```

To begin, we filter out all beats areas that do not have a precinct ID (water areas)

``` r
  beats <- dplyr::filter(beats, first_prec != '')
```

Next, we calculate the areal extent (in square miles) of each beat area. We do this by extracting a list of the polygon objects, calculating the raw area for each and then converting it to square miles (approx.)

``` r
  # Extract polygon objects
  beat.polys <- beats.sp@polygons
  
  # Extract area measurement from the shapefile
  poly.area <- unlist(lapply(beat.polys, function(x) x@area))
  
  # Convert to square miles
  poly.area <- poly.area * (68.99 ^ 2)
```

We then add these areal measurements to the beats sp object.

``` r
  beats.sp@data$size <- poly.area
```

Then we write the three beat objects back to an R workspace for future analysis.

``` r
  save(beats, beats.sp, beats.spf, 
       file= file.path(data.dir, 'geographic/beats.Rdata'))
```

Now we integrate the beat and centroid data to the sales.

We start by integrating the beat and the parcel data. We do this by joining the beat code to the parcel centroid data. First we set the parcel centroid beat to 'NONE'.

``` r
  parcel.centroids$beat <- 'NONE'
```

Then we run an intersect (overlay or spatial join) operation that assigns parcel centroid references to each beat.

``` r
  beats.overlay <- st_intersects(beats, parcel.centroids)
```

Next, we loop through each beat and assign the beat code to the parcels that fall within that beat.

``` r
  # Loop through each beat
  for (i in 1:length(beats.overlay)){
    
    # Extract the refenence IDs
    ov.id <- beats.overlay[[i]]
    
    # Apply relevant beat name
    parcel.centroids$beat[ov.id] <- as.character(beats$beat[i])

  }
```

Finally, we remove the parcel centroids that are not within one of the beats (not within the City of Seattle).

``` r
  parcel.centroids <- dplyr::filter(parcel.centroids, beat != "NONE")
```

Next we extract the latitude and longitude values from the jointly held lat/long field in the parcel centroid data and add them to their own separate fields.

``` r
  # Transform: Create Separate Lat/long Columns 
  parcel.centroids$longitude <- unlist(lapply(parcel.centroids$parcel.centroids, function(x) x[1]))
  parcel.centroids$latitude <- unlist(lapply(parcel.centroids$parcel.centroids, function(x) x[2]))
```

We then save the parcel centroid data to a new R workspace for future use.

``` r
  save(parcel.centroids, 
       file= file.path(data.dir, 'geographic/parcelcentroids_sales.Rdata'))
```

Now we integrate (join) the sales data and the parcel centroid data by the **pinx**. We do an inner join here so that only those observations with **pinx** in both the sales and the parcel centroids are retained.

``` r
  sales.data <- merge(sales.data, 
                      parcel.centroids[ , c('pinx', 'beat', 'longitude', 'latitude')],
                      by='pinx')
```

Now that we have limited the sales to the City of Seattle only, we re-classify the sales data zoning variables into five simple classes.

``` r
  sales.data$zoning[grep('LR1', sales.data$zoning)] <- 'LR1'
  sales.data$zoning[grep('LR2', sales.data$zoning)] <- 'LR2'
  sales.data$zoning[grep('LR3', sales.data$zoning)] <- 'LR3'
  sales.data$zoning[grep('NC', sales.data$zoning)] <- 'Comm'
  sales.data$zoning[grep('C1', sales.data$zoning)] <- 'Comm'
  sales.data$zoning[grep('MR', sales.data$zoning)] <- 'Other'
  sales.data$zoning[grep('RSL', sales.data$zoning)] <- 'Other'
  sales.data$zoning[grep('NR', sales.data$zoning)] <- 'Other'
```

#### Prepare Crime Data

We begin by reading in the crime data from the database.

``` r
  crime.data <- dbReadTable(db.conn, 'Crime')
```

Next, we fix the formatting on the date and time field.

First, we extract just the date values and convert to the R date format.

``` r
 ## Transform date and time fields  
  
  # Extract date only from time/date field
  crime.data <- dplyr::mutate(.data=crime.data,
                              crime.date = as.Date(substr(
                                occurred.date.or.date.range.start, 1, 10), '%m/%d/%Y'))
```

We then extract the time of the reported crime and convert to a 24 hour format.

``` r
  # Extract time from time+date field                           
  crime.data <- dplyr::mutate(.data=crime.data,
                              crime.time = substr(
                                occurred.date.or.date.range.start, 12, 20))
  
  # Convert to fractional hours  
  crime.data$crime.time <- as.numeric(substr(crime.data$crime.time,1,2)) + 
                         as.numeric(substr(crime.data$crime.time,4,5))/60
  
  # Convert Am and PM to 24 hour time
  crime.data$crime.time <- ifelse(substr(crime.data$occurred.date.or.date.range.start, 
                                   21, 21) == 'A', 
                            crime.data$crime.time,
                            crime.data$crime.time + 12)
  
  # Fix issues of 12Am being 24+ hours
  crime.data$crime.time <- ifelse(crime.data$crime.time >=24, 
                            crime.data$crime.time - 24, 
                            crime.data$crime.time)
```

We then convert the crime date field to the R date format.

``` r
  crime.data$crime.date <- as.Date(crime.data$crime.date)
```

Next, we re-classify the crime types from the 29 large categories into four: violent, property, behavior and traffic. All smaller categories are considered other.

``` r
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
```

#### Integrate Crime and Sales Data

Next, we integrate the crime and sales data.

We start by limiting the crime date to reported incidents in 2015 or later.

``` r
  crime.data <- crime.data[crime.data$year >= 2015, ]
```

Then we take each sale, count the number of reported crime (and of what type) that occurred within 400 meters of the property and happened in the year prior to the sale of the property. This is a multiple step operation and can be quite slow on machines with limited computing power.

We start by setting the distance threshold to 400 meters.

``` r
  dist.thres <- 400
```

Then we set the value of nearby crime counts for all sales to 0 for each crime type.

``` r
  sales.data$crime.violent <- 0
  sales.data$crime.property <- 0
  sales.data$crime.traffic <- 0
  sales.data$crime.behavior <- 0
  sales.data$crime.other <- 0
```

Next we start a loop through each sale

``` r
  for (j in 1:nrow(sales.data)){
```

We extract the individual sale, calculate the difference in days between each crime incident and limit to the crime within the one year prior window.

``` r
    # Extract sales data
    j.data <- sales.data[j,]
    
    # Calculate time difference
    x.days <- j.data$sales.date - crime.data$crime.date
    
    # Limit crime data to time window
    c.data <- crime.data[x.days > 0 & x.days < 365, ]
```

We then calculate the distance between all of these crimes and the sale and then limit to those within the threshold (400m or 1/4 mile)

``` r
    # Calculate distances
    j.dist <- distHaversine(j.data[,c('longitude', 'latitude')],
                            c.data[,c('longitude', 'latitude')])

    # Limit data to those within threshold
    cx.data <- c.data[j.dist < dist.thres, ]
```

We then add a count of each crime type to the sale observation

``` r
    sales.data$crime.violent[j] <- length(which(cx.data$crime.type == 'violent'))
    sales.data$crime.property[j] <- length(which(cx.data$crime.type == 'property'))
    sales.data$crime.behavior[j] <- length(which(cx.data$crime.type == 'behavior'))
    sales.data$crime.traffic[j] <- length(which(cx.data$crime.type == 'traffic'))
    sales.data$crime.other[j] <- length(which(cx.data$crime.type == 'other'))
```

After every 100 sales complete we output the progress (this could be removed from code it will not impact performance)

``` r
    if (j %% 100 == 0){
      cat('record number ', j, '\n')
    }
```

We close the loop

``` r
  }
```

#### Prepare Sentiment Tweets

We now prepare the sentiment tweet data

First we load in from the database

``` r
  tweet.data <- dbReadTable(db.conn, 'SentimentTweets')
```

Next, we limit the tweet to those within City of Seattle

First we create a boundary file for the City of Seattle by dissolving (or unary unioning) the police beats.

``` r
  seattle.bound <- gUnaryUnion(beats.sp)
```

We then convert the tweets to a SpatialPointsDataFrame, with the correct coordinate reference system.

``` r
  tweet.sp <- SpatialPointsDataFrame(cbind(tweet.data$longitude, tweet.data$latitude),
                                     data=tweet.data)
  proj4string(tweet.sp) <- CRS(proj4string(beats.sp))
```

Next, we intersect the two in order to determine which tweets fall within the city of Seattle. We then limit the data to those tweets within Seattle city limits.

``` r
  # Clip by city boundary
  in.seattle <- gIntersects(tweet.sp, seattle.bound, byid=T)
  
  # Filter to those in city
  tweet.sp <- tweet.sp[which(in.seattle), ]
```

The sentiment score are integer values ranging from -5 to 5, based on their sentiment. For the purposes of judging local sentiment we will only consider positive and negative sentiment, thus we must filter and transform the data.

We start by removing all neutral tweet (sentiment of 0) from the data.

``` r
  twsent.sp <- tweet.sp[tweet.sp@data$SentimentScore != 0, ]
```

Then we convert all tweet to simply negative (-1) or positive (1)

``` r
  twsent.sp@data$SS <- ifelse(twsent.sp@data$SentimentScore < 0, -1, 1)
```

Finally, we extract just the data portion of the tweet sp object.

``` r
  twsent <- twsent.sp@data
```

### Write Data Back to Database

Finally, we write the non-spatial data back to the data base.

First we convert the date fields in the sales and crime date back to simple character strings for storage purposes

``` r
  # Convert sales.date to character to write to database
  sales.data <- dplyr::mutate(.data=sales.data,
                              sales.date = as.character(sales.date))  
  
  # Convert sales.date to character to write to database
  crime.data <- dplyr::mutate(.data=crime.data,
                              crime.date = as.character(crime.date))  
```

Then we write the sales data to the database as **prepSales** or prepared sales.

``` r
  dbWriteTable(db.conn, 'prepSales', sales.data, row.names=FALSE, overwrite=TRUE)
```

Next, we write the crime data, overwriting the previous data.

``` r
  # Check if exists, if so overwrite
  if (dbExistsTable(db.conn, 'Crime')){
    dbRemoveTable(db.conn, 'Crime')
  }
  
  # Write to Database
  dbWriteTable(db.conn, 'Crime', crime.data, row.names=FALSE)
```

And, finally, we write the tweet sentiment data, again overwriting the previous data

``` r
  # Check if exists, if so overwrite
  if (dbExistsTable(db.conn, 'SentimentTweets')){
    dbRemoveTable(db.conn, 'SentimentTweets')
  }

  # Write to database
  dbWriteTable(db.conn, 'SentimentTweets', twsent, row.names=FALSE)
```

We complete the preparation phase by closing the connection to the database.

``` r
  dbDisconnect(db.conn)
```

Chapters 8 and 9
----------------

Chapters 8 and 9 cover the exploration and data cleaning steps. As these stages are highly iterative, and often rely on similar techniques and methods, we will cover them jointly in the code.

#### Preliminary commands

As usual, we begin with a set of preliminary commands.

First we load the necessary libraries

``` r
  library(sf)
  library(sp)
  library(tidyverse)
  library(RSQLite)
  library(RODBC)
  library(GGally)
```

Then we set the paths to the code and the data

``` r
  data.dir <- 'c:/temp/'                      # For Example
  code.dir <- 'c:/code/research/REAIA_book/'  # For Example
```

We load the custom functions

``` r
  source(paste0(code.dir, 'custom_functions.R'))  
```

And then we set the name of the database and establish a connection to it.

``` r
  # Set database location
  data.db <- file.path(data.dir, 'seattleCaseStudy.db')

  # Establish connection
  db.conn <- dbConnect(dbDriver('SQLite'), data.db)
```

#### Load Data

We begin by loading the data.

First we load the sales data from the database.

``` r
  sales.data <- dbReadTable(db.conn, 'prepSales')
```

Then we load in the three spatial representations of the Seattle Police Beat data from the saved R workspace.

``` r
  load(file=file.path(data.dir, 'geographic/beats.Rdata'))
```

We then convert the sales dates back into R format

``` r
  sales.data <- dplyr::mutate(.data=sales.data,
                               sales.date=as.Date(sales.date))
```

Finally, we ensure that the categorial variables are represented as such.

``` r
  # Identify Categorical Varialbes
  cat.vars <- c('present.use', 'zoning', 'topo', 'restr.szshp',
                'view.rainier', 'view.olympics', 'view.cascades',
                'view.terr', 'view.city', 'view.puget', 'view.lkwash',
                'view.lksamm', 'view.smwater', 'view.other', 'wfnt', 'wfnt.bank',
                'traffic.noise', 'bldg.grade', 'condition', 'beat')
  
  # Ensure they are represented as factors (categorical).
  for(cv in cat.vars) sales.data[, cv] <- as.factor(sales.data[, cv])
```

#### Univariate Exploration

We start with exploring the univariate properties of the sales data.

First, we get a list of the field types

``` r
  field.types <- unlist(lapply(sales.data, class))
```

For all fields that are numeric or integers, we build a summary statistics table using the **fullSummary** custom function. We then print this to the screen for review.

``` r
  # Compute summary table for numeric and integer variables
  summ.table <- t(do.call(cbind, 
                        lapply(sales.data[,field.types == 'numeric' |
                                           field.types == 'integer'],
                               fullSummary)))
  
  # Show the table
  summ.table
```

Next, we compute a set of summary statistic for the factor (categorical) variables.

``` r
  summary(sales.data[, field.types == 'factor'])
```

Given the summary statistics, we now filter out a number of observations that appear to be errors of value or mislabeled transactions that were not eliminated earlier.

First we look at the lowest sales prices and remove all of those under $50,000

``` r
  # Examine lowest prices
  head(sales.data$sale.price[order(sales.data$sale.price)], 300)
  
  # Filter out those below $50,000 
  sales.data <- dplyr::filter(sales.data, sale.price >= 50000)
```

Next we look at home sizes and remove those under 300 square feet in size.

``` r
  # Examine lowest home sizes
  head(sales.data$tot.sf[order(sales.data$tot.sf)], 300)
  
  # Filter those below 300
  sales.data <- dplyr::filter(sales.data, tot.sf >= 300)
```

And then we remove those homes with more than 10 bedrooms.

``` r
  # Examine lowest number of bedrooms
  tail(sales.data$beds[order(sales.data$beds)], 300)

  # Filter those greater htan 10
  sales.data <- dplyr::filter(sales.data, beds <= 10)
```

#### Univariate Visual/Informal Exploration

We now make visual explanation of most of the numeric fields. For continuous variables we use density plots and for the categorical or ordinal we use bar plots.

We start with the continuous variables.

``` r
  # Sale Price
  sp.dens <- ggplot(sales.data,
         aes(x=sale.price)) +
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Sale Price') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Lot Size
  ls.dens <- ggplot(sales.data,
         aes(x=lot.size)) +
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Lot Size') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Home Size
  hs.dens <- ggplot(sales.data,
                    aes(x=tot.sf)) +
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Home Size') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Home Age
  ag.dens <- ggplot(sales.data,
         aes(x=age))+
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Home Age') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Effective Age
  ea.dens <- ggplot(sales.data,
         aes(x=eff.age)) +
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Effective Age') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Basement Size
  bs.dens <- ggplot(sales.data,
         aes(x=bsmt.sf)) +
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Basement Size') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Garage Basement Size
  gs.dens <- ggplot(sales.data,
         aes(x=gar.bsmt.sf)) +
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Garage Basement Size') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Attached Garage Size
  as.dens <- ggplot(sales.data,
         aes(x=gar.att.sf)) +
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Attached Garage Size') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Deck Size
  ds.dens <- ggplot(sales.data,
         aes(x=deck.sf)) +
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Deck Size') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Waterfrontage
  wf.dens <- ggplot(sales.data,
                    aes(x=wfnt.ftg)) +
    geom_density(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Water Frontage Length') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Make Plots
  ggMultiPlots(sp.dens, ls.dens, hs.dens, ag.dens, 
               ea.dens, bs.dens, as.dens, gs.dens,
               ds.dens, wf.dens, cols=4)
  
  # Stories  
  st.hist <- ggplot(sales.data,
         aes(x=stories)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Stories') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Bedrooms
  bd.hist <- ggplot(sales.data,
         aes(x=beds))+
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Bedrooms') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Bathrooms
  bt.hist <- ggplot(sales.data,
         aes(x=baths)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Bathrooms') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
```

We then plot the density plots for the continuous variables.

``` r
  ggMultiPlots(st.hist, bd.hist, bt.hist, cols=3)
```

Next we build the bar plots for the categorical and ordinal variables.

``` r
  # Present Use
  pu.hist <- ggplot(sales.data,
                    aes(x=present.use)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Present Use') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Zoning
  zo.hist <- ggplot(sales.data,
                    aes(x=zoning)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Zoning') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Restrictive Topography
  tp.hist <- ggplot(sales.data,
                    aes(x=topo)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Restrictive Topography') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Restrictive Lot Size
  rs.hist <- ggplot(sales.data,
                    aes(x=restr.szshp)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Restrictive Lot Size/Shape') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # View of Rainier
  vr.hist <- ggplot(sales.data,
                    aes(x=view.rainier)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('View of Mt. Rainier') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # View of Olympics
  vo.hist <- ggplot(sales.data,
                    aes(x=view.olympics)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('View of Olympic Mountains') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # View of Cascades
  vc.hist <- ggplot(sales.data,
                    aes(x=view.cascades)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('View of Cascade Mountains') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Territorial View
  vt.hist <- ggplot(sales.data,
                    aes(x=view.terr)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Territorial View') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # City View
  vy.hist <- ggplot(sales.data,
                    aes(x=view.city)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('City View') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Puget Sound View
  vp.hist <- ggplot(sales.data,
                    aes(x=view.puget)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Puget Sound View') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Lake Washington View
  vw.hist <- ggplot(sales.data,
                    aes(x=view.lkwash)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Lake Washington View') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Lake Sammamish View
  vs.hist <- ggplot(sales.data,
                    aes(x=view.lksamm)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Lake Sammamish View') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Small waterbody view
  vb.hist <- ggplot(sales.data,
                    aes(x=view.smwater)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('View of Small Waterbody') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Other View
  v0.hist <- ggplot(sales.data,
                    aes(x=view.other)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('View of Other Amenity') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Water Frontage Type
  wt.hist <- ggplot(sales.data,
                    aes(x=wfnt)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Water Frontage Type') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Water front bank type
  wb.hist <- ggplot(sales.data,
                    aes(x=wfnt.bank)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Water Frontage Bank Type') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Traffic Noise
  tn.hist <- ggplot(sales.data,
                    aes(x=traffic.noise)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Traffic Noise Severity') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Building Grade
  bg.hist <- ggplot(sales.data,
                    aes(x=bldg.grade)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Building Grade') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Building Condition
  bc.hist <- ggplot(sales.data,
                    aes(x=condition)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Building Condition') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
  
  # Police Beat Count
  pb.hist <- ggplot(sales.data,
                    aes(x=beat)) +
    geom_bar(fill='blue', alpha=.5) +
    ylab('') +
    xlab('') +
    ggtitle('Police Beat') + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y=element_blank())
```

And then plot them.

``` r
  # Make Plots 1
  ggMultiPlots(pu.hist, zo.hist, tp.hist,
               rs.hist, vr.hist, vo.hist,
               vc.hist, vt.hist, vy.hist,
               cols=3)
  
  # Make Plots 2
  ggMultiPlots(vp.hist, vw.hist, vs.hist,
               vb.hist, v0.hist, wt.hist,
               wb.hist, tn.hist, bg.hist,
               bc.hist, pb.hist, 
               cols=3)
```

From the exploration above, we make a number transformations to the data.

First we eliminate the 'View of Lake Sammamish' variable as no observations have this.

``` r
  sales.data$view.lksamm <- NULL
```

And then we convert the waterfront variable from an ordinal to a binary due to the small number of categories in the ordinal variable.

``` r
  sales.data$is.waterfront <- ifelse(sales.data$wfnt != 0, 1, 0)
```

Finally, we convert the beds, baths and building quality variables to numeric (from categorical) due to their large number of levels.

``` r
  sales.data$beds <- as.numeric(as.character(sales.data$beds))
  sales.data$baths <- as.numeric(as.character(sales.data$baths))
  sales.data$bldg.grade <- as.numeric(as.character(sales.data$bldg.grade))
```

#### Multivariate Exploration

Next, we turn to multivariate exploration.

We start by building a scatter plot matrix involving some of the most important variables in the sales data.

``` r
  ggcorr(sales.data[,c('sale.price', 'tot.sf', 'lot.size','beds', 'baths',
                       'age', 'bldg.grade')], label=TRUE)
```

To get a better look at the bivariate relationship between the independent variables and sales price (dependent) we create individual scatterplots.

``` r
  # Home Size and Sale Price
  ggplot(sales.data,
         aes(x=tot.sf, y=sale.price))+
    geom_point() +
    scale_y_log10() +
    stat_smooth()

  # Age and Sale Price
  ggplot(sales.data,
         aes(x=age, y=sale.price))+
    geom_point() +
    scale_y_log10() +
    stat_smooth()
  
  # Eff Age and Sale Price
  ggplot(sales.data,
         aes(x=eff.age, y=sale.price))+
    geom_point() +
    scale_y_log10() +
    stat_smooth()
  
  # Lot Size and Sale Price
  ggplot(sales.data,
         aes(x=lot.size, y=sale.price))+
    geom_point() +
    scale_y_log10() +
    scale_x_log10() +
    stat_smooth()
```

And for the categorical and ordinal variables, we create boxplots

``` r
  # Present Use and Sale Price
  ggplot(sales.data,
         aes(x=present.use, y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()

  # Waterfront and Sale Price
  ggplot(sales.data,
         aes(x=(wfnt!=0), y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()
  
  # Zoning and Sale Price
  ggplot(sales.data,
         aes(x=zoning, y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()
  
  # Topography and Sale Price
  ggplot(sales.data,
         aes(x=topo, y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()
  
  # Building Grade and Sale Price
  ggplot(sales.data,
         aes(x=as.factor(bldg.grade), y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()

  # Condition and Sale Price
  ggplot(sales.data,
         aes(x=as.factor(condition), y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()
  
  # Baths and Sale Price
  ggplot(sales.data,
         aes(x=as.factor(baths), y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()

  # Beds and Sale Price
  ggplot(sales.data,
         aes(x=as.factor(beds), y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()
```

#### Transform View Variables

From the above exploration we find that the view variables need some recategorization and transformation. Particularly, we will transform the multiple view variables (9) into a single categorical variable indicating the best view that each sale has, if any.

We start by creating a matrix of whether or not each observation has a particular view.

``` r
  has.view <- as.data.frame(lapply(sales.data[,14:22], 
                             function(x) ifelse(as.numeric(x) == 1, 0, 1)))
```

Next, for each observation we record which view offers the maximum value (best rated view). This requires us to build a custom function to scan each observation and then return the column number of the best view rating.

``` r
  # Small custom function to extract
  getX <- function(x){
    if(max(x) == 0){
      return(0)
    } else {
      return(which(x == max(x))[1])
    }
  }
  
  # Get maximum  view
  max.view <- apply(has.view, 1, getX)
```

We then create a vector containing the name of the best view. First we set all values to no view, then we add the name of the view, if any, that has the maximum rating. Finally, we add this vector to the data under the **view.best** field.

``` r
  # Set Null View Type
  view.type <- rep('no view', length(max.view))
  
  # Assign Maximum
  view.type[max.view != 0] <- names(has.view)[max.view[max.view != 0]]
  
  # Add to sales.data
  sales.data$view.best <- view.type
```

We then plot prices against the various view types as a set of boxplots.

``` r
  ggplot(sales.data,
         aes(x=view.best, y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()
```

We now make two more transformations to the view scores. First we consolidate them into binary views based on whether there is a mountain, water or other view.

``` r
  # Create mountain view
  sales.data$view.mtn <- ifelse(sales.data$view.rainier != 0 |
                                  sales.data$view.olympics != 0 |
                                  sales.data$view.cascades != 0,
                                1, 0)

  # Create water view
  sales.data$view.water <- ifelse(sales.data$view.puget != 0 |
                                    sales.data$view.lkwash != 0 |
                                    sales.data$view.smwater != 0,
                                  1, 0)
  
  # Create other view
  sales.data$view.other <- ifelse(sales.data$view.city != 0 |
                                    sales.data$view.terr != 0 |
                                    sales.data$view.other != 0,
                                  1, 0)
```

We also total up all view scores across the nine categories to give a sum of views for each observations.

``` r
  # Create a matrix of view scores (as numeric)
  view.scores <- as.data.frame(lapply(sales.data[, 14:22], 
                                      function(x) as.numeric(as.character(x))))

  # Sum and add to the data
  sales.data$view.score <- rowSums(view.scores)
```

#### Mapping

Next we map the independent variables.

First we build a base map showing the location of the observations.

``` r
  base.map <- ggplot(sales.data,
                     aes(x=longitude, 
                         y=latitude)) +
    geom_point(color='gray40', size=.1)
    
  base.map <- base.map + geom_path(data=beats.spf,
                                   aes(x=long, y=lat, group=id),
                                   color='gray20')
  base.map
```

Then we plot the observations, colored by sale price.

``` r
  price.map <- ggplot(sales.data,
                   aes(x=longitude,
                       y=latitude, 
                       color=log(sale.price))) +
    scale_color_gradient2(low='red', high='blue', midpoint=12.5)+
    geom_point()+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  price.map
```

We then create point maps of the other variables.

``` r
  # Property use (Detached or townhome)
  use.map <- ggplot(sales.data,
                     aes(x=longitude, 
                         y=latitude,
                         color=present.use)) +
    geom_point(size=.4) +
    scale_color_manual(values=c('black', 'red')) + 
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  use.map
  
  # Zoning type
  zoning.map <- ggplot(sales.data,
                     aes(x=longitude, 
                         y=latitude,
                         color=zoning)) +
    geom_point() +
    scale_color_manual(values=c('red', 'royalblue', 'blue', 'navy',
                                'darkorange', 'gray80', 'gray50', 'gray20')) +
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  zoning.map
  
  # Challenging Topography
  topo.map <- base.map + 
    geom_point(data=sales.data[sales.data$topo==1, ],
               aes(x=longitude,
                   y=latitude),
               color='red')+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  topo.map
  
  # Waterfont
  wfnt.map <- base.map + 
    geom_point(data=sales.data[sales.data$wfnt!=0, ],
               aes(x=longitude,
                   y=latitude),
               color='blue') +
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  wfnt.map
  
  # Traffic Noise
  noise.map <- base.map + 
    geom_point(data=sales.data[sales.data$traffic.noise!=0, ],
               aes(x=longitude,
                   y=latitude, 
                   color=traffic.noise)) +
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  noise.map
  
  # Mountain View
  mtview.map <- base.map + 
    geom_point(data=sales.data[sales.data$view.rainier != 0, ],
               aes(x=longitude,
                   y=latitude),
               color='blue') + 
    geom_point(data=sales.data[sales.data$view.olympics != 0, ],
               aes(x=longitude,
                   y=latitude),
               color='red')+ 
    geom_point(data=sales.data[sales.data$view.cascades != 0, ],
               aes(x=longitude,
                   y=latitude),
               color='darkgreen')+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  mtview.map
  
  # Waterview
  waterview.map <- base.map + 
    geom_point(data=sales.data[sales.data$view.puget != 0, ],
               aes(x=longitude,
                   y=latitude),
               color='blue') + 
    geom_point(data=sales.data[sales.data$view.lkwash != 0, ],
               aes(x=longitude,
                   y=latitude),
               color='red')+ 
    geom_point(data=sales.data[sales.data$view.smwater != 0, ],
               aes(x=longitude,
                   y=latitude),
               color='darkgreen') +
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  waterview.map
  
  # Other View
  otherview.map <- base.map + 
    geom_point(data=sales.data[sales.data$view.terr != 0, ],
               aes(x=longitude,
                   y=latitude),
               color='blue') + 
    geom_point(data=sales.data[sales.data$view.city != 0, ],
               aes(x=longitude,
                   y=latitude),
               color='red')+ 
    geom_point(data=sales.data[sales.data$view.other != 0, ],
               aes(x=longitude,
                   y=latitude),
               color='darkgreen')+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  otherview.map
  
  # View Score
  viewscore.map <- ggplot(sales.data,
                      aes(x=longitude,
                          y=latitude, 
                          color=view.score)) +
    scale_color_gradient(low='gray50', high='darkred')+
    geom_point()+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  
  # Home Size
  sf.map <- ggplot(sales.data,
                    aes(x=longitude,
                        y=latitude, 
                        color=log(tot.sf))) +
    scale_color_gradient2(low='red', high='blue', midpoint=7.5)+
    geom_point()+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  sf.map

  # Lot Size
  ls.map <- ggplot(sales.data,
                   aes(x=longitude,
                       y=latitude, 
                       color=log(lot.size))) +
    scale_color_gradient2(low='red', high='blue', midpoint=9.5)+
    geom_point()+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  ls.map
  
  # Nbr of Bedrooms
  bed.map <- ggplot(sales.data,
                   aes(x=longitude,
                       y=latitude, 
                       color=as.factor(beds))) +
    geom_point()+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  bed.map

  # Nbr of bathrooms
  baths.map <- ggplot(sales.data,
                   aes(x=longitude,
                       y=latitude, 
                       color=baths)) +
    scale_color_gradient2(low='red', high='blue', midpoint=2.5)+
    geom_point()+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  baths.map

  # Nbr of Stories
  stories.map <- ggplot(sales.data,
         aes(x=longitude,
             y=latitude, 
             color=as.factor(stories))) +
    geom_point()
  stories.map
  
  # Age of Structure
  age.map <- ggplot(sales.data,
         aes(x=longitude,
             y=latitude, 
             color=age)) +
    scale_color_gradient2(low='red', high='blue', midpoint=45)+
    geom_point()
  age.map
  
  # Home grade (quality)
  grade.map <- ggplot(sales.data,
         aes(x=longitude,
             y=latitude, 
             color=bldg.grade)) +
    geom_point()
  grade.map
  
  # Home condition
  cond.map <- ggplot(sales.data,
                      aes(x=longitude,
                          y=latitude, 
                          color=condition)) +
    geom_point()
  cond.map
  
  # Basement size
  bsmt.map <- base.map + 
    geom_point(data=sales.data[sales.data$bsmt.sf != 0, ],
                  aes(x=longitude,
                       y=latitude, 
                       color=log(bsmt.sf+1))) +
    scale_color_gradient2(low='red', high='blue', midpoint=5.5)+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  bsmt.map  
  
  # Attached garage size
  gara.map <- base.map + 
    geom_point(data=sales.data[sales.data$gar.att.sf != 0, ],
               aes(x=longitude,
                   y=latitude, 
                   color=log(gar.att.sf+1))) +
    scale_color_gradient2(low='red', high='blue', midpoint=5.5)+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  gara.map  
  
  # Basement garage size
  garb.map <- base.map + 
    geom_point(data=sales.data[sales.data$gar.bsmt.sf != 0, ],
               aes(x=longitude,
                   y=latitude, 
                   color=log(gar.bsmt.sf+1))) +
    scale_color_gradient2(low='red', high='blue', midpoint=5.5)+
    geom_path(data=beats.spf,
              aes(x=long, y=lat, group=id),
              color='gray20')
  garb.map  
```

#### Label Discordant Values

Finally, based on the exploration above, we deal with the discordant values. We take a 'label and test' approach, by labeling the discordant values in this step and then testing for their impact in the modeling step that follows.

We start by adding three new fields to the sales data: 1. Discordant: Binary indicating discordant or not 2. Disc.fields: Text field with all fields that are discordant 3. Disc.type: Text field with the type of discordancy for each discordant field

Each of these is set to 0 or blank to begin.

``` r
  sales.data$discordant <- 0
  sales.data$disc.fields <- ""
  sales.data$disc.type <- ""
```

First, we label all properties with a restrictive lot size or shape as discordant. We do this because there are only a few of them and they are likely very unique properties not indicative of the broader market.

``` r
  rss <- which(sales.data$restr.szshp != 0)
  sales.data$discordant[rss] <- 1
  sales.data$disc.fields[rss] <- paste0(sales.data$disc.fields[rss], 'restr.szshp | ')
  sales.data$disc.type[rss] <- paste0(sales.data$disc.type[rss], 'small category | ')
```

Next, we apply a discordant label to a number of key fields based on their values. Particularly, we label all values below the 1% quantile or above the 99% quantile as potentially discordant. These are Univariate (UV) discordants.

``` r
  # Sale Price
  spd <- which(sales.data$sale.price > quantile(sales.data$sale.price, .99) |
                 sales.data$sale.price < quantile(sales.data$sale.price, .01))
  
  sales.data$discordant[spd] <- 1
  sales.data$disc.fields[spd] <- paste0(sales.data$disc.fields[spd], 'sale.price | ')
  sales.data$disc.type[spd] <- paste0(sales.data$disc.type[spd], 'UV: 1% Quant | ')
  
  # Home Size
  tsd <- which(sales.data$tot.sf > quantile(sales.data$tot.sf, .99) |
                 sales.data$tot.sf < quantile(sales.data$tot.sf, .01))
  
  sales.data$discordant[tsd] <- 1
  sales.data$disc.fields[tsd] <- paste0(sales.data$disc.fields[tsd], 'tot.sf | ')
  sales.data$disc.type[tsd] <- paste0(sales.data$disc.type[tsd], 'UV: 1% Quant | ')
 
  # Lot Size
  lsd <- which(sales.data$lot.size > quantile(sales.data$lot.size, .99) |
                 sales.data$lot.size < quantile(sales.data$lot.size, .01))
  
  sales.data$discordant[lsd] <- 1
  sales.data$disc.fields[lsd] <- paste0(sales.data$disc.fields[lsd], 'lot.size | ')
  sales.data$disc.type[lsd] <- paste0(sales.data$disc.type[lsd], 'UV: 1% Quant | ')
  
  # Building Quality
  bsd <- which(sales.data$bldg.grade > quantile(sales.data$bldg.grade, .99) |
                 sales.data$bldg.grade < quantile(sales.data$bldg.grade, .01))
  
  sales.data$discordant[bsd] <- 1
  sales.data$disc.fields[bsd] <- paste0(sales.data$disc.fields[bsd], 'bldg.grade | ')
  sales.data$disc.type[bsd] <- paste0(sales.data$disc.type[bsd], 'UV: 1% Quant | ')
  
  # Bed Count
  dsd <- which(sales.data$beds > quantile(sales.data$beds, .99) |
                 sales.data$beds < quantile(sales.data$beds, .01))
  
  sales.data$discordant[dsd] <- 1
  sales.data$disc.fields[dsd] <- paste0(sales.data$disc.fields[dsd], 'beds | ')
  sales.data$disc.type[dsd] <- paste0(sales.data$disc.type[dsd], 'UV: 1% Quant | ')

  # Baths
  tsd <- which(sales.data$baths > quantile(sales.data$baths, .99) |
                 sales.data$baths < quantile(sales.data$baths, .01))
  
  sales.data$discordant[tsd] <- 1
  sales.data$disc.fields[tsd] <- paste0(sales.data$disc.fields[tsd], 'baths | ')
  sales.data$disc.type[tsd] <- paste0(sales.data$disc.type[tsd], 'UV: 1% Quant | ')
```

Finally, we find the home size and price relationship to be one of the strongest relationships in the data. From this relationship we label multivariate (MV) discordant values using a multiple step process.

We begin by extracting a data frame of just the sales price and home size

``` r
  x.data <- sales.data[,c('sale.price', 'tot.sf')]
```

We then estimate the Mahalanobis Distance for each point from their multivariate mean.

``` r
  x.data$x.mah <- mahalanobis(x.data, colMeans(x.data), cov(x.data))
```

We convert the Mahalanobis Distances into quantile at 80%, 90%, 95%, 99% and 99.9% and add this quantile measure as a factor to the data

``` r
  # Create cutoff points
  cuts <- quantile(x.data$x.mah, probs=c(0, .8, .9, .95, .99, .999, 1))
  
  # Add as a factor to the data
  x.data$mah.col <- as.numeric(as.factor(cut(x.data$x.mah, cuts)))
```

We then add the original discordancy measure to the new data.

``` r
  x.data$discordant <- sales.data$discordant
```

Next, we make two plots. The first plotting sale price against total square footage, with the univariate discordant values labeled. The second shows the quantile value of each observation based on its mahalabonis distance from the above calculation

``` r
  # Existing discordancy
  ex.disc <- ggplot(x.data, 
                    aes(x=tot.sf, y=sale.price, 
                    color=as.factor(discordant))) +
    geom_point() + 
    scale_x_log10() +
    scale_y_log10()
  
  # Multivariate discordancy
  mv.disc <- ggplot(x.data, 
                    aes(x=tot.sf, y=sale.price, color=as.factor(mah.col),
                    size=sqrt(mah.col) / 2)) +
    geom_point() +
    scale_x_log10() + scale_y_log10() +
    scale_color_manual(values=c('gray70', 'gray55', 'gray40', 'gray15', 'orange', 'red'))
  
  # Plot to viewer
  ggMultiPlots(ex.disc, mv.disc, cols=2)
```

Based on the multivariate analysis, we then label anything at the 99% or greater as multivariate discordant.

``` r
  x.disc <- which(x.data$mah.col >= 5)
  sales.data$discordant[x.disc] <- 1
  sales.data$disc.fields[x.disc] <- paste0(sales.data$disc.fields[x.disc], 'price v tot.sf | ')
  sales.data$disc.type[x.disc] <- paste0(sales.data$disc.type[x.disc], 'MV: 1% Mah Quant | ')
```

#### Write to database

To finish the exploration and data cleaning, we write the data (as **cleanSales**) back to the database, first converting the sale price back to a character value.

``` r
  # Convert dates to character
  sales.data$sales.date <- as.character(sales.data$sales.date)
  
  # Write to the database
  dbWriteTable(db.conn, 'cleanSales', sales.data, row.names=FALSE, overwrite=TRUE)
```

And then we close the connection to the database.

``` r
  dbDisconnect(db.conn)
```

Chapter 10
----------

In chapter we cover the price modeling process.

As always, we begin with a set of preliminary commands. First, we load the necessary libraries.

``` r
  library(sf)
  library(sp)
  library(tidyverse)
  library(RSQLite)
  library(RODBC)
  library(OpenStreetMap)
  library(BMA)
  library(MASS)
  library(rgeos)
  library(geosphere)
  library(spgwr)
  library(spdep)
  library(car)
  library(lmtest)
  library(plm)
```

Then we set the paths to the data and code.

``` r
  data.dir <- 'c:/temp/'
  code.dir <- 'c:/code/research/REAIA_book/'
```

We then load the custom functions.

``` r
  source(paste0(code.dir, 'custom_functions.R'))  
```

And finally we set the location to the database and establish a connection to it.

``` r
  # Set path to database
  data.db <- file.path(data.dir, 'seattleCaseStudy.db')
  
  # Establish connection
  db.conn <- dbConnect(dbDriver('SQLite'), data.db)
```

#### Load data

We begin by loading the sales data from the database.

``` r
    sales.data <- dbReadTable(db.conn, 'cleanSales')
```

Next, we transform the sales date to the R date format.

``` r
  sales.data$sales.date <- as.Date(sales.data$sales.date)
```

#### Basic Price Modeling

We begin the price modeling be specificy a basic linear model; one that includes most of the variables (generally non-spatial) that could possibly influence sales price.

``` r
  base.lm <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                  tot.sf + bsmt.sf + gar.bsmt.sf + gar.att.sf + deck.sf + 
                  bldg.grade + condition + eff.age + baths + beds + 
                  traffic.noise + view.best + is.waterfront + topo + restr.szshp +
                  sales.date,
                data=sales.data)
```

To further assist in the specification building process we employ two additional techniques: 1) stepwise regression; and 2) Bayesian Model Averaging (BMA).

We specify a step-wise regression model.

``` r
  step.lm <- stepAIC(base.lm, direction="both")
```

Next, we specify a selection model through Bayesian Model Averaging (BMA).

``` r
  bma.lm <- bicreg(y=log(sales.data$sale.price),
                   x=sales.data[ ,c('present.use', 'lot.size', 'tot.sf', 
                                    'bsmt.sf', 'gar.bsmt.sf', 'gar.att.sf', 'deck.sf',
                                    'bldg.grade', 'condition', 'eff.age', 'baths', 'beds',
                                    'traffic.noise', 'view.best', 'is.waterfront',
                                    'topo', 'restr.szshp', 'sales.date')])

  summary(bma.lm)
```

Based on the results from the stepwise and the BMA process, we make a number of transformations to the data.

First we combine home condition levels 1 and 2 into level 2

``` r
  sales.data$condition[sales.data$condition == 1] <- 2
```

Next, we combine the traffic noise levels 2 and 3 into level 2

``` r
  sales.data$traffic.noise[sales.data$traffic.noise == 3] <- 2
```

And then we combine the six smallest view.best categories into a single other view level.

``` r
  view.combine <- which(sales.data$view.best %in% c('view.other', 'view.smwater',
                                                    'view.lkwash', 'view.puget', 
                                                    'view.rainier', 'view.terr'))
  sales.data$view.best[view.combine] <- 'view.other'
```

After the transformations we retest the BMA process with the new variables.

``` r
  bma.lm <- bicreg(y=log(sales.data$sale.price),
                   x=sales.data[ ,c('present.use', 'lot.size', 'tot.sf', 
                                    'bsmt.sf', 'gar.bsmt.sf', 'gar.att.sf', 'deck.sf',
                                    'bldg.grade', 'condition', 'eff.age', 'baths', 'beds',
                                    'traffic.noise', 'view.best', 'is.waterfront',
                                    'topo', 'restr.szshp', 'sales.date')])

  summary(bma.lm)
```

Given the information from the stepwise and BMA models together with the variable transformations, we respecify the base linear model.

``` r
  base.lm <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                  tot.sf + bsmt.sf + gar.att.sf + deck.sf + 
                  bldg.grade + condition + eff.age + baths + beds + 
                  traffic.noise + view.best + is.waterfront +
                  sales.date,
                data=sales.data)
```

#### Diagnostics

We now run some diagnostics on the base model.

We start by testing for multicollinearity. we check the variance inflation factors (VIF) and find that none of them exceed 10, the standard warning level.

``` r
  vif(base.lm)
  sqrt(vif(base.lm)) > 2
```

Next, we test for heteroskedasticity with two Breusch-Pagan tests, one studentized, one not. We find heteroskedasticity in studentized test, but not in the other.

``` r
  # Studentized
  bptest(base.lm)
  
  # Not studentized
  ncvTest(base.lm)
```

Given the presence of heteroskedasticity, we then apply White's correction to the standard errors to see if any of the previously significant variables drop out.

``` r
  coeftest(base.lm, vcov=vcovHC(base.lm, "HC1"))
```

Next, we test for spatial autocorrelation in the residuals of our model.

We begin by converting the sales data to a spatial points data frame.

``` r
  sales.sp <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                           sales.data$latitude),
                                     sales.data)
```

We then build a spatial weights matrix of the observations, using an inverse distance-weighted, 10-nearest neighbors matrix.

``` r
  # Create neighbor list (use 10 nearest) 
  nbList <- knn2nb(knearneigh(sales.sp, 10))

  # Create Distances
  nbDists <- nbdists(nbList, sales.sp)    

  # Create a distance weighting function (.0025 is the nugget)
  dwf <- function(x) {1 / ((x + .00025) ^ 2)}
  
  # Build the SWM
  swm <- listw2U(nb2listw(nbList, 
                          glist = lapply(nbDists, dwf),
                          style="W",
                          zero.policy=T))
```

Using the spatial weights matrix, we then apply a Moran's I test. We find signification spatial autocorrelation.

``` r
  mi.test <- moran.test(base.lm$resid, swm, zero.policy=TRUE)
```

Next, we use a Lagrange Multiplier test to determine if the spatial dependence is in the dependent variable (spatial lag) or in the model errors (spatial error).

``` r
  lm.test <- lm.LMtests(base.lm, 
                        swm,
                        test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
```

#### Spatial Models

Finding spatial autocorrelation in the error terms of our model, we now specify a spatial error model. We use the same spatial weights matrix specified above.

``` r
   mod.se <- errorsarlm(as.formula(base.lm),
                        data=sales.sp,
                        swm, 
                        method="spam", 
                        zero.policy=TRUE)
```

We then test for spatial autocorrelation in the error terms of the spatial model.

``` r
   mi.test.se <- moran.test(mod.se$resid, swm, zero.policy=TRUE)
```

Spatial heterogeneity may also be found in a model covering a large geographic area the size of Seattle. We test for spatial heterogeneity in our model (and data) through the use of a geographically weighted regression (GWR).

We do so by first specificying the geographically weighted model (bandwidth=.1)

``` r
   mod.gwr <-gwr(base.lm, sales.sp, bandwidth=.1)
```

We then examine the results of the GWR and find some spatial heterogeniety.

``` r
    summary(mod.gwr$SDF@data)
```

#### Sensitivity Tests

We now test the sensitivity of our model to various changes in specification, discordant values and data sample. After specifying each model we compare the coefficient estimates to those of the base model to check for robustness of the model.

We begin with sensitivity to specification. First we re-estimate the final spatial error model, but remove any variables explicitly tied to the location of the observation (view, traffic noise, waterfront, etc.)

``` r
   mod.se.sens.s1 <- errorsarlm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                                     tot.sf + bsmt.sf + gar.att.sf + deck.sf + 
                                     bldg.grade + condition + eff.age + baths + beds + 
                                     sales.date,
                                data=sales.sp,
                                swm, 
                                method="spam", 
                                zero.policy=TRUE)
```

Next, we remove accessory home characteristics such as basement size, garage size, etc. from the model.

``` r
   mod.se.sens.s2 <- errorsarlm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                                  tot.sf + 
                                  bldg.grade + condition + eff.age + baths + beds + 
                                  sales.date,
                                data=sales.sp,
                                swm, 
                                method="spam", 
                                zero.policy=TRUE)
```

Then we remove bedrooms count, bathroom count and home condition, leaving a rather sparse model.

``` r
   mod.se.sens.s3 <- errorsarlm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                                  tot.sf + bldg.grade + eff.age + 
                                  sales.date,
                                data=sales.sp,
                                swm, 
                                method="spam", 
                                zero.policy=TRUE)
```

Finally, we re-specify the above as a simple ordinary least squares model.

``` r
   mod.se.sens.s4 <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                          tot.sf + bldg.grade + eff.age + 
                          sales.date,
                        data=sales.data)
```

Next, we test for sensitivity to discordant values.

First, we test for sensitivity to all discordant values. Because we are changing the sample of observations we must specificy the spatial weights matrix before re-estimating the model.

``` r
   # Extract only data with no discordant labels
   d1.data <- sales.sp[sales.sp@data$discordant == 0, ]
   
   # Build new SWM
   nbList <- knn2nb(knearneigh(d1.data, 10))
   
   # Create Distances
   nbDists <- nbdists(nbList, d1.data)    
   
   # Building Weights Matrix
   swm <- listw2U(nb2listw(nbList, 
                           glist = lapply(nbDists, dwf),
                           style="W",
                           zero.policy=T))
   
   # Estimate Model
   mod.sens.d1 <- errorsarlm(as.formula(base.lm),
                             data=d1.data,
                             swm, method="spam", 
                             zero.policy=TRUE)
```

Next, we just remove the Univariate discordant observations and re-estimate.

``` r
   # Extract only those with no univariate discordancy
   d2.id <- grep('UV:', sales.sp@data$disc.type)
   d2.data <- sales.sp[-d2.id, ]
   
   # Create neighbors list
   nbList <- knn2nb(knearneigh(d2.data, 10))
   
   # Create Distances
   nbDists <- nbdists(nbList, d2.data)    
   
   # Building Weights Matrix
   swm <- listw2U(nb2listw(nbList, 
                           glist = lapply(nbDists, dwf), 
                           style="W",
                           zero.policy=T))
  
   # Re-estimate model 
   mod.sens.d2 <- errorsarlm(as.formula(base.lm),
                             data=d2.data,
                             swm, 
                             method="spam", 
                             zero.policy=TRUE)
```

Finally, we just remove those observations with discordancy in the dependent variable.

``` r
   # Extract only those with no sale price discordancy    
   d3.id <- grep('sale.price', sales.data$disc.fields)
   d3.data <- sales.sp[-d3.id, ]
   
   # Create neighbors list
   nbList <- knn2nb(knearneigh(d3.data, 10))
   
   # Create Distances
   nbDists <- nbdists(nbList, d3.data)    
   
   # Building Weights Matrix
   swm <- listw2U(nb2listw(nbList, 
                           glist = lapply(nbDists, dwf), 
                           style="W",
                           zero.policy=T))
   
   # Re-estimate model
   mod.sens.d3 <- errorsarlm(as.formula(base.lm),
                             data=d3.data,
                             swm, 
                             method="spam", 
                             zero.policy=TRUE)
```

Our last set of sensitivity test examine the sensitivity of the model do the sample of data itself. We run 10 cross validation tests where each one uses only 75% of the original sample. We then compare the coefficient estimates from the 10 iterations to check the robustness of the model.

We begin by setting up a capture list and then starting a loop through 10 iterations.

``` r
   # Create capture list
   cv.list <- list()
   
   # Loop through and re-estimate model
   for(i in 1:10){
```

Within each iteration, we begin by setting a random seed and then selecting a random 75% sample from the data.

``` r
     # Set seed for consistent sampling
     set.seed(i)
     
     # Sample 75% of observations
     cv.id <- sample(1:nrow(sales.sp), round(nrow(sales.sp) * .75), 0)
     cv.data <- sales.sp[-cv.id, ]
```

Next, we re-specify the spatial weights matrix.

``` r
     # Calculate neighbors
     nbList <- knn2nb(knearneigh(cv.data, 10))
     
     # Create Distances
     nbDists <- nbdists(nbList, cv.data)    
     
     # Building Weights Matrix
     swm <- listw2U(nb2listw(nbList, 
                             glist = lapply(nbDists, dwf),
                             style="W",
                             zero.policy=T))
```

And then re-estimate the model.

``` r
     cv.list[[i]] <- errorsarlm(as.formula(base.lm),
                               data=cv.data,
                               swm, method="spam", zero.policy=TRUE)
    
  }
```

#### Save workspace

Instead of writing to the database, we save the entire workspace for use later.

``` r
  save.image(file.path(data.dir, 'model_workspace.RData'))
```

Chapter 12
----------

In this chapter we present a case study of combining traditional real estate pricing models with data non-traditional data from government and social media sources.

As always, we begin with a set of preliminary commands. First, we load the necessary libraries.

``` r
  library(sf)
  library(sp)
  library(tidyverse)
  library(RSQLite)
  library(RODBC)
  library(OpenStreetMap)
  library(BMA)
  library(MASS)
  library(rgeos)
  library(geosphere)
  library(spgwr)
  library(spdep)
  library(car)
  library(lmtest)
  library(gstat)
  library(scales)
```

Then we set the paths to the data and code.

``` r
  data.dir <- 'c:/temp/'
  code.dir <- 'c:/code/research/REAIA_book/'
```

We then load the custom functions.

``` r
  source(paste0(code.dir, 'custom_functions.R'))  
```

#### Load Data

We begin by loading the saved R workspace from the modeling chapter (10).

``` r
  load(file.path(data.dir, 'model_workspace.RData'))
```

Next, we load the three different format of the spatial representations of the police beat boundaries.

``` r
  load(file=file.path(data.dir, 'geographic/beats.Rdata'))
```

Finally, we load the crime and tweet sentiment data. First we define the path to the database, set up a connection and then read in both the crime and the tweet data.

``` r
  # Set database location
  data.db <- file.path(data.dir, 'seattleCaseStudy.db')
  
  # Open connection to database
  db.conn <- dbConnect(dbDriver('SQLite'), data.db)
  
  # Read in Crime data
  crime.data <- dbReadTable(db.conn, 'Crime')
  
  # Read in tweet sentiment data
  tweet.data <- dbReadTable(db.conn, 'SentimentTweets')
```

#### Relationship between Crime and Price

We begin the case study analysis by looking at the relationship between crime and housing prices in Seattle in 2016. We do so by adding the localized crime counts to our base model (from chapter 10).

``` r
  crime.lm <- lm(update(as.formula(base.lm), . ~ . + crime.violent + crime.property + crime.traffic +
                   crime.behavior + crime.other),
                 data=sales.data)
```

Noting that spatial autocorrelation was heavily present before, we then re-estimate the 'crime' model with a spatial error specification. To do so, we convert the data to a spatial points data frame, build a spatial weights matrix and then esimate the spatial error model.

``` r
  # Build Spatial Points Data Frame
  sales.sp <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                           sales.data$latitude),
                                     sales.data)
  
  # Create neighbor list
  nbList <- knn2nb(knearneigh(sales.sp, 10))
  
  # Create Distances
  nbDists <- nbdists(nbList, sales.sp)    
  
  # Build Spatial Weights Matrix
  swm <- listw2U(nb2listw(nbList, 
                          glist = lapply(nbDists, dwf),
                          style="W",
                          zero.policy=T))
  
  # Estimate model
  crime.se <- errorsarlm(as.formula(crime.lm),
                         data=sales.sp,
                         swm,
                         method="spam", 
                         zero.policy=TRUE)
```

Seeing that under the spatial error model there is little remaining influence on price from crime, we also examine if rates of appreciation (not the underlying price levels) bear any relationship to crime levels. We do this analysis at the beat level -- estimate appreciation vs crime rates per police beat.

We begin by building a table containing the count of crimes of each type per police beat.

``` r
  beat.crime <- dplyr::group_by(crime.data, zone.beat) %>% 
    dplyr::summarize(
      violent=length(which(crime.type =='violent')),
      property=length(which(crime.type == 'property')),
      behavior=length(which(crime.type == 'behavior')),
      traffic = length(which(crime.type == 'traffic')),
      other = length(which(crime.type == 'other')),
      all= n())
```

We then calculate the appreciation rate (from Q1 to Q4) for each beat.

To do so, we start by assigning each beat a blank value for appreciation rate and sales count.

``` r
  beat.crime$appr <- 0
  beat.crime$sales <- 0
```

Next, we loop through each beat

``` r
  for(b in 1:nrow(beat.crime)){
```

Within each iteration, we begin by selecting only those sales within the beat.

``` r
    beat.sales <- sales.data[sales.data$beat == beat.crime$zone.beat[b], ]
```

If there are at least 100 sales in the beat we proceed, if not we give that beat a 0 and disqualify it from the anlaysis.

``` r
    if(nrow(beat.sales) >= 100){
```

We convert the data to a spatial points data frame and then re-build a spatial weights matrix.

``` r
      # Create spatial points data frame
      beat.sp <- SpatialPointsDataFrame(cbind(beat.sales$longitude,
                                              beat.sales$latitude),
                                        beat.sales)
      
      # Make neighbors
      nbList <- knn2nb(knearneigh(beat.sp, 10))
      
      # Create Distances
      nbDists <- nbdists(nbList, beat.sp)    
      
      # Building Weights Matrix
      swm <- listw2U(nb2listw(nbList, 
                              glist = lapply(nbDists, dwf), 
                              style="W",
                              zero.policy=T))
```

We then remove the sales data from the model specification and add quarterly dummy variables.

``` r
      # Create model specification
      beat.spec <- as.formula(base.lm)
      beat.spec <- update(beat.spec, ~ . - sales.date)
      beat.spec <- update(beat.spec, ~ . + as.factor(qtr))
```

If there are no waterfront properties in this beat, we remove this variable from the model specification. If there are only one type of view in the beat we remove that variable as well.

``` r
      # Remove waterfront if none exist
      if(length(which(beat.sales$is.waterfront == 1)) == 0){
        beat.spec <- update(beat.spec, ~ . - is.waterfront)
      }
      
      # Remove views if none exist
      if(length(table(beat.sales$view.best)) <= 1){
        beat.spec <- update(beat.spec, ~ . - view.best)
      }
```

Then we estimate a price model for each beat.

``` r
      # Estimate the model
      beat.se <- tryCatch(errorsarlm(beat.spec,
                                     data=beat.sp,
                                     swm, 
                                     method="spam", 
                                     zero.policy=TRUE), silent=T)
```

If the model estimation succeeds, we then extract the coefficient indicating the change in prices from 2016 Q1 to 2016 Q4.

``` r
      # If model is successful
      if(class(beat.se) == 'sarlm'){
        
        # Extract all coefficients
        coefs <- summary(beat.se)$coefficients
        
        # Extract quarter 4 appreciation rate
        b.coef <- coefs[grep('qtr)4', names(coefs))]
        
        # Add to the beat summary data frame
        beat.crime$appr[b] <- b.coef
        
        # Add the count of sales to the beat summary data frame
        beat.crime$sales[b] <- nrow(beat.sales)
        
      } else {
```

If the model is unsuccessful or if there are fewer than 100 sales in the beat, then we set the appreciation rate to NA. This closes the loop through the police beats.

``` r
        beat.crime$appr[b] <- NA
      
      }
      
    } else {
      
      beat.crime$appr[b] <- NA
    }  
  }
```

Next, we add the size of each beat to the beat summary data frame.

``` r
  beat.crime$area <- beats.sp@data$size[match(beat.crime$zone.beat,
                                              beats.sp@data$beat)]
```

We then remove all beats for which an appreciation rate was not estimated.

``` r
  beat.crime <- beat.crime[!is.na(beat.crime$appr), ]
```

Then, we convert all raw counts of crime into crime per square mile figures.

``` r
  beat.crime$viol.area <- beat.crime$violent / beat.crime$area
  beat.crime$prop.area <- beat.crime$property / beat.crime$area
  beat.crime$beha.area <- beat.crime$behavior / beat.crime$area
  beat.crime$traf.area <- beat.crime$traffic / beat.crime$area
  beat.crime$othe.area <- beat.crime$other / beat.crime$area
  beat.crime$all.area <- beat.crime$all / beat.crime$area
```

Finally, we plot the relationship between price appreciation and crimes per square mile for all of the applicable beats across all five crime types as well as the aggregated count of crimes.

``` r
  # All crime
  all.crime.plot <- ggplot(beat.crime, 
                           aes(x=all.area, y=appr)) + 
                      geom_point() +
                      stat_smooth(se=FALSE, size=2) +
                      xlab("Reported Crimes per Sq. Mile") + 
                      ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
                      ggtitle('All Crime vs Appreciation') +
                      theme(plot.title = element_text(hjust = 0.5))
  
  # Violent crime
  viol.crime.plot <- ggplot(beat.crime, 
                           aes(x=viol.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Violent vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))

  # Property crime
  prop.crime.plot <- ggplot(beat.crime, 
                            aes(x=prop.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Property vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Behavior crime
  beha.crime.plot <- ggplot(beat.crime, 
                            aes(x=beha.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Behavioral vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Property crime
  traf.crime.plot <- ggplot(beat.crime, 
                            aes(x=traf.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Traffic vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Other crime
  othe.crime.plot <- ggplot(beat.crime, 
                            aes(x=othe.area, y=appr)) + 
    geom_point() +
    stat_smooth(se=FALSE, size=2) +
    xlab("Reported Crimes per Sq. Mile") + 
    ylab("2016 Appreciation Rate:\nQ1 to Q4\n") + 
    ggtitle('Other vs Appreciation') +
    theme(plot.title = element_text(hjust = 0.5))
```

#### Relationship between Sentiment and House Prices

The final section of our case study looks at the relationship between local sentiment (as measured by Twitter) and localized house price changes.

We begin by converting the tweet data to a SpatialPointsDataFrame. This includes changing the coordinate reference system.

``` r
  # Conver to SPDF
  tweet.sp <- SpatialPointsDataFrame(cbind(tweet.data$longitude, tweet.data$latitude),
                                     data=tweet.data)

  # Fix the coordinate reference system
  proj4string(tweet.sp) <- CRS(proj4string(beats.sp))
```

We then make a simple map showing the location of the sentiment-scored tweets.

``` r
 ## Make plot of sentiment tweets
    
  sent.map <- ggplot() + 
    geom_polygon(data=beats.spf, aes(x=long, y=lat, group=beat), 
                 color='gray40', fill='gray80')+
    # coord_cartesian(xlim=c(min(sales.data$longitude), max(sales.data$longitude)),
    #                 ylim=c(min(sales.data$latitude), max(sales.data$latitude))) +
     geom_point(data=tweet.data, 
                aes(x=longitude, y=latitude, color=as.factor(SS)), 
                size=2) +
    scale_color_manual(values=c('blue', 'red'),
                       labels=c("Negative   ", 'Positive    '),
                       name='Sentiment    ') +
    ylab('') + 
    xlab('') +
    theme(legend.position='bottom', 
          axis.text=element_blank(),
          axis.ticks = element_blank())
```

Next, we take the point data tweets and convert them to a surface (areal coverage) using a custom point to surface function. (Detailed code on the point to surface process can be found in the *custom\_functions.R* file.)

``` r
 ## Create sentiment surface
  
  # Build surface
  sent.surf <- point2Surface(twsent.sp, 
                             twsent.sp$SS, 
                             res=.004, 
                             clip=seattle.bound, 
                             idp.val=5)
```

We extract the values from this surface and convert them to a simple data frame object.

``` r
  surf.df <- data.frame(long=sent.surf@coords[,1],
                        lat=sent.surf@coords[,2],
                        sentiment=sent.surf@data$var1.pred)
```

We then make a map showing the localized sentiment in each area of Seattle.

``` r
  sent.map <- ggplot(surf.df, aes(x=long, y=lat, z=sentiment)) +
    geom_tile(aes(fill = sentiment), alpha=.7) +
    scale_fill_gradient2(low=muted('red'), high=muted('blue'),
                         name='Sentiment    ',
                         breaks=c(-.75, 0, .75),
                         labels=c('Negative', 'Neutral', 'Positive')) + 
    ylab('') + 
    xlab('') +
    ggtitle('Sentiment in Seattle') +
    theme(legend.position='bottom',
          legend.key.width=unit(3,'cm'),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5))
```

Next, we estimate the local price appreciation at the same scale as the sentiment surface. To do so, we use a Geographycally Weighted Regression (GWR).

We begin by updating the base regression specification by removing sales date and insert a quarterly dummy variable.

``` r
  gwr.spec <- update(as.formula(base.lm), ~ . - sales.date)
  gwr.spec <- update(gwr.spec, ~ . + as.factor(qtr))
```

We then convert the sales data into a SpatialPointsDataFrame object.

``` r
  gwr.data <- SpatialPointsDataFrame(cbind(sales.data$longitude,
                                           sales.data$latitude),
                                     data=sales.data)
```

And then, estimate a geographically weighted regression, estimating a model at each one of the grid points in the sentiment surface.

``` r
  price.gwr <-gwr(gwr.spec, gwr.data, fit.points=sent.surf@coords, bandwidth=.01)
```

Next, we extract the coefficients for price appreciation (from Q1 to Q4) from the GWR results and add then to the data frame containing the local sentiment scores.

``` r
  # Extract the coefficients for Q4 appreciations
  gwr.coef <- price.gwr$SDF@data
  appr.coef <- gwr.coef[, ncol(gwr.coef)]
  
  # Add these to the data frame of surfact values
  surf.df$appr <- appr.coef
```

And finally, we plot the relationship between the local sentiment scores and the local price appreciation.

``` r
  sent.appr.plot <- ggplot(surf.df, aes(x=sentiment, y=appr)) + 
    geom_point(alpha=.4, size=.9) + 
    stat_smooth(size=2, se=TRUE, color='red') +
    scale_y_continuous(breaks=seq(0,.08, .01),
                       labels=c('0%', '1%', '2%', '3%', '4%', '5%', '6%', '7%', '8%')) +
    xlab('Sentiment Score') +
    ylab('Appreciation Rate in 2016\n') +
    # coord_cartesian(ylim=c(-.07, .17)) + 
    ggtitle('Local House Appreciation vs Sentiment') +
    theme(plot.title = element_text(hjust = 0.5))
```

#### Save workspace for Case Study analysis

We conclude the case studies by save the entire workspace to an R data object for future tasks such as visualization.

``` r
  save.image(file.path(data.dir, 'casestudy_workspace.RData'))
```

[1] R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <https://www.R-project.org/>.

[2] This can be reduced by deleted the downloaded ZIP files after extraction.
