##########################################################################################
#                                                                                        #            
#  Code for Chapter 4 (Gather) of Real Estate Analysis in the Information Age            #   
#                                                                                        #                  
##########################################################################################
 
### Set up directories -------------------------------------------------------------------

 ## Set your data directory

  data.dir <- 'c:/temp/'

 ## Set up a capture location for downloaded files
  
  # Create a directory if one doesn't exist
  if (!dir.exists(file.path(data.dir, 'raw_zip_files'))){
    dir.create(file.path(data.dir, 'raw_zip_files'))
  }
  
### Gather and extract the Seattle Police Beat Shapefile ---------------------------------

  # Check if file exists
  if (!file.exists(file.path(data.dir, 'raw_zip_files', 'beats.zip'))){
  
    # If it doesn't, then download
    download.file(url=paste0('https://data.seattle.gov/views/nnxn-434b/files/',
                         '96d998d4-ae20-4ea8-b912-436e68982a0d.zip'), 
                  destfile=file.path(data.dir, 'raw_zip_files', 'beats.zip'))
  }  

  # Create a directory for beats data if one doesn't exist
  if (!dir.exists(file.path(data.dir, 'beats'))){
    dir.create(file.path(data.dir, 'beats'))
  }
  
  # Unzip the files
  unzip(zipfile=file.path(data.dir, 'raw_zip_files', 'beats.zip'),
        exdir=file.path(data.dir, 'beats'))
        
### Gather and extract the King County Parcel Shapefile ----------------------------------
  
  # Check if file exists
  if (!file.exists(file.path(data.dir, 'raw_zip_files', 'parcel_shapefile.zip'))){
    
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
  
### Gather and extract the King County Assessor Data Files -------------------------------
  
  # Create a directory for assessor data if one doesn't exist
  if (!dir.exists(file.path(data.dir, 'assessor'))){
    dir.create(file.path(data.dir, 'assessor'))
  }
  
 ## Sales Transaction File  
  
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

 ## Parcel (land) information file  
  
  # Check if file exists
  if (!file.exists(file.path(data.dir, 'raw_zip.files', 'parcel.zip'))){
    
    # If it doesn't, then download
    download.file(url='http://your.kingcounty.gov/extranet/assessor/Parcel.zip', 
                  destfile=file.path(data.dir, 'raw_zip_files', 'parcel.zip'))
  }
  
  # Unzip
  unzip(zipfile=file.path(data.dir, 'raw_zip_files', 'parcel.zip'), 
        exdir=file.path(data.dir, 'assessor'))
  
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

### Gather Crime Statistics Data ---------------------------------------------------------  

  # Create a directory for assessor data if one doesn't exist
  if (!dir.exists(file.path(data.dir, 'crime'))){
    dir.create(file.path(data.dir, 'crime'))
  }
  
  # Download .csv from the City of Seattle
  download.file(url=paste0('https://data.seattle.gov/api/views/7ais-f98f/rows.csv?',
                           'accessType=DOWNLOAD'), 
                destfile=file.path(data.dir, 'crime', 'seattle_crime.csv' ))
  
### Gather Twitter Sentiment Data --------------------------------------------------------  
  
  # Create a directory for assessor data if one doesn't exist
  if (!dir.exists(file.path(data.dir, 'tweets'))){
    dir.create(file.path(data.dir, 'tweets'))
  }
  
  # Download .csv from the City of Seattle
  download.file(url=paste0('http://raw.githubusercontent.com/REAIABook/',
                           'REAIABook/master/tweetSentiment.csv'), 
                destfile=file.path(data.dir, 'tweets', 'sentimenttweets.csv' ))
  
##########################################################################################
##########################################################################################
