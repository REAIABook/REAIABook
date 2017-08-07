##########################################################################################
#                                                                                        #            
#  Code for Chapter 10 (Price Model)                                                     #
#   of Real Estate Analysis in the Information Age                                       #   
#                                                                                        #                  
##########################################################################################

### Preliminary Commands -----------------------------------------------------------------

 ## Load Libraries

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

 ## Set data and code directory

  data.dir <- 'c:/temp/'
  code.dir <- 'c:/code/reaiabook/'

 ## Load custom source files

  source(paste0(code.dir, 'custom_functions.R'))  

 ## Set the database path and name  

  data.db <- file.path(data.dir, 'seattleCaseStudy.db')
  
### Load data ----------------------------------------------------------------------------  

 ## Read in Sales File

  db.conn <- dbConnect(dbDriver('SQLite'), data.db)
  sales.data <- dbReadTable(db.conn, 'cleanSales')

 ## Fix Dates
  
  sales.data$sales.date <- as.Date(sales.data$sales.date)
  
### Basic Modeling -----------------------------------------------------------------------

 ## Specify Base Model

  base.lm <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                  tot.sf + bsmt.sf + gar.bsmt.sf + gar.att.sf + deck.sf + 
                  bldg.grade + condition + eff.age + baths + beds + 
                  traffic.noise + view.best + is.waterfront + topo + restr.szshp +
                  sales.date,
                data=sales.data)

 ## Apply a stepwise regression model

  step.lm <- stepAIC(base.lm, direction="both")

 ## Apply a Bayesian Model Averaging specification search

  bma.lm <- bicreg(y=log(sales.data$sale.price),
                   x=sales.data[ ,c('present.use', 'lot.size', 'tot.sf', 
                                    'bsmt.sf', 'gar.bsmt.sf', 'gar.att.sf', 'deck.sf',
                                    'bldg.grade', 'condition', 'eff.age', 'baths', 'beds',
                                    'traffic.noise', 'view.best', 'is.waterfront',
                                    'topo', 'restr.szshp', 'sales.date')])

  summary(bma.lm)
  
 ## Make transformations

  # Combine condition levels 1 and 2
  sales.data$condition[sales.data$condition == 1] <- 2
  
  # Combine Traffic levels 2 and 3
  sales.data$traffic.noise[sales.data$traffic.noise == 3] <- 2

  # Combine the insignificant view levels in a single factor
  view.combine <- which(sales.data$view.best %in% c('view.other', 'view.smwater',
                                                    'view.lkwash', 'view.puget', 
                                                    'view.rainier', 'view.terr'))
  sales.data$view.best[view.combine] <- 'view.other'

 ## Retest the BMA model  
  
  bma.lm <- bicreg(y=log(sales.data$sale.price),
                   x=sales.data[ ,c('present.use', 'lot.size', 'tot.sf', 
                                    'bsmt.sf', 'gar.bsmt.sf', 'gar.att.sf', 'deck.sf',
                                    'bldg.grade', 'condition', 'eff.age', 'baths', 'beds',
                                    'traffic.noise', 'view.best', 'is.waterfront',
                                    'topo', 'restr.szshp', 'sales.date')])

  summary(bma.lm)
  
 ## Respecify the model  
  
  base.lm <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                  tot.sf + bsmt.sf + gar.att.sf + deck.sf + 
                  bldg.grade + condition + eff.age + baths + beds + 
                  traffic.noise + view.best + is.waterfront +
                  sales.date,
                data=sales.data)

### Diagnostics --------------------------------------------------------------------------

 ## Multicollinearity

  # Check VIF
  vif(base.lm)
  sqrt(vif(base.lm)) > 2

 ## Heteroskedasticity

  # Studentized
  bptest(base.lm)
  
  # Not studentized
  ncvTest(base.lm)
  
  # Check white's errors
  coeftest(base.lm, vcov=vcovHC(base.lm, "HC1"))
  
 ## Spatial Autocorrelation

  # Convert to a SpatialPointsDataFrame
  sales.sp <- SpatialPointsDataFrame(coords=cbind(sales.data$longitude,
                                                  sales.data$latitude),
                                     data=sales.data)

  # Develop a spatial weights matrix (SWM)
 
  # Create neighbor list (use 10 nearest) 
  nbList <- knn2nb(knearneigh(sales.sp, 10))

  # Create Distances
  nbDists <- nbdists(nbList, sales.sp)    

  # Create a distance weighting function (.0025 is the nugget)
  dwf <- function(x) {1 / ((x + .00025) ^ 2)}
  
  # Build the SWM
  swm <- listw2U(nb2listw(nbList, 
                          glist=lapply(nbDists, dwf),
                          style="W",
                          zero.policy=T))
  
  # Test for spatial dependence in the error terms with Moran's I
  mi.test <- moran.test(base.lm$resid, swm, zero.policy=TRUE)
 
  # Determine form of spatial dependence
  lm.test <- lm.LMtests(base.lm, 
                        swm,
                        test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
 
### Spatial Models -----------------------------------------------------------------------  
  
 ## Specify spatial error Model
   
   mod.se <- errorsarlm(as.formula(base.lm),
                        data=sales.sp,
                        swm, 
                        method="spam", 
                        zero.policy=TRUE)
 
   # Test the moran's I of the spatial error model
   mi.test.se <- moran.test(mod.se$resid, swm, zero.policy=TRUE)
   
 ## Test for Spatial Heterogeneity 
   
    # Specify a geographically weighted regression model
    mod.gwr <-gwr(base.lm, sales.sp, bandwidth=.1)
    
    # Summarize the coefficient estimates
    summary(mod.gwr$SDF@data)

### Sensitivity Test ---------------------------------------------------------------------

  ## Sensitivity to Specification
  
   # Remove spatial Variables
   mod.se.sens.s1 <- errorsarlm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                                     tot.sf + bsmt.sf + gar.att.sf + deck.sf + 
                                     bldg.grade + condition + eff.age + baths + beds + 
                                     sales.date,
                                data=sales.sp,
                                swm, 
                                method="spam", 
                                zero.policy=TRUE)
   
   # Remove accessory home variables
   mod.se.sens.s2 <- errorsarlm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                                  tot.sf + 
                                  bldg.grade + condition + eff.age + baths + beds + 
                                  sales.date,
                                data=sales.sp,
                                swm, 
                                method="spam", 
                                zero.policy=TRUE)
   
   # Remove bed/bath/condition
   mod.se.sens.s3 <- errorsarlm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                                  tot.sf + bldg.grade + eff.age + 
                                  sales.date,
                                data=sales.sp,
                                swm, 
                                method="spam", 
                                zero.policy=TRUE)
   
   # Simples model as OLS
   mod.se.sens.s4 <- lm(log(sale.price) ~ as.factor(present.use) + lot.size + 
                          tot.sf + bldg.grade + eff.age + 
                          sales.date,
                        data=sales.data)
   
  ## Sensitivity to Discordant Values
  
  ## Extract only data with no discordant labels
   d1.data <- sales.sp[sales.sp@data$discordant == 0, ]
   
   # Build new SWM
   nbList <- knn2nb(knearneigh(d1.data, 10))
   
   # Create Distances
   nbDists <- nbdists(nbList, d1.data)    
   
   # Building Weights Matrix
   swm <- listw2U(nb2listw(nbList, 
                           glist=lapply(nbDists, dwf),
                           style="W",
                           zero.policy=T))
   
   # Estimate Model
   mod.sens.d1 <- errorsarlm(as.formula(base.lm),
                             data=d1.data,
                             swm, 
                             method="spam", 
                             zero.policy=TRUE)
  
  ## Extract only those with no univariate discordancy
   d2.id <- grep('UV:', sales.sp@data$disc.type)
   d2.data <- sales.sp[-d2.id, ]
   
   # Create neighbors list
   nbList <- knn2nb(knearneigh(d2.data, 10))
   
   # Create Distances
   nbDists <- nbdists(nbList, d2.data)    
   
   # Building Weights Matrix
   swm <- listw2U(nb2listw(nbList, 
                           glist=lapply(nbDists, dwf), 
                           style="W",
                           zero.policy=T))
  
   # Re-estimate model 
   mod.sens.d2 <- errorsarlm(as.formula(base.lm),
                             data=d2.data,
                             swm, 
                             method="spam", 
                             zero.policy=TRUE)
   
 ## Extract only those with no sale price discordancy    
   d3.id <- grep('sale.price', sales.data$disc.fields)
   d3.data <- sales.sp[-d3.id, ]
   
   # Create neighbors list
   nbList <- knn2nb(knearneigh(d3.data, 10))
   
   # Create Distances
   nbDists <- nbdists(nbList, d3.data)    
   
   # Building Weights Matrix
   swm <- listw2U(nb2listw(nbList, 
                           glist=lapply(nbDists, dwf), 
                           style="W",
                           zero.policy=T))
   
   # Re-estimate model
   mod.sens.d3 <- errorsarlm(as.formula(base.lm),
                             data=d3.data,
                             swm, 
                             method="spam", 
                             zero.policy=TRUE)
   
 ## Sensitivity to the sample 
   
   # Create capture list
   cv.list <- list()
   
   # Loop through and re-estimate model
   for(i in 1:10){
     
     # Set seed for consistent sampling
     set.seed(i)
     
     # Sample 75% of observations
     cv.id <- sample(1:nrow(sales.sp), round(nrow(sales.sp) * .75), 0)
     cv.data <- sales.sp[-cv.id, ]
     
     # Calculate neighbors
     nbList <- knn2nb(knearneigh(cv.data, 10))
     
     # Create Distances
     nbDists <- nbdists(nbList, cv.data)    
     
     # Building Weights Matrix
     swm <- listw2U(nb2listw(nbList, 
                             glist=lapply(nbDists, dwf),
                             style="W",
                             zero.policy=T))
     
     # Restimate and add to capture list
     cv.list[[i]] <- errorsarlm(as.formula(base.lm),
                                data=cv.data,
                                swm, 
                                method="spam", 
                                zero.policy=TRUE)
    
   }

### Save workspace for Case Study analysis -----------------------------------------------
  
  save.image(file.path(data.dir, 'model_workspace.RData'))

##########################################################################################
