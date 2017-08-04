##########################################################################################
#                                                                                        #            
#  Code for Chapter 8 (Explore) and 9 (Clean)                                            #
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
  library(GGally)
  library(ggplot2)

 ## Set data and code directory

  data.dir <- 'c:/temp/'
  code.dir <- 'c:/code/REAIAbook/'

 ## Load custom source files

  source(paste0(code.dir, 'custom_functions.R'))  

 ## Set the database path and name  

  data.db <- file.path(data.dir, 'seattleCaseStudy.db')

### Load data ----------------------------------------------------------------------------  
  
  # Read in Sales File
  db.conn <- dbConnect(dbDriver('SQLite'), data.db)
  sales.data <- dbReadTable(db.conn, 'prepSales')

  # Load City Police Beats Data
  load(file=file.path(data.dir, 'geographic/beats.Rdata'))

 ## Data conversion
  
  # Fix date issue
  sales.data <- dplyr::mutate(.data=sales.data,
                               sales.date=as.Date(sales.date))
  
  # Fix categorical varialbes
  cat.vars <- c('present.use', 'zoning', 'topo', 'restr.szshp',
                'view.rainier', 'view.olympics', 'view.cascades',
                'view.terr', 'view.city', 'view.puget', 'view.lkwash',
                'view.lksamm', 'view.smwater', 'view.other', 'wfnt', 'wfnt.bank',
                'traffic.noise', 'bldg.grade', 'condition', 'beat')
  
  for(cv in cat.vars) sales.data[, cv] <- as.factor(sales.data[, cv])
  
### Univariate Exploration ---------------------------------------------------------------
  
 ## Univariate Statistical  
  
  # Find field types
  field.types <- unlist(lapply(sales.data, class))
  
  # Compute summary table for numeric and integer variables
  summ.table <- t(do.call(cbind, 
                        lapply(sales.data[,field.types == 'numeric' |
                                           field.types == 'integer'],
                               fullSummary)))
  
  # Show the table
  summ.table

  # Compute summary table for factor variables
  summary(sales.data[, field.types == 'factor'])
  
 ## Filter out errors of value
  
  # Errors in Sale Price
  head(sales.data$sale.price[order(sales.data$sale.price)], 300)
  sales.data <- dplyr::filter(sales.data, sale.price >= 50000)

  # Errors in Home Size
  head(sales.data$tot.sf[order(sales.data$tot.sf)], 300)
  sales.data <- dplyr::filter(sales.data, tot.sf >= 300)
  
  # Errors in Beds
  tail(sales.data$beds[order(sales.data$beds)], 300)
  sales.data <- dplyr::filter(sales.data, beds <= 10)
  
 ## Univariate Visual/Informal
  
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
  
  # Homs Size
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
  
  # Make Plots
  ggMultiPlots(st.hist, bd.hist, bt.hist, cols=3)
  
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
  
  # Make Plot 1
  ggMultiPlots(pu.hist, zo.hist, tp.hist,
               rs.hist, vr.hist, vo.hist,
               vc.hist, vt.hist, vy.hist,
               cols=3)
  
  # Make Plot 2
  ggMultiPlots(vp.hist, vw.hist, vs.hist,
               vb.hist, v0.hist, wt.hist,
               wb.hist, tn.hist, bg.hist,
               bc.hist, pb.hist, 
               cols=3)

### Multivariate Exploration -------------------------------------------------------------  
  
  sales.data$view.lksamm <- NULL
  sales.data$is.waterfront <- ifelse(sales.data$wfnt != 0, 1, 0)

 ## Multivariate Statistical
  
  sales.data$beds <- as.numeric(as.character(sales.data$beds))
  sales.data$baths <- as.numeric(as.character(sales.data$baths))
  sales.data$bldg.grade <- as.numeric(as.character(sales.data$bldg.grade))

 ## Correlation Plot
  
  ggcorr(sales.data[,c('sale.price', 'tot.sf', 'lot.size','beds', 'baths',
                       'age', 'bldg.grade')], label=TRUE)
  
 ## Multivariate Visual/Informal  
  
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
  
  # Present Use and Sale Price
  ggplot(sales.data,
         aes(x=present.use, y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()
  #scale_x_log10() +
  #stat_smooth()
  
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
  
  # Make the View variable and plot  
  has.view <- as.data.frame(lapply(sales.data[,14:22], 
                             function(x) ifelse(as.numeric(x) == 1, 0, 1)))
  
  # Small custom function to extract
  getX <- function(x){
    if(max(x) == 0){
      return(0)
    } else {
      return(which(x == max(x))[1])
    }
  }
  
  # Get max
  max.view <- apply(has.view, 1, getX)
  
  # View Type
  view.type <- rep('no view', length(max.view))
  view.type[max.view != 0] <- names(has.view)[max.view[max.view != 0]]
  
  # Add to sales.data
  sales.data$view.best <- view.type
  
  # Plot View Type
  ggplot(sales.data,
         aes(x=view.best, y=sale.price))+
    geom_boxplot() +  
    scale_y_log10()

  sales.data$view.mtn <- ifelse(sales.data$view.rainier != 0 |
                                  sales.data$view.olympics != 0 |
                                  sales.data$view.cascades != 0,
                                1, 0)
  sales.data$view.water <- ifelse(sales.data$view.puget != 0 |
                                    sales.data$view.lkwash != 0 |
                                    sales.data$view.smwater != 0,
                                  1, 0)
  sales.data$view.other <- ifelse(sales.data$view.city != 0 |
                                    sales.data$view.terr != 0 |
                                    sales.data$view.other != 0,
                                  1, 0)
  
  view.scores <- as.data.frame(lapply(sales.data[, 14:22], 
                                      function(x) as.numeric(as.character(x))))
  
  sales.data$view.score <- rowSums(view.scores)

### Mapping ------------------------------------------------------------------------------  
  
  # Make the base map
  base.map <- ggplot(sales.data,
                     aes(x=longitude, 
                         y=latitude)) +
    geom_point(color='gray40', size=.1)
  
  base.map <- base.map + geom_path(data=beats.spf,
                                   aes(x=long, y=lat, group=id),
                                   color='gray20')
  base.map
  
  # Make a price map
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
  
### Label discordant values --------------------------------------------------------------
  
  sales.data$discordant <- 0
  sales.data$disc.fields <- ""
  sales.data$disc.type <- ""
  
  # Restrictive Shape/Size
  rss <- which(sales.data$restr.szshp != 0)
  sales.data$discordant[rss] <- 1
  sales.data$disc.fields[rss] <- paste0(sales.data$disc.fields[rss], 'restr.szshp | ')
  sales.data$disc.type[rss] <- paste0(sales.data$disc.type[rss], 'small category | ')
  
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
  
 ## Mlltivariate discordants    
  
  # Price V Size
  x.data <- sales.data[,c('sale.price', 'tot.sf')]
  x.data$x.mah <- mahalanobis(x.data, colMeans(x.data), cov(x.data))
  
  # Create cutoff points
  cuts <- quantile(x.data$x.mah, probs=c(0, .8, .9, .95, .99, .999, 1))
  
  # Add as a factor to the data
  x.data$mah.col <- as.numeric(as.factor(cut(x.data$x.mah, cuts)))
  
  # Add original discordant labels to data
  x.data$discordant <- sales.data$discordant
  
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

  # Label PVS
  x.disc <- which(x.data$mah.col >= 5)
  sales.data$discordant[x.disc] <- 1
  sales.data$disc.fields[x.disc] <- paste0(sales.data$disc.fields[x.disc], 'price v tot.sf | ')
  sales.data$disc.type[x.disc] <- paste0(sales.data$disc.type[x.disc], 'MV: 1% Mah Quant | ')
  
### Write out ----------------------------------------------------------------------------
  
  # Write to the database
  sales.data$sales.date <- as.character(sales.data$sales.date)
  dbWriteTable(db.conn, 'cleanSales', sales.data, row.names=FALSE, overwrite=TRUE)
  
  # Close
  dbDisconnect(db.conn)

##########################################################################################
##########################################################################################
  

  
  
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
