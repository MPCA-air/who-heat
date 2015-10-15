######### All the measures of inequality
# This manages the return of the inequalities measures inequality measures
#########



getInequalData <- function(indicator = NULL, stratifier = NULL, countries = NULL, years = NULL,
                       mostrecent=NULL, datasource=NULL,  inequal_types=NULL, multiplier1=TRUE,multiplier2=TRUE, 
                       elasticity=NULL){

  #browser()
  if(!is.null(elasticity)){
    # if mostrecent is TRUE then we want the focus_year, otherwise
    # we can use the input year
    years <- as.integer(years)
    years<-seq(years-elasticity, years+elasticity)
    
    
  }
  
  #
  filt_country <- TRUE
  filt_year <- TRUE
  filt_indicator <- TRUE
  filt_dimension <- TRUE
  filt_inequaltype<- TRUE
  filt_datasource <- TRUE
  
  if(!is.null(countries)) filt_country <- quote(country %in% countries)
  if(!is.null(years) && !is.null(mostrecent) && !mostrecent) filt_year <- quote(year %in% years)
  if(!is.null(years) && is.null(mostrecent)) filt_year <- quote(year %in% years)
  if(!is.null(indicator)) filt_indicator <- quote(indic %in% indicator)
  if(!is.null(stratifier)) filt_dimension <- quote(dimension %in% stratifier)
  if(!is.null(inequal_types)) filt_inequaltype <- quote(measure %in% inequal_types)
  if(!is.null(datasource) && datasource == 'All') filt_datasource <- TRUE
  if(!is.null(datasource) && datasource != 'All') filt_datasource <- quote(source == datasource)
  
  #     print(paste0('filt_country:', deparse(filt_country)))
  #     print(paste0('filt_year:', deparse(filt_year)))
  #     print(paste0('filt_indicator:', deparse(filt_indicator)))
  #     print(paste0('filt_dimension:', deparse(filt_dimension)))
  #     
  #     print(paste0('country:', countries))
  #     print(paste0('years:', years))
  #     print(paste0('indicator:', indicator))
  #     print(paste0('dimension:', stratifier))
  
  ineqDF <- filter(.rdata[['inequals']], filt_country, filt_year, filt_indicator, 
                   filt_dimension, filt_inequaltype, filt_datasource) %>% 
    select(country, year, indic, dimension,source, measure, inequal, boot.se, se, ccode)
  
  #   if(is.null(ineqDF)){
  #     return()
  #   }
  
  ineqDF$year <- as.integer(ineqDF$year)
  ineqDF$boot.se <- as.numeric(ineqDF$boot.se)
  ineqDF$boot.se[ineqDF$boot.se==0] <- NA
  ineqDF$se <- as.numeric(ineqDF$se)
  ineqDF$se[ineqDF$se == 0] <- NA
  ineqDF$combo.se <- ineqDF$se
  ineqDF$combo.se[is.na(ineqDF$se)] <- ineqDF$boot.se[is.na(ineqDF$se)]
  
  ineqDF$boot.lowerci <- ineqDF$inequal - (1.96 * ineqDF$boot.se) 
  ineqDF$boot.upperci <- ineqDF$inequal + (1.96 * ineqDF$boot.se) 
  ineqDF$se.lowerci <- ineqDF$inequal - (1.96 * ineqDF$se) 
  ineqDF$se.upperci <- ineqDF$inequal + (1.96 * ineqDF$se) 
  ineqDF$combo.lowerci <- ineqDF$inequal - (1.96 * ineqDF$combo.se) 
  ineqDF$combo.upperci <- ineqDF$inequal + (1.96 * ineqDF$combo.se) 
  
  
  
  ineqDF$combo.se[is.na(ineqDF$combo.se)] <- ineqDF$boot.se[is.na(ineqDF$combo.se)]  #  Make an se that is analytic if it exists, otherwise a boostrap
  #print("In getInequal function b")
  
  
  if(!is.null(multiplier1) && multiplier1){
    #print("In dataTableInequal a")
    ineqDF$inequal[ineqDF$measure=='ti'] <- ineqDF$inequal[ineqDF$measure=='ti'] *1000
    ineqDF$inequal[ineqDF$measure=='mld'] <- ineqDF$inequal[ineqDF$measure=='mld'] *1000
    ineqDF$se[ineqDF$measure=='ti'] <- ineqDF$se[ineqDF$measure=='ti'] *1000
    ineqDF$se[ineqDF$measure=='mld'] <- ineqDF$se[ineqDF$measure=='mld'] *1000
    ineqDF$se.lowerci[ineqDF$measure=='ti'] <- ineqDF$se.lowerci[ineqDF$measure=='ti'] *1000
    ineqDF$se.lowerci[ineqDF$measure=='mld'] <- ineqDF$se.lowerci[ineqDF$measure=='mld'] *1000
    ineqDF$se.upperci[ineqDF$measure=='ti'] <- ineqDF$se.upperci[ineqDF$measure=='ti'] *1000
    ineqDF$se.upperci[ineqDF$measure=='mld'] <- ineqDF$se.upperci[ineqDF$measure=='mld'] *1000
    ineqDF$boot.se[ineqDF$measure=='ti'] <- ineqDF$boot.se[ineqDF$measure=='ti'] *1000
    ineqDF$boot.se[ineqDF$measure=='mld'] <- ineqDF$boot.se[ineqDF$measure=='mld'] *1000
    ineqDF$boot.lowerci[ineqDF$measure=='ti'] <- ineqDF$boot.lowerci[ineqDF$measure=='ti'] *1000
    ineqDF$boot.lowerci[ineqDF$measure=='mld'] <- ineqDF$boot.lowerci[ineqDF$measure=='mld'] *1000
    ineqDF$boot.upperci[ineqDF$measure=='ti'] <- ineqDF$boot.upperci[ineqDF$measure=='ti'] *1000
    ineqDF$boot.upperci[ineqDF$measure=='mld'] <- ineqDF$boot.upperci[ineqDF$measure=='mld'] *1000
    ineqDF$combo.se[ineqDF$measure=='ti'] <- ineqDF$combo.se[ineqDF$measure=='ti'] *1000
    ineqDF$combo.se[ineqDF$measure=='mld'] <- ineqDF$combo.se[ineqDF$measure=='mld'] *1000
    ineqDF$combo.lowerci[ineqDF$measure=='ti'] <- ineqDF$combo.lowerci[ineqDF$measure=='ti'] *1000
    ineqDF$combo.upperci[ineqDF$measure=='mld'] <- ineqDF$combo.upperci[ineqDF$measure=='mld'] *1000
    ineqDF$combo.lowerci[ineqDF$measure=='ti'] <- ineqDF$combo.lowerci[ineqDF$measure=='ti'] *1000
    ineqDF$combo.upperci[ineqDF$measure=='mld'] <- ineqDF$combo.upperci[ineqDF$measure=='mld'] *1000
    
  }
  
  if(!is.null(multiplier2) && multiplier2){
    #print("In dataTableInequal b")
    ineqDF$inequal[ineqDF$measure=='rci'] <- ineqDF$inequal[ineqDF$measure=='rci'] *100
    ineqDF$se[ineqDF$measure=='rci'] <- ineqDF$se[ineqDF$measure=='rci'] *100
    ineqDF$se.lowerci[ineqDF$measure=='rci'] <- ineqDF$se.lowerci[ineqDF$measure=='rci'] *100
    ineqDF$se.upperci[ineqDF$measure=='rci'] <- ineqDF$se.upperci[ineqDF$measure=='rci'] *100
    ineqDF$boot.se[ineqDF$measure=='rci'] <- ineqDF$boot.se[ineqDF$measure=='rci'] *100
    ineqDF$boot.lowerci[ineqDF$measure=='rci'] <- ineqDF$boot.lowerci[ineqDF$measure=='rci'] *100
    ineqDF$boot.upperci[ineqDF$measure=='rci'] <- ineqDF$boot.upperci[ineqDF$measure=='rci'] *100
    ineqDF$combo.se[ineqDF$measure=='rci'] <- ineqDF$combo.se[ineqDF$measure=='rci'] *100
    ineqDF$combo.lowerci[ineqDF$measure=='rci'] <- ineqDF$combo.lowerci[ineqDF$measure=='rci'] *100
    ineqDF$combo.upperci[ineqDF$measure=='rci'] <- ineqDF$combo.upperci[ineqDF$measure=='rci'] *100
  }
  
  

  
  if(!is.null(elasticity)){
    
    maxyear <- group_by(ineqDF, country) %>% 
      summarise(maxyr = max(year))
    
    ineqDF <- semi_join(ineqDF, maxyear, by=c("year" = "maxyr"))
    
  }
  
  
#   natdata <- filter(.rdata[['nationaldata']], country == i, year == elastic_years, indic==indicator) %>% 
#     select(country, year, indic, r)
  
  nationaldata <- select(.rdata[['nationaldata']], country, year, source, indic, r)
  
  #!!!!! You need source!!!! but inequal does not have it yet
  #ineqDF <- left_join(ineqDF, nationaldata, by=c('country', 'year', 'source', 'indic'))
  ineqDF <- left_join(ineqDF, select(nationaldata, -source), by=c('country', 'year', 'indic'))
  
  
  ineqDF <- rename(ineqDF, estimate = r)
  
  return(ineqDF)
}





getDisagData <- function(indicator = NULL, stratifier = NULL, countries = NULL, 
                         years = NULL, mostrecent=FALSE, datasource=NULL, 
                         elasticity=NULL){
  

  

  
  if(!is.null(elasticity)){
    # if mostrecent is TRUE then we want the focus_year, otherwise
    # we can use the input year
    years <- as.integer(years)
    years<-seq(years-elasticity, years+elasticity)

    
  }
  
  
  
  filt_country <- TRUE
  filt_year <- TRUE
  filt_indicator <- TRUE
  filt_dimension <- TRUE
  filt_datasource <- TRUE
  
  
  
    if(!is.null(countries)) filt_country <- quote(country %in% countries)
    if(!is.null(years) && !is.null(mostrecent)) filt_year <- quote(year %in% years)
    if(!is.null(years) && is.null(mostrecent)) filt_year <- quote(year %in% years)
    if(!is.null(indicator)) filt_indicator <- quote(indic %in% indicator)
    if(!is.null(stratifier)) filt_dimension <- quote(dimension %in% stratifier)
    if(!is.null(datasource) && datasource == 'All') filt_datasource <- TRUE
    if(!is.null(datasource) && datasource != 'All') filt_datasource <- quote(source == datasource)
  
  
  # if you use this you need to use filter_ below ALSO the quote in Mother's education causes issues
#   if(!is.null(countries)) filt_country <- paste0("country %in%c('", paste0(countries, collapse="','"), "')")
#   if(!is.null(years) && !is.null(mostrecent) && !mostrecent) filt_year <- paste0("year %in%c('", paste0(years, collapse="','"), "')")
#   if(!is.null(years) && is.null(mostrecent)) filt_year <- paste0("year %in%c('", paste0(years, collapse="','"), "')")
#   if(!is.null(indicator)) filt_indicator <- paste0("indic %in%c('", paste0(indicator, collapse="','"), "')")
#   if(!is.null(stratifier)) filt_dimension <- paste0("dimension %in%c('", paste0(stratifier, collapse="','"), "')")
#   if(!is.null(datasource) && datasource == 'All') filt_datasource <- TRUE
#   if(!is.null(datasource) && datasource != 'All') filt_datasource <- paste0("source %in%c('", paste0(datasource, collapse="','"), "')")
#   
#   
  #     print(paste0('filt_country:', deparse(filt_country)))
  #     print(paste0('filt_year:', deparse(filt_year)))
  #     print(paste0('filt_indicator:', deparse(filt_indicator)))
  #     print(paste0('filt_dimension:', deparse(filt_dimension)))
  #     
  #     print(paste0('country:', countries))
  #     print(paste0('years:', years))
  #     print(paste0('indicator:', indicator))
  #     print(paste0('dimension:', stratifier))

  hetk.data <- filter(.rdata[['maindata']], filt_country, filt_year, filt_indicator, filt_dimension, filt_datasource) %>% 
    select(country, year, source, indic, dimension, subgroup, r, r_lower, r_upper, se, pop, iso3, 
           rankable, maxoptimum, popshare, flag, rankorder, indic_name)
  
#print(head(hetk.data))
  
  #national.data <- dbGetQuery(con, selectNationalStr)
  
  # in original function getHETK there was a query and filter of national data but 
  # I'm not sure why this would be necessary if we do an inner join.
  
  nationaldata <- select(.rdata[['nationaldata']], country, year, source, indic, r)
  
  
  hetk.data <- inner_join(hetk.data, nationaldata, by=c('country', 'year', 'source', 'indic')) %>% 
    rename(estimate = r.x, national=r.y, lower_95ci=r_lower, upper_95ci=r_upper)
  
  
  hetk.data <- filter(hetk.data, !is.null(estimate), !is.na(estimate), estimate!="")
  
  
  hetk.data$year <- as.integer(hetk.data$year)
  hetk.data$estimate <- as.numeric(hetk.data$estimate)
  hetk.data$se <- as.numeric(hetk.data$se)
  hetk.data$pop <- as.integer(hetk.data$pop)
  hetk.data$lower_95ci <- as.numeric(hetk.data$lower_95ci)
  hetk.data$upper_95ci <- as.numeric(hetk.data$upper_95ci)
  hetk.data$rankable <- as.integer(hetk.data$rankable)
  #names(hetk.data)[which(names(hetk.data)=='r')] <- 'estimate'
  #names(hetk.data)[which(names(hetk.data)=='r_lower')] <- 'lower_95ci'
  #names(hetk.data)[which(names(hetk.data)=='r_upper')] <- 'upper_95ci'
  

  if(!is.null(elasticity)){

    maxyear <- group_by(hetk.data, country) %>% 
      summarise(maxyr = max(year))
    
    hetk.data <- semi_join(hetk.data, maxyear, by=c("year" = "maxyr"))
    
  }
  
  hetk.data <- orderHetkFactors(hetk.data)
  
  return(hetk.data)
}





orderHetkFactors <- function(DF){
  geo_levels <- unique(DF$subgroup[which(DF$dimension == 'Geographic region')])
  factor_order <- c('Quintile 1 (poorest)', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5 (richest)', 'No education', 'Primary school', 'Secondary school+', 'Urban', 'Rural', 'Male', 'Female')
  if(length(geo_levels>0)){  # Test to see that there are any geo_levels
    factor_order <- c(factor_order, geo_levels)
  }
  DF$subgroup <- factor(DF$subgroup, levels = factor_order)
  DF$subgroup <- factor(DF$subgroup)
  return(DF)
}



#******************************************************************************
# 
#******************************************************************************
# 
# getComparisonCountries <- function(indicator = NULL, stratifier = NULL, countries = NULL, 
#                                    years = NULL, mostrecent=FALSE, datasource=NULL, 
#                                    elasticity, matchyears=F){
#   
#   
#   # indicator: one pre-selected health indicator
#   # stratifier: one pre-selected equity dimension
#   # countries: one or more pre-selected benchmark countries
#   # years: one or more comparison years
#   # elasticity: the number of years around which the benchmark countries' years of data collection can vary from the base country
#   
#   # countries<-c("Afghanistan", "Armenia")
#   # elasticity <-c(2010)
#   
#   #indicator <- "carep"
#   #stratifier <- "Sex"
#   #countries <- c("Armenia", "Afghanistan")
#   #datasource = "All"
#   #years <- 2010
#   #elasticity <- 5
#   
#   thedata <-getDisagData(indicator=indicator,
#                stratifier=stratifier, 
#                countries = countries, 
#                datasource=datasource)
#   
#   
#   # initial year restriction
#   
#   thedata<-filter(thedata, year%in%seq(years-elasticity, years+elasticity))
#   
#   # looks like it takes the max year
#   
#   maxyear <- group_by(thedata, country) %>% 
#     summarise(maxyr = max(year))
#   
#   thedata <- semi_join(thedata, maxyear, by=c("year" = "maxyr"))
#   
#   # what does this do, matchyears? I think nothing since match years are false
# #   if(nrow(yearsdata)>0 & matchyears==T){
# #     yearsdata$year <- j  # Fix the benchmark year to the anchor year not the actual benchmark year
# #   }
# #   
#   for(i in countries){
#     #i<-"Armenia"
#     available_years <- filter(.rdata[['years']], country == i) %>% .$year
#     print(paste0("elasticity ", elasticity))
#     for(j in as.integer(years)){
#       #j<-2010
#       elastic_years <- available_years[nearest(j, available_years, limit=elasticity, all=T)]
#       
#       if(length(elastic_years)>0){
#         if(!(elastic_years==F)){  # Check to see that there is a relevant year
#           elastic_years <- max(elastic_years)      
#           yearsdata <- getDisagData(indicator=indicator,
#                                     stratifier=stratifier, 
#                                     countries = i, 
#                                     years = elastic_years, 
#                                     datasource=datasource)
#           if(nrow(yearsdata)>0 & matchyears==T){
#             yearsdata$year <- j  # Fix the benchmark year to the anchor year not the actual benchmark year
#           }
#           if(exists('mergedDF') & nrow(yearsdata)>0){
#             mergedDF <- rbind(mergedDF, yearsdata)
#           }
#           if(!exists('mergedDF') & nrow(yearsdata)>0){
#             mergedDF <- yearsdata
#           }
#         }
#       }
#     }
#   }
#   #  dbDisconnect(con)
#   if(exists('mergedDF')){
#     return(mergedDF)
#   }
#   else{
#     return(NULL)
#   }
# }



#******************************************************************************
# 
#******************************************************************************



getComparisonSummaries <- function(summeasure=NULL, indicator=NULL, stratifier=NULL, 
                                   countries=NULL, years=NULL, mostrecent=FALSE, datasource=NULL,
                                   elasticity=NULL, matchyears=F){
  # summeasure: the Inequality summary measure
  # indicator: one pre-selected health indicator
  # stratifier: one pre-selected equity dimension
  # countries: one or more pre-selected benchmark countries
  # years: one or more comparison years
  # elasticity: the number of years around which the benchmark countries' years of data collection can vary from the base country  
  
  if(is.null(.rdata[['years']])) return()
  if(is.null(summeasure)) return()
  
  for(i in countries){
    
    #yearsStr <- paste('SELECT DISTINCT year FROM nationaldata WHERE country="', i, '";', sep='')
    #available_years <- dbGetQuery(con, yearsStr)$year
    available_years <- filter(.rdata[['years']], country == i) %>% .$year
    elastic_years <- available_years[nearest(as.integer(years), available_years, limit=elasticity, all=T)]
    
    if(length(elastic_years)>0){
      if(!(elastic_years==F)){  # Check to see that there is a relevant year
        
        elastic_years <- max(elastic_years)    
        #         selectNatStr <- paste0('SELECT country, year, indic, r FROM nationaldata WHERE  country="', i, '" AND year=', 
        #                             elastic_years, ' AND indic="', indicator, '";')
        
        natdata <- filter(.rdata[['nationaldata']], country == i, year == elastic_years, indic==indicator) %>% 
          select(country, year, indic, r)
        
        
        sumdata <- filter(.rdata[['inequals']], country == i, year == elastic_years, 
                          indic == indicator, dimension == stratifier, measure == summeasure) %>% 
          select(country, ccode, year, indic, dimension, measure, inequal, boot.se, se)
        
        if(nrow(sumdata)==0 || nrow(natdata)==0) return()

        if(nrow(natdata)>0 & matchyears==T) natdata$year <- as.integer(years)  # Fix the benchmark year to the anchor year not the actual benchmark year

        if(exists('mergedDF1') & nrow(natdata)>0) mergedDF1 <- rbind(mergedDF1, natdata)

        if(!exists('mergedDF1') & nrow(natdata)>0) mergedDF1 <- natdata

        
        if(nrow(sumdata)>0 & matchyears==T) sumdata$year <- as.integer(years)  # Fix the benchmark year to the anchor year not the actual benchmark year

        
       
        if(exists('mergedDF2') & nrow(sumdata)>0) mergedDF2 <- rbind(mergedDF2, sumdata)
        
        if(!exists('mergedDF2') & nrow(sumdata)>0)  mergedDF2 <- sumdata
        
      }
    }
  }   # end loop through countries
  if(!exists('mergedDF1')) return()
  

  mergedDF <- merge(mergedDF1, mergedDF2, by=c("country","year", "indic"))   
  names(mergedDF)[4] <- 'estimate'
  return(mergedDF)
}
