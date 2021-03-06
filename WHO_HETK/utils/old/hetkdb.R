getHETKdata <- function(indicator = NULL, stratifier = NULL, countries = NULL, years = NULL, mostrecent=FALSE, datasource=NULL){
  #print(mostrecent)
  #print("Getting data from getHETKdata")
  #print(.rdata[['focus_country']])

  #if(is.null(countries))return()
  # countries <- c("Afghanistan", "Armenia")
  
  #mostrecent<- .rdata[['mostrecent']]
  
  filt_country <- TRUE
  filt_year <- TRUE
  filt_indicator <- TRUE
  filt_dimension <- TRUE
  filt_datasource <- TRUE
  

  
#   if(!is.null(countries)) filt_country <- quote(country %in% countries)
#   if(!is.null(years) && !is.null(mostrecent) && !mostrecent) filt_year <- quote(year %in% years)
#   if(!is.null(years) && is.null(mostrecent)) filt_year <- quote(year %in% years)
#   if(!is.null(indicator)) filt_indicator <- quote(indic %in% indicator)
#   if(!is.null(stratifier)) filt_dimension <- quote(dimension %in% stratifier)
#   if(!is.null(datasource) && datasource == 'All') filt_datasource <- TRUE
#   if(!is.null(datasource) && datasource != 'All') filt_datasource <- quote(source == datasource)
  
  
  if(!is.null(countries)) filt_country <- paste0("country %in%c('", paste0(countries, collapse="','"), "')")
  if(!is.null(years) && !is.null(mostrecent) && !mostrecent) filt_year <- paste0("year %in%c('", paste0(years, collapse="','"), "')")
  if(!is.null(years) && is.null(mostrecent)) filt_year <- paste0("year %in%c('", paste0(years, collapse="','"), "')")
  if(!is.null(indicator)) filt_indicator <- paste0("indic %in%c('", paste0(indicator, collapse="','"), "')")
  if(!is.null(stratifier)) filt_dimension <- paste0("dimension %in%c('", paste0(stratifier, collapse="','"), "')")
  if(!is.null(datasource) && datasource == 'All') filt_datasource <- TRUE
  if(!is.null(datasource) && datasource != 'All') filt_datasource <- paste0("source %in%c('", paste0(datasource, collapse="','"), "')")
  
  
#     print(paste0('filt_country:', deparse(filt_country)))
#     print(paste0('filt_year:', deparse(filt_year)))
#     print(paste0('filt_indicator:', deparse(filt_indicator)))
#     print(paste0('filt_dimension:', deparse(filt_dimension)))
#     
#     print(paste0('country:', countries))
#     print(paste0('years:', years))
#     print(paste0('indicator:', indicator))
#     print(paste0('dimension:', stratifier))
  
  
  hetk.data <- filter_(.rdata[['maindata']], filt_country, filt_year, filt_indicator, filt_dimension, filt_datasource) %>% 
    select(country, year, source, indic, dimension, subgroup, r, r_lower, r_upper, se, pop, iso3, 
           rankable, maxoptimum, popshare, flag, rankorder)
  

  
  #national.data <- dbGetQuery(con, selectNationalStr)
  
  # in original function getHETK there was a query and filter of national data but 
  # I'm not sure why this would be necessary if we do an inner join.
  
  nationaldata <- select(.rdata[['nationaldata']], country, year, source, indic, r)
  

  hetk.data <- inner_join(hetk.data, nationaldata, by=c('country', 'year', 'source', 'indic')) %>% 
    rename(estimate = r.x, national=r.y, lower_95ci=r_lower, upper_95ci=r_upper)

  
  
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
  
  if(!is.null(mostrecent) && mostrecent) {
    hetk.data <- filter(hetk.data, year == max(hetk.data$year))
  }
  
  hetk.data <- orderHetkFactors(hetk.data)
  #print(head(.rdata[['maindata']]))
  
  return(hetk.data)
}














#############################################################################
#
#
############################################################################
# 
# getHETKdata <- function(indicator, stratifier, countries, years, mostrecent=F, datasource='All'){
# 
#   con <- .connection
#   
#   if(mostrecent){
#     # Determine the most recent year for 'country' data in the database
#     mostrecentStr <- paste("SELECT  MAX(year), COUNT(*) FROM maindata WHERE country='", countries, "'", sep='')
#     if(datasource!='All'){
#       # Take account of MICS/DHS choices
#       mostrecentStr <- paste(mostrecentStr, "AND source='", datasource, "';", sep='')
#     }
#     else{
#       mostrecentStr <- paste(mostrecentStr, ";", sep='')
#     }   
#     years <- as.list(dbGetQuery(con, mostrecentStr ))[[1]]
#   }
#   ###
#   
#   baseStr <- 'SELECT country, year, source, indic, dimension, subgroup, r, r_lower, r_upper, se, pop, iso3, rankable, maxoptimum, popshare, flag, rankorder FROM maindata WHERE country IN ('  
#   baseStr_nationalData <- 'SELECT country, year, source, indic, r FROM nationaldata WHERE country IN ('  
#   
#   countryStr <- c()
#   for(i in countries){
#     countryStr <- c(countryStr, paste('"', i, '"', sep=''))
#   }
#   countryStr <- paste(countryStr, collapse=', ')
#   
#   yearStr <- paste(years, collapse=', ') 
#   
#   indicStr <- c()
#   for(i in indicator){
#     indicStr <- c(indicStr, paste('"', i, '"', sep=''))
#   }
#   indicStr <- paste(indicStr, collapse=', ')
#   
#   stratStr <- c()
#   for(i in stratifier){
#     stratStr <- c(stratStr, paste('"', i, '"', sep=''))
#   }
#   stratStr <- paste(stratStr, collapse=', ')
#   
#   
#   
#   selectStr <- paste(baseStr, countryStr, ') AND year IN (', yearStr, ') AND indic IN (', indicStr, ') AND dimension IN (', stratStr, ');', sep='', collapse='')
#   #print(selectStr)
#   selectNationalStr <- paste(baseStr_nationalData, countryStr, ') AND year IN (', yearStr, ') AND indic IN (', indicStr, ');', sep='', collapse='')
#   
#   hetk.data <- dbGetQuery(con, selectStr)
#   national.data <- dbGetQuery(con, selectNationalStr)
#   #dbDisconnect(con)
#   if(mostrecent==T){
#     names(hetk.data)[2] <- 'year'
#     names(national.data)[2] <- 'year'
#   } 
#   
#   names(national.data)[5] <- 'national'
#   hetk.data <- merge(hetk.data, national.data, by=c('country', 'year', 'source', 'indic'))
#   
#   
#   hetk.data$year <- as.integer(hetk.data$year)
#   hetk.data$r <- as.numeric(hetk.data$r)
#   hetk.data$se <- as.numeric(hetk.data$se)
#   hetk.data$pop <- as.integer(hetk.data$pop)
#   hetk.data$r_lower <- as.numeric(hetk.data$r_lower)
#   hetk.data$r_upper <- as.numeric(hetk.data$r_upper)
#   hetk.data$rankable <- as.integer(hetk.data$rankable)
#   names(hetk.data)[which(names(hetk.data)=='r')] <- 'estimate'
#   names(hetk.data)[which(names(hetk.data)=='r_lower')] <- 'lower_95ci'
#   names(hetk.data)[which(names(hetk.data)=='r_upper')] <- 'upper_95ci'
#   
#   hetk.data <- orderHetkFactors(hetk.data)
#   return(hetk.data)
# }


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
