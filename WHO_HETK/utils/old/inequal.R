######### All the measures of inequality
# This manages the return of the inequalities measures inequality measures
#########



getInequal <- function(indicator = NULL, stratifier = NULL, countries = NULL, years = NULL,
                       mostrecent=NULL, datasource=NULL,  inequal_types=NULL, multiplier1=TRUE,multiplier2=TRUE){
  #print("In getInequal function a")
  #print(paste0(indicator, stratifier, countries, years))
  # Fetch the inequalities data from the inbuilt database  
  
  
  
#   inequal.types <- inequal_types
#   if(inequal_types=='all'){
#     inequal.types <- c('aci', 'bgv', 'idis', 'riikm', 'mdb', 'mdm', 'mld', 
#                        'paf', 'par', 'rci', 'rd', 'rii', 'rr', 'sii', 'ti')
# }
#   
  
  filt_country <- TRUE
  filt_year <- TRUE
  filt_indicator <- TRUE
  filt_dimension <- TRUE
  filt_inequaltype<- TRUE
  
  if(!is.null(countries)) filt_country <- quote(country %in% countries)
  if(!is.null(years) && !is.null(mostrecent) && !mostrecent) filt_year <- quote(year %in% years)
  if(!is.null(years) && is.null(mostrecent)) filt_year <- quote(year %in% years)
  if(!is.null(indicator)) filt_indicator <- quote(indic %in% indicator)
  if(!is.null(stratifier)) filt_dimension <- quote(dimension %in% stratifier)
  if(!is.null(inequal_types)) filt_inequaltype <- quote(measure %in% inequal_types)

  #     print(paste0('filt_country:', deparse(filt_country)))
  #     print(paste0('filt_year:', deparse(filt_year)))
  #     print(paste0('filt_indicator:', deparse(filt_indicator)))
  #     print(paste0('filt_dimension:', deparse(filt_dimension)))
  #     
  #     print(paste0('country:', countries))
  #     print(paste0('years:', years))
  #     print(paste0('indicator:', indicator))
  #     print(paste0('dimension:', stratifier))

  ineqDF <- filter(.rdata[['inequals']], filt_country, filt_year, filt_indicator, filt_dimension, filt_inequaltype) %>% 
    select(country, year, indic, dimension, measure, inequal, boot.se, se, ccode)
  
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
  
  
  
  
  if(!is.null(mostrecent) && mostrecent) {
    #print("in most recent")
    ineqDF <- filter(ineqDF, year == max(ineqDF$year))
  }
  
  #print(head(ineqDF))
  return(ineqDF)
}



# 
# getInequal <- function(indicator, stratifier, countries, years,  inequal_types='all'){
#   #print("In getInequal function a")
#   #print(paste0(indicator, stratifier, countries, years))
#   # Fetch the inequalities data from the inbuilt database  
#   
#   # Return NULL if any of the function-parameters are missing
#   if(is.null(indicator)){
#     #print("In getInequal function b")
#     return()
#   }
#   if(length(indicator)==0){
#     
#     return()
#   }
#   if(is.null(stratifier)){
#     return()
#   }
#   if(length(stratifier)==0){
#     return()
#   }
#   if(is.null(countries)){
#     return()
#   }
#   if(length(countries)==0){
#     return()
#   }
#   if(is.null(years)){
#     return()
#   }
#   if(length(years)==0){
#     return()
#   }
#   
#   
#   con <- .connection
#   
#   if(is.null(inequal_types)){
#     return()
#   }
#   
#   if(inequal_types=='all'){
#     inequal.types <- c('aci', 'bgv', 'idis', 'riikm', 'mdb', 'mdm', 'mld', 
#                        'paf', 'par', 'rci', 'rd', 'rii', 'rr', 'sii', 'ti')
#   }
#   else{
#     inequal.types <- inequal_types
#   }
#   
#   baseStr <- 'SELECT country, year, indic, dimension, measure, inequal, [boot.se], se FROM inequals WHERE country IN ('
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
#   measureStr <- c()
#   for(i in inequal.types){
#     measureStr <- c(measureStr, paste('"', i, '"', sep=''))
#   }
#   measureStr <- paste(measureStr, collapse=', ')
#   
#   
#   selectStr <- paste(baseStr, countryStr, ") AND year IN (", yearStr, ") AND indic IN (", indicStr, ") AND dimension IN (", stratStr, ") AND measure IN (", measureStr, ");", sep="", collapse="")
#   
#   #print(selectStr)
#   
#   ineqDF <- dbGetQuery(con, selectStr)
#   #dbDisconnect(con)
#   if(is.null(ineqDF)){
#     return()
#   }
#   
#   ineqDF$year <- as.integer(ineqDF$year)
#   ineqDF$boot.se <- as.numeric(ineqDF$boot.se)
#   ineqDF$boot.se[ineqDF$boot.se==0] <- NA
#   ineqDF$se <- as.numeric(ineqDF$se)
#   ineqDF$se[ineqDF$se == 0] <- NA
#   ineqDF$combo.se <- ineqDF$se
#   ineqDF$combo.se[is.na(ineqDF$se)] <- ineqDF$boot.se[is.na(ineqDF$se)]
#   
#   ineqDF$boot.lowerci <- ineqDF$inequal - (1.96 * ineqDF$boot.se) 
#   ineqDF$boot.upperci <- ineqDF$inequal + (1.96 * ineqDF$boot.se) 
#   ineqDF$se.lowerci <- ineqDF$inequal - (1.96 * ineqDF$se) 
#   ineqDF$se.upperci <- ineqDF$inequal + (1.96 * ineqDF$se) 
#   ineqDF$combo.lowerci <- ineqDF$inequal - (1.96 * ineqDF$combo.se) 
#   ineqDF$combo.upperci <- ineqDF$inequal + (1.96 * ineqDF$combo.se) 
#   
#   
#   
#   ineqDF$combo.se[is.na(ineqDF$combo.se)] <- ineqDF$boot.se[is.na(ineqDF$combo.se)]  #  Make an se that is analytic if it exists, otherwise a boostrap
#   #print("In getInequal function b")
#   return(ineqDF)
# }
# 





