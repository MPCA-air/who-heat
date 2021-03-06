#############################################################################
#
#
############################################################################



getComparisonCountries <- function(indicator, stratifier, countries, years, elasticity, matchyears=F){
  # indicator: one pre-selected health indicator
  # stratifier: one pre-selected equity dimension
  # countries: one or more pre-selected benchmark countries
  # years: one or more comparison years
  # elasticity: the number of years around which the benchmark countries' years of data collection can vary from the base country

  # countries<-c("Afghanistan", "Armenia")
  # elasticity <-c(2010)
  
  
  for(i in countries){
    #i<-"Afghanistan"
    available_years <- filter(.rdata[['years']], country == i) %>% .$year

    for(j in as.integer(years)){
      #j<-2010
      elastic_years <- available_years[nearest(j, available_years, limit=elasticity, all=T)]
      
      if(length(elastic_years)>0){
        if(!(elastic_years==F)){  # Check to see that there is a relevant year
          elastic_years <- max(elastic_years)      
          yearsdata <- getDisagData(indicator=indicator,
                                   stratifier=stratifier, 
                                   countries = i, 
                                   years = elastic_years, 
                                   datasource='All')
          if(nrow(yearsdata)>0 & matchyears==T){
            yearsdata$year <- j  # Fix the benchmark year to the anchor year not the actual benchmark year
          }
          if(exists('mergedDF') & nrow(yearsdata)>0){
            mergedDF <- rbind(mergedDF, yearsdata)
          }
          if(!exists('mergedDF') & nrow(yearsdata)>0){
            mergedDF <- yearsdata
          }
        }
      }
    }
  }
  #  dbDisconnect(con)
  if(exists('mergedDF')){
    return(mergedDF)
  }
  else{
    return(NULL)
  }
}

# 
# 
# getComparisonCountries <- function(indicator, stratifier, countries, years, elasticity, matchyears=F){
#   # indicator: one pre-selected health indicator
#   # stratifier: one pre-selected equity dimension
#   # countries: one or more pre-selected benchmark countries
#   # years: one or more comparison years
#   # elasticity: the number of years around which the benchmark countries' years of data collection can vary from the base country
#   require('RSQLite')
#   
#   for(i in countries){
#     drv <- dbDriver("SQLite")
#     con <- dbConnect(drv, 'data/HEMTK.db')
#     yearsStr <- paste('SELECT DISTINCT year FROM maindata WHERE country="', i, '";', sep="")
#     available_years <- dbGetQuery(con, yearsStr)$year
#     for(j in as.integer(years)){
#       elastic_years <- available_years[nearest(j, available_years, limit=elasticity, all=T)]
#       if(length(elastic_years)>0){
#         if(!(elastic_years==F)){  # Check to see that there is a relevant year
#           elastic_years <- max(elastic_years)      
#           yearsdata <- getHETKdata(indicator, stratifier, i, elastic_years, datasource='All')
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



getComparisonSummaries <- function(summeasure=NULL, indicator=NULL, stratifier=NULL, 
                                   countries=NULL, years=NULL, elasticity=NULL, matchyears=F){
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
       
        
#         sumdata <- filter(.rdata[['inequals']], country == i, year == elastic_years, 
#                           indic == indicator, dimension == stratifier,
#                           measure == summeasure) %>% 
#           select(country, ccode, year, indic, dimension, measure, inequal, boot.se, se) 
        
        #print(head(sumdata))
        
#         selectSumStr <- paste0('SELECT country, ccode, year, indic, dimension, measure, inequal, [boot.se], 
#                                se FROM inequals WHERE  country="', i, '" AND year=', 
#                                elastic_years, ' AND indic="', indicator, '" AND dimension="', stratifier, 
#                                '" AND measure="', summeasure, '";')
#         natdata <- dbGetQuery(con, selectNatStr)
#         sumdata <- dbGetQuery(con, selectSumStr)
        #print(selectSumStr)
        if(nrow(natdata)>0 & matchyears==T){
          natdata$year <- as.integer(years)  # Fix the benchmark year to the anchor year not the actual benchmark year
        }
        if(exists('mergedDF1') & nrow(natdata)>0){
          mergedDF1 <- rbind(mergedDF1, natdata)
        }
        if(!exists('mergedDF1') & nrow(natdata)>0){
          mergedDF1 <- natdata
        }
        
        if(nrow(sumdata)>0 & matchyears==T){
          sumdata$year <- as.integer(years)  # Fix the benchmark year to the anchor year not the actual benchmark year
        }
        if(exists('mergedDF2') & nrow(sumdata)>0){
          print("in merged df2")
          mergedDF2 <- rbind(mergedDF2, sumdata)
        }
        if(!exists('mergedDF2') & nrow(sumdata)>0){
          print("in merged df2b")
          mergedDF2 <- sumdata
        }
      }
    }
  }  
  if(!exists('mergedDF1')){
    return(NULL)
  }
  mergedDF <- merge(mergedDF1, mergedDF2, by=c("country","year", "indic"))   
  names(mergedDF)[4] <- 'estimate'
  return(mergedDF)
}

# 
# getComparisonSummaries <- function(summeasure, indicator, stratifier, countries, years, elasticity, matchyears=F){
#   # summeasure: the Inequality summary measure
#   # indicator: one pre-selected health indicator
#   # stratifier: one pre-selected equity dimension
#   # countries: one or more pre-selected benchmark countries
#   # years: one or more comparison years
#   # elasticity: the number of years around which the benchmark countries' years of data collection can vary from the base country  
#   require('RSQLite')
#   
#   drv <- dbDriver("SQLite")
#   con <- dbConnect(drv, "data/HEMTK.db")
#   
#   
#   
#   for(i in countries){
#     yearsStr <- paste('SELECT DISTINCT year FROM nationaldata WHERE country="', i, '";', sep='')
#     available_years <- dbGetQuery(con, yearsStr)$year
#     elastic_years <- available_years[nearest(as.integer(years), available_years, limit=elasticity, all=T)]
#     
#     if(length(elastic_years)>0){
#       if(!(elastic_years==F)){  # Check to see that there is a relevant year
#         elastic_years <- max(elastic_years)    
#         selectNatStr <- paste0('SELECT country, year, indic, r FROM nationaldata WHERE  country="', i, '" AND year=', 
#                             elastic_years, ' AND indic="', indicator, '";')
#         selectSumStr <- paste0('SELECT country, ccode, year, indic, dimension, measure, inequal, [boot.se], se FROM inequals WHERE  country="', i, '" AND year=', 
#                                elastic_years, ' AND indic="', indicator, '" AND dimension="', stratifier, 
#                                '" AND measure="', summeasure, '";')
#         natdata <- dbGetQuery(con, selectNatStr)
#         sumdata <- dbGetQuery(con, selectSumStr)
#         #print(selectSumStr)
#         if(nrow(natdata)>0 & matchyears==T){
#           natdata$year <- as.integer(years)  # Fix the benchmark year to the anchor year not the actual benchmark year
#         }
#         if(exists('mergedDF1') & nrow(natdata)>0){
#           mergedDF1 <- rbind(mergedDF1, natdata)
#         }
#         if(!exists('mergedDF1') & nrow(natdata)>0){
#           mergedDF1 <- natdata
#         }
#         
#         if(nrow(sumdata)>0 & matchyears==T){
#           sumdata$year <- as.integer(years)  # Fix the benchmark year to the anchor year not the actual benchmark year
#         }
#         if(exists('mergedDF2') & nrow(sumdata)>0){
#           mergedDF2 <- rbind(mergedDF2, sumdata)
#         }
#         if(!exists('mergedDF2') & nrow(sumdata)>0){
#           mergedDF2 <- sumdata
#         }
#       }
#     }
#   }  
#   if(!exists('mergedDF1')){
#     return(NULL)
#   }
#   mergedDF <- merge(mergedDF1, mergedDF2, by=c("country","year", "indic"))   
#   names(mergedDF)[4] <- 'estimate'
#   return(mergedDF)
# }
