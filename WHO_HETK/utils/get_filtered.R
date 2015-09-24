getFilteredCountries <- function(WBgroup=NULL, WHOregion=NULL)
{

  filt_WBgroup   <- TRUE
  filt_WHOregion <- TRUE
  
  
  if(!is.null(WBgroup) & all(WBgroup != "")) 
    filt_WBgroup <- quote(wbincome2014_4cat %in% WBgroup)

  if(!is.null(WHOregion) & all(WHOregion != "")) 
    filt_WHOregion <- quote(wbincome2014_4cat %in% WBgroup)
  
  
  countries <- filter(.rdata[['countrynames']], filt_WBgroup, filt_WHOregion) %>% 
    select(country) %>% .$country
  
  

  return(countries)
  
  
}



getFilteredYear <-  function(countryname, datasource='All', database){
  # This function filters the Years of surveys based on earlier choices about the Country
  # the Datasource and the Database 
  
  filt_country   <- quote(country %in% countryname)
  filt_source <- TRUE

  if(datasource != 'All') filt_source <- datasource
  
  years <- filter(.rdata[['years']], filt_country, filt_source) %>%
    arrange(year) %>% .$year
  
  return(years)
  
}


