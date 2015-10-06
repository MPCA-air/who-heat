

#*****************************************************************************
#  Observe -- interactions between explore and compare ----
#*****************************************************************************


observe({
  if(is.null(input$focus_country_explore)) return()
  .rdata[['focus_country']] <<- input$focus_country_explore
  
  
  
  country <- isolate(input$focus_country_compare)
  
  
  
  if(!is.null(country) && country!=.rdata[['focus_country']]){
    updateSelectInput(session, 'focus_country_compare', selected = .rdata[['focus_country']])
  }
  
  selectYears <- getFilteredYear(country=input$focus_country_explore, 
                                 isolate(input$focus_data_source_explore))
  
  .rdata[['all_years']]<<-selectYears
  .rdata[['focus_year']]<<-selectYears[1]
  
  updateSelectInput(session, 'focus_year_explore', choices = selectYears, selected = selectYears[1])
  updateSelectInput(session, 'focus_year_compare', choices = selectYears, selected = selectYears[1])
  
})



observe({
  if(is.null(input$focus_country_compare)) return()
  .rdata[['focus_country']] <<- input$focus_country_compare
  
  
  
  val <- isolate(input$focus_country_explore)
  
  if(is.null(val) || val!=.rdata[['focus_country']]){
    updateSelectInput(session, 'focus_country_explore', selected = .rdata[['focus_country']])
  }
  
  
  selectYears <- getFilteredYear(country=input$focus_country_compare, 
                                 isolate(input$focus_data_source_explore))
  
  .rdata[['all_years']]<<-selectYears
  .rdata[['focus_year']]<<-selectYears[1]
  
  updateSelectInput(session, 'focus_year_explore', choices = selectYears, selected = selectYears[1])
  updateSelectInput(session, 'focus_year_compare', choices = selectYears, selected = selectYears[1])
  
  
})





observe({
  if(is.null(input$focus_indicator_explore)){
    #print("in here")
    .rdata[['focus_indicator']] <<- NULL
    updateSelectInput(session, 'focus_indicator_compare', selected = NULL)
    return()
  }
  .rdata[['focus_indicator']] <<- input$focus_indicator_explore
  
  
  indicator <- isolate(input$focus_indicator_compare)
  
  if(!is.null(indicator) && indicator!=.rdata[['focus_indicator']]){
    
    updateSelectInput(session, 'focus_indicator_compare', selected = .rdata[['focus_indicator']])
  }
  
})



observe({
  if(is.null(input$focus_indicator_compare)){
    #return()
    updateSelectInput(session, 'focus_indicator_explore', selected = NULL)
    return()
  }
  .rdata[['focus_indicator']] <<- input$focus_indicator_compare
  
  
  indicator <- isolate(input$focus_indicator_explore)
  
  if(is.null(indicator) || indicator!=.rdata[['focus_indicator']]){
    
    updateSelectInput(session, 'focus_indicator_explore', selected = .rdata[['focus_indicator']])
  }
  
})




observe({
  if(is.null(input$focus_dimension_explore)) return()
  .rdata[['focus_dimension']] <<- input$focus_dimension_explore
  
  
  dimension <- isolate(input$focus_dimension_compare)
  
  if(is.null(dimension) || dimension!=.rdata[['focus_dimension']]){
    
    updateSelectInput(session, 'focus_dimension_compare', selected = .rdata[['focus_dimension']])
  }
  
})



observe({
  if(is.null(input$focus_dimension_compare)) return()
  .rdata[['focus_dimension']] <<- input$focus_dimension_compare
  
  
  dimension <- isolate(input$focus_dimension_explore)
  
  if(is.null(dimension) || dimension!=.rdata[['focus_dimension']]){
    
    updateSelectInput(session, 'focus_dimension_explore', selected = .rdata[['focus_dimension']])
  }
  
})


observe({
  
  if(is.null(input$benchmarkWBgroup) || input$benchmarkWBgroup == "") return()
  tmpCountries<-getFilteredCountries(input$benchmarkWBgroup, isolate(input$benchmarkWHOregion))
  tmpCountries <- append(.rdata[['benchmark_countries']], tmpCountries)
  #print("in observe")
  #bench <- .rdata[['benchmark_countries']]
  #.rdata[['benchmark_countries']] <<- bench[bench%in%tmpCountries]
  
  #updateSelectInput(session, "benchmark_countries", choices = tmpCountries, selected = .rdata[['benchmark_countries']])
  updateSelectInput(session, "benchmark_countries", choices = tmpCountries, selected=.rdata[['benchmark_countries']])
  
})



observe({
  
  if(is.null(input$benchmarkWHOregion) || input$benchmarkWHOregion == "") return()
  #print(input$benchmarkWHOregion)
  tmpCountries<-getFilteredCountries(isolate(input$benchmarkWBgroup), input$benchmarkWHOregion)
  
  tmpCountries <- append(.rdata[['benchmark_countries']], tmpCountries)
  
  #bench <- .rdata[['benchmark_countries']]
  #.rdata[['benchmark_countries']] <<- bench[bench%in%tmpCountries]
  
  #updateSelectInput(session, "benchmark_countries", choices = tmpCountries, selected = .rdata[['benchmark_countries']])
  updateSelectInput(session, "benchmark_countries", choices = tmpCountries, selected=.rdata[['benchmark_countries']])
  
})

#****************** Watch year_explore

observe({
  if(is.null(input$focus_year_explore)) return()
  .rdata[['focus_year']] <<- input$focus_year_explore
  
  
  
  year <- isolate(input$focus_year_compare)
  
  
  
  if(!is.null(year) && year!=.rdata[['focus_year']]){
    updateSelectInput(session, 'focus_year_compare', selected = .rdata[['focus_year']])
  }
  
  
})

#****************** Watch year_compare

observe({
  if(is.null(input$focus_year_compare)) return()
  .rdata[['focus_year']] <<- input$focus_year_compare
  
  
  
  year <- isolate(input$focus_year_explore)
  
  if(is.null(year) || year!=.rdata[['focus_year']]){
    updateSelectInput(session, 'focus_year_explore', selected = .rdata[['focus_year']])
  }
  
  
})


#****************** Watch mostrecent_explore

observe({
  if(is.null(input$mostrecent_explore)) return()
  .rdata[['mostrecent']] <<- input$mostrecent_explore
  
  mostrecent <- isolate(input$mostrecent_compare)
  
  
  if(!is.null(mostrecent) && mostrecent!=.rdata[['mostrecent']]){
    updateCheckboxInput(session,'mostrecent_compare', value=.rdata[['mostrecent']])
  }
  
  
})



#****************** Watch mostrecent_compare

observe({
  if(is.null(input$mostrecent_compare)) return()
  .rdata[['mostrecent']] <<- input$mostrecent_compare
  
  mostrecent <- isolate(input$mostrecent_explore)
  
  
  if(!is.null(mostrecent) && mostrecent!=.rdata[['mostrecent']]){
    updateCheckboxInput(session,'mostrecent_explore', value=.rdata[['mostrecent']])
  }
  
  
})




#****************** Watch focus_data_source_explore

observe({
  if(is.null(input$focus_data_source_explore)) return()
  .rdata[['focus_data_source']] <<- input$focus_data_source_explore
  
  focus_data_source <- isolate(input$focus_data_source_compare)
  
  
  if(!is.null(focus_data_source) && focus_data_source!=.rdata[['focus_data_source']]){
    updateCheckboxInput(session,'focus_data_source_compare', value=.rdata[['focus_data_source']])
  }
  
  
})



#****************** Watch focus_data_source_compare

observe({
  if(is.null(input$focus_data_source_compare)) return()
  .rdata[['focus_data_source']] <<- input$focus_data_source_compare
  
  focus_data_source <- isolate(input$focus_data_source_explore)
  
  
  if(!is.null(focus_data_source) && focus_data_source!=.rdata[['focus_data_source']]){
    updateCheckboxInput(session,'focus_data_source_explore', value=.rdata[['focus_data_source']])
  }
  
  
})


#****************** Watch focus_inequal_type_explore

observe({
  if(is.null(input$focus_inequal_type_explore)) return()
  .rdata[['focus_inequal_type']] <<- input$focus_inequal_type_explore
  
  focus_inequal_type <- isolate(input$focus_inequal_type_compare)
  
  
  if(!is.null(focus_inequal_type) && focus_inequal_type!=.rdata[['focus_inequal_type']]){
    updateCheckboxInput(session,'focus_inequal_type_compare', value=.rdata[['focus_inequal_type']])
  }
  
  
})



#****************** Watch focus_inequal_type_compare

observe({
  if(is.null(input$focus_inequal_type_compare)) return()
  .rdata[['focus_inequal_type']] <<- input$focus_inequal_type_compare
  
  focus_inequal_type <- isolate(input$focus_inequal_type_explore)
  
  
  if(!is.null(focus_inequal_type) && focus_inequal_type!=.rdata[['focus_inequal_type']]){
    updateCheckboxInput(session,'focus_inequal_type_explore', value=.rdata[['focus_inequal_type']])
  }
  
  
})
