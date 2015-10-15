#******************************************************************************
#******************************************************************************
# EXPLORE
#******************************************************************************
#******************************************************************************

#get_disag("indic", )

#******************************************************************************
# Explore inequality: SELECTORS ALL TABS
#******************************************************************************


# ----- Country selector -----------------------------------------------

output$focus_country_explore <- renderUI({
  
  focusCountry_selector("focus_country_explore")
  
})

# ----- Year and source selector -----------------------------------------------


output$focus_source_year_explore <- renderUI({

  list(
  #conditionalPanel(condition = "input.assessment_panel == 'datatable' | input.assessment_panel == 'dataplot'",
  radioButtons("focus_data_source_explore", "Select data sources",
               c("All", "DHS", "MICS"),
               inline=TRUE,
               selected="All"),
 # ),
  tags$span(class="control-label", "Select years"),
  checkboxInput('mostrecent_explore', 'Most recent year', .rdata[['mostrecent']]),
  
  conditionalPanel( condition = "!input.mostrecent_explore",  
 
                    selectInput(inputId="focus_year_explore", 
                                label='', 
                                choices=.rdata[['all_years']], 
                                multiple=T, 
                                selected=.rdata[['focus_country']])
                    )
  )
})



# ----- Indicator selector -----------------------------------------------


output$focus_indicator_explore <- renderUI({
  
  focusIndicator_selector("focus_indicator_explore", multiple=TRUE, core=FALSE)

  
})


# ----- Dimension selector -----------------------------------------------

output$focus_dimension_explore <- renderUI({
  #print(paste0("inputdatatable:", input$assessment_panel))
  focusDimension_selector("focus_dimension_explore", multiple=TRUE)
  
  
})


#******************************************************************************
# Explore inequality: SELECTOR disaggregated table ----
#******************************************************************************


# ----- Variable selector -----------------------------------------------



output$dataTableItems_explore <- renderUI({
  
  if(!is.null(input$dataTableItems)) .rdata[['focus_table_variables']] <- input$dataTableItems
  
  list(

    selectInput(inputId = "dataTableItems",
                "Select table content",
                choices = .rdata[['all_table_variables']],
                selected = .rdata[['focus_table_variables']],
                multiple=TRUE)
  )
  
})



#******************************************************************************
# Explore inequality: disaggregated plot ----
#******************************************************************************

output$disag_plot_type <- renderUI({
  
  radioButtons("ai_plot_type", "Select chart type",
               c("Bar Chart" = "data_bar",
                 "Line Chart" = "data_line"),
               inline=T,
               selected="data_line")
  
  
})


output$disag_plot_dimensions <- renderUI({
  
  list(
    sliderInput('plot_height1', 'Height', min=200, max=1500, value=400, step = 50,
                round = T,
                ticks = TRUE, animate = FALSE),
    
    sliderInput('plot_width1', 'Width', min=200, max=1500, value=400, step = 50,
                round = T,
                ticks = TRUE, animate = FALSE)
  )
  
})



# output$theDataPlot_web <- renderPlot({
#   #print("In theDataPlot_web")
#   p<-theDataPlot()
#   
#   
#   validate(
#     need(all(!is.null(p)), "There is no data for this combination of variables")
#   )
#   
#   print(p)  # Remember that print(theDataPlot) just prints the code
# })


#******************************************************************************
# Explore inequality: summary table and plot ----
#******************************************************************************




output$focus_summeasure_explore_summary <- renderUI({
  focusInequalType_selector("focus_inequal_type_explore", .rdata[['focus_dimension']])
})



#******************************************************************************
# Explore inequality:: summary table ----
#******************************************************************************



output$summary_measures <- renderUI({
  list(
    checkboxInput('summultiplier1', 'MLD and TI x1000', TRUE),
    checkboxInput('summultiplier2', 'RCI x100', TRUE),
    sliderInput('sumsigfig', 'Select estimate precision', min=0, max=5, value=2, round=T, width='50%'),
    radioButtons(inputId='se_type', 
                 label='Select standard error type', 
                 choices = c('Bootstrap and Analytic' = 'both',
                             'Analytic' = 'analytic',
                             'Bootstrap' = 'bootstrap'), 
                 selected = 'both', 
                 inline = FALSE)
  )
})









# Create a download button contingent on data in the table
output$downloadSummtable <- renderUI({ 
  theData <- datasetInequal()
  
  if(is.null(theData)){
    return()
  }
  if(nrow(theData)==0){
    return()
  } else {
    list(br(),
         actionButton("downloadSummtable", "Download table", class = "btn-primary"))
  }  
})






output$theDataPlot_web <- renderPlot({ 
  
  if(is.null(input$ai_plot_type)) return()
  #print("Reactive: theDataPlot")
  plotData <- datasetInput()
  plotData <- select(plotData, country, year, indic, subgroup, dimension, estimate, se)
  
  validate(
    need(!is.null(plotData) && nrow(plotData)>0, "There is no data for this combination of variables")
  )
  
  if(!is.null(plotData) & nrow(plotData)>0){
    chartopt <- list()
    # Chart options for axis max and min values
    chartopt <- lappend(chartopt, 'axmax' = as.integer(input$axis_limitsmax1))
    chartopt <- lappend(chartopt, 'axmin' = as.integer(input$axis_limitsmin1))
    # Chart options for whether the chart only carries geographic region data
    geo_only <- geoOnly(plotData)
    if(geo_only){
      chartopt <- lappend(chartopt, 'geo_only' = geo_only)     
    }
    
    if(input$main_title1 != ""){
      chartopt <- lappend(chartopt, 'main_title' = input$main_title1)
    }
    if(input$xaxis_title1 != ""){
      chartopt <- lappend(chartopt, 'xaxis_title' = input$xaxis_title1)
    }
    if(input$yaxis_title1 != ""){
      chartopt <- lappend(chartopt, 'yaxis_title' = input$yaxis_title1)
    }
    
    
    
    if(input$assessment_panel == 'dataplot' & input$ai_plot_type=='data_bar'){        
      p <- plotBar_explore(plotData, chartoptions=chartopt)
      
      print(p)
    }
    if(input$assessment_panel == 'dataplot' & input$ai_plot_type=='data_line'){
      
      p <- plotFigure2(plotData, chartoptions=chartopt)
      print(p)
    }
  }
  else{
    return()
  }
  
})  




#******************************************************************************
#******************************************************************************
# EXPLORE DATA
#******************************************************************************
#******************************************************************************



# Generate a view of the Managed Data
output$dataTable <- renderDataTable({
  
  if(is.null(input$focus_country_explore)) return()
  
  theData <- datasetInput()
  
  validate(
    need(!is.null(theData) && nrow(theData)>0, "There is no data for this combination of variables")
  )
  
  
  theData <- theData %>% 
    mutate(estimate = round(estimate, 2),
           lower_95ci = round(lower_95ci,2),
           upper_95ci = round(upper_95ci,2),
           popshare = round(popshare, 2),
           national = round(national, 2))
  
  theData<-theData %>% 
    rename(
      Country                = country,
      Year                   = year,
      `Data source`          = source,
      `Health indicator`     = indic,
      `Inequality dimension` = dimension,
      Subgroup               = subgroup,
      Estimate               = estimate,
      `Lower 95%CI`          = lower_95ci,
      `Upper 95%CI`          = upper_95ci,
      `Population share %`   = popshare,
      `National estimate`    = national,
      Flag                   = flag
    ) 
  
  
  
  theData <- theData[, input$dataTableItems]
  
  
  theData
}, options = list(dom = "ilftpr", pageLength = 100)  # see https://datatables.net/ for dataTable options
)



# Return the requested dataset based on the UI selection (dataSource)
datasetInput <- reactive({
  
  
  theData<-getDisagData(indicator=input$focus_indicator_explore, 
                        stratifier=input$focus_dimension_explore,  # in hetkdb.R
                        countries=input$focus_country_explore, 
                        years=input$focus_year_explore, 
                        mostrecent=input$mostrecent_explore,
                        datasource=input$focus_data_source_explore)
  
  
})



datasetInequal <- reactive({
  
  #if(is.null(input$summultiplier1)) return()
  if(is.null(input$assessment_panel)) return()
  
  input$focus_country_explore
  input$focus_data_source_explore
  input$mostrecent_explore
  input$focus_year_explore
  input$focus_indicator_explore
  input$focus_dimension_explore
  input$focus_inequal_type_explore
  

  ineqDF <- getInequalData(indicator=.rdata[['focus_indicator']],  
                           stratifier=.rdata[['focus_dimension']], 
                           countries=.rdata[['focus_country']], 
                           years=.rdata[['focus_year']], 
                           mostrecent=.rdata[['mostrecent']],
                           datasource=.rdata[['focus_data_source']],  
                           inequal_types=.rdata[['focus_inequal_type']],
                           multiplier1 = input$summultiplier1,
                           multiplier2 = input$summultiplier2)
  
  #blah
 

  if(input$assessment_panel=='sumplot'){
    
    ineqDF$boot.se[ineqDF$boot.se == 0] <- NA
    ineqDF$se[ ineqDF$se == 0] <- NA
    
    return(ineqDF)
  }  
  
  return(ineqDF)
})




# Generate a view of the HETKB 
output$dataTableInequal <- renderDataTable({
  #print("In dataTableInequal")
  
  theData <- datasetInequal()
  
  validate(
    need(!is.null(theData ) && nrow(theData )>0, "There is no data for this combination of variables")
  )
  
  if(is.null(input$sumsigfig)) return()

  if(!is.null(theData) && nrow(theData)>0){
    #theData <- datasetInequal()
    
    # this is somewhat confusing because theData may not have most of these
    # this could be much cleaner
    

    var_names <- names(theData)

    theData[, c('inequal', 'se', 'boot.se', 'combo.se', 'se.lowerci', 
                'se.upperci', 'boot.lowerci', 'boot.upperci', 
                'combo.lowerci', 'combo.upperci' )] <- 
      round(theData[, c('inequal', 'se', 'boot.se', 'combo.se', 
                        'se.lowerci', 'se.upperci', 'boot.lowerci', 'boot.upperci', 
                        'combo.lowerci', 'combo.upperci' )], input$sumsigfig)
    
    
    
    if(input$se_type == 'analytic'){
      theData <- theData[, setdiff(var_names, c('boot.se', 'boot.upperci', 'boot.lowerci', 'combo.se', 'combo.lowerci', 'combo.upperci', 'se'))]
      names(theData)[names(theData)=="se.upperci" ] <- "Analytic Upper 95%CI"
      names(theData)[names(theData)=="se.lowerci" ] <- "Analytic Lower 95%CI"
    }
    if(input$se_type == 'bootstrap'){
      theData <- theData[, setdiff(var_names, c('se', 'se.upperci', 'se.lowerci', 'combo.se', 'combo.lowerci', 'combo.upperci', 'boot.se'))]
      names(theData)[names(theData)=="boot.upperci" ] <- "Bootstrap Upper 95%CI"
      names(theData)[names(theData)=="boot.lowerci" ] <- "Bootstrap Lower 95%CI"
    }
    if(input$se_type == 'both'){
      theData <- theData[, setdiff(var_names, c('combo.se', 'combo.lowerci', 'combo.upperci', 'se', 'boot.se'))]
      names(theData)[names(theData)=="boot.upperci" ] <- "Bootstrap Upper 95%CI"
      names(theData)[names(theData)=="boot.lowerci" ] <- "Bootstrap Lower 95%CI"
      names(theData)[names(theData)=="se.upperci" ] <- "Analytic Upper 95%CI"
      names(theData)[names(theData)=="se.lowerci" ] <- "Analytic Lower 95%CI"
    }
    
    if(input$se_type == 'balance'){
      theData <- theData[, setdiff(var_names, c('boot.se', 'boot.upperci', 'boot.lowerci', 'se', 'se.lowerci', 'se.upperci', 'combo.se'))]
      names(theData)[names(theData)=="combo.upperci" ] <- "Upper 95%CI"
      names(theData)[names(theData)=="combo.lowerci" ] <- "Lower 95%CI"
      
    }
    
    
    names(theData)[names(theData)=="country" ] <- "Country" 
    names(theData)[names(theData)=="year" ] <- "Year"
    names(theData)[names(theData)=="indic" ] <- "Health indicator" 
    names(theData)[names(theData)=="dimension" ] <- "Inequality dimension" 
    names(theData)[names(theData)=="inequal" ] <- "Estimate"
    names(theData)[names(theData)=="measure" ] <- "Summary measure"
    
    theData <- select(theData, -ccode)
    
    #print(theData)
  }

  theData
}, options = list(dom = "ilftpr", pageLength = 100)  # see https://datatables.net/ for dataTable options
)


#******************************************************************************
# required in explore inequality: summary plot
#******************************************************************************

output$summary_plot_dimensions <- renderUI({
  list(
    sliderInput('plot_height_sum', 'Height', min=200, max=1500, value=400, step = 50,
                round = T,
                ticks = TRUE, animate = FALSE),
    
    sliderInput('plot_width_sum', 'Width', min=200, max=1500, value=600, step = 50,
                round = T,
                ticks = TRUE, animate = FALSE)
  )
})


output$summary_plot_type <- renderUI({
  radioButtons("sumplot_type", "Select chart type",
               c("Bar Chart" = "data_bar",
                 "Line Chart" = "data_line"),
               inline=T,
               selected="data_line")
})



# output$theSumPlot_web <- renderPlot({
#   p<-theSummaryPlot()
#   
# 
#   
#   print(p)
# 
# }, res=90)
# 


# Generate a reactive element for plotting the Summary Data.
# Pass to the webpage using renderPlot(print(theDataPlot))
output$theSumPlot_web <- renderPlot({ 
  
  input$focus_country_explore
  input$focus_data_source_explore
  input$mostrecent_explore
  input$focus_year_explore
  input$focus_indicator_explore
  input$focus_dimension_explore
  input$focus_inequal_type_explore
  
  plotData <- datasetInequal()

  if(is.null(input$sumplot_type)) return()
  validate(
    need(!is.null(plotData) && nrow(plotData)>0, "There is no data for this combination of variables")
  )
  

    #plotData <- datasetInequal()
    #if(class(plotData)=="data.frame" && nrow(plotData)>0 ){
      
  if(!is.null(plotData) && nrow(plotData)>0){
      chartopt <- list()
      chartopt <- lappend(chartopt, 'axmax' = as.integer(input$axis_limitsmax2))
      chartopt <- lappend(chartopt, 'axmin' = as.integer(input$axis_limitsmin2))
      
      if(input$main_title2 != ""){
        chartopt <- lappend(chartopt, 'main_title' = input$main_title2)
      }
      if(input$xaxis_title2 != ""){
        chartopt <- lappend(chartopt, 'xaxis_title' = input$xaxis_title2)
      }
      if(input$yaxis_title2 != ""){
        chartopt <- lappend(chartopt, 'yaxis_title' = input$yaxis_title2)
      }
      
   
        
        if(input$assessment_panel == 'sumplot' & input$sumplot_type=='data_bar'){                   
          p <- plotFigure3(plotData, chartoptions=chartopt)
          print(p)
        }
        if(input$assessment_panel == 'sumplot' & input$sumplot_type=='data_line'){          
          p <- plotFigure4(plotData, chartoptions=chartopt)
          print(p)
        }    
      
  }

}, res=90)  














#******************************************************************************
# Compare inquality: sidepanel
#******************************************************************************

output$focus_country_compare <- renderUI({
  
  focusCountry_selector("focus_country_compare")
  
})

output$focus_indicator_compare <- renderUI({
  
  focusIndicator_selector("focus_indicator_compare", multiple=FALSE)
  
})






output$focus_source_year_compare <- renderUI({
  
  list(
    #conditionalPanel(condition = "input.comparison_panel == 'inequalbenchmark' | input.comparison_panel == 'inequaldisag'",
                     radioButtons("focus_data_source_compare", "Select data sources",
                                  c("All", "DHS", "MICS"),
                                  inline=TRUE,
                                  selected="All"),
    #),
    tags$span(class="control-label", "Select years"),
    checkboxInput('mostrecent_compare', 'Most recent year', .rdata[['mostrecent']]),
    
    conditionalPanel( condition = "!input.mostrecent_compare",  
                      
                      selectInput(inputId="focus_year_compare", 
                                  label='', 
                                  choices=.rdata[['all_years']], 
                                  multiple=FALSE, 
                                  selected=.rdata[['focus_country']])
    )
  )
})






output$focus_summeasure_compare_summary <- renderUI({
  focusInequalType_selector("focus_inequal_type_compare", .rdata[['focus_dimension']])
})


output$focus_dimension_compare <- renderUI({
  
  focusDimension_selector("focus_dimension_compare", multiple=FALSE)
  
})



output$benchmark_countries <- renderUI({
  


  countries <- .rdata[['benchmark_country_list']]
  focus <-.rdata[['focus_country']]
  
  countries <- countries[!countries%in%focus]
  
  selectInput("benchmark_countries", 
              "Select benchmark countries", 
              choices=countries, 
              selected=.rdata[['benchmark_countries']],
              multiple=TRUE)
})



output$benchmarkWBgroup <- renderUI({
  
  selectInput("benchmarkWBgroup", label = "Filter benchmark countries by income group",
              .rdata[['income_groups']],
              selected = NULL,
              multiple=T)
})




output$benchmarkWHOregion <- renderUI({
  
  
  selectizeInput("benchmarkWHOregion", label = "Filter benchmark countries by WHO region",
              choices=.rdata[['who_regions']],
              multiple=T)
  
})


output$benchmarkYears <- renderUI({
  
  sliderInput('benchmarkYears', 'Select years', min=0, max=5, value=2, step = 1,
              round = T, ticks = TRUE, animate = FALSE)
  
})


output$benchmarkCountries <- renderUI({
  countries <- getFilteredCountries(input$benchmarkWBgroup, input$benchmarkWHOregion)  
  selectInput("benchmarkCountries", 
              "Select countries", 
              choices=countries, 
              selected=countries[1:5],
              multiple=T)
})



#******************************************************************************
# Comparison disaggregated plots SIDEPANEL
#******************************************************************************





#******************************************************************************
# Comparison benchmark MAINPANEL -----
#******************************************************************************


getBenchmarkData <- reactive({
  
  
#   input$benchmarkWBgroup
#   input$benchmarkWHOregion
#   input$getcomparisondata1
  
  if(is.null(input$benchmarkYears)) return()

  
  anchordata<-getDisagData(indicator=input$focus_indicator_compare, 
                          stratifier=input$focus_dimension_compare,  # in hetkdb.R
                          countries=input$focus_country_compare, 
                          years=input$focus_year_compare, 
                          mostrecent=input$mostrecent_compare,
                          datasource=input$focus_data_source_compare)
  
  #print(head(anchordata))
  if(!is.null(anchordata) && nrow(anchordata)>0) anchordata$anchor <- 1
  
  benchmarkdata <- getDisagData(indicator = input$focus_indicator_compare, 
                                          stratifier = input$focus_dimension_compare, 
                                          countries = input$benchmark_countries, 
                                          years =  input$focus_year_compare, 
                                          mostrecent = input$mostrecent_compare,
                                          datasource = input$focus_data_source_compare,
                                          elasticity = input$benchmarkYears)
  
  
  if(!is.null(benchmarkdata) && nrow(benchmarkdata)>0) benchmarkdata$anchor <- 0
  
  #     benchmarkdata <- getComparisonCountries(indicator = input$compplotBenchHealthIndicator, 
  #                                             stratifier = input$compplotBenchEquityDimension, 
  #                                             countries = input$benchmarkCountries, 
  #                                             years =  unique(anchordata$year), 
  #                                             elasticity = input$benchmarkYears, matchyears=F)
  
 
  
  
#   if(!is.null(anchordata) && !is.null(benchmarkdata) && ncol(anchordata)==ncol(benchmarkdata))  
 theData <- rbind(anchordata, benchmarkdata) 
  
 # Merge the relevant initial data with benchmarkdata
  
  return(theData)
})




getBenchmarkDataSum <- reactive({
  
  
#   input$benchmarkWBgroup
#   input$benchmarkWHOregion
#   input$getcomparisondata1
  
  if(is.null(input$focus_inequal_type_compare)) return()
  
  anchordata <- getInequalData(indicator=input$focus_indicator_compare,  
                           stratifier=input$focus_dimension_compare, 
                           countries=input$focus_country_compare, 
                           years=input$focus_year_compare, 
                           mostrecent=input$mostrecent_compare,
                           datasource=input$focus_data_source_compare,  
                           inequal_types=input$focus_inequal_type_compare,
                           elasticity = input$benchmarkYears,
                           multiplier1 = input$summultiplier1,
                           multiplier2 = input$summultiplier2)
 
  if(is.null(anchordata) || nrow(anchordata)==0) return()
  anchordata$anchor <- 1
#   anchordata <- getComparisonSummaries(indicator = input$focus_indicator_compare, 
#                                           stratifier = input$focus_dimension_compare, 
#                                           countries = input$focus_country_compare, 
#                                           years =  unique(input$focus_year_compare), 
#                                           mostrecent = input$mostrecent_compare,
#                                           datasource = input$focus_data_source_compare,
#                                           elasticity = input$benchmarkYears, 
#                                           matchyears=F,
#                                           summeasure = input$focus_inequal_type_compare)


  
  benchmarkdata <- getInequalData(indicator=input$focus_indicator_compare,  
                               stratifier=input$focus_dimension_compare, 
                               countries=input$benchmark_countries, 
                               years=input$focus_year_compare, 
                               mostrecent=input$mostrecent_compare,
                               datasource=input$focus_data_source_compare,  
                               inequal_types=input$focus_inequal_type_compare,
                               elasticity = input$benchmarkYears,
                               multiplier1 = input$summultiplier1,
                               multiplier2 = input$summultiplier2)
  
  
  if(is.null(benchmarkdata) || nrow(benchmarkdata)==0) return()
  benchmarkdata$anchor <- 0
#   benchmarkdata <- getComparisonSummaries(indicator = input$focus_indicator_compare, 
#                                           stratifier = input$focus_dimension_compare, 
#                                           countries = input$benchmark_countries, 
#                                           years =  unique(input$focus_year_compare), 
#                                           mostrecent = input$mostrecent_compare,
#                                           datasource = input$focus_data_source_compare,
#                                           elasticity = input$benchmarkYears, 
#                                           matchyears=F,
#                                           summeasure = input$focus_inequal_type_compare)
  
#       thedata <- getComparisonSummaries(summeasure=input$compplotSumMeasure, 
#                                         indicator=input$compplotSumHealthIndicator, 
#                                         stratifier=input$compplotSumEquityDimension, 
#                                         countries=thecountries, 
#                                         years=input$compplotSumYears, 
#                                         elasticity=input$benchmarkYears, matchyears=T)
  

  #print(head(benchmarkdata))
  #     benchmarkdata <- getComparisonCountries(indicator = input$compplotBenchHealthIndicator, 
  #                                             stratifier = input$compplotBenchEquityDimension, 
  #                                             countries = input$benchmarkCountries, 
  #                                             years =  unique(anchordata$year), 
  #                                             elasticity = input$benchmarkYears, matchyears=F)
  
  
  
  
  #   if(!is.null(anchordata) && !is.null(benchmarkdata) && ncol(anchordata)==ncol(benchmarkdata))  

  theData <- rbind(anchordata, benchmarkdata) 
  
  
  # Merge the relevant initial data with benchmarkdata
  
  return(theData)
})








# Generate a TEMPORARY view of the Comparison Summary Data
output$dataTableBenchmark <- renderDataTable({
  

  
  theData <- getBenchmarkData()

  validate(
    need(!is.null(theData) && nrow(theData)>0, "There is no data for this combination of variables")
  )


    
#     theData <- select(theData, country, year, source, indic, dimension, subgroup, estimate)
#     #theData <- select(theData, country, year, source, indic, dimension, subgroup, estimate)
#     names(theData)[names(theData)=="country" ] <- "Country" 
#     names(theData)[names(theData)=="year" ] <- "Year"
#     #names(theData)[names(theData)=="source" ] <- "Data source"
#     names(theData)[names(theData)=="indic" ] <- "Health indicator" 
#     names(theData)[names(theData)=="dimension" ] <- "Inequality dimension"
#     names(theData)[names(theData)=="subgroup" ] <- "Subgroup"
#     names(theData)[names(theData)=="estimate" ] <- "Estimate"
 
    theData <- theData %>% 
      mutate(estimate = round(estimate, 2),
             lower_95ci = round(lower_95ci,2),
             upper_95ci = round(upper_95ci,2),
             popshare = round(popshare, 2),
             national = round(national, 2))
    
    theData<-theData %>% 
      rename(
        Country                = country,
        Year                   = year,
        `Data source`          = source,
        `Health indicator`     = indic,
        `Inequality dimension` = dimension,
        Subgroup               = subgroup,
        Estimate               = estimate,
        `Lower 95%CI`          = lower_95ci,
        `Upper 95%CI`          = upper_95ci,
        `Population share %`   = popshare,
        `National estimate`    = national,
        Flag                   = flag
      ) 
    
    
    vars<-c("Country", "Year", "Health indicator", "Inequality dimension", 
                                                "Subgroup", "Estimate",  "Lower 95%CI", "Upper 95%CI",
            "Population share %")
     theData <- theData[, vars]

      return(theData)

  #})
  

}, options = list(dom = "ilftpr", pageLength = 100)  # see https://datatables.net/ for dataTable options
)


#******************************************************************************
# Comparison benchmark MAINPANEL diaggregated plot tab -----
#******************************************************************************



#####  Comparison Plot 1  Download
# Create a download button contingent on the existence of a plot of the comparison disaggregated data


# output$theComparisonPlot1_web <- renderPlot({
#   if(is.null(theComparisonPlot1())){
#     return()
#   }
#   print(theComparisonPlot1())  # Remember that print(theDataPlot) just prints the code
# }, res=90, height=exprToFunction(input$plot_height2), width=exprToFunction(input$plot_width2))



# Generate a reactive element for plotting the Disaggregated Comparison data.
# Pass to the webpage using renderPlot(print(theDataPlot))
output$theComparisonPlot1_web <- renderPlot({

  
  plotData <- getBenchmarkData()

  
  #if(is.null(plotData) || nrow(plotData)==0) return()

  validate(
    need(!is.null(plotData) && nrow(plotData)>0, "There is no data for this combination of variables")
  )
  
  
  
    plotData <- plotData[, c('country', 'year', 'indic', 'subgroup', 'dimension', 'estimate', 'se')]
    
    if(input$long_names3==T){
#       relevant_names <- which(names(.rdata[['health_indicator_abbr']]) %in% unique(plotData$indic))
#       plotData$indic <- factor(plotData$indic,
#                                levels = names(.rdata[['health_indicator_abbr']])[relevant_names],
#                                labels = unname(.rdata[['health_indicator_abbr']])[relevant_names]) 
    }
    
    
    chartopt <- list()
    chartopt <- lappend(chartopt, 'axmax' = as.integer(input$axis_limitsmax3))
    chartopt <- lappend(chartopt, 'axmin' = as.integer(input$axis_limitsmin3))
    
    if(input$main_title3 != ""){
      chartopt <- lappend(chartopt, 'main_title' = input$main_title3)
    }
    if(input$xaxis_title3 != ""){
      chartopt <- lappend(chartopt, 'xaxis_title' = input$xaxis_title3)
    }
    if(input$yaxis_title3 != ""){
      chartopt <- lappend(chartopt, 'yaxis_title' = input$yaxis_title3)
    }
    

    p <- plotFigure5(plotData, chartoptions=chartopt)
    return(p)
  
})  








#******************************************************************************
# Comparison benchmark MAINPANEL summary plot tab -----
#******************************************************************************



# output$theComparisonPlot2_web <- renderPlot({    
#   if(is.null(theComparisonPlot2())){
#     return(NULL)
#   }
#   print(theComparisonPlot2())  # Remember that print(theDataPlot) just prints the code
# }, res=90, height=exprToFunction(input$plot_height3), width=exprToFunction(input$plot_width3))
# 





# Generate a reactive element for plotting the Summary Comparison data.
# Pass to the webpage using renderPlot(print(theDataPlot))
output$theComparisonPlot2_web <- renderPlot({  
  #print("Reactive: theComparisonPlot2")
  #print(getData5())
  plotData<-getBenchmarkDataSum()
  
  validate(
    need(!is.null(plotData) && nrow(plotData)>0, "There is no data for this combination of variables")
  )
  



    plotData <- plotData[, c('country', 'ccode', 'year', 'indic', 'estimate', 'dimension', 
                             'measure', 'inequal', 'boot.se', 'se', 'anchor')]
    
    chartopt <- list()
    chartopt <- lappend(chartopt, 'xaxmax' = as.integer(input$xaxis_limitsmax4))
    chartopt <- lappend(chartopt, 'xaxmin' = as.integer(input$xaxis_limitsmin4))
    chartopt <- lappend(chartopt, 'yaxmax' = as.integer(input$yaxis_limitsmax4))
    chartopt <- lappend(chartopt, 'yaxmin' = as.integer(input$yaxis_limitsmin4))
    chartopt <- lappend(chartopt, 'yaxmin' = as.integer(input$yaxis_limitsmin4))
    
    if(input$points_ccode == TRUE){
      chartopt <- lappend(chartopt, 'points_dot' = input$points_ccode)
    }
    
    if(input$main_title4 != ""){
      chartopt <- lappend(chartopt, 'main_title' = input$main_title4)
    }
    if(input$xaxis_title4 != ""){
      chartopt <- lappend(chartopt, 'xaxis_title' = input$xaxis_title4)
    }
    if(input$yaxis_title4 != ""){
      chartopt <- lappend(chartopt, 'yaxis_title' = input$yaxis_title4)
    }
    
    p <- plotFigure6(plotData, chartoptions=chartopt)
    return(p)

})  


