

#observe({
  
  #if(is.null(input$dataSource)) return()
.rdata<<-list()
  #if(input$dataSource == "HETK"){
    
    .rdata[['maindata']]<<-readRDS("data/maindata.RDS")
    .rdata[['inequals']]<<-readRDS("data/inequals.RDS")
    .rdata[['nationaldata']]<<-readRDS("data/nationaldata.RDS")
    .rdata[['countrynames']]<<-readRDS("data/countrynames.RDS")
    .rdata[['years']]<<-readRDS("data/years.RDS")
    #print(head(.rdata[['countrynames']]))
#  }
  
  .rdata[['all_countries']]<<-.rdata[['countrynames']]$country
  
  .rdata[['focus_data_source']]<<-"All"
  .rdata[['mostrecent']]<<-FALSE
  
  .rdata[['focus_country']]<<-"Armenia"
  .rdata[['focus_indicator']]<<-c("carep")
  .rdata[['focus_dimension']]<<-c("Sex")
  .rdata[['focus_year']]<<-c(2010)
  
  .rdata[['focus_inequal_type']]<<-'rd'
  
  .rdata[['benchmark_countries']]<<-c("Afghanistan", "Belize", "Liberia")
  
  
  .rdata[['all_table_variables']] <- c("Country" = 'Country', 
                  "Year" = "Year", 
                  "Data source" = "Data source", 
                  "Health indicator" = "Health indicator", 
                  "Inequality dimension" = "Inequality dimension",
                  "Subgroup" = "Subgroup",
                  "Estimate" = "Estimate",
                  "Lower 95%CI"  = "Lower 95%CI",
                  "Upper 95%CI"  = "Upper 95%CI",
                  "Population share %"   = "Population share %",
                  "National estimate"    = "National estimate",
                  "Flag" = "flag")
  
  .rdata[['focus_table_variables']]<-c("Country", "Year", "Source", "Health indicator", "Inequality dimension", 
                                     "Subgroup", "Estimate", "Population share %", "Lower 95%CI", "Upper 95%CI")

  
  .rdata[['unrankable_dimensions']] <<- c("Sex", "Geographical region")
  .rdata[['rankable_dimensions']]<<- c("Economic status", "Mother's education", "Place of residence")
  
  .rdata[['summary_measures_all']]<<-sort( c("Range difference (RD)" = "rd",    
                                            "Between-Group variance (BGV)" = "bgv", 
                                            "Mean difference from best performing subgroup (MDB)" = "mdb",
                                            "Mean difference from mean (MDM)" = "mdm", 
                                            "Absolute concentration index (ACI)" = "aci",  
                                            "Slope index of inequality (SII)" = "sii",
                                            "Range Ratio (RR)" = "rr", 
                                            "Index of disparity (IDis)" = "idis", 
                                            "Relative concentration index (RCI)" = "rci", 
                                            "Relative index of inequality (RII)" = "rii",
                                            "Relative Index of Inequality (Kunst Mackenbach) (RIIKM)" = "riikm", 
                                            "Theil Index (TI)" = "ti", 
                                            "Population attributable risk (PAR)" = "par",
                                            "Population attributable risk % (PAF)" = "paf", 
                                            "Mean log deviation (MLD)" = "mld"))
  
  .rdata[['summary_measures_rank']]<<- sort(c("Slope index of inequality (SII)" = "sii", 
                                             "Absolute Concentration Index (ACI)" = "aci", 
                                             "Relative concentration index (RCI)" = "rci",
                                             "Relative index of inequality (RII)" = "rii",
                                             "Relative Index of Inequality (Kunst Mackenbach) (RIIKM)" = "riikm"))
  
    
    
  .rdata[['summary_measures_unrank']]<<-sort(c("Between-Group Variance (BGV)" = "bgv", 
                                            "Index of disparity (IDis)" = "idis", 
                                            "Mean difference from best performing subgroup (MDB)" = "mdb",
                                            "Mean difference from mean (MDM)" = "mdm", 
                                            "Population attributable risk % (PAF)" = "paf", 
                                            "Population attributable risk (PAR)" = "par", 
                                            "Range difference (RD)" = "rd",
                                            "Relative index of inequality (RII)" = "rii", 
                                            "Range Ratio (RR)" = "rr", 
                                            "Theil Index (TI)" = "ti",
                                            "Mean log deviation (MLD)" = "mld"))
  

  
  
  .rdata[['equity_dimensions']] <<- c("Economic status", "Geographic region", "Mother's education",
                                      "Place of residence","Sex")
  .rdata[['core_indicators']]<<-c("Antenatal care coverage - at least one visit (2/3 years) (%)" = "anc1",
                                  "Antenatal care coverage - at least one visit (5 years) (%)" = "anc15",
                                  "Antenatal care coverage - at least four visits (2/3 years) (%)" = "anc4",
                                  "Antenatal care coverage - at least four visits (5 years) (%)" = "anc45",
                                  #                     "Antibiotic treatment in children with ARI symptoms (%)" = "antip",
                                  #                     "Adolescent fertility rate (per 1000 girls aged 15-19 years)" = "asfr1",
                                  "BCG immunization coverage among 1-year-olds (%)" = "bcgv",
                                  "Children (<5 years) with ARI symptoms taken to facility (%)" = "carep",
                                  #                     "Co-coverage (less than 3 interventions)" = "cc31",
                                  #                     "Co-coverage (less than 3 interventions)" = "cc34",
                                  #                     "Co-coverage (6 or more interventions)" = "cc61",
                                  #                     "Co-coverage (6 or more interventions)" = "cc64",
                                  #                     "Composite coverage index" = "cci",
                                  #                     "Child mortality rate (per 1000 live births)" = "cmr",
                                  "Contraceptive prevalence - modern methods (%)" = "cpmo",
                                  "Contraceptive prevalence - modern and traditional methods (%)" = "cpmt",
                                  "Births by caesarean section (2/3 years) (%)" = "csection",
                                  "Births by caesarean section (5 years) (%)" = "csection5",
                                  "DTP3 immunization coverage among 1-year-olds (%)" = "dptv",
                                  "Early initiation of breastfeeding (2/3 years) (%)" = "ebreast",
                                  "Early initiation of breastfeeding (5 years) (%)" = "ebreast5",
                                  "Family planning needs satisfied (%)" = "fps",
                                  "Full immunization coverage among 1-year-olds (%)" = "fullv",
                                  #                     "Hib immunization coverage among 1-year-olds (%)" = "hib3v",
                                  #                     "Infant mortality rate (deaths per 1000 live births)" = "imr",
                                  #                     "Intermittent preventive treatment for malaria during pregnancy (%)" = "iptp",
                                  "Children (<5 years) sleeping under insecticide-treated nets (%)" = "itnch",
                                  "Pregnant women sleeping under insecticide-treated nets (%)" = "itnwm",
                                  "Measles immunization coverage among 1-year-olds (%)" = "mslv",
                                  #                     "Neonatal mortality rate (deaths per 1000 live births)" = "nmr",
                                  #                     "Prevalence of obesity in non-pregnant women (15-49 years) (%)" = "obesewm",
                                  #                     "Children (<5 years) with diarrhoea receiving ORS (%)" = "ors",
                                  "Children (<5 years) with diarrhoea receiving ORT and continued feeding (%)" = "ort",
                                  #                     "Children (<3 years) overweight (%)" = "overwgt3",
                                  #                     "Children (<5 years) overweight (%)" = "overwgt5",
                                  #                     "Postnatal care for babies (%)" = "pncall",
                                  #                     "Postnatal care for babies born outside a health facility (%)" = "pnchome",
                                  #                     "Postnatal care for women (%)" = "pncwm",
                                  #                     "Postnatal mortality rate (per 1000 live births)" = "pnmr",
                                  "Polio immunization coverage among 1-year-olds (%)" = "poliov",
                                  #                     "Population using improved sanitation facilities (%)" = "sanit",
                                  "Births attended by skilled health personnel (2/3 years) (%)" = "sba",
                                  "Births attended by skilled health personnel (5 years) (%)" = "sba5",
                                  #                     "Children (<3 years) stunted (%)" = "stunt3",
                                  #                     "Children (<5 years) stunted (%)" = "stunt5",
                                  #                     "Total fertility rate (per woman)" = "tfr",
                                  #                     "Under-five mortality rate (deaths per 1000 live births)" = "u5mr",
                                  #                     "Children (<3 years) underweight (%)" = "uweight3",
                                  #                     "Children (<5 years) underweight (%)" = "uweight5",
                                  "Children (6-59 months) who received vitamin A supplementation (%)" = "vita")
  #                     "Children (<3 years) wasted (%)" = "wast3",
  #                     "Children (<5 years) wasted (%)" = "wast5",
  #                     "Population using improved drinking water sources (%)" = "water")
  
  .rdata[['full_indicators']] <<-c("Antenatal care coverage - at least one visit (2/3 years) (%)" = "anc1",
    "Antenatal care coverage - at least one visit (5 years) (%)" = "anc15",
    "Antenatal care coverage - at least four visits (2/3 years) (%)" = "anc4",
    "Antenatal care coverage - at least four visits (5 years) (%)" = "anc45",
    #                     "Antibiotic treatment in children with ARI symptoms (%)" = "antip",
    #                     "Adolescent fertility rate (per 1000 girls aged 15-19 years)" = "asfr1",
    "BCG immunization coverage among 1-year-olds (%)" = "bcgv",
    "Children (<5 years) with ARI symptoms taken to facility (%)" = "carep",
    #                     "Co-coverage (less than 3 interventions)" = "cc31",
    #                     "Co-coverage (less than 3 interventions)" = "cc34",
    #                     "Co-coverage (6 or more interventions)" = "cc61",
    #                     "Co-coverage (6 or more interventions)" = "cc64",
    #                     "Composite coverage index" = "cci",
    #                     "Child mortality rate (per 1000 live births)" = "cmr",
    "Contraceptive prevalence - modern methods (%)" = "cpmo",
    "Contraceptive prevalence - modern and traditional methods (%)" = "cpmt",
    "Births by caesarean section (2/3 years) (%)" = "csection",
    "Births by caesarean section (5 years) (%)" = "csection5",
    "DTP3 immunization coverage among 1-year-olds (%)" = "dptv",
    "Early initiation of breastfeeding (2/3 years) (%)" = "ebreast",
    "Early initiation of breastfeeding (5 years) (%)" = "ebreast5",
    "Family planning needs satisfied (%)" = "fps",
    "Full immunization coverage among 1-year-olds (%)" = "fullv",
    #                     "Hib immunization coverage among 1-year-olds (%)" = "hib3v",
    #                     "Infant mortality rate (deaths per 1000 live births)" = "imr",
    #                     "Intermittent preventive treatment for malaria during pregnancy (%)" = "iptp",
    "Children (<5 years) sleeping under insecticide-treated nets (%)" = "itnch",
    "Pregnant women sleeping under insecticide-treated nets (%)" = "itnwm",
    "Measles immunization coverage among 1-year-olds (%)" = "mslv",
    #                     "Neonatal mortality rate (deaths per 1000 live births)" = "nmr",
    #                     "Prevalence of obesity in non-pregnant women (15-49 years) (%)" = "obesewm",
    #                     "Children (<5 years) with diarrhoea receiving ORS (%)" = "ors",
    "Children (<5 years) with diarrhoea receiving ORT and continued feeding (%)" = "ort",
    #                     "Children (<3 years) overweight (%)" = "overwgt3",
    #                     "Children (<5 years) overweight (%)" = "overwgt5",
    #                     "Postnatal care for babies (%)" = "pncall",
    #                     "Postnatal care for babies born outside a health facility (%)" = "pnchome",
    #                     "Postnatal care for women (%)" = "pncwm",
    #                     "Postnatal mortality rate (per 1000 live births)" = "pnmr",
    "Polio immunization coverage among 1-year-olds (%)" = "poliov",
    #                     "Population using improved sanitation facilities (%)" = "sanit",
    "Births attended by skilled health personnel (2/3 years) (%)" = "sba",
    "Births attended by skilled health personnel (5 years) (%)" = "sba5",
    #                     "Children (<3 years) stunted (%)" = "stunt3",
    #                     "Children (<5 years) stunted (%)" = "stunt5",
    #                     "Total fertility rate (per woman)" = "tfr",
    #                     "Under-five mortality rate (deaths per 1000 live births)" = "u5mr",
    #                     "Children (<3 years) underweight (%)" = "uweight3",
    #                     "Children (<5 years) underweight (%)" = "uweight5",
    "Children (6-59 months) who received vitamin A supplementation (%)" = "vita")
  #                     "Children (<3 years) wasted (%)" = "wast3",
  #                     "Children (<5 years) wasted (%)" = "wast5",
  #                     "Population using improved drinking water sources (%)" = "water")
  
  
  .rdata[['health_indicator_abbr']] <<- c("anc1" = "Antenatal care coverage - at least one visit (2/3 years) (%)",
                                    "anc15" = "Antenatal care coverage - at least one visit (5 years) (%)",
                                    "anc4" = "Antenatal care coverage - at least four visits (2/3 years) (%)",
                                    "anc45" = "Antenatal care coverage - at least four visits (5 years) (%)",
                                    #                                  "antip" = "Antibiotic treatment in children with ARI symptoms (%)",
                                    #                                  "asfr1" = "Adolescent fertility rate (per 1000 girls aged 15-19 years)",
                                    "bcgv" = "BCG immunization coverage among 1-year-olds (%)",
                                    "carep" = "Children (<5 years) with ARI symptoms taken to facility (%)",
                                    #                                   "cc31" = "Co-coverage (less than 3 interventions)",
                                    #                                   "cc34" = "Co-coverage (less than 3 interventions)",
                                    #                                   "cc61" = "Co-coverage (6 or more interventions)",
                                    #                                   "cc64" = "Co-coverage (6 or more interventions)",
                                    #                                   "cci" = "Composite coverage index",
                                    #                                   "cmr" = "Child mortality rate (per 1000 live births)",
                                    "cpmo" = "Contraceptive prevalence - modern methods (%)",
                                    "cpmt" = "Contraceptive prevalence - modern and traditional methods (%)",
                                    "csection" = "Births by caesarean section (2/3 years) (%)",
                                    "csection5" = "Births by caesarean section (5 years) (%)",
                                    "dptv" = "DTP3 immunization coverage among 1-year-olds (%)",
                                    "ebreast" = "Early initiation of breastfeeding (2/3 years) (%)",
                                    "ebreast5" = "Early initiation of breastfeeding (5 years) (%)",
                                    "fps" = "Family planning needs satisfied (%)",
                                    "fullv" = "Full immunization coverage among 1-year-olds (%)",
                                    #                                   "hib3v" = "Hib immunization coverage among 1-year-olds (%)",
                                    #                                   "imr" = "Infant mortality rate (deaths per 1000 live births)",
                                    #                                   "iptp" = "Intermittent preventive treatment for malaria during pregnancy (%)",
                                    "itnch" = "Children (<5 years) sleeping under insecticide-treated nets (%)",
                                    "itnwm" = "Pregnant women sleeping under insecticide-treated nets (%)",
                                    "mslv" = "Measles immunization coverage among 1-year-olds (%)",
                                    #                                   "nmr" = "Neonatal mortality rate (deaths per 1000 live births)",
                                    #                                   "obesewm" = "Prevalence of obesity in non-pregnant women (15-49 years) (%)",
                                    #                                   "ors" = "Children (<5 years) with diarrhoea receiving ORS (%)",
                                    "ort" = "Children (<5 years) with diarrhoea receiving ORT and continued feeding (%)",
                                    #                                   "overwgt3" = "Children (<3 years) overweight (%)",
                                    #                                   "overwgt5" = "Children (<5 years) overweight (%)",
                                    #                                   "pncall" = "Postnatal care for babies (%)",
                                    #                                   "pnchome" = "Postnatal care for babies born outside a health facility (%)",
                                    #                                   "pncwm" = "Postnatal care for women (%)",
                                    #                                   "pnmr" = "Postnatal mortality rate (per 1000 live births)",
                                    "poliov" = "Polio immunization coverage among 1-year-olds (%)",
                                    #                                   "sanit" = "Population using improved sanitation facilities (%)",
                                    "sba" = "Births attended by skilled health personnel (2/3 years) (%)",
                                    "sba5" = "Births attended by skilled health personnel (5 years) (%)",
                                    #                                   "stunt3" = "Children (<3 years) stunted (%)",
                                    #                                   "stunt5" = "Children (<5 years) stunted (%)",
                                    #                                   "tfr" = "Total fertility rate (per woman)",
                                    #                                   "u5mr" = "Under-five mortality rate (deaths per 1000 live births)",
                                    #                                   "uweight3" = "Children (<3 years) underweight (%)",
                                    #                                   "uweight5" = "Children (<5 years) underweight (%)",
                                    "vita" = "Children (6-59 months) who received vitamin A supplementation (%)")
  #                                   "wast3" = "Children (<3 years) wasted (%)",
  #                                   "wast5" = "Children (<5 years) wasted (%)",
  #                                   "water" = "Population using improved drinking water sources (%)")
  

  #})







reactive({
  
  
  WBgroup      <- input$benchmarkWBgroup
  WHOregion    <- input$benchmarkWHOregion
  focusCountry <- .rdata[['focus_country']]
  
  filt_WBgroup   <- TRUE
  filt_WHOregion <- TRUE
  
  
  if(!is.null(WBgroup) & all(WBgroup != "")) 
    filt_WBgroup <- quote(wbincome2014_4cat %in% WBgroup)
  
  if(!is.null(WHOregion) & all(WHOregion != "")) 
    filt_WHOregion <- quote(wbincome2014_4cat %in% WHOregion)
  
  
  countries <- filter(.rdata[['countrynames']], filt_WBgroup, filt_WHOregion) %>% 
    select(country) %>% .$country
  
  .rdata[['benchmark_countries']] <<-countries
  
  
  #return(countries)
  
  
})





##############################################################
# Explore inequality: all tabs
#############################################################

output$focus_country_explore <- renderUI({
  
  focusCountry_selector("focus_country_explore")
  
})



output$focus_source_year_explore <- renderUI({
  
  selectYears <- getFilteredYear(country=input$focus_country_explore, datasource=input$data_source_explore)
  .rdata[['focus_year']] <- selectYears[1]
  
  list(
  conditionalPanel(condition = "input.assessment_panel == 'datatable' | input.assessment_panel == 'dataplot'",
  radioButtons("focus_data_source_explore", h5("Select data sources"),
               c("All", "DHS", "MICS"),
               inline=TRUE,
               selected="All")
  ),
  
  h5("Select years"),
  checkboxInput('mostrecent_explore', 'Most recent year', .rdata[['mostrecent']]),
  
  conditionalPanel( condition = "!input.mostrecent_explore",  
 
                    selectInput(inputId="focus_year_explore", 
                                label='', 
                                choices=selectYears, 
                                multiple=T, 
                                selected=.rdata[['focus_year']])
                    )
  )
})



##############################################################
# Explore inequality: disggregated table and plot-----
#############################################################



output$focus_indicator_explore <- renderUI({
  
  focusIndicator_selector("focus_indicator_explore", multiple=TRUE, core=FALSE)
  
})


output$focus_dimension_explore <- renderUI({
  
  focusDimension_selector("focus_dimension_explore", multiple=TRUE)
  
})



# Return the requested dataset based on the UI selection (dataSource)
datasetInput <- reactive({

  
  
.rdata[['focus_country']] <- input$focus_country_explore
.rdata[['focus_data_source']] <- input$focus_data_source_explore
.rdata[['mostrecent']] <- input$mostrecent_explore
.rdata[['focus_year']] <- input$focus_year_explore

.rdata[['focus_indicator']] <- input$focus_indicator_explore
.rdata[['focus_dimension']] <- input$focus_dimension_explore


  
  getHETKdata(indicator=.rdata[['focus_indicator']], 
              stratifier=.rdata[['focus_dimension']],  # in hetkdb.R
              countries=.rdata[['focus_country']], 
              years=.rdata[['focus_year']], 
              mostrecent=.rdata[['mostrecent']],
              datasource=.rdata[['focus_data_source']])
  

})




# Generate a view of the Managed Data
output$dataTable <- renderDataTable({
  

  
  theData <- datasetInput()
  
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
  
  #if(theData[["Data source"]][1] %in% c('DHS', 'MICS')){
  #print(head(theData))
    theData <- theData[, input$dataTableItems]
    
#   }
#   else{
#     theData <- theData[, c('Country', 'Year', 'Data source', 'Health indicator', 
#                            'Inequality dimension', 'Subgroup', 'Estimate')]
#   }

  
  theData
}, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
)




##############################################################
# Explore inequality: disaggregated table ----
#############################################################








# Set up the selectInput for the display of data in the Disaggregated Data Table view
output$dataTableItems <- renderUI({

  
  if(!is.null(input$dataTableItems)) .rdata[['focus_table_variables']] <- input$dataTableItems
    
    list(
      h4("Table options"),
    selectInput(inputId = "dataTableItems",
                h5("Select table content"),
                choices = .rdata[['all_table_variables']],
                selected = .rdata[['focus_table_variables']],
                multiple=TRUE)
    )

})



# Create a download button contingent on data in the table
output$downloadDatatable <- renderUI({
  #print("In downloadDatatable")
  
  theData <- datasetInput()
  if(nrow(theData)==0){
    return()
  } else {
     list(br(),
          actionButton("downloadDatatable", "Download table", class = "btn-primary"))
   }  
})






#Handler for downloading the data selected in the modal download table
output$downloadAnyData <- downloadHandler(
  filename = function() {
    paste(input$focus_country, Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    sep <- switch(input$filetype1, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    write.table(datasetInput(), file, sep = sep,
                row.names = FALSE)
  }
)





##############################################################
# Explore inequality: disaggregated plot ----
#############################################################


# Create a download button contingent on the existence of a plot of the disaggregated data
output$downloadDataplot <- renderUI({
  #print("In downloadDataplot")
  
  thePlot <- theDataPlot()
  if(is.null(thePlot)){
    return()
  } else {
    list(br(),
         actionButton("downloadDataplot", "Download Plot", class = "btn-primary"))
  }  
})


output$theDataPlot_web <- renderPlot({
  #print("In theDataPlot_web")
  
  print(theDataPlot())  # Remember that print(theDataPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height1), width=exprToFunction(input$plot_width1))


# Generate a reactive element for plotting the Managed Data.
# Pass to the webpage using renderPlot(print(theDataPlot))
theDataPlot <- reactive({ 
  #print("In theDataPlot")
  
  #print("Reactive: theDataPlot")
  plotData <- datasetInput()
  plotData <- select(plotData, country, year, indic, subgroup, dimension, estimate, se)

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
    
    
    
    if(input$long_names1==T){
      relevant_names <- which(names(.rdata[['health_indicator_abbr']]) %in% unique(plotData$indic))
      plotData$indic <- factor(plotData$indic,
                               levels = names(.rdata[['health_indicator_abbr']])[relevant_names],
                               labels = unname(.rdata[['health_indicator_abbr']])[relevant_names]) 
    }
    
    if(input$assessment_panel == 'dataplot' & input$ai_plot_type=='data_bar'){        
      p <- plotFigure1(plotData, chartoptions=chartopt)
      return(p)
    }
    if(input$assessment_panel == 'dataplot' & input$ai_plot_type=='data_line'){
      
      p <- plotFigure2(plotData, chartoptions=chartopt)
      return(p)
    }
  }
  else{
    return()
  }
  
})  


# Handler for downloading the data selected in the modal download table
output$downloadAnyPlot <- downloadHandler(
  filename = function() { 
    paste(input$focus_country, '_disag_', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    pdf(file, width=(as.numeric(input$plot1_width)/2.54), height=(as.numeric(input$plot1_height)/2.45), paper=input$papersize1)
    print(theDataPlot()) 
    dev.off()
  }
)   




##############################################################
# Explore inequality: summary table and plot ----
#############################################################

# output$focus_indicator_explore_summary <- renderUI({
#   
#   focusIndicator_selector("focus_indicator_explore_summary", multiple=TRUE, core=TRUE)
#   
# })
# 
# 
# output$focus_dimension_explore_summary <- renderUI({
#   
#   focusDimension_selector("focus_dimension_explore_summary", multiple=TRUE)
#   
# })


output$focus_summeasure_explore_summary <- renderUI({
  focusInequalType_selector("focus_inequal_type", input$focus_dimension_explore)
})

##############################################################
# Explore inequality:: summary table ----
#############################################################




### Creating reactive input for the Summary Tables
###

# output$focus_summeasure_explore_summary <- renderUI({
#   focusSummaryMeasure_selector("focus_summeasure_explore_summary")
# })

# output$sumtableHealthIndicator <- renderUI({    
#   # Multiple select for the health indicator 
#   if(is.null(input$healthIndicator)){ 
#     selectionOptions <- c()
#   }
#   else{
#     selectionOptions <- sort(input$healthIndicator)
#     #print(input$healthIndicator)
#     selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
#   }
#   selectInput("sumtableHealthIndicator", 
#               h5("Select health indicators"), 
#               choices=selectionOptions, 
#               selected=selectionOptions, 
#               multiple=T)
# })




# output$sumtableEquityDimension <- renderUI({    
#   # Multiple select for the equity indicator 
#   if(is.null(input$equityDimension)){ 
#     selectionOptions <- c()
#   }
#   else{
#     selectionOptions <- sort(input$equityDimension)
#   }
#   selectInput("sumtableEquityDimension", 
#               h5("Select inequality dimensions"), 
#               choices=selectionOptions, 
#               selected=selectionOptions, 
#               multiple=T)
# })


# output$sumtableYears <- renderUI({    
#   # Multiple select for the years of interest
#   yearsOfInterest <- sort(unique(datasetInput()$year))
#   if(is.null(yearsOfInterest)){ 
#     selectionOptions <- c()
#   }
#   else{
#     selectionOptions <- yearsOfInterest
#   }
#   selectInput("sumtableYears", 
#               h5("Select years"), 
#               choices=selectionOptions, 
#               selected=selectionOptions, 
#               multiple=T)
# })


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





datasetInequal <- reactive({
  

  .rdata[['focus_country']] <- input$focus_country_explore
  .rdata[['focus_data_source']] <- input$focus_data_source_explore
  .rdata[['mostrecent']] <- input$mostrecent_explore
  .rdata[['focus_year']] <- input$focus_year_explore
  .rdata[['focus_indicator']] <- input$focus_indicator_explore
  .rdata[['focus_dimension']] <- input$focus_dimension_explore
  
  .rdata[['focus_inequal_type']] <- input$focus_inequal_type
  
  #if(input$dataSource=='HETK' & input$assessment_panel=='sumtable'){
    #print('Getting equity data table a')
    #print(.rdata[['focus_year']])
    ineqDF <- getInequal(indicator=.rdata[['focus_indicator']], 
                         stratifier=.rdata[['focus_dimension']], 
                         countries=.rdata[['focus_country']], 
                         years=.rdata[['focus_year']], 
                         mostrecent=.rdata[['mostrecent']],
                         datasource=.rdata[['focus_data_source']],  
                         inequal_types=.rdata[['focus_inequal_type']])

    #print(head(ineqDF))
    #return(ineqDF)
  #}    
  if(input$assessment_panel=='sumplot'){
    #print('Getting equity data plot')
#     ineqDF <- getInequal(indicator=.rdata[['focus_indicator']], 
#                          stratifier=.rdata[['focus_dimension']], 
#                          countries=.rdata[['focus_country']], 
#                          years=.rdata[['focus_year']],  
#                          inequal_types=.rdata[['focus_summary_measure']])
    ineqDF$boot.se[ ineqDF$boot.se == 0] <- NA
    ineqDF$se[ ineqDF$se == 0] <- NA
    
    return(ineqDF)
  }  
    
    return(ineqDF)
})



# Generate a view of the HETKB 
output$dataTableInequal <- renderDataTable({
  #print("In dataTableInequal")

  theData <- datasetInequal()

  if(!is.null(theData) && nrow(theData)>0){
    #theData <- datasetInequal()
    
    # this is somewhat confusing because theData may not have most of these
    # this could be much cleaner
    

    
    #print("In dataTableInequal c")
    var_names <- names(theData)
    # Round the data to selected significant figure
    
    theData[, c('inequal', 'se', 'boot.se', 'combo.se', 'se.lowerci', 'se.upperci', 'boot.lowerci', 'boot.upperci', 'combo.lowerci', 'combo.upperci' )] <- 
      round(theData[, c('inequal', 'se', 'boot.se', 'combo.se', 'se.lowerci', 'se.upperci', 'boot.lowerci', 'boot.upperci', 'combo.lowerci', 'combo.upperci' )], input$sumsigfig)
    
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
    
    #print(theData)
  }
#   if(is.null(theData) || nrow(theData)==0){
#     return()
#   }
#   if(nrow(theData)==0){
#     return()
#   }
  theData
}, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
)




# Handler for downloading the data selected in the modal download table
output$downloadAnySumm <- downloadHandler(
  filename = function() {
    paste(input$focus_country, Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    sep <- switch(input$filetype2, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    dat <- datasetInequal()

    
    write.table(dat, file, sep = sep,
                row.names = FALSE)
  }
)


output$focusCountry2 <- renderText({ 
  if(length(input$focus_country) > 0){
    return(input$focus_country)
  }
})


##############################################################
# required in explore inequality: summary plot
#############################################################



### Creating reactive input for the Summary Plots
###

# output$sumplotSumMeasures <- renderUI({
#   if(length(input$sumplotEquityDimension)>0){
#     if(input$sumplotEquityDimension  %in% .rdata[['rankable_dimensions']]){
#       selectionOptions <- .rdata[['summary_measures_all']]
#     }
#     if(!input$sumplotEquityDimension  %in% .rdata[['rankable_dimensions']]){
#       selectionOptions <- .rdata[['summary_measures_unrank']]
#     }
#   }
#   else{
#     selectionOptions <- NULL
#   }
#   selectInput("sumplotSumMeasures", 
#               h5("Select summary measure"), 
#               choices=selectionOptions, 
#               selected=c("Range difference" = "rd"), 
#               multiple=F)
# })


output$sumplotHealthIndicator <- renderUI({    
  # Multiple select for the health indicator 
  if(length(input$sumtableHealthIndicator)<1){ 
    selectionOptions <- c()
  }
  else{    
    selectionOptions <- sort(input$sumtableHealthIndicator)
    selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
  }
  selectInput("sumplotHealthIndicator", 
              h5("Select health indicators"), 
              choices = selectionOptions, 
              selected = selectionOptions,
              multiple = T)
})


output$sumplotEquityDimension <- renderUI({    
  # Multiple select for the equity indicator 
  if(is.null(input$equityDimension)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- input$equityDimension
  }
  selectInput("sumplotEquityDimension", 
              h5("Select inequality dimensions"), 
              choices=selectionOptions, 
              multiple=F)
})


output$sumplotYears <- renderUI({    
  # Multiple select for the years of interest
  if(length(input$sumtableYears)<1){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- input$sumtableYears
  }
  selectInput("sumplotYears", 
              h5("Select years"), 
              choices=selectionOptions, 
              selected=selectionOptions, 
              multiple=T)
})



output$downloadSummplot <- renderUI({
  thePlot <- theSummaryPlot()
  if(is.null(thePlot)){
    return()
  } else {
    list(br(),
         actionButton("downloadSummplot", "Download Plot", class = "btn-primary"))
  }  
})

output$theSumPlot_web <- renderPlot({
  print(theSummaryPlot())  # Remember that print(theSummaryPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height_sum), width=exprToFunction(input$plot_width_sum))





# Generate a reactive element for plotting the Summary Data.
# Pass to the webpage using renderPlot(print(theDataPlot))
theSummaryPlot <- reactive({ 
  
  
  plotData <- datasetInequal()
  #print(head(plotData))
  
  
  #print(class(plotData))
  #print("Reactive: theSummaryPlot")
  if(is.null(plotData)) return()

    #plotData <- datasetInequal()
    #if(class(plotData)=="data.frame" && nrow(plotData)>0 ){
      
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
      
#       relevant.rows <- which(plotData$year %in% input$sumplotYears & plotData$indic %in% input$sumplotHealthIndicator & 
#                                plotData$dimension %in% input$sumplotEquityDimension & plotData$measure %in% input$sumplotSumMeasures)
#       
      #if(length(relevant.rows)>0){  # This will generally fail because the Health Indicator has not *yet* been selected
        
        #plotData <- plotData[relevant.rows, ]      
        
        if(input$long_names2==T){
          #relevant_names <- which(names(.rdata[['health_indicator_abbr']]) %in% unique(plotData$indic))
          plotData$indic <- factor(plotData$indic,
                                   levels = names(.rdata[['health_indicator_abbr']]),
                                   labels = unname(.rdata[['health_indicator_abbr']])) 
        }
        
        
        if(input$assessment_panel == 'sumplot' & input$sumplot_type=='data_bar'){                   
          p <- plotFigure3(plotData, chartoptions=chartopt)
          return(p)
        }
        if(input$assessment_panel == 'sumplot' & input$sumplot_type=='data_line'){          
          p <- plotFigure4(plotData, chartoptions=chartopt)
          return(p)
        }    
      
      
      #}
    #}
#     else{
#       return()
#     }
  
})  





# Handler for downloading the data selected in the modal download table
output$downloadSummPlot <- downloadHandler(
  filename = function() { 
    paste(input$focus_country, '_summ_', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    pdf(file, width=(as.numeric(input$plot2_width)/2.54), height=(as.numeric(input$plot2_height)/2.45), paper=input$papersize2)
    print(theSummaryPlot()) 
    dev.off()
  }
)   










##############################################################
# Compare inquality: sidepanel
#############################################################

output$focus_country_compare <- renderUI({
  
  focusCountry_selector("focus_country_compare")
  
})

output$focus_indicator_compare <- renderUI({
  
  focusIndicator_selector("focus_indicator_compare", multiple=FALSE)
  
})


output$focus_year_compare <- renderUI({
  
  selectYears <- getFilteredYear(country=input$focus_country_compare)
  .rdata[['focus_year']] <- selectYears[1]
  
  list(

    h5("Select years"),
    checkboxInput('mostrecent_compare', 'Most recent year', .rdata[['mostrecent']]),
    
    conditionalPanel( condition = "!input.mostrecent_compare",  
                      
                      selectInput(inputId="focus_year_compare", 
                                  label='', 
                                  choices=selectYears, 
                                  multiple=FALSE, 
                                  selected=.rdata[['focus_year']][1])
    )
  )
})




output$focus_dimension_compare <- renderUI({
  
  focusDimension_selector("focus_dimension_compare", multiple=FALSE)
  
})



output$benchmark_countries <- renderUI({
  
  if(is.null(.rdata[['benchmark_countries']])) return()
  countries <- getFilteredCountries(input$benchmarkWBgroup, input$benchmarkWHOregion) 
  focus_country <- .rdata[['focus_country']]
  countries <- countries[!countries%in%focus_country]
  print(.rdata[['benchmark_countries']])
  
  selectInput("benchmark_countries", 
              h5("Select countries"), 
              choices=countries, 
              selected=.rdata[['benchmark_countries']],
              multiple=TRUE)
})





output$compplotBenchHealthIndicator <- renderUI({    
  # Multiple select for the health indicator 
  if(is.null(input$healthIndicator)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- sort(input$healthIndicator)
    selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
  }
  selectInput("compplotBenchHealthIndicator", 
              h5("Select health indicators"), 
              choice=selectionOptions, 
              selected=selectionOptions,
              multiple=T)
})


output$compplotBenchEquityDimension <- renderUI({    
  # Multiple select for the equity indicator 
  if(is.null(input$equityDimension)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- input$equityDimension
  }
  selectInput("compplotBenchEquityDimension", 
              h5("Select inequality dimensions"), 
              choices=selectionOptions, 
              multiple=F)
})


output$compplotBenchYears <- renderUI({    
  # Multiple select for the years of interest
  years<-getHETKdata(indicator=input$healthIndicator, 
              stratifier=input$equityDimension,  # in hetkdb.R
              countries=input$focus_country, 
              years=input$years, 
              mostrecent=input$mostrecent,
              datasource=input$data_source)
  
  yearsOfInterest <- sort(unique(years$years))
  if(is.null(yearsOfInterest)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- yearsOfInterest
  }
  selectInput("compplotBenchYears", 
              h5("Select years"), 
              choices=selectionOptions, 
              selected=max(selectionOptions), 
              multiple=F)
})

output$benchmarkCountries <- renderUI({
  countries <- getFilteredCountries(input$benchmarkWBgroup, input$benchmarkWHOregion)  
  selectInput("benchmarkCountries", 
              h5("Select countries"), 
              choices=countries, 
              selected=countries[1:5],
              multiple=T)
})



##############################################################
# Comparison disaggregated plots SIDEPANEL
#############################################################



###  Comparison Disaggregated Plots

output$compplotDisagHealthIndicator <- renderUI({    
  # Multiple select for the health indicator 
  if(is.null(input$healthIndicator)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- sort(input$healthIndicator)
    selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
  }
  selectInput("compplotDisagHealthIndicator", 
              h5("Select health indicators"), 
              choice=selectionOptions, 
              selected=selectionOptions,
              multiple=T)
})


output$compplotDisagEquityDimension <- renderUI({    
  # Multiple select for the equity indicator 
  if(is.null(input$equityDimension)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- input$equityDimension
  }
  selectInput("compplotDisagEquityDimension", 
              h5("Select inequality dimensions"), 
              choices=selectionOptions, 
              multiple=F)
})


output$compplotDisagYears <- renderUI({    
  # Multiple select for the years of interest
  yearsOfInterest <- sort(unique(datasetInput()$year))
  if(is.null(yearsOfInterest)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- yearsOfInterest
  }
  selectInput("compplotDisagYears", 
              h5("Select years"), 
              choices=selectionOptions, 
              selected=max(selectionOptions), 
              multiple=F)
})

##############################################################
# Comparison summary plots SIDEPANEL
#############################################################



# output$compplotSumMeasure <- renderUI({
#   if(length(input$compplotSumEquityDimension)>0){
#     if(input$compplotSumEquityDimension %in% .rdata[['rankable_dimensions']]){
#       selectionOptions <- .rdata[['summary_measures_all']]
#     }
#     if(!input$compplotSumEquityDimension %in% .rdata[['rankable_dimensions']]){
#       selectionOptions <- .rdata[['summary_measures_unrank']]
#     }
#   }
#   else{
#     selectionOptions <- NULL
#   }
#   
#   selectInput("compplotSumMeasure", 
#               h5("Select summary measure"), 
#               choices=selectionOptions,
#               selected=c("Range difference" = "rd"), 
#               multiple=F)
# })



output$compplotSumHealthIndicator <- renderUI({    
  # Multiple select for the health indicator 
  if(is.null(input$healthIndicator)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- sort(input$healthIndicator)
    selectionOptions <- healthIndicatorList(option='full')[which(healthIndicatorList(option='core') %in% selectionOptions)]
  }
  selectInput("compplotSumHealthIndicator", 
              h5("Select health indicators"), 
              choices=selectionOptions, 
              multiple=F)
})


output$compplotSumEquityDimension <- renderUI({    
  # Multiple select for the equity indicator 
  if(is.null(input$equityDimension)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- input$equityDimension
  }
  selectInput("compplotSumEquityDimension", 
              h5("Select inequality dimensions"), 
              choices=selectionOptions, 
              multiple=F)
})


output$compplotSumYears <- renderUI({    
  # Multiple select for the years of interest
  yearsOfInterest <- sort(unique(datasetInput()$year))
  if(is.null(yearsOfInterest)){ 
    selectionOptions <- c()
  }
  else{
    selectionOptions <- yearsOfInterest
  }
  selectInput("compplotSumYears", 
              h5("Select years"), 
              choices=selectionOptions, 
              selected=max(selectionOptions), 
              multiple=F)
})



##############################################################
# Comparison benchmark MAINPANEL -----
#############################################################



# Generate a TEMPORARY view of the Comparison Summary Data
output$dataTableBenchmark <- renderDataTable({
  
  input$benchmarkWBgroup
  input$benchmarkWHOregion
  input$getcomparisondata1

  
  
  
  .rdata[['focus_country']] <- input$focus_country_compare
  .rdata[['focus_indicator']] <- input$focus_indicator_compare
  .rdata[['focus_year']] <- input$focus_year_compare
  .rdata[['focus_dimension']] <- input$focus_dimension_compare
  .rdata[['benchmark_countries']] <- input$benchmark_countries
  
  isolate({
    
    anchordata <- getHETKdata(indicator=.rdata[['focus_indicator']], 
                stratifier=.rdata[['focus_dimension']],  # in hetkdb.R
                countries=.rdata[['focus_country']], 
                years=.rdata[['focus_year']], 
                mostrecent=.rdata[['mostrecent']],
                datasource=.rdata[['focus_data_source']])


    benchmarkdata <- getHETKdata(indicator=.rdata[['focus_indicator']], 
                              stratifier=.rdata[['focus_dimension']],  # in hetkdb.R
                              countries=.rdata[['benchmark_countries']], 
                              years=.rdata[['focus_year']], 
                              mostrecent=.rdata[['mostrecent']],
                              datasource=.rdata[['focus_data_source']])
    
    
#     benchmarkdata <- getComparisonCountries(indicator = input$compplotBenchHealthIndicator, 
#                                             stratifier = input$compplotBenchEquityDimension, 
#                                             countries = input$benchmarkCountries, 
#                                             years =  unique(anchordata$year), 
#                                             elasticity = input$benchmarkYears, matchyears=F)
  
    
    theData <- rbind(anchordata, benchmarkdata)  # Merge the relevant initial data with benchmarkdata

    if(is.null(theData)) return()
    if(nrow(theData)==0) return()

    
    theData <- select(theData, country, year, source, indic, dimension, subgroup, estimate)
    names(theData)[names(theData)=="country" ] <- "Country" 
    names(theData)[names(theData)=="year" ] <- "Year"
    names(theData)[names(theData)=="source" ] <- "Data source"
    names(theData)[names(theData)=="indic" ] <- "Health indicator" 
    names(theData)[names(theData)=="dimension" ] <- "Inequality dimension"
    names(theData)[names(theData)=="subgroup" ] <- "Subgroup"
    names(theData)[names(theData)=="estimate" ] <- "Estimate"
 
    
    
      return(theData)

  })
  

}, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
)

# 
# # A *Reactive* to fetch benchmark country disaggregated and merge it with fetched data
# getData4 <- reactive({
#   # This *reactive* fetches benchmark country data for the Disaggregated TABLE
#   input$getcomparisondata1
#   #    input$getcomparisondata2
#   #print(input$getcomparisondata1)
#   isolate({
#     
#     anchordata <- datasetInput()
#     print(head(anchordata))
#     
#     relevant.rows <- which(anchordata$year %in% input$compplotBenchYears &   # Select only the right years ...
#                              anchordata$indic == input$compplotBenchHealthIndicator &  # health indicator, and ... 
#                              anchordata$dimension == input$compplotBenchEquityDimension)  # equity dimension 
#     
#     anchordata <- anchordata[ relevant.rows , ]
#     
#     #print('Pre Benchmark')    
#     benchmarkdata <- getComparisonCountries(indicator = input$compplotBenchHealthIndicator, 
#                                             stratifier = input$compplotBenchEquityDimension, 
#                                             countries = input$benchmarkCountries, 
#                                             years =  unique(anchordata$year), 
#                                             elasticity = input$benchmarkYears, matchyears=F)
#     #print('Post Benchmark')    
#     
#     thedata <- rbind(anchordata, benchmarkdata)  # Merge the relevant initial data with benchmarkdata
#     #print(thedata)
#     if(is.null(thedata)){
#       return()
#     }
#     if(nrow(thedata)==0){
#       return()
#     }
#     else{
#       return(thedata)
#     }
#   })
# })


##############################################################
# Comparison benchmark MAINPANEL diaggregated plot tab -----
#############################################################



#####  Comparison Plot 1  Download
# Create a download button contingent on the existence of a plot of the comparison disaggregated data
output$downloadCompplot1 <- renderUI({
  thePlot <- theComparisonPlot1()
  if(is.null(thePlot)){
    return()
  } else {
    list(br(),
         actionButton("downloadCompplot1", "Download Plot", class = "btn-primary"))
  }  
})

output$theComparisonPlot1_web <- renderPlot({
  if(is.null(theComparisonPlot1())){
    return()
  }
  print(theComparisonPlot1())  # Remember that print(theDataPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height2), width=exprToFunction(input$plot_width2))



# Generate a reactive element for plotting the Disaggregated Comparison data.
# Pass to the webpage using renderPlot(print(theDataPlot))
theComparisonPlot1 <- reactive({ 

  
  plotData <- getData4a()

  if(is.null(plotData)){
    return()
  }
  else{
    plotData <- plotData[, c('country', 'year', 'indic', 'subgroup', 'dimension', 'estimate', 'se')]
    
    if(input$long_names3==T){
      relevant_names <- which(names(.rdata[['health_indicator_abbr']]) %in% unique(plotData$indic))
      plotData$indic <- factor(plotData$indic,
                               levels = names(.rdata[['health_indicator_abbr']])[relevant_names],
                               labels = unname(.rdata[['health_indicator_abbr']])[relevant_names]) 
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
  }
})  




# A *Reactive* to fetch benchmark country FOR the Disaggregated Comparison Plot
getData4a <- reactive({
  # This *reactive* fetches benchmark country data for the PLOT
  #print("Reactive: getData4a")
  if(input$comparison_panel != 'inequaldisag'){
    return()
  }
  if(length(input$compplotDisagYears)==0 | length(input$compplotDisagHealthIndicator)==0 | length(input$compplotDisagEquityDimension)==0){
    return()
  }
  #print(paste(input$compplotBenchYears, input$compplotDisagYears, input$compplotBenchHealthIndicator, input$compplotDisagHealthIndicator, input$compplotBenchEquityDimension, input$compplotDisagEquityDimension, sep=' >> '))
  if(input$compplotBenchYears == input$compplotDisagYears & input$compplotBenchHealthIndicator == input$compplotDisagHealthIndicator & input$compplotBenchEquityDimension == input$compplotDisagEquityDimension){
    return( getData4() )
  } else {
    
    anchordata <- datasetInput()
    
    relevant.rows <- which(anchordata$year %in% input$compplotDisagYears &   # Select only the right years ...
                             anchordata$indic == input$compplotDisagHealthIndicator &  # health indicator, and ... 
                             anchordata$dimension == input$compplotDisagEquityDimension)  # equity dimension 
    
    anchordata <- anchordata[ relevant.rows , ]
    
    benchmarkdata <- getComparisonCountries(indicator = input$compplotDisagHealthIndicator, 
                                            stratifier = input$compplotDisagEquityDimension, 
                                            countries = input$benchmarkCountries, 
                                            years =  unique(anchordata$year), 
                                            elasticity = input$benchmarkYears, matchyears=F)
    
    thedata <- rbind(anchordata, benchmarkdata)  # Merge the relevant initial data with benchmarkdata
    if(is.null(thedata) | nrow(thedata)==0){
      return()
    }
    else{
      return(thedata)
    }
  }
})





##############################################################
# Comparison benchmark MAINPANEL summary plot tab -----
#############################################################


#####  Comparison Plot 2  Download
# Create a download button contingent on the existence of a plot of the comparison disaggregated data
output$downloadCompplot2 <- renderUI({
  thePlot <- theComparisonPlot2()
  if(is.null(thePlot)){
    return()
  } else {
    list(br(),
         actionButton("downloadCompplot2", "Download Plot", class = "btn-primary"))
  }  
})







# Generate a reactive element for plotting the Summary Comparison data.
# Pass to the webpage using renderPlot(print(theDataPlot))
theComparisonPlot2 <- reactive({ 
  #print("Reactive: theComparisonPlot2")
  #print(getData5())
  plotData<-getData5()
  
  if(is.null(plotData)){
    return()
  }
  if(nrow(plotData)==0){
    return()
  }    
  else{
    #print('Never got here')
    plotData <- plotData[, c('country', 'ccode', 'year', 'indic', 'estimate', 'dimension', 'measure', 'inequal', 'boot.se', 'se', 'anchor')]
    
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
  }
})  






#A *Reactive* to fetch benchmark country summary data
getData5 <- reactive({
  # This *reactive* fetches benchmark country summary data
  if(length(input$focus_country) > 0){
    thecountries <- unique(c(input$focus_country, input$benchmarkCountries))
  
    thedata <- getComparisonSummaries(summeasure=input$compplotSumMeasure, 
                                      indicator=input$compplotSumHealthIndicator, 
                                      stratifier=input$compplotSumEquityDimension, 
                                      countries=thecountries, 
                                      years=input$compplotSumYears, 
                                      elasticity=input$benchmarkYears, matchyears=T)
    thedata$anchor <- 0
    thedata$anchor[thedata$country==input$focus_country] <- 1
    
  } else {
    thedata <- NULL
  }
  return(thedata)
})



output$theComparisonPlot2_web <- renderPlot({    
  if(is.null(theComparisonPlot2())){
    return()
  }
  print(theComparisonPlot2())  # Remember that print(theDataPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height3), width=exprToFunction(input$plot_width3))





##############################################################
# Comparison benchmark MAINPANEL diaggregated plot tab
#############################################################


# 
# # Focus country
# output$focusCountry1 <- renderText({ 
#   if(length(input$focus_country) > 0){
#     return(input$focus_country)
#   }
# })
# 
# 

# 
# output$focusCountry3 <- renderText({ 
#   if(length(input$focus_country) > 0){
#     return(input$focus_country)
#   }
# })
# 
# 
# output$focusCountry4 <- renderText({ 
#   if(length(input$focus_country) > 0){
#     return(input$focus_country)
#   }
# })
# 
# 
# output$focusCountry5 <- renderText({ 
#   if(length(input$focus_country) > 0){
#     return(input$focus_country)
#   }
# })
# 
# 
# 
# 
# 

# 

# 


# 
 

# 

# 
# #  Select the summary measures from the legitimate choices available, given the 
# output$sumMeasures <- renderUI({
#   if(input$mostrecent){
#     mostrecent_years <- sort(unique(datasetInput()$year))
#     return(selectInput("uddYears", 
#                        h5("Select years"), 
#                        mostrecent_years, 
#                        multiple=T))
#   }
#   if(!input$mostrecent){
#     selected_years <- input$years
#     return(selectInput("uddYears", 
#                        h5("Select years"), 
#                        choices=input$years,
#                        selected=selected_years,
#                        multiple=T))
#   }
# })
# 
# 


 


# 
 


# # Create a download button contingent on the existence of a comparison plot of the disaggregated data
# output$downloadCompplot1 <- renderUI({
#   thePlot <- theDataPlot()
#   if(is.null(thePlot)){
#     return()
#   } else {
#     list(br(),
#          actionButton("downloadCompplot1", "Download Plot", class = "btn-primary"))
#   }  
# })
# 
# 

# output$benchmarkCountriesSum <- renderUI({
#   countries <- getFilteredCountries(input$benchmarkWBgroup, input$benchmarkWHOregion, input$dataSource)   
#   if(is.null(countries)){ countries <- c()}
#   print(countries)
#   selectInput("benchmarkCountriesSum", 
#               h5("Countries"), 
#               choices=countries, 
#               selected=countries,
#               multiple=T)
# })
# 
# observe({
#   code <- digest(input$admin_code, serialize=F)
#   if(code == "6885b5b29162de9e6f0bae3347828acb"){  # 'WH02015'
#     updateCheckboxInput(session, inputId='admin_show', value = TRUE)
#   } else {
#     updateCheckboxInput(session, inputId='admin_show', value = FALSE)
#   }
# })
# 
# 
# 
# ###########################
# # Part 1 of  *Reactive* to set up the download of data from the Global Health Observatory (GHO)
# getData1 <- reactive({
#   # This *reactive* puts an approptiate URL together for according to the GHO API
#   input$getdata
#   isolate({
#     if(input$dataSource == 'GHO'){
#       gho.url <- setupGHOdata(indicator=input$gho_equityIndic, stratifier=input$equityStrata, 
#                               countries=input$focus_country, years=input$years)
#       print(gho.url)
#       return(gho.url)
#     }
#   })
# })
# 
# 
# # Part 2 of  *Reactive* to set up thje download of data from the Global Health Observatory (GHO) 
# getData1a <- reactive({
#   # This *reactive* gets the data from GHO or the HETKdb
#   input$getdata
#   isolate({  # Isolate holds of the getting of the data until the Process button in the ui.R is pressed
#     if(input$dataSource == 'GHO'){  # Get the data from GHO
#       gho.data <- getGHOdata(getData1(), indicator=input$equityIndic)
#       return (gho.data)
#     }
#   })
# })
# 
# 

# 
# # A *Reactive* to upload your own data
# getData3 <- reactive({
#   # This *reactive* uploads a users own equity data
#   
#   # input$file1 will be NULL initially. After the user selects
#   # and uploads a file, it will be a data frame with 'name',
#   # 'size', 'type', and 'datapath' columns. The 'datapath'
#   # column will contain the local filenames where the data can
#   # be found.
#   
#   inFile <- input$ownData
#   
#   if (is.null(inFile))
#     return()
#   
#   minimum_headers <- c('country', 'year', 'source', 'indic', 'dimension', 'subgroup', 'estimate', 'se', 'pop', 'flag', 'rankable', 'order', 'maxoptimum')
#   tmpDF <- read.csv(inFile$datapath, sep=input$sep_type, header=T, stringsAsFactors = F)
#   if(setdiff(names(tmpDF), minimum_headers) %in%  minimum_headers){
#     # If the header does not contains the minimum required variables, it returns NULL
#     toggleModal(session, "upload_error1")
#     return()
#   }
#   
#   # Remove rows with missing estimates or stratifiers; all else is forgiven 
#   #    tmpDF <- tmpDF[!is.na(tmpDF$estimate) & (tmpDF$dimension !=''), ]  
#   return(tmpDF)    
# })
# 
# #  Pass a NULL own data upload back to ui.R
# output$fileUploaded <- reactive({
#   theData <- getData3()
#   print(theData)
#   if(!is.null(theData)){
#     return()
#   } else {
#     return()
#   }
#   
# })
# outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
# 
# 

# 
# 
# 
# 

# 
# ## Download the datset
# output$downloadData <- downloadHandler(
#   filename = function() { paste(input$dataset, '.csv', sep='') },
#   content = function(file) {
#     write.csv(datasetInput(), file)
#   }
# )
# 
# 
# 
# # Generate a view of the Managed Data
# output$uploadTable <- renderDataTable({
#   theData <- datasetInput()
#   if(is.null(theData)){
#     return()
#   }
#   if(nrow(theData)==0){
#     return()
#   } 
#   
#   theData <- theData[, c('country', 'year', 'source', 'indic', 'dimension', 'subgroup', 'estimate', 'lower_95ci', 'upper_95ci', 'popshare', 'flag')] 
#   theData[, c('estimate', 'lower_95ci', 'upper_95ci', 'popshare')]  <-
#     round(theData[, c('estimate', 'lower_95ci', 'upper_95ci', 'popshare')] , 2)
#   names(theData)[names(theData)=="country" ] <- "Country" 
#   names(theData)[names(theData)=="year" ] <- "Year"
#   names(theData)[names(theData)=="source" ] <- "Data source"    
#   names(theData)[names(theData)=="indic" ] <- "Health indicator" 
#   names(theData)[names(theData)=="dimension" ] <- "Inequality dimension" 
#   names(theData)[names(theData)=="subgroup" ] <- "Subgroup"
#   names(theData)[names(theData)=="estimate" ] <- "Estimate"
#   names(theData)[names(theData)=="lower_95ci" ] <- "Lower 95%CI"
#   names(theData)[names(theData)=="upper_95ci" ] <- "Upper 95%CI"
#   names(theData)[names(theData)=="popshare" ] <- "Population share %"
#   names(theData)[names(theData)=="flag" ] <- "Flag"
#   #
#   theData
# }, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
# )
# 
# 
# 
# 

# 
# 
# 

# 
# 

# 
# 

# 
# # Generate a TEMPORARY view of the Comparison Summary Data
# output$dataTableCompSum <- renderDataTable({
#   if(!is.null(getData5())){
#     getData5()
#   }
# }, options = list(dom = "ilftpr", pageLength = 10)  # see https://datatables.net/ for dataTable options
# )
# 
# 
# 

# benchmarkText <- reactive({
#   theText <- NULL
#   input$getcomparisondata1
#   isolate({
#     theText <- paste(readLines('./www/benchmarkSelection.html'), collapse=" ")
#   })
#   print(theText)
#   return(theText)
# })
# 
# #   output$benchmarktxt <- htmlOutput(benchmarkText())
# 
# 
# 


# 
# ## Download the plot of the managed data
# 
# # Include a downloadable file of the plot in the output list.
# output$theDataPlot.print <- downloadHandler(
#   # downloadHandler(filename, content, contentType)'
#   
#   # filename  -- make sure it has a png extension
#   function(){
#     testStr <- input$downloadPlotFileName
#     if(tolower(substr(testStr, nchar(testStr)-3, nchar(testStr))) != ".png"){
#       testStr <- paste(testStr, "png", sep='.')
#     }      
#     return(testStr)
#   },
#   
#   # content
#   content <-function(file) {
#     png <- function(..., width, height) {  
#       # This is included to manage the size of the png plot 
#       # see: ... http://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
#       grDevices::png(...,  
#                      width=input$downloadPlotWidth, 
#                      height=input$downloadPlotHeight, 
#                      res = 300, # as.integer(input$downloadPlotHeightRes), 
#                      units = "cm")
#     }
#     ggsave(file, plot=theDataPlot(), device=png)
#   }
# )
# 
# 

# 
# 
# # Handler for downloading the data selected in the modal download plot
# output$downloadCompPlot1 <- downloadHandler(
#   filename = function() { 
#     paste(input$focus_country, '_comp_', Sys.Date(), '.pdf', sep='')
#   },
#   content = function(file) {
#     pdf(file, width=(as.numeric(input$plot1_width)/2.54), height=(as.numeric(input$plot1_height)/2.45), paper=input$papersize1)
#     print(theComparisonPlot1()) 
#     dev.off()
#   }
# )   
# 
# 

# 
# # Handler for downloading the data selected in the modal download plot
# output$downloadCompPlot2 <- downloadHandler(
#   filename = function() { 
#     paste(input$focus_country, '_comp_', Sys.Date(), '.pdf', sep='')
#   },
#   content = function(file) {
#     pdf(file, width=(as.numeric(input$plot1_width)/2.54), height=(as.numeric(input$plot1_height)/2.45), paper=input$papersize1)
#     print(theComparisonPlot2()) 
#     dev.off()
#   }
# )   