output$explore_inequality_ui <- renderUI({
  
  
  #includeScript(file %>% .path(r_path,"base/www/js/returnTextAreaBinding.js")),
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel( condition = "(input.assessment_panel == 'datatable' || input.assessment_panel == 'dataplot')",   #### output.owndata gopt from server.R
                        
                        uiOutput("equityCountry"), #),
                        
                        radioButtons("data_source", h5("Select data sources"),
                                     c("All", "DHS", "MICS"),
                                     inline=T,
                                     selected="All"),
                        
                        h5("Select years"),
                        checkboxInput('mostrecent', 'Most recent year', FALSE),
                        
                        conditionalPanel( condition = "!input.mostrecent",                                           
                                          uiOutput("years"),
                                          hr()), 
                        
                        
                        #conditionalPanel(condition = "1==1",
                                         uiOutput("healthIndicator"),
                                         uiOutput("equityDimension"),
                                         hr(),
                                         actionButton("getdata", "Fetch Data",  class = "btn-success"),
                                        uiOutput('dataTableItems')
                        #) # End conditional panel
      ),
      conditionalPanel(condition = "input.assessment_panel == 'dataplot'",
                       tags$p(),
                       radioButtons("ai_plot_type", h3("Plot options"),
                                    c("Bar Chart" = "data_bar",
                                      "Line Chart" = "data_line"),
                                    inline=T,
                                    selected="data_line"), 
                       br(),
                       
                       h4('Plot dimensions'),
                       
                       sliderInput('plot_height1', h5('Height'), min=200, max=1500, value=400, step = 50,
                                   round = T,
                                   ticks = TRUE, animate = FALSE),
                       
                       sliderInput('plot_width1', h5('Width'), min=200, max=1500, value=400, step = 50,
                                   round = T,
                                   ticks = TRUE, animate = FALSE),
                       
                       br(),
                       h4('Plot names'),
                       
                       checkboxInput(inputId='long_names1', label='Long health indicator names', value = FALSE),
                       
                       textInputRow(inputId="axis_limitsmin1", label=h5("Axis-min"), value = NULL),
                       textInputRow(inputId="axis_limitsmax1", label=h5("Axis-max"), value = NULL),
                       
                       textInput(inputId = 'main_title1', label = h5('Main title'), value = ""),
                       textInput(inputId = 'xaxis_title1', label = h5('X-axis label'), value = ""),
                       textInput(inputId = 'yaxis_title1', label = h5('Y-axis label'), value = "")
      ),
      conditionalPanel(condition = "input.assessment_panel == 'sumtable'",
                       
                       h4(textOutput('focusCountry2')),
                       hr(),
                       
                       uiOutput("sumtableSumMeasure"),                 
                       uiOutput("sumtableHealthIndicator"),
                       uiOutput("sumtableEquityDimension"),
                       uiOutput("sumtableYears"),
                       h4('Summary measure options'),
                       checkboxInput('summultiplier1', 'MLD and TI x1000', TRUE),
                       checkboxInput('summultiplier2', 'RCI x100', TRUE),
                       sliderInput('sumsigfig', h5('Estimate precision'), min=0, max=5, value=2, round=T, width='50%'),
                       hr(),
                       radioButtons(inputId='se_type', 
                                    label=h5('Standard error options'), 
                                    choices = c('Bootstrap and Analytic' = 'both',
                                                'Analytic' = 'analytic',
                                                'Bootstrap' = 'bootstrap',
                                                'Aggregated' = 'balance'), 
                                    selected = 'balance', 
                                    inline = FALSE)
      ),
      conditionalPanel(condition = "input.assessment_panel == 'sumplot'",
                       #h4(textOutput('focusCountry3')),
                       hr(),
                       
                       uiOutput("sumplotSumMeasures"),
                       uiOutput("sumplotHealthIndicator"),
                       uiOutput("sumplotEquityDimension"),
                       uiOutput("sumplotYears"),
                       br(),
                       radioButtons("sumplot_type", h3("Plot options"),
                                    c("Bar Chart" = "data_bar",
                                      "Line Chart" = "data_line"),
                                    inline=T,
                                    selected="data_line"),
                       
                       #uiOutput('downloadSummplot'),
                       
                       h4('Plot dimensions'),
                       
                       sliderInput('plot_height_sum', h5('Height'), min=200, max=1500, value=400, step = 50,
                                   round = T,
                                   ticks = TRUE, animate = FALSE),
                       
                       sliderInput('plot_width_sum', h5('Width'), min=200, max=1500, value=600, step = 50,
                                   round = T,
                                   ticks = TRUE, animate = FALSE),
                       
                       br(),   
                       
                       h4('Plot names'),
                       
                       checkboxInput(inputId='long_names2', label='Long health indicator names', value = FALSE),
                       textInputRow(inputId="axis_limitsmin2", label=h5("Axis-min"), value = NULL),
                       textInputRow(inputId="axis_limitsmax2", label=h5("Axis-max"), value = NULL),                      
                       textInput(inputId = 'main_title2', label = h5('Main title'), value = ""),
                       textInput(inputId = 'xaxis_title2', label = h5('X-axis label'), value = ""),
                       textInput(inputId = 'yaxis_title2', label = h5('Y-axis label'), value = "")
                       
                       
                       
      )
    ),# end sidebarpanel
    
    
    mainPanel(
      tabsetPanel(id="assessment_panel",
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(tables)<h6>"), value='datatable' , 
                           uiOutput('downloadDatatable'),
                           dataTableOutput(outputId="dataTable")
                  ), 
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), value='dataplot' ,
                           uiOutput('downloadDataplot'),
                           plotOutput('theDataPlot_web')
                           ), 
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(tables)<h6>"), value='sumtable' , 
                           uiOutput('downloadSummtable'),
                           dataTableOutput(outputId="dataTableInequal")
                  ),              
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), value='sumplot' ,
                           uiOutput('downloadSummplot'),
                           plotOutput('theSumPlot_web')
                  )
      )
              
    )
  )
  
  
  
  
})