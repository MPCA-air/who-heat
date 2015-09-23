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
      bsModal(id = "datatableModal", title = "Download disaggregated data", trigger = "downloadDatatable", 
              tags$p("The data in the table will be downloaded as a text file with the values separated
                       by a comma or a tab.  Select your preferred field separator and then download the data.
               These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype1", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'downloadAnyData', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "dataplotModal", title = "Download disaggregated plot (PDF)", trigger = "downloadDataplot", 
              tags$p("Set the dimensions for the plot here and download it.  The fit of the plot is determined
                                 by the size of the paper you choose, and not by the display dimensions you select.
                                 For printing purposes, you may need to reduce the number of variables plotted"),
              br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              selectInput(inputId="papersize1", label='Plot size:',
                          choices=c("A4 portrait" = "a4",
                                    "A4 landscape" = "a4r",
                                    "US Letter portrait" = "us",
                                    "US Letter landscape" = "USr",
                                    "Custom" = "special")),
              conditionalPanel(condition = "input.papersize1 == 'special'",  # && assessment_panel = 'dataplot'
                               helpText('For custom plots the default size is 7" x 7" (i.e., 17.78cm x 17.78cm)'),
                               textInputRow(inputId="plot1_height", label="Height cm", value = '17.78'),
                               textInputRow(inputId="plot1_width", label='Width cm', value = '17.78'),
                               br(), br()
              ),
              downloadButton(outputId = 'downloadAnyPlot', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "summplotModal", title = "Download summary plot (PDF)", trigger = "downloadSummplot", 
              tags$p("Set the dimensions for the plot here and download it.  The fit of the plot is determined
                                 by the size of the paper you choose, and not by the display dimensions you select.
                                 For printing purposes, you may need to reduce the number of variables plotted"),
              br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              selectInput(inputId="papersize2", label='Plot size:',
                          choices=c("A4 portrait" = "a4",
                                    "A4 landscape" = "a4r",
                                    "US Letter portrait" = "us",
                                    "US Letter landscape" = "USr",
                                    "Custom" = "special")),
              conditionalPanel(condition = "input.papersize2 == 'special'",  # && assessment_panel = 'dataplot'
                               helpText('For custom plots the default size is 7" x 7" (i.e., 17.78cm x 17.78cm)'),
                               textInputRow(inputId="plot2_height", label="Height cm", value = '17.78'),
                               textInputRow(inputId="plot2_width", label='Width cm', value = '17.78'),
                               br(), br()
              ),
              downloadButton(outputId = 'downloadSummPlot', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "summtableModal", title = "Download summary data", trigger = "downloadSummtable", 
              tags$p("The summary measures in the table will be downloaded as a text file with the values
                                  separated by a comma or a tab.  Select your preferred field separator and then download
                                  the data.  These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype2", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'downloadAnySumm', label = "Start", class = NULL),
              size = "medium"),
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
      )#end tabsetPanel
              
    )#end mainPanel
  )
  
  
  
  
})