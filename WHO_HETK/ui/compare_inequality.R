output$compare_inequality_ui <- renderUI({
  
  
  
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("focus_country_compare"),
      uiOutput('focus_source_year_compare'),
      #uiOutput("focus_year_compare"),
      uiOutput("focus_indicator_compare"),
      uiOutput("focus_dimension_compare"),
      uiOutput("benchmark_countries"),

      hr(),
      conditionalPanel(condition = "input.comparison_panel == 'inequalbenchmark'",
                       
                       #uiOutput("focus_country"),
                       #uiOutput("compplotBenchHealthIndicator"),
                       #uiOutput("compplotBenchEquityDimension"),
                       #uiOutput("compplotBenchYears"),
                       
                       h4("Benchmark countries"),
                       uiOutput("benchmarkWBgroup"),
                       uiOutput("benchmarkWHOregion"),


                       
                       
                       
                       # On the fly drop down menu to select countries of interest
                       
                       uiOutput('benchmarkYears'),
                       
                       helpText(HTML("By how many years can the benchmark countries' data vary from the focus country's data?"),
                                style="color:black; font-size: 85%"),
                       
                       actionButton("getcomparisondata1", "Fetch data", class = "btn-success")
                       
      ),
      conditionalPanel(condition = "input.comparison_panel == 'inequaldisag'",
                       
                       #uiOutput("focus_country"),
                       #uiOutput("compplotDisagHealthIndicator"),
                       #uiOutput("compplotDisagEquityDimension"),
                       #uiOutput("compplotDisagYears"),
                       hr(),
                       h3('Plot options'),
                       h4('Select plot dimensions'),
                       
                       sliderInput('plot_height2', h5('Height'), min=200, max=1500, value=400, step = 50,
                                   round = T, 
                                   ticks = TRUE, animate = FALSE),
                       
                       sliderInput('plot_width2', h5('Width'), min=200, max=1500, value=600, step = 50,
                                   round = T,
                                   ticks = TRUE, animate = FALSE),                                        
                       
                       h4("Select plot names"),
                       checkboxInput(inputId='long_names3', label=h5('Long health indicator names'), value = FALSE),                                         
                       textInputRow(inputId="axis_limitsmin3", label=h5("Axis-min"), value = NULL),
                       textInputRow(inputId="axis_limitsmax3", label=h5("Axis-max"), value = NULL),
                       
                       textInput(inputId = 'main_title3', label = h5('Main title'), value = ""),
                       textInput(inputId = 'xaxis_title3', label = h5('X-axis label'), value = ""),
                       textInput(inputId = 'yaxis_title3', label = h5('Y-axis label'), value = "")
      ),
      conditionalPanel(condition = "input.comparison_panel == 'inequalsum'",
                       
                       
                       #uiOutput("focus_country"),
                       #uiOutput("compplotSumMeasure"),
                       uiOutput("focus_summeasure_compare_summary"),
                       #uiOutput("compplotSumHealthIndicator"),
                       #uiOutput("compplotSumEquityDimension"),
                       #uiOutput("compplotSumYears"),
                       p(),
                       hr(),
                       p(),
                       h3('Plot options'),
                       h4('Select plot dimensions'),
                       
                       sliderInput('plot_height3', h5('Height'), min=200, max=800, value=400, step = 50,
                                   round = T, 
                                   ticks = TRUE, animate = FALSE),
                       
                       sliderInput('plot_width3', h5('Width'), min=200, max=800, value=600, step = 50,
                                   round = T, 
                                   ticks = TRUE, animate = FALSE),
                       
                       
                       h4("Select plot names"),
                       ## INSERT  Long health indicator names
                       textInputRow(inputId="xaxis_limitsmin4", label = h5("X-axis min"), value = NULL),
                       textInputRow(inputId="xaxis_limitsmax4", label = h5("X-axis max"), value = NULL),
                       textInputRow(inputId="yaxis_limitsmin4", label = h5("Y-axis min"), value = NULL),
                       textInputRow(inputId="yaxis_limitsmax4", label = h5("Y-axis max"), value = NULL),
                       
                       
                       textInput(inputId = 'main_title4', label = h5('Main title'), value = ""),
                       textInput(inputId = 'xaxis_title4', label = h5('X-axis label'), value = ""),
                       textInput(inputId = 'yaxis_title4', label = h5('Y-axis label'), value = "")
      )
      
      
    ),# end sidebarpanel
    
    
    mainPanel(
      bsModal(id = "compdataModal", title = "Download disaggregated data", trigger = "btnDownloadDisagData_compare", 
              tags$p("The data in the table will be downloaded as a text file with the values separated
                       by a comma or a tab.  Select your preferred field separator and then download the data.
               These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype_benchmark", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadDisagData_compare', label = "Start"),
              size = "medium"),
      bsModal(id = "compplot1Modal", title = "Download comparison plot (PDF)", trigger = "btnDownloadDisagPlot_compare", 
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
              downloadButton(outputId = 'btnStartDownloadDisagPlot_compare', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "compplot2Modal", title = "Download comparison plot (PDF)", trigger = "btnDownloadSummaryPlot_compare", 
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
                               textInputRow(inputId="plot3_height", label="Height cm", value = '17.78'),
                               textInputRow(inputId="plot3_width", label='Width cm', value = '17.78'),
                               br(), br()
              ),
              downloadButton(outputId = 'btnStartDownloadSummaryPlot_compare', label = "Start", class = NULL),
              size = "medium"),
      tabsetPanel(id = "comparison_panel", 
                  tabPanel(h6("Benchmark countries"), value='inequalbenchmark',
                           uiOutput('btnDownloadDisagData_compare'),
                           dataTableOutput(outputId="dataTableBenchmark")
                  ),
                  tabPanel(h6("Disaggregated plot"), value='inequaldisag', 
                           uiOutput('btnDownloadDisagPlot_compare'),
                           div(class="container-fluid", style="overflow:visible;height:1000px;", plotOutput('theComparisonPlot1_web'))
                  ),
                  tabPanel(h6("Summary plot"), value='inequalsum', 
                           # Plot points (default) or country codes on the Comparison Summary Plot
                           checkboxInput(inputId='points_ccode', 'Show country codes', value=FALSE),
                           uiOutput('btnDownloadSummaryPlot_compare'),
                           div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('theComparisonPlot2_web'))
                  )
      )#endtabsetpanel
      
      
    )# end mainPanel
  )# end sidebarLayout
  
  
  
  
})