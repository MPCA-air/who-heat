output$compare_inequality_ui <- renderUI({
  
  
  
  sidebarLayout(
    sidebarPanel(
      
      h4(
        #textOutput('focusCountry4')
        ),
      hr(),
      conditionalPanel(condition = "input.comparison_panel == 'inequalbenchmark'",
                       
                       uiOutput("compplotBenchHealthIndicator"),
                       uiOutput("compplotBenchEquityDimension"),
                       uiOutput("compplotBenchYears"),
                       
                       h4("Benchmark countries"),
                       
                       selectInput("benchmarkWBgroup", label = h5("Filter by country income group"),
                                   c("Low-income", 
                                     "Lower middle-income",
                                     "Upper middle-income",
                                     "High-income"),
                                   selected = NULL,
                                   multiple=T),
                       
                       selectInput("benchmarkWHOregion", label = h5("Filter by WHO region"),
                                   c("Eastern Mediterranean" = "EMR", 
                                     "Europe" = "EUR",
                                     "South East Asia" = "SEAR",
                                     "Americas" = "AMR",
                                     "Africa" = "AFR",
                                     "Western Pacific" = "WPR"),
                                   selected = NULL,
                                   multiple=T),
                       
                       
                       
                       # On the fly drop down menu to select countries of interest
                       uiOutput("benchmarkCountries"),
                       
                       sliderInput('benchmarkYears', h5('Select years'), min=0, max=5, value=2, step = 1,
                                   round = T, ticks = TRUE, animate = FALSE),
                       helpText(HTML("By how many years can the benchmark countries' data vary from the focus country's data?"),
                                style="color:black; font-size: 85%"),
                       
                       actionButton("getcomparisondata1", "Fetch data", class = "btn-success")
                       
      ),
      conditionalPanel(condition = "input.comparison_panel == 'inequaldisag'",
                       uiOutput("compplotDisagHealthIndicator"),
                       uiOutput("compplotDisagEquityDimension"),
                       uiOutput("compplotDisagYears"),
                       hr(),
                       h3('Plot options'),
                       h4('Plot dimensions'),
                       
                       sliderInput('plot_height2', h5('Height'), min=200, max=1500, value=400, step = 50,
                                   round = T, 
                                   ticks = TRUE, animate = FALSE),
                       
                       sliderInput('plot_width2', h5('Width'), min=200, max=1500, value=600, step = 50,
                                   round = T,
                                   ticks = TRUE, animate = FALSE),                                        
                       
                       h4("Plot names"),
                       checkboxInput(inputId='long_names3', label=h5('Long health indicator names'), value = FALSE),                                         
                       textInputRow(inputId="axis_limitsmin3", label=h5("Axis-min"), value = NULL),
                       textInputRow(inputId="axis_limitsmax3", label=h5("Axis-max"), value = NULL),
                       
                       textInput(inputId = 'main_title3', label = h5('Main title'), value = ""),
                       textInput(inputId = 'xaxis_title3', label = h5('X-axis label'), value = ""),
                       textInput(inputId = 'yaxis_title3', label = h5('Y-axis label'), value = "")
      ),
      conditionalPanel(condition = "input.comparison_panel == 'inequalsum'",
                       
                       uiOutput("compplotSumMeasure"),
                       uiOutput("compplotSumHealthIndicator"),
                       uiOutput("compplotSumEquityDimension"),
                       uiOutput("compplotSumYears"),
                       p(),
                       hr(),
                       p(),
                       h3('Plot options'),
                       h4('Plot dimensions'),
                       
                       sliderInput('plot_height3', h5('Height'), min=200, max=800, value=400, step = 50,
                                   round = T, 
                                   ticks = TRUE, animate = FALSE),
                       
                       sliderInput('plot_width3', h5('Width'), min=200, max=800, value=600, step = 50,
                                   round = T, 
                                   ticks = TRUE, animate = FALSE),
                       
                       
                       h4("Plot names"),
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
      bsModal(id = "compplot1Modal", title = "Download comparison plot (PDF)", trigger = "downloadCompplot1", 
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
              downloadButton(outputId = 'downloadCompPlot1', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "compplot2Modal", title = "Download comparison plot (PDF)", trigger = "downloadCompplot2", 
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
              downloadButton(outputId = 'downloadCompPlot2', label = "Start", class = NULL),
              size = "medium"),
      tabsetPanel(id = "comparison_panel", 
                  tabPanel(h6("Benchmark countries"), value='inequalbenchmark', 
                           dataTableOutput(outputId="dataTableBenchmark")
                  ),
                  tabPanel(h6("Disaggregated plot"), value='inequaldisag', 
                           uiOutput('downloadCompplot1'),
                           div(class="container-fluid", style="overflow:visible;height:1000px;", plotOutput('theComparisonPlot1_web'))
                  ),
                  tabPanel(h6("Summary plot"), value='inequalsum', 
                           # Plot points (default) or country codes on the Comparison Summary Plot
                           checkboxInput(inputId='points_ccode', 'Show country codes', value=FALSE),
                           uiOutput('downloadCompplot2'),
                           div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('theComparisonPlot2_web'))
                  )
      )#endtabsetpanel
      
      
    )# end mainPanel
  )# end sidebarLayout
  
  
  
  
})