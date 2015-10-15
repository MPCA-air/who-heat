output$compare_inequality_ui <- renderUI({
  
  
  
  sidebarLayout(
    sidebarPanel(
      tags$div(class="sectionhead1", "Variable options"),
      uiOutput("focus_country_compare"),
      uiOutput('focus_source_year_compare'),
      #uiOutput("focus_year_compare"),
      uiOutput("focus_indicator_compare"),
      uiOutput("focus_dimension_compare"),
      conditionalPanel(condition = "input.comparison_panel == 'inequalsum'",
                       uiOutput("focus_summeasure_compare_summary")),
      tags$div(class="sectionhead", "Benchmark options"),
      uiOutput("benchmarkWBgroup"),
      uiOutput("benchmarkWHOregion"),
      uiOutput("benchmark_countries"),



      conditionalPanel(condition = "input.comparison_panel == 'inequalbenchmark'",

                       
                       uiOutput('benchmarkYears'),
                       
                       helpText(HTML("By how many years can the benchmark countries' data vary from the focus country's data?"),
                                style="color:#666666; font-size: 85%")

                       
      ),
      conditionalPanel(condition = "input.comparison_panel == 'inequaldisag'",

                       tags$div(class="sectionhead", "Graph options"),
                       tags$span(class="control-label", "Select graph names"),

                       checkboxInput(inputId='long_names3', label=h5('Long health indicator names'), value = FALSE),                                         
                       textInputRow(inputId="axis_limitsmin3", label=h5("Axis-min"), value = NULL),
                       textInputRow(inputId="axis_limitsmax3", label=h5("Axis-max"), value = NULL),
                       
                       textInput(inputId = 'main_title3', label = h5('Main title'), value = ""),
                       textInput(inputId = 'xaxis_title3', label = h5('X-axis label'), value = ""),
                       textInput(inputId = 'yaxis_title3', label = h5('Y-axis label'), value = "")
      ),
      conditionalPanel(condition = "input.comparison_panel == 'inequalsum'",
                       
                       
                       tags$div(class="sectionhead", "Graph options"),
                       tags$span(class="control-label", "Select graph names"),

 
                       
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
      bsModal(id = "compplot1Modal", title = "Download comparison graph (PDF)", trigger = "btnDownloadDisagPlot_compare", 
              tags$p("Set the dimensions for the plot here and download it. "),
              br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              textInput("disagPlotWitdth_compare", "Graph width (cm)", value="15" ),
              textInput("disagPlotHeight_compare", "Graph width (cm)", value="12" ),
              selectInput(inputId="disagPlotType_compare", label='Output format:',
                          choices=c("PDF" = "PDF",
                                    "PNG" = "PNG",
                                    "JPG" = "JPG")),
              downloadButton(outputId = 'btnStartDownloadDisagPlot_compare', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "compdataDisagModal", title = "Download disaggregated data", trigger = "btnDownloadDisagPlotData_compare", 
              tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype_benchmark_disag", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadDisagPlotData_compare', label = "Start"),
              size = "medium"),
      bsModal(id = "compplot2Modal", title = "Download comparison graph (PDF)", trigger = "btnDownloadSummaryPlot_compare", 
              tags$p("Set the dimensions for the plot here and download it."),
              br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              textInput("summaryPlotWitdth_compare", "Graph width (cm)", value="15" ),
              textInput("summaryPlotHeight_compare", "Graph width (cm)", value="12" ),
              selectInput(inputId="summaryPlotType_compare", label='Output format:',
                          choices=c("PDF" = "PDF",
                                    "PNG" = "PNG",
                                    "JPG" = "JPG")),
              downloadButton(outputId = 'btnStartDownloadSummaryPlot_compare', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "compdataSummaryModal", title = "Download summary data", trigger = "btnDownloadSummaryPlotData_compare", 
              tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype_benchmark_summary", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadSummaryPlotData_compare', label = "Start"),
              size = "medium"),
      tabsetPanel(id = "comparison_panel", 
                  tabPanel(h6("Benchmark countries"), value='inequalbenchmark',
                           uiOutput('btnDownloadDisagData_compare'),
                           dataTableOutput(outputId="dataTableBenchmark")
                  ),
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), value='inequaldisag', 
                           uiOutput('btnDownloadDisagPlotData_compare'),
                           uiOutput('btnDownloadDisagPlot_compare'),
                           #uiOutput('btnDownloadDisagPlotData_compare'),
                           div(class="container-fluid", style="overflow:visible;height:1000px;", plotOutput('theComparisonPlot1_web'))
                  ),
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), value='inequalsum', 
                           # Plot points (default) or country codes on the Comparison Summary Plot
                           checkboxInput(inputId='points_ccode', 'Show country codes', value=FALSE),
                           uiOutput('btnDownloadSummaryPlotData_compare'),
                           uiOutput('btnDownloadSummaryPlot_compare'),
                           div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('theComparisonPlot2_web'))
                  )
      )#endtabsetpanel
      
      
    )# end mainPanel
  )# end sidebarLayout
  
  
  
  
})