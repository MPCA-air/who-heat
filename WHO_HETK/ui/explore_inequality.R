output$explore_inequality_ui <- renderUI({
  
  
  #includeScript(file %>% .path(r_path,"base/www/js/returnTextAreaBinding.js")),
  sidebarLayout(
    sidebarPanel(
      uiOutput("focus_country_explore"), 
      uiOutput('focus_source_year_explore'),
      uiOutput("focus_indicator_explore"),
      uiOutput("focus_dimension_explore"),
      
      
      conditionalPanel(condition = "input.assessment_panel == 'datatable'",   #### output.owndata gopt from server.R

                                         hr(),
                        uiOutput('dataTableItems')

      ),
      conditionalPanel(condition = "input.assessment_panel == 'dataplot'",
                       tags$p(),
                       uiOutput("disag_plot_type"),
   
                       br(),
                       
                       #h4('Select plot dimensions'),
                       #uiOutput("disag_plot_dimensions"),
                       #br(),
                       h4('Select plot names'),
                
                       textInputRow(inputId="axis_limitsmin1", label=h5("Axis-min"), value = NULL),
                       textInputRow(inputId="axis_limitsmax1", label=h5("Axis-max"), value = NULL),
                       
                       textInput(inputId = 'main_title1', label = h5('Main title'), value = ""),
                       textInput(inputId = 'xaxis_title1', label = h5('X-axis label'), value = ""),
                       textInput(inputId = 'yaxis_title1', label = h5('Y-axis label'), value = "")
      ),
      
      conditionalPanel(condition = "input.assessment_panel == 'sumtable' | input.assessment_panel == 'sumplot'",
                       
                       uiOutput("focus_summeasure_explore_summary")
                       
      ),
      
      
      conditionalPanel(condition = "input.assessment_panel == 'sumtable'",
                       
                       hr(),
                       h4('Summary measure options'),
                       uiOutput("summary_measures"),
                       h5('Select estimate to display')
              
      ),
      conditionalPanel(condition = "input.assessment_panel == 'sumplot'",

                       br(),
                       uiOutput("summary_plot_type"),
       
                       br(),
                       #h4('Select plot dimensions'),
                      
                       #uiOutput("summary_plot_dimensions"),
                       
                       #br(),   
                       
                       h4('Select plot names'),
                       
                       #checkboxInput(inputId='long_names2', label='Long health indicator names', value = FALSE),
                       textInputRow(inputId="axis_limitsmin2", label=h5("Axis-min"), value = NULL),
                       textInputRow(inputId="axis_limitsmax2", label=h5("Axis-max"), value = NULL),                      
                       textInput(inputId = 'main_title2', label = h5('Main title'), value = ""),
                       textInput(inputId = 'xaxis_title2', label = h5('X-axis label'), value = ""),
                       textInput(inputId = 'yaxis_title2', label = h5('Y-axis label'), value = "")
                       
                       
                       
      )
    ),# end sidebarpanel
    
    
    mainPanel(
      bsModal(id = "datatableModal", title = "Download disaggregated data", trigger = "btnDownloadDisagData_explore", 
              tags$p("The data in the table will be downloaded as a text file with the values separated
                       by a comma or a tab.  Select your preferred field separator and then download the data.
               These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype1", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadDisagData_explore', label = "Start"),
              size = "medium"),
      
      bsModal(id = "dataplotModal", title = "Download disaggregated plot (PDF)", trigger = "btnDownloadDisagPlot_explore", 
              tags$p("Set the dimensions for the plot here and download it."),
              br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              textInput("disagPlotWitdth_explore", "Plot width (cm)", value="15" ),
              textInput("disagPlotHeight_explore", "Plot width (cm)", value="12" ),
              selectInput(inputId="disagPlotType_explore", label='Output format:',
                          choices=c("PDF" = "PDF",
                                    "PNG" = "PNG",
                                    "JPG" = "JPG")),
              downloadButton(outputId = 'btnStartDownloadDisagPlot_explore', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "summtableModal", title = "Download summary data", trigger = "btnDownloadSummaryData_explore", 
              tags$p("The summary measures in the table will be downloaded as a text file with the values
                                  separated by a comma or a tab.  Select your preferred field separator and then download
                                  the data.  These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype2", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadSummaryData_explore', label = "Start", class = NULL),
              size = "medium"),
      
      bsModal(id = "summplotModal", title = "Download summary plot (PDF)", trigger = "btnDownloadSummaryPlot_explore", 
              tags$p("Set the dimensions for the plot here and download it."),
              br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              textInput("summaryPlotWitdth_explore", "Plot width (cm)", value="15" ),
              textInput("summaryPlotHeight_explore", "Plot width (cm)", value="12" ),
              selectInput(inputId="summaryPlotType_explore", label='Output format:',
                          choices=c("PDF" = "PDF",
                                    "PNG" = "PNG",
                                    "JPG" = "JPG")),
              downloadButton(outputId = 'btnStartDownloadSummaryPlot_explore', label = "Start", class = NULL),
              size = "medium"),

      
      
      tabsetPanel(id="assessment_panel",
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(tables)<h6>"), value='datatable' , 
                           uiOutput('btnDownloadDisagData_explore'),
                           dataTableOutput(outputId="dataTable")
                  ), 
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), value='dataplot' ,
                           uiOutput('btnDownloadDisagPlot_explore'),
                           plotOutput('theDataPlot_web')
                           ), 
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(tables)<h6>"), value='sumtable' , 
                           uiOutput('btnDownloadSummaryData_explore'),
                           dataTableOutput(outputId="dataTableInequal")
                  ),              
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), value='sumplot' ,
                           uiOutput('btnDownloadSummaryPlot_explore'),
                           plotOutput('theSumPlot_web')
                  )
      )#end tabsetPanel
              
    )#end mainPanel
  )
  
  
  
  
})