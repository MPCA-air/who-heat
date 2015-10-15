output$explore_inequality_ui <- renderUI({
  
  
  #includeScript(file %>% .path(r_path,"base/www/js/returnTextAreaBinding.js")),
  sidebarLayout(
    sidebarPanel(
      tags$div(class="sectionhead1", "Variable options"),
      uiOutput("focus_country_explore"), 
      uiOutput('focus_source_year_explore'),
      uiOutput("focus_indicator_explore"),
      uiOutput("focus_dimension_explore"),
      
      
      conditionalPanel(condition = "input.assessment_panel == 'datatable'",   #### output.owndata gopt from server.R

  
                        tags$div(class="sectionhead", "Table options"),
                        uiOutput('dataTableItems_explore')

      ),
      conditionalPanel(condition = "input.assessment_panel == 'dataplot'",

                       tags$div(class="sectionhead", "Graph options"),
                       uiOutput("disag_plot_type"),

                       tags$span(class="control-label", "Select graph names"),
                
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
                       tags$div(class="sectionhead", "Summary measure options"),
                       uiOutput("summary_measures")
                      
              
      ),
      conditionalPanel(condition = "input.assessment_panel == 'sumplot'",
                       
                       tags$div(class="sectionhead", "Graph options"),
                       uiOutput("summary_plot_type"),
 
                       tags$span(class="control-label", "Select graph names"),
                       
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
      
      bsModal(id = "dataplotModal", title = "Download disaggregated graph (PDF)", trigger = "btnDownloadDisagPlot_explore", 
              tags$p("Set the dimensions for the plot here and download it."),
              br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              textInput("disagPlotWitdth_explore", "Graph width (cm)", value="15" ),
              textInput("disagPlotHeight_explore", "Graph width (cm)", value="12" ),
              selectInput(inputId="disagPlotType_explore", label='Output format:',
                          choices=c("PDF" = "PDF",
                                    "PNG" = "PNG",
                                    "JPG" = "JPG")),
              downloadButton(outputId = 'btnStartDownloadDisagPlot_explore', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "compdataDisagModal", title = "Download disaggregated data", trigger = "btnDownloadDisagPlotData_explore", 
              tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype_explore_disag", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadDisagPlotData_explore', label = "Start"),
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
      
      bsModal(id = "summplotModal", title = "Download summary graph (PDF)", trigger = "btnDownloadSummaryPlot_explore", 
              tags$p("Set the dimensions for the plot here and download it."),
              br(),
              tags$p("Titles and axis labels are displayed according to your selections."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              textInput("summaryPlotWitdth_explore", "Graph width (cm)", value="15" ),
              textInput("summaryPlotHeight_explore", "Graph width (cm)", value="12" ),
              selectInput(inputId="summaryPlotType_explore", label='Output format:',
                          choices=c("PDF" = "PDF",
                                    "PNG" = "PNG",
                                    "JPG" = "JPG")),
              downloadButton(outputId = 'btnStartDownloadSummaryPlot_explore', label = "Start", class = NULL),
              size = "medium"),
      bsModal(id = "compdataSummaryModal", title = "Download summary data", trigger = "btnDownloadSummaryPlotData_explore", 
              tags$p("The data in the table will be downloaded as a text file with the values separated
                     by a comma or a tab.  Select your preferred field separator and then download the data.
                     These can be opened in a text editor, or spreadsheet package."),
              br(),
              tags$p("Close the window once the download has commenced."),
              br(),
              radioButtons(inputId="filetype_explore_summary", label='Field separator:',
                           choices=c("Comma separated valued" = "csv",
                                     "Tab separated values" = "tsv")),
              downloadButton(outputId = 'btnStartDownloadSummaryPlotData_explore', label = "Start"),
              size = "medium"),

      
      
      tabsetPanel(id="assessment_panel",
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(tables)<h6>"), value='datatable' , 
                           uiOutput('btnDownloadDisagData_explore'),
                           dataTableOutput(outputId="dataTable")
                  ), 
                  tabPanel(HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), value='dataplot' ,
                           uiOutput('btnDownloadDisagPlotData_explore'),
                           uiOutput('btnDownloadDisagPlot_explore'),
                           plotOutput('theDataPlot_web')
                           ), 
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(tables)<h6>"), value='sumtable' , 
                           uiOutput('btnDownloadSummaryData_explore'),
                           dataTableOutput(outputId="dataTableInequal")
                  ),              
                  tabPanel(HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), value='sumplot' ,
                           uiOutput('btnDownloadSummaryPlotData_explore'),
                           uiOutput('btnDownloadSummaryPlot_explore'),
                           plotOutput('theSumPlot_web')
                  )
      )#end tabsetPanel
              
    )#end mainPanel
  )
  
  
  
  
})