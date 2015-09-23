output$data_management_ui <- renderUI({

  

    #includeCSS(file.path(r_path,"base/www/style.css")),
    #includeScript(file %>% .path(r_path,"base/www/js/returnTextAreaBinding.js")),
    sidebarLayout(
      sidebarPanel(
        
        selectInput("dataSource", label = h5("Select data sources"),
                    c("-" = "-",
                      "Upload your own data" = "OWN", 
                      "Health Equity Monitor Database" = "HETK"
                      #"Use GHO data" = "GHO"  REMOVE THIS OPTION UNTIL hetk IS SYNCHRONISED WITH goh
                    ),
                    selected = "HETK"),
        
        helpText(HTML("Once you have selected your data source, select the ‘Explore Inequality' tab to fetch your data"),
                 style="color:black; font-size:100%"),
        hr(),
        
        conditionalPanel(condition = "input.dataSource == 'OWN'",                 
                         # Manage the uploading of an individual data set                         
                         conditionalPanel( condition = "input.dataSource == 'OWN'",
                                           
                                           helpText(HTML("You cannot load a file larger than 100K.  The file must be in CSV format
                                                         and conform to the sample template found <a href=\"file:///uploadTemplate.csv\" target=\"_blank\">here</a>"),
                                                    style="color:black; font-size: 85%"),
                                           
                                           hr(),
                                           
                                           radioButtons(inputId = "sep_type", 
                                                        label = "Data separator",
                                                        choices = c("Comma" = ",",
                                                                    "Semi-colon" = ";",
                                                                    "Tab" = "\t"),
                                                        selected = '\t',
                                                        inline=T),
                                           
                                           fileInput('ownData', h5('Select local data file:'),
                                                     accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv') ),
                                           
                                           hr()
                         )
        )
        
      ),# end sidebarpanel
      
      
      mainPanel(includeHTML("www/landing_page.html")

      )
    )
  


                 

                 

# 
# 
# 
# 

# 
# tabPanel(tags$h5("Select Database"), value='data_management',
#          conditionalPanel(condition = "input.dataSource != 'OWN'",
#                           includeHTML("./www/landing_page.html")),
#          conditionalPanel(condition = "input.dataSource == 'OWN'",
#                           dataTableOutput(outputId="uploadTable")))

})