output$home_ui <- renderUI({


    sidebarLayout(
      sidebarPanel(
         
        
        helpText("This application uses the"),
        tags$div(class="sectionhead1", "Health Equity Monitor Database")


        
      ),
      
      mainPanel(includeHTML("www/landing_page.html")

      )
    )
  



})