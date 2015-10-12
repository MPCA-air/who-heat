output$home_ui <- renderUI({


    sidebarLayout(
      sidebarPanel(
         
        
        helpText("This application uses the"),
        h2("Health Equity Monitor Database")

        
      ),
      
      mainPanel(includeHTML("www/landing_page.html")

      )
    )
  



})