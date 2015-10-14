output$information_ui <- renderUI({
  
  
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("This application uses the"),
      tags$div(class="sectionhead1", "Health Equity Monitor Database")
      
    ),# end sidebarpanel
    
    
    
    
    mainPanel(
      tabsetPanel(id = "info_panels",
                  tabPanel(h6("Glossary"), value='gloss_panel', includeHTML("www/glossary.html")),
                  tabPanel(h6("About"), value='about_panel', 
                           
                           bsCollapse(id = "aboutPanel", open = "scatterPlotPanel", multiple = FALSE,
                                      bsCollapsePanel("Software",  includeHTML("www/software.html")),
                                      bsCollapsePanel("License", includeHTML("www/license.html")),
                                      bsCollapsePanel("Acknowledgements", includeHTML("www/acknowledgement.html"))
                           )
                           
                           
                  )
                  #tabPanel(h6("Administration"), value='admin_panel',
                  #         conditionalPanel(condition='input.admin_show==true', source('ui/administration-panel.R')$value),
                  #         conditionalPanel(condition='input.admin_show==false', includeHTML("./www/adminFail.html"))
                  #)
      )
      
      
    )# end mainPanel
  )# end sidebarLayout
  
  
  
  
})