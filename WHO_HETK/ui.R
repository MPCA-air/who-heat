
shinyUI(navbarPage("Health Equity Assessment Toolkit", id= "who_heat", inverse=TRUE, collapsible = TRUE,
                   #   fluidPage(  theme = shinytheme("readable"), 
                   #               img(src='WHO-EN-B-H.jpg', align = "right", height = 72, width = 232),
                   
                   
                   tabPanel("Home", uiOutput("home_ui")),
                   tabPanel("Explore Inequality", uiOutput('explore_inequality_ui')),
                   tabPanel("Compare Inequality", uiOutput('compare_inequality_ui')),
                   tabPanel("Information", uiOutput("information_ui"))
                   
                   
) # end navbarpage
)  # End shinyUi

