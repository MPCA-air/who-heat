
shinyUI(
  
  tagList(
    
  tags$head(tags$link(rel="stylesheet", type="text/css",href="spacelab.min.css")),
  tags$head(tags$link(rel="stylesheet", type="text/css",href="style.css")),
  
  navbarPage(title = HTML('<a class="navbar-brand" rel="home" href="#" title="World Health Organization">
                                         <img class="whoimg" src="who_logo_white40px.png"></a> <span class="navtitle">Health Equity Assessment Toolkit</span>'),
                   id= "who_heat", 
                   inverse=TRUE, 
                   collapsible = TRUE,
                   #theme = shinytheme("spacelab"),
                   #tags$head(tags$style("style.css")),
                   #   fluidPage(  theme = shinytheme("readable"), 
                   #               img(src='WHO-EN-B-H.jpg', align = "right", height = 72, width = 232),
                   #tags$a(class="navbar-brand", tags$img("who_logo_blue50px.png")),
                   tabPanel("Home", uiOutput("home_ui")),
                   tabPanel("Explore Inequality", uiOutput('explore_inequality_ui')),
                   tabPanel("Compare Inequality", uiOutput('compare_inequality_ui')),
                   tabPanel("Information", uiOutput("information_ui"))
                   
                   
) # end navbarpage
)  # End shinyUi
)


# HTML('<a class="navbar-brand" rel="home" href="#" title="World Health Organization">
#                           <img class="whoimg" src="who_logo_blue50px.png"></a> Health Equity Assessment Toolkit')
# runApp(list(
#   ui = tagList(
#     tags$head(tags$style(HTML("body{ background: red; }"))),
#     navbarPage(
#       "Title",
#       tabPanel(title = "First"),
#       tabPanel(title = "Second")
#     )
#   ),
#   server = function(input, output, session) {}
# ))

# deployWHO <- function(){
#   unlink("d:/junk/who/", recursive = TRUE, force = FALSE)
#   file.copy("X:/projects/who_heat/web/who-heat/WHO_HETK", "d:/junk/who/", recursive=TRUE)
#   #file.rename("", "d:/junk/who")
#   shinyapps::deployApp("d:/junk/who/WHO_HETK", account="spatial")
# }
# deployWHO()