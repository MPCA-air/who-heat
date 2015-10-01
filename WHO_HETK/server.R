library(shiny)
library(shinyBS)  # devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3")
library(ggplot2)
library(grid)
#library(plyr)
#library(countrycode)
library(RColorBrewer)
library(dplyr)
#library(RSQLite)
#library(digest)
#library(stringr) # this was in the userful.R file, not sure how important
library(shinythemes)


options(shiny.maxRequestSize = 0.100*1024^2)


shinyServer(function(input, output, session){  
  source("utils/initial_settings.R", local=TRUE)

  source("utils/get_filtered.R", local=TRUE)

  source("utils/get_plots.R", local=TRUE)
  source("utils/get_data.R", local=TRUE)


  source("ui/data_management.R", local=TRUE)
  source("ui/explore_inequality.R", local=TRUE)
  source("ui/compare_inequality.R", local=TRUE)
  source("ui/information.R", local=TRUE)
  source("server/tmp_all_server.R", local=TRUE)
  source("server/server_observers.R", local=TRUE)
  
#   session$onSessionEnded(function() {
#     isolate({
#       # This will get executed when a session exits
#       rm(.rdata)
#     })
#   })
  
  
})
