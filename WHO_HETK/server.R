library(shiny)
library(shinyBS)  # devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3")
library(ggplot2)
library(grid)
library(plyr)
library(countrycode)
library(RColorBrewer)
library(RSQLite)
library(digest)
library(stringr) # this was in the userful.R file, not sure how important

#library(shiny)
library(shinythemes)
#library(shinyBS)  # devtools::install_github("ebailey78/shinyBS")
#library(ggplot2)
#library(digest)




shinyServer(function(input, output, session){  
  #if(exists(".connection")) rm(.connection)
  drv <- dbDriver("SQLite")
  .connection<<-dbConnect(drv, "data/HEMTK.db")

  source("helper.R", local=TRUE)
  source("utils/get_filtered.R", local=TRUE)
  source("utils/health_indicator_list.R", local=TRUE)
  source("utils/hetkdb.R", local=TRUE)
  source("utils/plotter.R", local=TRUE)
  source("utils/inequal.R", local=TRUE)
  source("utils/comparisonCountries.R", local=TRUE)

  source("ui/data_management.R", local=TRUE)
  source("ui/explore_inequality.R", local=TRUE)
  source("ui/compare_inequality.R", local=TRUE)
  source("server/tmp_all_server.R", local=TRUE)
  #source("server/tmp_all_server.R", local = TRUE)
  serverfiles <- list.files("server/", pattern="\\.(r|R)$", full.names = TRUE)
  uifiles     <- list.files("ui/", pattern="\\.(r|R)$", full.names = TRUE)
  inequalfiles     <- list.files("utils/inequal_functions/", pattern="\\.(r|R)$", full.names = TRUE)
  utils     <- list.files("utils/", pattern="\\.(r|R)$", full.names = TRUE)

  
#  for (file in c(serverfiles, utils)) {
#    source(file, local = TRUE)
#     
#  }
  
})
