library(shiny)
library(shinyBS)  # devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3")
library(ggplot2)
library(grid)
library(plyr)
library(countrycode)
library(RColorBrewer)
library(RSQLite)
library(digest)





shinyServer(function(input, output, session){  
  #if(!dbIsValid(.connection)) dbDisconnect(.connection)
  drv <- dbDriver("SQLite")
  .connection<<-dbConnect(drv, "data/HEMTK.db")

  source("ui/data_management.R", local=TRUE)
  source("ui/explore_inequality.R", local=TRUE)
  source("utils/get_filtered.R", local=TRUE)
  source("utils/health_indicator_list.R", local=TRUE)
  source("server/tmp_all_server.R", local=TRUE)
  #source("server/tmp_all_server.R", local = TRUE)
  serverfiles <- list.files("server/", pattern="\\.(r|R)$", full.names = TRUE)
  uifiles     <- list.files("ui/", pattern="\\.(r|R)$", full.names = TRUE)
  utils     <- list.files("utils/", pattern="\\.(r|R)$", full.names = TRUE)

  
#  for (file in c(serverfiles, utils)) {
#    source(file, local = TRUE)
#     
#  }
  
})
