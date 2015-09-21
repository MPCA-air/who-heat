library(shiny)
library(shinyBS)  # devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3")
library(ggplot2)
library(grid)
library(plyr)
library(countrycode)
library(RColorBrewer)
library(RSQLite)
library(digest)


source('helper.R')


shinyServer(function(input, output, session){  
  
  #source("server/tmp_all_server.R", local = TRUE)
  serverfiles <- list.files("server/", pattern="\\.(r|R)$", full.names = TRUE)
  uifiles     <- list.files("ui/", pattern="\\.(r|R)$", full.names = TRUE)
  utils     <- list.files("utils/", pattern="\\.(r|R)$", full.names = TRUE)

  
  for (file in c(serverfiles, uifiles, utils)) {
    source(file, local = TRUE)
    
  }
  
})
