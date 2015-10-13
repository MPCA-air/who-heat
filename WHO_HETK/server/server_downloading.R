#******************************************************************************
# Downloading explore
#******************************************************************************


# ----------------------------------------
# --- Explore disaggregated data
# ----------------------------------------

output$btnDownloadDisagData_explore <- renderUI({
  
  list(br(),
       actionButton("btnDownloadDisagData_explore", "Download table", class = "btn-primary"))
  
})



output$btnStartDownloadDisagData_explore <- downloadHandler(
  filename = function() {
    paste(input$focus_country_explore, "_disaggregated_", Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    dat <- datasetInput()
    sep <- switch(input$filetype1, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    write.table(dat, file, sep = sep,
                row.names = FALSE)
  }
)


# the dataTable is left in server

# ----------------------------------------
# --- Explore disaggregated plot
# ----------------------------------------


output$btnDownloadDisagPlot_explore <- renderUI({
  #print("In downloadDataplot")
  thePlot <-"xx"
  #thePlot <- theDataPlot()
  if(is.null(thePlot)){
    return()
  } else {
    list(br(),
         actionButton("btnDownloadDisagPlot_explore", "Download graph", class = "btn-primary"))
  }  
})






output$btnStartDownloadDisagPlot_explore  <- downloadHandler(
  filename = function() { 
    paste(input$focus_country_explore, '_disaggregated_', Sys.Date(), '.', input$disagPlotType_explore, sep='')
  },
  content = function(file) {
    ggsave(file, width=as.numeric(input$disagPlotWitdth_explore), 
           height=as.numeric(input$disagPlotHeight_explore), units="cm")
  }
)   

# ----- disaggregated plot data

output$btnDownloadDisagPlotData_explore <- renderUI({
  
  list(
    br(),
    
    actionButton("btnDownloadDisagPlotData_explore", "Download table", class = "btn-primary")
    
  )
  
})



output$btnStartDownloadDisagPlotData_explore <- downloadHandler(
  filename = function() {
    paste(input$focus_country_explore, "_disaggregated_", Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    dat <- datasetInput()
    sep <- switch(input$filetype_explore_disag, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    write.table(dat, file, sep = sep,
                row.names = FALSE)
  }
)



# ----------------------------------------
# --- Explore summary data
# ----------------------------------------


output$btnDownloadSummaryData_explore <- renderUI({
  
  list(br(),
       actionButton("btnDownloadSummaryData_explore", "Download table", class = "btn-primary"))
  
})



output$btnStartDownloadSummaryData_explore <- downloadHandler(
  filename = function() {
    paste(input$focus_country_explore, "_summary_", Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    dat <- datasetInequal()
    sep <- switch(input$filetype2, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    write.table(dat, file, sep = sep,
                row.names = FALSE)
  }
)


# dataTableInequal left in main server




# ----------------------------------------
# --- Explore summary plot
# ----------------------------------------


output$btnDownloadSummaryPlot_explore <- renderUI({
  thePlot <- "xx" #theDataPlot()
  if(is.null(thePlot)){
    return()
  } else {
    list(br(),
         actionButton("btnDownloadSummaryPlot_explore", "Download graph", class = "btn-primary"))
  }
})



output$btnStartDownloadSummaryPlot_explore  <- downloadHandler(
  filename = function() { 
    paste(input$focus_country_explore, '_disaggregated_', Sys.Date(), '.', input$summaryPlotType_explore, sep='')
  },
  content = function(file) {
    ggsave(file, width=as.numeric(input$summaryPlotWitdth_explore), 
           height=as.numeric(input$summaryPlotHeight_explore), units="cm")
  }
)   




# ----- summary plot data

output$btnDownloadSummaryPlotData_explore <- renderUI({
  
  list(
    br(),
    
    actionButton("btnDownloadSummaryPlotData_explore", "Download table", class = "btn-primary")
    
  )
  
})



output$btnStartDownloadSummaryPlotData_explore <- downloadHandler(
  filename = function() {
    paste(input$focus_country_explore, "_summary_", Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    dat <- datasetInequal()
    sep <- switch(input$filetype_explore_summary, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    write.table(dat, file, sep = sep,
                row.names = FALSE)
  }
)




#******************************************************************************
# Downloading compare
#******************************************************************************

# ----------------------------------------
# --- Compare summary data
# ----------------------------------------



output$btnDownloadDisagData_compare <- renderUI({
  
  list(br(),
       actionButton("btnDownloadDisagData_compare", "Download table", class = "btn-primary"))
  
})



output$btnStartDownloadDisagData_compare <- downloadHandler(
  filename = function() {
    paste(input$focus_country_compare, "_benchmark_disaggregated_", Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    dat <- getBenchmarkData()
    sep <- switch(input$filetype_benchmark, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    write.table(dat, file, sep = sep,
                row.names = FALSE)
  }
)


#getBenchmarkData 






# ----------------------------------------
# --- Compare disaggregated plot
# ----------------------------------------



output$btnDownloadDisagPlotData_compare <- renderUI({
  
  list(
    br(),
    
    actionButton("btnDownloadDisagPlotData_compare", "Download table", class = "btn-primary")
    
  )
  
})


output$btnDownloadDisagPlot_compare <- renderUI({
  
  list(br(),
       
       actionButton("btnDownloadDisagPlot_compare", "Download graph", class = "btn-primary")
  )
  
  
  # }  btnDownloadDisagPlotData_compare
})





# Handler for downloading the data selected in the modal Download graph
# output$btnStartDownloadDisagPlot_compare <- downloadHandler(
#   filename = function() { 
#     paste(input$focus_country_compare, '_benchmark_disaggregated_', Sys.Date(), '.pdf', sep='')
#   },
#   content = function(file) {
#     pdf(file, width=(as.numeric(input$plot2_width)/2.54), height=(as.numeric(input$plot2_height)/2.45), paper=input$papersize2)
#     print(theComparisonPlot1()) 
#     dev.off()
#   }
# ) 



output$btnStartDownloadDisagPlot_compare  <- downloadHandler(
  filename = function() { 
    paste(input$focus_country_compare, '_benchmark_summary_', Sys.Date(), '.', input$disagPlotType_compare, sep='')
  },
  content = function(file) {
    ggsave(file, width=as.numeric(input$disagPlotWitdth_compare), 
           height=as.numeric(input$disagPlotHeight_compare), units="cm")
  }
)   





# output$btnDownloadDisagPlotData_compare <- renderUI({
#   #   thePlot <- theSummaryPlot()
#   #   if(is.null(thePlot)){
#   #     return()
#   #   } else {
#        actionButton("btnDownloadDisagPlotData_compare", "Download table", class = "btn-primary")
# 
#   # }  btnDownloadDisagPlotData_compare
# })



output$btnStartDownloadDisagPlotData_compare <- downloadHandler(
  filename = function() {
    paste(input$focus_country_compare, "_benchmark_disaggregated_", Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    dat <- getBenchmarkData()
    sep <- switch(input$filetype_benchmark_disag, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    write.table(dat, file, sep = sep,
                row.names = FALSE)
  }
)







# ----------------------------------------
# --- Compare summary plot
# ----------------------------------------


output$btnDownloadSummaryPlotData_compare <- renderUI({
  #   thePlot <- theSummaryPlot()
  #   if(is.null(thePlot)){
  #     return()
  #   } else {
  list(br(),
       actionButton("btnDownloadSummaryPlotData_compare", "Download table", class = "btn-primary")
  )
  #}  
})




output$btnDownloadSummaryPlot_compare <- renderUI({
  #   thePlot <- theSummaryPlot()
  #   if(is.null(thePlot)){
  #     return()
  #   } else {
  list(br(),
       actionButton("btnDownloadSummaryPlot_compare", "Download graph", class = "btn-primary")
  )
  #}  
})




# output$btnStartDownloadSummaryPlot_compare <- downloadHandler(
#   filename = function() { 
#     paste(input$focus_country_compare, '_benchmark_summary_', Sys.Date(), '.pdf', sep='')
#   },
#   content = function(file) {
#     pdf(file, width=(as.numeric(input$plot3_width)/2.54), height=(as.numeric(input$plot3_height)/2.45), paper=input$papersize2)
#     print(theComparisonPlot2()) 
#     dev.off()
#   }
# ) 


output$btnStartDownloadSummaryPlot_compare  <- downloadHandler(
  filename = function() { 
    paste(input$focus_country_compare, '_benchmark_summary_', Sys.Date(), '.', input$summaryPlotType_compare, sep='')
  },
  content = function(file) {
    ggsave(file, width=as.numeric(input$summaryPlotWitdth_compare), 
           height=as.numeric(input$summaryPlotHeight_compare), units="cm")
  }
)   






output$btnStartDownloadSummaryPlotData_compare <- downloadHandler(
  filename = function() {
    paste(input$focus_country_compare, "_benchmark_summary_", Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    dat <- getBenchmarkDataSum()
    sep <- switch(input$filetype_benchmark_summary, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    write.table(dat, file, sep = sep,
                row.names = FALSE)
  }
)







