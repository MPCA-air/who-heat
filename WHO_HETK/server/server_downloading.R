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
  
  thePlot <- theDataPlot()
  if(is.null(thePlot)){
    return()
  } else {
    list(br(),
         actionButton("btnDownloadDisagPlot_explore", "Download graph", class = "btn-primary"))
  }  
})







output$btnStartDownloadDisagPlot_explore  <- downloadHandler(
  filename = function() { 
    paste(input$focus_country_explore, '_disaggregated_', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    pdf(file, width=(as.numeric(input$plot1_width)/2.54), height=(as.numeric(input$plot1_height)/2.45), paper=input$papersize1)
    print(theDataPlot()) 
    dev.off()
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
  thePlot <- theDataPlot()
  if(is.null(thePlot)){
    return()
  } else {
  list(br(),
       actionButton("btnDownloadSummaryPlot_explore", "Download graph", class = "btn-primary"))
  }
})



output$btnStartDownloadSummaryPlot_explore  <- downloadHandler(
  filename = function() { 
    paste(input$focus_country_explore, '_summary_', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    pdf(file, width=(as.numeric(input$plot_width_sum)/2.54), height=(as.numeric(input$plot_height_sum)/2.45), paper=input$papersize2)
    p <- theSummaryPlot()
    print(p) 
    #plot(1:10, 1:10)
    dev.off()
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
    paste(input$focus_country_compare, "_and_benchmark_countries_", Sys.Date(), '.csv', sep='')
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



output$btnDownloadDisagPlot_compare <- renderUI({
#   thePlot <- theSummaryPlot()
#   if(is.null(thePlot)){
#     return()
#   } else {
    list(br(),
         actionButton("btnDownloadDisagPlot_compare", "Download graph", class = "btn-primary"))
 # }  
})


output$theComparisonPlot1_web <- renderPlot({
  if(is.null(theComparisonPlot1())){
    return()
  }
  print(theComparisonPlot1())  # Remember that print(theDataPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height2), width=exprToFunction(input$plot_width2))


# Handler for downloading the data selected in the modal Download graph
output$btnStartDownloadDisagPlot_compare <- downloadHandler(
  filename = function() { 
    paste(input$focus_country_compare, '_disaggregated_compare_', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    pdf(file, width=(as.numeric(input$plot2_width)/2.54), height=(as.numeric(input$plot2_height)/2.45), paper=input$papersize2)
    print(theComparisonPlot1()) 
    dev.off()
  }
) 

# ----------------------------------------
# --- Compare summary plot
# ----------------------------------------


output$btnDownloadSummaryPlot_compare <- renderUI({
#   thePlot <- theSummaryPlot()
#   if(is.null(thePlot)){
#     return()
#   } else {
    list(br(),
         actionButton("btnDownloadSummaryPlot_compare", "Download graph", class = "btn-primary"))
  #}  
})




output$btnStartDownloadSummaryPlot_compare <- downloadHandler(
  filename = function() { 
    paste(input$focus_country_compare, '_summary_compare_', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    pdf(file, width=(as.numeric(input$plot3_width)/2.54), height=(as.numeric(input$plot3_height)/2.45), paper=input$papersize2)
    print(theComparisonPlot2()) 
    dev.off()
  }
) 



output$theComparisonPlot2_web <- renderPlot({    
  if(is.null(theComparisonPlot2())){
    return(NULL)
  }
  print(theComparisonPlot2())  # Remember that print(theDataPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height3), width=exprToFunction(input$plot_width3))





 

