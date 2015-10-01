#******************************************************************************
# Downloading explore
#******************************************************************************

# ----- Explore

#Handler for downloading the data selected in the modal download table
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





# Create a download button contingent on the existence of a plot of the disaggregated data
output$btnDownloadDisagPlot_explore <- renderUI({
  #print("In downloadDataplot")
  
  thePlot <- theDataPlot()
  if(is.null(thePlot)){
    return()
  } else {
    list(br(),
         actionButton("btnDownloadDisagPlot_explore", "Download Plot", class = "btn-primary"))
  }  
})


output$theDataPlot_web <- renderPlot({
  #print("In theDataPlot_web")
  
  print(theDataPlot())  # Remember that print(theDataPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height1), width=exprToFunction(input$plot_width1))


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



output$btnDownloadSummaryPlot_explore <- renderUI({
#   thePlot <- theDataPlot()
#   print(thePlot)
#   if(is.null(thePlot)){
#     return()
#   } else {
  list(br(),
       actionButton("btnDownloadSummaryPlot_explore", "Download plot", class = "btn-primary"))
  #}
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























#XXXXXX



# Handler for downloading the data selected in the modal download table
output$downloadSummaryData_explore <- downloadHandler(
  filename = function() {
    paste(input$focus_country_explore, "_summary_", Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    sep <- switch(input$filetype2, "csv" = ",", "tsv" = "\t")
    
    # Write to a file specified by the 'file' argument
    dat <- datasetInequal()
    
    
    write.table(dat, file, sep = sep,
                row.names = FALSE)
  }
)

output$downloadSummplot <- renderUI({
  thePlot <- theSummaryPlot()
  if(is.null(thePlot)){
    return()
  } else {
    list(br(),
         actionButton("downloadSummplot", "Download Plot", class = "btn-primary"))
  }  
})

output$theSumPlot_web <- renderPlot({
  print(theSummaryPlot())  # Remember that print(theSummaryPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height_sum), width=exprToFunction(input$plot_width_sum))

# ----- Compare




#downloadDisagPlot_explore

#******************************************************************************
# Downloading plots
#******************************************************************************








# ----- Explore

# Handler for downloading the data selected in the modal download table


# Handler for downloading the data selected in the modal download table
output$downloadCompplot1 <- downloadHandler(
  filename = function() { 
    paste(input$focus_country_explore, '_summary_', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    pdf(file, width=(as.numeric(input$plot2_width)/2.54), height=(as.numeric(input$plot2_height)/2.45), paper=input$papersize2)
    print(theSummaryPlot()) 
    dev.off()
  }
)   




# ----- Compare


output$theComparisonPlot2_web <- renderPlot({
  
  if(is.null(theComparisonPlot2())){
    print("here")
    return()
  }
  print(theComparisonPlot2())  # Remember that print(theDataPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height3), width=exprToFunction(input$plot_width3))




output$theComparisonPlot1_web <- renderPlot({
  if(is.null(theComparisonPlot1())){
    return()
  }
  print(theComparisonPlot1())  # Remember that print(theDataPlot) just prints the code
}, res=90, height=exprToFunction(input$plot_height2), width=exprToFunction(input$plot_width2))


# Handler for downloading the data selected in the modal download plot
output$downloadCompPlot1 <- downloadHandler(
  filename = function() { 
    paste(input$focus_country, '_comp_', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    pdf(file, width=(as.numeric(input$plot1_width)/2.54), height=(as.numeric(input$plot1_height)/2.45), paper=input$papersize1)
    print(theComparisonPlot1()) 
    dev.off()
  }
) 


# Handler for downloading the data selected in the modal download plot
output$downloadCompPlot2 <- downloadHandler(
  filename = function() { 
    paste(input$focus_country, '_comp_', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    pdf(file, width=(as.numeric(input$plot1_width)/2.54), height=(as.numeric(input$plot1_height)/2.45), paper=input$papersize1)
    print(theComparisonPlot2()) 
    dev.off()
  }
)   



 

