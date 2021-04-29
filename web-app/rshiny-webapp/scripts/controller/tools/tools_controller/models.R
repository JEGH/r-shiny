output$WIP_fault <- renderPrint({
  if (USER$Logged == TRUE) {
    # Use a reactiveFileReader to read the file on change, and load the content into a new environment
    env <-
      reactiveFileReader(1000, session, "local/rdata/result1.RData", LoadToEnvironment)
    # Access the first item in the new environment, assuming that the Rdata contains only 1 item which is a data frame
    print(as.list(env())$result[['modelStatsT']])
  } else {
    login
  }
})

output$plot_SevEstim <- renderPlotly({
  if (USER$Logged == TRUE) {
    print("entrou WIP_severity x")
    # Use a reactiveFileReader to read the file on change, and load the content into a new environment
    env <-
      reactiveFileReader(1000, session, "local/rdata/result2.RData", LoadToEnvironment)
    # Access the first item in the new environment, assuming that the Rdata contains only 1 item which is a data frame
    print(as.list(env()))
    
    
    Plot1 <- plot_ly(
      x = 1:nrow(as.list(env())$result[["knn.validationResults.sort"]])
      ,
      y = as.list(env())$result[["knn.validationResults.sort"]]$predict
      ,
      type = "scatter"
      ,
      mode = input$plotType2
      ,
      name = "predicted"
      ,
      text = as.list(env())$result[["knn.validationResults.sort"]]$label
    ) %>% add_markers(
      x = 1:nrow(as.list(env())$result[["knn.validationResults.sort"]])
      ,
      y = as.list(env())$result[["knn.validationResults.sort"]]$truth
      ,
      name = "truth"
    )
    
    Plot1
    
  } else {
    login
  }
})