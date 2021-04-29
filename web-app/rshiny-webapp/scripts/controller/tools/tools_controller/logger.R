###---- Render Print mongodb -----


output$mongodb_test <- renderPrint({

    print("Output:")
    data <- loadData_MongoDB("test1")
    print(data)
 
  
})

###---- Render Print kafka mongodb -----
output$kafkamongodb_test <- renderPrint({
 
    print("Output:")
    data <- loadData_MongoDB("kafka-mongodb")
    print(data)

  
})

output$info_collections <- renderPrint({
    listDataNames <- getname_datasetsxlsx_list()
    print("listDataNames:")
    print(listDataNames)
    sc <- create_spark_connection()
    #listDataSpark <- copy_Dataxlsx_Spark(listDataNames,sc)
    print("listDataSpark")
    print(listDataSpark)
  
  
})

observeEvent(input$show_raw_data1,{
  output$postgres_raw_data <- renderPrint({
    
    res<-Query_REARM_DB(paste0('select * from "Data"."Raw" limit 10000'))
    PrintByParts(res,80)
    
    
    
  })   
})

observeEvent(input$show_raw_data2,{
  output$postgres_raw_data <- renderPrint({
    
    res<-Query_REARM_DB(paste0('select * from "Data"."Raw2" limit 10000'))
    PrintByParts(res,80)
    
    
    
  })   
})
observeEvent(input$show_raw_data3,{
  output$postgres_raw_data <- renderPrint({
    
    res<-Query_REARM_DB(paste0('select * from "Data"."Raw3" limit 10000'))
    PrintByParts(res,80)
    
    
    
  })   
})
