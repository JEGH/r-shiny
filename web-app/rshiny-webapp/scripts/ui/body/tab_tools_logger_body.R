

output$tab_tools_logger_body <- renderUI({
  #__________________________________________________________________________________________________________________________________
  #__Loggger tabItem ----
  #___________________________________________________________________________________________________________________________________
  fluidRow(
    navbarPage(
      "Logger",
      tabPanel(#kafka-mongodb
        "MongoDB",
        
        mainPanel(
          tabsetPanel(
            tabPanel("Collections" 
                     
                     
            ),tabPanel("View", 
                       verbatimTextOutput("mongodb_test"),
                       verbatimTextOutput("kafkamongodb_test")
            ),tabPanel("Options")
          )
        )
      ),tabPanel(
        "Kafka",
        mainPanel(
          tabsetPanel(
            tabPanel("Topics")
            ,tabPanel("Consumers")
            ,tabPanel("Producers")
            ,tabPanel("Options")
          )
        )
        
      ),
      tabPanel(
        "Spark",
        mainPanel(
          tabsetPanel(
            tabPanel("Datasets", 
                     tableOutput("table-spark")
            ),tabPanel("Info", 
                       verbatimTextOutput("info_collections")),
            tabPanel("Options")
          )
        )
        
      ),
      tabPanel(
        "Postgres",
        mainPanel(
          tabsetPanel(
            tabPanel("Show Raw Data",
                     
                     
                     verbatimTextOutput("postgres_raw_data"),
                     #actionButton("hide_raw_data","Hide data"),
                     actionButton("show_raw_data1","Show Raw Data 1"),
                     actionButton("show_raw_data2","Show Raw Data 2"),
                     actionButton("show_raw_data3","Show Raw Data 3")
            )
            
          )
        )
        
      )
    )
    
  )
})