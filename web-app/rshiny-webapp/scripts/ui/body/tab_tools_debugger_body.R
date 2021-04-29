

output$tab_tools_debugger_body <- renderUI({
  #__________________________________________________________________________________________________________________________________
  #__Debugger tabItem ----
  #___________________________________________________________________________________________________________________________________
  fluidRow(
    navbarPage(
      "Debugger",
      tabPanel(#kafka-mongodb
        "Browser",
        
        mainPanel(
          tabsetPanel(
            tabPanel("Tests", 
                     h6("not implement yet")
                     
            ),
            tabPanel("Compare Datasets", 
                     actionButton("start_comparing","Start Comparing Datasets"),
                     verbatimTextOutput("postgres_test")
            )
          )
        )
      )
    )
  )
})