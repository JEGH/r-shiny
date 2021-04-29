

output$tab_tools_models_body <- renderUI({
  #__________________________________________________________________________________________________________________________
  #__Models tabItem ----
  #___________________________________________________________________________________________________________________________________
  fluidRow(
    navbarPage(
      "Models",
      tabPanel(
        "Fault Diagnosis",
        fluidRow(
          h3("Ouput: Stats Test Data")
        ),
        fluidRow( 
          mainPanel(verbatimTextOutput("WIP_fault"))
        )
        
      ),
      tabPanel(
        "Severity Estimation",
        fluidRow(
          h3("Ouput: Plot Cubist Performance for the validation and test")
        ),
        
        fluidRow(  
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "plotType2",
                "Mode",
                c("Markers" = 'markers'
                  , "Lines" = "lines")
                
              )
            ),
            mainPanel(plotlyOutput("plot_SevEstim")
            )
          )
        )
        
      )
    )
  )
})