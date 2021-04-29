output$tab_analysis_body <- renderUI({
  fluidRow(
    fluidRow(
      align="center",
      h1("Analysis",icon("dashboard",lib = "font-awesome"))
      
    ),
    br(),
    tags$hr(style="border-color: black;"),
    fluidRow(
      align="center",
      conditionalPanel(
        inputId = "panel-selection",
        condition = 'output.ShowPanel_Flag == "1"',
        
        fluidRow(
          align="center",
          column(
            width = 12,
            h4("Motor Selection"),
            selectInput(
              inputId = "motorSelect",
              label = "Motor:",
              choices = c("Motor SCIM ABB",
                          "Motor SCIM Optisigma",
                          "Motor SCIM Navigator"),
              selected = "",
              multiple = FALSE
            )
          )
        ),
        fluidRow(
          align="center",
          column(
            width = 12,
            h4("Date Selection") ,
            dateRangeInput('dateselection',
                           label = 'Date range:',
                           start = Sys.Date() - 2, end = Sys.Date() + 2
            )
            
          )  
        ),
        fluidRow(
          align="center",
          column(
            width = 12,
            actionButton("start", "START", width="100px"),
            tags$head(
              tags$style(HTML('#start{background-color:orange}'))
            )
          )
        )
      )
      
    ),
    fluidRow(
      h6("#=Not Implement yet"),
      h6("*=Demo")
    ),
    conditionalPanel(
      inputId = "panel-show-btn-back",
      condition = 'output.ShowPanel_Flag == "2"',
      actionButton("back", "<- BACK"),
      tags$head(
        tags$style(HTML('#back{background-color:orange}'))
      )
    ),
    tags$hr(style="border-color: black;"),
    fluidRow(
      conditionalPanel(
        inputId = "panel-analytics",
        condition = 'output.ShowPanel_Flag == "2"',
        navbarPage(
          textOutput(outputId = "name_motor_analysis"), ## Falta mudar para  label
          tabPanel(
            "Currents",
            fluidRow(
              column(10, 
                     h4("*Park Currents Analysis "),
                     plotlyOutput("p1")),
              column(2,  plotlyOutput("p2"))
            ),
            fluidRow(
              column(6, 
                     h4("#Risk Currents Analysis "),
                     plotOutput("risk_matrix_currs")
              ),
              column(6, 
                     h4("#Percent Change"),
                     #tags$h5(id="h5_num_errors","Number of Errors",style="font-weight:bold;text-align:center"),
                     plotOutput("change_percent_currs")
              )
            ),
            fluidRow(
              h6("#=Not Implement yet"),
              h6("*=Demo")
              
            )
          ),
          tabPanel(
            "Vibrations",
            h6("Pending: New Datasets "),
            h6("Pending: Thesis Hugo Racoes"),
            fluidRow(
              column(10, 
                     h4("#Frenquecy  Analysis "),
                     h5("#Pending ")
                     #,
                     #plotlyOutput("p1")
              ),
              column(2
                     #,  
                     #plotlyOutput("p2")
              )
            ),
            fluidRow(
              column(6, 
                     h4("#Risk Vibrations Analysis ")
                     ,
                     plotOutput("risk_matrix_vibra")
              ),
              column(6, 
                     h4("#Percent Change"),
                     plotOutput("change_percent_vibra")
              )
            ),
            fluidRow(
              h6("#=Not Implement yet"),
              h6("*=Demo")
            )
            
          )
        )
        
      )
      
    )
  )
  
})
