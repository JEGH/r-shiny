output$tab_stats_body <- renderUI({

  tabItem(tabName = "t_stats",
          fluidRow(
            align="center",
            
            h1("Statistical Estimations", icon("signal",lib = "font-awesome"))
            
          ),
          br(),
          tags$hr(style="border-color: black;"),
          fluidRow(
            column(width=3,
                   h4(textOutput(outputId = "name_motor_stats"))
            )
            # column(width=2,
            #        actionButton("change_motor", "Change Motor"),
            #        tags$head(
            #          tags$style(HTML('#change_moto{background-color:orange}'))
            #        )
            # )
          ),
          tags$hr(style="border-color: black;"),
          fluidRow( 
            ## Motor Tables
            column(width=7,
                   h3("#Key Performace Table"),
                   DT::dataTableOutput("table_stats") 
                   
            ),
            column(width=4,offset=1,
                   h3("Description"),
                   h4(textOutput("description_title")),
                   tags$hr(style="border-color: blue;"),
                   em(h5(textOutput("description_text")))
            )
            
          ),
          fluidRow( 
            ## Motor Tables
            column(width=5,
                   h3("#Alarm Criteria")
            )
          ),
          navbarPage(
            "KPI",
            #Falta inserir aqui a -> Estimation of Evolving Faults 
            tabPanel("MTTF",
                     fluidRow(
                       column(6, 
                              h4("Mean Time To Failure") 
                       )
                     ),
                     fluidRow(
                       column(6, 
                              gaugeOutput("gauge_MTTF")
                       )
                     ),
                     fluidRow(
                       column(6,
                              numericInput(
                                "value_MTTF",
                                label = "Select Alarm value",
                                min = 0,
                                max = 100,
                                value = 25,
                                step = 1
                              )
                       )
                       
                     )
                     
            )
            ,
            tabPanel(
              "MTTCF",
              fluidRow(
                h4("Mean Time To Critical Failure") 
              ),
              fluidRow(
                column(6, 
                       gaugeOutput("gauge_MTTCF")
                )
              ),
              fluidRow(
                column(6,
                       numericInput(
                         "value_MTTCF",
                         label = "Select Alarm value",
                         min = 0,
                         max = 100,
                         value = 25,
                         step = 1
                       )
                )
                
              )
            ),
            tabPanel(
              "MTBF",
              fluidRow(
                h4("Mean Time Between Failure")
              ),
              fluidRow(
                column(6, 
                       gaugeOutput("gauge_MTBF")
                )
              ),
              fluidRow(
                column(6,
                       numericInput(
                         "value_MTBF",
                         label = "Select Alarm value",
                         min = 0,
                         max = 100,
                         value = 25,
                         step = 1
                       )
                )
                
              )
            ),
            tabPanel(
              "MCTR",
              fluidRow(
                h4("Mean Cost to Repair")
              ),
              fluidRow(
                column(6, 
                       gaugeOutput("gauge_MCTR")
                )
              ),
              fluidRow(
                column(6,
                       numericInput(
                         "value_MCTR",
                         label = "Select Alarm value",
                         min = 0,
                         max = 100,
                         value = 25,
                         step = 1
                       )
                )
                
              )
            ),
            tabPanel(
              "MTTR",
              fluidRow(
                h4("Mean Time To Repair")
              ),
              fluidRow(
                column(6, 
                       gaugeOutput("gauge_MTTR")
                )
              ),
              fluidRow(
                column(6,
                       numericInput(
                         "value_MTTR",
                         label = "Select Alarm value",
                         min = 0,
                         max = 100,
                         value = 25,
                         step = 1
                       )
                )
                
              )
            )
          ),
          fluidRow(
            h6("#=Not Implement yet"),
            h6("*=Demo")
          )
          
          
          
  )

})