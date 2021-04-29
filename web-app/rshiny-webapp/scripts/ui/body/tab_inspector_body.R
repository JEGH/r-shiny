
library(shinyTime)
output$tab_inspector_body <- renderUI({
  
  fluidRow(
    tags$style(HTML(".content-wrapper {background-color:  #00b2bc  !important;   
                                       background-image: none  !important;}")), #atenção CSS martelado
   
    fluidRow(
      align="center",
      
      h1("Inspector",
         icon("search",lib = "font-awesome"))
      
    ),
    br(),
    tags$hr(style="border-color: black;"),
    #__Inspector Header ----
    fluidRow( 
      
      conditionalPanel(
       
        inputId = "panel-motor-selection-inspect",
        condition = 'output.ShowPanel_inspector_Flag == "0"',
       
        fluidRow(
          align="center",    
          fluidRow(
            align="center",
            column(
              width = 12,
              h3("Menu Inspector")
            )
          ),
          fluidRow(
            align="center",
            column(
              width = 12,
              selectInput(
                inputId = "motorSelect_0",
                label = "Motor:",
                choices = get_motors_id_and_model(db_name="rearm-db"),
                selected = "",
                multiple = FALSE
              ),
              actionButton("ok_select", "Start", icon("play-circle")),
              tags$head(
                tags$style(HTML('#ok_select{background-color:orange}'))
              )
              
            )
          )
        )
        
      ),
      
      
      conditionalPanel(
        inputId = "panel-motor-selection-inspect",
        condition = 'output.ShowPanel_inspector_Flag != "0"',
        fluidRow(
         
          align="center",
          column(
            width = 12,
            h3("Menu Inspector")
          )
        ),
        fluidRow(
          align="center",  
          id="headerInspector",
          tags$style(HTML("#headerInspector {background-color:  white  !important;}")),
          column(width=2,
                 h4(tags$b("Target Room")),
                 h4(textOutput(outputId = "target_room"))
          ),
          column(width=1,
                 h4(tags$b("Target Type")),
                 h4(textOutput(outputId = "target_type"))
          ),
          column(width=2,
                 fluidRow(
                   column(width=10,
                          h4(tags$b("Target Status"))
                   )
                 ),
                 fluidRow(
                   column(width=1,
                          uiOutput("icon_status") 
                   ),
                   column(width=2,
                          h4(textOutput(outputId = "target_status"))
                   )
                 )
          ),
          column(width=2,
                 h4(tags$b("Target Name")),
                 h4(textOutput(outputId = "target_name"))
          ),
          
          column(width=2,
                 imageOutput("image_viewer", width = "50%", height = "50px")
                 
          ),
          column(width=3,
                 conditionalPanel(
                   inputId = "panel-motor-selection-inspect",
                   condition = 'output.ShowPanel_inspector_Flag == "1"',
                   fluidRow(
                     actionButton("go_to_settings","Settings", icon = icon("wrench"), width = "125px"),
                     tags$head(
                       tags$style(HTML('#go_to_settings {background-color:orange}'))
                     )
                   ),
                   fluidRow(
                     actionButton("change_target_i", "Change Target", width = "125px"),
                     tags$head(
                       tags$style(HTML('#change_target_i{background-color:orange}'))
                     )
                   )
                 ),
                 conditionalPanel(
                   inputId = "panel-motor-selection-inspect",
                   condition = 'output.ShowPanel_inspector_Flag == "2"',
                   column(
                     width = 6,
                     h5("Motor Selection"),
                     selectInput(
                       inputId = "motorSelect",
                       label = "Motor:",
                       choices = get_motors_id_and_model(db_name="rearm-db"),
                       selected = "",
                       multiple = FALSE
                     )
                   ),
                   column(width = 4,
                          actionButton("ok_change", "OK"),
                          tags$head(
                            tags$style(HTML('#ok_change{background-color:orange}'))
                          )
                          
                   )
                 )
          )
        )
      )
      
      
      
    ),
    
    tags$hr(style="border-color: black;"),
    ## ___Measurements ----
    
    conditionalPanel(
      inputId = "panel-motor-selection-inspect",
      condition = 'output.ShowPanel_inspector_Flag != "0"',   
      fluidRow(
        align="center",
        h3("Measurements")
        
      ),
      tags$hr(style="border-color: black;"),
      fluidRow(
        
        column(width=8,
               fluidRow(
                 align="center",
                 h4(tags$b("Data"))
               ),
               fluidRow(
                 column(width=4,
                        
                        fluidRow(
                          align="center",
                          h5(tags$b("Voltages"))
                        ),
                        br(),
                        fluidRow(
                          align="center",
                          plotOutput("voltages")
                        )
                 ),
                 column(width=4,
                        fluidRow(
                          align="center",
                          h5(tags$b("Currents"))
                        ),
                        br(),
                        fluidRow(
                          align="center",
                          plotOutput("currents") 
                        )
                 ),
                 column(width=4,
                        fluidRow(
                          align="center",
                          h5(tags$b("Accelerometric"))
                        ),
                        br(),
                        fluidRow(
                          align="center",
                          plotOutput("vibrations")
                        )
                 )
               ),
               fluidRow(
                 
                 tags$h5(id="h5_num_errors","Number of Errors",style="font-weight:bold;text-align:center"),
                 plotOutput("number_of_errors")
                 
                 
               ),
               #start performance
               
               conditionalPanel(
                 inputId = "panel-motor-performance-inspect",
                 condition = 'output.ShowPanel_performance_Flag == "1"', 
                 
                 fluidRow(
                   align="center",
                   h4("Performance")
                   
                 ),  
                 tags$hr(style="border-color: black;"),
                 
                 fluidRow(         
                   column(width=5,
                          align="center",
                          #br(),
                          #br(),
                          #br(),
                          fluidRow(
                            align="center",
                            h5(tags$b("Stator Windings Analysis"))
                          ),
                          column(width=4,
                                 align="center",
                                 fluidRow(
                                   column(width=12,
                                          align="center",
                                          h6("Condition"),
                                          tags$hr(style="border-color: gray;")
                                   )
                                 ), fluidRow(
                                   column( width=2,
                                           align="center",
                                           #imageOutput("info_stator", width = "50%", height = "50px")
                                           tags$style(".color-green {color:#00FF00}"),
                                           icon("check-circle",class="color-green")
                                           
                                   ),
                                   column(width=6,   
                                          align="center",
                                          tags$b("Good!") ##batota
                                   )
                                 )
                                 
                                 
                                 
                                 
                          ),
                          column(width=4,
                                 fluidRow(
                                   column( width=12,
                                           align="center",
                                           h6("Insight"),
                                           tags$hr(style="border-color: gray;")
                                   )
                                 ),
                                 fluidRow(
                                   column( width=12,
                                           align="center",
                                           h6(textOutput(outputId = "text_info_stator"), align="center")
                                   )
                                 )
                          ),
                          column(width=4,
                                 fluidRow(
                                   column(width=12,
                                          align="center",
                                          h6("Options"),
                                          tags$hr(style="border-color: gray;")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=12,
                                          align="center",
                                          actionButton("go_to_stator_analysis","Analysis", icon = icon("dashboard"), width='100px'),
                                          actionButton("go_to_stator_stats","Stats", icon = icon("signal"), width='100px')
                                   )
                                 )
                          )
                   ),
                   
                   
                   column(width=5,
                          #br(),
                          #br(),
                          
                          fluidRow(
                            align="center",
                            h5(tags$b("Bearings Analysis"))
                          ),    
                          
                          column(width=4,
                                 align="center",
                                 fluidRow(
                                   column(width=12,
                                          align="center",
                                          h6("Condition"),
                                          tags$hr(style="border-color: gray;")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=2,
                                          align="center",
                                          #imageOutput("info_bearing", width = "50%", height = "50px")
                                          #tags$style(".color-red {color:#FF0000}"),
                                          #icon("exclamation-triangle",class="color-red")
                                          tags$style(".color-green {color:#00FF00}"),
                                          icon("check-circle",class="color-green")
                                   ),
                                   column(width=6,
                                          align="left",
                                          #h5(tags$b("Good!")) #batota
                                          tags$h5(id="bearings_condition",style="font-weight: bold;text-align:center","Good!")
                                   )
                                   
                                   
                                 )
                                 
                                 
                                 
                                 
                          ),
                          column(width=4,
                                 fluidRow(
                                   column(width=12,
                                          align="center",
                                          h6("Insight"),
                                          tags$hr(style="border-color: gray;")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=12,
                                          align="center",
                                          h6(textOutput(outputId = "text_info_bearing"), align="center")
                                   )
                                 )
                          ),
                          column(width=4,
                                 fluidRow(
                                   column(width=12,
                                          align="center",
                                          h6("Options"),
                                          tags$hr(style="border-color: gray;")
                                   )
                                 ),
                                 fluidRow(
                                   column(width=12,
                                          align="center",
                                          actionButton("go_to_bearing_analysis","Analysis", icon = icon("dashboard"), width='100px'),
                                          actionButton("go_to_bearing_stats",   "Stats",    icon = icon("signal"),    width='100px')
                                   )
                                 )  
                          )
                          
                          
                   )
                   
                   
                 )
               )
               
               #end performance
               
               
               
        ),
        column(width=4,
               fluidRow(
                 align="center",
                 h4(tags$b("Time"))
               ),
               fluidRow(
                 align="left",
                 dateRangeInput('motor_date_selection',
                                label = 'Date range:',
                                start = Sys.Date(), end = Sys.Date() + 1
                 )
               ),
               fluidRow(
                 align="center",
                 #tags$input(id="start_hour",type="time",value="01:00"),
                 #numericInput("start_hour",value=2,label = ''),
                 #tags$label(":"),
                 #tags$input(id="end_hour",type="time",value="23:00")
                 timeInput("start_hour", "Start time", value = strptime("00:00:00", "%T")),
                 timeInput("end_hour", "End time", value = strptime("23:59:59", "%T"))
                 
               ),
               fluidRow(
                 align="left",
                 disabled(checkboxGroupInput("variable", "Last:",
                                             c("1 Hour"   = "1_hour",
                                               "3 Hours"  = "3_hours",
                                               "24 Hours" = "24_hours",
                                               "raw"   = "raw"
                                               #"2 weeks"   = "2_weeks",
                                               #"1 Month"   = "1_month"
                                             )
                                             , inline = T, selected = "raw"))
               ),
               fluidRow(
                 align="left",
                 actionButton("get_motor_timestamp","Show", icon("play")),
                 tags$head(
                   tags$style(HTML('#get_motor_timestamp{background-color:orange}'))
                 )
               ),
               fluidRow(
                 numericInput(inputId = "plot_range" ,label = "Plot Range",value = 500, min =100  ,step =100 ,width = 200 ),
                 numericInput(inputId = "load_range" ,label = "Load Range",value = 100000,step =10000 ,width = 200 ),
                 numericInput(inputId = "plot_speed" ,label = "Show Plot Speed",value = 0.4,step =0.01,max = 1, min = 0.01 ,width = 200 )
               ),
               fluidRow(
                 align="center",
                 h4(tags$b("Info")),
                 wellPanel(
                   fluidRow(
                     column(width=3,
                            h6("Nominal Current:")
                     ),column(width=7,
                              h6(verbatimTextOutput("n_current"))
                     )
                   ),
                   fluidRow(
                     column(width=3,
                            h6("Nominal Voltage:")
                     ),column(width=7,
                              h6(verbatimTextOutput("n_voltage"))
                     )
                   ),fluidRow(
                     column(width=3,
                            h6("Speed:")
                     ),column(width=7,
                              h6(verbatimTextOutput("speed"))
                     )
                   ),fluidRow(
                     column(width=3,
                            h6("Binary:")
                     ),column(width=7,
                              h6(verbatimTextOutput("binary"))
                     )
                   ),
                   fluidRow(
                     column(width=3,
                            h6("Nominal Power:")
                     ),column(width=7,
                              h6(verbatimTextOutput("n_power"))
                     )
                   )
                 )
               )
        )
      ),
      
      tags$hr(style="border-color: black;"),
      ## ___performance ----
      ## Motor Selection
      conditionalPanel(
        inputId = "panel-motor-performance-inspect",
        condition = 'output.ShowPanel_performance_Flag == "1"', 
        ## ___General Table ----
        fluidRow(
          ## Motor Tables
          
          align="center",
          h3("General Stats Table"),
          column(width=12,
                 align="center",
                 tags$style(HTML('table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color: orange !important;}')),
                 DT::dataTableOutput("table_inspect") 
                 
          )
        ),
        fluidRow(
          offset=1,
          ## Motor Bar plot and Images
          conditionalPanel(
            inputId = "panel-info-stator",
            condition = 'output.ShowPanel_Info_Flag == "1"',
            column(width=7,
                   h3("Bar Inspector Stator Windings"),
                   plotlyOutput("bar_inspect_stator") 
            ),
            
            column(width=4,offset=1,
                   h3("Details"),
                   textOutput("details_stator")
            )
          ),
          conditionalPanel(
            inputId = "panel-info-bearing",
            condition = 'output.ShowPanel_Info_Flag == "2"',
            column(width=7,
                   h3("Bar Inspector Bearings"),
                   plotlyOutput("bar_inspect_bearing") )
            ,
            column(width=4,offset=1,
                   h3("Details"),
                   textOutput("details_bearing")
            )
          )
          
        )
      ),
      fluidRow(
        h6("#=Not Implement yet"),
        h6("*=Demo")
      )
      
    )
    
  )
})