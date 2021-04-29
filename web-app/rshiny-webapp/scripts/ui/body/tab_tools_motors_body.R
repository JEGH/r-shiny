
output$tab_tools_motors_body <- renderUI({
  #__________________________________________________________________________________________________________________________________
  #____Motors TabItem  ----
  #___________________________________________________________________________________________________________________________________
  fluidRow(
          fluidRow(
            align="center",
            column(width=10,
                   h2("Motor Settings")
            )
          ),
          br(),
          tags$hr(style="border-color: black;"),
          
          
          titlePanel("Menu Motors"),
          sidebarLayout(position = "left",
                        sidebarPanel(id="sidebar_motor",
                                     sidebarMenu(id = "menu_motors",
                                                 menuItem("Add Motor", tabName = "add_motor_menu", icon = icon("plus")),
                                                 menuItem("Remove Motor",tabName = "remove_motor_menu" , icon = icon("trash")),
                                                 menuItem("Change Motor", tabName = "change_motor_menu", icon = icon("edit"))
                                     )
                                     
                        ),
                        mainPanel(id="main_panel_motors",
                                  uiOutput("body_menu_motors")
                        )
                        
          )
          
          
          
          
          
          
  )
})