
output$tab_tools_rooms_body <- renderUI({
  #________#__________________________________________________________________________________________________________________________________
  #____Rooms TabItem  ----
  #___________________________________________________________________________________________________________________________________
  
  fluidRow(
          fluidRow(
            align="center",
            column(width=10,
                   h2("Settings")
            )
          ),
          br(),
          tags$hr(style="border-color: black;"),
          titlePanel("Menu Rooms"),
          sidebarLayout(position = "left",
                        sidebarPanel(id="sidebar_room",
                                     sidebarMenu(id = "menu_rooms",
                                                 menuItem("Add Room", tabName = "add_room_menu", icon = icon("plus")),
                                                 menuItem("Remove Room",tabName = "remove_room_menu" , icon = icon("trash")),
                                                 menuItem("Change Room", tabName = "change_room_menu", icon = icon("edit"))
                                     )
                                     
                        ),
                        mainPanel(id="main_panel_rooms",
                                  uiOutput("body_menu_rooms")
                        )
          )
          
  )
})