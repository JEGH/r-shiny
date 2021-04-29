output$tab_tools_users_body <- renderUI({

  fluidRow(
          fluidRow(
            align="center",
            
            h1("Settings", icon = icon("wrench"))
            
          ),
          br(),
          tags$hr(style="border-color: black;"),
          titlePanel("Menu Users"),
          sidebarLayout(position = "left",
                        sidebarPanel(id="sidebar_user",
                                     sidebarMenu(id = "menu_users",
                                                 menuItem("Add User", tabName = "add_user_menu", icon = icon("user-plus")),
                                                 menuItem("Remove User",tabName = "remove_user_menu" , icon = icon("user-times")),
                                                 menuItem("Change User", tabName = "change_user_menu", icon = icon("edit"))
                                     )
                                     
                        ),
                        mainPanel(id="main_panel_users",
                                  uiOutput("body_menu_users")
                        )
          )
          
  )
})

