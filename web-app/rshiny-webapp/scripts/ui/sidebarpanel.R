##Server Sidebarpanel UI -----
output$sidebarpanel <- renderUI({
  if (USER$Logged == TRUE) {
    if (USER$Type == 1) {
      div(
        sidebarUserPanel(
          isolate(input$userName),
          subtitle = a(icon("usr"), "Logout", href = login.page)
        ),
        sidebarMenu(
          id = "tab",
          menuItem(
            "Inspector",
            tabName = "t_inspector",
            icon = icon("search",lib = "font-awesome")
            #, badgeLabel = "Guest/Admin", badgeColor = "green"
          ),
          
          menuItem(
            "Analysis",
            tabName = "t_analysis",
            icon = icon("dashboard",lib = "font-awesome")
            #, badgeLabel = "Guest/Admin", badgeColor = "green"
          ),
          
          menuItem(
            "Stats",
            tabName = "t_stats",
            icon = icon("signal",lib = "font-awesome")
            #,badgeLabel = "Guest/Admin", badgeColor = "green"
      
          ),
          menuItem("Tools", tabName = "t_tools", icon = icon("wrench"),
             
             menuSubItem("Models", tabName = "dashboard", icon = icon("th")),
   
              
             menuSubItem("Logger", tabName = "logger", icon = icon("eye")),

              
             menuSubItem("Debugger", tabName = "debug", icon = icon("bug")),
             
             menuItem("Settings", tabName = "t_settings",icon = icon("cogs"),
                      menuSubItem('User', tabName = 'users',icon = icon("user")),
                      menuSubItem('Motors', tabName = 'motors',icon = icon("microchip")),
                      menuSubItem('Rooms', tabName = 'rooms',icon = icon("industry"))
             ),
             
             menuSubItem("Global Options", tabName = "options",   icon = icon("cog"))

          ),
          menuItem(
            "Help",
            tabName = "t_help",
            icon = icon("question")
            
          )
        )
      )
    }else if(USER$Type == 2) {
      div(
        sidebarUserPanel(
          isolate(input$userName),
          subtitle = a(icon("usr"), "Logout", href = login.page)
        ),
        sidebarMenu(
          id = "tab",
          menuItem(
            "Inspector",
            tabName = "t_inspector",
            icon = icon("search",lib = "font-awesome")
            #, badgeLabel = "Guest/Admin", badgeColor = "green"
          ),
          
          menuItem(
            "Analysis",
            tabName = "t_analysis",
            icon = icon("dashboard",lib = "font-awesome")
            #, badgeLabel = "Guest/Admin", badgeColor = "green"
          ),
          
          menuItem(
            "Stats",
            tabName = "t_stats",
            icon = icon("signal",lib = "font-awesome")
            #,badgeLabel = "Guest/Admin", badgeColor = "green"
            
          ),
          menuItem("Tools", tabName = "t_tools", icon = icon("wrench"),
                   
                   menuSubItem("Models", tabName = "dashboard", icon = icon("th")),
                   
                   
                   menuSubItem("Logger", tabName = "logger", icon = icon("eye")),
                   
                   
                   menuSubItem("Debugger", tabName = "debug", icon = icon("bug")),
                   
                   menuItem("Settings", tabName = "t_settings",icon = icon("cogs"),
                            menuSubItem('User', tabName = 'users',icon = icon("user")),
                            menuSubItem('Motors', tabName = 'motors',icon = icon("microchip")),
                            menuSubItem('Rooms', tabName = 'rooms',icon = icon("industry"))
                   ),
                   
                   menuSubItem("Global Options", tabName = "options",   icon = icon("cog"))
                   
          ),
          menuItem(
            "Help",
            tabName = "t_help",
            icon = icon("question")
            
          )
        )
      )
    
      
    }else if(USER$Type == 3) {
      div(
        sidebarUserPanel(
          isolate(input$userName),
          subtitle = a(icon("usr"), "Logout", href = login.page)
        ),
        sidebarMenu(
          id = "tab",
          menuItem(
            "Inspector",
            tabName = "t_inspector",
            icon = icon("search",lib = "font-awesome")
            #, badgeLabel = "Guest/Admin", badgeColor = "green"
          ),
          
          menuItem(
            "Analysis",
            tabName = "t_analysis",
            icon = icon("dashboard",lib = "font-awesome")
            #, badgeLabel = "Guest/Admin", badgeColor = "green"
          ),
          
          menuItem(
            "Stats",
            tabName = "t_stats",
            icon = icon("signal",lib = "font-awesome")
            #,badgeLabel = "Guest/Admin", badgeColor = "green"
            
          ),
          menuItem("Tools", tabName = "t_tools", icon = icon("wrench"),
                   
                   menuSubItem("Models", tabName = "dashboard", icon = icon("th")),
                   
                   
                   menuSubItem("Logger", tabName = "logger", icon = icon("eye")),
                   
                   
                   menuSubItem("Debugger", tabName = "debug", icon = icon("bug")),
                   
                   menuItem("Settings", tabName = "t_settings",icon = icon("cogs"),
                            menuSubItem('User', tabName = 'users',icon = icon("user")),
                            menuSubItem('Motors', tabName = 'motors',icon = icon("microchip")),
                            menuSubItem('Rooms', tabName = 'rooms',icon = icon("industry"))
                   ),
                   
                   menuSubItem("Global Options", tabName = "options",   icon = icon("cog"))
                   
          ),
          menuItem(
            "Help",
            tabName = "t_help",
            icon = icon("question")
            
          )
        )

      )
    }
    
  }
})