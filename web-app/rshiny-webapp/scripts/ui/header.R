output$header <- renderUI({
       
          if (USER$Logged == TRUE){
            fluidRow(
            column(width = 2,
                tags$li(class = "dropdown " , dropdownMenuOutput("messageMenu"))
              ),
            column(width = 2,
                tags$li(class = "dropdown ", dropdownMenuOutput("notificationsMenu"))
              ),
              column(width = 8,
                     
                     tags$a(class = "dropdown ", href='https://kalijai.zyrosite.com/',
                            tags$img(src='kali_logo.png',height='50',width='166'))
                     
              )
            )
          }else{
            fluidRow(
              column(width = 8,
                     
                     tags$a(class = "dropdown ", href='https://kalijai.zyrosite.com/',
                            tags$img(src='kali_logo.png',height='50',width='166'))
                     
              )
            )
          }
          
        
      
})