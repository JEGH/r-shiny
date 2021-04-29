output$body_menu_users <- renderUI({
  if (USER$Logged == TRUE) {
    tabItems(
      tabItem(tabName = "add_user_menu",
            
            fluidRow(
              column(6, 
                     title = "Add User",
                     textInput("userName", "Username"),
                     textInput("email", "Email"),
                     #textInput("company", "Company"),
                     selectInput(
                       inputId = "company",
                       label = "Company",
                       choices = select_companies(),
                       selected = "",
                       multiple = FALSE
                     ),
                     #numericInput("type_user", "Type", value =""),
                     selectInput(
                       inputId = "type_user",
                       label = "Type",
                       choices = select_type_of_users(),
                       selected = "",
                       multiple = FALSE
                     ),
                     passwordInput("passwd2", "Password"),
                     passwordInput("passwd2r", "Repeat Password"),
                     br(),
                     actionButton("submit_add_user", "Submit"),
                     tags$button(id="fail_success_add_user",style="visibility:hidden","")
              )
              
            )
      ),tabItem(tabName = "remove_user_menu",
                fluidRow(
                  column(6, 
                         title = "Remove User",
                         #textInput("username_rem", "Username"),
                         selectInput(
                           inputId = "username_rem",
                           label = "Choose Username to remove",
                           choices = select_usernames(),
                           selected = "",
                           multiple = FALSE
                         ),
                         br(),
                         actionButton("submit_rem_user", "Submit")
                  )
                  
                )
                
      ),
            
      tabItem(tabName = "change_user_menu",
              fluidRow(
                column(6, 
                       title = "Change User",
                       #textInput("edit_userName", "Username"),
                       selectInput(
                         inputId = "edit_userName",
                         label = "Username",
                         choices = select_usernames(),
                         selected = "",
                         multiple = FALSE
                       ),
                       disabled(textInput("edit_email", "Email")),
                       #textInput("company", "Company"),
                       disabled(
                         selectInput(
                           inputId = "edit_company",
                           label = "Company",
                           choices = select_companies(),
                           selected = "",
                           multiple = FALSE
                         )
                       ),
                       #numericInput("type_user", "Type", value =""),
                       disabled( 
                         selectInput(
                           inputId = "edit_type_user",
                           label = "Type",
                           choices = select_type_of_users(),
                           selected = "",
                           multiple = FALSE
                         )
                       ),
                       disabled(passwordInput("edit_passwd2", "Password")),
                       disabled(passwordInput("edit_passwd2r", "Repeat Password")),
                       br(),
                       actionButton("back_home7", "<- Back"),
                       actionButton("change_edit_user","Change"),
                       disabled(actionButton("submit_edit_user", "Submit")),
                       tags$button(id="fail_success_edit_user",style="visibility:hidden","")
                       # tags$button(id='success_edit_user', style="background-color:rgb(0,250,60,1);width:100px;border-radius:30px;color:rgb(255,255,255);padding:10px",
                       #    HTML("Success &#10004;")),
                       # tags$button(id='fail_edit_user', style="background-color:rgb(255,0,0,1);width:70px;border-radius:30px;color:rgb(255,255,255);padding:11px",
                       #        HTML("Fail &#x2716;"))
                )
                
              )
      )
    )
  } else {
    login
  }
})