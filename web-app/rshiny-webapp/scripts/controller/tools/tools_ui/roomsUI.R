output$body_menu_rooms <- renderUI({
  if (USER$Logged == TRUE) {
    tabItems(
      tabItem(tabName = "add_room_menu",
        fluidRow(
          column(6, 
                 title = "Add Motor Room",
                 textInput("name_room", "Name"),
                 textInput("local_room", "Localization"),
                 textInput("contact_room", "Contact"),
                 #textInput("company_room", "Company"),
                 selectInput(
                   inputId = "company_room",
                   label = "Company",
                   choices = select_companies(),
                   selected = "",
                   multiple = FALSE
                 ),
                 br(),
                 actionButton("submit_create_motor_room", "Submit"),
                 tags$button(id="fail_success_add_room",style="visibility:hidden","")
          )
          
        )
      ),
      tabItem(tabName = "remove_room_menu",
        fluidRow(
          column(6, 
                 title = "Remove Motor Room",
                 #textInput("serialNum", "Name"),
                 selectInput(
                   inputId = "serialNum",
                   label = "Choose Room to Delete",
                   choices = get_room_ids_and_names(),
                   selected = "",
                   multiple = FALSE
                 ),
                 br(),
                 actionButton("submit_rem_room", "Submit")
          )
          
        )
      ),
      tabItem(tabName = "change_room_menu",
              fluidRow(
                column(6, 
                       title = "Edit Motor Room",
                       selectInput(
                         inputId = "edit_room_id_name",
                         label = "Choose Room to Edit",
                         choices = get_room_ids_and_names(),
                         selected = "",
                         multiple = FALSE
                       ),
                       disabled(textInput("edit_name_room", "Name")),
                       disabled(textInput("edit_local_room", "Localization")),
                       disabled(textInput("edit_contact_room", "Contact")),
                       #textInput("company_room", "Company"),
                       disabled(
                         selectInput(
                           inputId = "edit_company_room",
                           label = "Company",
                           choices = select_companies(),
                           selected = "",
                           multiple = FALSE
                         )
                       ),
                       br(),
                       actionButton("back_home_ER", "<- Back"),
                       actionButton("change_edit_room", "Change"),
                       disabled(actionButton("submit_edit_motor_room", "Submit")),
                       tags$button(id="fail_success_edit_room",style="visibility:hidden","")
                )
                
              )
      )
    
 )
  } else {
    login
  }
})