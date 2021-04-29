output$body_menu_motors <- renderUI({
  if (USER$Logged == TRUE) {
    tabItems(
     
      tabItem(tabName = "add_motor_menu",

        fluidRow(
          column(6, 
                 title = "Add Motor",
                 
                 textInput("model", "Model"),
                 numericInput("nominal_power", "Nominal Power",value = ""),
                 numericInput("nominal_frequency", "Nominal Frequency",value = ""),
                 textInput("type", "Type"),
                 numericInput("gamma_i", "Gamma_I",value = ""),
                 numericInput("gamma_v", "Gamma_V",value = ""),
                 numericInput("delta_i", "Delta I",value = ""),
                 numericInput("delta_v", "Delta V",value = ""),
                 numericInput("powerFactor", "Power Factor",value = ""),
                 numericInput("operFreq", "Operation Frequency",value = ""),
                 #textInput("id_room", "ID Room"),
                 selectInput(
                   inputId = "id_room",
                   label = "ID Room",
                   choices = get_room_ids_and_names(),
                   selected = "",
                   multiple = FALSE
                 ),
                 
                 br(),
                 actionButton("submit_add_motor", "Submit"),
                 tags$button(id="fail_success_add_motor",style="visibility:hidden","")
          )
          
        )
      ),
  

  tabItem(tabName = "remove_motor_menu",
    fluidRow(
      column(6, 
             title = "Remove Motor",
             #textInput("model_rem", "Serial Number"),
             selectInput(
               inputId = "model_rem",
               label = "Choose Motor to Remove",
               choices = get_motors_id_and_model(),
               selected = "",
               multiple = FALSE
             ),
             br(),
             actionButton("submit_rem_motor", "Submit")
      )
      
    )
  ),
  
  tabItem(tabName = "change_motor_menu",
          fluidRow(
            column(6, 
                   title = "Edit Motor",
                   selectInput(
                     inputId = "motor_edit_id_model",
                     label = "Choose Motor to Edit",
                     choices = get_motors_id_and_model(),
                     selected = "",
                     multiple = FALSE
                   ),
                   
                   disabled(textInput("edit_model", "Model")),
                   disabled(numericInput("edit_nominal_power", "Nominal Power",value = "")),
                   disabled(numericInput("edit_nominal_frequency", "Nominal Frequency",value = "")),
                   disabled(textInput("edit_motor_type", "Type")),
                   disabled(numericInput("edit_gamma_i", "Gamma_I",value = "")),
                   disabled(numericInput("edit_gamma_v", "Gamma_V",value = "")),
                   disabled(numericInput("edit_delta_i", "Delta I",value = "")),
                   disabled(numericInput("edit_delta_v", "Delta V",value = "")),
                   disabled(numericInput("edit_powerFactor", "Power Factor",value = "")),
                   disabled(numericInput("edit_operFreq", "Operation Frequency",value = "")),
                   #textInput("id_room", "ID Room"),
                   disabled(selectInput(
                     inputId = "edit_motor_id_room",
                     label = "ID Room",
                     choices = get_room_ids_and_names(),
                     selected = "",
                     multiple = FALSE
                   )),
                   
                   br(),
                   actionButton("back_home_CM", "<- Back"),
                   actionButton("change_edit_motor", "Change"),
                   disabled(actionButton("submit_edit_motor", "Submit")),
                   tags$button(id="fail_success_edit_motor",style="visibility:hidden","")
            )
            
          )
  )
  
)
    
} else {
    login
  }
})