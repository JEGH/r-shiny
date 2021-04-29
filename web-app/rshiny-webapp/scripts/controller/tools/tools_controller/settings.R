## Show Panels Flags for Inspector -----
ShowPanel_settings = new.env(parent = emptyenv())
ShowPanel_settings$Flag = "home"
ShowPanel_settings_Function = function(i){
  ShowPanel_settings$Flag = i
}


#Observe inputs in settings ----
ShowPanel_settings_Flag = reactiveVal(ShowPanel_settings$Flag)
output$ShowPanel_settings_Flag = renderText({ShowPanel_settings_Flag()})


#########Observe events Actual Submenus----------------------------
#__Control Add User tab menu
observeEvent(input$add_user, {
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("add_user")))
})
###__Control Edit User Tab Menu-----------
observeEvent(input$change_user, {
  ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("change_user")))
})
###__Control Add Motor Tab Menu-----------
observeEvent(input$add_motor, {
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("add_motor")))
})
#__Control Change motor tab menu-------
observeEvent(input$change_motor_settings, {
  ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("edit_motor")))
})
####__Control AddMOtor Tab Menu-----------
observeEvent(input$create_motor_room,{  
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("create_motor_room")))
})
#__Change Room obs event-------
observeEvent(input$change_room, {
  ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("edit_motor_room")))
})
#__Remove motor tab menu ----------
observeEvent(input$rem_motor, {
   ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("rem_motor")))
})
#__Remove User Tab Menu---------
observeEvent(input$remove_user,{
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("rem_user")))
})
#__Remove rooms tab menu-------
observeEvent(input$remove_room,{
    
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("rem_room")))
})
#_______________________________________________________________________________________________________________
#Input Select obs events-----
##__OnChange Edit_username Select Input----
observeEvent(input$edit_userName, {
  user<- get_query_bd(paste0("SELECT * from \"Users\" where \"Name\"='",input$edit_userName,"'  "))
  user_types<-get_query_bd(paste0("SELECT * from \"UserType\" "))
  companies<-get_query_bd(paste0("SELECT * from \"Company\" "))
  #get the correspondent user type description for user number
  type_des<-which(user_types[['ID_UserType']]==user[['Type']])
  #get correspondent company id from company name
  companie_id<-which(companies[['Name']]==user[['Company']])
  updateTextInput(session,"edit_email", value=user[['Email']])
  updateTextInput(session,"edit_passwd2", value=user[['Password']])
  updateTextInput(session,"edit_passwd2r", value=user[['Password']])
  updateSelectInput(session,"edit_company", selected = concate_n_strings(c(companies[['ID_Company']][companie_id],'->',user[['Company']])))
  updateSelectInput(session,"edit_type_user", selected =concate_n_strings(c(user[['Type']],'->',user_types[['Description']][type_des])))
})
#____________________________________________________________________________________________________________-
#__Change btn on edit user-------
observeEvent(input$change_edit_user,{
  
  #number of clicks is odd
  if(input$change_edit_user[[1]]%%2!=0){
    disable_html_elements(FALSE,c('edit_email','edit_company','edit_type_user','edit_passwd2','edit_passwd2r'))
    shinyjs::html("change_edit_user","dont change!")
    shinyjs::enable("submit_edit_user")
   
  }
  else{
    disable_html_elements(TRUE,c('edit_email','edit_company','edit_type_user','edit_passwd2','edit_passwd2r'))
    shinyjs::html("change_edit_user","Change")
    shinyjs::disable("submit_edit_user")
  }
  
   
})
#___________________________________________________________________________________________________________________

##__OnChange Motor ID Select Input----
observeEvent(input$motor_edit_id_model, {
  motor_id<-parse_remove_motor_input(input$motor_edit_id_model)
  motor<-get_query_bd(paste0("SELECT * from \"Motors\" where \"ID_Motor\"='",motor_id,"'  "))
  rooms<-get_query_bd(paste0("SELECT * from \"Rooms\"  "))
  
  room_id<-which(rooms[['ID_Room']]==motor[['ID_Room']])
  
  updateTextInput(session,"edit_model",value = motor$Model)
  updateNumericInput(session,"edit_nominal_power", value=motor$NominalPower)
  updateNumericInput(session,"edit_nominal_frequency", value=motor$NominalFreq)
  updateTextInput(session,"edit_motor_type",value = motor$Type)
  updateNumericInput(session,"edit_gamma_i", value=motor$GammaI)
  updateNumericInput(session,"edit_gamma_v", value=motor$GammaV)
  updateNumericInput(session,"edit_delta_i", value=motor$DeltaI)
  updateNumericInput(session,"edit_delta_v", value=motor$DeltaV)
  updateNumericInput(session,"edit_powerFactor", value=motor$PowerFactor)
  updateNumericInput(session,"edit_operFreq", value=motor$OperFreq)
  updateSelectInput(session,"edit_motor_id_room", selected = concate_n_strings(c(rooms[['ID_Room']][room_id],'->',rooms[['Name']][room_id])))
})
#____________________________________________________________________________________________________________-
# Change btns in edit menus------
#__Change btn on edit motor-------
observeEvent(input$change_edit_motor,{
  
  #number of clicks is odd
  if(input$change_edit_motor[[1]]%%2!=0){
    disable_html_elements(FALSE,c('edit_model','edit_nominal_power','edit_nominal_frequency','edit_motor_type','edit_gamma_i','edit_gamma_v','edit_delta_i','edit_delta_v','edit_powerFactor','edit_operFreq','edit_motor_id_room'))
    shinyjs::html("change_edit_motor","dont change!")
    shinyjs::enable("submit_edit_motor")
    
  }
  else{
    disable_html_elements(TRUE,c('edit_model','edit_nominal_power','edit_nominal_frequency','edit_motor_type','edit_gamma_i','edit_gamma_v','edit_delta_i','edit_delta_v','edit_powerFactor','edit_operFreq','edit_motor_id_room'))
    shinyjs::html("change_edit_motor","Change")
    shinyjs::disable("submit_edit_motor")
    
  }

})
#___________________________________________________________________________________________________________________
#__OnChange RoomID Select Input ---------- 
observeEvent(input$edit_room_id_name, {
  roomID <- as.integer(strsplit(input$edit_room_id_name,split = '->')[[1]][1])  
  
  room<- get_query_bd(paste0("SELECT * from \"Rooms\" where \"ID_Room\"=",roomID," "))
  companies<-get_query_bd(paste0("SELECT * from \"Company\" "))
  company_index<-which(companies[['Name']]==room[['Company']])
  
  updateTextInput(session,"edit_name_room",value = room$Name)
  updateTextInput(session,"edit_local_room",value = room$Localization)
  updateTextInput(session,"edit_contact_room",value = room$Contact)
  updateSelectInput(session,"edit_company_room", selected = concate_n_strings(c(companies[['ID_Company']][company_index],'->',companies[['Name']][company_index])))
  
})
#___________________________________________________________________________________________________________________________
#__Change btn Room----------
observeEvent(input$change_edit_room,{
  
  #number of clicks is odd
  if(input$change_edit_room[[1]]%%2!=0){
    disable_html_elements(FALSE,c('edit_name_room','edit_local_room','edit_contact_room','edit_company_room'))
    shinyjs::html("change_edit_room","dont change!")
    shinyjs::enable("submit_edit_motor_room")
    
  }
  else{
    disable_html_elements(TRUE,c('edit_name_room','edit_local_room','edit_contact_room','edit_company_room'))
    shinyjs::html("change_edit_room","Change")
    shinyjs::disable("submit_edit_motor_room")
    
  }
  
  
})
#_______________________________________________________________________________________________________________
##Add USER ----
observeEvent(input$submit_add_user, {
  error<-0
  Name_ <- input$userName
  company<-as.character(strsplit(input$company,split = '->')[[1]][2])
  df <- data.frame(Name = Name_,
                   Email = input$email,
                   Password = input$passwd2,
                   Company = company,
                   Type = strsplit(input$type_user,split = '->')[[1]][1])
  
  #if any value of the inputs is null does not make query
  if(all_elements_not_empty(c(Name_,input$email,input$passwd2,company,input$type_user)) && input$passwd2==input$passwd2r){
    
    if(!user_already_exists(Name_)){
      res = insert_table_bd(table_name="Users", df_ = df)  
      
    }
    else{
      res=FALSE
      error<-1
    }
    
  }
  else{
    res<-FALSE
    error<-2
  }
  if(res==TRUE){

    updateTextInput(session,"userName", value="")
    updateTextInput(session,"email", value="")
    updateTextInput(session,"passwd2", value="")
    updateTextInput(session,"passwd2r", value="")
    updateTextInput(session,"company", value="")
    updateTextInput(session,"type_user", value="")
    #update select input with results from DB
    updateSelectInput(session,"username_rem",choices = select_usernames())
    updateSelectInput(session,"edit_userName",choices = select_usernames())
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    #Show success info to user for a while
    runjs(paste0("
       var btn=document.getElementById('fail_success_add_user');
       btn.style='background-color:rgb(0,250,60,1);width:100px;border-radius:30px;color:rgb(255,255,255);padding:10px';
       btn.innerHTML='Added user <b>",Name_,"</b> with Success &#10004;';
       setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))
    
  }
  else{
    
    if(input$passwd2!=input$passwd2r){
      runjs('alert("Passwords dont Match");')
    }
    else{
      if(error==2){
        runjs('alert("Fill the form and try again!");')
        #show that user fail
        runjs(paste0("
             var btn=document.getElementById('fail_success_add_user');
             btn.style='background-color:rgb(255,0,0,1);width:70px;border-radius:30px;color:rgb(255,255,255);padding:11px;visibility:visible';
             btn.innerHTML='Fail &#x2716;';
             setTimeout(function(){btn.style='visibility:hidden'},4000);
         "))
      }
      if(error==1){
        runjs("alert('Username already exists!');")

        #show that user fail
        runjs(paste0("
             var btn=document.getElementById('fail_success_add_user');
             btn.style='background-color:rgb(255,0,0,1);width:70px;border-radius:30px;color:rgb(255,255,255);padding:11px;visibility:visible';
             btn.innerHTML='Fail &#x2716;';
             setTimeout(function(){btn.style='visibility:hidden'},4000);
         "))
      }
      if(error==0){

          #show that user fail
          runjs(paste0("
             var btn=document.getElementById('fail_success_add_user');
             btn.style='background-color:rgb(255,0,0,1);width:70px;border-radius:30px;color:rgb(255,255,255);padding:11px;visibility:visible';
             btn.innerHTML='Fail &#x2716;';
             setTimeout(function(){btn.style='visibility:hidden'},4000);
         "))
      }
    }
  }
})
#____________________________________________________________________________________________________________
###Edit User-----
observeEvent(input$submit_edit_user, {
  
  
  Name_ <- input$edit_userName
  company<-as.character(strsplit(input$edit_company,split = '->')[[1]][2])
  df <- data.frame(Name = Name_,
                   Email = input$edit_email,
                   Password = input$edit_passwd2,
                   Company = company,
                   Type = strsplit(input$edit_type_user,split = '->')[[1]][1])
   
  #if any value of the inputs is null does not make query
  if(all_elements_not_empty(c(Name_,input$edit_email,input$edit_passwd2,company,input$edit_type_user)) && input$edit_passwd2==input$edit_passwd2r){
    # # writes df to the PostgreSQL database "postgres", table "cartable"
    res = send_query_bd(paste0("update \"Users\" set \"Name\"='",df[['Name']],"',\"Email\"='",df[['Email']],"',\"Password\"='",df[['Password']],"',\"Company\"='",df[['Company']],"',\"Type\"='",df[['Type']],"' where \"Name\"='",df[['Name']],"' "))
  }
  else{
    res<-FALSE
  }
  # query the data from postgreSQL 
  if(res==TRUE){

    updateTextInput(session,"edit_userName", value="")
    updateTextInput(session,"edit_email", value="")
    updateTextInput(session,"edit_passwd2", value="")
    updateTextInput(session,"edit_passwd2r", value="")
    updateTextInput(session,"edit_company", value="")
    updateTextInput(session,"edit_type_user", value="")
    #update select input with results from DB
    updateSelectInput(session,"username_rem",choices = select_usernames())
    updateSelectInput(session,"edit_userName",choices = select_usernames())
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    
    #Show success info to user for a while
    runjs(paste0("
      var btn=document.getElementById('fail_success_edit_user');
      btn.style='background-color:rgb(0,250,60,1);width:100px;border-radius:30px;color:rgb(255,255,255);padding:10px';
      btn.innerHTML='Edited <b>",Name_,"</b> user with Success &#10004;';
      setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))
    
    #if number of clicks is odd then click on btn again to reset its state
    if(input$change_edit_user[[1]]%%2!=0){
      runjs("$('#change_edit_user').click();")  
    }
    
  }
  else{
    if(input$edit_passwd2!=input$edit_passwd2r){
      runjs('alert("Passwords dont Match");')
    }
    else{
      runjs('alert("Fill the form and try again!");')

    }
    #show that user fail
    runjs(paste0("
       var btn=document.getElementById('fail_success_edit_user');
       btn.style='background-color:rgb(255,0,0,1);width:70px;border-radius:30px;color:rgb(255,255,255);padding:11px';
       btn.innerHTML='Fail &#x2716;';
       setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))
  }
})



#____________________________________________________________________________________________________________-
##Remove USER ----
observeEvent(input$submit_rem_user, {
  #Ask user if he really wants to remove
  #here in js we send a variable with name confirm_user_delete
  #to shiny app knows what to do
  runjs("
    if(confirm('Are you sure you want to delete?')){
    	alert('Deleted!');
      Shiny.onInputChange('confirm_user_delete', 'Delete');
    }
    else{
    	alert('Cancel!');
      Shiny.onInputChange('confirm_user_delete', 'Cancel');  
    }    
  ")
  observeEvent(input$confirm_user_delete,{
    
    if(input$confirm_user_delete=="Delete"){
        Name_ <- input$username_rem
        
        # remove table from database
        res = send_query_bd(paste0("DELETE FROM public.\"Users\" WHERE \"Name\" = '",Name_ ,"'"))
      
        if(res==TRUE){

          updateSelectInput(session,"username_rem",choices = select_usernames())
          ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
          updateSelectInput(session,"edit_userName",choices = select_usernames())
          
        }else{
          runjs("alert('Fail to Remove User!');")
        }
    }
    else{
      
    }
  })
  
  
})

#____________________________________________________________________________________________________________-
##Add Motor --------
observeEvent(input$submit_add_motor, {
  #gets user form input
  model_ <- input$model

  df     <- data.frame(Model = model_, 
                        NominalPower =   as.double(input$nominal_power), 
                        Type =           as.character(input$type),
                        GammaV =         as.integer(input$gamma_v),
                        GammaI =         as.integer(input$gamma_i),
                        DeltaV =         as.integer(input$delta_v),
                        DeltaI =         as.integer(input$delta_i),
                        PowerFactor =    as.double(input$powerFactor),
                        OperFreq =       as.double(input$operFreq),
                        NominalFreq =    as.double(input$nominal_frequency),
                        ID_Room =        as.integer(strsplit(input$id_room,split = '->')[[1]][1]))
  
  #if any value of the inputs is null does not make query
  if(all_elements_not_empty(c(as.character(df[['Model']]),df[['NominalPower']],as.character(df[['Type']]),df[['GammaV']],df[['GammaI']],df[['DeltaV']],df[['DeltaI']],df[['PowerFactor']],df[['OperFreq']],df[['NominalFreq']],df[['ID_Room']]))){
    
    # writes df to the PostgreSQL database "postgres", table "cartable" 
    tryCatch({
      
      res = insert_table_bd(table_name="Motors", df_ = df)
      
    },error=function(cond) {
  
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
      
    }, warning=function(cond) {
      message("Here's the original warning message:")
      message(cond)
    },finally={
      ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    })
  }
  else{
    res<-FALSE
  }
  if(res==TRUE){
    
    updateTextInput(session,"model", value="")
    updateTextInput(session,"nominal_power", value="")
    updateTextInput(session,"type", value="")
    updateTextInput(session,"gamma_v", value="")
    updateTextInput(session,"gamma_i", value="")
    updateTextInput(session,"delta_v", value="")
    updateTextInput(session,"delta_i", value="")
    updateTextInput(session,"powerFactor", value="")
    updateTextInput(session,"operFreq", value="")
    updateTextInput(session,"nominal_frequency", value="")
    updateTextInput(session,"id_room", value="")
    
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    ##Update Select input with motors user can select
    updateSelectInput(session,"model_rem",choices = get_motors_id_and_model())
    updateSelectInput(session,"motor_edit_id_model",choices = get_motors_id_and_model())
    
    #Show success info to user for a while
    runjs(paste0("
      var btn=document.getElementById('fail_success_add_motor');
      btn.style='background-color:rgb(0,250,60,1);width:100px;border-radius:30px;color:rgb(255,255,255);padding:10px';
      btn.innerHTML='Create <b>",model_,"</b> with Success &#10004;';
      setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))

  }else{

    runjs("
       alert('Some Form inputs are not filled!');  
       //$('#submit_add_motor').css('color','red');
       //$('#submit_add_motor').html('Error Submiting Form');
       //setTimeout(function(){$('#submit_add_motor').css('color','black');$('#submit_add_motor').html('Submit');},1000);
           
    ")
    #show that user fail
    runjs(paste0("
       var btn=document.getElementById('fail_success_add_motor');
       btn.style='background-color:rgb(255,0,0,1);width:70px;border-radius:30px;color:rgb(255,255,255);padding:11px';
       btn.innerHTML='Fail &#x2716;';
       setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))
  }
})
#_______________________________________________________________________________________________
##Edit Motor BTN------------
observeEvent(input$submit_edit_motor, {
  id_model<-input$motor_edit_id_model
  motor_id<-parse_remove_motor_input(input$motor_edit_id_model)
  #gets user form input
  model_ <- input$edit_model
  df     <- data.frame(Model = model_, 
                       NominalPower =   as.double(input$edit_nominal_power), 
                       Type =           as.character(input$edit_motor_type),
                       GammaV =         as.integer(input$edit_gamma_v),
                       GammaI =         as.integer(input$edit_gamma_i),
                       DeltaV =         as.integer(input$edit_delta_v),
                       DeltaI =         as.integer(input$edit_delta_i),
                       PowerFactor =    as.double(input$edit_powerFactor),
                       OperFreq =       as.double(input$edit_operFreq),
                       NominalFreq =    as.double(input$edit_nominal_frequency),
                       ID_Room =        as.integer(strsplit(input$edit_motor_id_room,split = '->')[[1]][1]))
  
  #if any value of the inputs is null does not make query
  if(all_elements_not_empty(c(as.character(df[['Model']]),df[['NominalPower']],as.character(df[['Type']]),df[['GammaV']],df[['GammaI']],df[['DeltaV']],df[['DeltaI']],df[['PowerFactor']],df[['OperFreq']],df[['NominalFreq']],df[['ID_Room']]))){
    
    # writes df to the PostgreSQL database "postgres", table "cartable" 
    tryCatch({
      
      res = send_query_bd(paste0("update \"Motors\" set \"Model\"='",df$Model,"',\"NominalPower\"=",df[['NominalPower']],",\"Type\"='",df[['Type']],"',\"GammaV\"=",df$GammaV,",\"GammaI\"=",df[['GammaI']],",\"DeltaI\"=",df$DeltaI,",\"DeltaV\"=",df$DeltaV,", \"PowerFactor\" =",df$PowerFactor,", \"OperFreq\"=",df$OperFreq,", \"NominalFreq\"=",df$NominalFreq,", \"ID_Room\"=",df[['ID_Room']],"  where \"ID_Motor\"='",motor_id,"' "))
      
    },error=function(cond) {
      
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
      
    }, warning=function(cond) {
      message("Here's the original warning message:")
      message(cond)
    },finally={
      ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    })
  }
  else{
    res<-FALSE
  }

  if(res==TRUE){

    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    ##Update Select input with motors user can select
    updateSelectInput(session,"model_rem",choices = get_motors_id_and_model())
    updateSelectInput(session,"motor_edit_id_model",choices = get_motors_id_and_model())
    ### reset change btn state
    if(input$change_edit_motor[[1]]%%2!=0){
      runjs("$('#change_edit_motor').click();")  
    }
    #Show success info to user for a while
    runjs(paste0("
      var btn=document.getElementById('fail_success_edit_motor');
      btn.style='background-color:rgb(0,250,60,1);width:100px;border-radius:30px;color:rgb(255,255,255);padding:10px';
      btn.innerHTML='Edited <b>",model_,"</b> with Success &#10004;';
      setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))
    
    
    
  }else{

    runjs("
           alert('Some Form inputs are not filled!');  
      ")
    #show that user fail
    runjs(paste0("
       var btn=document.getElementById('fail_success_edit_motor');
       btn.style='background-color:rgb(255,0,0,1);width:70px;border-radius:30px;color:rgb(255,255,255);padding:11px';
       btn.innerHTML='Fail &#x2716;';
       setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))
  }
  
})
#_______________________________________________________________________________________________
##Remove Motor ----
observeEvent(input$submit_rem_motor, {

  runjs("
    if(confirm('Are you sure you want to delete?')){
    	alert('Deleted!');
      Shiny.onInputChange('confirm_motor_delete', 'Delete');
    }
    else{
    	alert('Cancel!');
      Shiny.onInputChange('confirm_motor_delete', 'Cancel');  
    }    
  ")
  observeEvent(input$confirm_motor_delete,{
    
    if(input$confirm_motor_delete=="Delete"){    
        input_text<-input$model_rem
        #extract motor id from received input
        motor_id<-parse_remove_motor_input(input_text)
        # remove table from database
        res = send_query_bd(paste0("DELETE FROM public.\"Motors\" WHERE \"ID_Motor\" = ",motor_id ," "))
      
        if(res==TRUE){

          ##Update Select input with motors user can select
          updateSelectInput(session,"model_rem",choices = get_motors_id_and_model())
          updateSelectInput(session,"motor_edit_id_model",choices = get_motors_id_and_model())
          ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
      
        }else{
          #show error msg
          runjs("alert('Fail to Remove Motor!');")
        }
    }
    
  })
})



##Create Room ----
observeEvent(input$submit_create_motor_room, {

  name_room_ <- input$name_room
  company<-as.character(strsplit(input$company_room,split = '->')[[1]][2])
  df     <- data.frame(Name = name_room_, 
                       Localization =   as.character(input$local_room), 
                       Contact =           as.character(input$contact_room),
                       Company =         company
                       
                       )
  #if any value of the inputs is null does not make query
  if(all_elements_not_empty(c(name_room_,company,input$contact_room,input$local_room))){
    
    # writes df to the PostgreSQL database "postgres", table "cartable" 
    tryCatch({
      
      res = insert_table_bd(table_name="Rooms", df_ = df)
      
    },error=function(cond) {
      
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))

    }, warning=function(cond) {
      message("Here's the original warning message:")
      message(cond)
    },finally={
      ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    })
  }
  else{res=FALSE}

  if(res==TRUE){

    updateTextInput(session,"name_room", value="")
    updateTextInput(session,"local_room", value="")
    updateTextInput(session,"contact_room", value="")
    updateTextInput(session,"company_room", value="")
    #Update select inputs that show DB
    newRoomValues<-get_room_ids_and_names()
    updateSelectInput(session,"serialNum",choices = newRoomValues)
    updateSelectInput(session,"edit_room_id_name",choices = newRoomValues)
    #update selects from motor settings
    updateSelectInput(session,"id_room",choices = newRoomValues)
    updateSelectInput(session,"edit_motor_id_room",choices = newRoomValues)
    
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    
    #Show success info to user for a while
    runjs(paste0("
           var btn=document.getElementById('fail_success_add_room');
           btn.style='background-color:rgb(0,250,60,1);width:100px;border-radius:30px;color:rgb(255,255,255);padding:10px';
           btn.innerHTML='Created <b>",name_room_,"</b> with Success &#10004;';
           setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))
    
  }else{
    runjs("alert('Fill the Form! and try again!');")

    #show that user fail
    runjs(paste0("
       var btn=document.getElementById('fail_success_add_room');
       btn.style='background-color:rgb(255,0,0,1);width:70px;border-radius:30px;color:rgb(255,255,255);padding:11px';
       btn.innerHTML='Fail &#x2716;';
       setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))
   
  }
})
#___________________________________________________________________________________
#Edit Room Submit btn-------
observeEvent(input$submit_edit_motor_room, {
  selcted_room<-input$edit_room_id_name
  name_room_ <- input$edit_name_room
  
  room_id<-as.integer(strsplit(input$edit_room_id_name,split = "->")[[1]][1])
  company<-as.character(strsplit(input$edit_company_room,split = '->')[[1]][2])
  df     <- data.frame(Name = name_room_, 
                       Localization =   as.character(input$edit_local_room), 
                       Contact =           as.character(input$edit_contact_room),
                       Company =         company
                       
  )
  #if any value of the inputs is null does not make query
  print("Localization")
  print(df$localization)
  if(all_elements_not_empty(c(name_room_,company,as.character(input$edit_contact_room),as.character(input$edit_local_room)))){
    
    # writes df to the PostgreSQL database "postgres", table "cartable" 
    tryCatch({
      
      res = send_query_bd(paste0("update \"Rooms\" set \"Name\"='",df$Name,"', \"Localization\"='",df$Localization,"',\"Contact\"='",df$Contact,"',\"Company\"='",df$Company,"' where \"ID_Room\"=",room_id,"  "))
      
    },error=function(cond) {
      
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
      
    }, warning=function(cond) {
      message("Here's the original warning message:")
      message(cond)
    },finally={
      ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    })
  }
  else{res=FALSE}

  if(res==TRUE){

    updateTextInput(session,"name_room", value="")
    updateTextInput(session,"local_room", value="")
    updateTextInput(session,"contact_room", value="")
    updateTextInput(session,"company_room", value="")
    #Update select inputs that show DB
    newRoomValues<-get_room_ids_and_names()
    updateSelectInput(session,"edit_room_id_name",choices = newRoomValues)
    updateSelectInput(session,"serialNum",choices = newRoomValues)
    #update selects from motor settings
    updateSelectInput(session,"id_room",choices = newRoomValues)
    updateSelectInput(session,"edit_motor_id_room",choices = newRoomValues)
    ### reset change btn state
    if(input$change_edit_room[[1]]%%2!=0){
      runjs("$('#change_edit_room').click();")  
    }
    
    ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
    #Show success info to user for a while
    runjs(paste0("
           var btn=document.getElementById('fail_success_edit_room');
           btn.style='background-color:rgb(0,250,60,1);width:100px;border-radius:30px;color:rgb(255,255,255);padding:10px';
           btn.innerHTML='Edited <b>",name_room_,"</b> with Success &#10004;';
           setTimeout(function(){btn.style='visibility:hidden'},4000);
     "))
    
  }else{
    runjs("alert('Fill the Form! and try again!');")

    runjs(paste0("
       var btn=document.getElementById('fail_success_edit_room');
       btn.style='background-color:rgb(255,0,0,1);width:70px;border-radius:30px;color:rgb(255,255,255);padding:11px';
       btn.innerHTML='Fail &#x2716;';
       setTimeout(function(){btn.style='visibility:hidden'},4000);
    "))
  }
})
#____________________________________________________________________________________________
##Remove Room ----
observeEvent(input$submit_rem_room, {
  #Ask user if wants to delete
  runjs("
    if(confirm('Are you sure you want to delete?')){
    	alert('Deleted!');
      Shiny.onInputChange('confirm_room_delete', 'Delete');
    }
    else{
    	alert('Cancel!');
      Shiny.onInputChange('confirm_room_delete', 'Cancel');  
    }    
  ")
  observeEvent(input$confirm_room_delete,{
  
    if(input$confirm_room_delete=="Delete"){
    
        inputValue<-input$serialNum
        serialNum <- as.integer(strsplit(inputValue,split = '->')[[1]][1])
        # remove table from database
        res = send_query_bd(paste0("DELETE FROM public.\"Rooms\" WHERE \"ID_Room\" = '",serialNum ,"'"))
      
        if(res==TRUE){

          #updateTextInput(session,"serialNum", value="")
          ShowPanel_settings_Flag(ShowPanel_settings_Function(as.character("home")))
          #Update select inputs that show DB
          newRoomValues<-get_room_ids_and_names()
          updateSelectInput(session,"serialNum",choices = newRoomValues)
          updateSelectInput(session,"id_room",choices = newRoomValues)
          updateSelectInput(session,"edit_room_id_name",choices = newRoomValues)
        }else{
            #show error msg
            runjs("alert('Fail to Remove Room!');")
        }
    }

  })  
})