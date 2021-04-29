#__________________________________________________________________________________________________________________________________
#Login Config ----
#___________________________________________________________________________________________________________________________________


if(in_altran_net){
  login_details <- get_query_bd("SELECT * from public.\"Users\"")
}else{
  login_details <-
    data.frame(Name = c("test"),  #Login  unsafe!!, only for demo purposes
               Password = c("test"))  #Login  unsafe!!, only for demo purposes
}


output$login <- renderUI({
  #script use validate form on press "enter"
      
      mainPanel(
        
        titlePanel(
          h2("Drive Sense Open", 
                   style ="font-style: italic; font-weight: normal" ) 
        ),
        br(),
        br(),
        conditionalPanel(
          tags$script(src="/JavaScript/login.js"),
          h4(tags$b("Login")),
          textInput("userName", "Username"),
          passwordInput("passwd", "Password"),
          br(),
          actionButton("Login", "Sign in", style = "background-color: lightblue; "),
          actionLink("link-forgotPass", "Forgot your password?",style = "color: black;")
        )
      )
  
  
  
})


# To logout back to login page
login.page = paste(
  isolate(session$clientData$url_protocol),
  "//",
  isolate(session$clientData$url_hostname),
  ":",
  isolate(session$clientData$url_port),
  sep = ""
)

USER <- reactiveValues(Logged = F, Type=0)
observe({
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        print("teste login_details: ")
        print(login_details)
        Id.username <- which(login_details$Name %in% Username)
        Id.password <- which(login_details$Password %in% Password)
        if (length(Id.username) > 0 ) {
          #see if password saved for the existing username matched the inputted password
          if (login_details[['Password']][Id.username]== Password) {
            USER$Logged <- TRUE
            runjs('$("#Login").css("color","green");')
            
          }
          else{
            runjs('
                  $("#Login").html("Login Failed");
                  $("#Login").css("color", "red");
                  setTimeout(function(){$("#Login").html("Log in");$("#Login").css("color", "black");},1000);
                  ')
          }
          
        }
        else{
          runjs('
                  $("#Login").html("Login Failed");
                  $("#Login").css("color", "red");
                  setTimeout(function(){$("#Login").html("Log in");$("#Login").css("color", "black");},1000);
                ')
        }
      }
    }
  }
})

observe({
  if (USER$Logged == TRUE && USER$Type == 0) {
    print("Entrou! ")
    print(as.character(input$userName))
    if(in_altran_net){
      res = get_query_bd(paste0("SELECT  \"Type\" from public.\"Users\" WHERE \"Name\" = '",as.character(input$userName) ,"'"))
      USER$Type <- as.numeric(res$Type)
    }else{ 
      USER$Type <- 1 #offline
    }
    #dbDisconnect(con)
    #dbUnloadDriver(drv)

  }
  
})