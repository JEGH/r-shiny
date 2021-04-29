
library(httr)
source('scripts/utils/Plots_Utilities.R', local = TRUE)
#Utils / Vars 

#Global vars used in inspector-----
#number of points user see in plot
plot_range<-500
#number of rows from DB collected each time it goes there
load_range<-100000
#global variable that gets the selected motor at current time
#by user
motor_selected=""
#plot speed
plot_speed<-0.1

#Plot and Load Range inputs -----
observeEvent(input$plot_range,{
  plot_range<<-input$plot_range
})

observeEvent(input$load_range,{
  load_range<<-input$load_range
})
#plot speed btn -----
observeEvent(input$plot_speed,{
  plot_speed<<-input$plot_speed
})


## Show Panels Flags for Inspector -----
ShowPanel_inspector = new.env(parent = emptyenv())
ShowPanel_inspector$Flag = 0
ShowPanel_inspector_Function = function(i){
  ShowPanel_inspector$Flag = i
}
#Observe inputs in motor inspector ----
ShowPanel_inspector_Flag = reactiveVal(ShowPanel_inspector$Flag)
output$ShowPanel_inspector_Flag = renderText({ShowPanel_inspector_Flag()})
outputOptions(output, 'ShowPanel_inspector_Flag', suspendWhenHidden=FALSE)
observeEvent(input$change_target_i, ShowPanel_inspector_Flag(ShowPanel_inspector_Function(as.numeric(2))))


## Show info for Inspector -----
ShowPanel_Info = new.env(parent = emptyenv())
ShowPanel_Info$Flag = 0
ShowPanel_Info_Function = function(i){
  ShowPanel_Info$Flag = i
}

ShowPanel_Info_Flag = reactiveVal(ShowPanel_Info$Flag)
output$ShowPanel_Info_Flag = renderText({ShowPanel_Info_Flag()})
outputOptions(output, 'ShowPanel_Info_Flag', suspendWhenHidden=FALSE)


## Show Performace  Flags for Inspector -----
ShowPanel_performance = new.env(parent = emptyenv())
ShowPanel_performance$Flag = 0
ShowPanel_performance_Function = function(i){
  ShowPanel_performance$Flag = i
}

#Observe inputs in motor inspector ----
ShowPanel_performance_Flag = reactiveVal(ShowPanel_performance$Flag)
output$ShowPanel_performance_Flag = renderText({ShowPanel_performance_Flag()})
outputOptions(output, 'ShowPanel_performance_Flag', suspendWhenHidden=FALSE)



observeEvent(input$ok_select, 
             {          
               print("ok_select = ")
               print(input$motorSelect_0)
               #get motor id
               motor_selected<<-parse_remove_motor_input(input$motorSelect_0)
               
               
               t1 <- strsplit(input$motorSelect_0, ":")
               
               t2 <- trimws(t1[[1]][3], "l")
               
               t3 <- Query_REARM_DB(paste0("SELECT * FROM \"App\".\"Motors\" where \"Model\"=" ,"'", t2,"'"))
               
               ###--___target_room -----
               output$target_room <- renderText({
                 print(t3$ID_Room)
                 res <- Query_REARM_DB(paste0("SELECT * FROM \"App\".\"Rooms\" where \"ID_Room\"=" ,"'", t3$ID_Room,"'"))
                 res$Name
               }) 
               
               ###--___target_type -----
               output$target_type <- renderText({
                 print(t3$Type)
                 t3$Type
               })
               ###--___target_name -----
               output$target_name <- renderText({
                 t3$Model
               })
               
               
               ###___Target image ----
               output$image_viewer <- renderImage({
                 # Get width and height of image output
                 width  <- session$clientData$output_image_viewer_width
                 height <- session$clientData$output_image_viewer_height
                 npixels <- width * height
                 return(list(
                   src = "imgs/SCIM1.jpg",
                   #contentType = "image/png",
                   width = width,
                   height = height
                 ))
                 
                 
               }, deleteFile = FALSE)
               
               
               ###--___target_status -----
               output$target_status <- renderText({
                 print("Status")
                 print(t3$Status)
                 
                 if(t3$Status){
                   output$icon_status <- renderUI({
                     column(width = 6, 
                            tags$style(".color_green {color:#00FF00}"),
                            icon("circle",class="color_green") 
                     )
                   })
                   "running"
                 }else{
                   output$icon_status <- renderUI({
                     column(width = 6, 
                            tags$style(".color_red {color:#B22222}"),
                            icon("circle",class="color_red")
                     )
                     
                   })
                   "Stopped"
                 }
               })
               
               ShowPanel_inspector_Flag(ShowPanel_inspector_Function(as.numeric(1)))
               
             }
)







observeEvent(input$ok_change, 
             {          
               
               #get motor id
               motor_selected<<-parse_remove_motor_input(input$motorSelect)
               
               t1 <- strsplit(input$motorSelect, ":")
               
               t2 <- trimws(t1[[1]][3], "l")
               
               t3 <- Query_REARM_DB(paste0("SELECT * FROM \"App\".\"Motors\" where \"Model\"=" ,"'", t2,"'"))
               
               ###--___target_room -----
               output$target_room <- renderText({
                 print(t3$ID_Room)
                 res <- Query_REARM_DB(paste0("SELECT * FROM \"App\".\"Rooms\" where \"ID_Room\"=" ,"'", t3$ID_Room,"'"))
                 res$Name
               }) 
               
               ###--___target_type -----
               output$target_type <- renderText({
                 print(t3$Type)
                 t3$Type
               })
               ###--___target_name -----
               output$target_name <- renderText({
                 t3$Model
               })
               
               
               ###___Target image ----
               output$image_viewer <- renderImage({
                 # Get width and height of image output
                 width  <- session$clientData$output_image_viewer_width
                 height <- session$clientData$output_image_viewer_height
                 npixels <- width * height
                 return(list(
                   src = "imgs/SCIM1.jpg",
                   #contentType = "image/png",
                   width = width,
                   height = height
                 ))
                 
               }, deleteFile = FALSE)
               
               
               ###--___target_status -----
               output$target_status <- renderText({
                 print("Status")
                 print(t3$Status)
                 if(t3$Status){
                   output$icon_status <- renderUI({
                     column(width = 6, 
                            tags$style(".color_green {color:#00FF00}"),
                            icon("circle",class="color_green") 
                     )
                   })
                   "running"
                 }else{
                   output$icon_status <- renderUI({
                     column(width = 6, 
                            tags$style(".color_red {color:#B22222}"),
                            icon("circle",class="color_red")
                     )
                     
                   })
                   "Stopped"
                 }
               })
               
               withProgress(message = 'Loading', value = 0, {
                 n<-100
                 
                 start<-format(input$motor_date_selection[1])
                 end<-format(input$motor_date_selection[2])
                 
                 #get start hour
                 start_hour<-strsplit(as.character(input$start_hour)," ")[[1]][2]
                 #get end interval hour
                 end_hour<-strsplit(as.character(input$end_hour)," ")[[1]][2]
                 
                 #Protection when time val is 00:00:00 
                 if(is.na(start_hour)){
                   start_hour='00:00:001'
                 }
                 if(is.na(end_hour)){
                   start_hour='00:00:001'
                 }
                 
                 setProgress(0.5,detail = "50%")
                 plot_obj<<-PlotClass2$new(c("V1","V2","I1","I2","Vibration"),motor_selected,start,end,start_hour,end_hour,plot_range,load_range,plot_speed)
                 setProgress(1,detail = "100%")
               })  
               
               ShowPanel_inspector_Flag(ShowPanel_inspector_Function(as.numeric(1)))
               
             }
)



#---------------------------------------------------------------------------------------------------------------------------------------------------


###--Reactive Change motor name -----
changeMotorname <- reactive({
  input$motorSelect
})

###--Reactive Change motor name analysis -----
output$name_motor_analysis <- renderText({
  paste0(changeMotorname())
})
###--Reactive Change motor name stats -----
output$name_motor_stats <- renderText({
  paste0(changeMotorname())
})

### Render Motor Stator windings Inspect ----
output$bar_inspect_stator <- renderPlotly({
  motorData <- data.frame( Windings_Part = c("Phase A","Phase B","Phase C"), Severity = c(0,0,0) ) #Demo Purpose
  
  p <- plot_ly(
    x = motorData[,"Windings_Part"],
    y = motorData[,"Severity"],
    name = "Stator Windings",
    type = "bar"
  )
  return(p)
})

### Render Motor Bearings Inspect ----
output$bar_inspect_bearing <- renderPlotly({
  motorData <- data.frame( Bearings_Part = c("Bearings Losses","Rollings Elements","Bearings Raceway"), Severity = c(0,0,0) ) #Demo Purpose
  
  p <- plot_ly(
    x = motorData[,"Bearings_Part"],
    y = motorData[,"Severity"],
    name = "Bearings Inspect",
    type = "bar"
  )
  return(p)
})


myValue <- reactiveValues(detail = '')

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

df <- reactiveValues(data = data.frame(
  
  Motor_Part = c('Stator Windings', 'Bearings'),
  Severity = c(0, 0),
  MTTF = c(0,0),
  MTTCF = c(0,0),
  MTBF = c(0,0),
  MCTR = c(0,0),
  MTTR = c(0,0),
  Actions = shinyInput(actionButton, 2, 'button_', label = "Inspect", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
  stringsAsFactors = FALSE,
  row.names = 1:2
))


output$table_inspect <- DT::renderDataTable(
  df$data, server = FALSE, escape = FALSE, selection = 'none', options = list(
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",  #mudar cor!!
      "}")
  )
)

observeEvent(input$select_button, {
  selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
  
  ShowPanel_Info_Flag(ShowPanel_Info_Function(as.numeric(selectedRow)))
  if(df$data[selectedRow,1] == "Stator Windings")
    myValue$details_stator <<- paste('click on ',df$data[selectedRow,1])
  else if(df$data[selectedRow,1] == 'Bearings')
    myValue$details_bearing  <<- paste('click on ',df$data[selectedRow,1])
})


output$details_stator<- renderText({
  
  myValue$details_stator
  
})

output$details_bearing <- renderText({
  
  myValue$details_bearing 
  
})



##info stator----
output$text_info_stator<- renderText({
  "0% Faulty" 
  #not implement yet
})

###Inspector info stator ----
output$info_stator <- renderImage({
  # Get width and height of image output
  width  <- session$clientData$output_info_stator_width
  print(width)
  height <- session$clientData$output_info_stator_height
  print(height)
  npixels <- width * height
  return(list(
    src = "okMotorv3.png",
    #contentType = "image/png",
    width = width,
    height = height
  ))
  
  
  
}, deleteFile = FALSE)

##info stator bearing
output$text_info_bearing <- renderText({
  "0% Faulty"
  #not implement yet
})

###Inspector info bearing ----
output$info_bearing <- renderImage({
  # Get width and height of image output
  width  <- session$clientData$output_info_bearing_width
  print(width)
  height <- session$clientData$output_info_bearing_height
  print(height)
  npixels <- width * height
  return(list(
    src = "faultyMotorv3.png",
    #contentType = "image/png",
    width = width,
    height = height
  ))
  
  
  
}, deleteFile = FALSE)

#observe Events ----
observeEvent( 
  input$go_to_stator_analysis,
  { ShowPanel_Flag(ShowPanel_Function(as.numeric(2)))
    updateTabItems(session, "tab",selected = "t_analysis")
  }
)
observeEvent( 
  input$go_to_bearing_analysis,{
    ShowPanel_Flag(ShowPanel_Function(as.numeric(2)))
    updateTabItems(session, "tab",selected = "t_analysis")
  }
  
)
observeEvent( 
  input$go_to_stator_stats,
  updateTabItems(session, "tab",selected = "t_stats")
)
observeEvent( 
  input$go_to_bearing_stats,
  updateTabItems(session, "tab",selected = "t_stats")
)
observeEvent( 
  input$go_to_settings,
  updateTabItems(session, "tab",selected = "motors")
)

#Measurements----
# Function to get new observations
get_new_data <- function(){
  data <- rnorm(5) %>% rbind %>% data.frame
  return(data)
}

# Initialize my_data
my_data <<- get_new_data()

# Function to update my_data
update_data <- function(){
  my_data <<- rbind(get_new_data(), my_data)
}
#obs for button that gets data and plot it to the user
observeEvent(input$get_motor_timestamp,{
  withProgress(message = 'Loading', value = 0, {
    
    #Get initial timestamp
    start<-format(input$motor_date_selection[1])
    #Get final timestamp
    end<-format(input$motor_date_selection[2])
    #get start hour
    start_hour<-strsplit(as.character(input$start_hour)," ")[[1]][2]
    #get end interval hour
    end_hour<-strsplit(as.character(input$end_hour)," ")[[1]][2]
    
    #Protection when time val is 00:00:00 
    if(is.na(start_hour)){
      start_hour='00:00:001'
    }
    if(is.na(end_hour)){
      start_hour='00:00:001'
    }
    
    #set loading bar to 50%
    setProgress(0.5,detail =paste0(50,"% ") )
    
    #create plotclass obj
    #start end is the timestamp interval
    #next parameter is plot window
    # the last is the backet of rows to get each time it goes to DB
    plot_obj<<-PlotClass2$new(c("V1","V2","I1","I2","Vibration"),motor_selected,start,end,start_hour,end_hour,plot_range,load_range,plot_speed)
    
    #___voltages----
    output$voltages <- renderPlot({
      #repeat function every 100 ms
      invalidateLater(100, session)
      #col_index is the column index from plotClass obj that we want to plot
      #in this case index 1 and 2 represent V1 and V2
      plot_obj$Plot(ylab = "Voltage (V)",col_index = c(1,2))
      
    })
    #set loading bar to 60%
    setProgress(0.6,detail =paste0(60,"% ") )
    
    #___currents----
    output$currents <- renderPlot({
      #repeat function every 100 ms
      invalidateLater(100, session)
      #col_index is the column index from plotClass obj that we want to plot
      #in this case index 3 and 4 represent I1 and I2
      plot_obj$Plot(ylab = "Current (A)",col_index = c(3,4))
    })
    #...
    setProgress(0.7,detail =paste0(70,"% ") )
    
    #___vibrations----
    output$vibrations <- renderPlot({
      #repeat function every 100 ms
      invalidateLater(100, session)
      #col_index is the column index from plotClass obj that we want to plot
      #in this case index 5 represent Vibration
      plot_obj$Plot(ylab = "Vibration (100mV/g)",col_index = c(5))
      
    })
    
    #____errors-----
    output$number_of_errors <- renderPlot({
      invalidateLater(100, session)
      #if there is something to plot
      if(length(plot_obj$errors_df)!=0 && !all(is.na(plot_obj$errors_df))){
        plot_obj$PlotMotorState()
      }
      #Show user total number of errors
      runjs(paste0("
                   $('#h5_num_errors').html(\"Number of Errors - total: ",plot_obj$total_errors_number," \");
                   "))
    })
    
    
    setProgress(1,detail =paste0(100,"% ") )
    
    })
  ShowPanel_performance_Flag(ShowPanel_performance_Function(as.numeric(1)))
  
})

###data info ---

##data current----
output$n_current <- renderText({
  "8 A" 
  #not implement yet
})

##data current----
output$n_voltage <- renderText({
  "400 V" 
  #not implement yet
})

##data current----
output$speed <- renderText({
  "1460 rpm" 
  #not implement yet
})

##data current----
output$binary <- renderText({
  "26,6 N.m" 
  #not implement yet
})

##data current----
output$n_power <- renderText({
  "4,1 kW" 
  #not implement yet
})