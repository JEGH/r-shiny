##_______________________________________________________________________________
##data.structurify 
##_______________________________________________________________________________
data.structurify <- function(datasets, attr, rm.outliers){
  if(!is.null(attr)){
    datasetStructured <- lapply(
      datasets,
      function(ds){
        dt <- unlist(lapply(
          ds,
          function(el){
            return(el[[attr]])
          }
        ), recursive = F)
        if(rm.outliers){
          outlier <- boxplot.stats(dt)$out
          dt <- dt[!dt %in% outlier]
        }
        return(dt)
      }
    )
    datasets <- datasetStructured
  }
  return(datasets)
} 

##_______________________________________________________________________________
##plot_dataset 
##_______________________________________________________________________________
plot_dataset <- function(list2plot_DS, listDSIdx, listElIdx){
  res <- plotly::plot_ly()
  for(i in 1:length(listDSIdx)){
    for(el in list2plot_DS[[listDSIdx[i]]][unlist(listElIdx[[i]])]){
      #res <- rearmSignalProcessing::reducePlotly(res,el[[1]])
      #res <- rearmSignalProcessing::reducePlotly(res,el[[2]])
      #res <- rearmSignalProcessing::reducePlotly(res,el[[3]])
    }
  }
  return(res)
}

##_______________________________________________________________________________
##createViolinPlot 
##_______________________________________________________________________________
createViolinPlot <- function(datasets, attr = NULL, seq2iterate = NULL , rm.outliers = F, colors, names, selected=NULL){
  
  datasets <- data.structurify(datasets, attr,rm.outliers)
  
  # computes the scatter plot for the choosen idx
  if(is.null(seq2iterate)){
    seq2iterate <- seq_along(datasets)
  }
  
  max_y <- max(unlist(lapply(
    datasets,
    max
  )))
  
  median.quartile <- function(x){
    out <- quantile(x, probs = c(0.25,0.5,0.75))
    names(out) <- c("ymin","y","ymax")
    return(out) 
  }
  
  #library(ggplot2)
  plotList <- lapply(
    seq2iterate,
    function(idx, datasets, names, colors){
      dt <- data.frame(
        score = datasets[[idx]],
        name = names[[idx]],
        color = colors[[idx]],
        fill = colors[[idx]]
      )
      
      violin_plot <- ggplot2::ggplot(dt, aes(x=factor(name), y=score))
      if(selected[idx] == 1){
        violin_plot <- violin_plot + geom_violin(colour = "black", fill=colors[[idx]])
      }else{
        violin_plot <- violin_plot + geom_violin(colour = colors[[idx]], fill=colors[[idx]])
      }
      
      violin_plot <- violin_plot + stat_summary(fun.data=median.quartile)
      violin_plot.ymax <- violin_plot + ylim(0,max_y) + labs(y = attr)
      return(
        plotly::ggplotly(
          violin_plot.ymax + theme_classic(), source = "violinplot"
        )
      )
      
    },
    datasets = datasets,
    names = names,
    colors = colors
  )
  
  return(
    plotly::subplot(
      plotList,
      shareY = T
    )
  )
}
##_______________________________________________________________________________
##drawCurrentOverTime
##_______________________________________________________________________________
drawCurrentOverTime <- function(dataset, filter, subsample = NULL, attr, list2plot_DS){
  #subsample is NULL or an integer
  print(paste("filter:",filter))
  matchesLines <- unlist(lapply(
    filter,
    function(key){
      return(grep(key, dataset[[attr]]))
    }
  ))
  if(!is.null(subsample)){
    dataset <- dataset[matchesLines, ]
    dataset <- data.frame(
      dataset,
      row.names = 1:nrow(dataset)
    )
    uniqueGroups <- unique(dataset$dsName)
    nPergroup <- floor(subsample/length(uniqueGroups))
    nrows <- c()
    for(i in uniqueGroups){
      print("i")
      print(i)
      dsG <- dataset[dataset$dsName %in% i,]
      nr <- row.names(dsG)
      print("length nr")
      print(length(nr))
      print("nr")
      print(nr)
      if(length(nr) > nPergroup){
        print("nr[sample(length(nr),nPergroup)]")
        print(nr[sample(length(nr),nPergroup)])
        nrows<-c(nrows, nr[sample(length(nr),nPergroup)])
      }else{
        nrows<-c(nrows, nr)
      }
    }
    
  }
  matchesLines <- unlist(lapply(
    filter,
    function(key){
      return(grep(key, dataset[[attr]]))
    }
  ))
  print(paste("matchesLines",matchesLines))
  dsIdxs <- dataset[matchesLines, ]$dsIdx
  elIdxs <- dataset[matchesLines, ]$elIdx
  print(paste("dsIdx:",dsIdxs))
  print(paste("elIdx",elIdxs))
  lastDsIdx <- dsIdxs[1]
  listElements <- list(list(elIdxs[1]))
  if(length(dsIdxs)>1){
    for(i in 2:length(dsIdxs)){
      curr <- dsIdxs[i]
      if(curr == lastDsIdx){
        listElements[length(listElements)] = list(unlist(list( listElements[length(listElements)],  elIdxs[i])))
        print("point1")
      }
      else{
        listElements[length(listElements)+1] = list(elIdxs[i])
        print("point2")
      }
      lastDsIdx <- curr
    }
  }
  print(listElements)
  
  plotly::toWebGL(plot_dataset(list2plot_DS, unique(dsIdxs), listElements)) 
}

#PlotClass---------
# PlotClass <- setRefClass(
#   "PlotClass",
#   fields = list(
#     #path = "character",
#     column = "character",
#     plot_end_index = "numeric",
#     plot_start_index = "numeric",
#     load_start_index="numeric",
#     load_end_index="numeric",
#     data = "ANY",
#     col_names="ANY",
#     plot_range = "numeric",
#     load_step="numeric",
#     date1="ANY",
#     date2="ANY",
#     plot_step="numeric",
#     motor="numeric",
#     colors="ANY"
#   ),
#   methods = list(
#     initialize=function(column_,motor_,t1,t2,plot_range_,load_step_ ){
#       #table<<-table_
#       column<<-column_
#       plot_range<<-plot_range_
#       plot_step<<-round(plot_range*0.1)
#       load_step<<-load_step_
#       plot_start_index<<-1
#       plot_end_index<<-plot_range
#       load_start_index<<-1
#       load_end_index<<-load_step
#       date1<<-t1
#       date2<<-t2
#       motor<<-motor_
#       colors<<-getRandomColors()
#       ReadDF()
#     },
#     getRandomColors=function(){
#       colors_<-c()
#       for(i in 1:length(column)){
#         color<-rgb(runif(1),runif(1),runif(1))
#         colors_<-append(colors_,color)
#       }
#       
#       return (colors_)
#     },
#     getData=function(query,columns){
#       response <- httr::POST('http://localhost:8081/query',
#                              body = paste0('query=',query,'&columns=',columns), 
#                              httr::content_type_json())
#       print("Message:")
#       #m<-strsplit(strsplit(content(response),split = '<p>')[[1]][2],split = '</p>')[[1]][1]
#       m<-strsplit(as.character(content(response)),split = "<p>")[[1]][2]
#       m<-strsplit(m,split = '</p>')[[1]][1]
#       #print(m)
#       df<-read.csv(text=m)
#       print(df[1,])
#       return (df)
#     },
#     ReadDF=function(){
#       
#       #q<-'select'
#       q<-''
#       for(i in 1:length(column)){
#         q<-paste0(q,' "',column[i],'",')
#       }
#       #print(q)
#       #data<<-getData(paste0('select ',q,'EXTRACT (MILLISECONDS FROM "Timestamp") as mili,"Timestamp","Target" from "Data"."Raw3" where "Timestamp">\'',date1,'\' AND "Timestamp"<\'',date2,'\' and "ID_Motor"=\'',motor,'\' and "Target"=\'ball\'  order by "Timestamp" asc  offset ',load_start_index,' limit ',load_step,'  '),paste0(q,'mili,Timestamp,Target'))
#       #data<<-getData(paste0('select ',q,'EXTRACT (MILLISECONDS FROM "Timestamp") as mili,"Timestamp","Target" from "Data"."Raw3" where "Timestamp">\'',date1,'\' AND "Timestamp"<\'',date2,'\' and "ID_Motor"=',motor,'  order by "Timestamp" asc  offset ',load_start_index,' limit ',load_step,'  '),paste0(q,'mili,Timestamp,Target'))
#       data<<-getData(paste0('select ',q,'EXTRACT (MILLISECONDS FROM "Timestamp") as mili,"Timestamp","Target" from "Data"."Raw3" where "Timestamp">\'',date1,'\' AND "Timestamp"<\'',date2,'\' and "ID_Motor"=',motor,'  order by "Timestamp" asc  offset ',240000,' limit ',load_step,'  '),paste0(q,'mili,Timestamp,Target'))
#       #col_names<<-colnames(data) 
#       
#     },
#     UpdataDF=function(){
#       
#       q<-''
#       for(i in 1:length(column)){
#         q<-paste0(q,' "',column[i],'",')
#       }
#       new_data<-getData(paste0('select ',q,'EXTRACT (MILLISECONDS FROM "Timestamp") as mili,"Timestamp","Target" from "Data"."Raw3" where "Timestamp">\'',date1,'\' AND "Timestamp"<\'',date2,'\' and "ID_Motor"=',motor,'  order by "Timestamp" asc  offset ',load_start_index,' limit ',load_step,'  '),paste0(q,'mili,Timestamp,Target'))
#       #new_data<-as.data.frame(Query_REARM_DB(paste0(q,'EXTRACT (MILLISECONDS FROM "Timestamp") as mili,"Timestamp" from "Data"."Raw3" where "Timestamp">\'',date1,'\' AND "Timestamp"<\'',date2,'\' and "ID_Motor"=\'',motor,'\'  order by "Timestamp" asc  offset ',load_start_index,' limit ',load_step,'  ')))
#       #new_data<-as.data.frame(Query_REARM_DB(paste0(q,'EXTRACT (MILLISECONDS FROM "Timestamp") as mili,"Timestamp" from "Data"."Raw3" where "Timestamp">\'',date1,'\' AND "Timestamp"<\'',date2,'\'  order by "Timestamp" asc  offset ',load_start_index,' limit ',load_step,'  ')))
#       if(length(new_data)>0){
#         load_start_index <<- load_end_index
#         load_end_index<<-load_end_index+load_step
#         data <<- rbind(data,new_data)  
#       }
#       
#     },
#     Plot=function(windowx=plot_start_index:plot_end_index,windowy=plot_start_index:plot_end_index,xlab="row index",ylab=column){
#       if(length(data[[column[1]]])!=0 && !all(is.na(data[[column[1]]]))){
#         #print(paste0("pstart: ",plot_start_index," :: pend: ",plot_end_index))
#         if(plot_start_index<load_end_index){
#           
#           if( !all(is.na(data[[column[1]]]))){
#             plot(windowx,data[[column[1]]][windowy],type = 'l',xlab = xlab,ylab = ylab,sub = data$Timestamp[windowx[1]],col=colors[1])  
#             #if there is problem in motor
#             faulty_outer<-length(which(data$Target[windowx]==c(Target='Faulty_Outer')))
#             ball_error<-length(which(data$Target[windowx]==c(Target='ball')))
#             inner_errors<-length(which(data$Target[windowx]==c(Target='inner')))
#             outer_erros<-length(which(data$Target[windowx]==c(Target='outer')))
#             if(faulty_outer!=0 || ball_error!=0){
#               #update motor condition
#               #print(paste0("ball len: ",ball_error))
#               if(faulty_outer!=0 || outer_erros!=0){
#                 runjs(paste0('$("#bearings_condition").html("<font color=\'red\'>Faulty_outer!</font>");'))
#               }
#               if(ball_error!=0){
#                 runjs(paste0('$("#bearings_condition").html("<font color=\'red\'>Bearings Fault!</font>")'))
#               }
#               if(inner_errors!=0){
#                 runjs(paste0('$("#bearings_condition").html("<font color=\'red\'>Inner problem!</font>")'))
#               }
#               #icon_color_flag_Function("icon-color{color:red}")
#               #icon_name_inspector_flag_Function("times-circle")
#               #icon_color_flag(icon_color_flag_Function("icon-color{color:red}"))
# 
#             }
#             #Motor is healthy
#             else{
#               #update motor condition
#               runjs(paste0('$("#bearings_condition").html("Good!")'))
#               #icon_color_flag_Function("icon-color{color:green}")
#               #icon_name_inspector_flag_Function("check-circle")
#               #icon_color_flag(icon_color_flag_Function("icon-color{color:green}"))
#             }
#           }
#           
#           
#           
#           if(length(column)>1){
#             for(i in 2:length(column)){
#               if( all(!is.na(data[[column[i]]]))){
#                 lines(windowx,data[[column[i]]][windowy],col=colors[i])
#               }
#               
#               
#             }  
#           }
#           #put legends in graph
#           if(length(column)>1){
#             legend("topleft", legend=column,
#                    col=colors, lty=1, cex=0.8)
#           }
#           
#           plot_start_index<<-plot_start_index+plot_step
#           plot_end_index<<-plot_end_index+plot_step
#         }
#         else{
#           UpdataDF()
#         }
#         
#       }
#       else{
#         #Plot zeros
#         plot(windowx,numeric(plot_range),type='l',xlab = xlab,ylab = ylab)
#       }
#       
#     }
#   )
# )
#________________________________________________________________________________________________________________\
#PlotClass2----
# Class responsible for managing the graph plots in inspector view
# 
#
# variables:
#       see comments below
#       
#   
# methods:
#       getData:
#             send http request and returns dataframe
#       REAdDF:
#             send http request with query to DB and initializes dataframe data
#       UPDATEDF:
#             does the same as previous function but it concatenates to the end
#             of dataframe data the new info from DB
#       PLot:
#           plots a range of rows from dataframe data. when it reaches the end of dataframe 
#           the PLOT function will get more data from DB using UPDATEDF
#       
#       PlotMotorStatus:
#           plots errors_df dataframe that stores all classifications made to the showned
#           motor data.
#________________________________________________________________________________________________________________/
PlotClass2 <- setRefClass(
  "PlotClass2",
  fields = list(
    column = "character",# array of strings with all the columns we want to select from DB
    #this two variables plot_start and end index represent the interval of rows of the 
    # DB/data frame to plot in the inspector
    plot_end_index = "numeric", 
    plot_start_index = "numeric",
    plot_step_per="numeric",
    plot_iterations="numeric",
    # These two variables represent the interval of rows to select from DB and add it to the data frame 
    load_start_index="numeric",
    load_end_index="numeric",
    ## data variable is the data frame that will receive data from the DB  #######################
    data = "ANY",
    col_names="ANY",
    #plot range will be plot_end_index-plot_start_index
    plot_range = "numeric",
    # load step represents the amount of data/rows collected from DB each time PlotClass Collects data from DB 
    load_step="numeric",
    # date1 and date2 represent time interval to use in the select query to the DB
    date1="ANY",
    date2="ANY",
    time1="ANY",
    time2="ANY",
    # represents the number of steps in plot progression until plot_start_index becomes plot_end_index
    # and plot_end_index+=plot_range
    plot_step="numeric",
    #motor to use in select query to DB
    motor="numeric",
    # character array that stores colors for each column selected from DB
    colors="ANY",
    #Variables used to draw motor state
    total_errors_number="numeric",
    errors_iterations="ANY",
    errors_df="ANY",
    plot_once="ANY"
    
  ),
  ##Methods of this class
  methods = list(
    #Method for initializing variables
    initialize=function(column_,motor_,date1_,date2_,time1_,time2_,plot_range_,load_step_,plot_speed ){
      column<<-column_
      plot_range<<-plot_range_
      plot_step_per<<-plot_speed
      plot_step<<-round(plot_range*plot_step_per)
      plot_iterations<<-0 
      load_step<<-load_step_
      plot_start_index<<-1
      plot_end_index<<-plot_range
      load_start_index<<-1
      load_end_index<<-load_step
      date1<<-date1_
      date2<<-date2_
      time1<<-time1_
      time2<<-time2_
      motor<<-motor_
      colors<<-getRandomColors()
      #Method to initialize data frame data with info from DB
      ReadDF()
      total_errors_number<<-0
      errors_iterations<<-0
      errors_df<<-NULL
      plot_once<<-FALSE
    },
    #Select random colors for each element in array column
    getRandomColors=function(){
      colors_<-c()
      for(i in 1:length(column)){
        color<-rgb(runif(1),runif(1),runif(1))
        colors_<-append(colors_,color)
      }
      
      return (colors_)
    },
    # Send http request to ML Server and return data frame with data.
    getData=function(query,columns){
      response <- httr::POST('http://localhost:8081/query',
                             body = paste0('query=',query,'&columns=',columns), 
                             httr::content_type_json())
      #print("Message:")
      m<-strsplit(as.character(content(response)),split = "<p>")[[1]][2]
      m<-strsplit(m,split = '</p>')[[1]][1]
      df<-read.csv(text=m)
      #print(df[1,])
      return (df)
    },
    #Method responsible for initializing the first time the data varible
    #It sends a query to ml server and initialize class data frame
    ReadDF=function(){
      #create query string
      q<-''
      for(i in 1:length(column)){
        q<-paste0(q,' "',column[i],'",')
      }
      data<<-getData(paste0('select ',q,'EXTRACT (MILLISECONDS FROM "Timestamp") as mili,"Timestamp" from "Data"."Raw3" where "Timestamp"::date between \'',date1,'\' AND \'',date2,'\'and "Timestamp"::time between \'',time1,'\' and \'',time2,'\' and "ID_Motor"=',motor,' order by "Timestamp" asc  offset ',load_start_index,' limit ',load_step,'  '),paste0(q,'mili,Timestamp'))
    },
    # Does the same as the previous method, but it concates the new information collected from the DB
    # to the class data frame variable
    UpdataDF=function(){
      #update load start/end index for the next data collection  
      load_start_index <<- load_end_index
      load_end_index<<-load_end_index+load_step
      
      q<-''
      for(i in 1:length(column)){
        q<-paste0(q,' "',column[i],'",')
      }
      new_data<-getData(paste0('select ',q,'EXTRACT (MILLISECONDS FROM "Timestamp") as mili,"Timestamp" from "Data"."Raw5" where "Timestamp"::date between \'',date1,'\' AND \'',date2,'\'and "Timestamp"::time between \'',time1,'\' and \'',time2,'\' and "ID_Motor"=',motor,'  order by "Timestamp" asc  offset ',load_start_index,' limit ',load_step,'  '),paste0(q,'mili,Timestamp'))
      if(length(new_data)>0){
        
        data <<- rbind(data,new_data)  
      }
    },
    #insert in dataframe errors_df data
    #as inputs receives iterations(number of times new data was showned to user)
    # percentage of points in an iteration that were good, ball....
    insertInErrorsDF=function(good,ball,inner,outer){
      #if errors_df was not yet initialized
      if(is.null(errors_df)){
        
        iterations<-errors_iterations
        errors_df<<-data.frame(iterations,good,ball,inner,outer)
      }
      else{
        iterations<-errors_iterations
        new_data<-data.frame(iterations,good,ball,inner,outer)
        #concat at the end of errors_df new data
        errors_df<<-rbind(errors_df,new_data)
      }
    },
    #Plots errors_df variable
    #p window is the window slide to show
    PlotMotorState=function(p_window=200){
      #print(errors_df)
      if(nrow(errors_df)>p_window){
        start_index=nrow(errors_df)-p_window  
      }
      else{
        start_index=0
      }
      end_index=nrow(errors_df)
      w=start_index:end_index
      plot(errors_df[['iterations']][w],errors_df[['good']][w],type='l',col='green',sub = tail(errors_df$iterations,n=1),xlab='Time',ylab='status%')
      lines(errors_df[['iterations']][w],errors_df[['ball']][w],type='l',col="red")
      lines(errors_df[['iterations']][w],errors_df[['inner']][w],type='l',col="purple")
      lines(errors_df[['iterations']][w],errors_df[['outer']][w],type='l',col="brown")
      legend("topleft", legend=c('healthy','ball','inner','outer'), col=c('green','red','purple','brown'), lty=1, cex=0.8)
    },
    #Class responsible to Plot  the graphs
    # It is possible to change lot of parameters like x-range and y - range, but they need to be equal
    # change x and y label, 
    # Finally select which columns from data frame variable that we want to plot (col_index)
    Plot=function(windowx=plot_start_index:plot_end_index,windowy=plot_start_index:plot_end_index,xlab="Time",ylab=column[1],col_index=c(1)){
      #if the first column of the data frame is empty, don't plot
      #But need to change this!!! Need to see if all columns that will be plotted are different than nulls or nan 
      #finally I just need to see if data[[column[col_index[1]]]][windowy] is nan or null because
      # in a different set of rows there are things to plot I need to plot them
      
      
      #if data frame has rows draw it else draw a horizontal line
      if(nrow(data[windowx,])!=0 && !all(is.na(data[windowx,]))){
        
        plot_once<<-TRUE
        #plot first selected column
        if( !all(is.na(data[[column[col_index[1]]]]))){
          plot(strptime(data$Timestamp[windowx],"%Y-%m-%d %H:%M:%OS"),data[[column[col_index[1]]]][windowy],type = 'l',xlab = xlab,ylab = ylab,sub = data$Timestamp[windowx[1]],col=colors[col_index[1]])  
        }
        else{
          plot(strptime(data$Timestamp[windowx],"%Y-%m-%d %H:%M:%OS"),numeric(plot_range),type = 'l',xlab = xlab,ylab = ylab,sub = data$Timestamp[windowx[1]],col="black")  
        }
        #if there is problem in motor
        ############################
        # plot_step_per=0.1 meaning 10%, this variable controls the number of steps 
        # the plot does until the plot has only new data to show the customer
        # for instace in the first 100ms the plotted graph goes from 1 to plot_range
        # in time 200ms the plot will be like (1+plot_step) to plot_range+plot_step
        # only at time 1000ms the user will just see new data again, there is where we need to count the errors again
        ############################
        if(plot_iterations%%(1/plot_step_per)==0){
          #count the number of indexes found in data$Target[windowx] that have column Target equal to Faulty_Outer
          faulty_outer<-length(which(data$Target[windowx]==c(Target='Faulty_Outer')))
          #similar to the above
          ball_error<-length(which(data$Target[windowx]==c(Target='ball')))
          #similar to the above
          inner_errors<-length(which(data$Target[windowx]==c(Target='inner')))
          #similar to the above
          outer_erros<-length(which(data$Target[windowx]==c(Target='outer')))
          #similar to the above
          healthy<-length(which(data$Target[windowx]==c(Target='Healthy')))
          #Sum all erros
          number_of_errors<-faulty_outer+ball_error+inner_errors+outer_erros
          
          
          #get the type of error with more errors
          max_index<-which.max(c(faulty_outer,outer_erros,ball_error,inner_errors))
          ##Need to change errors_iterations before do insertINErrorsDf becuase this function
          #uses this variable!!!!!!!!!!
          errors_iterations<<-strptime(data$Timestamp[windowx[1]],"%Y-%m-%d %H:%M:%OS")
          #print(errors_iterations)
          insertInErrorsDF(round((healthy/plot_range)*100),round((ball_error/plot_range)*100),round((inner_errors/plot_range)*100),round((outer_erros/plot_range)*100))
          
          
          total_errors_number<<-total_errors_number+number_of_errors
          
          ##Show type of error to client
          if(number_of_errors>healthy){
            if(max_index==1 || max_index==2){
              runjs(paste0('$("#bearings_condition").html("<font color=\'red\'>Faulty_outer! (',as.character(round((outer_erros/plot_range)*100)),'%)</font>");'))
            }
            if(max_index==3){
              runjs(paste0('$("#bearings_condition").html("<font color=\'red\'>Bearings problem! (',as.character(round((ball_error/plot_range)*100)),'%)</font>")'))
            }
            if(max_index==4){
              runjs(paste0('$("#bearings_condition").html("<font color=\'red\'>Inner problem! (',as.character(round((inner_errors/plot_range)*100)),'%)</font>")'))
            }
          }
          else{
            runjs(paste0('$("#bearings_condition").html("Good! (',as.character(round((healthy/plot_range)*100)),'%)")'))
          }
          
        }
        #counts number of times this function is used
        plot_iterations<<-plot_iterations+1
        
        #draw other lines in graph if col_index>1
        if(length(col_index)>1){
          for(i in 2:length(col_index)){
            if( all(!is.na(data[[column[col_index[i]]]]))){
              lines(strptime(data$Timestamp[windowx],"%Y-%m-%d %H:%M:%OS"),data[[column[col_index[i]]]][windowy],col=colors[col_index[i]])
            }
          }  
        }
        #put legends in graph
        if(length(col_index)>1){
          legend("topleft", legend=column[col_index],
                 col=colors[col_index], lty=1, cex=0.8)
        }
        #increase plot window view
        plot_start_index<<-plot_start_index+plot_step
        plot_end_index<<-plot_end_index+plot_step
      }
      
      
      
      #when data is empty or NA
      else{
        #if it is the first time we are plotting and df is emptry 
        if(!plot_once){
          #Plot zeros because there are no columns
          plot(windowx,numeric(plot_range),type='l',xlab = xlab,ylab = ylab)  
        }
        else{
          ##if already plotted everything and needs update
          if(plot_start_index>=load_end_index){
            UpdataDF()
          }
          ## plot the last values
          plot(strptime(tail(data$Timestamp,plot_range),"%Y-%m-%d %H:%M:%OS"),tail(data[[column[col_index[1]]]],plot_range),type = 'l',xlab = xlab,ylab = ylab,sub = data$Timestamp[windowx[1]],col=colors[col_index[1]])
          #draw other lines in graph if col_index>1
          #their last values
          if(length(col_index)>1){
            for(i in 2:length(col_index)){
              if( all(!is.na(data[[column[col_index[i]]]]))){
                lines(strptime(tail(data$Timestamp,plot_range),"%Y-%m-%d %H:%M:%OS"),tail(data[[column[col_index[i]]]],plot_range),col=colors[col_index[i]])
              }
            }  
          }
        }
        
      }
      
    })
  
)
