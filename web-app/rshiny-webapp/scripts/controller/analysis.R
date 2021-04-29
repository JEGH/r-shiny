## Show Panels Flags for Analysis -----
ShowPanel = new.env(parent = emptyenv())
ShowPanel$Flag = 1
ShowPanel_Function = function(i){
  ShowPanel$Flag = i
}



#Observe inputs in motor analysis ----
ShowPanel_Flag = reactiveVal(ShowPanel$Flag)
output$ShowPanel_Flag = renderText({ShowPanel_Flag()})
outputOptions(output, 'ShowPanel_Flag', suspendWhenHidden=FALSE)
observeEvent(input$start, ShowPanel_Flag(ShowPanel_Function(as.numeric(2))))
observeEvent(input$back,  
             {
               ShowPanel_Flag(ShowPanel_Function(as.numeric(2)))
               updateTabItems(session, "tab",selected = "t_inspector")
               })


# ###Render Change Percent ----
# output$change_percent_currs <- renderPlot({
#   example <- read.csv("example.txt" , stringsAsFactors=FALSE)
#   curr <- example$Value[-1]
#   prev <- example$Value[1:(length(example$Value)-1)]
#   monChange <- 100 * round( (curr-prev) / prev, 2 )
#   barCols <- sapply(monChange,
#                     function(x) {
#                       if (x < 0) {
#                         return("green")
#                       } else if( x >= 0 && x <= 5 ) {
#                         return("yellow")
#                       }else if(x >5 ) {
#                         return("red")
#                       }
#                     })
#   barplot(monChange, border=NA, space=0, las=1, col=barCols, main="Percent Change Faultly")
#   
# })
# 
# ###Render Change Percent ----
# output$change_percent_vibra <- renderPlot({
#   example <- read.csv("example.txt" , stringsAsFactors=FALSE)
#   curr <- example$Value[-1]
#   prev <- example$Value[1:(length(example$Value)-1)]
#   monChange <- 100 * round( (curr-prev) / prev, 2 )
#   barCols <- sapply(monChange,
#                     function(x) {
#                       if (x < 0) {
#                         return("green")
#                       } else if( x >= 0 && x <= 5 ) {
#                         return("yellow")
#                       }else if(x >5 ) {
#                         return("red")
#                       }
#                     })
#   barplot(monChange, border=NA, space=0, las=1, col=barCols, main="Percent Change Faultly")
#   
# })
# 
# ###Render Risk Matrix----
# output$risk_matrix_currs <- renderPlot({
#   withProgress(message = 'Rendering plot', value = 0, {
#       n <- 100
#       
#       # Create the matrix to for the heat map
#       nRow <- 5 #9
#       nCol <- 5 #16
#       m3 <- matrix(c(2,2,3,3,3,1,2,2,3,3,1,1,2,2,3,1,1,2,2,2,1,1,1,1,2), nrow = 5, ncol = 5, byrow = TRUE)
#       myData <- m3 #matrix(rnorm(nRow * nCol), ncol = nCol)
#       rownames(myData) <- c("5", "4", "3", "2","1")  #letters[1:nRow]
#       colnames(myData) <- c("1", "2", "3", "4","5")  #LETTERS[1:nCol]
#       
#       # Increment the progress bar, and update the detail text.
#       incProgress(10/n, detail = paste("Loading: ", 10,"% of ",n,""))
#       
#       # For melt() to work seamlessly, myData has to be a matrix.
#       # Tidy up the data for processing. The longData dataframe is used to set the colors for the heat map
#       longData <- melt(myData)
#       colnames(longData) <- c("Likelihood", "Consequence", "value")
#       longData <- mutate(longData, value = Consequence + Likelihood)
#       # Create the Color Pallete
#       myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))
#       
#       ##____________________________________________________________________________________________________________
#       #-Read the Data ----
#       ##____________________________________________________________________________________________________________
#       
#       risk_data <- data.frame(
#         Consequence = c(1,4,5,4,4,2,1,4,3,2,1,1),
#         Likelihood =  c(4,5,5,1,1,1,4,3,1,3,2,2)
#       )
#       # Increment the progress bar, and update the detail text.
#       incProgress(10/n, detail = paste("Loading: ", 20,"% of ",n,""))
#       
#       # Filter the risk data for only those you want to display
#       #display_risk <- filter(risk_data, DisplayOnGraph. == 1) %>%
#       #  arrange(CurrentConsequence, CurrentLikelihood)
#       # Change the variable name for the plot and add the value column
#       #display_risk <-  rename(display_risk,  Consequence = CurrentConsequence, Likelihood = CurrentLikelihood)
#       display_risk <-  mutate(risk_data, value = Consequence + Likelihood)
#       
#       #_______________________________________________________________________________________________________________-
#       #Plot the Graph ----
#       #________________________________________________________________________________________________________________
#       zp1 <- ggplot(longData,aes(x = Consequence, y = Likelihood, fill = value))
#   
#       # Increment the progress bar, and update the detail text.
#       incProgress(10/n, detail = paste("Loading: ", 30,"% of ",n,""))
#       
#       zp1 <- zp1 + geom_tile()
#       zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(10))
#       zp1 <- zp1 + scale_x_continuous(breaks = 0:6, expand = c(0, 0))
#       # Increment the progress bar, and update the detail text.
#       incProgress(10/n, detail = paste("Loading: ", 40,"% of ",n,""))
#       
#       zp1 <- zp1 + scale_y_continuous(breaks = 0:6, expand = c(0, 0))
#       zp1 <- zp1 + coord_fixed()
#       # Increment the progress bar, and update the detail text.
#       incProgress(10/n, detail = paste("Loading: ", 50,"% of ",n,""))
#       zp1 <- zp1 + theme_bw()
#       # Increment the progress bar, and update the detail text.
#       incProgress(10/n, detail = paste("Loading: ", 60,"% of ",n,""))
#       zp1 <- zp1 + geom_point(data = display_risk,  size = display_risk$value, shape =17)
#       #zp1 <- zp1 + geom_text(data = display_risk,  size = display_risk$value, label ="motor-legend")
#       zp1 <- zp1 + ggtitle("Risk Matrix")
#       # Increment the progress bar, and update the detail text.
#       incProgress(10/n, detail = paste("Loading: ", 120,"% of ",n,""))
#       print(zp1)
#       
#       # Increment the progress bar, and update the detail text.
#       incProgress(10/n, detail = paste("Loading: ", 100,"% of ",n,""))
#       
#       return(zp1)
#   })
# })
# 
# ###Render Risk Matrix----
# output$risk_matrix_vibra <- renderPlot({
#   # Create the matrix to for the heat map
#   nRow <- 5 #9
#   nCol <- 5 #16
#   m3 <- matrix(c(2,2,3,3,3,1,2,2,3,3,1,1,2,2,3,1,1,2,2,2,1,1,1,1,2), nrow = 5, ncol = 5, byrow = TRUE)
#   myData <- m3 #matrix(rnorm(nRow * nCol), ncol = nCol)
#   rownames(myData) <- c("5", "4", "3", "2","1")  #letters[1:nRow]
#   colnames(myData) <- c("1", "2", "3", "4","5")  #LETTERS[1:nCol]
#   
#   # For melt() to work seamlessly, myData has to be a matrix.
#   # Tidy up the data for processing. The longData dataframe is used to set the colors for the heat map
#   longData <- melt(myData)
#   colnames(longData) <- c("Likelihood", "Consequence", "value")
#   longData <- mutate(longData, value = Consequence + Likelihood)
#   # Create the Color Pallete
#   myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))
#   
#   ##____________________________________________________________________________________________________________
#   #-Read the Data ----
#   ##____________________________________________________________________________________________________________
#   
#   risk_data <- data.frame(
#     Consequence = c(1,4,5,4,4,2,1,4,3,2,1,1),
#     Likelihood =  c(4,5,5,1,1,1,4,3,1,3,2,2)
#   )
#   
#   # Filter the risk data for only those you want to display
#   #display_risk <- filter(risk_data, DisplayOnGraph. == 1) %>%
#   #  arrange(CurrentConsequence, CurrentLikelihood)
#   # Change the variable name for the plot and add the value column
#   #display_risk <-  rename(display_risk,  Consequence = CurrentConsequence, Likelihood = CurrentLikelihood)
#   display_risk <-  mutate(risk_data, value = Consequence + Likelihood)
#   
#   #_______________________________________________________________________________________________________________-
#   #Plot the Graph ----
#   #________________________________________________________________________________________________________________
#   zp1 <- ggplot(longData,aes(x = Consequence, y = Likelihood, fill = value))
#   zp1 <- zp1 + geom_tile()
#   zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(10))
#   zp1 <- zp1 + scale_x_continuous(breaks = 0:6, expand = c(0, 0))
#   zp1 <- zp1 + scale_y_continuous(breaks = 0:6, expand = c(0, 0))
#   zp1 <- zp1 + coord_fixed()
#   zp1 <- zp1 + theme_bw()
#   zp1 <- zp1 + geom_point(data = display_risk,  size = display_risk$value, shape =17)
#   #zp1 <- zp1 + geom_text(data = display_risk,  size = display_risk$value, label ="motor-legend")
#   zp1 <- zp1 + ggtitle("Risk Matrix")
#   print(zp1)
#   
#   return(zp1)
# })







#_________________________________________________________________________________________________________
# motor currents exploration plots
#_________________________________________________________________________________________________________
elMax <- 30

# nms <- row.names(dataset_featured_merged)
# 
# dataset_featured_merged<-cbind(dataset_featured_merged, nms)
colorsCurrent <- c("#00cc00","#009933","#cc5200","#cc5200","#ff0000","#ff0000","#800000")
colorsVoltage <- c("#132B43", "#56B1F7")



### Render Park Analysis p1----
output$p1 <- renderPlotly({
  withProgress(message = 'Rendering plot', value = 0, {
      n <- 100
      #READ LOCAL DATA
      env <- reactiveFileReader(1000, session, "local/rdata/dataset_featured_merged.RData", LoadToEnvironment)
      dataset_featured_merged <- as.list(env())$dataset_featured_merged
      
      # Increment the progress bar, and update the detail text.
      incProgress(10/n, detail = paste("Loading: ", 10,"% of ",n,""))
      
      env <- reactiveFileReader(1000, session, "local/rdata/data.currents.parkTransform.eigenTransform.RData", LoadToEnvironment)
      data.currents.parkTransform.eigenTransform <- as.list(env())$data.currents.parkTransform.eigenTransform
      
      # Increment the progress bar, and update the detail text.
      incProgress(10/n, detail = paste("Loading: ", 20,"% of ",n,""))
      
      
      env <- reactiveFileReader(1000, session, "local/rdata/list2plot_DS.RData", LoadToEnvironment)
      list2plot_DS <- as.list(env())$list2plot_DS
      
      d <- event_data("plotly_selected")
      s <- event_data("plotly_click",source = "violinplot")
      print("d")
      print(d)
      print("s")
      print(s)
      
      # Increment the progress bar, and update the detail text.
      incProgress(10/n, detail = paste("Loading: ", 30,"% of ",n,""))
      
      if(!is.null(s) && !is.null(d)){
        if(s[['timestamp']][1]>d[['timestamp']][1]){
          d <- s
        }
      }else{
        if(is.null(d)){
          d <- s
        }
      }
      # Increment the progress bar, and update the detail text.
      incProgress(20/n, detail = paste("Loading: ", 50,"% of ",n,""))
      #load dataset_featured_merged
      p <- plot_ly(dataset_featured_merged, x = ~current.pc1x, y = ~current.pc1y, key = ~label)
      
      # Increment the progress bar, and update the detail text.
      incProgress(10/n, detail = paste("Loading: ", 60,"% of ",n,""))
      
      if (!is.null(d)) {
        #caso para ser possivel selecionar datasets através do violin plot. hardcoded
        if(length(d[["key"]])>0){
          p <- add_markers(p, color=~current.score, colors=colorsCurrent, text = ~paste(label, "<br />", current.score), symbol = ~factor(class), opacity=0.2)
          if(startsWith(d[["key"]][1],"factor(name):")){
            #caso de ser selecionado do violin plot
            names <- c()
            for(i in 1:length(d[["key"]])){
              names <- c(names,strsplit(strsplit(d[["key"]], "<br>")[[1]][1], "factor\\(name): ")[[1]][2])
            }
            m <- dataset_featured_merged[dataset_featured_merged$dsName %in% names, ]
            p <- add_markers(p, data = m, color=~current.score, colors=colorsCurrent,text = ~paste(label, "<br />", current.score), symbol = ~factor(class), opacity=1)
            
            # Increment the progress bar, and update the detail text.
            incProgress(20/n, detail = paste("Loading: ", 80,"% of ",n,""))
            
          }
          else{
            #test[grep("b2-", test$label),]
            #m <- dataset_featured_merged[dataset_featured_merged$label %in% d[["key"]], ]
            matchesLines <- unlist(lapply(
              d[["key"]],
              function(key){
                return(grep(key, dataset_featured_merged$label))
              }
              
            ))
            # Increment the progress bar, and update the detail text.
            incProgress(20/n, detail = paste("Loading: ", 80,"% of ",n,""))
            
            m <- dataset_featured_merged[matchesLines, ]
            p <- add_markers(p, data = m, color=~current.score, colors=colorsCurrent,text = ~paste(label, "<br />", current.score), symbol = ~factor(class), opacity=1)
          }
        }
      }else{
        p <- add_markers(p,color=~current.score, colors=colorsCurrent,text = ~paste(label, "<br />", current.score), symbol = ~factor(class))
      }
      
      # Increment the progress bar, and update the detail text.
      incProgress(20/n, detail = paste("Loading: ", 100,"% of ",n,""))
      
      layout(p, dragmode = "lasso", showlegend = T)
  })
})

### Render Park Analysis p2 ----
output$p2 <- renderPlotly({
  #withProgress(message = 'Rendering plot', value = 0, {
      #n <- 100
      env <- reactiveFileReader(1000, session, "local/rdata/dataset_featured_merged.RData", LoadToEnvironment)
      dataset_featured_merged <- as.list(env())$dataset_featured_merged
      
      # Increment the progress bar, and update the detail text.
      #incProgress(20/n, detail = paste("Loading: ", 20,"% of ",n,""))
      
      env <- reactiveFileReader(1000, session, "local/rdata/data.currents.parkTransform.eigenTransform.RData", LoadToEnvironment)
      data.currents.parkTransform.eigenTransform <- as.list(env())$data.currents.parkTransform.eigenTransform
      
      # Increment the progress bar, and update the detail text.
      #incProgress(20/n, detail = paste("Loading: ", 40,"% of ",n,""))
      
      
      env <- reactiveFileReader(1000, session, "local/rdata/list2plot_DS.RData", LoadToEnvironment)
      list2plot_DS <- as.list(env())$list2plot_DS
      
      # Increment the progress bar, and update the detail text.
      #incProgress(20/n, detail = paste("Loading: ", 60,"% of ",n,""))
      
      print("RST Space")
      d <- event_data("plotly_selected")
      s <- event_data("plotly_click",source = "violinplot")
      print("d")
      print(d)
      print("s")
      print(s)
      
      # Increment the progress bar, and update the detail text.
      #incProgress(10/n, detail = paste("Loading: ", 70,"% of ",n,""))
      
      
      if(!is.null(s) && !is.null(d)){
        if(s[['timestamp']][1]>d[['timestamp']][1]){
          d <- NULL
        }
        else{
          s <- NULL
        }
      }
      if(!is.null(s)){
        if(startsWith(s[["key"]][1],"factor(name):")){
          #caso de ser selecionado do violin plot
          names <- c()
          for(i in 1:length(s[["key"]])){
            names <- c(names,strsplit(strsplit(s[["key"]], "<br>")[[1]][1], "factor\\(name): ")[[1]][2])
          }
          return(drawCurrentOverTime(dataset_featured_merged, names, elMax, attr="dsName"))
        }
      }
      
      # Increment the progress bar, and update the detail text.
      #incProgress(10/n, detail = paste("Loading: ", 80,"% of ",n,""))
      
      if (!is.null(d)) {
        filter <- d[["key"]]
        if(length(d[["key"]])<=elMax ){
          drawCurrentOverTime(dataset_featured_merged, filter, attr="label", list2plot_DS)
          
          # Increment the progress bar, and update the detail text.
          #incProgress(10/n, detail = paste("Loading: ", 80,"% of ",n,""))
          
        }else{
          print("Case d2")
          drawCurrentOverTime(dataset_featured_merged, filter, elMax, attr="label", list2plot_DS)
          
          # Increment the progress bar, and update the detail text.
          #incProgress(10/n, detail = paste("Loading: ", 80,"% of ",n,""))
          
        }
      }else{
        plotly::plotly_empty()
      }
      # Increment the progress bar, and update the detail text.
      #incProgress(30/n, detail = paste("Loading: ", 100,"% of ",n,""))
  #})
})