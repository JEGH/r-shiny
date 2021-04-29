observeEvent(input$start_comparing,{
  output$postgres_test <- renderPrint({
    #Verify if all local datasets are equal to what we have in DB
    #print(list.files("Noload"))  
    foldersNames<-c("NoLoad","HalfLoad","FullLoad")
    sampling_types<-c(1,10,2,5)
    number_of_files<-0
    sampling_index<-1
    index<-1
    lapply(foldersNames, function(folder) {
      files<-list.files(folder)
      number_of_files<<-number_of_files+length(files)
      
    })
    withProgress(message = 'Comparing All Datasets', value = 0, {
      lapply(foldersNames, function(folder) {
        files<-list.files(folder)
        lapply(files,function(file){
          ex_file<-as.data.frame(read_excel(paste0("./",folder,"/",file,"")))
          res<-Query_REARM_DB(paste0('select * from "Data"."Raw" where "SamplingType"=',sampling_types[sampling_index],' and "LoadType"=\'',folder,'\' '))
          print(paste0("Excel file ",file," is equal to what we have in DB with sampling type=",sampling_types[sampling_index]," and LoadType=",folder," ?"))
          print(CompareDataFrames(res,ex_file))
          print("--------------------------------------")
          
          # #increase indexs
          sampling_index<<-sampling_index+1
          # Increment the progress bar, and update the detail text.
          incProgress(1/number_of_files, detail = paste("Compare File: ", index," of ",number_of_files,""))
          index<<-index+1
        })
        #reset index
        sampling_index<<-1
      })
    })
    
  })  
  
  
})
