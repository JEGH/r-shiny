#____________________________________________________________________________________________________
#Funtions to Manipulate Spark Transformations and Actions
#____________________________________________________________________________________________________

create_spark_connection <- function(){
  library(sparklyr)
  config <- spark_config()
  config$`sparklyr.cores.local` <- 4
  config$`sparklyr.shell.driver-memory` <- "4G"
  config$spark.memory.fraction <- 0.9
  sc <- spark_connect(master = "local",version = "2.2.1", config=config)
  sc
}

getname_datasetsxlsx_list <- function(Cache=TRUE){
  basefn <- "rearm_ii_first_dataset_healthy"
  urlPath <- "xxxx"
  xlsFile <- paste0(basefn,".xls")
  zipFile <- paste0(basefn,".zip")
  if(Cache == FALSE)
    download.file(paste0(urlPath,zipFile),zipFile)
  files = unzip(zipFile)
  files_xlsx = files[grepl(".xlsx", files)]
  files_xlsx
}

copy_Dataxlsx_Spark <- function(files_xlsx,sc){
  
  listFiles <- lapply(files_xlsx,
                      function(file) {
                        t <- file
                        t2 <- strsplit(t, split="/")
                        t3 <- t2[[length(t2)]]
                        t4 <- t3[length(t3)]
                        t5 <- strsplit(t4, split=".xlsx")
                        t6 <- as.character(t5 )
                        data = read_excel(file,1)
                        print("copying to spark...")
                        print(t6)
                        res = copy_to(sc,data,t6, overwrite = TRUE)
                        print("done...")
                        rm(data)
                        t6
                        
                      })
  listFiles

}

copy_spark_mongodb <- function(listFiles,sc){
  res <- lapply(listFiles,
                function(name) {
                  collection= mongo(collection = name, db = "REARM-Datasets")
                  data <- as.data.frame(tbl(sc, name))
                  print("insert dataset...")
                  collection$insert(data)
                  
                  
                })
  new_collection = mongo(collection = "names-collections-datasets" , db = "REARM-Options")
  new_collection$insert(as.data.frame(listFiles))
}

get_mongodb_spark_data <- function(sc){
  new_collection = mongo(collection = "names-Collections-datasets" , db = "REARM-Options")
  
  list_names_collections <- as.list(new_collection$find())
  print(list_names_collections)
  
  # config <- spark_config()
  # config$`sparklyr.cores.local` <- 4
  # config$`sparklyr.shell.driver-memory` <- "4G"
  # config$spark.memory.fraction <- 0.9
  # sc <- spark_connect(master = "local",version = "2.2.1", config=config)
  
  res <- lapply(list_names_collections,
                function(name) {
                  new_collection = mongo(collection = name , db = "xxxx")
                  #bearings_data <- createDataFrame((new_collection$find()))
                  data <- new_collection$find()
                  print("from mongo to to spark....")
                  copy_to(sc,data,name, overwrite = TRUE)
                  print("Done....")
                  name
                })
  res
}



