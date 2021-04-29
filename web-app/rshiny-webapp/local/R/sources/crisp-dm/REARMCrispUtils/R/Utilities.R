

#'  Read data for classfication
#' @param PROJHOME character input
#' @param loader_type character input
#' @examples
#' #
#' @return data read
#' @export
read_data_classification <- function(PROJHOME = "C:\\Users\\johenriques\\REARM\\REARM-Project\\rearm\\REARM", loader_type) {
  library("rearmStatistics")
  library("rearmSignalProcessing")
  library("rearmUtils")
  library("plotly")
  library("RColorBrewer")
  input_folder <- NULL
  files <- NULL
  if(loader_type == "2016_12_02" ){
    #2016_12_02
    input_folder   <- "\\loaders\\2016-12-02\\"
    files         <- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json", "db_noload_scC.json", 'db_noload_scA_descSCCurrent.json')
    files         <- c(files, "db_load_healthy.json","db_load_scA.json", "db_load_scB.json", "db_load_scC.json", 'db_load_scA_descSCCurrent.json')



  }
  if(loader_type == "tip" ){
    #tip
    print("tip")
    input_folder          <- "\\loaders\\tip\\"
    files               <- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json"
                                   , "db_noload_scC.json", "db_load_healthy.json","db_load_scA.json"
                                   , "db_load_scB.json", "db_load_scC.json", "db_noload_scA_descSCCurrent.json"
                                   , "db_noload_scB_descSCCurrent.json", "db_noload_scC_descSCCurrent.json"
                                   , "db_load_scA_descSCCurrent.json")

  }
  if(loader_type == "regressionSCSeverity" ){
    #regressionSCSeverity
    input_folder       <- "\\loaders\\regressionSCSeverity\\"
    files <- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json", "db_noload_scC.json", 'db_noload_scA_descSCCurrent.json')
    files <- c(files, "db_load_healthy.json","db_load_scA.json", "db_load_scB.json", "db_load_scC.json", 'db_load_scA_descSCCurrent.json')
    #-------------------------------------------------------------------------------
  }
  print("files:")
  print(files)
  res <- lapply(
    files,
    function(el){
      resSetup <- rearmUtils::readRunner(file.path(PROJHOME, input_folder,el), usemode="classification")
    }
  )
  res
}

#'  Read data for regression
#' @param PROJHOME character input
#' @param loader_type character input
#' @examples
#' #
#' @return data read
#' @export
read_data_regression <- function(PROJHOME, loader_type) {
  library("rearmStatistics")
  library("rearmSignalProcessing")
  library("rearmUtils")
  library("plotly")
  library("RColorBrewer")
  input_folder <- NULL
  files <- NULL
  if(loader_type == "2016_12_02" ){
    #2016_12_02
    input_folder   <- "\\loaders\\2016-12-02\\"
    files         <- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json", "db_noload_scC.json", 'db_noload_scA_descSCCurrent.json')
    files         <- c(files, "db_load_healthy.json","db_load_scA.json", "db_load_scB.json", "db_load_scC.json", 'db_load_scA_descSCCurrent.json')



  }
  if(loader_type == "tip" ){
    #tip
    input_folder          <- "\\loaders\\tip\\"
    files               <- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json"
                             , "db_noload_scC.json", "db_load_healthy.json","db_load_scA.json"
                             , "db_load_scB.json", "db_load_scC.json", "db_noload_scA_descSCCurrent.json"
                             , "db_noload_scB_descSCCurrent.json", "db_noload_scC_descSCCurrent.json"
                             , "db_load_scA_descSCCurrent.json")

  }
  if(loader_type == "regressionSCSeverity" ){
    #regressionSCSeverity
    input_folder       <- "\\loaders\\regressionSCSeverity\\"
    files<- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json", "db_noload_scC.json", 'db_noload_scA_descSCCurrent.json')
    files <- c(files, "db_load_healthy.json","db_load_scA.json", "db_load_scB.json", "db_load_scC.json", 'db_load_scA_descSCCurrent.json')
    #-------------------------------------------------------------------------------
  }

  res <- lapply(
    files,
    function(el){
      resSetup <- rearmUtils::readRunner(file.path(PROJHOME, input_folder,el))
    }
  )
}
