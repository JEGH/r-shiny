
library("rearmStatistics")
library("rearmSignalProcessing")
library("rearmUtils")
library("plotly")
library("RColorBrewer")

#---------------------------------------------------------------------------------------
PROJHOME <- "C:\\Users\\johenriques\\REARM\\REARM-Project\\rearm\\REARM"

#----------------------------------------------------------------------------------------
#2016_12_02
input_folder_2016_12_02   <- "\\loaders\\2016-12-02\\"
files_2016_12_02          <- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json", "db_noload_scC.json", 'db_noload_scA_descSCCurrent.json')
files_2016_12_02          <- c(files_2016_12_02, "db_load_healthy.json","db_load_scA.json", "db_load_scB.json", "db_load_scC.json", 'db_load_scA_descSCCurrent.json')

#tip
input_folder_tip          <- "\\loaders\\tip\\"
files_tip                 <- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json"
                             , "db_noload_scC.json", "db_load_healthy.json","db_load_scA.json"
                             , "db_load_scB.json", "db_load_scC.json", "db_noload_scA_descSCCurrent.json"
                             , "db_noload_scB_descSCCurrent.json", "db_noload_scC_descSCCurrent.json"
                             , "db_load_scA_descSCCurrent.json")
#regressionSCSeverity
input_folder_regressionSCSeverity         <- "\\loaders\\regressionSCSeverity\\"
files_regressionSCSeverity <- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json", "db_noload_scC.json", 'db_noload_scA_descSCCurrent.json')
files_regressionSCSeverity <- c(files_regressionSCSeverity, "db_load_healthy.json","db_load_scA.json", "db_load_scB.json", "db_load_scC.json", 'db_load_scA_descSCCurrent.json')
#-------------------------------------------------------------------------------




read_data <- function(PROJHOME, input_folder, files) {
  res <- lapply(
    files,
    function(el){
      resSetup <- rearmUtils::readRunner(file.path(PROJHOME, input_folder,el), usemode="classification")
    }
  )
  res
}

read_data_regression <- function(PROJHOME, input_folder, files) {

  res <- lapply(
    files,
    function(el){
      resSetup <- rearmUtils::readRunner(file.path(PROJHOME, input_folder,el))
    }
  )
}