#MAIN
#______________________________________________________________________
#||Description: Regression for Fault Severity Estimation 
#||Author: Joao Henriques
#||Reference work & acknowledgements: Tiago dos Santos
#________________________________________________________________________
create_solution_RegrFSECUBIST <- function(PROJHOME){
  library(REARMCrispUtils)
  library(REARMDataPreparation)
  library(REARMDataModeling)
  
  #______________________________________________________________________
  ##########################DATA PREPARATION##############################
  #-------------------------________________-------------------------------
  data_in        <- REARMCrispUtils::read_data_regression(PROJHOME, loader_type =  "regressionSCSeverity") 
  
  dataPreparated <- REARMDataPreparation::preparationRegression(data_in)
  
  #______________________________________________________________________
  ##########################DATA MODELING && EVALUATION##############################
  #-------------------------________________-------------------------------
  
  REARMDataModeling::start_Eval_Model_Regresssion_SeverityProb(dataPreparated[["dataFilter"]],
                                            dataPreparated[["normByRMS"]],
                                            dataPreparated[["normByPeak"]])

}