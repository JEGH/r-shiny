#MAIN
#______________________________________________________________________
#||Description: Classificafor for phase-in-short-circuit detection
#||Author: Joao Henriques
#||Reference work & acknowledgements: Tiago dos Santos
#________________________________________________________________________


create_solution_ClassSCRF <- function(PROJHOME){
  
  library(REARMCrispUtils)
  library(REARMDataPreparation)
  library(REARMDataModeling)
  #______________________________________________________________________
  ##########################DATA PREPARATION##############################
  #-------------------------________________-------------------------------
  data_in               <- REARMCrispUtils::read_data_classification(PROJHOME, loader_type = "tip") 
  
  dataPreparated        <- REARMDataPreparation::preparationClassification(data_in, mode="RF-ShortCircuit")
  
  list_SourceLabel      <- REARMDataPreparation::get_listSourceLabel(data_in)

  #______________________________________________________________________
  ##########################DATA MODELING##############################
  #-------------------------________________-------------------------------

  
  result  <- REARMDataModeling::start_Eval_Model_Classifier_ShortCircuit(list_SourceLabel,dataPreparated[["DQImbABCImbPlus.normByPeak"]] )

  
  result
}

    