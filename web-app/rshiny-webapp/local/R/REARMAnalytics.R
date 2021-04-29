#'  prepare data for Solution REARM
#' @param method Is a type of Data Mining method. I.e, Class for classification, Reg for Regression
#' @param name Short Name of script 
#' @examples
#' #result <- REARM::create_solution("Class", "SC",PROJHOME)
#' @return Result of data evalutaion as list
#' @export 
create_solution  <- function(method, name,PROJHOME){
  result = NULL
  
  if (method == "Class"){
    if (name == "SC"){
      source(paste0(PROJHOME,"/R/sources/Classification-SC-RF.R"))
      print("Classification-SC-RF")
      result <- create_solution_ClassSCRF(PROJHOME)
    }
  
  }else if (method == "Reg"){
    if (name == "FSE"){
      source(paste0(PROJHOME,"/R/sources/Regression-FSE-CubistKNN.R"))
      print("Regression-FSE-CubistKNN")
      result <- create_solution_RegrFSECUBIST(PROJHOME)
    }
  }
  
  result

}
