zscore <- function(iData){
  return((iData - mean(iData))/sd(iData))
}