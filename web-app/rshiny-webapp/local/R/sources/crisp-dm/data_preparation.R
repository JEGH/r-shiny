normalize_byRMS2 <- function(res) {
  res.normByRMS <- lapply(
    seq_along(res),
    function(idx,res){
      el <- res[[idx]]
      shiftedData <- lapply(
        seq_along(el$dataToAnalyzeCurated),
        function(idx, ds, ratedCurrent, ratedVoltage){
          ratedCurrent <- ratedCurrent[[idx]]
          ratedVoltage <- ratedVoltage[[idx]]
          el <- ds[[idx]]
          currA <- el$currentPhaseR / ratedCurrent
          currB <- el$currentPhaseS / ratedCurrent
          currC <- el$currentPhaseT / ratedCurrent 
          
          voltA <- el$voltageR / ratedVoltage
          voltB <- el$voltageS / ratedVoltage
          voltC <- el$voltageT / ratedVoltage 
          
          el$voltageR <- voltA
          el$voltageS <- voltB
          el$voltageT <- voltB
          
          el$currentPhaseR <- currA
          el$currentPhaseS <- currB
          el$currentPhaseT <- currC
          
          return(
            el
          )
        }
        , ds = el$dataToAnalyzeCurated, ratedCurrent = el$motorCurrentRMSs
        , ratedVoltage = el$motorVoltageRMSs
      )
      res[[idx]]$dataToAnalyzeCurated <- shiftedData
      return(
        res[[idx]]
      )
    }, res = res
  )
  res.normByRMS
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
normalize_byPeak2 <- function(res) {
  res.normByPeak <- lapply(
    seq_along(res),
    function(idx,res){
      el <- res[[idx]]
      shiftedData <- lapply(
        seq_along(el$dataToAnalyzeCurated),
        function(idx, ds){
          el <- ds[[idx]]
          maxPeakC <- max(
            max(el$currentPhaseR)
            , max(el$currentPhaseS)
            , max(el$currentPhaseT)
          )
          maxPeakV <- max(
            max(el$voltageR)
            , max(el$voltageS)
            , max(el$voltageT)
          )
          currA <- el$currentPhaseR / maxPeakC
          currB <- el$currentPhaseS / maxPeakC
          currC <- el$currentPhaseT / maxPeakC 
          
          voltA <- el$voltageR / maxPeakV
          voltB <- el$voltageS / maxPeakV
          voltC <- el$voltageT / maxPeakV 
          
          el$voltageR <- voltA
          el$voltageS <- voltB
          el$voltageT <- voltB
          
          el$currentPhaseR <- currA
          el$currentPhaseS <- currB
          el$currentPhaseT <- currC
          
          return(
            el
          )
        }
        , ds = el$dataToAnalyzeCurated
      )
      res[[idx]]$dataToAnalyzeCurated <- shiftedData
      return(
        res[[idx]]
      )
    }, res = res
  )
  res.normByPeak
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
resShifted <- function(res) {
  resShifted <- lapply(
    seq_along(res),
    function(idx,res){
      el <- res[[idx]]
      shiftedData <- lapply(
        el$dataToAnalyzeCurated,
        function(el){
          current <- rearmSignalProcessing::shiftMaxPhaseAToZero(el)
          voltage <- rearmSignalProcessing::shiftMaxPhaseAToZero(el, threePhaseCurr = c("voltageR", "voltageS", "voltageT"))
          
          el$currentPhaseR <- current$currentPhaseR
          el$currentPhaseS <- current$currentPhaseS
          el$currentPhaseT <- current$currentPhaseT
          
          el$voltageR <- voltage$voltageR
          el$voltageS <- voltage$voltageS
          el$voltageT <- voltage$voltageT
          
          return(el)
        }
      )
      res[[idx]]$dataToAnalyzeCurated <- shiftedData
      return(
        res[[idx]]
      )
    }, res = res
  )
  resShifted
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
resShifted_normByRMS <- function(resShifted) {
  
  resShifted.normByRMS <- lapply(
    seq_along(resShifted),
    function(idx,res){
      el <- res[[idx]]
      shiftedData <- lapply(
        seq_along(el$dataToAnalyzeCurated),
        function(idx, ds, ratedCurrent, ratedVoltage){
          ratedCurrent <- ratedCurrent[[idx]]
          ratedVoltage <- ratedVoltage[[idx]]
          el <- ds[[idx]]
          currA <- el$currentPhaseR / ratedCurrent
          currB <- el$currentPhaseS / ratedCurrent
          currC <- el$currentPhaseT / ratedCurrent 
          
          voltA <- el$voltageR / ratedVoltage
          voltB <- el$voltageS / ratedVoltage
          voltC <- el$voltageT / ratedVoltage 
          
          el$voltageR <- voltA
          el$voltageS <- voltB
          el$voltageT <- voltB
          
          el$currentPhaseR <- currA
          el$currentPhaseS <- currB
          el$currentPhaseT <- currC
          
          return(
            el
          )
        }
        , ds = el$dataToAnalyzeCurated, ratedCurrent = el$motorCurrentRMSs
        , ratedVoltage = el$motorVoltageRMSs
      )
      res[[idx]]$dataToAnalyzeCurated <- shiftedData
      return(
        res[[idx]]
      )
    }, res = resShifted
  )
  resShifted.normByRMS
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
resShifted_normByPeak <- function(resShifted) {
  resShifted.normByPeak <- lapply(
    seq_along(resShifted),
    function(idx,res){
      el <- res[[idx]]
      shiftedData <- lapply(
        seq_along(el$dataToAnalyzeCurated),
        function(idx, ds){
          el <- ds[[idx]]
          maxPeakC <- max(
            max(el$currentPhaseR)
            , max(el$currentPhaseS)
            , max(el$currentPhaseT)
          )
          maxPeakV <- max(
            max(el$voltageR)
            , max(el$voltageS)
            , max(el$voltageT)
          )
          currA <- el$currentPhaseR / maxPeakC
          currB <- el$currentPhaseS / maxPeakC
          currC <- el$currentPhaseT / maxPeakC 
          
          voltA <- el$voltageR / maxPeakV
          voltB <- el$voltageS / maxPeakV
          voltC <- el$voltageT / maxPeakV 
          
          el$voltageR <- voltA
          el$voltageS <- voltB
          el$voltageT <- voltB
          
          el$currentPhaseR <- currA
          el$currentPhaseS <- currB
          el$currentPhaseT <- currC
          
          return(
            el
          )
        }
        , ds = el$dataToAnalyzeCurated
      )
      res[[idx]]$dataToAnalyzeCurated <- shiftedData
      return(
        res[[idx]]
      )
    }, res = resShifted
  )
  resShifted.normByPeak
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
normalize_byRMS <- function(res) {
  
  lapply(
    seq_along(res),
    function(idx,res){
      el <- res[[idx]]
      shiftedData <- lapply(
        seq_along(el$dataToAnalyzeCurated),
        function(idx, ds, ratedCurrent, ratedVoltage){
          ratedCurrent <- ratedCurrent[[idx]]
          ratedVoltage <- ratedVoltage[[idx]]
          el <- ds[[idx]]
          currA <- el$currentPhaseR / ratedCurrent
          currB <- el$currentPhaseS / ratedCurrent
          currC <- el$currentPhaseT / ratedCurrent 
          
          voltA <- el$voltageR / ratedVoltage
          voltB <- el$voltageS / ratedVoltage
          voltC <- el$voltageT / ratedVoltage 
          
          el$voltageR <- voltA
          el$voltageS <- voltB
          el$voltageT <- voltB
          
          el$currentPhaseR <- currA
          el$currentPhaseS <- currB
          el$currentPhaseT <- currC
          
          return(
            el
          )
        }
        , ds = el$dataToAnalyzeCurated, ratedCurrent = el$motorCurrentRMSs
        , ratedVoltage = el$motorVoltageRMSs
      )
      res[[idx]]$dataToAnalyzeCurated <- shiftedData
      return(
        res[[idx]]
      )
    }, res = res
  )

}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
normalize_byPeak <- function(res) {

  lapply(
    seq_along(res),
    function(idx,res){
      el <- res[[idx]]
      shiftedData <- lapply(
        seq_along(el$dataToAnalyzeCurated),
        function(idx, ds){
          el <- ds[[idx]]
          maxPeakC <- max(
            max(el$currentPhaseR)
            , max(el$currentPhaseS)
            , max(el$currentPhaseT)
          )
          maxPeakV <- max(
            max(el$voltageR)
            , max(el$voltageS)
            , max(el$voltageT)
          )
          currA <- el$currentPhaseR / maxPeakC
          currB <- el$currentPhaseS / maxPeakC
          currC <- el$currentPhaseT / maxPeakC 
          
          voltA <- el$voltageR / maxPeakV
          voltB <- el$voltageS / maxPeakV
          voltC <- el$voltageT / maxPeakV 
          
          el$voltageR <- voltA
          el$voltageS <- voltB
          el$voltageT <- voltB
          
          el$currentPhaseR <- currA
          el$currentPhaseS <- currB
          el$currentPhaseT <- currC
          
          return(
            el
          )
        }
        , ds = el$dataToAnalyzeCurated
      )
      res[[idx]]$dataToAnalyzeCurated <- shiftedData
      return(
        res[[idx]]
      )
    }, res = res
    )
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
get_listSourceLabel <- function(res) {
  
  ##### 0. SourceLabels dataframe
  listSourceLabel <- unlist(lapply(
    res,
    function(ds){
      lapply(
        ds$dataToAnalyzeCurated,
        function(el){
          el$sourceLabel[1]
        }
      )
    }
  ))
  listSourceLabel
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"

get_ABCImbalance_normByRMS <- function(res.normByRMS) {
  ##### 4. ABC Imbalance plus RMS per phase
  ABCImbalance.normByRMS                <- rearmUtils::genABC_Imbalance_features(res.normByRMS, T)
  ABCImbalance.normByRMS.outliers       <- boxplot.stats(ABCImbalance.normByRMS$current.rmssd)$out
  ABCImbalance.normByRMS.validIndices   <- !ABCImbalance.normByRMS$current.rmssd %in% ABCImbalance.normByRMS.outliers
  ABCImbalance.normByRMS.removedOut     <- ABCImbalance.normByRMS[ABCImbalance.normByRMS.validIndices,]
  #nrow(ABCImbalance.normByRMS.removedOut) #1895
  out <- list(ABCImbalance.normByRMS, 
            ABCImbalance.normByRMS.outliers,
            ABCImbalance.normByRMS.validIndices,
            ABCImbalance.normByRMS.removedOut)
  names(out) <- c("normByRMS","outliers","validIndices","removedOut")
  out
}


"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"

get_ABCImbalance_normByPeak <- function(res.normByPeak) {
  ABCImbalance.normByPeak               <- rearmUtils::genABC_Imbalance_features(res.normByPeak, T)
  ABCImbalance.normByPeak.outliers      <- boxplot.stats(ABCImbalance.normByPeak$current.rmssd)$out
  ABCImbalance.normByPeak.validIndices  <- !ABCImbalance.normByPeak$current.rmssd %in% ABCImbalance.normByPeak.outliers
  ABCImbalance.normByPeak.removedOut    <- ABCImbalance.normByPeak[ABCImbalance.normByPeak.validIndices,]
  #nrow(ABCImbalance.normByPeak.removedOut) #1895
  out <- list(ABCImbalance.normByPeak, 
            ABCImbalance.normByPeak.outliers,
            ABCImbalance.normByPeak.validIndices,
            ABCImbalance.normByPeak.removedOut)
  names(out) <- c("normByPeak","outliers","validIndices","removedOut")
  out
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"

get_DQImbalance_normByRMS <- function(res.normByRMS) {
  ##### 5. DQ Imbalance 
  DQImbalance.normByRMS               <- rearmUtils::genDQ_Imbalance_features(res.normByRMS, T)
  DQImbalance.normByRMS.outliers      <- boxplot.stats(DQImbalance.normByRMS$current.score)$out
  DQImbalance.normByRMS.validIndices  <- !DQImbalance.normByRMS$current.score %in% DQImbalance.normByRMS.outliers
  DQImbalance.normByRMS.removedOut    <- DQImbalance.normByRMS[DQImbalance.normByRMS.validIndices,]
  #nrow(DQImbalance.normByRMS.removedOut) #1901
  out <- list(DQImbalance.normByRMS,
            DQImbalance.normByRMS.outliers,
            DQImbalance.normByRMS.validIndices,
            DQImbalance.normByRMS.removedOut)
  names(out) <- c("normByRMS","outliers","validIndices","removedOut")
  out
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"

get_DQImbalance_normByPeak <- function(res.normByPeak) {
  DQImbalance.normByPeak              <- rearmUtils::genDQ_Imbalance_features(res.normByPeak, T)
  DQImbalance.normByPeak.outliers     <- boxplot.stats(DQImbalance.normByPeak$current.score)$out
  DQImbalance.normByPeak.validIndices <- !DQImbalance.normByPeak$current.score %in% DQImbalance.normByPeak.outliers
  DQImbalance.normByPeak.removedOut   <- DQImbalance.normByPeak[DQImbalance.normByPeak.validIndices,]
  #nrow(DQImbalance.normByPeak.removedOut) #1902
  out <- list(DQImbalance.normByPeak,
            DQImbalance.normByPeak.outliers,
            DQImbalance.normByPeak.validIndices,
            DQImbalance.normByPeak.removedOut)
  names(out) <- c("normByPeak","outliers","validIndices","removedOut")
  out
}



"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"


get_ABCImbalance_avgs_normByRMS <- function(ABCImbalance.normByRMS.removedOut) {
  ##### 6. ABC Imbalance, RMS per phase, Imbalance average plus RMS per phase average 
  # ABCImbalance_avgs.normByRMS <- rearmUtils::genABC_Imbalance_withAvgs_features(dataGlobal=resShifted.normByRMS, RMSPerPhaseAvg = T)
  ABCImbalance_avgs.normByRMS         <- rearmUtils::genABC_Imbalance_withAvgs_features(dataset_imbalance_ft = ABCImbalance.normByRMS.removedOut, RMSPerPhaseAvg = T)
  ABCImbalance_avgs.normByRMS.noNa    <- ABCImbalance_avgs.normByRMS[complete.cases(ABCImbalance_avgs.normByRMS),]
  #nrow(ABCImbalance_avgs.normByRMS.noNa) #1487
  ABCImbalance_avgs.normByRMS.noNa 
}


"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"

get_ABCImbalance_avgs_normByPeak  <- function(ABCImbalance.normByPeak.removedOut) {
  ABCImbalance_avgs.normByPeak        <- rearmUtils::genABC_Imbalance_withAvgs_features(dataset_imbalance_ft = ABCImbalance.normByPeak.removedOut, RMSPerPhaseAvg = T)
  ABCImbalance_avgs.normByPeak.noNa   <- ABCImbalance_avgs.normByPeak[complete.cases(ABCImbalance_avgs.normByPeak),]
  #nrow(ABCImbalance_avgs.normByPeak.noNa) #1487
  ABCImbalance_avgs.normByPeak.noNa 
}


"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"


get_DQImbalance_avgs_normByRMS  <- function(DQImbalance.normByRMS.removedOut) {
  ##### 7. DQ Imbalance plus Imbalance average 
  DQImbalance_avgs.normByRMS      <- rearmUtils::genDQ_Imbalance_withAvgs_features(dataset_imbalance_ft = DQImbalance.normByRMS.removedOut, full=T)
  DQImbalance_avgs.normByRMS.noNa <- DQImbalance_avgs.normByRMS[complete.cases(DQImbalance_avgs.normByRMS),]
  DQImbalance_avgs.normByRMS.noNa
}


"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"


get_DQImbalance_avgs_normByPeak  <- function(DQImbalance.normByPeak.removedOut) { 
   #nrow(DQImbalance_avgs.normByRMS.noNa) #1493
  DQImbalance_avgs.normByPeak <- rearmUtils::genDQ_Imbalance_withAvgs_features(dataset_imbalance_ft = DQImbalance.normByPeak.removedOut, full = T)
  DQImbalance_avgs.normByPeak.noNa <- DQImbalance_avgs.normByPeak[complete.cases(DQImbalance_avgs.normByPeak),]
  #nrow(DQImbalance_avgs.normByPeak.noNa) #1494
  DQImbalance_avgs.normByPeak.noNa
}



"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"


get_DQImbABCImbPlus  <- function(ABCImbalance.normByRMS, ABCImbalance.normByPeak,
                                 DQImbalance.normByRMS,  DQImbalance.normByPeak) { 
  
  

  ##### 8. Dataset 6 plus Dataset 7 
  normByRMS.valid.idxs <- DQImbalance.normByRMS[["validIndices"]] & ABCImbalance.normByRMS[["validIndices"]]
  normByPeak.valid.idxs <- DQImbalance.normByPeak[["validIndices"]] & ABCImbalance.normByPeak[["validIndices"]]
  
  ABCImbalance.normByRMS.validJoinedIdxs <- ABCImbalance.normByRMS[["normByRMS"]][normByRMS.valid.idxs,]
  ABCImbalance.normByPeak.validJoinedIdxs <- ABCImbalance.normByPeak[["normByPeak"]][normByPeak.valid.idxs,]
  
  DQImbalance.normByRMS.validJoinedIdxs <-  DQImbalance.normByRMS[["normByRMS"]][normByRMS.valid.idxs,]
  DQImbalance.normByPeak.validJoinedIdxs <- DQImbalance.normByPeak[["normByPeak"]][normByPeak.valid.idxs,]
  
  ABCImbalance_avgs.normByRMS.validJoinedIdxs <- rearmUtils::genABC_Imbalance_withAvgs_features(dataset_imbalance_ft = ABCImbalance.normByRMS.validJoinedIdxs, RMSPerPhaseAvg = T)
  ABCImbalance_avgs.normByRMS.validJoinedIdxs$class <- NULL
  ABCImbalance_avgs.normByRMS.validJoinedIdxs.noNA <- ABCImbalance_avgs.normByRMS.validJoinedIdxs[complete.cases(ABCImbalance_avgs.normByRMS.validJoinedIdxs),]
  names(ABCImbalance_avgs.normByRMS.validJoinedIdxs.noNA) <- paste0("ABC.",names(ABCImbalance_avgs.normByRMS.validJoinedIdxs.noNA))
  
  ABCImbalance_avgs.normByPeak.validJoinedIdxs <- rearmUtils::genABC_Imbalance_withAvgs_features(dataset_imbalance_ft = ABCImbalance.normByPeak.validJoinedIdxs, RMSPerPhaseAvg = T)
  ABCImbalance_avgs.normByPeak.validJoinedIdxs$class <- NULL
  ABCImbalance_avgs.normByPeak.validJoinedIdxs.noNA <- ABCImbalance_avgs.normByPeak.validJoinedIdxs[complete.cases(ABCImbalance_avgs.normByPeak.validJoinedIdxs),]
  names(ABCImbalance_avgs.normByPeak.validJoinedIdxs.noNA) <- paste0("ABC.",names(ABCImbalance_avgs.normByPeak.validJoinedIdxs.noNA))
  
  
  DQImbalance_avgs.normByRMS.validJoinedIdxs <- rearmUtils::genDQ_Imbalance_withAvgs_features(dataset_imbalance_ft = DQImbalance.normByRMS.validJoinedIdxs, full=T)
  DQImbalance_avgs.normByRMS.validJoinedIdxs.noNA <- DQImbalance_avgs.normByRMS.validJoinedIdxs[complete.cases(DQImbalance_avgs.normByRMS.validJoinedIdxs),]
  names(DQImbalance_avgs.normByRMS.validJoinedIdxs.noNA) <- paste0("DQ.",names(DQImbalance_avgs.normByRMS.validJoinedIdxs.noNA))
  
  DQImbalance_avgs.normByPeak.validJoinedIdxs <- rearmUtils::genDQ_Imbalance_withAvgs_features(dataset_imbalance_ft = DQImbalance.normByPeak.validJoinedIdxs, full=T)
  DQImbalance_avgs.normByPeak.validJoinedIdxs.noNA <- DQImbalance_avgs.normByPeak.validJoinedIdxs[complete.cases(DQImbalance_avgs.normByPeak.validJoinedIdxs),]
  names(DQImbalance_avgs.normByPeak.validJoinedIdxs.noNA) <- paste0("DQ.",names(DQImbalance_avgs.normByPeak.validJoinedIdxs.noNA))
  
  DQImbABCImbPlus.normByRMS <- cbind(ABCImbalance_avgs.normByRMS.validJoinedIdxs.noNA, DQImbalance_avgs.normByRMS.validJoinedIdxs.noNA)
  #nrow(DQImbABCImbPlus.normByRMS) #1487
  DQImbABCImbPlus.normByPeak <- cbind(ABCImbalance_avgs.normByPeak.validJoinedIdxs.noNA, DQImbalance_avgs.normByPeak.validJoinedIdxs.noNA)
  #nrow(DQImbABCImbPlus.normByPeak) #1487
  
  out <- list(DQImbABCImbPlus.normByRMS,
           DQImbABCImbPlus.normByPeak)
  names(out) <- c("normByRMS","normByPeak")
  out
}


"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"

preparationRegression <- function(res ) {
  
  data.normByRMS    <- normalize_byRMS2(res)
  data.normByPeak   <- normalize_byPeak2(res)
  listSourceLabel   <- get_listSourceLabel(res)
  
  ABCImbalance.normByRMS    = get_ABCImbalance_normByRMS(res.normByRMS = data.normByRMS)
  ABCImbalance.normByPeak   = get_ABCImbalance_normByPeak(res.normByPeak  = data.normByPeak )
  DQImbalance.normByRMS     = get_DQImbalance_normByRMS(res.normByRMS = data.normByRMS)
  DQImbalance.normByPeak    = get_DQImbalance_normByPeak(res.normByPeak = data.normByPeak)
  
  ABCImbalance.normByRMS.removedOut   = get_ABCImbalance_normByRMS(res.normByRMS = data.normByRMS)[["removedOut"]]
  ABCImbalance_avgs_normByRMS         = get_ABCImbalance_avgs_normByRMS(ABCImbalance.normByRMS.removedOut  )
  
  ABCImbalance.normByPeak.removedOut  = get_ABCImbalance_normByPeak(res.normByPeak  = data.normByPeak )[["removedOut"]]
  ABCImbalance_avgs_normByPeak        = get_ABCImbalance_avgs_normByPeak(ABCImbalance.normByPeak.removedOut) 
  
  DQImbalance.normByRMS.removedOut    = get_DQImbalance_normByRMS(res.normByRMS = data.normByRMS)[["removedOut"]]
  DQImbalance_avgs_normByRMS          = get_DQImbalance_avgs_normByRMS(DQImbalance.normByRMS.removedOut) 
  
  DQImbalance.normByPeak.removedOut   = get_DQImbalance_normByPeak(res.normByPeak = data.normByPeak)[["removedOut"]]  
  DQImbalance_avgs_normByPeak         = get_DQImbalance_avgs_normByPeak( DQImbalance.normByPeak.removedOut) 
  
  DQImbABCImbPlus = get_DQImbABCImbPlus(ABCImbalance.normByRMS, ABCImbalance.normByPeak,DQImbalance.normByRMS,  DQImbalance.normByPeak )
  
  DQImbABCImbPlus.normByRMS           <- DQImbABCImbPlus[["normByRMS"]]
  
  DQImbABCImbPlus.normByPeak          <- DQImbABCImbPlus[["normByPeak"]]
  
  DQImbABCImbPlus.normByPeak$sourceLabel  <- listSourceLabel[as.numeric(rownames(DQImbABCImbPlus.normByPeak))]
  DQImbABCImbPlus.normByRMS$sourceLabel   <- listSourceLabel[as.numeric(rownames(DQImbABCImbPlus.normByRMS))]
  
  dataFilter <- DQImbABCImbPlus.normByRMS[as.character(DQImbABCImbPlus.normByRMS$DQ._class) != "Healthy",]
  data
  
  out <- list(dataFilter, DQImbABCImbPlus.normByRMS,
              DQImbABCImbPlus.normByPeak)
  names(out) <- c("dataFilter","normByRMS","normByPeak")
  out
  
}

"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"

preparationClassification <- function(res, mode ) {
  
  if(mode == "RF-PhaseFault"){
    print("Phase-Fault")
    resShifted.normByRMS    <- normalize_byRMS(res)
    resShifted.normByPeak   <- normalize_byPeak(res)
  }
  
  else{
    print("RF-ShortCircuit")
    resShifted        <- resShifted(res)
    resShifted.normByRMS  <- resShifted_normByRMS(resShifted) 
    resShifted.normByPeak <- resShifted_normByPeak(resShifted)
  }
  
  ABCImbalance.normByRMS    = get_ABCImbalance_normByRMS(res.normByRMS = resShifted.normByRMS)
  ABCImbalance.normByPeak   = get_ABCImbalance_normByPeak(res.normByPeak  = resShifted.normByPeak )
  
  DQImbalance.normByRMS     = get_DQImbalance_normByRMS(res.normByRMS = resShifted.normByRMS)
  DQImbalance.normByPeak    = get_DQImbalance_normByPeak(res.normByPeak = resShifted.normByPeak)
  
  
  DQImbalance.normByRMS.removedOut    = get_DQImbalance_normByRMS(res.normByRMS   = resShifted.normByRMS)[["removedOut"]]
  DQImbalance.normByPeak.removedOut   = get_DQImbalance_normByPeak(res.normByPeak = resShifted.normByPeak)[["removedOut"]]
  
  DQImbalance.normByRMS.removedOut.positive <- nrow(DQImbalance.normByRMS.removedOut[DQImbalance.normByRMS.removedOut$`_class`=="Healthy",])
  DQImbalance.normByRMS.removedOut.negative <- nrow(DQImbalance.normByRMS.removedOut[DQImbalance.normByRMS.removedOut$`_class`!="Healthy",])
  DQImbalance.normByRMS.removedOut.classImb <- DQImbalance.normByRMS.removedOut.negative/DQImbalance.normByRMS.removedOut.positive
  
  DQImbalance.normByPeak.removedOut.positive <- nrow(DQImbalance.normByPeak.removedOut[DQImbalance.normByPeak.removedOut$`_class`=="Healthy",])
  DQImbalance.normByPeak.removedOut.negative <- nrow(DQImbalance.normByPeak.removedOut[DQImbalance.normByPeak.removedOut$`_class`!="Healthy",])
  DQImbalance.normByPeak.removedOut.classImb <- DQImbalance.normByPeak.removedOut.negative/DQImbalance.normByPeak.removedOut.positive
  
  
  DQImbABCImbPlus = get_DQImbABCImbPlus(ABCImbalance.normByRMS, ABCImbalance.normByPeak,DQImbalance.normByRMS,  DQImbalance.normByPeak )
  
  DQImbABCImbPlus.normByRMS           <- DQImbABCImbPlus[["normByRMS"]]
  
  DQImbABCImbPlus.normByPeak          <- DQImbABCImbPlus[["normByPeak"]]
  
  out <- list(DQImbABCImbPlus.normByRMS,
              DQImbABCImbPlus.normByPeak)
  names(out) <- c("DQImbABCImbPlus.normByRMS","DQImbABCImbPlus.normByPeak")
  out
  
}
