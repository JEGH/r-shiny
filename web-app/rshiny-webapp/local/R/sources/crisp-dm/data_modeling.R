


start_Eval_Model_C_Fault_WithoutTime  <- function(list_SourceLabel,DQImbABCImbPlus.normByPeak ) {

  
  DQImbABCImbPlus.normByPeak$sourceLabel <- list_SourceLabel[as.numeric(rownames(DQImbABCImbPlus.normByPeak))]
  percTrain <- 0.5
  percValid <- 0.2
  percTest <- 0.3 
  
  nTrain <- round(nrow(DQImbABCImbPlus.normByPeak)*percTrain)
  nValidation <- round(nrow(DQImbABCImbPlus.normByPeak)*percValid)
  nTest <- nrow(DQImbABCImbPlus.normByPeak)-(nTrain+nValidation)
  
  idxTrain <- sample(nrow(DQImbABCImbPlus.normByPeak),nTrain)
  DQImbABCImbPlus.normByPeak.noTrain <- DQImbABCImbPlus.normByPeak[-idxTrain,]
  idxValid <- sample(nrow(DQImbABCImbPlus.normByPeak.noTrain),nValidation)
  DQImbABCImbPlus.normByPeak.noTrain.noValid <- DQImbABCImbPlus.normByPeak.noTrain[-idxValid,]
  idxTrainValid <- c(idxTrain,idxValid)
  idxTest <- sample(nrow(DQImbABCImbPlus.normByPeak.noTrain.noValid),nTest)
  
  
  trainData       <- DQImbABCImbPlus.normByPeak[idxTrain,]
  validationData  <- DQImbABCImbPlus.normByPeak.noTrain[idxValid,]
  testData        <- DQImbABCImbPlus.normByPeak.noTrain.noValid[idxTest,]
  
  notInGrp <- c("ABC.label", "DQ.label", "sourceLabel", "DQ._class", "ABC._class", "_class", "label","class","experience")
  predictors <- c("ABC.current.rmssd" , "ABC.current.rmsR" , "ABC.current.rmsS" ,"ABC.current.rmsT"  ,"ABC.current.meanRMS" , "ABC.voltage.rmssd" , "ABC.voltage.rmsR"   ,   "ABC.voltage.rmsS","ABC.voltage.rmsT"  , "ABC.voltage.meanRMS"  , "ABC.positiveSeq" , "ABC.current.rmssd.avg","ABC.current.rmsR.avg" , "ABC.current.rmsS.avg" , "ABC.current.rmsT.avg" , "ABC.voltage.rmssd.avg"
                  ,"ABC.voltage.rmsR.avg" , "ABC.voltage.rmsS.avg"  ,"ABC.voltage.rmsT.avg" , "DQ.current.score" ,"DQ.current.score.avg" , "DQ.voltage.score" , "DQ.voltage.score.avg")
  #predictors <- names(DQImbABCImbPlus.normByPeak)[!(names(DQImbABCImbPlus.normByPeak) %in% notInGrp)]
  target <- c("DQ._class")
  
  trainX <- trainData[,predictors]
  trainY <- trainData[,target]
  
  # trainX <- rbind(trainData[,predictors], validationData[,predictors])
  # trainY <- c(trainData[,target], validationData[,target])
  
  ## See http://topepo.github.io/caret/training.html#metrics
  f1 <- function(data, lev = NULL, model = NULL) {
    f1_val <- MLmetrics::F1_Score(y_pred = data$pred, y_true = data$obs, positive="Faulty")
    c(F1 = f1_val)
  }
  
  ## BINARY
  ## Seed: 59, 93, 131, 206, 252, 285, 298, 299, 310
  set.seed(356)
  model <- caret::train(
    x=trainX, y=trainY
    , method = "parRF"
    , tuneGrid = data.frame(mtry= c(2))
    #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
    , metric = "F1"
    , trControl = caret::trainControl(
      method = "cv",
      number = 10,
      classProbs = T,
      allowParallel = T,
      returnData = F
      , summaryFunction = f1
    )
    , preProcess = c("BoxCox")
  )

  modelPredV <- predict(model, validationData[,predictors])
  u = sort(union(modelPredV, validationData[,target]))
  modelStatsV <- caret::confusionMatrix(data = factor(modelPredV, u), reference = factor(validationData[,target], u))
  
  modelPredT <- predict(model, testData[,predictors])
  u = sort(union(modelPredT, testData[,target]))
  modelStatsT <- caret::confusionMatrix(data = factor(modelPredT, u), reference = factor(testData[,target], u))
  
  out <- list(modelPredV,
              modelPredT,
              modelStatsV,
              modelStatsT)
  names(out) <- c("modelPredV", "modelPredT","modelStatsV","modelStatsT")
  out
  
}


#----WIP : metric kappa ---------------------------------------------------------------
#
# predictors <- names(DQImbABCImbPlus.normByPeak)[!(names(DQImbABCImbPlus.normByPeak) %in% notInGrp)]
#   target <- c("DQ._class")
#   
#   trainX <- trainData[,predictors]
#   trainY <- trainData[,target]
#   
#   set.seed(59)
#   model <- caret::train(
#     x=trainX, y=trainY
#     ,method = "parRF"
#     , tuneGrid = data.frame(mtry= c(2))
#     #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
#     ,metric = "Kappa"
#     ,trControl = caret::trainControl(
#       method = "cv",
#       number = 4,
#       classProbs = T,
#       allowParallel = T,
#       returnData = F
#       #, summaryFunction = f1
#     )
#     ,preProcess = c("BoxCox")
#   )
#   
#   modelPredV <- predict(model, validationData[,predictors])
#   u = sort(union(modelPredV, validationData[,target]))
#   modelStatsV <- caret::confusionMatrix(data = factor(modelPredV, u), reference = factor(validationData[,target], u))
#   
#   modelPredT <- predict(model, testData[,predictors])
#   u = sort(union(modelPredT, testData[,target]))
#   modelStatsT <- caret::confusionMatrix(data = factor(modelPredT, u), reference = factor(testData[,target], u))


start_Eval_Model_C_Fault_WithTime  <- function(list_SourceLabel,DQImbABCImbPlus.normByPeak ) {

  DQImbABCImbPlus.normByPeak$sourceLabel <- list_SourceLabel[as.numeric(rownames(DQImbABCImbPlus.normByPeak))]
  p.train <- 0.5
  p.valid <- 0.2
  p.test <- 0.3
  
  DQImbABCImbPlus.normByPeak$experience <- unlist(lapply(
    DQImbABCImbPlus.normByPeak$ABC.label,
    function(label){
      strsplit(as.character(label), " - Entity")[[1]][1]
    }
  ))
  
  
  DQImbABCImbPlus.normByPeak.ordered <- DQImbABCImbPlus.normByPeak[order(DQImbABCImbPlus.normByPeak$sourceLabel),]
  uniqueExperiences <- unique(DQImbABCImbPlus.normByPeak$experience)
  
  DQImbABCImbPlus.normByPeak.ordered$DSindx <- rep(0,nrow(DQImbABCImbPlus.normByPeak.ordered))
  
  for(i in 1:length(uniqueExperiences)){
    positions <- which(DQImbABCImbPlus.normByPeak.ordered$experience == uniqueExperiences[i])
    idxStart <- min(positions)
    lengthExp <- length(positions)
    nTrain <- round(lengthExp*p.train)
    nValid <- round(lengthExp*p.valid)
    validStart <- idxStart+nTrain
    nTtest <- lengthExp-(nTrain+nValid)
    testStart <- validStart + nValid
    
    DQImbABCImbPlus.normByPeak.ordered[idxStart:(idxStart+nTrain-1),"DSindx"] <- rep(1,nTrain)
    DQImbABCImbPlus.normByPeak.ordered[validStart:(validStart+nValid-1),"DSindx"] <- rep(2,nValid)
    DQImbABCImbPlus.normByPeak.ordered[testStart:(testStart+nTtest-1),"DSindx"] <- rep(3,nTtest)
    
  }
  
  trainData <- DQImbABCImbPlus.normByPeak.ordered[DQImbABCImbPlus.normByPeak.ordered$DSindx == 1,]
  validationData <- DQImbABCImbPlus.normByPeak.ordered[DQImbABCImbPlus.normByPeak.ordered$DSindx == 2,]
  testData <- DQImbABCImbPlus.normByPeak.ordered[DQImbABCImbPlus.normByPeak.ordered$DSindx == 3,]
  notInGrp <- c("ABC.label", "DQ.label", "sourceLabel", "DQ._class", "ABC._class", "_class", "label","class","experience","DSindx")
  predictors <- names(DQImbABCImbPlus.normByPeak.ordered)[!(names(DQImbABCImbPlus.normByPeak.ordered) %in% notInGrp)]
  target <- c("DQ._class")
  
  trainX <- trainData[,predictors]
  trainY <- trainData[,target]
  
  trainX <- rbind(trainData[,predictors], validationData[,predictors])
  trainY <- c(trainData[,target], validationData[,target])
  
  set.seed(22)
  model <- caret::train(
    x=trainX, y=trainY
    ,method = "parRF"
    , tuneGrid = data.frame(mtry= c(2))
    # , tuneGrid = data.frame(C= c(1), sigma=c(0.1))
    ,metric = "Kappa"
    ,trControl = caret::trainControl(
      method = "cv",
      number = 4,
      classProbs = T,
      allowParallel = T,
      returnData = F
    )
    ,preProcess = c("BoxCox")
  )
  
  modelPredV <- predict(model, validationData[,predictors])
  u = sort(union(modelPredV, validationData[,target]))
  modelStatsV <- caret::confusionMatrix(data = factor(modelPredV, u), reference = factor(validationData[,target], u))
  
  modelPredT <- predict(model, testData[,predictors])
  u = sort(union(modelPredT, testData[,target]))
  modelStatsT <- caret::confusionMatrix(data = factor(modelPredT, u), reference = factor(testData[,target], u))
  
  failedV <- modelPredV != validationData[,target]
  #validationData[,c("ABC.label")]
  validationData[failedV,c("ABC.label")]
  
  failedT <- modelPredT != testData[,target]
  #testData[,c("ABC.label")]
  testData[failedT,c("ABC.label")]
  
  out <- list(modelPredV,
              modelPredT,
              modelStatsV,
              modelStatsT)
  names(out) <- c("modelPredV", "modelPredT","modelStatsV","modelStatsT")
  out
}



start_Eval_Model_Classifier_ShortCircuit  <- function(list_SourceLabel,DQImbABCImbPlus.normByPeak ) {
  
  
  DQImbABCImbPlus.normByPeak$sourceLabel <- list_SourceLabel[as.numeric(rownames(DQImbABCImbPlus.normByPeak))]
  DQImbABCImbPlus.normByPeak.ordered     <- DQImbABCImbPlus.normByPeak[order(DQImbABCImbPlus.normByPeak$sourceLabel),]
  
  modelingPhaseData     <- DQImbABCImbPlus.normByPeak.ordered[DQImbABCImbPlus.normByPeak.ordered$sourceLabel < "2017-02-13 10:43:18",]
  modelingPhaseTestData <- DQImbABCImbPlus.normByPeak.ordered[DQImbABCImbPlus.normByPeak.ordered$sourceLabel >= "2017-02-13 10:43:18",]
  notInGrp              <- c("ABC.label", "DQ.label", "sourceLabel", "DQ._class", "ABC._class", "_class", "label","class")
  predictors            <- names(modelingPhaseData)[!(names(modelingPhaseData) %in% notInGrp)]
  target <- c("DQ._class")
  

  
  set.seed(22)
  model <- caret::train(
    x=modelingPhaseData[,predictors], y=modelingPhaseData[,target]
    ,method = "parRF"#as.character(currSetting$method
    , tuneGrid = data.frame(mtry= c(2))
    #,tuneGrid = currSetting$parameters
    ,metric = "F1"
    ,trControl = caret::trainControl(
      method = "cv",
      number = 4,
      classProbs = T,
      allowParallel = T,
      returnData = F
      #summaryFunction = f1
    )
    ,preProcess = c("BoxCox")
  )
  
  
  modelPredV <- predict(model, modelingPhaseTestData[,predictors])
  u = sort(union(modelPredV, modelingPhaseTestData[,target]))
  
  failed <- modelPredV != modelingPhaseTestData[,target]
  
  modelingPhaseTestData[failed,c("ABC.label")]
  
  modelStatsV <- caret::confusionMatrix(data = factor(modelPredV, u), reference = factor(modelingPhaseTestData[,target], u), positive="Faulty")
  
  modelPredT <- predict(model, modelingPhaseTestData [,predictors])
  u = sort(union(modelPredT, modelingPhaseTestData [,target]))
  modelStatsT <- caret::confusionMatrix(data = factor(modelPredT, u), reference = factor(modelingPhaseTestData[,target], u), positive="Faulty")
  
  out <- list(modelPredV,
              modelPredT,
              modelStatsV,
              modelStatsT)
  names(out) <- c("modelPredV", "modelPredT","modelStatsV","modelStatsT")
  out
  
}




#REGRESSION!!!!!!!!!!!!!!!!!!!!!
start_Eval_Model_Regresssion_SeverityProb <- function(data,DQImbABCImbPlus.normByRMS,DQImbABCImbPlus.normByPeak ) {


    healthyData <- data[data$DQ._class == "Healthy", ]
    print("healthyData")
    print(healthyData)
    healthyData.sample <- sample(nrow(healthyData),nrow(healthyData)*0.7)
    predictors <- c("ABC.current.rmssd" , "ABC.current.rmsR" , "ABC.current.rmsS" ,"ABC.current.rmsT"  
                    ,"ABC.current.meanRMS" , "ABC.voltage.rmssd" , "ABC.voltage.rmsR"
                    , "ABC.voltage.rmsS","ABC.voltage.rmsT"  , "ABC.voltage.meanRMS"  
                    , "ABC.current.rmssd.avg","ABC.current.rmsR.avg" , "ABC.current.rmsS.avg" 
                    , "ABC.current.rmsT.avg" , "ABC.voltage.rmssd.avg"
                    ,"ABC.voltage.rmsR.avg" , "ABC.voltage.rmsS.avg"  ,"ABC.voltage.rmsT.avg" 
                    , "DQ.current.score" ,"DQ.current.score.avg" , "DQ.voltage.score" 
                    , "DQ.voltage.score.avg")
    
    notInGrp <- c("DQ.voltage.pc2y","DQ.voltage.pc2x","DQ.current.pc2x","DQ.current.pc2y","ABC.positiveSeq","X","currentSeverity","turnsSeverity","target_noise","ABC.label", "DQ.label", "sourceLabel", "DQ._class", "DQ._target", "ABC._target", "ABC._class","_class", "label","class","experience")
    # notInGrp <- c("X","currentSeverity","turnsSeverity","target_noise","ABC.label", "DQ.label", "sourceLabel", "DQ._class", "DQ._target", "ABC._target", "ABC._class","_class", "label","class","experience")
    # predictors <- c("ABC.current.rmssd" , "ABC.current.rmsR" , "ABC.current.rmsS" ,"ABC.current.rmsT"  ,"ABC.current.meanRMS" , "ABC.voltage.rmssd" , "ABC.voltage.rmsR"   ,   "ABC.voltage.rmsS","ABC.voltage.rmsT"  , "ABC.voltage.meanRMS"  , "ABC.positiveSeq" , "ABC.current.rmssd.avg","ABC.current.rmsR.avg" , "ABC.current.rmsS.avg" , "ABC.current.rmsT.avg" , "ABC.voltage.rmssd.avg"
    #                 ,"ABC.voltage.rmsR.avg" , "ABC.voltage.rmsS.avg"  ,"ABC.voltage.rmsT.avg" , "DQ.current.score" ,"DQ.current.score.avg" , "DQ.voltage.score" , "DQ.voltage.score.avg")
    
    predictors <- names(DQImbABCImbPlus.normByPeak)[!(names(DQImbABCImbPlus.normByPeak) %in% notInGrp)]
    
    predictors.add <- c()
    
    predictors <- c(predictors, predictors.add)

    
    percTrain <- 0.5
    percValid <- 0.2
    percTest <- 0.3 
    
    nTrain <- round(nrow(data)*percTrain)
    nValidation <- round(nrow(data)*percValid)
    nTest <- nrow(data)-(nTrain+nValidation)
    
    idxTrain <- sample(nrow(data),nTrain)
    data.noTrain <- data[-idxTrain,]
    idxValid <- sample(nrow(data.noTrain),nValidation)
    data.noTrain.noValid <- data.noTrain[-idxValid,]
    idxTrainValid <- c(idxTrain,idxValid)
    idxTest <- sample(nrow(data.noTrain.noValid),nTest)
    
    trainData <- data[idxTrain,]
    validationData <- data.noTrain[idxValid,]
    testData <- data.noTrain.noValid[idxTest,]

    target <- c("DQ._target")

    trainX <- trainData[,predictors]

    trainY <- trainData[,target]

    print(trainY)
    
    set.seed(1205)
    model_2 <- caret::train(
      x=trainX, y=trainY
      ,method = "kknn"
      , tuneGrid = data.frame(
        kmax = 49
        , distance = 0.5
        , kernel = "rectangular"
      )
      #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
      ,metric = "MAE"
      ,trControl = caret::trainControl(
        method = "boot",
        number = 25,
        allowParallel = T,
        returnData = F
        , summaryFunction = mae <- function(data, lev = NULL, model = NULL){
          mae_val <- MLmetrics::MAE(y_pred = data$pred, y_true = data$obs)*-1
          c(MAE = mae_val)
        }
      )
      ,preProcess = c("spatialSign")
    )
    
    cubist <- caret::train(
      x=trainX, y=trainY
      ,method = "cubist"
      , tuneGrid = data.frame(committees= c(31), neighbors = c(5))
      #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
      ,metric = "MAE"
      ,trControl = caret::trainControl(
        method = "boot",
        number = 50,
        allowParallel = T,
        returnData = F
        , summaryFunction = mae <- function(data, lev = NULL, model = NULL){
          mae_val <- MLmetrics::MAE(y_pred = data$pred, y_true = data$obs)*-1
          c(MAE = mae_val)
        }
      )
      ,preProcess = c("center")
    )
    
    model_2.prediction.train <- predict(model_2, trainData[,predictors])
    model_2.prediction.validation <- predict(model_2, validationData[,predictors])
    model_2.prediction.test <- predict(model_2, testData[,predictors])
    
    model_2.performance.train.mae <- MLmetrics::MAE(model_2.prediction.train, trainData[,target])
    model_2.performance.validation.mae <- MLmetrics::MAE(model_2.prediction.validation, validationData[,target])
    model_2.performance.test.mae <- MLmetrics::MAE(model_2.prediction.test, testData[,target])
    
    model_2.performance.validation.rmsle <- MLmetrics::RMSLE(model_2.prediction.validation, validationData[,target])
    model_2.performance.test.rmsle <- MLmetrics::RMSLE(model_2.prediction.test, testData[,target])
    
    model_2.performance.validation.r2 <- MLmetrics::R2_Score(model_2.prediction.validation, validationData[,target])
    model_2.performance.test.r2 <- MLmetrics::R2_Score(model_2.prediction.test, testData[,target])
    
    
    cubist.prediction.train <- predict(cubist, trainData[,predictors])
    cubist.prediction.validation <- predict(cubist, validationData[,predictors])
    cubist.prediction.test <- predict(cubist, testData[,predictors])
    
    cubist.performance.train.mae <- MLmetrics::MAE(cubist.prediction.train, trainData[,target])
    cubist.performance.validation.mae <- MLmetrics::MAE(cubist.prediction.validation, validationData[,target])
    cubist.performance.test.mae <- MLmetrics::MAE(cubist.prediction.test, testData[,target])
    
    cubist.performance.validation.rmsle <- MLmetrics::RMSLE(cubist.prediction.validation, validationData[,target])
    cubist.performance.test.rmsle <- MLmetrics::RMSLE(cubist.prediction.test, testData[,target])
    
    cubist.performance.validation.r2 <- MLmetrics::R2_Score(cubist.prediction.validation, validationData[,target])
    cubist.performance.test.r2 <- MLmetrics::R2_Score(cubist.prediction.test, testData[,target])
    
    
    validationResults <- data.frame(
      truth = validationData[, target],
      predict = model_2.prediction.validation,
      label = validationData[, "ABC.label"]
    )
    
    validationResults.sort <- validationResults[order(validationResults$truth),]
    
    plot_ly(
      x = 1:nrow(validationResults.sort)
      , y = validationResults.sort$predict
      , type="scatter"
      , mode="markers"
      , name="predicted"
      , text = validationResults.sort$label
    ) %>% add_markers(
      x= 1:nrow(validationResults.sort)
      , y = validationResults.sort$truth
      , name = "truth"
    ) 
    
    #KNN Validation
    validAndTest <- rbind(validationData[, c(predictors,target,"ABC.label","sourceLabel")], testData[, c(predictors,target,"ABC.label","sourceLabel")])
    cubist.prediction.valAndTest <- predict(cubist, validAndTest[, predictors])
    
    cubist.validationResults <- data.frame(
      truth = validAndTest[, target],
      predict = cubist.prediction.valAndTest,
      label = validAndTest[,"ABC.label"],
      sourceLabel = validAndTest[,"sourceLabel"]
    )
    
    cubist.validationResults.sort <- cubist.validationResults[order(cubist.validationResults$truth),]
    
  
    #KNN Validation
    knn.prediction.valAndTest <- predict(model_2, validAndTest[, predictors])
    
    knn.validationResults <- data.frame(
      truth = validAndTest[, target],
      predict = knn.prediction.valAndTest,
      label = validAndTest[,"ABC.label"],
      sourceLabel = validAndTest[,"sourceLabel"]
    )
    
    knn.validationResults.sort <- knn.validationResults[order(knn.validationResults$truth),]
 
    
    out <- list(knn.validationResults.sort,
                cubist.validationResults.sort)
    names(out) <- c("knn.validationResults.sort", 
                    "cubist.validationResults.sort")
    out
   
    
}



#---- WIP: SeverityEstimation----------------------------------------------------------------


# start_Eval_Model_Regresssion_SeverityEstimation  <- function(list_SourceLabel,DQImbABCImbPlus.normByPeak ) {
# 
#   percTrain <- 0.5
#   percValid <- 0.2
#   percTest <- 0.3 
# 
#   nTrain <- round(nrow(DQImbABCImbPlus.normByPeak)*percTrain)
#   nValidation <- round(nrow(DQImbABCImbPlus.normByPeak)*percValid)
#   nTest <- nrow(DQImbABCImbPlus.normByPeak)-(nTrain+nValidation)
# 
#   idxTrain <- sample(nrow(DQImbABCImbPlus.normByPeak),nTrain)
#   idxValid <- sample(nrow(DQImbABCImbPlus.normByPeak[-idxTrain,]),nValidation)
#   idxTrainValid <- c(idxTrain,idxValid)
#   idxTest <- sample(nrow(DQImbABCImbPlus.normByPeak[-idxTrainValid,]),nTest)
# 
#   DQImbABCImbPlus.normByPeak$target_noise <- DQImbABCImbPlus.normByPeak$DQ._target + rnorm(n = nrow(DQImbABCImbPlus.normByPeak),mean = 0, sd = 0.05)
#   
#   
#   trainData <- DQImbABCImbPlus.normByPeak[idxTrain,]
#   validationData <- DQImbABCImbPlus.normByPeak[idxValid,]
#   testData <- DQImbABCImbPlus.normByPeak[idxTest,]
#   
#   notInGrp <- c("ABC.positiveSeq","X","currentSeverity","turnsSeverity","target_noise","ABC.label", "DQ.label", "sourceLabel", "DQ._class", "DQ._target", "ABC._target", "ABC._class","_class", "label","class","experience")
# 
#   predictors <- names(DQImbABCImbPlus.normByPeak)[!(names(DQImbABCImbPlus.normByPeak) %in% notInGrp)]
#   target <- c("DQ._target")
# 
#   trainX <- trainData[,predictors]
#   trainY <- trainData[,target]
# 
#   ## BINARY
#   ## Seed: 59, 93, 131, 206, 252, 285, 298, 299, 310
#   
#   set.seed(1205)
#   model_2 <- caret::train(
#     x=trainX, y=trainY
#     ,method = "kknn"
#     , tuneGrid = data.frame(
#       kmax = 49
#       , distance = 0.5
#       , kernel = "rectangular"
#     )
#     #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
#     ,metric = "MAE"
#     ,trControl = caret::trainControl(
#       method = "boot",
#       number = 25,
#       allowParallel = T,
#       returnData = F
#       , summaryFunction = mae <- function(data, lev = NULL, model = NULL){
#         mae_val <- MLmetrics::MAE(y_pred = data$pred, y_true = data$obs)*-1
#         c(MAE = mae_val)
#       }
#     )
#     ,preProcess = c("spatialSign")
#   )
#   
#   print("cheguei")
#   
#   knn.rmsle <- caret::train(
#     x=trainX, y=trainY
#     ,method = "kknn"
#     , tuneGrid = data.frame(
#       kmax = 49
#       , distance = 0.5
#       , kernel = "rectangular"
#     )
#     #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
#     ,metric = "RMSLE"
#     ,trControl = caret::trainControl(
#       method = "boot",
#       number = 25,
#       allowParallel = T,
#       returnData = F
#       , summaryFunction = mae <- function(data, lev = NULL, model = NULL){
#         mae_val <- MLmetrics::RMSLE(y_pred = data$pred, y_true = data$obs)*-1
#         c(RMSLE = mae_val)
#       }
#     )
#     ,preProcess = c("spatialSign")
#   )
#   
#   #Evaluation
#   
#   model_2.prediction.validation <- predict(model_2, validationData[,predictors])
#   model_2.prediction.test <- predict(model_2, testData[,predictors])
#   
#   model_2.performance.validation.mape <- MLmetrics::MAPE(model_2.prediction.validation, validationData[,target])
#   model_2.performance.test.mape <- MLmetrics::MAxPE(model_2.prediction.test, testData[,target])
#   
#   model_2.performance.validation.rmsle <- MLmetrics::RMSLE(model_2.prediction.validation, validationData[,target])
#   model_2.performance.test.rmsle <- MLmetrics::RMSLE(model_2.prediction.test, testData[,target])
#   
#   
#   plot_ly(
#     x = 1:length(validationData[, target])
#     , y = c(predict(model_2, validationData[,predictors]))
#     , type="scatter"
#     , mode="markers"
#     , name="predicted"
#     , text = validationData[, "ABC.label"]
#   ) %>% add_markers(
#     x= 1:length(validationData[, "DQ._target"])
#     , y = validationData[, "DQ._target"]
#     , name = "truth"
#   ) 
#   
#   plot_ly(
#     x = 1:length(validationData[, target])
#     , y = c(predict(knn.rmsle, validationData[,predictors]))
#     , type="scatter"
#     , mode="markers"
#     , name="predicted"
#     , text = validationData[, "ABC.label"]
#   ) %>% add_markers(
#     x= 1:length(validationData[, "DQ._target"])
#     , y = validationData[, "DQ._target"]
#     , name = "truth"
#   ) 
#   
#   
#   set.seed(59)
#   model_3 <- caret::train(
#     x=trainX, y=trainY
#     ,method = "kknn"
#     #, tuneGrid = data.frame(mtry= c(2))
#     #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
#     #,metric = "F1"
#     ,trControl = caret::trainControl(
#       method = "cv",
#       number = 10,
#       allowParallel = T,
#       returnData = F
#       #, summaryFunction = f1
#     )
#     ,preProcess = c("spatialSign")
#   )
#   
#   plot_ly(
#     x = 1:length(testData[, target])
#     , y = c(predict(model_3, testData[,predictors]))
#     , type="scatter"
#     , mode="markers"
#     , name="predicted"
#     , text = testData[, "ABC.label"]
#   ) %>% add_markers(
#     x= 1:length(testData[, "DQ._target"])
#     , y = testData[, "DQ._target"]
#     , name = "truth"
#   ) 
#   
#   set.seed(59)
#   model_3 <- caret::train(
#     x=trainX, y=trainY
#     ,method = "kknn"
#     , tuneGrid = data.frame(kernel= c("cos"), distance = c(2), kmax = c(20))
#     , tuneLength = 8
#     #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
#     #,metric = "F1"
#     ,trControl = caret::trainControl(
#       method = "cv",
#       number = 10,
#       allowParallel = T,
#       returnData = F
#       #, summaryFunction = f1
#     )
#     ,preProcess = c("center","scale")
#   )
#   
#   plot_ly(
#     x = 1:length(testData[, target])
#     , y = c(predict(model_3, testData[,predictors]))
#     , type="scatter"
#     , mode="markers"
#     , name="predicted"
#     , text = testData[, "ABC.label"]
#   ) %>% add_markers(
#     x= 1:length(testData[, "DQ._target"])
#     , y = testData[, "DQ._target"]
#     , name = "truth"
#   ) 
#   
#   
#   set.seed(59)
#   model_4 <- caret::train(
#     x=trainX, y=trainY
#     ,method = "kknn"
#     , tuneGrid = data.frame(kernel= c("gaussian"), distance = c(1), kmax = c(31))
#     # , tuneLength = 8
#     #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
#     #,metric = "F1"
#     ,trControl = caret::trainControl(
#       method = "cv",
#       number = 10,
#       allowParallel = T,
#       returnData = F
#       #, summaryFunction = f1
#     )
#     ,preProcess = c("center","scale")
#   )
#   
#   plot_ly(
#     x = 1:length(testData[, target])
#     , y = c(predict(model_4, testData[,predictors]))
#     , type="scatter"
#     , mode="markers"
#     , name="predicted"
#     , text = testData[, "ABC.label"]
#   ) %>% add_markers(
#     x= 1:length(testData[, "DQ._target"])
#     , y = testData[, "DQ._target"]
#     , name = "truth"
#   ) 
#   
#   DQImbABCImbPlus.normByPeak$turnsSeverity <- 1
#   DQImbABCImbPlus.normByPeak$currentSeverity <- 1
#   DQImbABCImbPlus.normByPeak <- dplyr::mutate(DQImbABCImbPlus.normByPeak,
#                                               turnsSeverity = ifelse(DQ._target == 0.396 | DQ._target == 0.333 | DQ._target == 0.271 | DQ._target == 0.239 | DQ._target == 0.115, 50, 25)
#   )
#   DQImbABCImbPlus.normByPeak <- dplyr::mutate(DQImbABCImbPlus.normByPeak,
#                                               turnsSeverity = ifelse(DQ._target == 0, 0, turnsSeverity)
#   )
#   
#   DQImbABCImbPlus.normByPeak <- dplyr::mutate(DQImbABCImbPlus.normByPeak,
#                                               currentSeverity = ifelse(DQ._target == 0.396, 2.5, currentSeverity)
#   )
#   DQImbABCImbPlus.normByPeak <- dplyr::mutate(DQImbABCImbPlus.normByPeak,
#                                               currentSeverity = ifelse(DQ._target == 0.354, 2.5, currentSeverity)
#   )
#   DQImbABCImbPlus.normByPeak <- dplyr::mutate(DQImbABCImbPlus.normByPeak,
#                                               currentSeverity = ifelse(DQ._target == 0.333 , 2.0, currentSeverity)
#   )
#   DQImbABCImbPlus.normByPeak <- dplyr::mutate(DQImbABCImbPlus.normByPeak,
#                                               currentSeverity = ifelse(DQ._target == 0.271 , 1.5, currentSeverity)
#   )
#   DQImbABCImbPlus.normByPeak <- dplyr::mutate(DQImbABCImbPlus.normByPeak,
#                                               currentSeverity = ifelse(DQ._target == 0.239 , 1.25, currentSeverity)
#   )
#   DQImbABCImbPlus.normByPeak <- dplyr::mutate(DQImbABCImbPlus.normByPeak,
#                                               currentSeverity = ifelse(DQ._target == 0.115 , 0.25, currentSeverity)
#   )
#   DQImbABCImbPlus.normByPeak <- dplyr::mutate(DQImbABCImbPlus.normByPeak,
#                                               currentSeverity = ifelse(DQ._target == 0 , 0, currentSeverity)
#   )
#   
#   
#   target <- c("currentSeverity","turnsSeverity")
#   
#   trainY <- trainData[,target]
#   
#   ## BINARY
#   ## Seed: 59, 93, 131, 206, 252, 285, 298, 299, 310
#   
#   set.seed(59)
#   model_2 <- caret::train(
#     x=trainX, y=trainY
#     ,method = "cubist"
#     #, tuneGrid = data.frame(mtry= c(2))
#     #, tuneGrid = data.frame(C= c(1), sigma=c(0.1))
#     #,metric = "F1"
#     ,trControl = caret::trainControl(
#       method = "cv",
#       number = 4,
#       allowParallel = T,
#       returnData = F
#       #, summaryFunction = f1
#     )
#     ,preProcess = c("BoxCox")
#   )
#   
# }
# 
# 




      