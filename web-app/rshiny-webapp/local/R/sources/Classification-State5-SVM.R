#MAIN
#______________________________________________________________________
#||Description: Classificafor for phase-in-short-circuit detection
#||Author: Joao Henriques
#||Reference work & acknowledgements: Tiago dos Santos
#________________________________________________________________________

library(rstudioapi)    

source(paste0(dirname((rstudioapi::getActiveDocumentContext()$path)),"/crisp-dm/Utilities.R"))
source(paste0(dirname((rstudioapi::getActiveDocumentContext()$path)),"/crisp-dm/data_preparation.R"))
source(paste0(dirname((rstudioapi::getActiveDocumentContext()$path)),"/crisp-dm/data_modeling.R"))
source(paste0(dirname((rstudioapi::getActiveDocumentContext()$path)),"/crisp-dm/data_evaluation.R"))

#______________________________________________________________________
##########################DATA PREPARATION##############################
#-------------------------________________------------------------------
  
G_ident <- ""
files <- c("db_noload_healthy.json","db_noload_scA.json", "db_noload_scB.json", "db_noload_scC.json", 'db_noload_scA_descSCCurrent.json')
names <- c("Healthy","SC A 2.5A","SC B 2.5A","SC C 2.5A", 'SC A diff sc A')

colors = brewer.pal(length(files), "Set1")
ellipse.colors = brewer.pal(length(files), "Set2")

res               <- read_data(PROJHOME, input_folder_tip, files) 

res <- lapply(
  seq(res),
  function(idx, res, colors){
    res[[idx]]$color <- colors[[idx]]
    return(res[[idx]])
  },
  res = res,
  colors = colors
)
  
data.current.dataParkTransform <- lapply(
    res,
    function(el){
      rearmSignalProcessing::process_park_transform(
        dataset = el$dataToAnalyzeCurated, motorNominalRMS = el$motorCurrentRMSs, 
        label = el$datasetLabels, color = el$color
      )
    }
 )

data.current.EigenTransform <- lapply(
  seq_along(data.current.dataParkTransform),
  function(idx, ds, commonInfo){
    signals <- lapply(
      ds[[idx]],
      function(el){
        el$signal
      }
    )
    rearmParkAnalysis::eigenTransform(
      signals, commonInfo[[idx]]$datasetLabels, commonInfo[[idx]]$color,
      list2plot = T, denormalizePC = T
    )
  },
  ds = data.current.dataParkTransform,
  commonInfo = res
)

outliers.score.idx <- lapply(
  data.current.EigenTransform,
  function(ds){
    scores <- unlist(lapply(
      ds,
      function(el){
        el$score
      }
    ))
    outliers <- boxplot.stats(scores)$out
    outliers.idxs <- which(scores %in% outliers)
    return(
      outliers.idxs
    )
  }
)

res.noOut <- lapply(
  seq_along(res),
  function(idx, commonInfo, outliers.idx){
    if(length(outliers.idx[[idx]])>0){
      commonInfo[[idx]]$datasetLabels <- commonInfo[[idx]]$datasetLabels[-outliers.idx[[idx]]]
      commonInfo[[idx]]$sampledFrequencies <- commonInfo[[idx]]$sampledFrequencies[-outliers.idx[[idx]]]
      commonInfo[[idx]]$dataToAnalyzeCurated <- commonInfo[[idx]]$dataToAnalyzeCurated[-outliers.idx[[idx]]]
      commonInfo[[idx]]$classes <- commonInfo[[idx]]$classes[-outliers.idx[[idx]]]
      commonInfo[[idx]]$motorCurrentRMSs <- commonInfo[[idx]]$motorCurrentRMSs[-outliers.idx[[idx]]]
      commonInfo[[idx]]$motorVoltageRMSs <- commonInfo[[idx]]$motorVoltageRMSs[-outliers.idx[[idx]]]
    }
    return(commonInfo[[idx]])
  },
  commonInfo = res,
  outliers.idx = outliers.score.idx
)

data.positiveSequence <- unlist(lapply(
  seq_along(res.noOut),
  function(idx, commonInfo){
    ds <- commonInfo[[idx]]$dataToAnalyzeCurated
    lapply(
      ds,
      function(el){
        idxSeq <- (which.max(el$currentPhaseR) - 2)%%length(el$currentPhaseR)
        ifelse(idxSeq ==0, idxSeq <- length(el$currentPhaseR), idxSeq <- idxSeq)
        res <- NULL
        ifelse(el$currentPhaseS[idxSeq] > el$currentPhaseT[idxSeq], res <- 1, res <- 0)
        return(res)
      }
    )
    
  },
  commonInfo = res.noOut
))

data.currents.minmax <- lapply(
  res.noOut,
  function(ds){
    lapply(
      ds$dataToAnalyzeCurated,
      function(el){
        return(
          data.frame(
            maxR = max(el$currentPhaseR),
            maxS = max(el$currentPhaseS),
            maxT = max(el$currentPhaseT),
            minR = min(el$currentPhaseR),
            minS = min(el$currentPhaseS),
            minT = min(el$currentPhaseT)
          )
        )
      }
    )
  }
)

data.current.EigenTransform.noOutliers <- lapply(
  seq_along(data.current.EigenTransform),
  function(idx, eigen, outliers.idx){
    if(length(outliers.idx[[idx]])>0){
      data.current.EigenTransform[[idx]] <- data.current.EigenTransform[[idx]][-outliers.idx[[idx]]]
    }
    return(data.current.EigenTransform[[idx]])
  },
  eigen = data.current.EigenTransform,
  outliers.idx = outliers.score.idx
)


#______________________________________________________________________
##########################DATA MODELING && EVALUATION##############################
#-------------------------___________________________-------------------------------

evalModel <- function(dataset, classifierColumns, classifierGroundTruth, listTrainTestConfig = NULL, missmatchTable = T){
  if(is.null(listTrainTestConfig)){
    n <- nrow(dataset_featured_merged)
    ntrain <- round(n*0.7)
    tindex <- sample(n,ntrain) # indices of training samples
  }else{
    n <- listTrainTestConfig$n
    ntrain <- listTrainTestConfig$ntrain
    tindex <- listTrainTestConfig$tindex
  }
  
  xtrain <- dataset[tindex,classifierColums]
  xtest <- dataset[-tindex,classifierColums]
  ytrain <- dataset[tindex,classifierGroundTruth]
  ytest <- dataset[-tindex,classifierGroundTruth]
  istrain=rep(0,n)
  istrain[tindex]=1
  
  cvCtrl <- caret::trainControl(method = "repeatedcv", repeats = 3)
  svmTune <- caret::train(x = xtrain,
                          y = ytrain,
                          method = "svmRadial",
                          # The default grid of cost parameters go from 2^-2,
                          # 0.5 to 1,
                          # Well fit 9 values in that sequence via the tuneLength
                          # argument.
                          tuneLength = 9,
                          ## Also add options from preProcess here too
                          preProc = c("center", "scale"),
                          trControl = cvCtrl)
  
  svmPred <- predict(svmTune, xtest)
  
  
  u = sort(union(svmPred, ytest))
  xtab <- table(factor(svmPred, u), factor(ytest, u))
  modelStats <- caret::confusionMatrix(xtab)
  
  if(missmatchTable){
    
    which_missclassified <- which(svmPred != ytest)
    
    missclassified_entities <- dataset[-tindex,][which_missclassified,"label"]
    
    missclassifies <- data.frame(
      labels = missclassified_entities,
      predicted = svmPred[which_missclassified],
      truth = ytest[which_missclassified],
      score = dataset[-tindex,][which_missclassified,"score"],
      pc1x = dataset[-tindex,][which_missclassified,"pc1x"],
      pc1y = dataset[-tindex,][which_missclassified,"pc1y"]
    )
  }
  
  
  confusion_matrix <- knitr::kable(
    modelStats$table,
    booktabs = TRUE,
    caption = 'Confusion Matrix of the model RMS3P.')
  
  
  overal_matrix <- knitr::kable(
    t(modelStats$overall),
    booktabs = TRUE,
    caption = 'Overall status of the model RMS3P.')
  
  if(missmatchTable){
    missclassifies_table <- DT::datatable(missclassifies)
  }else{
    missclassifies_table <- NULL
  }
  
  
  return(
    list(
      confusion_table = confusion_matrix,
      overall_table = overal_matrix,
      missclassifies_table = missclassifies_table,
      model = svmTune
    )
  )
  
}


#----Model-1--------

dataset_featured <-unlist(lapply(
  seq_along(data.current.EigenTransform.noOutliers),
  function(idx, datasets, commonInfo){
    lapply(
      seq_along(datasets[[idx]]),
      function(idx, ds, el_class, labels){
        el <- ds[[idx]]
        return(
          data.frame(
            pc1x = el$pc1$x[2],
            pc1y = el$pc1$y[2],
            pc2x = el$pc2$x[2],
            pc2y = el$pc2$y[2],
            score = el$score,
            class = el_class[[idx]],
            label = labels[[idx]]
          )
        )
      },
      ds = datasets[[idx]],
      el_class = commonInfo[[idx]]$class,
      labels = commonInfo[[idx]]$datasetLabels
    )
  },
  datasets = data.current.EigenTransform.noOutliers,
  commonInfo = res.noOut
), recursive = F)  



dataset_featured_merged <- Reduce(
  function(acc, curr){
    rbind(acc, curr)
  },
  dataset_featured[2:length(dataset_featured)],
  dataset_featured[[1]]
)

classifierColums <- c("pc1x", "pc1y", "pc2x", "pc2y","score")
classifierGroundTruth <- c("class")

listTrainTestConfig <- list(
  n = nrow(dataset_featured_merged),
  ntrain = round(nrow(dataset_featured_merged)*0.7),
  tindex = sample(nrow(dataset_featured_merged),round(nrow(dataset_featured_merged)*0.7))
)


model1 <- evalModel(dataset_featured_merged, classifierColums, classifierGroundTruth, listTrainTestConfig)
model1$confusion_table
model1$overall_table
model1$missclassifies_table


#----Model-2------

classifierColums <- c("pc1x", "pc1y","score")
model2 <- evalModel(dataset_featured_merged, classifierColums, classifierGroundTruth, listTrainTestConfig)
model2$confusion_table
model2$overall_table
model2$missclassifies_table


#----Model-3------


avg.weights <- c(0.5, 0.1,0.1,0.1,0.1,0.1)
dataset_featured <-lapply(
  seq_along(data.current.EigenTransform.noOutliers),
  function(idx, datasets, commonInfo){
    ds <- datasets[[idx]]
    lengthDs = length(ds)
    attr.pc1x <- c()
    attr.pc1y <- c()
    attr.score <- c()
    attr.pc1x.avg <- c()
    attr.pc1y.avg <- c()
    attr.score.avg <- c()
    attr.class <- c()
    attr.desc <- c()
    # experiemtnar abordagem onde retiro os 6 primeiros pontos, porque n tenho as médias paras eles
    for(i in 1:lengthDs){
      el <- ds[[i]]
      avgs = unlist(lapply(
        seq(i-1,i-length(avg.weights),by=-1),
        # sample(lengthDs,length(avg.weights)),
        function(x){
          v <- x %% lengthDs
          if(v == 0){
            v <- lengthDs
          }
          return(v)
        }
      ), recursive = F)
      pc1x.avg <- 0
      sign.x <- 0
      sign.y <- 0
      pc1y.avg <- 0
      score.avg <- 0
      for(j in 1:length(avg.weights)){
        pc1x.avg <- pc1x.avg + abs(ds[avgs[[j]]][[1]]$pc1$x[2]) * avg.weights[j]
        if(ds[avgs[[j]]][[1]]$pc1$x[2] > 0){
          sign.x <- sign.x + 1
        }
        else{
          sign.x <- sign.x - 1
        }
        pc1y.avg <- pc1y.avg + abs(ds[avgs[[j]]][[1]]$pc1$y[2]) * avg.weights[j]
        if(ds[avgs[[j]]][[1]]$pc1$y[2] > 0){
          sign.y <- sign.y + 1
        }
        else{
          sign.y <- sign.y - 1
        }
        score.avg <- score.avg + ds[avgs[[j]]][[1]]$score * avg.weights[j]
      }
      if(sign.x < 0){
        pc1x.avg = pc1x.avg*-1
      }
      if(sign.y < 0){
        pc1y.avg = pc1y.avg*-1
      }
      attr.pc1x <- c(attr.pc1x, el$pc1$x[2])
      attr.pc1y <- c(attr.pc1y, el$pc1$y[2])
      attr.score <- c(attr.score, el$score)
      attr.pc1x.avg <- c(attr.pc1x.avg, pc1x.avg)
      attr.pc1y.avg <- c(attr.pc1y.avg, pc1y.avg)
      attr.score.avg <- c(attr.score.avg, score.avg)
      attr.class <- c(attr.class, commonInfo[[idx]]$class[[i]])
      attr.desc <- c(attr.desc, commonInfo[[idx]]$datasetLabels[[i]])
    }
    return(
      data.frame(
        pc1x = attr.pc1x,
        pc1y = attr.pc1y,
        score = attr.score,
        pc1xavg = attr.pc1x.avg,
        pc1yavg = attr.pc1y.avg,
        scoreavg = attr.score.avg,
        label = attr.desc,
        class = attr.class
      )
    )
  },
  datasets = data.current.EigenTransform.noOutliers,
  commonInfo = res.noOut
)


dataset_featured_merged <- Reduce(
  function(acc, curr){
    rbind(acc, curr)
  },
  dataset_featured[2:length(dataset_featured)],
  dataset_featured[[1]]
)


dataset_featured_merged <- cbind(dataset_featured_merged, data.frame(
  positiveSequence = data.positiveSequence
))

classifierColums <- names(dataset_featured_merged)[names(dataset_featured_merged) != 'class']
classifierColums <- classifierColums[classifierColums!="label"]

model3 <- evalModel(dataset_featured_merged, classifierColums, classifierGroundTruth, listTrainTestConfig)
model3$confusion_table
model3$overall_table
model3$missclassifies_table

#----Model-4------

avg.weights <- c(0.2, 0.2,0.2,0.2,0.2)
dataset_featured <-lapply(
  seq_along(data.current.EigenTransform.noOutliers),
  function(idx, datasets, commonInfo){
    ds <- datasets[[idx]]
    lengthDs = length(ds)
    attr.pc1x <- c()
    attr.pc1y <- c()
    attr.score <- c()
    attr.pc1x.avg <- c()
    attr.pc1y.avg <- c()
    attr.score.avg <- c()
    attr.class <- c()
    attr.desc <- c()
    # experiemtnar abordagem onde retiro os 6 primeiros pontos, porque n tenho as médias paras eles
    for(i in 1:lengthDs){
      el <- ds[[i]]
      avgs = unlist(lapply(
        sample(lengthDs,length(avg.weights)),
        function(x){
          v <- x %% lengthDs
          if(v == 0){
            v <- lengthDs
          }
          return(v)
        }
      ), recursive = F)
      pc1x.avg <- 0
      sign.x <- 0
      sign.y <- 0
      pc1y.avg <- 0
      score.avg <- 0
      for(j in 1:length(avg.weights)){
        pc1x.avg <- pc1x.avg + abs(ds[avgs[[j]]][[1]]$pc1$x[2]) * avg.weights[j]
        if(ds[avgs[[j]]][[1]]$pc1$x[2] > 0){
          sign.x <- sign.x + 1
        }
        else{
          sign.x <- sign.x - 1
        }
        pc1y.avg <- pc1y.avg + abs(ds[avgs[[j]]][[1]]$pc1$y[2]) * avg.weights[j]
        if(ds[avgs[[j]]][[1]]$pc1$y[2] > 0){
          sign.y <- sign.y + 1
        }
        else{
          sign.y <- sign.y - 1
        }
        score.avg <- score.avg + ds[avgs[[j]]][[1]]$score * avg.weights[j]
      }
      if(sign.x < 0){
        pc1x.avg = pc1x.avg*-1
      }
      if(sign.y < 0){
        pc1y.avg = pc1y.avg*-1
      }
      attr.pc1x <- c(attr.pc1x, el$pc1$x[2])
      attr.pc1y <- c(attr.pc1y, el$pc1$y[2])
      attr.score <- c(attr.score, el$score)
      attr.pc1x.avg <- c(attr.pc1x.avg, pc1x.avg)
      attr.pc1y.avg <- c(attr.pc1y.avg, pc1y.avg)
      attr.score.avg <- c(attr.score.avg, score.avg)
      attr.class <- c(attr.class, commonInfo[[idx]]$class[[i]])
      attr.desc <- c(attr.desc, commonInfo[[idx]]$datasetLabels[[i]])
    }
    return(
      data.frame(
        pc1x = attr.pc1x,
        pc1y = attr.pc1y,
        score = attr.score,
        pc1xavg = attr.pc1x.avg,
        pc1yavg = attr.pc1y.avg,
        scoreavg = attr.score.avg,
        label = attr.desc,
        class = attr.class
      )
    )
  },
  datasets = data.current.EigenTransform.noOutliers,
  commonInfo = res.noOut
)


dataset_featured_merged <- Reduce(
  function(acc, curr){
    rbind(acc, curr)
  },
  dataset_featured[2:length(dataset_featured)],
  dataset_featured[[1]]
)


dataset_featured_merged <- cbind(dataset_featured_merged, data.frame(
  positiveSequence = data.positiveSequence
))


classifierColums <- names(dataset_featured_merged)[names(dataset_featured_merged) != 'class']
classifierColums <- classifierColums[classifierColums!="label"]

model4 <- evalModel(dataset_featured_merged, classifierColums, classifierGroundTruth, listTrainTestConfig)
model4$confusion_table
model4$overall_table
model4$missclassifies_table
