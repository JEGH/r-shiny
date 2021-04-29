devtools::use_package("RMySQL")
devtools::use_package("rjson")
devtools::use_package("rearmSignalProcessing")

#' Fetch the data from a rearm DB given a list with the necessery parameters
#'
#' \code{getFromREARMDB} is a function that was made to easily access the monitoring data
#' presented in the database. For example, the InMonitor datalogger generates 32 points per signal per minute,
#' virtually generating 32 rows with 6 measurements each minute, with a timestamp associated.
#' This functions simplifies the fetch of a set of rows by given, between other things, the timestamps.
#'
#' @param dbConfig cannot be null. It is a string with the absolute path of the json file that contains the information to access to a database.
#' @param where is a string containing a mysql `where` condition to fetch the data from the Monitoring table of the REARM database.
#' @param label is a string defining the label to be associated to the set of rows being fetched. It is important to notice that, to each cycle,
#' a sufix will be added to numerate the cycle. For example, if the given label is \"No load Healthy \", the first 20 rows are associated to a cycle and therefore those
#' first 20 rows will have a label associated containing \"No load Healthy - Entity 1\".
#' @param decimatedMagnitude is a numeric parameters. There are cases where some sources present an high data acquisition sample frequency (e.g. 10kHz).
#' This parameters let's you decide the step in which the data is being fetched. For example, supose that you have a cycle obtainded with a sample frequency of 10kHz -
#' which corresponds to 200 rows. If the decimated magnitude parameters value is 1, all the rows will be fetched with a step of 1, returning 200 rows and giving a sample frequency of 10Khz.
#' But if the decimated magnitude value is 2, the data will be fetched with a step of 2 resulting on 100 returning rows, and a simulated sample frequency of 5kHz.
#' @param class is a string containing the desired description of the class to be given to this set of data. Can also be used for multilabeling classification/regression.
#' @param target is a value mainly used for regression problem, where the target value for the dataset is defined. Can also be used for multilabeling classification/regression.
#' @param usemod is a string with one of the three values: \code{classification}, \code{regression}, \code{both}
#' @export
getFromREARMDB <- function(dbConfig = NULL, where = NULL, label = NULL, decimatedMagnitude = 1, class = NULL, target = NULL, usemode = NULL){

  if(is.null(dbConfig)){
    stop("The parameter `dbConfig` cannot be null")
  }

  if(is.null(where)){
    stop("The parameter `where` cannot be null")
  }

  if(is.null(usemode)){
    stop("usemode must be one following: classification, regression, both")
  }
  if(usemode != "regression" && is.null(class)){
    stop("for the selected usemode, the `class` parameter cannot be null")
  }else{
    if(usemode != "regression" && !is.character(class) && !is.factor(class)){
      stop("the `class` parameter must be either a string or a factor")
    }
  }
  if(usemode != "classification" && is.null(target)){
    stop("for the selected usemode, the `target` parameter cannot be null")
  }else{
    if(usemode != "classification" && !is.numeric(target)){
      stop("the `target` parameter must be numeric")
    }
  }

  config = list(
    "translationSchema" = list(
      "Line" = "Line"
      , "currentR/10" = "currentPhaseR"
      , "currentS/10" = "currentPhaseS"
      , "currentT/10" = "currentPhaseT"
      , "voltageR" = "voltageR"
      , "voltageS" = "voltageS"
      , "voltageT" = "voltageT"
      , "entryEndTime" =  "sourceLabel"
    )
    , "type" = "sql"
    , "uri" = "localhost:3306"
    , "configFile" = dbConfig
    , "motorCurrentRMS" =  4.0
    , "motorVoltageRMS" = 230
    , "divideByColumn" = "Line"
    , "rowsPerDividerColumn" = 20
    , "datasets" = list(
      list(
        "from" = "rearm.Monitoring"
        ,"where" = where
        ,"label"    = label
        ,"decimatedMagnitude" =  decimatedMagnitude
        ,"class"  = class
        ,"target" = target
      )
    )
  )

  return(rearmUtils::parseConfigSchema(config = config, usemode = usemode))

}

#' @export
curateData <- function(dataset, attrMap, decimateStep = 1, start = 1, sampleLength = "end", options, lclass, ltarget = NULL, sep=";", dec="," ){

  evalListForDataset <- list()
  usr.env <- new.env()
  if(!is.null(options)){
    if(is.element("forDataset",attributes(options)$names)){
      evalListForDataset[["applyOnEnd"]] <- ifelse(
        !is.element("applyOnEnd",attributes(options[["forDataset"]])$names),
        "",
        gsub(
          "%this%",
          "df",
          options[["forDataset"]][["applyOnEnd"]]
        )
      )
    }
  }

  if(sampleLength == "end"){
    sampleLength = nrow(dataset)
    if(sampleLength==0){
      e <- simpleError("dataset have length 0. Maybe you are trying to fetch something that does not exist on the data source?")
      stop(e)
    }
  }
  seqSample <- seq(start,sampleLength,decimateStep)

  nm <- colnames(dataset)
  if(length(nm) > 0){
    df <- data.frame(
      temp <- dataset[[nm[1]]][seqSample]
    )
    colnames(df)[1] <- attrMap[[1]]
    for (i in 2:length(nm)){
      df[attrMap[[i]]] <- dataset[[nm[i]]][seqSample]
    }
    df[["_class"]] <- rep(lclass, nrow(df))
    if(!is.null(ltarget)){
      df[["_target"]] <- rep(ltarget, nrow(df))
    }
    assign("df", df, envir=usr.env)
    if(!is.null(evalListForDataset$applyOnEnd)){
      eval(parse(text=evalListForDataset$applyOnEnd),envir=usr.env)
    }

    return(usr.env$df)
  }
  else{
    # throw error warning that the dataset dataframe has no colnames
    e <- simpleError("dataset dataframe has no colnames")
    stop(e)
  }
}

#' @export
parseConfigSchema <- function(config, sep = ";", dec = ",", timePerPeriod = 0.02, usemode){

  dataSchema <- config$translationSchema
  filesMetaInfo <- config$datasets
  motorCurrentRMS <- as.numeric(config$motorCurrentRMS)
  motorVoltageRMS <- as.numeric(config$motorVoltageRMS)
  db_user <- config$db_user
  db_password <- config$db_password
  db_name <- config$db_name
  db_host <- config$db_host
  configFile <- config$configFile #Must be a json file
  type <- tolower(config$type)
  divideByCycles <- NULL
  divideByColumn <- NULL
  rowsPerDividerColumn <- NULL
  isSQL <- type=="sql"
  if(isSQL){
    divideByColumn <- config$divideByColumn
    rowsPerDividerColumn <- config$rowsPerDividerColumn
    if(!is.null(configFile)){
      dbConfig <- rjson::fromJSON(file=configFile)
      db_user <- dbConfig$user
      db_password <-  dbConfig$password
      db_name <- dbConfig$name
      db_host <- dbConfig$host
    }
    mydb = RMySQL::dbConnect(
      RMySQL::MySQL(),
      user=ifelse(is.null(db_user),'root',db_user),
      password=ifelse(is.null(db_password),'1234',db_password),
      dbname=ifelse(is.null(db_name),'rearm',dbname),
      host=ifelse(is.null(db_host), '0.0.0.0', db_host)
    )
  }else{
    divideByCycles <- as.numeric(config$divideByCycles)
  }

  subsetByCycle <- F
  splitByNumberOfCycles <- 1

  if(!is.null(divideByCycles) && !isSQL){
    subsetByCycle <- T
    splitByNumberOfCycles <- divideByCycles
  }

  #same for sql case or csv case

  fnAnalizeFiles <- NULL
  if(isSQL){
    fnAnalizeFiles <- function(el){
      sqlQuery <- "SELECT "
      for(i in names(dataSchema)){
        sqlQuery <- paste(sqlQuery, i, ",")
      }
      sqlQuery <- substr(sqlQuery, 1, nchar(sqlQuery)-1)
      sqlQuery <- paste(sqlQuery, "FROM", el$from, " ")
      sqlQuery <- paste(sqlQuery, "WHERE", el$where)
      return(sqlQuery)
    }
  }else{
    fnAnalizeFiles <- function(el){
      file.path(PROJHOME,el$filePath)
    }
  }

  filesToAnalyze <-  unlist(
    lapply(
      filesMetaInfo,
      fnAnalizeFiles
    )
  )

  datasetLabels <- unlist(
    lapply(
      filesMetaInfo,
      function(el){
        limit <- 1
        if(!is.null(el[["repeatDataset"]])){
          limit <- el[["repeatDataset"]]
        }
        lapply(
          1:limit,
          function(idx, limit){
            if(limit > 1){
              return(paste0(el$label, "#R",idx))
            }else{
              return(el$label)
            }
          },
          limit
        )
      }
    )
  )

  # json attribute for classification
  classes <-  unlist(
    lapply(
      filesMetaInfo,
      function(el){
        limit <- 1
        if(!is.null(el[["repeatDataset"]])){
          limit <- el[["repeatDataset"]]
        }
        lapply(
          1:limit,
          function(idx){
            el$class
          }
        )
      }
    )
  )

  # json attribute for regression
  targets <-  unlist(
    lapply(
      filesMetaInfo,
      function(el){
        limit <- 1
        if(!is.null(el[["repeatDataset"]])){
          limit <- el[["repeatDataset"]]
        }
        lapply(
          1:limit,
          function(idx){
            if(is.null(el$target)){
              return(NA)
            }else{
              return(el$target)
            }
          }
        )
      }
    )
  )


  #same for sql case or csv case
  decimatedMagnitudes <-  unlist(
    lapply(
      filesMetaInfo,
      function(el){
        el$decimatedMagnitude
      }
    )
  )



  datasetOptions <-  lapply(
    filesMetaInfo,
    function(el){
      return(
        list(
          repeatDataset = el[["repeatDataset"]],
          forDataset = el[["forDataset"]]
        )
      )
    }
  )

  readDataset <- function(isSQL, file){
    if(isSQL){
      res<-RMySQL::dbSendQuery(mydb, file)
      return(RMySQL::fetch(res, n= -1))
    }
    else{
      return(
        read.csv(file, sep=sep, dec=dec)
      )
    }
  }

  #TODO: alterar nome de curate para parse
  dataToAnalyzeCurated <- lapply(
    seq_along(filesMetaInfo),
    function(index, files, decimations, options, lclass, ltarget, lmode){
      limit <- 1
      if(!is.null(options[[index]][["repeatDataset"]])){
        limit <- options[[index]][["repeatDataset"]]
      }
      lapply(
        1:limit,
        function(idx){
          lclassV <- NULL
          ltargetV <- NULL
          if(lmode != "regression"){
            lclassV <- lclass[[index]]
          }
          if(!is.null(ltarget) && lmode != "classification"){
            ltargetV <- ltarget[[index]]
          }
          rearmUtils::curateData(
            readDataset(isSQL,files[[index]]),
            dataSchema,
            decimations[[index]],
            options = options[[index]],
            lclass = lclassV,
            ltarget = ltargetV
          )
        }
      )
    },
    filesToAnalyze,
    decimatedMagnitudes,
    datasetOptions,
    classes,
    targets,
    usemode
  )
  dataToAnalyzeCurated <- unlist(dataToAnalyzeCurated, recursive=F)

  if(isSQL){
    if(!is.null(divideByColumn)){
      #dataToAnalyzeCurated[[1]][dataToAnalyzeCurated[[1]][["Line"]]==5535,]
      dividedData <- lapply(
        dataToAnalyzeCurated,
        function(el){
          uniqueDivide <- unique(el[[divideByColumn]])
          res <- list()
          for(curr in uniqueDivide){
            res[[length(res)+1]] <- el[el[[divideByColumn]]==curr,]
            res[[length(res)]] <- res[[length(res)]][1:rowsPerDividerColumn,]
            res[[length(res)]]$time <- seq(0,(0.001*(nrow(res[[length(res)]])-1)),0.001)
          }
          return(res)
        }
      )

      datasetLabels <- unlist(
        lapply(
          seq_along(datasetLabels),
          function(idx, ds){
            el <- ds[[idx]]
            repslbl <- rep(el,length(dividedData[[idx]]))
            lapply(
              seq_along(repslbl),
              function(idx, lbl){
                paste0(lbl[idx], " - Entity ",idx)
              },
              lbl = repslbl
            )
          },
          ds = datasetLabels
        ),
        recursive = F
      )


      classes <- unlist(
        lapply(
          seq_along(classes),
          function(idx, ds){
            el <- ds[[idx]]
            rep(el,length(dividedData[[idx]]))
          },
          ds = classes
        )
      )

      targets <- unlist(
        lapply(
          seq_along(targets),
          function(idx, ds){
            el <- ds[[idx]]
            rep(el,length(dividedData[[idx]]))
          },
          ds = targets
        )
      )

      dataToAnalyzeCurated <- unlist(dividedData, recursive = F)


    }
  }
  else{
    if(subsetByCycle){

      if(is.null(datasetLabels)){
        e <- simpleError("datasetLabels is required when process_park_transform is called with subsetByCycle = T")
        stop(e)
      }

      timeDSInfo <- lapply(
        dataToAnalyzeCurated,
        function(element){
          cycles <- list()
          timeInit <- element$time[1]
          timeEnd <- tail(element$time, n=1)
          totalTime <- (timeEnd - timeInit)

          totalIterationsPerDS <- round(totalTime/(timePerPeriod*splitByNumberOfCycles))

          return(
            list(
              iterations = totalIterationsPerDS,
              timeInit = timeInit,
              timeEnd = timeEnd,
              totalTime = totalTime
            )

          )
        }
      )

      DSSubsetCycles <- lapply(
        seq_along(dataToAnalyzeCurated),
        function(idx, ds, timeDSInfo){
          element <- ds[[idx]]
          totalIterationsPerDS <- timeDSInfo[[idx]]$iterations
          cycles <- list()
          for(i in 1:totalIterationsPerDS){
            cycles[[i]] <- subset(
              element,
              round(time, digits=4) >= round(timeDSInfo[[idx]]$timeInit + (timePerPeriod*splitByNumberOfCycles)*(i-1),digits=4) & round(time,digits=4) < round(timeDSInfo[[idx]]$timeInit + (timePerPeriod*splitByNumberOfCycles)*i,digits=4))
          }
          return(
            cycles
          )
        }#,
        , dataToAnalyzeCurated
        ,timeDSInfo
      )

      dataToAnalyzeCurated <- unlist(DSSubsetCycles, recursive = F)

      datasetLabels <- unlist(
        lapply(
          seq_along(datasetLabels),
          function(idx, ds, timeDSInfo){
            el <- ds[[idx]]
            repslbl <- rep(el,timeDSInfo[[idx]]$iterations)
            lapply(
              seq_along(repslbl),
              function(idx, lbl){
                paste0(lbl[idx], " - Cycle",idx)
              },
              lbl = repslbl
            )
          },
          ds = datasetLabels,
          timeDSInfo = timeDSInfo
        ),
        recursive = F
      )


      classes <- unlist(
        lapply(
          seq_along(classes),
          function(idx, ds, timeDSInfo){
            el <- ds[[idx]]
            rep(el,timeDSInfo[[idx]]$iterations)
          },
          ds = classes,
          timeDSInfo = timeDSInfo
        )
      )

      targets <- unlist(
        lapply(
          seq_along(targets),
          function(idx, ds, timeDSInfo){
            el <- ds[[idx]]
            rep(el,timeDSInfo[[idx]]$iterations)
          },
          ds = targets,
          timeDSInfo = timeDSInfo
        )
      )


    }

  }


  dataToAnalyzeCurated <- lapply(
    dataToAnalyzeCurated,
    function(el){
      el$time <- el$time + -1*el$time[1]
      return(el)
    }
  )

  sampledFrequencies <-  unlist(
    lapply(
      dataToAnalyzeCurated,
      function(el){
        if(length(el$time)>1){
          return(1/(el$time[2] - el$time[1]))
        }
        else{
          e <- simpleError("dataset don't have more than 1 sample")
          stop(e)
        }
      }
    )
  )


  if(isSQL){
    RMySQL::dbDisconnect(mydb)
  }

  if(usemode != "both"){
    if(usemode == "classification"){
      targets <- NULL
    }else{
      classes <- NULL
    }
  }

  return(
    list(
      datasetLabels = datasetLabels,
      filesToAnalyze = filesToAnalyze,
      sampledFrequencies = sampledFrequencies,
      dataToAnalyzeCurated = dataToAnalyzeCurated,
      classes = classes,
      targets = targets,
      motorCurrentRMSList = rep(motorCurrentRMS, length(datasetLabels)),
      motorVoltageRMSList = rep(motorVoltageRMS, length(datasetLabels))
    )
  )
}

#' @export
readRunner <- function(configFile, usemode="both"){
  # values for mode are "both","classification" or "regression"
  datasetLabels = list()
  sampledFrequencies = list()
  dataToAnalyzeCurated = list()
  classes = list()
  targets = list()
  motorCurrentRMSList = list()
  motorVoltageRMSList = list()

  cfgDS <- rjson::fromJSON(file=configFile)
  for(i in 1:length(cfgDS)){
    el <- cfgDS[[i]] #translation schema level
    curr <- rearmUtils::parseConfigSchema(el, usemode = usemode)
    datasetLabels[[length(datasetLabels)+1]] <- curr$datasetLabels
    sampledFrequencies[[length(sampledFrequencies)+1]] <- curr$sampledFrequencies
    dataToAnalyzeCurated[[length(dataToAnalyzeCurated)+1]] <- curr$dataToAnalyzeCurated
    classes[[length(classes)+1]] <- curr$classes
    targets[[length(targets)+1]] <- curr$targets
    motorCurrentRMSList[[length(motorCurrentRMSList)+1]] <- curr$motorCurrentRMSList
    motorVoltageRMSList[[length(motorVoltageRMSList)+1]] <- curr$motorVoltageRMSList
  }

  datasetLabels = unlist(datasetLabels)
  sampledFrequencies = unlist(sampledFrequencies)
  dataToAnalyzeCurated = unlist(dataToAnalyzeCurated, recursive=F)
  classes = unlist(classes, recursive = F)
  targets = unlist(targets, recursive = F)
  motorCurrentRMSList = unlist(motorCurrentRMSList, recursive = F)
  motorVoltageRMSList = unlist(motorVoltageRMSList, recursive = F)

  return(
    list(
      datasetLabels = datasetLabels,
      sampledFrequencies = sampledFrequencies,
      dataToAnalyzeCurated = dataToAnalyzeCurated,
      classes = classes,
      targets = targets,
      motorCurrentRMSs = motorCurrentRMSList,
      motorVoltageRMSs = motorVoltageRMSList
    )
  )
}

#' @export
genPCAPark_RMS_Features <- function(dataGlobal, avg.weights = rep(1/6 , 6), withEigenVectors = T, normalizeByMotor = F){
  if(is.null(dataGlobal) || is.na(dataGlobal)){
    stop("dataGlobal (first argument of genPCAParkFeatures) must be the return object of the `rearmUtils::readRunner` function")
  }
  data.currents.parkTransform <- lapply(
    dataGlobal,
    function(el){
      rearmSignalProcessing::process_park_transform(
        dataset = el$dataToAnalyzeCurated, motorNominalRMS = el$motorCurrentRMSs,
        label = el$datasetLabels, color = el$color
      )
    }
  )

  data.voltages.parkTransform <- lapply(
    dataGlobal,
    function(el){
      rearmSignalProcessing::process_park_transform(
        dataset = el$dataToAnalyzeCurated, motorNominalRMS = el$motorVoltageRMSs,
        label = el$datasetLabels, color = el$color, threePhaseAttributes = c('voltageR', 'voltageS', 'voltageT')
      )
    }
  )

  data.currents.parkTransform.eigenTransform <- lapply(
    seq_along(data.currents.parkTransform),
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
    ds = data.currents.parkTransform,
    commonInfo = dataGlobal
  )

  data.voltages.parkTransform.eigenTransform <- lapply(
    seq_along(data.voltages.parkTransform),
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
    ds = data.voltages.parkTransform,
    commonInfo = dataGlobal
  )

  outliers.score.idx <- lapply(
    data.currents.parkTransform.eigenTransform,
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

  dataGlobal.noOut <- lapply(
    seq_along(dataGlobal),
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
    commonInfo = dataGlobal,
    outliers.idx = outliers.score.idx
  )

  data.currents.parkTransform.eigenTransform.noOutliers <- lapply(
    seq_along(data.currents.parkTransform.eigenTransform),
    function(idx, eigen, outliers.idx){
      if(length(outliers.idx[[idx]])>0){
        eigen[[idx]] <- eigen[[idx]][-outliers.idx[[idx]]]
      }
      return(eigen[[idx]])
    },
    eigen = data.currents.parkTransform.eigenTransform,
    outliers.idx = outliers.score.idx
  )

  data.voltages.parkTransform.eigenTransform.noOutliers <- lapply(
    seq_along(data.voltages.parkTransform.eigenTransform),
    function(idx, eigen, outliers.idx){
      if(length(outliers.idx[[idx]])>0){
        eigen[[idx]] <- eigen[[idx]][-outliers.idx[[idx]]]
      }
      return(eigen[[idx]])
    },
    eigen = data.voltages.parkTransform.eigenTransform,
    outliers.idx = outliers.score.idx
  )

  feature.PCATransform <- unlist(lapply(
    seq_along(data.voltages.parkTransform.eigenTransform),
    function(idx, currents, voltages, commonInfo){
      lapply(
        seq_along(voltages[[idx]]),
        function(idx, curr, volt, el_class, labels){
          elC <- curr[[idx]]
          elV <- volt[[idx]]
          return(
            data.frame(
              current.pc1x = elC$pc1$x[2],
              current.pc1y = elC$pc1$y[2],
              current.pc2x = elC$pc2$x[2],
              current.pc2y = elC$pc2$y[2],
              current.score = elC$score,
              voltage.pc1x = elV$pc1$x[2],
              voltage.pc1y = elV$pc1$y[2],
              voltage.pc2x = elV$pc2$x[2],
              voltage.pc2y = elV$pc2$y[2],
              voltage.score = elV$score,
              class = el_class[[idx]],
              label = labels[[idx]]
            )
          )
        },
        curr = currents[[idx]],
        volt = voltages[[idx]],
        el_class = commonInfo[[idx]]$class,
        labels = commonInfo[[idx]]$datasetLabels
      )
    },
    currents = data.currents.parkTransform.eigenTransform.noOutliers,
    voltages = data.voltages.parkTransform.eigenTransform.noOutliers,
    commonInfo = dataGlobal.noOut
  ), recursive = F)

  feature.PCATransform <- do.call(rbind, feature.PCATransform)

  feature.PCATransform.withAvgs <- lapply(
    seq_along(data.voltages.parkTransform.eigenTransform),
    function(idx, currents, voltages, commonInfo){

      # Since the labels carry information related to the lab test condition,
      # here the Entity content of the label is splitted from the original label
      DSLabels <- unlist(lapply(
        commonInfo[[idx]]$datasetLabels,
        function(label){
          strsplit(label, " - Entity")[[1]][1]
        }
      ))
      ds3P <- commonInfo[[idx]]$dataToAnalyzeCurated
      currs <- currents[[idx]]
      volts <- voltages[[idx]]
      if(length(volts) != length(currs)){
        stop("feature.PCATransform.withAvgs: length(volts) != length(currs)", call. = TRUE, domain = NULL)
      }
      lengthDs = length(volts)
      attr.pc1x <- c()
      attr.pc1y <- c()
      attr.score <- c()
      attr.pc1x.avg <- c()
      attr.pc1y.avg <- c()
      attr.score.avg <- c()

      attr.v.pc1x <- c()
      attr.v.pc1y <- c()
      attr.v.score <- c()
      attr.v.pc1x.avg <- c()
      attr.v.pc1y.avg <- c()
      attr.v.score.avg <- c()

      attr.class <- c()
      attr.desc <- c()

      for(i in 1:lengthDs){
        elC <- currs[[i]]
        elV <- volts[[i]]
        avgs = unlist(lapply(
          seq(i-1,i-length(avg.weights),by=-1),
          # sample(lengthDs,length(avg.weights)),
          function(x){
            #check if indice is valid or not
            if(x < 1){
              x <- NA
            }else{
              if(DSLabels[i] != DSLabels[x]){
                x <- NA
              }
            }
            return(x)
          }
        ), recursive = F)

        pc1x.avg <- NA
        sign.x <- NA
        sign.y <- NA
        pc1y.avg <- NA
        score.avg <- NA

        v.pc1x.avg <- NA
        v.sign.x <- NA
        v.sign.y <- NA
        v.pc1y.avg <- NA
        v.score.avg <- NA

        if(!any(is.na(avgs))){
          if(is.na(pc1x.avg)){
            #se aquele é, então todos os outros são
            pc1x.avg <- 0
            sign.x <- 0
            sign.y <- 0
            pc1y.avg <- 0
            score.avg <- 0

            v.pc1x.avg <- 0
            v.sign.x <- 0
            v.sign.y <- 0
            v.pc1y.avg <- 0
            v.score.avg <- 0
          }
          for(j in 1:length(avg.weights)){
            pc1x.avg <- pc1x.avg + abs(currs[avgs[[j]]][[1]]$pc1$x[2]) * avg.weights[j]
            if(currs[avgs[[j]]][[1]]$pc1$x[2] > 0){
              sign.x <- sign.x + 1
            }
            else{
              sign.x <- sign.x - 1
            }
            pc1y.avg <- pc1y.avg + abs(currs[avgs[[j]]][[1]]$pc1$y[2]) * avg.weights[j]
            if(currs[avgs[[j]]][[1]]$pc1$y[2] > 0){
              sign.y <- sign.y + 1
            }
            else{
              sign.y <- sign.y - 1
            }
            score.avg <- score.avg + currs[avgs[[j]]][[1]]$score * avg.weights[j]

            v.pc1x.avg <- v.pc1x.avg + abs(volts[avgs[[j]]][[1]]$pc1$x[2]) * avg.weights[j]
            if(volts[avgs[[j]]][[1]]$pc1$x[2] > 0){
              v.sign.x <- v.sign.x + 1
            }
            else{
              v.sign.x <- v.sign.x - 1
            }
            v.pc1y.avg <- v.pc1y.avg + abs(volts[avgs[[j]]][[1]]$pc1$y[2]) * avg.weights[j]
            if(volts[avgs[[j]]][[1]]$pc1$y[2] > 0){
              v.sign.y <- v.sign.y + 1
            }
            else{
              v.sign.y <- v.sign.y - 1
            }
            v.score.avg <- v.score.avg + volts[avgs[[j]]][[1]]$score * avg.weights[j]

          }
          if(sign.x < 0){
            pc1x.avg = pc1x.avg*-1
          }
          if(sign.y < 0){
            pc1y.avg = pc1y.avg*-1
          }

          if(v.sign.x < 0){
            v.pc1x.avg = v.pc1x.avg*-1
          }
          if(v.sign.y < 0){
            v.pc1y.avg = v.pc1y.avg*-1
          }
        }


        attr.pc1x <- c(attr.pc1x, elC$pc1$x[2])
        attr.pc1y <- c(attr.pc1y, elC$pc1$y[2])
        attr.score <- c(attr.score, elC$score)
        attr.pc1x.avg <- c(attr.pc1x.avg, pc1x.avg)
        attr.pc1y.avg <- c(attr.pc1y.avg, pc1y.avg)
        attr.score.avg <- c(attr.score.avg, score.avg)

        attr.v.pc1x <- c(attr.v.pc1x, elV$pc1$x[2])
        attr.v.pc1y <- c(attr.v.pc1y, elV$pc1$y[2])
        attr.v.score <- c(attr.v.score, elV$score)
        attr.v.pc1x.avg <- c(attr.v.pc1x.avg, v.pc1x.avg)
        attr.v.pc1y.avg <- c(attr.v.pc1y.avg, v.pc1y.avg)
        attr.v.score.avg <- c(attr.v.score.avg, v.score.avg)

        attr.class <- c(attr.class, commonInfo[[idx]]$class[[i]])
        attr.desc <- c(attr.desc, commonInfo[[idx]]$datasetLabels[[i]])
      }
      resDF <- data.frame(
        current.pc1x = attr.pc1x,
        current.pc1y = attr.pc1y,
        current.score = attr.score,
        current.pc1xavg = attr.pc1x.avg,
        current.pc1yavg = attr.pc1y.avg,
        current.scoreavg = attr.score.avg,

        voltage.pc1x = attr.v.pc1x,
        voltage.pc1y = attr.v.pc1y,
        voltage.score = attr.v.score,
        voltage.pc1xavg = attr.v.pc1x.avg,
        voltage.pc1yavg = attr.v.pc1y.avg,
        voltage.scoreavg = attr.v.score.avg,

        label = attr.desc,
        class = attr.class
      )
      return(
        resDF
      )
    },
    currents = data.currents.parkTransform.eigenTransform.noOutliers,
    voltages = data.voltages.parkTransform.eigenTransform.noOutliers,
    commonInfo = dataGlobal.noOut
  )

  feature.PCATransform.withAvgs <- do.call(rbind, feature.PCATransform.withAvgs)
  feature.PCATransform.withAvgs <- feature.PCATransform.withAvgs[complete.cases(feature.PCATransform.withAvgs),]

  #calculate waveform unbalance
  data.waveformImbalance <- lapply(
    dataGlobal.noOut,
    function(ds){
      lapply(
        seq_along(ds$dataToAnalyzeCurated),
        function(idx, dataset){
          el <- dataset$dataToAnalyzeCurated[[idx]]
          label <- dataset$datasetLabels[[idx]]
          motorCurrentRMS <- dataset$motorCurrentRMSs[[idx]]

          rmsR = rearmSignalProcessing::rms(el$currentPhaseR)/motorCurrentRMS
          rmsS = rearmSignalProcessing::rms(el$currentPhaseS)/motorCurrentRMS
          rmsT = rearmSignalProcessing::rms(el$currentPhaseT)/motorCurrentRMS
          meanRMS = mean(c(rmsR,rmsS,rmsT))

          unb <- max(c(rmsR-meanRMS, rmsS-meanRMS, rmsT-meanRMS))/meanRMS

          return(
            data.frame(
              current.rmssd = unb,
              current.rmsR = rmsR,
              current.rmsS = rmsS,
              current.rmsT = rmsT,
              label = label
            )
          )
        }
        ,dataset = ds
      )
    }
  )

  #calculate waveform unbalance + averages of unbalances features
  feature.waveformImbalance.withAvgs <- lapply(
    seq_along(data.waveformImbalance),
    function(idx, datasets, commonInfo){
      ds <- datasets[[idx]]

      # Since the labels carry information related to the lab test condition,
      # here the Entity content of the label is splitted from the original label
      DSLabels <- unlist(lapply(
        commonInfo[[idx]]$datasetLabels,
        function(label){
          strsplit(label, " - Entity")[[1]][1]
        }
      ))
      lengthDs = length(ds)

      attr.rmssd <- c()
      attr.rmssd.avg <- c()
      attr.rmsR <- c()
      attr.rmsS <- c()
      attr.rmsT <- c()
      attr.rmsR.avg <- c()
      attr.rmsS.avg <- c()
      attr.rmsT.avg <- c()
      # experiemtnar abordagem onde retiro os 6 primeiros pontos, porque n tenho as médias paras eles
      for(i in 1:lengthDs){
        el <- ds[[i]]
        avgs = unlist(lapply(
          seq(i-1,i-length(avg.weights),by=-1),
          # sample(lengthDs,length(avg.weights)),
          function(x){
            #check if indice is valid or not
            if(x < 1){
              x <- NA
            }else{
              if(DSLabels[i] != DSLabels[x]){
                x <- NA
              }
            }
            return(x)
          }
        ), recursive = F)
        rmssd.avg <- NA
        rmsR.avg <- NA
        rmsS.avg <- NA
        rmsT.avg <- NA

        # se nenhum elemento da var `avgs` for NA,
        # então este elemento pode ter média associada dos últimos N elements
        if(!any(is.na(avgs))){
          #Se há um elemento inicializado com NA, então todos os outros ainda estão com NA
          if(is.na(rmssd.avg)){
            rmssd.avg <- 0
            rmsR.avg <- 0
            rmsS.avg <- 0
            rmsT.avg <- 0
          }
          for(j in 1:length(avg.weights)){
            rmssd.avg <- rmssd.avg + ds[avgs[[j]]][[1]]$current.rmssd * avg.weights[j]
            rmsR.avg <- rmsR.avg + ds[avgs[[j]]][[1]]$current.rmsR * avg.weights[j]
            rmsS.avg <- rmsS.avg + ds[avgs[[j]]][[1]]$current.rmsS * avg.weights[j]
            rmsT.avg <- rmsT.avg + ds[avgs[[j]]][[1]]$current.rmsT * avg.weights[j]
          }
        }

        attr.rmssd <- c(attr.rmssd, el$current.rmssd)
        attr.rmssd.avg <- c(attr.rmssd.avg, rmssd.avg)
        attr.rmsR <- c(attr.rmsR, el$current.rmsR)
        attr.rmsS <- c(attr.rmsS, el$current.rmsS)
        attr.rmsT <- c(attr.rmsT, el$current.rmsT)
        attr.rmsR.avg <- c(attr.rmsR.avg, rmsR.avg)
        attr.rmsS.avg <- c(attr.rmsS.avg, rmsS.avg)
        attr.rmsT.avg <- c(attr.rmsT.avg, rmsT.avg)
      }
      return(
        data.frame(
          current.rmssd = attr.rmssd,
          current.rmssdavg = attr.rmssd.avg,
          current.rmsR = attr.rmsR,
          current.rmsS = attr.rmsS,
          current.rmsT = attr.rmsT,
          current.rmsRavg = attr.rmsR.avg,
          current.rmsSavg = attr.rmsS.avg,
          current.rmsTavg = attr.rmsT.avg
        )
      )
    },
    datasets = data.waveformImbalance,
    commonInfo = dataGlobal.noOut
  )

  feature.waveformImbalance.withAvgs <- do.call(rbind, feature.waveformImbalance.withAvgs)
  feature.waveformImbalance.withAvgs <- feature.waveformImbalance.withAvgs[complete.cases(feature.waveformImbalance.withAvgs),]

  # join the features of PCATransform unbalance + waveform unbalance
  feature.PCATransform.waveformImbalance.withAvgs <- cbind(feature.PCATransform.withAvgs, feature.waveformImbalance.withAvgs)

  if(withEigenVectors){
    return(feature.PCATransform.waveformImbalance.withAvgs)
  }else{
    notInGrp <- c("current.pc1x", "current.pc1y", "current.pc1xavg", "current.pc1yavg", "voltage.pc1x", "voltage.pc1y", "voltage.pc1xavg", "voltage.pc1yavg")
    columsToSelect <- !(names(feature.PCATransform.waveformImbalance.withAvgs) %in% notInGrp)
    dataset <- feature.PCATransform.waveformImbalance.withAvgs[,columsToSelect]
    return(dataset)
  }
}

#' @export
genABC_RMS_features <- function(dataGlobal){
  Apply_RMSFeatures <- function(dataset){
    lapply(
      seq_along(dataset$dataToAnalyzeCurated),
      function(idx, ds, el_class,el_target){
        el <- ds[[idx]]
        res <- data.frame(
          rmsCurrentA = rearmSignalProcessing::rms(el$currentPhaseR),
          rmsCurrentB = rearmSignalProcessing::rms(el$currentPhaseS),
          rmsCurrentC = rearmSignalProcessing::rms(el$currentPhaseT),
          rmsVoltageA = rearmSignalProcessing::rms(el$voltageR),
          rmsVoltageB = rearmSignalProcessing::rms(el$voltageS),
          rmsVoltageC = rearmSignalProcessing::rms(el$voltageT)
        )
        if(!is.null(el$class)){
          res$class <- el$class[1]
        }
        if(!is.null(el_class)){
          res[["_class"]] <- el_class[[idx]]
        }
        if(!is.null(el_target)){
          res[["_target"]] <- el_class[[idx]]
        }


        idxSeq <- (which.max(el$currentPhaseR) - 2)%%length(el$currentPhaseR)
        ifelse(idxSeq ==0, idxSeq <- length(el$currentPhaseR), idxSeq <- idxSeq)
        positiveSeq <- NULL
        ifelse(el$currentPhaseS[idxSeq] > el$currentPhaseT[idxSeq], positiveSeq <- 1, positiveSeq <- 0)

        res$positiveSeq <- positiveSeq

        return(
          res
        )
      },
      ds = dataset$dataToAnalyzeCurated,
      el_class = dataset$classes,
      el_target = dataset$targets
    )
  }

  dataset_featured <-unlist(lapply(
    dataGlobal,
    Apply_RMSFeatures
  ), recursive = F)

  # dataset_featured_merged <- Reduce(
  #   function(acc, curr){
  #     rbind(acc, curr)
  #   },
  #   dataset_featured[2:length(dataset_featured)],
  #   dataset_featured[[1]]
  # )

  dataset_featured_merged <- do.call(rbind, dataset_featured)

  return(dataset_featured_merged)
}

#' @export
genABC_Imbalance_features <- function(dataGlobal, full = F, normalizeByMotor=F){
  data.waveformImbalance <- unlist(lapply(
    dataGlobal,
    function(ds){
      lapply(
        seq_along(ds$dataToAnalyzeCurated),
        function(idx, dataset){
          el <- dataset$dataToAnalyzeCurated[[idx]]
          el_class <- dataset$classes
          el_target <- dataset$targets
          label <- dataset$datasetLabels[[idx]]
          motorCurrentRMS <- dataset$motorCurrentRMSs[[idx]]
          motorVoltageRMS <- dataset$motorVoltageRMSs[[idx]]


          rmsR = rearmSignalProcessing::rms(el$currentPhaseR)
          rmsS = rearmSignalProcessing::rms(el$currentPhaseS)
          rmsT = rearmSignalProcessing::rms(el$currentPhaseT)

          v.rmsR = rearmSignalProcessing::rms(el$voltageR)
          v.rmsS = rearmSignalProcessing::rms(el$voltageS)
          v.rmsT = rearmSignalProcessing::rms(el$voltageT)

          if(normalizeByMotor){
            rmsR <- rmsR/motorCurrentRMS
            rmsS <- rmsS/motorCurrentRMS
            rmsT <- rmsT/motorCurrentRMS

            v.rmsR <- v.rmsR/motorVoltageRMS
            v.rmsS <- v.rmsS/motorVoltageRMS
            v.rmsT <- v.rmsT/motorVoltageRMS
          }

          meanRMS = mean(c(rmsR,rmsS,rmsT))
          v.meanRMS = mean(c(v.rmsR,v.rmsS,v.rmsT))


          unb <- max(c(rmsR-meanRMS, rmsS-meanRMS, rmsT-meanRMS))/meanRMS
          v.unb <- max(c(v.rmsR-v.meanRMS, v.rmsS-v.meanRMS, v.rmsT-v.meanRMS))/v.meanRMS

          if(full){
            res <-  data.frame(
              current.rmssd = unb,
              current.rmsR = rmsR,
              current.rmsS = rmsS,
              current.rmsT = rmsT,
              current.meanRMS = meanRMS,
              voltage.rmssd = v.unb,
              voltage.rmsR = v.rmsR,
              voltage.rmsS = v.rmsS,
              voltage.rmsT = v.rmsT,
              voltage.meanRMS = v.meanRMS,
              label = label
            )
          }else{
            res <-  data.frame(
              current.rmssd = unb,
              voltage.rmssd = v.unb,
              label = label
            )
          }

          if(!is.null(el$class)){
            res$class <- el$class[1]
          }

          if(!is.null(el_target)){
            res[["_target"]] <- el_target[[idx]]
          }
          if(!is.null(el_class)){
            res[["_class"]] <- el_class[[idx]]
          }


          idxSeq <- (which.max(el$currentPhaseR) - 2)%%length(el$currentPhaseR)
          ifelse(idxSeq ==0, idxSeq <- length(el$currentPhaseR), idxSeq <- idxSeq)
          positiveSeq <- NULL
          ifelse(el$currentPhaseS[idxSeq] > el$currentPhaseT[idxSeq], positiveSeq <- 1, positiveSeq <- 0)

          res$positiveSeq <- positiveSeq

          return(res)

        }
        ,dataset = ds
      )
    }
  ), recursive = F)

  return(do.call(rbind, data.waveformImbalance))
}

#' @export
genABC_Imbalance_withAvgs_features <- function(dataGlobal = NULL, dataset_imbalance_ft = NULL, avg.weights = rep(1/6, 6), RMSPerPhaseAvg = F){
  if(is.null(dataset_imbalance_ft)){
    dataset_imbalance_ft <- rearmUtils::genABC_Imbalance_features(dataGlobal,RMSPerPhaseAvg,F)
  }
  else{
    if(RMSPerPhaseAvg){
      if(is.null(dataset_imbalance_ft$current.rmsR) && is.null(dataset_imbalance_ft$voltage.rmsR)){
        stop("Can't generate RMS per phase features with a dataset_imbalance_ft dataset which do not have the rms pher phase information")
      }
    }
  }
  DSLabels <- unlist(lapply(
    dataset_imbalance_ft[["label"]],
    function(str){
      strsplit(as.character(str), " - Entity")[[1]][1]
    }
  ))

  attr.rmssd.avg <- c()
  attr.rmsR.avg <- c()
  attr.rmsS.avg <- c()
  attr.rmsT.avg <- c()

  attr.v.rmssd.avg <- c()
  attr.v.rmsR.avg <- c()
  attr.v.rmsS.avg <- c()
  attr.v.rmsT.avg <- c()

  for(i in 1:nrow(dataset_imbalance_ft)){
    el <- dataset_imbalance_ft[i,]
    avgs = unlist(lapply(
      seq(i-1,i-length(avg.weights),by=-1),
      # sample(lengthDs,length(avg.weights)),
      function(x){
        #check if indice is valid or not
        if(x < 1){
          x <- NA
        }else{
          if(DSLabels[i] != DSLabels[x]){
            x <- NA
          }
        }
        return(x)
      }
    ), recursive = F)
    rmssd.avg <- NA
    rmsR.avg <- NA
    rmsS.avg <- NA
    rmsT.avg <- NA

    v.rmssd.avg <- NA
    v.rmsR.avg <- NA
    v.rmsS.avg <- NA
    v.rmsT.avg <- NA

    # se nenhum elemento da var `avgs` for NA,
    # então este elemento pode ter média associada dos últimos N elements
    if(!any(is.na(avgs))){
      #Se há um elemento inicializado com NA, então todos os outros ainda estão com NA
      if(is.na(rmssd.avg)){
        rmssd.avg <- 0
        rmsR.avg <- 0
        rmsS.avg <- 0
        rmsT.avg <- 0

        v.rmssd.avg <- 0
        v.rmsR.avg <- 0
        v.rmsS.avg <- 0
        v.rmsT.avg <- 0
      }
      for(j in 1:length(avg.weights)){
        rmssd.avg <- rmssd.avg + dataset_imbalance_ft[avgs[[j]],"current.rmssd"] * avg.weights[j]
        rmsR.avg <- rmsR.avg + dataset_imbalance_ft[avgs[[j]],"current.rmsR"] * avg.weights[j]
        rmsS.avg <- rmsS.avg + dataset_imbalance_ft[avgs[[j]],"current.rmsS"] * avg.weights[j]
        rmsT.avg <- rmsT.avg + dataset_imbalance_ft[avgs[[j]],"current.rmsT"] * avg.weights[j]

        v.rmssd.avg <- rmssd.avg + dataset_imbalance_ft[avgs[[j]],"voltage.rmssd"] * avg.weights[j]
        v.rmsR.avg <- rmsR.avg + dataset_imbalance_ft[avgs[[j]],"voltage.rmsR"] * avg.weights[j]
        v.rmsS.avg <- rmsS.avg + dataset_imbalance_ft[avgs[[j]],"voltage.rmsS"] * avg.weights[j]
        v.rmsT.avg <- rmsT.avg + dataset_imbalance_ft[avgs[[j]],"voltage.rmsT"] * avg.weights[j]
      }
    }

    attr.rmssd.avg <- c(attr.rmssd.avg, rmssd.avg)
    attr.rmsR.avg <- c(attr.rmsR.avg, rmsR.avg)
    attr.rmsS.avg <- c(attr.rmsS.avg, rmsS.avg)
    attr.rmsT.avg <- c(attr.rmsT.avg, rmsT.avg)

    attr.v.rmssd.avg <- c(attr.v.rmssd.avg, v.rmssd.avg)
    attr.v.rmsR.avg <- c(attr.v.rmsR.avg, v.rmsR.avg)
    attr.v.rmsS.avg <- c(attr.v.rmsS.avg, v.rmsS.avg)
    attr.v.rmsT.avg <- c(attr.v.rmsT.avg, v.rmsT.avg)

  }
  dataset_imbalance_ft$current.rmssd.avg <- attr.rmssd.avg
  dataset_imbalance_ft$current.rmsR.avg <- attr.rmsR.avg
  dataset_imbalance_ft$current.rmsS.avg <- attr.rmsS.avg
  dataset_imbalance_ft$current.rmsT.avg <- attr.rmsT.avg

  dataset_imbalance_ft$voltage.rmssd.avg <- attr.v.rmssd.avg
  dataset_imbalance_ft$voltage.rmsR.avg <- attr.v.rmsR.avg
  dataset_imbalance_ft$voltage.rmsS.avg <- attr.v.rmsS.avg
  dataset_imbalance_ft$voltage.rmsT.avg <- attr.v.rmsT.avg
  return(dataset_imbalance_ft)
}

#' @export
genDQ_Imbalance_features <- function(dataGlobal, full = T){
  data.dqImbalance <- unlist(lapply(
    dataGlobal,
    function(ds){
      lapply(
        seq_along(ds$dataToAnalyzeCurated),
        function(idx, dataset){
          el <- dataset$dataToAnalyzeCurated[[idx]]
          el_class <- dataset$classes
          el_target <- dataset$targets
          label <- dataset$datasetLabels[[idx]]
          elC <- rearmSignalProcessing::transform_park(
            a = el$currentPhaseR, b = el$currentPhaseS, c = el$currentPhaseT
          )
          elC <- rearmParkAnalysis::transform_dqPoints_to_eigenVector(elC)
          elV <- rearmSignalProcessing::transform_park(
            a = el$voltageR, b = el$voltageS, c = el$voltageT
          )
          elV <- rearmParkAnalysis::transform_dqPoints_to_eigenVector(elV)

          if(full){
            res <-  data.frame(
              current.pc1x = elC$pc1$x[2],
              current.pc1y = elC$pc1$y[2],
              current.pc2x = elC$pc2$x[2],
              current.pc2y = elC$pc2$y[2],
              current.score = elC$score,
              voltage.pc1x = elV$pc1$x[2],
              voltage.pc1y = elV$pc1$y[2],
              voltage.pc2x = elV$pc2$x[2],
              voltage.pc2y = elV$pc2$y[2],
              voltage.score = elV$score,
              label = label
            )
          }else{
            res <-  data.frame(
              current.score = elC$score,
              voltage.score = elV$score,
              label = label
            )
          }

          if(!is.null(el$class)){
            # todos os outros atributos deveriam ser herdados (por agregação).
            # No caso de serem categóricos, o default podia ser a moda.
            # No caso de serem numéricos, o default podia ser a moda também
            res$class <- el$class[1]
          }
          if(!is.null(el_class)){
            res[["_class"]] <- el_class[[idx]]
          }
          if(!is.null(el_target)){
            res[["_target"]] <- el_target[[idx]]
          }


          return(res)

        }
        ,dataset = ds
      )
    }
  ), recursive = F)

  return(do.call(rbind, data.dqImbalance))
}

#' @export
genDQ_Imbalance_withAvgs_features <- function(dataGlobal = NULL, dataset_imbalance_ft = NULL, avg.weights = rep(1/6, 6), full=T){
  if(is.null(dataset_imbalance_ft)){
    dataset_imbalance_ft <- rearmUtils::genDQ_Imbalance_features(dataGlobal)
  }
  DSLabels <- unlist(lapply(
    dataset_imbalance_ft[["label"]],
    function(str){
      strsplit(as.character(str), " - Entity")[[1]][1]
    }
  ))

  attr.pc1x <- c()
  attr.pc1y <- c()
  attr.score <- c()
  attr.pc1x.avg <- c()
  attr.pc1y.avg <- c()
  attr.score.avg <- c()

  attr.v.pc1x <- c()
  attr.v.pc1y <- c()
  attr.v.score <- c()
  attr.v.pc1x.avg <- c()
  attr.v.pc1y.avg <- c()
  attr.v.score.avg <- c()

  attr.class <- c()
  attr.desc <- c()

  for(i in 1:nrow(dataset_imbalance_ft)){
    el <- dataset_imbalance_ft[i,]
    avgs = unlist(lapply(
      seq(i-1,i-length(avg.weights),by=-1),
      # sample(lengthDs,length(avg.weights)),
      function(x){
        #check if indice is valid or not
        if(x < 1){
          x <- NA
        }else{
          if(DSLabels[i] != DSLabels[x]){
            x <- NA
          }
        }
        return(x)
      }
    ), recursive = F)

    pc1x.avg <- NA
    sign.x <- NA
    sign.y <- NA
    pc1y.avg <- NA
    score.avg <- NA

    v.pc1x.avg <- NA
    v.sign.x <- NA
    v.sign.y <- NA
    v.pc1y.avg <- NA
    v.score.avg <- NA

    if(!any(is.na(avgs))){
      if(is.na(pc1x.avg)){
        #se aquele é, então todos os outros são
        pc1x.avg <- 0
        sign.x <- 0
        sign.y <- 0
        pc1y.avg <- 0
        score.avg <- 0

        v.pc1x.avg <- 0
        v.sign.x <- 0
        v.sign.y <- 0
        v.pc1y.avg <- 0
        v.score.avg <- 0
      }
      for(j in 1:length(avg.weights)){
        score.avg <- score.avg + dataset_imbalance_ft[avgs[[j]], "current.score"] * avg.weights[j]
        v.score.avg <- v.score.avg + dataset_imbalance_ft[avgs[[j]],"voltage.score"] * avg.weights[j]

        if(full){
          pc1x.avg <- pc1x.avg + abs(dataset_imbalance_ft[avgs[[j]],"current.pc1x"]) * avg.weights[j]
          if(dataset_imbalance_ft[avgs[[j]],"current.pc1x"] > 0){
            sign.x <- sign.x + 1
          }
          else{
            sign.x <- sign.x - 1
          }
          pc1y.avg <- pc1y.avg + abs(dataset_imbalance_ft[avgs[[j]],"current.pc1y"]) * avg.weights[j]
          if(dataset_imbalance_ft[avgs[[j]],"current.pc1y"] > 0){
            sign.y <- sign.y + 1
          }
          else{
            sign.y <- sign.y - 1
          }


          v.pc1x.avg <- v.pc1x.avg + abs(dataset_imbalance_ft[avgs[[j]],"voltage.pc1x"]) * avg.weights[j]
          if(dataset_imbalance_ft[avgs[[j]],"voltage.pc1x"] > 0){
            v.sign.x <- v.sign.x + 1
          }
          else{
            v.sign.x <- v.sign.x - 1
          }
          v.pc1y.avg <- v.pc1y.avg + abs(dataset_imbalance_ft[avgs[[j]],"voltage.pc1y"]) * avg.weights[j]
          if(dataset_imbalance_ft[avgs[[j]],"voltage.pc1y"] > 0){
            v.sign.y <- v.sign.y + 1
          }
          else{
            v.sign.y <- v.sign.y - 1
          }
        }

      }
      if(full){
        if(sign.x < 0){
          pc1x.avg = pc1x.avg*-1
        }
        if(sign.y < 0){
          pc1y.avg = pc1y.avg*-1
        }

        if(v.sign.x < 0){
          v.pc1x.avg = v.pc1x.avg*-1
        }
        if(v.sign.y < 0){
          v.pc1y.avg = v.pc1y.avg*-1
        }
      }
    }

    if(full){
      attr.pc1x <- c(attr.pc1x, el$current.pc1x)
      attr.pc1y <- c(attr.pc1y, el$current.pc1y)
      attr.pc1x.avg <- c(attr.pc1x.avg, pc1x.avg)
      attr.pc1y.avg <- c(attr.pc1y.avg, pc1y.avg)
      attr.v.pc1x <- c(attr.v.pc1x, el$voltage.pc1x)
      attr.v.pc1y <- c(attr.v.pc1y, el$voltage.pc1y)
      attr.v.pc1x.avg <- c(attr.v.pc1x.avg, v.pc1x.avg)
      attr.v.pc1y.avg <- c(attr.v.pc1y.avg, v.pc1y.avg)
    }

    attr.score <- c(attr.score, el$current.score)

    attr.score.avg <- c(attr.score.avg, score.avg)

    attr.v.score <- c(attr.v.score, el$voltage.score)
    attr.v.score.avg <- c(attr.v.score.avg, v.score.avg)

    # attr.class <- c(attr.class, commonInfo[[idx]]$class[[i]])
    # attr.desc <- c(attr.desc, commonInfo[[idx]]$datasetLabels[[i]])
  }
  if(full){
    dataset_imbalance_ft$current.pc1x <- attr.pc1x
    dataset_imbalance_ft$current.pc1y <- attr.pc1y
    dataset_imbalance_ft$current.pc1x.avg <- attr.pc1x.avg
    dataset_imbalance_ft$current.pc1y.avg <- attr.pc1y.avg
    dataset_imbalance_ft$voltage.pc1x <- attr.v.pc1x
    dataset_imbalance_ft$voltage.pc1y <- attr.v.pc1y
    dataset_imbalance_ft$voltage.pc1x.avg <- attr.v.pc1x.avg
    dataset_imbalance_ft$voltage.pc1y.avg <- attr.v.pc1y.avg
  }

  dataset_imbalance_ft$current.score <- attr.score

  dataset_imbalance_ft$current.score.avg <- attr.score.avg


  dataset_imbalance_ft$voltage.score <- attr.v.score

  dataset_imbalance_ft$voltage.score.avg <- attr.v.score.avg

  if(!full){
    return(dataset_imbalance_ft[,c("current.score","current.score.avg","voltage.score","voltage.score.avg","label","_class")])
  }

  return(dataset_imbalance_ft)
}

#' @export
apply_DQImb_ABCImb_Transform <- function(res){

  ABCImbalance <- rearmUtils::genABC_Imbalance_features(res, T)
  ABCImbalance.outliers <- boxplot.stats(ABCImbalance$current.rmssd)$out
  ABCImbalance.validIndices <- !ABCImbalance$current.rmssd %in% ABCImbalance.outliers
  ABCImbalance.removedOut <- ABCImbalance[ABCImbalance.validIndices,]
  #nrow(ABCImbalance.removedOut) #1895


  DQImbalance <- rearmUtils::genDQ_Imbalance_features(res, T)
  DQImbalance.outliers <- boxplot.stats(DQImbalance$current.score)$out
  DQImbalance.validIndices <- !DQImbalance$current.score %in% DQImbalance.outliers
  DQImbalance.removedOut <- DQImbalance[DQImbalance.validIndices,]
  #nrow(DQImbalance.removedOut) #1902

  ##### 6. ABC Imbalance, RMS per phase, Imbalance average plus RMS per phase average ----
  ABCImbalance_avgs <- rearmUtils::genABC_Imbalance_withAvgs_features(dataset_imbalance_ft = ABCImbalance.removedOut, RMSPerPhaseAvg = T)
  ABCImbalance_avgs.noNa <- ABCImbalance_avgs[complete.cases(ABCImbalance_avgs),]
  #nrow(ABCImbalance_avgs.noNa) #1487


  ##### 7. DQ Imbalance plus Imbalance average -----
  DQImbalance_avgs <- rearmUtils::genDQ_Imbalance_withAvgs_features(dataset_imbalance_ft = DQImbalance.removedOut, full = T)
  DQImbalance_avgs.noNa <- DQImbalance_avgs[complete.cases(DQImbalance_avgs),]
  #nrow(DQImbalance_avgs.noNa) #1494


  ##### 8. Dataset 6 plus Dataset 7 -----
  normByPeak.valid.idxs <- DQImbalance.validIndices & ABCImbalance.validIndices

  ABCImbalance.validJoinedIdxs <- ABCImbalance[normByPeak.valid.idxs,]

  DQImbalance.validJoinedIdxs <- DQImbalance[normByPeak.valid.idxs,]

  ABCImbalance_avgs.validJoinedIdxs <- rearmUtils::genABC_Imbalance_withAvgs_features(dataset_imbalance_ft = ABCImbalance.validJoinedIdxs, RMSPerPhaseAvg = T)
  ABCImbalance_avgs.validJoinedIdxs$class <- NULL
  ABCImbalance_avgs.validJoinedIdxs.noNA <- ABCImbalance_avgs.validJoinedIdxs[complete.cases(ABCImbalance_avgs.validJoinedIdxs),]
  names(ABCImbalance_avgs.validJoinedIdxs.noNA) <- paste0("ABC.",names(ABCImbalance_avgs.validJoinedIdxs.noNA))

  DQImbalance_avgs.validJoinedIdxs <- rearmUtils::genDQ_Imbalance_withAvgs_features(dataset_imbalance_ft = DQImbalance.validJoinedIdxs, full=T)
  DQImbalance_avgs.validJoinedIdxs.noNA <- DQImbalance_avgs.validJoinedIdxs[complete.cases(DQImbalance_avgs.validJoinedIdxs),]
  names(DQImbalance_avgs.validJoinedIdxs.noNA) <- paste0("DQ.",names(DQImbalance_avgs.validJoinedIdxs.noNA))

  DQImbABCImbPlus <- cbind(ABCImbalance_avgs.validJoinedIdxs.noNA, DQImbalance_avgs.validJoinedIdxs.noNA)

  return(DQImbABCImbPlus)
}

#' Converts the hexadecimal representation of the color to its rgba representation
#' @param hex hexadecimal representation of the color
#' @export
color2rgba <- function(hex, a, scale = 1){
  hex2rgbI <- col2rgb(hex)
  cl1 <- paste0("rgba(",hex2rgbI[1]/scale,",",hex2rgbI[2]/scale,",",hex2rgbI[3]/scale,",",a,")")
  return(cl1)
}

#' Converts the hexadecimal color to a new hexadecimal color that is the hexadecimal input with a given transparency.
#' @param hex hexadecimal representation of the color
#' @param a transparency
#' @export
colorHexAlpha <- function(hex, a){
  #rearmUtils::color2rgba(hex, a, scale = 255)
  hex2rgbI <- col2rgb(hex)/255
  r <- a * hex2rgbI[1] + (1 - a) * 1
  g <- (1 - a) * 1 + a * hex2rgbI[2]
  b <- (1 - a) * 1 + a * hex2rgbI[3]
  return(rgb(r,g,b))
}

