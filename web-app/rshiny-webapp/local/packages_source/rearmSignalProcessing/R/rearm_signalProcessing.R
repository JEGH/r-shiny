devtools::use_package("hht")
devtools::use_package("plotly")

#' @export
rms <- function(data){
  return(sqrt(mean(data^2)))
}

#' @export
transform_park <- function(a, b, c, rmsValue = 1){
  d <- sqrt(2/3)*a - sqrt(1/6)*b - sqrt(1/6)*c
  q <- sqrt(1/2)*b - sqrt(1/2)*c
  return(
    list(
      d = d/rmsValue,
      q = q/rmsValue
    )
  )
}

#' @export
transform_hilbert <- function(a, b, c){
  return(
    list(
      a = hht::HilbertTransform(a),
      b = hht::HilbertTransform(b),
      c = hht::HilbertTransform(c)
    )
  )
}

#' @export
extended_park_vector <- function(d, q){
  return(
    sqrt(d^2 + q^2)
  )
}

#==========parseFFTToPlot#==========#
# Input:
#   - fft_matrix: FFT Matrix (the output of the fft function)
#   - Fs: sample frequency of the signal
#   - L: length of the fitted signal
# Output:
#   - Returns a dataframe with the wave frequencies and wave amplitudes identified in the fft_matrix
#' @export
parseFFTToPlot <- function(fft_matrix, Fs, L){
  P2 <- abs(fft_matrix/L);
  P1 = P2[seq(1,L/2+1)]; #nyquist
  P1[seq(2,length(P1)-1)] = 2*P1[seq(2,length(P1)-1)];
  result <- data.frame("frequency" =  Fs*(0:(L/2))/L, "ampere" = P1)
  return(result)
}
#**********parseFFTToPlot**********#


#==========fftParsed#==========#
# Input:
#   - signal: the signal which will be fitted for fft
#   - Fs: sample frequency of the signal
#   - L: length of the fitted signal
# Output:
#   - Returns a dataframe with the wave frequencies and wave amplitudes identified in the fft_matrix
#' @export
fftParsed <- function(signal, Fs){
  calc_fft <- fft(signal, inverse = FALSE)
  return(
    parseFFTToPlot(
      calc_fft,
      Fs,
      length(signal)
    )
  )
}
#**********fftParsed**********#

#' @export
plot_pvmL <- function(elList, layoutTitle, layoutXname = 'Time', layoutYname = 'PVM Module'){
  plotListLayout(elList, layoutTitle, layoutXname, layoutYname)
}

#' @export
plot_pvm <- function(iSigA, iSigB, iTimeA, iTimeB, iLabelSigA, iLabelSigB){

  viz <- list()
  viz[[length(viz)+1]] = list(
    x = iTimeA,
    y = iSigA,
    name = iLabelSigA,
    mode = 'lines+markers'
  )
  viz[[length(viz)+1]] = list(
    x = iTimeB,
    y = iSigB,
    name = iLabelSigA,
    mode = 'lines+markers'
  )
  return(
    plot_pvmL(
      viz,
      paste0("PVM ", iLabelSigA, " VS ", iLabelSigB)
    )
  )
}

#' @export
plot_fftL <- function(elList, layoutTitle, layoutXname = 'Frequency', layoutYname = 'Amplitude'){
  plotListLayout(elList, layoutTitle, layoutXname, layoutYname)
}

#' @export
plot_fft <- function(iSigA, iSigB, iLabelSigA, iLabelSigB){

  viz <- list()
  viz[[length(viz)+1]] = list(
    x = iSigA$frequency,
    y = iSigA$ampere,
    name = iLabelSigA,
    mode = 'lines+markers'
  )
  viz[[length(viz)+1]] = list(
    x = iSigB$frequency,
    y = iSigB$ampere,
    name = iLabelSigA,
    mode = 'lines+markers'
  )
  return(
    plot_fftL(
      viz,
      paste0("FFT ", iLabelSigA, " VS ", iLabelSigB)
    )
  )
}

#' @export
plot_park_transformL <- function(elList, layoutTitle, layoutXname = 'd component', layoutYname = 'q component'){
  plotListLayout(elList, layoutTitle, layoutXname, layoutYname)
}

#' @export
plot_park_transform <- function(iParkTransA, iParkTransB, iLabelA, iLabelB){
  viz <- list()
  viz[[length(viz)+1]] = list(
    x = iParkTransA$d,
    y = iParkTransA$q,
    name = iLabelA,
    mode = 'lines'
  )
  viz[[length(viz)+1]] = list(
    x = iParkTransB$d,
    y = iParkTransB$q,
    name = iLabelB,
    mode = 'lines'
  )
  return(
    plot_park_transformL(
      viz,
      paste0("Park Transform Space ", iLabelA, " VS ", iLabelB)
    )
  )
}

#' @export
parseSignalToPlot <- function(index,dataset,datalabels,datacolors, mode,x,y){
  element <- dataset[[index]]
  label <- datalabels[[index]]
  list(
    x = element[[x]],
    y = element[[y]],
    name = datalabels[[index]],
    mode = mode,
    line = list(color = datacolors[index],
                width = 2)
  )
}

reducePlotly <- function(acc, curr){
  do.call(add_trace,c(list(p=acc),curr))
}

plotListLayout<- function(elList, layoutTitle, layoutXname, layoutYname){
  plotly::layout(
    Reduce(reducePlotly,elList,init=plotly::plot_ly()),
    title= layoutTitle,
    xaxis= list(
      title= layoutXname
    ),
    yaxis=list(
      title= layoutYname
    )
  )
}

#' @export
addNoise <- function(dataset, magnitude = NULL){
  #Assumes that the schema of the dataset is:
  #time, currentPhaseR, currentPhaseS, currentPhaseT,
  #voltageRS, voltageST, voltageTR
  #Only add noise to current

  columnsToConsider <- c("currentPhaseR","currentPhaseS","currentPhaseT")


  if(is.null(magnitude)){

    calcMinMax <- lapply(
      columnsToConsider,
      function(column, ds){
        return(
          list(
            max = max(ds[[column]]),
            min = min(ds[[column]])
          )
        )
      },
      dataset
    )

    calcAmpl <- unlist(
      lapply(
        calcMinMax,
        function(el){
          el$max - el$min
        }
      ),
      recursive = F
    )

    calcMeanAmpl <- mean(calcAmpl)
    magnitude <- calcMeanAmpl*0.01
  }

  randMatrix = matrix(runif(prod(dim(dataset)), min = -1*magnitude, max = magnitude), nrow = nrow(dataset))

  dataset[,columnsToConsider] = dataset[,columnsToConsider] +
    randMatrix
  return(dataset)
}

#' Computes the Park Transform for the given dataset and returns the signal (d/q attriutes) and a list with plotly properties to be plotted
#'
#' The dataset parameter is a list of lists containing, at leat, the 3 phase signal features.
#' This functions uses `lapply` to iterator over the dataset list, and for each
#' element computes the park transform with the function `rearmSignalProcessing::transform_park`.
#' After the park transform, a list of plotly properties is created so that each
#' entity can be plotted easily.
#' @param dataset The set of entities (list)
#' @param motorNominalRMS A list with the nominal value of the signal for normalization purposes (character)
#' @param threePhaseAttributes The vector with the 3 phase attributes for the entities in @param dataset (character)
#' @param labels A list of labels for each entity in @param dataset
#' @param color The color associated with all the entities of the @param dataset
#' @return A list with the d/q signal list and the list2plot list
#' @export
process_park_transform <- function(dataset, motorNominalRMS, threePhaseAttributes = c('currentPhaseR','currentPhaseS','currentPhaseT'), timeAttribute = "time", labels = NULL, color = NULL){

  dataParkTransformed <- lapply(
    seq_along(dataset),
    function(idx, ds, rms, labels, colors){
      el <- ds[[idx]]
      label <- labels[[idx]]
      park_transformed <- rearmSignalProcessing::transform_park(
        el[[threePhaseAttributes[1]]],
        el[[threePhaseAttributes[2]]],
        el[[threePhaseAttributes[3]]],
        rms[[idx]]
      )

      park_transformed$time <- el[[timeAttribute]]

      list2plot <- list(
        type = 'scatter',
        x = park_transformed$d,
        y = park_transformed$q,
        mode = 'lines',
        name = label,
        line = list(
          color = color
        )
      )

      return(
        list(
          signal = park_transformed,
          list2plot = list2plot
        )
      )
    },
    ds = dataset,
    rms = motorNominalRMS,
    labels = labels,
    colors = colors
  )


  return(dataParkTransformed)
}

#' Computes the Park Vector Module of the given dataset and returns the signal and a list with plotly properties to be plotted
#'
#' The dataset parameter is a list of lists containing the 2 (d/q or alpha/beta) features.
#' This functions uses `lapply` to iterator over the dataset list, and for each
#' element computes the vector modulus with the function `rearmSignalProcessing::extended_park_vector`.
#' After the vector modulus, a list of plotly properties is created so that each
#' entity can be plotted easily.
#' @param dataset The set of entities (list)
#' @param alphaBetaAttributes The vector with the d/q attributes for the entities in @param dataset (character)
#' @param timeAttribute The string with the time attributes for the entities in @param dataset (character)
#' @param signalAttribute If the entity is itself the signal, then this param is null. Else, this param must indicate the attribute in which the signal is.
#' @param labels A list of labels for each entity in @param dataset
#' @param color The color associated with all the entities of the @param dataset
#' @return A list with the pvm signal list and the list2plot list
#' @export
process_extended_park_vector <- function(dataset, alphaBetaAttributes = c('d', 'q'),
           timeAttribute = "time", signalAttribute = NULL, labels = NULL, color = NULL){
  dataPVM <- lapply(
    seq_along(dataset),
    function(idx, ds, labels, colors){
      if(is.null(signalAttribute)){
        el <- ds[[idx]]
      }else{
        el <- ds[[idx]][[signalAttribute]]
      }
      label <- labels[[idx]]
      pvm <- rearmSignalProcessing::extended_park_vector(
        el[[alphaBetaAttributes[1]]],
        el[[alphaBetaAttributes[2]]]
      )

      list2plot <- list(
        type = 'scatter',
        x = el[[timeAttribute]],
        y = pvm,
        mode = 'lines',
        name = label,
        line = list(
          color = color
        )
      )

      return(
        list(
          signal = pvm,
          list2plot = list2plot
        )
      )

    },
    ds = dataset,
    labels = labels,
    colors = colors
  )
  return(dataPVM)
}

process_fft <- function(dataset, signalAttribute = NULL,  Fs, labels = NULL, color = NULL){
  dataFFT <- lapply(
    seq_along(dataset),
    function(idx, ds){
      if(is.null(signalAttribute)){
        el <- ds[[idx]]
      }else{
        el <- ds[[idx]][[signalAttribute]]
      }
      label <- labels[[idx]]
      fftdf <- fftParsed(el, Fs[[idx]])

      list2plot <- list(
        type = 'scatter',
        x = fftdf[-c(1),]$frequency,
        y = fftdf[-c(1),]$ampere,
        mode = 'lines',
        name = label,
        line = list(
          color = color
        )
      )

      return(
        list(
          list2plot = list2plot,
          signal = fftdf
        )
      )

    },
    ds = dataset
  )
}

shiftMaxPhaseAToZero <- function(ds, threePhaseCurr = c('currentPhaseR','currentPhaseS','currentPhaseT'), phaseStartMax = 1){

  startIdx <- which(max(ds[[threePhaseCurr[phaseStartMax]]]) == ds[[threePhaseCurr[phaseStartMax]]])[1]
  limitIdx <- length(ds[[threePhaseCurr[phaseStartMax]]])
  finished <- F
  i <- 1

  newEl <- data.frame(
    currentPhaseR = ds[[threePhaseCurr[1]]][startIdx],
    currentPhaseS = ds[[threePhaseCurr[2]]][startIdx],
    currentPhaseT = ds[[threePhaseCurr[3]]][startIdx]
  )


  while(!finished) {
    curr <- (startIdx+i)%%limitIdx
    if(curr == 0){
      curr <- limitIdx
    }

    newEl <- rbind(
      newEl,
      data.frame(
        currentPhaseR = ds[[threePhaseCurr[1]]][curr],
        currentPhaseS = ds[[threePhaseCurr[2]]][curr],
        currentPhaseT = ds[[threePhaseCurr[3]]][curr]
      )
    )
    i <- i + 1
    if(i == limitIdx){
      finished <- T
    }
  }
  names(newEl) <- threePhaseCurr
  return(newEl)
}

orderBytMaxPhaseAFirst <- function(ds, threePhaseCurr = c('currentPhaseR','currentPhaseS','currentPhaseT')){

  startIdx <- which(max(ds$currentPhaseR) == ds[[threePhaseCurr[1]]])[1]
  limitIdx <- length(ds$currentPhaseR)

  order <- startIdx:limitIdx

  if(startIdx!=1){
    order <- c(order, 1:(limitIdx-(limitIdx-startIdx+1)))
  }
  return(order)
}
