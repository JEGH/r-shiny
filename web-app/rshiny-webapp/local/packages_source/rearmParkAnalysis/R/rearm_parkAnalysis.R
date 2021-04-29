devtools::use_package("rearmSignalProcessing")




transform_dqPoints_to_eigenVector <- function(parkTransfCycle, fnEccentricity = NULL){
  if(is.null(fnEccentricity)){
    fnEccentricity <- function(majAxisMag, minAxisMag){
      return(
        round(majAxisMag-minAxisMag,3)
      )
    }
  }
  currMat <- as.matrix(data.frame(parkTransfCycle$d, parkTransfCycle$q))
  resPCA <- prcomp(currMat)
  resPCA.sum <- summary(resPCA)
  # score <- round(resPCA$sdev[1] - resPCA$sdev[2],3)# resPCA.sum$importances
  score <- fnEccentricity(resPCA$sdev[1], resPCA$sdev[2])
  return(
    list(
      score = score,
      pc1mag = resPCA$sdev[1],
      pc2mag = resPCA$sdev[2],
      pc1 = list(
        # x = c(0,resPCA$rotation[1,1]*resPCA$sdev[1]),
        # y = c(0, resPCA$rotation[2,1])*resPCA$sdev[1]
        x = c(0,resPCA$rotation[1,1]),
        y = c(0, resPCA$rotation[2,1])
        #,name = paste0(labels[[index]]," pc1 ::",score),
        #mode = 'lines',
        #line = list(color = colors[index],
        #            width = 2,
        #            dash = 'dash')
      ),
      pc2 = list(
        # x = c(0,resPCA$rotation[1,2]*resPCA$sdev[2]),
        # y = c(0, resPCA$rotation[2,2])*resPCA$sdev[2]
        x = c(0,resPCA$rotation[1,2]),
        y = c(0, resPCA$rotation[2,2])
        #,name = paste0(labels[[index]]," pc2 ::",score),
        #mode = 'lines',
        #line = list(color = colors[index],
        #            width = 2)
      )
    )
  )
}

ellipse.fit <- function (x, y = NULL) {
  # from:
  # http://r.789695.n4.nabble.com/Fitting-a-half-ellipse-curve-tp2719037p2720560.html
  #
  # Least squares fitting of an ellipse to point data
  # using the algorithm described in:
  #   Radim Halir & Jan Flusser. 1998.
  #   Numerically stable direct least squares fitting of ellipses.
  #   Proceedings of the 6th International Conference in Central Europe
  #   on Computer Graphics and Visualization. WSCG '98, p. 125-132
  #
  # Adapted from the original Matlab code by Michael Bedward (2010)
  # michael.bedward@gmail.com
  #
  # Subsequently improved by John Minter (2012)
  #
  # Arguments:
  # x, y - x and y coordinates of the data points.
  #        If a single arg is provided it is assumed to be a
  #        two column matrix.
  #
  # Returns a list with the following elements:
  #
  # coef - coefficients of the ellipse as described by the general
  #        quadratic:  ax^2 + bxy + cy^2 + dx + ey + f = 0
  #
  # center - center x and y
  #
  # major - major semi-axis length
  #
  # minor - minor semi-axis length
  #
  EPS <- 1.0e-8
  dat <- xy.coords(x, y)

  D1 <- cbind(dat$x * dat$x, dat$x * dat$y, dat$y * dat$y)
  D2 <- cbind(dat$x, dat$y, 1)
  S1 <- t(D1) %*% D1
  S2 <- t(D1) %*% D2
  S3 <- t(D2) %*% D2 #equation 23 of the paper
  T <- -solve(S3) %*% t(S2) #equation 24 of the paper
  # -solve(S3) is the inverse of S3
  M <- S1 + S2 %*% T #equation 29 of the paper
  M <- rbind(M[3,] / 2, -M[2,], M[1,] / 2)
  #M is a 3x3 scatter matrix and the divisions come from eq 18, because of the constraints matrix C1 from eq 29
  evec <- eigen(M)$vec
  cond <- 4 * evec[1,] * evec[3,] - evec[2,]^2
  a1 <- evec[, which(cond > 0)]
  f <- c(a1, T %*% a1)
  names(f) <- letters[1:6]

  # calculate the center and lengths of the semi-axes
  #
  # see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2288654/
  # J. R. Minter
  # for the center, linear algebra to the rescue
  # center is the solution to the pair of equations
  # 2ax +  by + d = 0
  # bx  + 2cy + e = 0
  # or
  # | 2a   b |   |x|   |-d|
  # |  b  2c | * |y| = |-e|
  # or
  # A x = b
  # or
  # x = Ainv b
  # or
  # x = solve(A) %*% b
  A <- matrix(c(2*f[1], f[2], f[2], 2*f[3]), nrow=2, ncol=2, byrow=T )
  b <- matrix(c(-f[4], -f[5]), nrow=2, ncol=1, byrow=T)
  soln <- solve(A) %*% b

  b2 <- f[2]^2 / 4

  center <- c(soln[1], soln[2])
  names(center) <- c("x", "y")

  num  <- 2 * (f[1] * f[5]^2 / 4 + f[3] * f[4]^2 / 4 + f[6] * b2 - f[2]*f[4]*f[5]/4 - f[1]*f[3]*f[6])
  den1 <- (b2 - f[1]*f[3])
  den2 <- sqrt((f[1] - f[3])^2 + 4*b2)
  den3 <- f[1] + f[3]

  semi.axes <- sqrt(c( num / (den1 * (den2 - den3)),  num / (den1 * (-den2 - den3)) ))

  # calculate the angle of rotation
  term <- (f[1] - f[3]) / f[2]
  angle <- atan(1 / term) / 2

  list(coef=f, center = center, major = max(semi.axes), minor = min(semi.axes), angle = unname(angle))
}

ellipse.get <- function( fit, n=20 )
{
  # Calculate points on an ellipse described by
  # the fit argument as returned by fit.ellipse
  #
  # n is the number of points to render

  tt <- seq(0, 2*pi, length=n)
  sa <- sin(fit$angle)
  ca <- cos(fit$angle)
  ct <- cos(tt)
  st <- sin(tt)

  x <- fit$center[1] + fit$maj * ct * ca - fit$min * st * sa
  y <- fit$center[2] + fit$maj * ct * sa + fit$min * st * ca

  cbind(x=x, y=y)
}

conic.eccentricity <- function(coefs){
  mew.matrix <- matrix(
    c(coefs["a"], coefs["b"]/2, coefs["d"]/2, coefs["b"]/2, coefs["c"], coefs["e"]/2,coefs["d"]/2, coefs["e"]/2, coefs["f"]),
    nrow=3,
    ncol=3
  )
  mew.matrix.det <- det(mew.matrix)
  mew <- ifelse(mew.matrix.det>0, -1, 1)
  numerator <- 2*sqrt((coefs["a"]-coefs["c"])^2 + coefs["b"]^2)
  denominator <- mew*(coefs["a"]+coefs["c"]) + sqrt((coefs["a"]-coefs["c"])^2 + coefs["b"]^2)
  return(sqrt(numerator/denominator))
}

denormalizeVector <- function(v, mag){
  v.theta <- atan2(v$y[2],v$x[2])
  v.r <- mag

  v$x[2] <- v.r*cos(v.theta)
  v$y[2] <- v.r*sin(v.theta)
  return(v)
}

parseEigenAnalysisToPlot_ly <- function(el, label, color, denormalizePC){

  if(denormalizePC){
    el$pc1 <- denormalizeVector(el$pc1, el$pc1mag)
    el$pc2 <- denormalizeVector(el$pc2, el$pc2mag)
  }
  el$pc1$r <- sqrt(el$pc1$x[2]^2 + el$pc1$y[2]^2)
  el$pc2$r <- sqrt(el$pc2$x[2]^2 + el$pc2$y[2]^2)

  newEl <- list(
    pc1 = list(
      x = el$pc1$x,
      y = el$pc1$y,
      # name = paste0(labels[[index]]," | pc1 ::",el$score),
      name = paste0(label," | pc1 ::",round(el$pc1$r,digits=3)),
      mode = 'lines',
      type = 'scatter',
      line = list(
        width = 2,
        dash = 'dash',
        color = color
      )
    ),
    pc2 = list(
      x = el$pc2$x,
      y = el$pc2$y,
      name = paste0(label," | pc2 ::",round(el$pc2$r,digits=3)),
      mode = 'lines',
      type = 'scatter',
      line = list(
        width = 2,
        color = color
      )
    )
  )
  return(newEl)
}

parseParkTransfToPlot_ly <- function(el, label, color){
  return(
    list(
      type = 'scatter',
      x = el$d,
      y = el$q,
      mode = 'lines',
      name = label,
      line = list(
        color = colors
      )
    )
  )
}


eigenTransform <- function(ds, labels = NULL, color = NULL, list2plot = T, denormalizePC = F, fnEccentricity = NULL){
  EigenAnalysisAllDS <- lapply(
    ds,
    transform_dqPoints_to_eigenVector,
    fnEccentricity = fnEccentricity
  )


  data_transformed <- lapply(
    seq_along(EigenAnalysisAllDS),
    function(idx, ds, labels, color){
      el <- ds[[idx]]
      r <- sqrt(el$pc1$x[2]^2 + el$pc1$y[2]^2)
      theta <- atan(el$pc1$y[2]/el$pc1$x[2])

      eigenplot <- NULL
      if(list2plot){
        eigenplot <- parseEigenAnalysisToPlot_ly(el, labels[[idx]], color, denormalizePC)
      }

      return(
        list(
          score = unname(el$score),
          ellipse.rotationAngle = theta,
          sdev = c(el$pc1mag, el$pc2mag),
          pc1 = denormalizeVector(el$pc1, el$pc1mag),
          pc2 = denormalizeVector(el$pc2, el$pc2mag),
          rotation = matrix(c(el$pc1$x[2], el$pc1$y[2], el$pc2$x[2], el$pc2$y[2]), nrow=2),
          eigen.list2plot = eigenplot
        )
      )
    },
    ds = EigenAnalysisAllDS,
    labels = labels,
    color = color
  )


  return(
    data_transformed
  )

}

ellipseTransform <- function(ds, x="d", y="q", labels = NULL, color = NULL, list2plot = T, ellipse.eccentricity.fn = NULL){
  if(is.null(ellipse.eccentricity.fn)){
    ellipse.eccentricity.fn <- function(maj, min){
      return(sqrt(1-(min^2/maj^2)))
    }
  }
  ds.ellipse <- lapply(
    seq_along(ds),
    function(idx, ds, color, labels){
      el <- ds[[idx]]
      label <- labels[[idx]]
      fitted <- rearmParkAnalysis::ellipse.fit(x=el[[x]], y=el[[y]])
      fittedEccentricity <- rearmParkAnalysis::conic.eccentricity(fitted$coef)
      mat <- rearmParkAnalysis::ellipse.get(fitted)
      major.x <- c(0,fitted$major*cos(fitted$angle))
      major.y <- c(0,fitted$major*sin(fitted$angle))
      minor.x <- c(0,fitted$minor*cos(fitted$angle + pi/2))
      minor.y <- c(0,fitted$minor*sin(fitted$angle + pi/2))
      list2plot.points <- NULL
      list2plot.axis <- NULL
      if(list2plot){
        list2plot.points <- list(
          x = mat[,1], y = mat[,2], mode='lines', type="scatter", line = list(color = color),
          name = paste0("EllipseFit::",label)
        )
        list2plot.axis <- list(
          major = list(
            x=major.x, y = major.y, name = paste0("Major::", round(fitted$major,digits=3)),
            mode = 'lines', type = 'scatter', line = list(color = color, dash = 'dash')
          ),
          minor = list(
            x=minor.x, y = minor.y, name = paste0("Minor::", round(fitted$minor,digits=3)),
            mode = 'lines', type = 'scatter', line = list(color = color)
          )
        )
      }

      return(
        list(
          conic.eccentricity = fittedEccentricity,
          ellipse.eccentricity = ellipse.eccentricity.fn(fitted$major, fitted$minor),
          rotationAngle = fitted$angle,
          major = fitted$major,
          minor = fitted$minor,
          list2plot.points = list2plot.points,
          list2plot.axis = list2plot.axis,
          coeffs = fitted$coef
        )
      )
    },
    ds = ds,
    color = color,
    labels = labels
  )
  return(ds.ellipse)

}
