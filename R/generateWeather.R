#' @title Generate multivariate weather series using a Bayesian Network Weather Generator.
#' @description Uses a Bayesian network model, output from \code{buildWeatherGenerator()}
#'  to simulate a multivariate series. If the model was learnt without predictors, x
#'  must be NULL. Otherwise x is required. 
#' @param wg Bayesian network weather generator model, as output from \code{buildWeatherGenerator()}
#' @param initial An initial observation, as vector. If \code{NULL}, a random observation will be chosen. 
#' @param n Number of observations to generate.
#' @param x A predictors object (grid)
#' @param inference.type Either \code{"approximate"} or \code{"exact"}. If junction tree is unavailable, exact inference is
#' deactivated
#' @param initial.date A date element. If provided, rownames will be labeled after this date.
#' @param advance.type \code{"simulation"} (default) is used to generate stochastic weather series. 
#' Other options are included for debugging purposes, which are \code{"exact"}, so that the series are generated
#' deterministically using most likely and \code{threshold.vector}.
#' @param threshold.vector Ignored if \code{advance.type = "simulation"}, used as the threshold for deciding
#' if probability value should be 1 or 0.  
#' @param event Name of the positive event.
#' @export

generateWeather <- function(wg, initial = NULL, n = 1, x = NULL,
                            inference.type = NULL,
                            initial.date = NULL,
                            advance.type = "simulation", 
                            threshold.vector = 0.5,
                            resample.size = 10000,
                            event = "1"){
  # expects x in the form prepare_newdata(newdata = tx, predictor = grid)
  # n overriden when x is not NULL.
  BN <- wg$BN
  BN.fit <- wg$BN.fit
  junction <- wg$junction
  epochs <- wg$dynamic.args.list$epochs
  NY <- wg$NY
  NX <- wg$NX

  if (!is.null(x)){
    if (is.list(x)){
      return(
        lapply(x$x.global, FUN = function(member, wg, initial, inference.type,
                                          advance.type) {
                                  return(
                                    generateWeather(wg = wg, initial = initial,
                                                      x = member,
                                                      inference.type = inference.type,
                                                      advance.type = advance.type)
                                  )
                                 },
               wg = wg, initial = initial, inference.type = inference.type,
               advance.type = advance.type
             )
      )
    } else if (!is.matrix(x)){
      stop("Provide a valid x.")
    }
  }

  if (is.null(junction) && !(is.null(inference.type))) {
    message("Warning: Compiling junction tree for weather generators might be (and usually
            is) unefficient.")
    readline(prompt="Press [enter] to continue")

    junction <- compileJunction( BN.fit )
  }

  predictors <- as.vector(unlist(lapply(wg$names.distribution[1:(epochs-1)],
                                        function(x) {return(x$y.names)}
                                        )
                                 )
                          )
  predictands <- as.vector(wg$names.distribution[[length(wg$names.distribution)]]$y.names)

  if (!(is.null(x))) {
    predictorsG <- as.vector(
                    unlist(lapply(wg$names.distribution,
                      function(xx) {
                        return(xx$x.names)
                      }
                      )
                    )
    )

    predictors <- c(predictorsG, predictors)
    past.present.G <- sapply(wg$names.distribution,
                             function(x) return(length(x$x.names))
                             )
  }

  if (is.null(initial)){
    if (is.null(x)){
      initial <- rbn(wg$BN.fit, n=1)[ , unlist(wg$names.distribution[1:(epochs-1)])]
      series <- matrix(toOperableVector(initial), nrow = epochs-1, ncol = NY, byrow = TRUE)
      evidence_ <- c(t(series))
    }
    else {
      x.at <- sum(past.present.G)/NX
      evidence_ <- c( c(t(x))[1:sum(past.present.G)] )
      initial <- queryBN(evidence = evidence_, dbn = wg,
                         evidence.nodes = predictorsG,
                         predictands = as.vector(
                           unlist(wg$names.distribution[1:(length(wg$names.distribution)-1)]
                       )
              ), resample.size = resample.size
      )
      initial <- toOperableVector(
        sapply(initial, function(x) {
          return(names(x)[order(x, decreasing = TRUE)][1] )
          }
        )
      )
      initial <- toOperableMatrix(initial)
      evidence_ <- c(c(t(x[1:x.at, ])), initial)
      n <- nrow(x) - (x.at)
      x <- x[(x.at+1):nrow(x), ]
    }
  }
  else {
    evidence_ <- initial
  }
  series <- toOperableMatrix(initial)
  colnames(series) <- predictands    #evidence_ <- c( c(t(x))[1:sum(past.present.G)], evidence_ )

  step.size <- difftime( rownames(wg$training.data)[2], rownames(wg$training.data)[1] , units = "secs")

  tt_ <- system.time(queryBN(evidence = evidence_, dbn = wg,
                             evidence.nodes = predictors,
                             predictands = predictands, type = advance.type,
                             resample.size = resample.size
                             )
                     )[3]*n
  if (tt_ > 60){
    tt_ <- paste0(list(as.character(floor(tt_/3600)), " hours and ", as.character(
      floor((tt_ - floor(tt_/3600)*3600)/60) ),  " minutes."
      ), collapse = ''
    )
    print(paste0("Process will approximately take ", tt_))
  }

  maux_ <- paste0(paste0("Generating series of ", as.character(n)), " slices ..." )
  print(maux_)
  pb = txtProgressBar(min = 0, max = n, initial = 1, style = 3)
  setTxtProgressBar(pb, 0)

  for (epoch in 1:n){
    simulated <- queryBN(evidence = evidence_, dbn = wg,
                         evidence.nodes = predictors,
                         predictands = predictands, type = advance.type,
                         resample.size = resample.size
                        )
    if (advance.type == "exact"){
      simulated <- simplify2array( sapply(simulated, simplify2array,
                                                simplify = FALSE),
                                         higher = TRUE
                                 )
      simulated <- simulated[ ,match(predictands, colnames(simulated))]
      simulated <- isMostLikelyEvent(simulated, event, threshold.vector)
    }

    series <- rbind(series, toOperableVector(simulated))
    evidence_ <- c(t(series[ (nrow(series)-(epochs-2)):(nrow(series)) , ]))
    if (!is.null(x) && epoch != n){
      evidence_ <- c(t( x[1:x.at, ] ), evidence_)
      x <- x[2:nrow(x), , drop = FALSE]
    }
    setTxtProgressBar(pb, epoch)
  }

  colnames(series) <- unlist(strsplit(predictands,
                                      split = paste0(".T", as.character(epochs-1))
                                      )
                            )

  rownames(series) <- seq(-(epochs-2), n)
  if (!(is.null(initial.date))){
    rownames(series)[1:(nrow(series)-n)] <- as.character(as.POSIXct(initial.date[1:(length(initial.date)-1)],
                                                       format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                            )
    rownames(series)[(nrow(series)-n+1):nrow(series)] <- as.character(as.POSIXct(initial.date[length(initial.date)],
                                                                    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
                                                                    ) + seq(step.size, by = step.size, length.out = n)
                                                          )
  }

  return(series)
}
