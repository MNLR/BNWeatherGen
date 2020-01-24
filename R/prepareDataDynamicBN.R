#' prepareDataDinamicBN()
#' @title Prepare Datasets for Dinamic Bayesian Network.
#' @param data Expects output from \code{\link[downscaleR.BN]{prepare_predictors.forBN()}}.
#' @author MN Legasa

prepareDataDynamicBN <- function(data_, epochs){

  if (class(data_) == "splitSpellsNA"){
    dates <- data_[[1]]$dates
    data.list <- lapply(data_, function(sp, epochs) {
                          if (nrow(sp$data) < epochs) {
                            return(NULL)
                          } else {
                            return(sp)
                          }
                         }, epochs = epochs
            )
    data.list[sapply(data.list, is.null)] <- NULL
    proc <- list()
    for (i in 1:length(data.list)){
      proc[[i]] <- prepareDataDynamicBN(data.list[[i]], epochs = epochs)
    }
    data_out <- proc[[1]]
    data_ <- lapply(proc, FUN = function(x) {return(x$data)} )
    data_out$data <- do.call( rbind, data_ )
    data_out[["dates"]] <- dates$start[as.numeric(rownames(data_out$data))]
    rownames(data_out$data) <- dates$start[as.numeric(rownames(data_out$data))]

    return(data_out)
  }
  else {
  # Data conversion, expects either matrix  or data.frame (2dim)
  dinamic.data <- data_$data[ 1:( nrow(data_$data)-(epochs-1) ) , , drop = FALSE]
  names_ <- colnames(data_$data)
  nvars <- length(names_)
  layers <- rep(0, nvars)

  if (data_$nx != 0){
    names.distribution <- list( list(x.names = as.vector(mapply( FUN = function(node, time) {
      return(paste0(node, paste0(".T", time))) }, node = data_$x.names, time = rep(0, data_$nx) )),
                                     y.names = as.vector(mapply( FUN = function(node, time) {
                                       return(paste0(node, paste0(".T", time)))
                                     }, node = data_$y.names, time = rep(0, data_$ny)) )
                                    )
                              )

    for (epoch in 1:(epochs-1)) {
      dinamic.data <- cbind.data.frame( dinamic.data, data_$data[ (epoch+1):(nrow(data_$data)-(epochs-1-epoch)) , ] )
      names_ <- c(names_, colnames(data_$data))
      layers <- c(layers, rep(epoch, nvars))
      names.distribution[[epoch + 1]] <- list(
                                            x.names = as.vector(mapply(
                                            FUN = function(node, time) {
                                                    return(paste0(node, paste0(".T", time)))
                                                  },
                                            node = data_$x.names,
                                            time = rep(epoch, data_$nx),
                                            SIMPLIFY = TRUE
                                            )),
                                            y.names = as.vector(mapply(
                                              FUN = function(node, time) {
                                                      return(paste0(node,
                                                        paste0(".T", time))
                                                      )
                                                    },
                                              node = data_$y.names, time = rep(epoch, data_$ny)
                                              )
                                              )
                                            )
    }
    colnames(dinamic.data) <- mapply(FUN = function(node, time) {
                                             return(paste0(node, paste0(".T", time)))
                                           }, node = names_, time = layers
                                     )
  }
  else {
    names.distribution <- rep(list(list(y.names = names_)), epochs)
    for (epoch in 1:(epochs)){
      if (epoch != epochs){
        dinamic.data <- cbind.data.frame( dinamic.data,
                                          data_$data[ (epoch+1):(nrow(data_$data)-(epochs-1-epoch)), , drop = FALSE]
                                        )
        layers <- c(layers, rep(epoch, nvars))
      }
      epnames <- paste0(names.distribution[[epochs]]$y.names, paste0(".T", epoch-1))
      names.distribution[[epoch]]$y.names <- epnames
    }
    colnames(dinamic.data) <- unlist(names.distribution)
  }

  data_$data <- dinamic.data

  # Others
  data_[["names.distribution"]] <- names.distribution
  data_[["x.names"]] <- NULL
  data_[["y.names"]] <- NULL
  data_[["positions"]] <- t(rep(1, epochs) %x% t(data_$positions))
  colnames(data_$positions) <- colnames(dinamic.data)
  rownames(data_$positions) <- c("x","y")
  class(data_) <- "pp.forDynBN"

  return(data_)
  }
}
