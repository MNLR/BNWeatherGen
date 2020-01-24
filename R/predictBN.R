#' @title Downscale with Bayesian Networks
#' @param cbn Climatic Bayesian Network, as returned by either
#'  \code{buildCBN() or buildDynamicCBN()}.
#' @param x may be output of prepareNewData for predictive non-past, NULL for a no-G dynamic
#' CBN or a single dataset for a G dynamic CBN (i.e. dynamic CBN do not accept members).
#' @param y Predictands
#' @param prediction.type Options are \code{"exact"}, \code{"approximate"} and \code{"simulation"}.
#' Exact inference requires a compilable junction.
#' @param output Options are: \code{"probabilities"}, \code{"event"} and
#'  \code{"probabilities.list"}. You should probably not use the last one. Note that if
#'  \code{prediction.type = "simulation"}, output is forced to \code{"event"}.
#' @author M.N. Legasa
#' @importFrom parallel parApply stopCluster
#' @importFrom pbapply pbapply
#' @export

predictBN <- function(cbn, x, y = NULL, output = "probabilities",
                      prediction.type = "exact", event = "1", threshold.vector = NULL,
                      output.attr.evidence = FALSE,
                      cl = NULL, stop.cluster = TRUE, parallelize = FALSE, n.cores = NULL,
                      cluster.type = "FORK"){

  BN <- cbn$BN
  BN.fit <- cbn$BN.fit
  Nglobal <- cbn$NX
  junction <- cbn$junction
  aux <- auxHandlePredictors(cbn, x, y)
  predictors <- aux$predictors
  predictands <- aux$predictands
  x <- aux$x

  if (prediction.type == "exact" && is.null(junction)) {
    print("Junction was not compiled at training stage.")
    junction <- compileJunction( BN.fit )
    print("Done.")
  }
  else if (prediction.type == "exact" && is.na(cbn$junction)) {
    warning("Junction was set as not compilable at training stage, prediction.type has been set to
          approximate",  immediate. = TRUE )
    prediction.type <- "approximate"
  }

  print("Propagating evidence and computing Probability Tables...")
  if ( parallelize == TRUE || !is.null(cl) ) {
    PSOCK.varExports.list <- list( "cbn", "predictors", "type", "predictands", "x")
    PSOCK.funcExportsNames.list <- list("setEvidence", "querygrain", "queryBN")
    cl <- parallelHandler(cluster.type, n.cores, PSOCK.varExports.list, PSOCK.funcExportsNames.list, cl)
    PT <- lapply(x, FUN = function (x) { pbapply(cl = cl, X = x, MARGIN = 1, FUN = queryBN,
                                                 dbn = cbn, evidence.nodes = predictors,
                                                 predictands = predictands,
                                                 type = prediction.type
                                                 )
                                        }
                )
    if (stop.cluster) {
      stopCluster(cl)
      print("Cluster off.")
    }
  }
  else { # Do not parallelize
    PT <- lapply( x, FUN = function(x) {pbapply(X = x, MARGIN = 1, FUN =  queryBN,
                                                dbn = cbn, evidence.nodes = predictors,
                                                predictands = predictands,
                                                type = prediction.type
                                                )
                           }
                )
  }
  print("Done.")

  if (prediction.type == "simulation"){
    PT <- lapply(PT,
                 FUN = function(PT_) { return(toOperableMatrix(t(PT_)))}
                 )

    return(lapply(PT, FUN = function(PTel, predictands) {
                    order.index <- match(predictands, colnames(PTel))
                    return( PTel[ , order.index] )
                  }, predictands = predictands
           )
    )
  }
  else {
    if ( output == "probabilities.list" ) { return(PT) }
    else {
      return(auxParseProbabilitiesList(PT, predictands, prediction.type, threshold.vector,
                                       marginals, event)
            )
    }
  }

}


auxHandlePredictors <- function(cbn, x = NULL, y = NULL){
  if (class(cbn) == "DynamicCBNnoG"){
    predictors <- as.vector(unlist(cbn$names.distribution[1:(length(cbn$names.distribution)-1)]))
    predictands <- cbn$names.distribution[[length(cbn$names.distribution)]]$y.names
    data_ <- prepareDataDynamicBN(prepare_Dataset_forDescriptiveBN(y),
                                  cbn$dynamic.args.list$epochs)
    x <- list(member_1 = data_$data[ , predictors ])
  }
  else if (class(cbn) == "DynamicCBNG"){
    predictors <- c(as.vector(unlist(cbn$names.distribution[1:(length(cbn$names.distribution)-1)])),
                    cbn$names.distribution[[length(cbn$names.distribution)]]$x.names
                    )
    predictands <- cbn$names.distribution[[length(cbn$names.distribution)]]$y.names
    data_ <- prepareDataDynamicBN(prepare_predictors.forBN(prepareData(x, y)),
                                 cbn$dynamic.args.list$epochs)
    if (cbn$dynamic.args.list$remove.past.G){
      data_ <- purgePastGs(data_, cbn$dynamic.args.list$epochs)
    }
    x <- list(member_1 = data_$data[ , predictors])
  }
  else if (class(cbn) == "cbn"){
    predictors <- names(cbn$BN$nodes)[1:cbn$NX]
    predictands <- names(cbn$BN$nodes)[ -(1:cbn$NX) ]
    x <- x$x.global
  }
  else { stop("cbn not recongnized, it should be the output of either buildCBN() or buildDynamicCBN()") }

  return(list(x = x, predictors = predictors, predictands = predictands))
}
