#' @title Query Bayesian Network Model.
#' @description Performs inference in a Bayesian network model, which can be output from 
#' \code{buildWeatherGenerator()}, \code{buildPredictive()} and \code{buildDescriptive()}. 
#' @param evidence An evidence vector.
#' @param dbn A Bayesian network model, as output by \code{buildWeatherGenerator()},
#' \code{buildPredictive()} or \code{buildDescriptive()}
#' @param evidence.nodes Names of the nodes for which evidence will be provided. 
#'  Must have the same length as \code{evidence}.
#' @param predictands Nodes queried.
#' @param type Inference type. Either \code{"simulation"}, \code{"approximate"}, \code{"exact"},
#' or \code{"random"}. Exact inference requires the compiled junction tree and \code{gRain} library. 
#' Approximate uses \code{cpdist()} function from \code{bnlearn}. Simulation is used for weather generation,
#' and generates stochastic predictions. \code{"random"} generates a non spatially consistent observation, 
#' based directly on the probability vector obtained using only evidence nodes.
#' @param which_ Probabilities to return. Used only for exact inference. 
#' @param resample.size Number of samples for the likelihood weighting approximate inference method.
#' @param cl A cluster object from \code{parallel} library, used only if \code{"approximate"} inference
#' is selected.
#' @export

queryBN <- function( evidence, dbn, evidence.nodes, predictands, type = "exact",
                     which_ = "marginal", resample.size = 10000,  cl = NULL
                     )  {
  junction <- dbn$junction
  BN.fit <- dbn$BN.fit

  if (type == "exact" && !(is.null(junction)) && !(is.na(junction))){
    evid <- setEvidence(junction, evidence.nodes, as.character(evidence)) # Evidence must be provided as character
    return( querygrain( object = evid, nodes = predictands, type = which_) )
  }
  else if (type == "simulation"){
    sim_ <- simulate1(BN.fit = BN.fit, junction = junction,
                      evidence.nodes = evidence.nodes,
                      evidence = evidence)
    order.index <- match(predictands, names(sim_))
    return( sim_[order.index] )
  }
  else if (( type == "exact" && (is.null(junction) || is.na(junction)) ) ||
           type == "approximate"){
    lwsample <- cpdist( fitted = BN.fit, nodes = predictands,
             evidence = auxEvidenceTocpdistInput(evidence.nodes, evidence),
             method = 'lw', cluster = cl)
    simsample <- lwsample[sample(1:nrow(lwsample), prob = attributes(lwsample)$weights,
                                 size = resample.size , replace = TRUE), ]
    if (length(predictands) == 1){
      tsimsample <- table(simsample)
      return( tsimsample/sum(tsimsample) )
    }
    else {
      return( lapply(simsample, FUN = function(x) {
                                        tx <- table(x)
                                        return(tx/sum(tx))
                                      }
                     )
             )
    }
  }
  else if (type == "random") {
    probs <- queryBN(evidence = evidence, dbn = dbn, evidence.nodes = evidence.nodes,
                     predictands = predictands)
    probs <- sapply(probs, c)
    aux <- array(dim = c(1,nrow(probs), ncol(probs)), dimnames = list(c(1), rownames(probs),colnames(probs) ) )
    aux[1,,] <- probs
    return(convertEvent(aux, threshold.vector = "random"))
  }

  else { stop("Please use a valid inference type: exact, approximate, simulation.") }
}
