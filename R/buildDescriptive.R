#' @title Build a discrete Bayesian descriptive network.
#' @description A descriptive Bayesian network that approximates the Joint Probability
#' of the dataset \code{y}, withoyt taking into account the temporal aspect.
#' @param y Stations dataset, as output by \code{loadeR::loadStationData()}
#' @param structure.learning.algorithm Algorithm used to perform structure learning, with name
#' as text. Supports all the score-based, constraint-based and hybrid bayesian network structure
#' learning algorithms from \code{\link[bnlearn]{bnlearn}}.
#' Refer to \code{Details} for a list of supported algorithms.
#' @param structure.learning.args.list List of arguments passed to structure.learning.algorithm,
#'  in particular distance argument if local learning is used. Note that other arguments, e.g.
#'  \code{whitelist}, are an option (check the naming convention, see \code{Details}).
#'  Refer to \code{\link[bnlearn]{bnlearn}} for the specific options.
#' @param param.learning.method Either "bayes", for bayesian estimation; or "mle", for 
#' Maximum Likelihood Estimation. 
#' @param compile.junction Compile the junction tree from BN.fit to compute probabilities. Can be
#'  set to FALSE. Compiling the junction tree is necessary for using exact inference at the
#'  simulating stage.
#' @param parallelize Set to \code{TRUE} for parallelization. Refer to the
#'  \code{\link[parallel]{parallel}} and see \code{Details}.
#' @param n.cores When \code{parallelize = TRUE}, number of threads to be used, will use
#'  detectCores()-1 if not set.
#' @param cluster.type Either "PSOCK" or "FORK". Use the former under Windows systems,
#'  refer to \code{\link[parallel]{parallel}} package.
#' @details
#' buildDescriptive() can be used to build a Bayesian network that characterizes the
#' spatial aspects of the dataset, whithout taking into account autocorrelation of
#' the weather series.
#' @export
#' @author Mikel N. Legasa

buildDescriptive <- function(y,
                             structure.learning.algorithm =  "tabu",
                             structure.learning.args.list = list(),
                             compile.junction = FALSE,
                             param.learning.method = NULL,
                             parallelize = FALSE, cluster.type = "FORK",
                             n.cores = NULL) {
                             
  if ( is.null(param.learning.method) ){
    print("No param.learning.method set.")
    if (class(y$Data[,1]) == "numeric"){
      print("Inferred Gaussian BN from first variable. param.learning.method has been set to mle-g")
      param.learning.method <- "mle-g"
    } else if (class(y$Data[,1]) == "factor"){
      print("Inferred Discrete BN from first variable. param.learning.method has been set to mle")
      param.learning.method <- "mle"
    } else {
      stop("Could not infer distribution of nodes. Did you input valid variables? (numeric or factor)")
    }
  }

  py <- prepareDatasetDescriptiveBN(y)
  descbn <- buildPredictive(py,
                            structure.learning.algorithm = structure.learning.algorithm,
                            structure.learning.args.list = structure.learning.args.list,
                            param.learning.method = param.learning.method,
                            forbid.GG = FALSE, forbid.DD = FALSE,
                            compile.junction = compile.junction,
                            parallelize = parallelize, cluster.type = cluster.type,
                            n.cores = n.cores
                            )

  return(descbn)
}
