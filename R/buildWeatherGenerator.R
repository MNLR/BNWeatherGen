#' @title Build a Discrete Bayesian Network Weather Generator.
#' @description Builds a discrete weather generator Bayesian network model, using
#' \href{https://www.bnlearn.com}{bnlearn}. Data should be discretized prior to calling 
#' the function by the user. The Bayesian network will approximate the Joint Probability 
#' Distribution of the dataset \code{y} considering the temporal order specified in \code{epochs}.
#' @param y Stations dataset, as output by \code{loadeR::loadStationData()} (see Details)
#' @param x By default \code{NULL}, can be used to provide a predictor dataset, as output by
#'  \code{loadeR::loadGridData()}.
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
#' @param force.DD If set to "-", the final DAG will be force to have a link between each station and 
#' its past. If set to "->" the arc will be forced from past to present, and from present to past if 
#' set to "<-".
#' @param forbid.GG If set to TRUE, arcs between grid or G nodes will be forbidden.
#' @param forbid.DD If set to TRUE, arcs between local, i.e. station or D nodes, will be forbidden.
#' @param forbid.DtoG If set to TRUE, arcs from D nodes to G nodes will be forbidden.
#' @param force.closest.GD Expects a positive integer or \code{NULL}. If not \code{NULL}, each D node will
#'  be linked, see \code{closest.GD.direction}, with the n closest G node(s), where n is
#'  \code{force.closest.GD}.
#' @param closest.GD.direction Either \code{NULL}, which lets the structure learning algorithm
#'  decide the direction, "up", which will place the arc(s) in the form \code{D -> G}, or "down",
#'  which will place the arc(s) in the form \code{G -> D}.
#' @param forbid.GD If set to TRUE, arcs between G and D nodes will be forbidden. See
#'  \code{Details}.
#' @param structure.learning.steps It is used to perform structure learning in up to three steps.
#'  Note that \code{past} refers to the past epochs when \code{dynamic = TRUE}.
#'  Refer to \code{Details}.
#' \itemize{
#'  \item \code{1} or \code{NULL} (Default) 1 step
#'  \item \code{2} or \code{c("local", "global")} If \code{dynamic = FALSE} learn first a DAG
#'  for D nodes, then inject G nodes. If \code{dynamic = TRUE} it equals
#'  c("local-global", "past")
#'  \item \code{3} Equals c("local", "global", "past")
#'  \item \code{c("local-global", "past")} or \code{c("global-local", "past")}.
#'   Learn first DAG for D and G nodes, then inject past nodes.
#'  \item \code{c("local", "global-past")} or \code{c("local", "past-global")}.
#'  Learn first DAG for D nodes, then inject past and G nodes.
#'  \item \code{c("local-past", "global")} or \code{c("past-local", "global")}.
#'  Learn first DAG for D and past nodes, then inject G nodes.
#'  \item \code{c("local", "global", "past")} Learn first DAG for D nodes, then inject G nodes,
#'  then inject past nodes.
#'  \item \code{c("local", "past", "global")} Learn first DAG for D nodes, then inject
#'  past nodes, then inject G nodes.
#' }
#' Note that only first two options are valid when \code{dynamic = FALSE}
#' @param fix.intermediate Set to TRUE to forbid the creation of new arcs in the next steps
#' for already built DAGs. See \code{Details}.
#' \code{structure.learning.algorithm2} and \code{structure.learning.args.list2}. See
#'  \code{Details}.
#' @param structure.learning.algorithm2   Same as structure.learning.algorithm for the second
#' step if \code{structure.learning.steps} is employed. Ignored otherwise.
#' @param structure.learning.args.list2   Same as structure.learning.args.list for the second
#'  step if \code{structure.learning.steps} is employed. Ignored otherwise.
#' @param epochs Number of epochs to consider for Dynamic Bayesian Networks.
#' @param remove.past.G When \code{dynamic = TRUE} Set to TRUE to remove the past G nodes.
#' @param keep.dynamic.distance When \code{dynamic = TRUE} and local learning is employed,
#'  if set to TRUE it will use its corresponding distance (See \code{Details}) between nodes
#'  from diferent epochs.
#' @param forbid.backwards When \code{dynamic = TRUE}, set to TRUE to forbid arcs going
#' back in time.
#' @param forbid.past.dynamic.GD When \code{dynamic = TRUE}, set to TRUE to forbid arcs in the
#' form G->D or D->G between different epochs.
#' @param forbid.dynamic.GG When \code{dynamic = TRUE} and \code{remove.past.G = FALSE}, set to
#' TRUE to forbid arcs in the form G-G in the past epochs.
#' @param forbid.past.DD When \code{dynamic = TRUE}, set to TRUE to forbid arcs in the form D-D
#' in the past epochs.
#' @param structure.learning.algorithm3   Same as structure.learning.algorithm for the third
#'  step if \code{structure.learning.steps} with 3 steps is employed. Ignored otherwise.
#' @param structure.learning.args.list3   Same as structure.learning.args.list for the third step
#' if \code{structure.learning.steps} with 3 steps is employed. Ignored otherwise.
#' See \code{Details}.
#' @param return.intermediate Add the intermediate DAGs to the output, as $intermediateDBN1 and
#'  $intermediateDBN2 (if any) if \code{structure.learning.steps} is employed.
#' @param compile.junction Compile the junction tree from BN.fit to compute probabilities. Can be
#'  set to FALSE. Compiling the junction tree is necessary for using exact inference at the
#'  simulating stage.
#' @param parallelize Set to \code{TRUE} for parallelization. Refer to the
#'  \code{\link[parallel]{parallel}} and see \code{Details}.
#' @param n.cores When \code{parallelize = TRUE}, number of threads to be used, will use
#'  detectCores()-1 if not set.
#' @param cluster.type Either "PSOCK" or "FORK". Use the former under Windows systems,
#'  refer to \code{\link[parallel]{parallel}} package.
#'
#' @details
#' buildWeatherGenerator() can be used with just a stations dataset. A Grid dataset may be specified
#' by either using parameter \code{x} or using a "pp.forBN" class object (as output from
#' \code{preparePredictorsBN()}) for parameter \code{y}. If none is specified the Bayesian
#' Network may be used as a naive weather generator.
#'
#' \strong{Structure Learning Algorithms}
#' Use \code{structure.learning.algorithm} to specify the algorithm for the structure (DAG) learning
#' process.
#' Currently it DOES NOT support local discovery algorithms, expect malfuncion if used.
#' List of supported algorithms:
#' \code{"hc"}, \code{"tabu"} (score-based), \code{"gs"}, \code{"iamb"}, \code{"fast.iamb"},
#' \code{"inter.iamb"} (constraint-based),
#' \code{"mmhc"}, \code{"rsmax2"} (hybrid).
#' Check their corresponding parameters in \code{\link[bnlearn]{bnlearn}}, arguments may be
#' passed to the algorithm through
#' the parameter structure.learning.args.list. Do not forget to set the distance argument
#' in \code{structure.learning.args.list} for
#' local learning.
#'
#' \strong{Two or Three Step Learning}
#' \itemize{
#' \item \code{structure.learning.steps} allows to build separate DAGs for each set of nodes.
#'  Note that by employing the three \code{structure.learning.algorithm},
#'  \code{structure.learning.algorithm2}, \code{structure.learning.algorithm3} arguments and their
#' corresponding \code{structure.learning.args.list*} counterparts, many different configurations
#' can be used for the structure learning process, e.g. by using grow-shrink for D nodes with
#' distance set to 1, then injecting the left nodes using hill-climbing without distance
#' restriction.
#' \item \code{fix.intermediate}, if set to \code{TRUE}, will forbid the creation of new arcs
#' between nodes that were present in the previous learning step. E.g. if
#' \code{structure.learning.steps = c("local", "global\-past")}, no new arcs between D nodes
#' will be created in the second step, as the first DAG will be considered finished.
#' If set to \code{FALSE}, the previous step DAG will be kept, but the next
#' learning algorithm could create new arcs between D nodes over the first one.
#' }
#'
#' \strong{Forbidding or Forcing Arcs}
#' \code{force.DD}, \code{forbid.GG}, \code{forbid.DD}, \code{forbid.DtoG}, \code{force.closest.GD},
#' \code{forbid.GD}, \code{fix.intermediate}, \code{structure.learning.steps} allow for
#' introducing constraints to the structure learning algorithm. The user might also combine them
#' with \code{structure.learning.args.list$whitelist} and
#' \code{structure.learning.args.list$blacklist}. As \code{whitelist} has priority over
#'  \code{blacklist}, i.e. an arc placed in both will always be present in the DAG, they provide
#'  maximum flexibility. Bearing the priority of the \code{whitelist}, \code{force.closest.GD = TRUE}
#'  and \code{forbid.GD = TRUE} will, for example, forbid the placement of \emph{aditional}
#'  arcs beyond those specified as the closest G-D.
#'
#'  When manually specifying a whitelist or blacklist through \code{structure.learning.args.list},
#'  beware of the naming convention. It overrides the names and marks them as either "D.X" or "G.X",
#'  predictand and predictor nodes, respectivelly. A plot of a dummy network using \code{plotDBN()} 
#'  is may help.
#'
#' @return An object of type CBN which contains the learnt Bayesian Network.
#' @author Mikel N. Legasa
#' @export

buildWeatherGenerator <- function(y, x = NULL,
                                  structure.learning.algorithm = "tabu",
                                  structure.learning.args.list = list(),
                                  param.learning.method = NULL,
                                  epochs = 2,
                                  force.DD = NULL,
                                  forbid.GG = FALSE, forbid.DD = FALSE, 
                                  forbid.DtoG = FALSE, force.closest.GD = NULL, 
                                  closest.GD.direction = NULL, forbid.GD = FALSE,
                                  structure.learning.steps = 1,
                                  fix.intermediate = TRUE,
                                  structure.learning.algorithm2 = NULL,
                                  structure.learning.args.list2 = list(),
                                  structure.learning.algorithm3 = NULL,
                                  structure.learning.args.list3 = list(),
                                  keep.dynamic.distance = TRUE,
                                  remove.past.G = TRUE,
                                  forbid.backwards = FALSE,
                                  forbid.past.dynamic.GD = TRUE,
                                  forbid.dynamic.GG = FALSE,
                                  forbid.past.DD = FALSE,
                                  return.intermediate = FALSE,
                                  compile.junction = FALSE, 
                                  parallelize = FALSE, n.cores= NULL,
                                  cluster.type = "FORK") {
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
  if (is.null(x) && class(y) != "pp.forBN"){
    DCBN <- buildDynamicCBNnoG(y = y,
                               structure.learning.algorithm = structure.learning.algorithm,
                               structure.learning.args.list = structure.learning.args.list,
                               param.learning.method = param.learning.method,
                               epochs = epochs, 
                               structure.learning.steps = structure.learning.steps,
                               fix.intermediate = fix.intermediate,
                               structure.learning.algorithm2 = structure.learning.algorithm2,
                               structure.learning.args.list2 = structure.learning.args.list2,
                               force.DD = force.DD,
                               keep.dynamic.distance = keep.dynamic.distance,
                               remove.past.G = remove.past.G,
                               forbid.backwards = forbid.backwards,
                               forbid.past.dynamic.GD = forbid.past.dynamic.GD,
                               forbid.dynamic.GG = forbid.dynamic.GG,
                               forbid.past.DD = forbid.past.DD,
                               return.intermediate = return.intermediate,
                               compile.junction = compile.junction,
                               parallelize = parallelize, n.cores= n.cores,
                               cluster.type = cluster.type
                               )
  }
  else {
    DCBN <- buildDynamicCBNG(y = y, x = x,
                             structure.learning.algorithm = structure.learning.algorithm,
                             structure.learning.args.list = structure.learning.args.list,
                             param.learning.method = param.learning.method,
                             epochs = epochs,
                             forbid.GG = forbid.GG,
                             force.DD = force.DD, forbid.DD = forbid.DD,
                             forbid.DtoG = forbid.DtoG,
                             force.closest.GD = force.closest.GD,
                             closest.GD.direction = closest.GD.direction,
                             forbid.GD = forbid.GD,
                             structure.learning.steps = structure.learning.steps,
                             fix.intermediate = fix.intermediate,
                             structure.learning.algorithm2 = structure.learning.algorithm2,
                             structure.learning.args.list2 = structure.learning.args.list2,
                             structure.learning.algorithm3 = structure.learning.algorithm3,
                             structure.learning.args.list3 = structure.learning.args.list3,
                             keep.dynamic.distance = keep.dynamic.distance,
                             remove.past.G = remove.past.G,
                             forbid.backwards = forbid.backwards,
                             forbid.past.dynamic.GD = forbid.past.dynamic.GD,
                             forbid.dynamic.GG = forbid.dynamic.GG,
                             forbid.past.DD = forbid.past.DD,
                             return.intermediate = return.intermediate,
                             compile.junction = compile.junction,
                             parallelize = parallelize, n.cores= n.cores,
                             cluster.type = cluster.type
                             )
  }

  return(DCBN)
}
