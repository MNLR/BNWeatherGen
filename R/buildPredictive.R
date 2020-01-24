#' @title Build a predictive discrete Bayesian Network using climate data.
#' @description A predictive network expects a Global Climate Model dataset and a stations dataset.
#' Can be used to downscale discrete data to local scales by means of Bayesian Networks.
#' @param data Expects output from \code{preparePredictorsBN()}.
#' @param structure.learning.algorithm Algorithm used to perform structure learning, with name
#' as text. Supports all the score-based, constraint-based and hybrid bayesian network structure
#' learning algorithms from \code{\link[bnlearn]{bnlearn}}.
#' Refer to \code{Details} for a list of supported algorithms.
#' @param structure.learning.args.list List of arguments passed to structure.learning.algorithm,
#'  in particular distance argument if local learning is used. Note that other arguments, e.g.
#'  \code{whitelist}, are an option (check the naming convention, see \code{Details}).
#'  Refer to \code{\link[bnlearn]{bnlearn}} for the specific options.
#' @param param.learning.method Either "bayes" or "mle", passed to learn the parameters for the
#' Conditional Probability Tables for the built DAG \code{data}.
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
#' @param structure.learning.steps It is used to perform structure learning in
#'  two steps.
#'  Refer to \code{Details}.
#' \itemize{
#'  \item \code{1} or \code{NULL} (Default) 1 step
#'  \item \code{2} or \code{c("local", "global")} If \code{dynamic = FALSE} learn first a DAG
#'  for D nodes, then inject G nodes. If \code{dynamic = TRUE} it equals
#'  c("local-global", "past")
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
#' @param return.intermediate Add the intermediate DAGs to the output, as $intermediateDBN1 and
#'  $intermediateDBN2 (if any) if \code{structure.learning.steps} is employed.
#' @param compile.junction Compile the junction from BN.fit to compute probabilities. Can be set
#'  to FALSE, in which case it can still be computed if needed at the training stage, i.e. through
#'  \code{downscale.BN()}.
#' @param parallelize Set to \code{TRUE} for parallelization. Refer to the
#'  \code{\link[parallel]{parallel}} and see \code{Details}.
#' @param n.cores When \code{parallelize = TRUE}, number of threads to be used, will use
#'  detectCores()-1 if not set.
#' @param cluster.type Either "PSOCK" or "FORK". Use the former under Windows systems,
#'  refer to \code{\link[parallel]{parallel}} package.
#'
#' @details
#' \strong{Structure Learning Algorithms}
#' Use \code{structure.learning.algorithm} to specify the algorithm for the structure (DAG) learning process.
#' Currently it DOES NOT support local discovery algorithms, expect malfuncion if used.
#' List of supported algorithms:
#' \code{"hc"}, \code{"tabu"} (score-based), \code{"gs"}, \code{"iamb"}, \code{"fast.iamb"}, \code{"inter.iamb"} (constraint-based),
#' \code{"mmhc"}, \code{"rsmax2"} (hybrid).
#' Check their corresponding parameters in \code{\link[bnlearn]{bnlearn}}, arguments may be passed to the algorithm through
#' the parameter structure.learning.args.list. Do not forget to set the distance argument in \code{structure.learning.args.list} for
#' local learning.
#'
#' \strong{Two or Three Step Learning}
#' \itemize{
#' \item \code{structure.learning.steps} allows to build separate DAGs for each set of nodes. Note that by employing the three
#' \code{structure.learning.algorithm}, \code{structure.learning.algorithm2}, \code{structure.learning.algorithm3} arguments and their
#' corresponding \code{structure.learning.args.list*} counterparts, many different configurations can be used for the structure learning
#' process, e.g. by using grow-shrink for D nodes with distance set to 1, then injecting the left nodes using hill-climbing without distance
#' restriction.
#' \item \code{fix.intermediate}, if set to \code{TRUE}, will forbid the creation of new arcs between nodes that were present in the previous
#' learning step. E.g. if \code{structure.learning.steps = c("local", "global\-past")}, no new arcs between D nodes will be created in the
#' second step, as the first DAG will be considered finished. If set to \code{FALSE}, the previous step DAG will be kept, but the next
#' learning algorithm could create new arcs between D nodes over the first one.
#' }
#'
#' \strong{Forbidding or Forcing Arcs}
#' For non dynamic Bayesian Networks, i.e. when \code{dynamic = FALSE} (default),
#' \code{forbid.GG}, \code{forbid.DD}, \code{forbid.DtoG}, \code{force.closest.GD},
#' \code{forbid.GD}, \code{fix.intermediate}, \code{structure.learning.steps} allow
#' introducing constraints to the structure learning algorithm. The user might also combine them
#' with \code{structure.learning.args.list$whitelist} and
#' \code{structure.learning.args.list$blacklist}. As \code{whitelist} has priority over
#'  \code{blacklist}, i.e. an arc placed in both will always be present in the DAG, they
#'  provide maximum flexibility. Bearing the priority of the \code{whitelist},
#'  \code{force.closest.GD = TRUE} and \code{forbid.GD = TRUE} will, for example, forbid
#'  the placement of \emph{aditional} arcs beyond those specified as the closest G-D.
#'
#'  When manually specifying a whitelist or blacklist through
#'  \code{structure.learning.args.list}, beware of the naming convention. It overrides
#'   the names and marks them as either "D.X" or "G.X", preditand and predictor nodes,
#'   respectivelly. It is best to plot a dummy network using plotDBN() first.
#'
#' \strong{Aditional details}
#' Parameters \code{output.marginals} and \code{compile.junction} are useful to save time
#'  if the user only intends to visualize the DAG.
#' @return An object of type CBN which contains the learnt Bayesian Network.
#' @author Mikel N. Legasa
#' @export

buildPredictive <- function(data,
                            structure.learning.algorithm = "tabu",
                            structure.learning.args.list = list(),
                            param.learning.method = "bayes",
                            forbid.GG = FALSE, forbid.DD = FALSE, forbid.DtoG = FALSE,
                            force.closest.GD = NULL, closest.GD.direction = NULL,
                            forbid.GD = FALSE,
                            structure.learning.steps = 1,
                            fix.intermediate = TRUE,
                            structure.learning.algorithm2 = NULL,
                            structure.learning.args.list2 = list(),
                            return.intermediate = FALSE,
                            compile.junction = TRUE,
                            parallelize = FALSE, n.cores = NULL, cluster.type = "FORK"
                            ) {

  if (!(is.character(structure.learning.algorithm))) {
    stop("Input algorithm name as character")
  }

  POS <- data$positions
  NX <- data$nx
  NY <- data$ny
  steps.left <- 0

  if (!is.null(structure.learning.steps) && structure.learning.steps != 1){
    hls <- handleLearningSteps(data, structure.learning.steps,
                               structure.learning.args.list,
                               structure.learning.algorithm,
                               forbid.GG, forbid.DD
                               )
    structure.learning.steps <- hls$structure.learning.steps
    step.data <- hls$step.data
    POS <- hls$POS
    DATA <- hls$DATA
    steps.left <- hls$steps.left
    structure.learning.args.list <- hls$structure.learning.args.list
  }
  else{ # Single or last step
    step.data <- NULL
    print( paste0(paste0("Building Bayesian Network using ", structure.learning.algorithm) , "..." ) )
    structure.learning.args.list <- addtoBlacklist(structure.learning.args.list,
                                                   data, forbid.GG, forbid.DD, forbid.DtoG,
                                                   force.closest.GD, closest.GD.direction,
                                                   forbid.GD
                                                   )

    DATA <- data$data
  }

  if ( !(is.null(structure.learning.args.list$distance)) ){   # local learning
    distance <- structure.learning.args.list$distance
    if (is.null(step.data)) { step.data <- data }
    structure.learning.args.list <- handleLocalLearning(step.data,
                                                        structure.learning.args.list
                                                        )
    structure.learning.args.list$distance <- NULL
  } else { distance <- NULL }

  structure.learning.args.list[["x"]] <- DATA
  bn <- learnDAG(structure.learning.algorithm, structure.learning.args.list,
                 parallelize, cluster.type, n.cores
                )

  if (steps.left == 0){
    bn.fit <- bn.fit(bn, data = DATA, method = param.learning.method)
    print("Done building Bayesian Network.")
  }

  if ( steps.left >= 1){
    print("Injecting next step into the DAG...")
    whitelist <- bn$arcs
    if (fix.intermediate){
      structure.learning.args.list2 <- fixIntermediate(step.data$names.distribution,
                                                       structure.learning.args.list2,
                                                       whitelist
                                                       )
    }

    if (is.null(structure.learning.algorithm2) ){
      structure.learning.algorithm2 <- structure.learning.algorithm
    }

    structure.learning.args.list2 <- initializeDummyGreylist(structure.learning.args.list2,
                                                             "whitelist")
    structure.learning.args.list2$whitelist <- rbind(
                                                structure.learning.args.list2$whitelist,
                                                whitelist
                                                )

    DBN <- 
      buildPredictive(data,
                      structure.learning.algorithm = structure.learning.algorithm2,
                      structure.learning.args.list = structure.learning.args.list2,
                      param.learning.method = param.learning.method,
                      forbid.GG = forbid.GG,
                      forbid.DD = forbid.DD,
                      forbid.DtoG = forbid.DtoG,
                      force.closest.GD = force.closest.GD,
                      closest.GD.direction = closest.GD.direction,
                      forbid.GD = forbid.GD,
                      structure.learning.steps = structure.learning.steps,
                      fix.intermediate = fix.intermediate,
                      return.intermediate = return.intermediate,
                      compile.junction = compile.junction,
                      parallelize = parallelize,
                      n.cores= n.cores,
                      cluster.type = cluster.type
                      )

    if (return.intermediate){
      if (steps.left == 1){
        DBN[["intermediateDBN1"]] <- list(BN = bn, training.data = DATA, positions = POS,
                                          names.distribution = step.data$names.distribution,
                                          NX=NX, NY=NY,
                                          structure.learning.args.list = structure.learning.args.list)
      }
      return(DBN)
    }
    else { return(DBN) }
  }
  else {
    if (compile.junction){
      junction <- compileJunction(bn.fit)
    } else { junction <- NULL }

    marginals_ <- marginals( list(BN = bn, NX = NX, junction = junction,
                                  training.data = DATA) )

    if (!(is.null(distance))) { structure.learning.args.list[["distance"]] <- distance }

    cbn <-
      list(BN = bn, training.data = DATA, positions = POS,
           BN.fit = bn.fit, junction = junction,
           NX = NX, NY = NY,
           marginals = marginals_,
           structure.learning.algorithm = structure.learning.algorithm,
           structure.learning.args.list = structure.learning.args.list,
           param.learning.method = param.learning.method
           )
    class(cbn) <- "cbn"
    return(cbn)
  }
}
