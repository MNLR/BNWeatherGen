
buildDynamicCBNnoG <- function(y, processed = FALSE,
                               structure.learning.algorithm = "hc",
                               structure.learning.args.list = list(),
                               param.learning.method = "bayes",
                               epochs = 2,
                               structure.learning.steps = 1,
                               fix.intermediate = TRUE,
                               structure.learning.algorithm2 = NULL,
                               structure.learning.args.list2 = list(),
                               force.DD = NULL,
                               keep.dynamic.distance = TRUE,
                               remove.past.G = TRUE,
                               forbid.backwards = FALSE,
                               forbid.past.dynamic.GD = TRUE,
                               forbid.dynamic.GG = FALSE,
                               forbid.past.DD = FALSE,
                               return.intermediate = FALSE,
                               compile.junction = FALSE,
                               override.junction = TRUE,
                               parallelize = FALSE, n.cores= NULL,
                               cluster.type = "FORK"){


  if (epochs <= 1){
    stop("epochs must be greater or equal than 2.")
  }

  if (structure.learning.steps != 1){
    structure.learning.args.list <- 
      handleLearningStepsDynamic(y,
                                 structure.learning.algorithm,
                                 structure.learning.args.list,
                                 structure.learning.args.list2,
                                 parallelize, cluster.type,
                                 n.cores, epochs,
                                 fix.intermediate)

    if (!is.null(structure.learning.algorithm2))  {
      structure.learning.algorithm <- structure.learning.algorithm2
    }
    print("Done.")
  }
  steps.left <- 0

  if (!processed){
    y <- splitSpellsNA(prepareDatasetDescriptiveBN(y))

    print( paste0(paste0("Building Bayesian Network using ",
                         structure.learning.algorithm) , "..."
                  )
           )

    if (  class(y) != "pp.forDynBN" ) { # is.null(data$data) = TRUE when already processed for Dynamic
      y <- prepareDataDynamicBN(y, epochs)
    }
  }

  POS <- y$positions
  NX <- y$nx
  NY <- y$ny
  DATA <- y$data

  structure.learning.args.list <- addtoGraylistDynamic(structure.learning.args.list,
                                                       y$names.distribution,
                                                       forbid.backwards,
                                                       forbid.past.DD,
                                                       force.DD
                                                       )

  structure.learning.args.list[["x"]] <- DATA
  bn <- learnDAG(structure.learning.algorithm, structure.learning.args.list,
                 parallelize, cluster.type, n.cores
  )

  if (steps.left == 0){
    bn.fit <- bn.fit(bn, data = DATA, method = param.learning.method)
    print("Done building Bayesian Network.")
  }

  if (compile.junction){
    message("Warning: Compiling junction tree for weather generators is memory intensive.")
    if (!override.junction) readline(prompt="Press [enter] to continue...")
    junction <- compileJunction(bn.fit)
  } else { junction <- NA }

  marginals_ <- marginals( list(BN = bn, NX = NX, junction = junction,
                                training.data = DATA) )

  dynamic.args.list <- list( epochs = epochs,
                             forbid.backwards = forbid.backwards,
                             forbid.past.DD = forbid.past.DD
  )
  names.distribution <- y$names.distribution

  wg <- list(BN = bn, training.data = DATA, positions = POS, BN.fit = bn.fit,
             junction = junction,
             dynamic.args.list = dynamic.args.list,
             NX = NX, NY = NY, names.distribution = names.distribution,
             marginals = marginals_,
             structure.learning.algorithm = structure.learning.algorithm,
             structure.learning.args.list = structure.learning.args.list,
             param.learning.method = param.learning.method
  )
  class(wg) <- "DynamicCBNnoG"
  return(wg)
}
