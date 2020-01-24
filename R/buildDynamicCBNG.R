
buildDynamicCBNG <- function(y, x,
                            structure.learning.algorithm = "hc",
                            structure.learning.args.list = list(),
                            param.learning.method = "bayes",
                            epochs = 2,
                            forbid.GG = FALSE, 
                            force.DD = NULL, forbid.DD = FALSE, 
                            forbid.DtoG = FALSE,
                            force.closest.GD = NULL, closest.GD.direction = NULL,
                            forbid.GD = FALSE,
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
                            override.junction = TRUE,
                            parallelize = FALSE, n.cores= NULL,
                            cluster.type = "FORK") {

  if (class(y) != "pp.forDynBN") {
    if (class(y) != "pp.forBN"){
      y <- preparePredictorsBN(grid = prepareData(x = x,y = y),
                               rm.na = TRUE, rm.na.mode = "observations"
                               )
    }
    y <- splitSpellsNA(y)

    if ( epochs >= 2 ) { # MARCADO - usar clases
      y <- prepareDataDynamicBN(y, epochs)
      if (remove.past.G) {
        y <- purgePastGs(y, epochs)
        forbid.dynamic.GG <- FALSE
      }
    } else { stop("epochs must be greater than or equal to 2.") }
  }

  data_ <- y

  POS <- data_$positions
  NX <- data_$nx
  NY <- data_$ny
  steps.left <- 0

  if (!is.null(structure.learning.steps) && structure.learning.steps != 1){
    hls <- handleLearningSteps(data_, structure.learning.steps,
                               structure.learning.args.list,
                               structure.learning.algorithm,
                               forbid.GG, forbid.DD,
                               TRUE, remove.past.G, epochs, forbid.backwards,
                               forbid.past.DD, forbid.past.dynamic.GD,
                               forbid.dynamic.GG,
                               force.DD
                              )
    structure.learning.steps <- hls$structure.learning.steps
    int.dynamic.args.list <- hls$int.dynamic.args.list
    step.data <- hls$step.data
    POS <- hls$POS
    DATA <- hls$DATA
    steps.left <- hls$steps.left
    structure.learning.args.list <- hls$structure.learning.args.list
  }
  else{
    step.data <- NULL
    print( paste0(paste0("Building Bayesian Network using ",
                         structure.learning.algorithm) , "..." )
           )
    # WARNING: addtoGraylistDynamic() has yet forbid.DtoG to be implemented
    # WARNING: addtoGraylistDynamic() has yet force.closest.GD to be implemented
    # WARNING: addtoGraylistDynamic() has yet forbid.GD to be implemented
    structure.learning.args.list <- 
      addtoGraylistDynamic(structure.learning.args.list,
                           data_$names.distribution,
                           forbid.backwards,
                           forbid.past.DD,
                           force.DD,
                           forbid.past.dynamic.GD,
                           forbid.dynamic.GG,
                           forbid.GG,
                           forbid.DD)
    DATA <- data_$data
  }

  if ( !(is.null(structure.learning.args.list$distance)) ){  # local learning
    distance <- structure.learning.args.list$distance
    if (is.null(step.data)) { step.data <- data_ }
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

  if ( steps.left >= 1 ){
    print("Injecting next step into the DAG...")
    whitelist <- bn$arcs
    if (fix.intermediate){
      structure.learning.args.list2 <- fixIntermediate(step.data$names.distribution,
                                                       structure.learning.args.list2,
                                                       whitelist
                                                       )
    }

    if (is.null(structure.learning.algorithm2)){
      structure.learning.algorithm2 <- structure.learning.algorithm
    }
    if (steps.left == 2){
      if (is.null(structure.learning.algorithm3)){
        structure.learning.algorithm3 <- structure.learning.algorithm2
      }
    }

    structure.learning.args.list2 <- initializeDummyGreylist(structure.learning.args.list2,
                                                             "whitelist")
    structure.learning.args.list2$whitelist <- rbind(
      structure.learning.args.list2$whitelist, whitelist
    )

    DBN <-  buildDynamicCBNG(data_,
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
                            structure.learning.algorithm2 = structure.learning.algorithm3,
                            structure.learning.args.list2 = structure.learning.args.list3,
                            epochs = epochs,
                            remove.past.G = remove.past.G,
                            keep.dynamic.distance = keep.dynamic.distance,
                            forbid.backwards = forbid.backwards,
                            forbid.past.dynamic.GD = forbid.past.dynamic.GD,
                            forbid.dynamic.GG = forbid.dynamic.GG,
                            forbid.past.DD = forbid.past.DD,
                            return.intermediate = return.intermediate,
                            compile.junction = compile.junction,
                            parallelize = parallelize,
                            n.cores= n.cores,
                            cluster.type = cluster.type
                            )

    if (return.intermediate){
      if (steps.left == 2){
        DBN[["intermediateDBN2"]] <- list(BN = bn, training.data = DATA, positions = POS,
                                          dynamic.args.list = int.dynamic.args.list,
                                          names.distribution = step.data$names.distribution,
                                          NX=NX, NY=NY,
                                          structure.learning.args.list = structure.learning.args.list)
      }
      if (steps.left == 1){
        DBN[["intermediateDBN1"]] <- list(BN = bn, training.data = DATA, positions = POS,
                                          dynamic.args.list = int.dynamic.args.list,
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
      message("Warning: Compiling junction tree for weather generators is memory intensive.")
      if (!override.junction) readline(prompt="Press [enter] to continue...")
      junction <- compileJunction(bn.fit)
    } else { junction <- NA }

    marginals_ <- marginals( list(BN = bn, NX = NX, junction = junction,
                                  training.data = DATA) )

    dynamic.args.list <- list( epochs = epochs, remove.past.G = remove.past.G,
                               forbid.backwards = forbid.backwards,
                               forbid.past.dynamic.GD = forbid.past.dynamic.GD,
                               forbid.dynamic.GG = forbid.dynamic.GG,
                               forbid.past.DD = forbid.past.DD
    )
    names.distribution <- data_$names.distribution

    if (!(is.null(distance))) { structure.learning.args.list[["distance"]] <- distance }

    wgG <-
      list(BN = bn, training.data = DATA, positions = POS, BN.fit = bn.fit, junction = junction,
           dynamic.args.list = dynamic.args.list,
           NX = NX, NY = NY, names.distribution = names.distribution,
           marginals = marginals_,
           structure.learning.algorithm = structure.learning.algorithm,
           structure.learning.args.list = structure.learning.args.list,
           param.learning.method = param.learning.method
           )
    class(wgG) <- "DynamicCBNG"

    return(wgG)
  }
}
