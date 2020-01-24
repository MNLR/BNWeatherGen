
handleLearningSteps <- function(data, structure.learning.steps,
                                structure.learning.args.list,
                                structure.learning.algorithm,
                                forbid.GG, 
                                forbid.DD,
                                dynamic = FALSE,
                                remove.past.G = NULL, epochs = NULL,
                                forbid.backwards = NULL,
                                forbid.past.DD = NULL, forbid.past.dynamic.GD = NULL,
                                forbid.dynamic.GG = NULL, 
                                force.DD = NULL
                                ) {
  structure.learning.steps <- parseStructureLearningStepsArg(structure.learning.steps,
                                                             dynamic, remove.past.G
                                                             )
  if (grepl("past", structure.learning.steps[1])) {
    int.dynamic.args.list <- list(remove.past.G = FALSE, epochs = epochs)
  }
  else {int.dynamic.args.list <- NULL} # marks intermediate DAGs

  step.data <- auxHandleLearningSteps(data, structure.learning.steps, dynamic)
  if (is.null(step.data)) {stop("Please, use a valid structure.learning.steps option.")}

  POS <- step.data$positions
  DATA <- step.data$DATA
  steps.left <- length(step.data$structure.learning.steps)
  print(paste0( c("Building intermediate DAG ", steps.left , " using ",
                  structure.learning.algorithm, " for ", structure.learning.steps[1],
                  " nodes", "..." ),
                collapse = ""))
  structure.learning.steps <- step.data$structure.learning.steps
  if (dynamic){
    structure.learning.args.list <- addtoGraylistDynamic(structure.learning.args.list,
                                                         step.data$names.distribution,
                                                         forbid.backwards,
                                                         forbid.past.DD,
                                                         force.DD,
                                                         forbid.past.dynamic.GD,
                                                         forbid.dynamic.GG,
                                                         forbid.GG,
                                                         forbid.DD
                                                         )
  }
  return(list(structure.learning.steps = structure.learning.steps,
              int.dynamic.args.list = int.dynamic.args.list,
              step.data = step.data, POS = POS, DATA = DATA, steps.left = steps.left,
              structure.learning.args.list = structure.learning.args.list
              )
         )
}


####
#### auxHandleLearningSteps
####

auxHandleLearningSteps <- function(data, structure.learning.steps, dynamic) {
  POS <- data$positions
  NX <- data$nx
  NY <- data$ny

  if (!dynamic){
    if ( all(structure.learning.steps == c("local", "global")) ){
      return( list(DATA = data$data[ , (NX+1):(NX+NY)], names.distribution = list(y.names = data$y.names),
                   positions = POS[ , (NX+1):(NX+NY)], structure.learning.steps = 1) )
    } else { stop("For non Dynamic Bayesian Networks use either one step learning with structure.learning.steps = 1
                    or two step learning with structure.learning.steps = 2 or structure.learning.steps = c(\"local\", \"global\")") }
  }
  else { # Dynamic Bayesian Network
    Nsteps <- length(structure.learning.steps)
    Nepochs <- length(data$names.distribution)

    if (Nsteps == 2){
      if (all(structure.learning.steps == c("local-global", "past")) | all(structure.learning.steps == c("global-local", "past"))){
        selected <- as.vector(unlist(data$names.distribution[[Nepochs]]))
        return( list(DATA = data$data[ , selected], positions = POS[ , selected], structure.learning.steps = 1,
                     names.distribution = list(data$names.distribution[[Nepochs]])
        )
        )
      }
      if (all(structure.learning.steps == c("local-past", "global")) | all(structure.learning.steps == c("past-local", "global"))){
        selected <- as.vector(unlist(lapply(data$names.distribution, function(x) { return(x$y.names) })))
        return( list(DATA = data$data[ , selected], positions = POS[ , selected], structure.learning.steps = 1,
                     names.distribution = lapply(data$names.distribution, function(x) { return(list(y.names = x$y.names) ) })
        )
        )
      }
      if (all(structure.learning.steps == c("local", "past-global")) | all(structure.learning.steps == c("local", "global-past"))){
        selected <- data$names.distribution[[Nepochs]]$y.names
        return( list(DATA = data$data[ , selected], positions = POS[ , selected], structure.learning.steps = 1,
                     names.distribution = list(list(y.names = data$names.distribution[[Nepochs]]$y.names))
        )
        )
      }
    }

    else { # 3 STEPS
      if (all(structure.learning.steps == c("local", "global", "past"))){
        selected <- data$names.distribution[[Nepochs]]$y.names
        return( list(DATA = data$data[ , selected], positions = POS[ , selected],
                     structure.learning.steps = c("local-global", "past"),
                     names.distribution = list(list(y.names = data$names.distribution[[Nepochs]]$y.names))
        )
        )
      }
      if (all(structure.learning.steps == c("local", "past", "global"))){
        selected <- data$names.distribution[[Nepochs]]$y.names
        return( list(DATA = data$data[ , selected], positions = POS[ , selected],
                     structure.learning.steps = c("local-past", "global"),
                     names.distribution = list(list(y.names = data$names.distribution[[Nepochs]]$y.names))
        )
        )
      }
    }
  }
}

###
### parseStructureLearningStepsArg
###

parseStructureLearningStepsArg <- function(structure.learning.steps, dynamic, remove.past.G){
  if ( dynamic & length(structure.learning.steps) == 1 && structure.learning.steps == 2) {
    structure.learning.steps <- c("local-global", "past")
    print(paste0("Learning process set to default dynamic 2 step:", " c(\"local-global\", \"past\")"))
  }
  if ( dynamic & length(structure.learning.steps) == 1 && structure.learning.steps == 3) {
    structure.learning.steps <- c("local", "global", "past")
    print(paste0("Learning process set to default dynamic 3 step:", " c(\"local\", \"global\", \"past\")"))
  }
  if ( !dynamic & length(structure.learning.steps) == 1 && structure.learning.steps >= 3) {
    stop("Only 2 step learning is available for non dynamic Bayesian Networks.")
  }
  if ( !dynamic & length(structure.learning.steps) >= 3 ) {
    stop("Only 2 step learning is available for non dynamic Bayesian Networks.")
  }
  if ( !dynamic & length(structure.learning.steps) == 1 && structure.learning.steps == 2){
    structure.learning.steps <- c("local", "global")
  }
  return(structure.learning.steps)
}
