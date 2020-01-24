
learnDAG <- function(structure.learning.algorithm, structure.learning.args.list,
                     parallelize, cluster.type, n.cores){
  alg <- strsplit(structure.learning.algorithm, split = ".", fixed = TRUE)[[1]][1]
  if ( (alg == "gs") | (alg == "iamb") | (alg == "fast")  | (alg == "inter") | (alg == "inter")
       | (alg == "pc") ) { # Constraint based, parallelizable
    cl <- NULL
    if ( parallelize ) { # constraint-based algorithms allow parallelization
      cl <- parallelHandler(cluster.type, n.cores)
      structure.learning.args.list[["cluster"]] <- cl
    }
    bn <- cextend( do.call(structure.learning.algorithm, structure.learning.args.list) )
    if (!(is.null(cl))) {stopCluster(cl)} # Stops parallel cluster
  }
  else if ( (alg == "mmhc") | (alg == "rsmax2") ) {
    bn <- cextend( do.call(structure.learning.algorithm, structure.learning.args.list) )
  } # Non parallelizable, needs cextend arc direction
  else { bn <-  do.call(structure.learning.algorithm, structure.learning.args.list) } # Non parallelizable, already DAG (directed)

  return(bn)
}
