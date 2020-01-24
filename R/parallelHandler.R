#' @title Parallelization Cluster Starter
#' @param type Cluster type, either FORK or PSOCK.
#' @author M.N. Legasa
#' @importFrom parallel detectCores makeCluster

parallelHandler <- function(type, n.cores,
                             PSOCK.funcExports.list = list(),
                             PSOCK.varExports.list = list(),
                             cl = NULL){

  if (is.null(cl)){   # Initiate cluster, if not already
    if ( is.null(n.cores) ){
      n.cores <- detectCores()-1
    }
    print(paste0(paste0("Starting cluster (", n.cores), " threads) for parallel computation..."))
    cl <- makeCluster( n.cores, type = type )
  }

  if (type == "PSOCK") {
    print("Exporting data to PSOCK cluster...")
    PSOCK.exports <- c(PSOCK.funcExports.list, PSOCK.varExports.list)
    print(PSOCK.exports)
    clusterExport(cl, PSOCK.exports, envir = environment())
  }
  print("Cluster good to go.")
  return(cl)
}
