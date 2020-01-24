buildDistanceBlacklist <- function(names, positions, distance, exceptions = NULL, blacklist = NULL, norm = "2",
                                    plotrestrictions = FALSE, debug = FALSE) {

  try(if(length(names) != NCOL(positions)) stop("Number of positions is not equal to number of nodes."))

  if ( !(is.null(exceptions)) ){
    names <- names[ -exceptions ]
    positions <- positions[ , - exceptions]
  }

  names.list <- list()
  positions.list <- list()
  Nnodes <- length(names)

  nrow_ <- NROW(positions)
  for (i in 1:(Nnodes - 1) ) {
    names.list[[i]] <- names
    names <- names[2:length(names)]
    positions.list[[i]] <- positions
    positions <- matrix(positions[ , 2:NCOL(positions)], nrow = nrow_) # matrix needs to be preserved
  }

  distanceBlacklist <- mapply(FUN = learningComplement, nodes = names.list, 
                              positions = positions.list,
                              MoreArgs = list(distance = distance, norm_ = norm) )
  distanceBlacklist <- do.call("rbind", distanceBlacklist)

  if (debug){
    print("Generated blacklist: ")
    print(distanceBlacklist)
  }

  if (is.null(blacklist) ) { 
    blacklist <- matrix(nrow = 0, ncol = 2, byrow = TRUE,
                        dimnames = list(NULL, c("from", "to")))
  }
  blacklist <- rbind(blacklist, distanceBlacklist)

  return(blacklist)
}
