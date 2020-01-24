reallocateDynamicNodes <- function(positions, names.distribution, break.axis = 1, Nepochs,
                                   separation.ratio = 0.1){

  range <- abs(max(positions[break.axis, ]) - min(positions[break.axis,]))
  separation <- range + range*separation.ratio
  #names <- as.vector(sapply(auxnames, FUN = function(x) { return(paste0(x, ".T0") ) } ))
  nodes.gapS <- sapply(names.distribution, FUN = function(x) {return(length( unlist(x) ))})

  auxpos <- positions
  cum.nodes.gapS <- nodes.gapS[1]
  for (i in 2:Nepochs){
    positions[ break.axis, (cum.nodes.gapS+1):(cum.nodes.gapS + nodes.gapS[i]) ] <- auxpos[ break.axis, (cum.nodes.gapS+1):(cum.nodes.gapS + nodes.gapS[i]) ] + (i-1)*separation
    cum.nodes.gapS <- cum.nodes.gapS + nodes.gapS[i]
    #positions <- cbind(positions, auxpos)
    #names <- c(names, as.vector(sapply(auxnames, FUN = function(x, ep) { return( paste0( x, paste0(".T", as.character(ep)) ) ) }, ep = i) ))
  }

  attr(positions, 'separation') <- separation
  return(positions)
}
