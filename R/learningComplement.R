learningComplement <- function(nodes, positions, distance, norm_) {
  
  distances <- apply(sweep(positions, 1, positions[ ,1]), 2, norm, type = norm_)
  toS <- nodes[distances > distance]
  fromS <- array(nodes[1], length(toS))
  
  return(matrix(c(fromS, toS, toS, fromS), ncol=2 ) )
}
