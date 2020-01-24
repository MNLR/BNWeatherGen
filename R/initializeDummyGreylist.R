initializeDummyGreylist <- function( slal , blackOrWhite ){
  if ( is.null(slal[[blackOrWhite]]) ){
    greylist <- matrix(, nrow = 0, ncol = 2)
    colnames(greylist) <- c("from", "to")
    slal[[blackOrWhite]] <-  greylist
  }
  return(slal)
}
