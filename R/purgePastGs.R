
purgePastGs <- function(Ddata, epS){

  nx <- Ddata$nx
  ny <- Ddata$ny

  if (epS >= 2){
    purge.index <- c()
    for (ep in 1:(epS-1)){
      aux.purge.index <- seq(((ep-1)*(nx+ny))+1, ep*nx + (ep-1)*ny)
      purge.index <- c(purge.index , aux.purge.index)
      Ddata$names.distribution[[ep]]$x.names <- NULL
    }
  }

  Ddata$data <- Ddata$data[ , -purge.index]
  Ddata$positions <- Ddata$positions[, -purge.index]

  return(Ddata)
}
