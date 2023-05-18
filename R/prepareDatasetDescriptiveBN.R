
prepareDatasetDescriptiveBN <- function(y, discretize = FALSE) {
  idS <- as.vector(sapply(y$Metadata$station_id, function(x){return(paste0("D.",x))}))
  # prepending of "D." is compulsory due to limitations in as.grain()
  data <- as.data.frame(y$Data)

  rownames(data) <- seq(1, nrow(y$Data))
  index.NA <-  complete.cases(data)
  dates <- list(start = y$Dates$start[index.NA],
                end = y$Dates$end[index.NA]
            )
  data <- data[ complete.cases(data) , ]

  rr <- nrow(y$Data) - sum(index.NA)
  if (rr != 0){ print(paste0("Removed ", rr , " observations with NA values." ))}

  if (discretize){
    for (j in 1:ncol(data)){
      data[, j] <- as.factor(data[ , j])
    }
  }

  colnames(data) <- idS
  positions <- t(as.matrix(y$xyCoords))
  colnames(positions) <- idS
  data <- list(data = data, positions = positions, Metadata = y$Metadata,
                nx = 0, ny = ncol(positions), dates = y$Dates, dates.noNA = dates
               )
  class(data) <- "pp.forDescBN"

  return(data)
}
