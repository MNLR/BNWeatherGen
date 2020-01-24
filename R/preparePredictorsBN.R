#' preparePredictorsBN()
#' @title Prepare datasets from \code{loadeR} for the BN.
#' @param grid Expects output from \code{\link[downscaleR]{prepare_predictors}}.
#' @return A processed dataset to be passed to build.downscalingBN().
#' @author MN Legasa

preparePredictorsBN <- function(grid, rm.na = TRUE , rm.na.mode = "observations" ) {
  # preparePredictorsBN() prepares standard climate4R grids, as output from prepare_predictors(), to be used for BN downscaling.
  # grid$x.global
  # grid$y              are expected.
  # rm.na parameter should probably not be set to FALSE
  # rm.na.mode shoud probably not be changed.

  x.positions <- t(expand.grid(attributes(grid)$xyCoords$y,
                               attributes(grid)$xyCoords$x ))[2:1, ]
  nx <- NCOL(x.positions)
  x.names <- mapply(paste0, array("G", nx), seq(1,nx), SIMPLIFY = TRUE, USE.NAMES = FALSE)
  y.positions <- t(grid$y$xyCoords)
  ny <- NCOL(y.positions)
  if (!is.null(grid$y$Metadata$station_id)){
    ynames_ <- grid$y$Metadata$station_id
  } else { ynames_ <- seq(1,ny) }
  y.names <- mapply(paste0, array("D", ny), ynames_ , SIMPLIFY = TRUE, USE.NAMES = FALSE)

  positions <- cbind( x.positions, y.positions )
  colnames(positions) <- c(x.names, y.names)
  rownames(positions) <- c("x","y")

  data <- cbind.data.frame(grid$x.global, grid$y$Data)
  names(data) <- colnames(positions)

  if (rm.na){
    NCOL0 <- NCOL(data)
    NROW0 <- NROW(data)
    if (rm.na.mode == "stations"){
      NAS <- colSums(is.na(data))
      data <- data[ , NAS == 0]
      positions <- positions[ , NAS == 0]
      nx <- length(grep("G", colnames(positions)))
      ny <- length(grep("D", colnames(positions)))
      dates <- grid$y$Dates
    }
    else {
      NAS <- rowSums(is.na(data))
      data <- data[ complete.cases(data) , ]
      dates <- list(start = grid$y$Dates$start[NAS == 0],
                    end = grid$y$Dates$end[NAS == 0]
                    )
    }

    rc <- NCOL0 - NCOL(data)
    rr <- NROW0 - NROW(data)
    if (rc != 0){ print(paste0("Removed ", rc , " stations which contained NA values.")) }
    if (rr != 0){ print(paste0("Removed ", rr , " observations with NA values." ))}
  }
  else { NAS = NULL }

  # Makes sure data columns are factors.
  data[] <- lapply( data, factor) # the "[]" keeps the dataframe structure
  col_names <- names(data)

  NA.count <- NAS
  NAS[ NAS > 0] <- TRUE
  NAS[ NAS == 0 ] <- FALSE

  dates.noNA <- list(start = grid$y$Dates$start[ !NAS ],
                     end = grid$y$Dates$end[ !NAS ]
                     )
  pdata <- list(data = data, positions = positions, x.names = x.names, nx = nx,
                y.names = y.names, ny = ny, NA.count = NA.count, dates = grid$y$Dates,
                dates.noNA = dates.noNA
                )

  class(pdata) <- "pp.forBN"

  return( pdata )
}
