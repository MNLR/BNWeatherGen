closestG <- function( n, x.names, y.names, positions, direction ) {
  G.poS <- positions[ , x.names]
  D.poS <- positions[ , y.names]

  closest.GS <- apply(D.poS, MARGIN = 2,
                     FUN = function(D, G.poS, n, x.names){
                            distanceS <- order(apply( G.poS - D, MARGIN = 2, FUN = norm, type = "2"))
                            return(x.names[distanceS[1:n]])
                     },
                     G.poS = G.poS, n = n, x.names = x.names
                     )
  if (n == 1){
    closest.GS <- matrix(closest.GS, ncol = length(closest.GS))
  }
  colnames(closest.GS) <- y.names

  if (is.null(direction)){
    direction <- "up"
    bidirectional <- TRUE
  } else {
    bidirectional <- FALSE
  }

  whitelist.list <- apply(closest.GS, MARGIN = 1,
                          function(closest.G, Ds, direction, bidirectional){
                            if (direction == "up") {
                            fromS <- Ds
                            toS <- closest.G
                            } else {
                              fromS <- closest.G
                              toS <- Ds
                            }
                            return(mapply(FUN = buildBlacklist, fromS = fromS,
                                           toS =  toS,
                                           MoreArgs = list(bidirectional = bidirectional),
                                           SIMPLIFY = FALSE
                                          )
                                   )
                          },
                          Ds = y.names, direction = direction, bidirectional = bidirectional
                          )

  if (bidirectional){ #additional processing is needed
    whitelist_ <- do.call(rbind, lapply(lapply(whitelist.list, FUN = function(x){do.call(cbind, x)}), t))
    colnames(whitelist_) <- c("from", "to")
    rownames(whitelist_) <- NULL
  } else {
    whitelist_ <- do.call(rbind, lapply(whitelist.list, function(x) return(do.call(rbind, x))))
  }

  return(whitelist_)
}
