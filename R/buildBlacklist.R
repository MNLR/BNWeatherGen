buildBlacklist <-function(fromS, toS, bidirectional){
  # combines all the fromS with all the toS

  NtoS <- length(toS)
  blacklist.raw <- lapply(fromS, FUN =  function(from, toS) { cbind( matrix(from, NtoS, 1), toS) }, toS = toS  )
  blacklist <- do.call(rbind, blacklist.raw)
  colnames(blacklist) <- c("from", "to")

  if (bidirectional) { blacklist <- rbind( blacklist , matrix(nrow = nrow(blacklist), ncol = 2, c(blacklist[,2], blacklist[,1])) ) }

  return(blacklist)
}
