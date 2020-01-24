addtoBlacklist2 <- function(nodes, bnlearning.args.list){
  if ( !(is.null(bnlearning.args.list$debug)) ){debug <- bnlearning.args.list$debug} else {debug <- FALSE}
  global.restrictions <- buildDistanceBlacklist(names = nodes,
                                               positions = matrix(seq(1,length(nodes)), nrow = 1),
                                               distance =  0.1, debug = debug  )
  if ( !( is.null(bnlearning.args.list$blacklist) ) ){
    bnlearning.args.list$blacklist <- rbind(bnlearning.args.list$blacklist,  global.restrictions)
  }
  else { bnlearning.args.list[["blacklist"]] <- global.restrictions }
  return(bnlearning.args.list)
}
