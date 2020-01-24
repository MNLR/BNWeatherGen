
addtoBlacklist <- function(sla, data, forbid.GG, forbid.DD, forbid.DtoG, force.closest.GD,
                           closest.GD.direction, forbid.GD){

  x.names <- data$x.names
  y.names <- data$y.names

  if (forbid.GG){
    sla <- addtoBlacklist2(x.names, sla)
  }
  if (forbid.DD){
    sla <- addtoBlacklist2(y.names, sla)
  }
  if (forbid.DtoG){
    sla <- initializeDummyGreylist( sla, "blacklist" )
    sla$blacklist <- rbind(sla$blacklist, buildBlacklist(y.names, x.names, bidirectional = FALSE)
    )
  }
  if (!(is.null(force.closest.GD))){
    sla <- forceClosestG( sla, data = data, n = force.closest.GD, direction = closest.GD.direction )
  }
  if (forbid.GD) {
    sla <- initializeDummyGreylist( sla, "blacklist" )
    sla$blacklist <- rbind( sla$blacklist, buildBlacklist(data$y.names, x.names, bidirectional = TRUE) )
  }

  return(sla)
}
