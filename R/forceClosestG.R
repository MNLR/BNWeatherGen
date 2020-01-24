forceClosestG <- function(sla, data, n, direction){
  sla <- initializeDummyGreylist(sla, "whitelist")
  whitelist_ <- closestG(n, data$x.names, data$y.names, data$positions, direction)
  sla$whitelist <- rbind(sla$whitelist, whitelist_)
  return(sla)
}
