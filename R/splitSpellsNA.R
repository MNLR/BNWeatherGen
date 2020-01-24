splitSpellsNA <- function( ppfBNdata ) {
  # expects gaps to be given by row number

  data_ <- ppfBNdata$data
  consecutive.days <- c(NA,diff(as.Date(ppfBNdata$dates.noNA$start))==1)
  consecutive.days[1] <- TRUE

  days <- seq(1,length(consecutive.days))
  gaps <- which(!consecutive.days)

  if (length(gaps) != 0){
    for (i in 1:length(gaps)){
      days[gaps[i]:length(days)] <- days[gaps[i]:length(days)] + 1
    }

    spells <- list()
    sp.index <- 1
    i0 <- 1
    for (i in 1:(length(days)-1)){
      if ( (days[i]+1) != days[i+1] ){
        spells[[sp.index]] <- ppfBNdata
        spells[[sp.index]]$data <- data_[i0:i, , drop = FALSE]
        i0 <- i+1
        sp.index <- sp.index + 1
      }
    }
  }
  else{  # NO GAPS
    spells <- list(ppfBNdata)
  }

  class(spells) <- "splitSpellsNA"
  return(spells)
}
