
addtoGraylistDynamic <- function(structure.learning.args.list, names.distribution,
                                 forbid.backwards, 
                                 forbid.past.DD,
                                 force.DD = NULL,
                                 forbid.past.dynamic.GD = FALSE,
                                 forbid.dynamic.GG = FALSE,
                                 forbid.GG = FALSE,
                                 forbid.DD = FALSE
                                 ){

  structure.learning.args.list <- initializeDummyGreylist(structure.learning.args.list, "blacklist")
  epochs <- length(names.distribution)

  if (forbid.backwards & epochs >= 2) {
      blacklist.list <- list()
      for (ep in 2:epochs){
        froms <- as.vector(unlist(names.distribution[[ep]]))
        tos <-  as.vector(unlist(names.distribution[1:(ep-1)]))
        blacklist.list[[ep]] <- buildBlacklist(froms, tos, bidirectional = FALSE)
      }

      structure.learning.args.list$blacklist <- rbind(structure.learning.args.list$blacklist, do.call(rbind, blacklist.list))
  }
  if (forbid.past.dynamic.GD & epochs >= 2) {
      blacklist.list <- mapply(FUN = function(froms, tos, epoch) {
                                      if (!(is.null(froms$x.names))){ # can be NULL if past G nodes have been purged
                                        return( buildBlacklist(froms$x.names, as.vector(sapply(tos[-epoch],
                                                                                               function(x) {x$y.names})),
                                                               bidirectional = TRUE) )
                                      }
                                    },
                             froms = Filter(Negate(is.null), 
                                            names.distribution[1:epochs]), 
                             epoch = seq(1:epochs),
                             MoreArgs = list(tos = names.distribution),
                             SIMPLIFY = FALSE
    )
    structure.learning.args.list$blacklist <- rbind(structure.learning.args.list$blacklist,
                                                    do.call(rbind, blacklist.list))
  }
  if (forbid.dynamic.GG & epochs >= 2) {
      blacklist.list <- mapply(FUN = function(froms, tos, epoch) {
                                       return(buildBlacklist( froms$x.names, 
                                                              as.vector(sapply(tos[-(1:epoch)], function(x)
                                                                                                                 {x$x.names})),
                                                                                                             bidirectional = TRUE ))
                                     },
                              froms = Filter( Negate(is.null), names.distribution[1:(epochs-1)] ), epoch = seq(1:(epochs-1)),
                              MoreArgs = list(tos = names.distribution),
                              SIMPLIFY = FALSE
      )
    structure.learning.args.list$blacklist <- rbind(structure.learning.args.list$blacklist, do.call(rbind, blacklist.list))
    }

  if (forbid.past.DD & epochs >= 2){
    atB <- as.vector(unlist(lapply( names.distribution[1:(epochs-1)], function(epoch) {return(epoch$y.names)})))
    if (!is.null(atB)){
      structure.learning.args.list <- addtoBlacklist2(atB, structure.learning.args.list)
    }

    #for (epoch in 1:(length(names.distribution)-1)){
    #  structure.learning.args.list <- addtoBlacklist2(names.distribution[[epoch]]$y.names, structure.learning.args.list)
    #}
  }

  if (forbid.GG){
    for (epoch in (1:length(names.distribution))){
      if (!(is.null(names.distribution[[epoch]]$x.names))){ # Can be NULL if past G nodes have been purged
        structure.learning.args.list <- addtoBlacklist2( names.distribution[[epoch]]$x.names, structure.learning.args.list )
      }
    }
  }
  if (forbid.DD){
    for (epoch in 1:length(names.distribution)){
      structure.learning.args.list <- addtoBlacklist2(names.distribution[[epoch]]$y.names, structure.learning.args.list)
    }
  }
  
  if (!is.null(force.DD)){
    if (force.DD == "-"){
      DDwhitelist <- 
        do.call(rbind,
                lapply(1:(length(names.distribution)-1), 
                       function(epoch){
                         do.call(rbind,
                                 mapply(names.distribution[[epoch]]$y.names,
                                        names.distribution[[epoch+1]]$y.names,
                                        FUN = function(x,y) buildBlacklist(fromS = x,
                                                                           toS = y, 
                                                                           T),
                                        SIMPLIFY = F
                                        )
                                 )
                       }
                       )
                )
    }
    else if (force.DD == "->"){
      DDwhitelist <- 
        do.call(rbind,
                lapply(1:(length(names.distribution)-1), 
                       function(epoch){
                         do.call(rbind,
                                 mapply(names.distribution[[epoch]]$y.names,
                                        names.distribution[[epoch+1]]$y.names,
                                        FUN = function(x,y) buildBlacklist(fromS = x,
                                                                           toS = y, 
                                                                           F),
                                        SIMPLIFY = F
                                 )
                         )
                       }
                )
        )
    }
    else if (force.DD == "<-"){
      DDwhitelist <- 
        do.call(rbind,
                lapply(1:(length(names.distribution)-1), 
                       function(epoch){
                         do.call(rbind,
                                 mapply(names.distribution[[epoch]]$y.names,
                                        names.distribution[[epoch+1]]$y.names,
                                        FUN = function(x,y) buildBlacklist(fromS = y,
                                                                           toS = x, 
                                                                           F),
                                        SIMPLIFY = F
                                 )
                         )
                       }
                )
        )
    } else stop("Provide a valid D-D link option: either -, -> or <-")
    structure.learning.args.list <-
      initializeDummyGreylist(structure.learning.args.list, "whitelist")
    structure.learning.args.list$whitelist <- 
      rbind(structure.learning.args.list$whitelist, DDwhitelist)
    
  }
  
  return(structure.learning.args.list)
}
