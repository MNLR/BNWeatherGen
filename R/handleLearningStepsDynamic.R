handleLearningStepsDynamic <- function(y, structure.learning.algorithm,
                                      structure.learning.args.list,
                                      structure.learning.args.list2,
                                      parallelize, cluster.type, n.cores, epochs,
                                      fix.intermediate) {
  print("Building intermediate DAG...")
  invisible(capture.output(
    descbn <- buildDescriptive(y,
                               structure.learning.algorithm = structure.learning.algorithm,
                               structure.learning.args.list = structure.learning.args.list,
                               parallelize = parallelize, cluster.type = cluster.type,
                               n.cores = n.cores, compile.junction = FALSE
                               )
    )
  )
  # plotCBN(descbn, dev = TRUE)
  whitelist_ <-
    do.call(what = rbind,
            lapply(0:(epochs-1), FUN = function(epoch_){
              return(
                apply(descbn$BN$arcs,
                      MARGIN = 2,
                      FUN = function(x, epoch_){
                        return(paste(x,paste0("T", as.character(epoch_)),sep = "."))
                      }, epoch_ = epoch_
                )
              )
            }
            )
    )

  structure.learning.args.list2 <-
    initializeDummyGreylist(slal = structure.learning.args.list2, "whitelist")

  structure.learning.args.list2$whitelist <- rbind(structure.learning.args.list2$whitelist,
                                                   whitelist_)

  if (fix.intermediate) {
    all.nodes <- names(descbn$BN$nodes)

    blacklist_ <-
      do.call(rbind,
      lapply(0:(epochs-1), FUN = function(epoch_){
                                aux <-
                                  sapply(all.nodes,
                                         FUN = function(x, epoch_){
                                          return(paste(x,paste0("T", as.character(epoch_)),
                                                       sep = ".")
                                                 )
                                         }, epoch_ = epoch_
                                  )
                    return(
                      buildBlacklist(fromS = aux, toS = aux, bidirectional = TRUE)
                    )
            }
     )
    )
    rownames(blacklist_ ) <- NULL

    structure.learning.args.list2 <-
      initializeDummyGreylist(slal = structure.learning.args.list2, "blacklist")
    structure.learning.args.list2$blacklist <- rbind(structure.learning.args.list2$blacklist,
                                                     blacklist_)
  }

  return(structure.learning.args.list2)
}
