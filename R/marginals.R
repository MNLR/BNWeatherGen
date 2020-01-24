
marginals <- function(dbn, use.junction = FALSE){
  BN <- dbn$BN
  Nglobal <- dbn$NX
  junction <- dbn$junction
  td <- dbn$training.data

  if (Nglobal > 0){
    predictands <- names(BN$nodes)[- (1:Nglobal) ]
  } else {predictands <- names(BN$nodes)}

  if (use.junction && !is.null(junction)){
    MPT <-sapply(predictands,
                 function(pred, junction) {
                    querygrain(junction, nodes = pred)[[1]]
                 },
                 junction = junction)
  }
  else{
    MPT <- apply( td[ , (Nglobal+1):ncol(td)], MARGIN = 2, FUN = function(x) {
                                                                  tx <- table(x)
                                                                  return(t(tx/sum(tx)))
                                                                }
           )
    MPTnames <- apply( td[ , (Nglobal+1):ncol(td)], MARGIN = 2, FUN = function(x) {
                                                                        return(names(table(x)))
                                                                      }
                      )

    attr(MPT, "names") <- MPTnames
  }

  return( MPT )
}
