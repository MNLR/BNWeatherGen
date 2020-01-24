simulate1 <- function(BN.fit, junction, evidence.nodes, evidence) {

  if (length(evidence.nodes) != length(evidence)) {stop("Provide a single evidence for each
                                                        evidence node.")}
  if (!identical(sort(intersect(names(BN.fit), evidence.nodes)), sort(evidence.nodes))){
    print(names(BN.fit))
    print(evidence.nodes)
    stop("Some of the evidence.nodes are not present in the DAG.")
  }
  predictandS <- setdiff(names(BN.fit), evidence.nodes)

  mbn <- mutilated(BN.fit, evidence = auxEvidenceTocpdistInput(evidence.nodes, evidence))
  mbn <- mbn[predictandS]
  nodes.parents <- sapply(mbn, FUN = function(x) {return(length(x$parents))})
  mbn <- mbn[names(nodes.parents)[order(nodes.parents)]]
  evidence0 <- length(evidence.nodes)

  if (is.null(junction) || is.na(junction)){
    type <- "approximate"
  } else{
    type <- "exact"
  }

  dummydbn <- list(junction = junction, BN.fit = BN.fit)

  for (i in 1:length(mbn)){
    predictand <- mbn[[i]]$node
    PT <- queryBN(evidence = evidence, dbn = dummydbn,
                  evidence.nodes = evidence.nodes,
                  predictands = predictand, 
                  type = type, which_ = "marginal")
    if (type == "exact"){
      PT <- PT[[predictand]]
    }

    simulated <- sample(x = names(PT), size = 1, prob = PT)
    evidence.nodes <- c(evidence.nodes, predictand)
    evidence <- c(evidence, simulated)
  }
  unsorted.evidence <- evidence[ (evidence0+1):length(evidence) ]
  names(unsorted.evidence) <- evidence.nodes[(evidence0+1):length(evidence.nodes)]

  return(unsorted.evidence)
}

auxEvidenceTocpdistInput <- function(evidence.nodes, evidence){
  return(setNames(as.list(as.character(evidence)), evidence.nodes))
}
