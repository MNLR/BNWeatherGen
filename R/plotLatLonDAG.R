#' @title Plot a Graph placing nodes in euclidean coordinates \code{positions}
#' @author M.N. Legasa
#' @importFrom igraph graph_from_data_frame plot.igraph

plotLatLonDAG <- function(dbn, positions, no.colors = FALSE, no.labels = FALSE,
                          vertex.label.dist = 1,
                          node.size = 4, edge.width = 0.5, edge.arrow.size = 0.15,
                          mark.edge.strength = FALSE,
                          edges.color = c("azure2", "grey", "black"),
                          dev.new = FALSE, axes = TRUE, xlab = "x", ylab = "y") {
  # Plots the graph of class bn with nodes in positions and shows the nodes dependance distance as a circle, for a given distance d assumed to be euclidean distance.
  #  ---- INPUT:
  # graph             An object of class bn whose Directed Acyclic Graph is going to be plotted.
  # positions         Sorted array of the locations of x, can only be 1 or 2 dimensional, must contain the same number of columns as the number of variables in x, and number of rows is the dimension.
  #                   Won't check column names, positions must be sorted the same way as the columns (Variables) of x, the data.frame used to learn graph.
  # nodes             Index of nodes whose dependancy is going to be shown, can be a vector for several nodes. By default, nodes = 0 plots circles for all nodes. -1 will plot no circle, still
  #                   plotting nodes in given positions.
  bn <- dbn$BN

  if (dev.new) {  dev.new()  }
  else { plot.new() }

  nodes_ <- names(bn$nodes)
  if (NROW(positions) == 1){
    positions <- rbind(positions, 0)
    minx <- min(positions)
    maxx <-  max(positions)
    miny <- -distance
    maxy <- distance
  }
  else {
    minx <- min(positions[1 , ])
    maxx <- max(positions[1 , ])
    miny <- min(positions[2 , ])
    maxy <- max(positions[2 , ])
  }

  Nnodes <- length(bn$nodes)

  NodeList <- data.frame(nodes_, positions[1, ] , positions[2, ])
  EdgeList <- data.frame(bn$arcs)
  a <- graph_from_data_frame(vertices = NodeList, d = EdgeList)

  nodes <- seq(1, Nnodes)
  COL <- "black"
  if ( (length(nodes) == 1 && nodes != -1) | (length(nodes) != 1) ) {
    cpositions <- as.matrix(positions[ ,nodes] )
  }

  if (mark.edge.strength){
    valueS <- arc.strength(bn, dbn$training.data, criterion = "bic")
    rbPal <- unique(colorRampPalette(edges.color)(1000))
    colored.edges <- abs(valueS[,3])
    edge.color.pattern <- rbPal[round(1 + (length(rbPal) - 1)/(max(colored.edges)-min(colored.edges))*(colored.edges - min(colored.edges)))]
  }
  else{
    if (!is.null(edges.color)) {
      Nedges <- nrow(EdgeList)
      GtoD <- intersect(grep(pattern = "G", EdgeList[, 1]), grep(pattern = "D",
                                                                 EdgeList[, 2]) )
      DtoG <- intersect(grep(pattern = "D", EdgeList[, 1]), grep(pattern = "G", 
                                                                 EdgeList[, 2]) )
      GtoG <- intersect(grep(pattern = "G", EdgeList[, 1]), grep(pattern = "G",
                                                                 EdgeList[, 2]) )
      # TXtoTY <- intersect(grep(pattern = "T", EdgeList[, 1]), grep(pattern = "G", EdgeList[, 2]))


      edge.color.pattern <- rep(edges.color[length(edges.color)], Nedges)
      if (length(edges.color) == 2) {
        edge.color.pattern[c(GtoD, DtoG, GtoG)] <- edges.color[2]
      }
      else if (length(edges.color) == 3){
        edge.color.pattern[c(GtoD, DtoG)] <- edges.color[2]
        edge.color.pattern[c(GtoG)] <- edges.color[3]
      }
      else if (length(edges.color) >= 4){
        edge.color.pattern[GtoD] <- edges.color[2]
        edge.color.pattern[DtoG] <- edges.color[3]
        edge.color.pattern[c(GtoG)] <- edges.color[4]
      }
    }
  }

  if (no.labels){ plot.igraph(a, layout=t(positions),
                              vertex.size = node.size,
                              vertex.label.dist = vertex.label.dist,
                              vertex.color=COL, vertex.label=NA,  rescale=F,
                              edge.color = edge.color.pattern,
                              xlim=c(minx, maxx), ylim=c(miny, maxy),
                              xlab = xlab, ylab = ylab, asp=FALSE, axes = axes,
                              edge.width = edge.width, 
                              edge.arrow.size = edge.arrow.size)
  } else { plot.igraph(a, layout=t(positions),
                       vertex.size = node.size,
                       vertex.label.dist = vertex.label.dist,
                       vertex.color=COL,
                       rescale=F,
                       edge.color = edge.color.pattern,
                       xlim=c(minx, maxx), ylim=c(miny, maxy), 
                       xlab = xlab, ylab = ylab, asp = FALSE , axes = axes,
                       edge.width = edge.width, edge.arrow.size = edge.arrow.size)
  }
}

