#' @title Plot Bayesian Network Weather Model
#' @param CBN Downscaling Bayesian Network, as returned 
#' by \code{buildWeatherGenerator()}, \code{buildPredictive()} or
#' \code{buildDescriptive()}. Uses \code{igraph} package.
#' @param title Plot title.
#' @param dev.new Plot in a new device.
#' @param no.labels Do not print labels
#' @param vertex.label.dist Distance from node to labels
#' @param no.colors Set to \code{TRUE} to remove all coloring.
#' @param return.plot Set to TRUE to return the plot as a variable.
#' @param ... Refer to \code{igraph} package.
#' @author Mikel N. Legasa
#' @export

plotCBN <- function(CBN, title = NULL, dev.new = FALSE, no.labels = FALSE,
                    vertex.label.dist = 0.8, no.colors = TRUE, node.size = 3,  
                    edge.width = 0.6, edge.arrow.size = 0.2,
                    edges.color = c("azure3", "grey", "azure4", "black"),
                    mark.edge.strength = FALSE,
                    break.axis = 1, separation.ratio = 0.1, Nlabels = 4, 
                    return.plot = TRUE){

  if (!(is.null(CBN$dynamic.args.list))){
    CBN$positions <- reallocateDynamicNodes(CBN$positions,
                                            names.distribution = CBN$names.distribution, 
                                            break.axis, CBN$dynamic.args.list$epochs,
                                            separation.ratio = separation.ratio)

    sep <- attributes(CBN$positions)$separation
    if (!is.null(CBN$dynamic.args.list$remove.past.G) &&
        CBN$dynamic.args.list$remove.past.G){ # purged past G nodes
      nx <- CBN$NX
      ny <- CBN$NY
      epS <- CBN$dynamic.args.list$epochs

      purge.index <- 1:nx
      aux.purge.index <- purge.index
      #if (epS > 2){
      #  for (ep in 1:(epS-2)){
      #    purge.index <- c(purge.index , aux.purge.index + (nx+ny))
      #  }
      #}
      #CBN$positions <-CBN$positions[ , -purge.index]
    }
    axes <- FALSE
  } else { axes <- TRUE }

  plotLatLonDAG( dbn = CBN,
                 positions = CBN$positions,
                 vertex.label.dist = vertex.label.dist,
                 node.size = node.size, 
                 no.colors = no.colors,
                 no.labels = no.labels, edge.width = edge.width,
                 edge.arrow.size = edge.arrow.size, 
                 mark.edge.strength = mark.edge.strength,
                 edges.color = edges.color,
                 dev.new = dev.new, 
                 xlab = "Longitude", ylab = "Latitude", axes
                 )

  if (!is.null(title)) {title(title)} else {
    if (is.null(CBN$structure.learning.args.list$distance)) { ds <- ""} else {
      ds <- paste0("d=",as.character(CBN$structure.learning.args.list$distance))}
    title( paste0(list(CBN$BN$learning$algo, ds)) )

  }
  if (!(is.null(CBN$dynamic.args.list))){ # Aditional operations for dynamic node placement:
    mn <- min(CBN$positions[break.axis, ])
    mx <- max(CBN$positions[break.axis, ])
    range <- abs( mx - mn )
    par(xpd=FALSE)
    for (i in 1:(CBN$dynamic.args.list$epochs - 1)){
      abline(v = c(mn + i*range/CBN$dynamic.args.list$epochs) )
    }
    par(xpd=TRUE)

    # Fix broken axis:
    N.atempnodes <- CBN$NX + CBN$NY
    eps <- CBN$dynamic.args.list$epochs

    min.axis <- min(CBN$positions[ break.axis ,
                                   (ncol(CBN$positions)-N.atempnodes):ncol(CBN$positions)])
    max.axis <- max(CBN$positions[ break.axis ,
                                   (ncol(CBN$positions)-N.atempnodes):ncol(CBN$positions)])

    range <- abs(max.axis - min.axis)

    label.positions <- seq(min.axis, max.axis, by = range/Nlabels)
    aux.label.positions <- label.positions

    for (ep in 1:(eps-1)){
      label.positions <- c(label.positions, aux.label.positions + ep*sep)
    }

    # Broken axis fixed
    labelS <- sprintf(rep(aux.label.positions, eps), fmt = '%#.1f')
    axis(break.axis, at=label.positions, labels=labelS)
    axis(as.numeric(xor(1,break.axis-1)) + 1)
  }
  if (return.plot){
    tbr <- recordPlot()
    return(tbr)
  }
}
