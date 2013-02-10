getVertexSize = function (g, max.vertex.size = 20) {
  # Set vertex attributes
  return(max.vertex.size * (radius(g) / eccentricity(g))^2)
}

getVertexColor = function (g) {
  numColors = max(eccentricity(g)) - radius(g) + 1
  return(rainbow(numColors)[eccentricity(g) - radius(g) + 1])
}

#  Given the list of all paths in G1, and the corresponding stretch data frame, find the path with the longest stretch
getG1PathLongestCStretch = function (G) {
  if (is.connected(G$g2)) {
    longest.cstretch.index = sample(which(G$cbtime$cstretch == max(G$cbtime$cstretch)), 1)
    row = G$cbtime[longest.cstretch.index,]
    g1.paths = get.shortest.paths(G$g1, from=row$root.index, to=row$leaf.index)
    path = unlist(g1.paths[1])
    return(path)
  } else {
    cat("\nG2 is not connected")
    return(NULL)
  }
}

#' @title plot.compgraph
#' @description Plot a composite graph
#' @details Plot a composite graph
#' 
#' @param G A composite graph object
#' @param hilite.cbtime A boolean value
#' @param ... Parameters to be passed to plot.igraph for g1 and g2
#' @export
#' @examples
#' plot(ercg(20, 0.5))

plot.compgraph = function (G, hilite.cbtime = FALSE, suppress.vertex.labels=FALSE, ...) {

  g1 = G$g1
  g2 = G$g2
  
  # Set vertex attributes
  V(g1)$size = getVertexSize(g1, 10)
  V(g1)$color = getVertexColor(g1)
  
  V(g2)$size = getVertexSize(g1, 10)
  #  V(g2)$size = 1
  V(g2)$color = getVertexColor(g1)
  if (suppress.vertex.labels) {
    V(g2)$label = NA
  }
  
  # Set edge attributes
  E(g1)$color = "lightgrey"
  E(g1)$width = 1
  E(g2)$color = "lightgrey"
  E(g2)$width = 1
  
  if(hilite.cbtime) {
    g1.hilite = getG1PathLongestCStretch(G)
    E(g1, path=g1.hilite)$color = "red"
    E(g1, path=g1.hilite)$width <- 4
    
    if (!is.connected(g2)) {
      cat("\nCannot hilite maximum cstretch: G2 is not connected")
    } else {
      g1.hilite = getG1PathLongestCStretch(G)
      g2.hilite = citinerary(G, g1.hilite)
      E(g2, path=g2.hilite)$color = "red"
      E(g2, path=g2.hilite)$width = 4
    }
  }
  
  # Set the layout
  if(!is.null(g1$layout)) { g1.layout = g1$layout } else { g1.layout = layout.kamada.kawai(g1) }
  if(!is.null(g2$layout)) { g2.layout = g2$layout } else { g2.layout = layout.fruchterman.reingold(g2) }
  
  if(!is.null(G$g2$r)) {
    xlab.rad = paste("Radius of Connectivity =", round(G$g2$r,2))
  } else {
    xlab.rad = ""
  }
  
  # Make the plot
  par(mfrow=c(1,2))
  
  plot(g1, main=paste("G1:", g1$name)
       , vertex.label.cex = V(g1)$size / max(V(g1)$size)
       , vertex.label.family = "serif"
       , edge.arrow.size = 0.5, edge.label.family = "Palatino"
       , layout=g1.layout, edge.curved=TRUE
       , xlab = paste("|V| =", vcount(g1), ", |E| =", ecount(g1), ", Diameter =", diameter(g1))
#       , xlab = paste(xlab.rad, ", Diameter =", diameter(g2), ", Broadcast Time =", G$cbtime.max )
       , ...
  )
  
  plot(g2, main=paste("G2:", g2$name)
       , vertex.label.cex = V(g2)$size / max(V(g2)$size)
       , vertex.label.family = "serif"
       , edge.arrow.size = 0.5, edge.label.family = "Palatino"
       , layout=g2.layout, edge.curved=TRUE
       , xlab = paste("|V| =", vcount(g2), ", |E| =", ecount(g2), ", Diameter =", diameter(g2))
#    , xlab = paste("Diameter =", diameter(g2), ", Broadcast Time =", G$cbtime.max )
    , ...
  )
  
  par(mfrow=c(1,1))
}



plot.T = function (G, ...) {
  T = G$T
  
  # Set vertex attributes
  V(T)$size = getVertexSize(T, 20)
  V(T)$color = getVertexColor(T)
  
  # Set edge attributes
  E(T)$color = "lightgrey"
  E(T)$width = 1

  plot(T, main=paste("G1:", G$g1$name)
    , vertex.label.cex = V(T)$size / max(V(T)$size)
    , edge.arrow.size = 0.2, layout=layout.reingold.tilford
    , xlab = paste("Number of Nodes =", length(V(T)), ", Diameter =", diameter(T))
    , ...
  )
}



pdf.compgraph = function (G, filepath, ...) {
  pdf(filepath, width=16, height=8, fonts=c("serif", "Palatino", "Helvetica"))
  plot(G, ...)
  dev.off()
}
