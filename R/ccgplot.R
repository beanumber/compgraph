#' @title plot.ccgraph
#' @aliases ccgplot
#' @description Plot a collaborative composite graph
#' @details Plot a collaborative composite graph
#' 
#' @param ccg A ccgraph object
#' @param ... Parameters to be passed to plot.igraph for g1 and g2
#' 
#' @export
#' @examples
#' plot(ercg(20, 0.5))
#' 

plot.ccgraph = function (ccg, ...) {
  if (!is.ccg(ccg)) {
    cat("\nThis is not a valid CCG")
    return(FALSE)
  }
  
  g1 = ccg$g1
  g2 = ccg$g2
  R = ccg$R
  
  
  numColors = vcount(g1)
  palette = rainbow(numColors)
  
  # Set vertex attributes
  V(g1)$size = 5 * V(g1)$expertise
#  V(g1)$color = palette[degree(g1) + 1]
  V(g1)$color = palette[degree(R)[1:vcount(g1)] + 1]
  V(g1)$label = paste("v", 1:vcount(g1), sep="")
  
  V(g2)$size = 5 * V(g2)$difficulty
  complete = sapply(as.vector(V(g2)), is.completed, ccg = ccg)
  solvable = sapply(as.vector(V(g2)), is.solvable, ccg = ccg)
  V(g2)$color = ifelse(complete, "red", ifelse(solvable, "pink", "white"))
  V(g2)$label = paste("w", 1:vcount(g2), sep="")
  
  V(R)$size = c(V(g1)$size, V(g2)$size)
  V(R)$color = c(V(g1)$color, V(g2)$color)
  V(R)$label = c(paste("v", 1:vcount(g1), sep=""), paste("w", 1:vcount(g2), sep=""))
  
  # Set edge attributes
  edensity.g1 = ecount(g1) / choose(vcount(g1), 2)
  E(g1)$color = "lightgrey"
  E(g1)$width = 1 - edensity.g1
  E(g2)$color = "lightgrey"
  E(g2)$width = 1 - (ecount(g2) / choose(vcount(g2), 2))
  E(R)$color = "lightgrey"
  E(R)$width = 1 - (ecount(R) / choose(vcount(R), 2))
  
  # Set the layout
  if(!is.null(g1$layout)) { g1.layout = g1$layout } else { g1.layout = layout.kamada.kawai(g1) }
  if(!is.null(g2$layout)) { g2.layout = g2$layout } else { g2.layout = layout.fruchterman.reingold(g2) }
  
#  args = list(...)
#  cat(str(args))
  
  # Is the CCNet completed?
  if (is.completed(ccg)) { 
    clab = "\nAlls Tasks are Completed" 
  } else { 
    clab = "\nAlls Tasks are Not Completed" 
  }
  
  # Make the plot
  par(mfrow=c(1,3))
  
  plot(g1, main=paste("G1 (Social Network)\n", g1$name)
       #       , vertex.label.cex = V(g1)$size / max(V(g1)$size)
       , vertex.label.family = "serif"
       , layout=g1.layout, edge.curved=TRUE
       , xlab = paste("|V| =", vcount(g1), ", |E| =", ecount(g1), ", Edge Density =", round(edensity.g1, 3)
                      , "\nCollective Expertise =", round(collaboration(g1, ctype = ccg$ctype), 2))
       #       , xlab = paste(xlab.rad, ", Diameter =", diameter(g2), ", Broadcast Time =", G$cbtime.max )
       , ...
  )
  
  plot(R, main=paste("R (Assignments)\n", R$name)
       #       , vertex.label.cex = V(g1)$size / max(V(g1)$size)
       , vertex.label.family = "serif"
       , layout=layout.bipartite(R), edge.curved=TRUE
       , xlab = paste("|E| =", ecount(R), ", Edge Density =", round(ecount(R) / prod(table(V(R)$type)), 3), clab)
  )
  
  plot(g2, main=paste("G2 (Task Graph)\n", g2$name)
       #       , vertex.label.cex = V(g2)$size / max(V(g2)$size)
       , vertex.label.family = "serif"
       , edge.arrow.size = 0.1
       , layout=g2.layout, edge.curved=TRUE
       , xlab = paste("|V| =", vcount(g2), ", |E| =", ecount(g2), ", Diameter =", diameter(g2)
                      , "\n", sum(complete), "complete, ", sum(solvable) - sum(complete), "solvable, ", vcount(g2) - sum(solvable), "remaining"
                      , "\nMax Difficulty =", round(max(V(g2)$difficulty), 2))
       #    , xlab = paste("Diameter =", diameter(g2), ", Broadcast Time =", G$cbtime.max )
       , ...
  )
  
  par(mfrow=c(1,1))
}

layout.bipartite = function (g) {
  if (!is.bipartite(g)) {
    cat("\nThis graph is not bipartite!")
  } else {
    x = V(g)$type * 2
    n2 = sum(V(g)$type)
    n1 = vcount(g) - n2
    max.height = max(n1, n2)
    y = c(1:n1 * (max.height/n1), 1:n2 * (max.height/n2)) 
    return(cbind(x,y))
  }
}

