#' @title set.mapping
#' 
#' @description Set the relation R between the two graphs
#' 
#' @details Fix the mapping between the two vertex sets
#' @author Ben Baumer
#' 
#' @param G a composite graph object
#' @param type the type of mapping desired
#' @param ... Arguments to be passed to compgraph
#' 
#' @return TRUE or FALSE
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' G = ccg.game(n, p)
#' is.solvable(G, 3)
#' 
#' 
set.mapping = function (G, type = "random", r=0.5, ...) {
  # Create a bipartite graph for the mapping
  n1 = vcount(G$g1)
  n2 = vcount(G$g2)
  R.V = c(rep(0, n1), rep(1, n2))
  
  # Now determine the edges in some way
  if (type == "random") {
    if(!is.null(r) && 0 <= r && r <= 1) {
    } else {
      r = 0.5
      #      r = log(n1 + n2) / (n1 * n2)
    }
    possible.edges = expand.grid(vId = 1:n1, wId = (n1+1):(n1 + n2))
    edges = sample(possible.edges, size = n1 * n2 * r)
    R.E = as.vector(t(edges[,c("vId", "wId")]))
    #    R.E = as.vector(t(unique(cbind(g1.vIds, g2.vIds + n1))))
    R = graph.bipartite(R.V, R.E)
  }
  if (type == "one-to-one") {
    
  }
  # Keep track of which vertices are which
  V(R)$g1.vId = c(1:n1, rep(NA, n2))
  V(R)$g2.vId = c(rep(NA, n1), 1:n2)
  G$R = R
  return(G)
}
