#' @title collaboration
#' 
#' @description Computes the collaboration score for a subgraph of the social network
#' 
#' @details Computes the collaboration score for a subgraph of the social network
#' @author Ben Baumer
#' 
#' @param g an igraph object representing the social network of the researchers
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
collaboration = function (g, ...) {
  return(sum(V(g)$expertise) * (1 + ecount(g) / ((vcount(g) * vcount(g) - 1) / 2)))
}