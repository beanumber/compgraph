#' @title ercg
#' 
#' @description Generate an Erdos-Renyi random composite graph
#' 
#' @details Generates two Erdos-Renyi graphs, and then returns the resulting
#' composite graph object
#' @author Ben Baumer
#' 
#' @param n1 Number of vertices in g1
#' @param p1 Probability of edge in g1
#' @param n2 Number of vertices in g2. Default value is n1
#' @param p2 Probability of edge in g2. Default value is p1
#' @param ... Arguments to be passed to compgraph
#' 
#' @return A compgraph object
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' G = ercg(n, p)
#' 
#' 
ercg = function (n1, p1, n2 = n1, p2 = p1, ...) {
  if(p1 <= 0 | p1 > 1) { 
    p1 = 1/n1
    p2 = p1
  } else {
    g1 = erdos.renyi.game(n1, p1)
    g2 = erdos.renyi.game(n2, p2)
    return(compgraph(g1, g2, ...)) 
  }
}