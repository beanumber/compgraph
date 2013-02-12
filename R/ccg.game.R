#' @title ccg.game
#' 
#' @description Generate a random collaborative composite graph
#' 
#' @details Generates a random collaborative composite graph then returns the resulting
#' composite graph object
#' @author Ben Baumer
#' 
#' @param n1 Number of vertices in g1
#' @param p1 Probability of edge in g1
#' @param n2 Number of vertices in g2. Default value is n1.
#' @param ... Arguments to be passed to compgraph
#' 
#' @return A compgraph object
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' G = ccg.game(n, p)
#' 
#' 
ccg.game = function (n1, p1, n2 = n1, ...) {
  if(p1 <= 0 | p1 > 1) { 
    p1 = 1/n1
    p2 = p1
  } else {
    g1 = erdos.renyi.game(n1, p1)
    V(g1)$expertise = rep(1, vcount(g1))
    g2 = graph.tree(n2, 2)   # a regular binary tree
    V(g2)$difficulty = rep(1, vcount(g2))
    return(compgraph(g1, g2, ...)) 
  }
}