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
  # Check for valid entries for the probability of an edge
  if(p1 <= 0 | p1 > 1) { 
    p1 = 1/n1
    p2 = p1
  } else {
    g1 = erdos.renyi.game(n1, p1)
    # Draw the researcher's expertise from an exponential distribution
    # with mean 2
    lambda.s = 1/2
    V(g1)$expertise = rexp(vcount(g1), rate=lambda.s)
    if (n2 == 1) {
      g2 = graph.empty(1)   # a single node
    } else {
      g2 = graph.tree(n2, 2)   # a regular binary tree
    }
    # Draw the difficulty from an exponential distribution as well
    # Set lambda.t = lambda.s / n.s
    V(g2)$difficulty = rexp(vcount(g2), rate = (lambda.s / ((1 + p1) * vcount(g1))))  
    return(compgraph(g1, g2, ...)) 
  }
}