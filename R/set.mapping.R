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

set.mapping = function (ccg, type="random", r=runif(1), ...) {
  ccg = init.mapping(ccg)
  if (type == "random") {
    if (r<0 | r>1) {
      r = runif(1)
    }
    nm = vcount(ccg$g1) * vcount(ccg$g2)
    ccg = add.assignments(ccg, alg="random", n = r * nm)
  }
  if (type == "greedy") {
    ccg = add.assignments(ccg, alg="greedy", n = r * nm)
  }
  return(ccg)
}



init.mapping = function (ccg, ...) {
  # Create a bipartite graph for the mapping
  n1 = vcount(ccg$g1)
  n2 = vcount(ccg$g2)
  R.V = c(rep(0, n1), rep(1, n2))
  R = graph.bipartite(R.V, NULL)
  
  # Keep track of which vertices are which
  V(R)$g1.vId = c(1:n1, rep(NA, n2))
  V(R)$g2.vId = c(rep(NA, n1), 1:n2)
  R$name = "Bipartite Mapping"
  ccg$R = R
  return(ccg)
}


