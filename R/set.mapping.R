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
    ccg = add.random.assignments(ccg, n = r * nm)
  }
  if (type == "greedy") {
    # Implement the Greedy algorithm
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
  ccg$R = R
  return(ccg)
}

add.random.assignments = function (ccg, n=1) {
  n1 = vcount(ccg$g1)
  n2 = vcount(ccg$g2)
  r.full = graph.full.bipartite(n1,n2)
  r.comp = graph.difference(r.full, ccg$R)
#  plot(r.comp, layout=layout.bipartite)
  
  # Find all possible edges that don't exist
  E = get.data.frame(r.comp, what="edges")
  if (nrow(E) == 0) {
    warning("You have already made all possible assignments!")
    return(ccg)
  }
  if (n > nrow(E)) {
    n = nrow(E)
  }
  if (n < 1) {
    n = 1
  }
  edge.idx = sample(nrow(E), n)
  ccg$R = ccg$R + edges(as.vector(t(E[edge.idx,])))
  return(ccg)
}
