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
    ccg = add.greedy.assignments(ccg)
    # Implement the Greedy algorithm
    
    # Search for person with highest expertise
    #mostExp = which.max(V(ccg$g1)$expertise)
    # Assign top person to all tasks
    #How do you do this?
    
    #while()
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

#' @title add.random.assignments
#' @aliases add.greedy.assignments
#' 
#' @description Adds assignments of researchers to tasks
#' 
#' @details Augment the mapping between the two vertex sets
#' @author Ben Baumer
#' 
#' @param ccg a ccgraph object
#' @param n the number of assignments to add simultaneously
#' 
#' @return a ccgraph object
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' ccg = ccg.game(n, p)
#' plot(add.random.assignments(ccg))
#' 

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

add.greedy.assignments = function (ccg) {
  n1 = vcount(ccg$g1)
  n2 = vcount(ccg$g2)
  r.full = graph.full.bipartite(n1,n2)
  r.comp = graph.difference(r.full, ccg$R)
  #  plot(r.comp, layout=layout.bipartite)
  
  # Find all possible edges that don't exist
  E = get.data.frame(r.comp, what="edges")
  
  E$score = V(ccg$g1)$expertise[E$from]
  E$solvable = sapply(as.numeric(E$to) - vcount(ccg$g1), is.solvable, ccg=ccg)
  E.add = subset(E, solvable == FALSE)
  queue = E.add[order(E.add$score, decreasing=TRUE),]
  
  ccg$R = ccg$R + edges(queue[1,1:2])
  return(ccg)
}
