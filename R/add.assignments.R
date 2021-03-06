#' @title add.assignments
#' @aliases find.potential.assignments
#' 
#' @description Adds assignments of researchers to tasks
#' 
#' @details Augment the mapping between the two vertex sets
#' @author Ben Baumer
#' 
#' @param ccg a ccgraph object
#' @param alg the type of algorithm to use
#' @param blind a logical indicating if the algorithm is blind to task completion
#' @param n the number of assignments to add simultaneously
#' 
#' @return a ccgraph object
#' 
#' @export
#' @export find.potential.assignments
#' 
#' @examples
#' n = 20
#' p = 1/2
#' ccg = ccg.game(n, p)
#' plot(add.assignments(ccg))
#' 

add.assignments = function (ccg, alg="random", blind=FALSE, n=1) {
  if (n < 1) {
    warning("No assignments to make")
    return(ccg)
  }
  E = find.potential.assignments(ccg, blind)
  if (nrow(E) == 0) {
    warning("You have already made all possible assignments!")
    return(ccg)
  }
  
  # Set scores according to some criteria
  if (alg == "random") {
    E$score = runif(nrow(E))
  }
  if (alg == "greedy-dumb") {
    E$score = V(ccg$g1)$expertise[E$from]
  }
  if (alg == "greedy-smart") {
    E$score = V(ccg$g1)$expertise[E$from] * V(ccg$g2)$difficulty[E$to - vcount(ccg$g1)]
  }
  if (alg == "greedy") {
    E$score = apply(as.matrix(E[,1:2]), 1, collaboration.update, ccg=ccg)
  }
  
  queue = E[order(E$score, decreasing=TRUE),]
#  cat(nrow(queue))
  if (nrow(queue) == 0) {
    warning("No edges to add!")
    return(ccg)
  }
  if (n > nrow(queue)) {
    n = nrow(queue)
  }
  
  # Add the edges
  ccg$R = ccg$R + edges(as.vector(t(queue[1:n,1:2])))
  return(ccg)
}

find.potential.assignments = function (ccg, blind = FALSE,...) {
  n1 = vcount(ccg$g1)
  n2 = vcount(ccg$g2)
  r.full = graph.full.bipartite(n1,n2)
  r.comp = graph.difference(r.full, ccg$R)
  #  plot(r.comp, layout=layout.bipartite)
  
  # Find all possible edges that don't exist
  E = get.data.frame(r.comp, what="edges")
  
  # Remove the edges from researchers that have already reached their capacity
  tapped.out = which(degree(ccg$R)[1:n1] >= V(ccg$g1)$capacity)
  E = subset(E, !from %in% tapped.out)
  
  # Are we allowed to know which tasks are complete?
  if (!blind) {
    # Restrict the edges to only those tasks that are unsolved  
    solvable = sapply(1:vcount(ccg$g2), is.solvable, ccg=ccg)
    E = transform(E, solvable = solvable[E$to - vcount(ccg$g1)])
    E = subset(E, solvable == FALSE)
  }  
  return(E)
}

collaboration.update = function(ccg, e) {
  wId = e[2] - vcount(ccg$g1)
  c0 = collaboration(assigned(ccg, wId), ctype=ccg$ctype)
  ccg$R = ccg$R + edges(e)
  c1 = collaboration(assigned(ccg, wId), ctype=ccg$ctype)
  return(c1 - c0)
}
