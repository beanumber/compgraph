#' @title ccnpk.greedy
#' 
#' @description Compute a greedy solution to the CCNP-k problem 
#' 
#' @details Tries complete all tasks
#' @author Ben Baumer
#' 
#' @param ccg a ccgraph object
#' @param ... Arguments to be passed to compgraph
#' 
#' @return A greedy solution, or NULL is the tasks are unsolvable
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' ccg = ccg.game(n, p)
#' ccnpk.greedy(ccg)
#' 
#' 

ccnpk.greedy = function(ccg, ...) {
  E = find.potential.assignments(ccg)
  if (nrow(E) == 0) {
    warning("You have already made all possible assignments!")
    return(ccg)
  }
  # Find the number of unsolved tasks
  k = length(unique(E$to))
  
  if (is.null(brute.force(ccg, E, nrow(E)))) {
    cat("\nThis task is unsolvable!")
    return(NULL)
  }
  
  unsolved.tasks = V(ccg$g2)[!solvable]
  wId = 2
  difficulty = V(ccg$g2)$difficulty[wId]
  g.s = assigned(ccg, wId)
  available = unique(E$from)
  
  E$score = apply(as.matrix(E[,1:2]), 1, collaboration.update, ccg=ccg)
  
  # Todo: optimize this loop
  for(i in k:nrow(E)) {
    win = brute.force(ccg, E, i)
    if (!is.null(win)) {
      #      cat(win)
      return(i)
    }
  }
}

collaboration.update = function(ccg, e) {
  wId = e[2] - vcount(ccg$g1)
  c0 = collaboration(assigned(ccg, wId), ctype=ccg$ctype)
  ccg$R = ccg$R + edges(e)
  c1 = collaboration(assigned(ccg, wId), ctype=ccg$ctype)
  return(c1 - c0)
}

