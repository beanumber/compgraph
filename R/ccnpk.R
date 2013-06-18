#' @title ccnp-k
#' 
#' @description Compute solutions to the CCNP-k problem
#' 
#' @details Calls various algorithms
#' @author Ben Baumer
#' 
#' @param ccg a ccgraph object
#' @param ... Arguments to be passed to compgraph
#' 
#' @return A solution, or NULL is the tasks are unsolvable
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' ccg = ccg.game(n, p, capacity=2)
#' ccnpk(ccg)
#' 
#' 

ccnpk = function (ccg, alg = "greedy", ...) {
  
  E = find.potential.assignments(ccg)
  
  if (nrow(E) == 0) {
    warning("You have already made all possible assignments!")
    return(ccg)
  }
  
  if (is.null(brute.force(ccg, E, nrow(E)))) {
    cat("\nThis task is unsolvable!")
    return(ccg)
  }
  
  if (alg == "opt") {
    opt = ccnpk.opt(ccg)
    # Add the edges
    ccg$R = ccg$R + edges(as.vector(t(opt[,1:2])))
    return(ccg)
  }
  while(!is.completed(ccg) & nrow(find.potential.assignments(ccg)) > 0) {
    ccg = add.assignments(ccg, alg=alg)
  }
  return(ccg)
}
