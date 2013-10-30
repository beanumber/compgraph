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
  message(paste("Implementing algorithm:", alg))
  E = find.potential.assignments(ccg)
  
  if (nrow(E) == 0) {
    warning("You have already made all possible assignments!")
    return(ccg)
  }
  
  # Can't do this in the presence of capacities
#  if (is.null(brute.force(ccg, E, nrow(E)))) {
#    cat("\nThis task is unsolvable!")
#    return(ccg)
#  } else {
#    cat("\nThis task is solvable...proceeding...")
#  }
  
  if (alg == "opt") {
    opt = ccnpk.opt(ccg)
    # Add the edges
    ccg$R = ccg$R + edges(as.vector(t(opt[,1:2])))
    return(ccg)
  }
  
  # 
  R.now = ecount(ccg$R)
  R.new = R.now + 1
  while(!is.completed(ccg) & R.new > R.now) {
    R.now = ecount(ccg$R)
    ccg = add.assignments(ccg, alg=alg)
    R.new = ecount(ccg$R)
  }
  return(ccg)
}
