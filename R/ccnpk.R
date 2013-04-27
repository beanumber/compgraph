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
  if (alg == "opt") {
    opt = ccnpk.opt(ccg)
    # Add the edges
    ccg$R = ccg$R + edges(as.vector(t(opt[,1:2])))
    return(ccg)
  }
  while(!is.completed(ccg) & ecount(ccg$R) < vcount(ccg$g1) * vcount(ccg$g2)) {
    ccg = add.assignments(ccg, alg=alg)
  }
  return(ccg)
}
