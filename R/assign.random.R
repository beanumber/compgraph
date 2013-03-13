#' @title assign.random
#' 
#' @description Randomly assign researchers to tasks until all tasks are complete
#' 
#' @details Randomly assign researchers to tasks until all tasks are complete
#' @author Ben Baumer
#' 
#' @param ccg a composite graph object
#' @param ... Arguments to be passed to compgraph
#' 
#' @return A ccg with all tasks complete, or FALSE
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' ccg = ccg.game(n, p, n2=2)
#' is.completed(ccg)
#' ccg = assign.random(ccg)
#' is.completed(ccg)
#' 

assign.random = function (ccg, ...) {
  while(!is.completed(ccg) & ecount(ccg$R) < vcount(ccg$g1) * vcount(ccg$g2)) {
    ccg = add.random.assignments(ccg, n=1)
  }
  if (!is.completed(ccg)) {
    cat("\nAll tasks could not be completed")
  }
  return(ccg)
}
