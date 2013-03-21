#' @title is.completed
#' 
#' @description Determine whether a task will be completed
#' 
#' @details A recursive function that will traverse a tree and check whether tasks are completed.
#' @author Ben Baumer
#' 
#' @param ccg collaborative composite graph
#' @param w vertex id corresponding to the task
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
#' 
is.completed = function (ccg, wId = 1, ...) {
  if (!is.solvable(ccg, wId, ...)) {
    return(FALSE)
  } else {
    children = neighbors(ccg$g2, wId)
    if (length(children) == 0) {
      return(TRUE)
    } else {
      return(prod(sapply(children, is.completed, ccg=ccg)))
    }
  }
}