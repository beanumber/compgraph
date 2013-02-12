#' @title is.solvable
#' 
#' @description Determines whether a task is solvable
#' 
#' @details Determines whether a task is solvable
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
is.solvable = function (G, wId = 3, ...) {
  difficulty = V(G$g2)$difficulty[wId]
  rId = min(which(V(G$R)$g2.vId == wId))
  researchers = neighbors(G$R, rId)
  g.s = induced.subgraph(G$g1, researchers)
  if (collaboration(g.s) > difficulty) { 
    return(TRUE)
  } else {
    return(FALSE)
  }
}