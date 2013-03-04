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
is.solvable = function (ccg, wId = 1, ...) {
  difficulty = V(ccg$g2)$difficulty[wId]
  g.s = assigned(ccg, wId)
  if (collaboration(g.s) >= difficulty) { 
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title assigned
#' 
#' @description Figure out who is assigned to a particular task
#' 
#' @details Figure out who is assigned to a particular task
#' @author Ben Baumer
#' 
#' @param ccg collaborative composite graph
#' @param w vertex id corresponding to the task
#' @param ... Arguments to be passed to compgraph
#' 
#' @return An igraph object consisting of the subgraph of the social
#' network induced by those researchers assigned to the task
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' ccg = ccg.game(n, p)
#' assigned(ccg, 3)
#' 
#' 

assigned = function (ccg, wId = 3, ...) {
  rId = min(which(V(ccg$R)$g2.vId == wId))
  researchers = neighbors(ccg$R, rId)
  g.s = induced.subgraph(ccg$g1, researchers)
  return(g.s)
}
