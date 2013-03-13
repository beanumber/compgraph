#' @title is.ccg
#' 
#' @description Check to see whether a compgraph is a ccg
#' 
#' @details Checks to see whether a compgrah is a collaborative
#' composite graph
#' @author Ben Baumer
#' 
#' @param cg a compgraph object
#' 
#' @return TRUE or FALSE
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' cg = ccg.game(n, p)
#' is.ccg(cg)
#' 
#' 
is.ccg = function (cg) {
  if(!is.dag(cg$g2)) {
    cat("\nThe task graph is not a DAG")
    return(FALSE)
  }
  if(is.null(get.vertex.attribute(cg$g1, "expertise"))) {
    cat("\nThe researchers have no expertise")
    return(FALSE)
  }
  if(is.null(get.vertex.attribute(cg$g2, "difficulty"))) {
    cat("\nThe tasks have no difficulty")
    return(FALSE)
  }
  return(TRUE)
}