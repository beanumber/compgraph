#' @title ccgraph
#' @aliases ccgraph.default
#' 
#' @description Instantiate a ccgraph object
#' 
#' @details Class function for collaborative composite graphs 
#' @author Ben Baumer
#' 
#' @param cg A compgraph object
#' @param ... Currently ignored
#' 
#' @return A ccgraph object, or an error
#' 
#' @export
#' @examples
#' # Create a ccgraph
#' ccg = ccg.game(10, 0.5, 2, r=0.5, name="myCCG")
#' ccg
#' 
#' 
#' 
ccgraph = function (cg, ctype = "density", ...) UseMethod("ccgraph")

ccgraph.default = function (cg, ctype = "density", ...) {
  args = list(...)
  cat(str(args))
  if (is.ccg(cg)) {
    class(cg) = c("ccgraph", class(cg))
    cg$ctype = ctype
    return(cg)    
  } else {
    stop("This is not a ccgraph")
  }
}

