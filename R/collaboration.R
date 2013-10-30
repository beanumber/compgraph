#' @title collaboration
#' 
#' @description Computes the collaboration score for a subgraph of the social network
#' 
#' @details Computes the collaboration score for a subgraph of the social network
#' @author Ben Baumer
#' 
#' @param g an igraph object representing the social network of the researchers
#' @param ctype the type of collaboration function to be applied
#' @param ... Arguments to be passed to compgraph
#' 
#' @return TRUE or FALSE
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' cg = ccg.game(n, p)
#' collaboration(cg$g1)
#' 
#' 
collaboration = function (g, ctype = "social-density", a = 1, ...) {
  if (vcount(g) == 0) {
    return(0)
  } else if (vcount(g) == 1) {
    return(V(g)$expertise)
  } else {
    c = switch(ctype
        , "multiplicative" = sum(V(g)$expertise * (degree(g)/2 + 1))
        , "social-density" = sum(V(g)$expertise) * (a + ecount(g) / choose(vcount(g), 2))
        , "additive" = sum(V(g)$expertise)
        , sum(V(g)$expertise) 
    )
    return(c)
  }
}