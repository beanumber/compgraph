#' @title cstretch
#' 
#' @description Compute the composite stretch
#' 
#' @details Compute the composite stretch of a path in g1 
#' @author Ben Baumer
#' 
#' @param G A compgraph object
#' @param list.path A path in g1 represented as a list of vertex Ids
#' 
#' @return An integer representing the length of the composite stretch of the
#' path in G. That is, the length of the corresponding path in g2 for the vertices
#' in the path in g1 mapped according to R.
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' g1 = erdos.renyi.game(n, p)
#' g2 = erdos.renyi.game(n, p)
#' cg = compgraph(g1, g2, name="myCompGraph")
#' 
#' 
cstretch = function (G, list.path) {
  if(length(list.path) <= 1) { 
    return(0) 
  } else {
    path = unlist(list.path)
    vIds = G$R[path[1:(length(path)-1)]]
    wIds = G$R[path[2:length(path)]]
    return(sum(diag(G$D2.geodesic[vIds, wIds])))  
  }
}