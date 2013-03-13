# Find the shortest itinerary through a series of vertices
itinerary = function (g, vIds) {
  if (length(vIds) < 2) {
    return(vIds)
  }
  out = NULL
  for (i in 1:(length(vIds)-1)) {
    path = unlist(as.vector(get.shortest.paths(g, from=vIds[i], to=vIds[i+1])))
    #    cat("\n",path)
    if (length(out) == 0) { out = c(out, path) } else { out = c(out, path[2:length(path)]) }
  }
  return(out)
}

#' @title citinerary
#' @description Find the the composite itinerary for a path in g1
#' @details Find the the composite itinerary for a path in g1
#' 
#' @param G A compgraph object
#' @param g1.path A vector of vertex Ids for a path in g1
#' 
#' @export
#' @examples
#' G = ercg(20, 0.5)
#' citinerary(G, c(1,3))

#  Given a path in G1, find the corresponding path in G2
citinerary = function (G, g1.path) {
  g2.path = G$R[g1.path]
  return(itinerary(G$g2, g2.path))
}