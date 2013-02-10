p.k = function (n,k) { return(c(rep(0,k), choose((k-1):(n-2), k-1)) / choose(n-1, k)) }

getExpCBTimeStar = function (G, k) {
  n = vcount(G$g2)
  p_k = p.k(n,k)
  v_n = rep(1,n)/n
  return(t(v_n) %*% G$f.D2.geodesic %*% p_k)
}

getExpCBTimeStar.max = function (G) {
  getExpCBTimeStar(G, max(degree(G$T, mode = "out")))
}

getAllCStretch = function (G, g1.paths) {
#    Build a data frame with one row for each child node in G1
#   , recording the geodesic distance to that node in G1, 
#	  and the geodesic distance to that node in G2
#   (i.e. - the composite stretch)
#   V = as.vector(V(G$g1))
  g1.geodesic = sapply(g1.paths, length) - 1
  # compute all of the cstretchs for these paths
  cstretch = unlist(sapply(g1.paths, cstretch, G = G))
#  print(head(stretch, 10))
  df = data.frame(cbind("leaf.index" = 1:length(g1.paths)
        , g1.geodesic, cstretch, "height" = rep(max(g1.geodesic)
        , length(g1.paths))), "degree" = degree(G$T, mode="out"))
  return(df)
}

cbtimeFromRoot = function (G, root) {
  if (!is.connected(G$g2)) {
    cat("\nG2 is not connected")
    return(NA)
  }
  # Get a list of all paths from the root node to every other node
  all.paths.from.root = as.vector(get.shortest.paths(G$T, from=root))  
  # Make sure all paths have non-zero, finite length
  pathLengths = sapply(all.paths.from.root, length)
  if (sum(!is.finite(pathLengths)) > 0) { 
    cat("\nWarning: At least one path from that root is not of finite length"); 
  }
  stretch.df = getAllCStretch(G, all.paths.from.root)
#  print(head(stretch.df, 25))
  maxes = aggregate(stretch.df, by=list(stretch.df$g1.geodesic), max)
#  print(maxes)
  star.ub = sum(sapply(maxes$degree, getExpCBTimeStar, G = G))
  longest.cstretch.indices = which(stretch.df$cstretch == max(stretch.df$cstretch))
  return(cbind("root.index" = root, stretch.df[longest.cstretch.indices,][1,], "star.ub" = star.ub))
}

#' @title cbtime
#' @description Compute the broadcast time
#' @details Compute the broadcast time
#' 
#' @param G A compgraph object
#' 
#' @export
#' @examples
#' cg = compgraph(g1, g2)
#' cbtime(cg)

cbtime = function (G) {
  # First, make sure g1 is connected, otherwise abort
  if (!is.connected(g1)) { cat("g1 is not connected"); return("NA"); }
  # If g1 is a tree, then set the root vertex to 1
  # , otherwise, try all of the vertices
  if (vcount(G$g1) - 1 == ecount(G$g1)) { n = 1; } else { n = vcount(G$T); }
  # Find the broadcast time from all possible roots
  temp = data.frame(t(sapply(1:n, cbtimeFromRoot, G = G)))
  # Need this only if n > 1
  temp2 = data.frame(matrix(unlist(temp), ncol = 7))
  names(temp2) = names(temp[1,])
  return(temp2)
}
