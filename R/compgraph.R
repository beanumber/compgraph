#' @title compgraph
#' 
#' @description Instantiate a compgraph object
#' 
#' @details Class function for compgraphs 
#' @author Ben Baumer
#' 
#' @param g1 An igraph object
#' @param g2 An igraph object
#' @param name The name of the resulting compgraph object
#' 
#' @return A compgraph object, which consists of a list containing
#' \item{name}{The name of the compgraph}
#' \item{g1}{The social igraph object}
#' \item{g2}{The communication igraph object}
#' \item{R}{A vector that provides a 1-1 mapping between the vertices in g1 and
#' the vertices in g2}
#' \item{D1.geodesic}{A matrix of the geodesic distances in g1}
#' \item{D2.geodesic}{A matrix of the geodesic distances in g2}
#' \item{T}{A minimum spanning tree for g1}
#' 
#' @export
#' @examples
#' # Create a compgraph
#' cg = compgraph (g1, g2, name="myCompGraph")
#' cg
#' 
#' 
#' 
compgraph = function (g1, g2, name = "G", ...) UseMethod("compgraph")

compgraph.default = function (g1, g2, name = "G", ...) {
  require(igraph)
  require(mosaic)
  # Make sure both graph arguments are actually graphs
  if (!is.igraph(g1)) { cat("g1 is not a valid igraph object"); break; }
  if (!is.igraph(g2)) { cat("g2 is not a valid igraph object"); break; }
  
  G = list("name" = name
           , "g1" = g1, "g2" = g2
           , "D1.geodesic" = shortest.paths(g1)
           , "D2.geodesic" = shortest.paths(g2)
           , "T" = minimum.spanning.tree(g1))
  class(G) = "compgraph"
#  args = list(...)
#  cat(str(args))
  G = set.mapping(G, ...)
  G$f.D2.geodesic = t(apply(G$D2.geodesic, 1, sort))
  #  G$g1.paths = getAllPaths(G, fromNodeIndex = g1.root.index)
  #  G$stretch = getAllPathStretch(G)
  #  G$ccd = max(G$stretch$cstretch)
  if (is.connected(g1) & is.connected(g2)) {
    #    G$cbtime = cbtime(G)
    #    G$cbtime.max = max(G$cbtime$cstretch)
  } else {
    cat("\nOne of the graphs is not connected, so the broadcast time is infinite")
    G$cbtime = Inf
    G$cbtime.max = Inf
  }
  #  G$mean.link.stretch = getMeanLinkStretch(G)
  return(G)
}




#' @title summary

summary.compgraph = function (G) {
  cat(paste("Composite Graph: ", G$name, "\n"))
  cat("G1: \n")
  summary(G$g1)
  if (is.connected(G$g1)) { cat("G1 is connected\n") } else { cat("G1 is NOT connected\n") } 
  cat("G2: \n")
  summary(G$g2)
  if (is.connected(G$g2)) { cat("G2 is connected\n") } else { cat("G2 is NOT connected\n") } 
  cat("R: \n")
  summary(G$R)
  cat("G1 Geodesic Distance Matrix: \n")
  print(summary(as.vector(G$D1.geodesic)))
  cat("G2 Geodesic Distance Matrix: \n")
  print(summary(as.vector(G$D2.geodesic)))
  #  cat("Path Stretch Summary: \n")
  #  print(summary(G$stretch))
  #  cat("Mean Link Stretch: ")
  #  print(G$mean.link.stretch)
  #  cat("\nCCD: ")
  #  print(G$ccd)
  cat("\nBroadcast Time: ")
  print(G$cbtime.max)
}

# stats = function (G) {
#   return(c("g2.euclidean.mean" = mean(G$g2$D.euclidean)
#            , "g2.geodesic.mean" = mean(G$D2.geodesic)
# #  , "mean.link.stretch" = G$mean.link.stretch
#   , "g2.edges" = length(E(G$g2)), "cbtime" = G$cbtime.max))
# }
# 
# shuffle = function (G) {
#   G$R = sample.int(vcount(G$g1), size=vcount(G$g1)) - 1
#   G$cbtime = cbtime(G)
#   G$cbtime.max = max(G$cbtime$cstretch)
#   return(G)
# }
# 
# #getMeanLinkStretch = function (G) {
# #  return(mean(G$stretch$cstretch / G$stretch$g1.geodesic, na.rm = TRUE))
# #}
# 
# 
# #getAllPaths = function (G, fromNodeIndex = 0, graph = "g1") {
# #  if(graph == "g2") { g = G$g2 } else { g = G$g1 }
# #  return(as.vector(get.shortest.paths(g, from=fromNodeIndex)))  	# A list of all paths from the root node to every other node
# #}
# 
# 

# 
# # 

# 

# 
# 
# passes.thru = function (path, v) {
#   return(v %in% path)
# }
# 
# 
# get.last = function (x) {
#   return(x[length(x)])
# }
# 
# 
# # Composite Betweenness
# cbetweenness.ve = function (G, g2.v = 446, g1.from=1, g1.to=NULL) {
# #  cat(paste("\nComputing betweenness for", g2.v))
#   if (length(g1.to) < 1) {
#     g1.to = neighbors(G$g1, g1.from)
#     # Remove the node that you're checking
#     g1.to = g1.to[!g1.to == g2.v]
#   }
#   # Make sure that g2.v is in G2
#   if (!length(V(G$g2)[g2.v]) > 0) {
#     cat("\nInvalid G2 vertex Id")
#     return(NULL)
#   }
#   # Make sure that both G1 vertices exist
#   if (!length(V(G$g1)[c(g1.from, g1.to)]) > 0) {
#     cat("\nInvalid G1 vertex Id")
#     return(NULL)
#   }
#   # Don't check for complete graphs for some reason
#   if (ecount(G$g1) != choose(vcount(G$g1),2)) {
#     # Make sure that the edge in G1 exists
#     if (!length(G$g1[g1.from, g1.to]) > 0) {
#   #    cat("\nNone of those edges exist in G1")
#       return(NULL)
#     }
#   }
#   sId = G$R[g1.from]
#   tIds = G$R[g1.to]
#   st = get.all.shortest.paths(G$g2, from = sId, to = tIds)
#   # why aren't these unique?
# #  st.paths = unique(st$res)
#   df = data.frame(src = rep(g1.from, length(tIds)), dest = tIds)
#   pass.thru = st$res[which(sapply(st$res, passes.thru, v=g2.v))]
#   pass.thru.dests = as.numeric(lapply(pass.thru, get.last))
#   df$sigma.st = as.numeric(table(c(tIds, pass.thru.dests)) - rep(1,length(tIds)))
#   df$n = st$nrgeo[tIds]
# #  cat(paste("\nFor v=", g1.from, "and w=", g1.to, "there were", length(st$res), "shortest paths"))
#   return(df)
# }
# 
# cbetweenness.v.fast = function (G, g2.v) {
#   V = 1:vcount(G$g1)
#   # Remove the node that you're checking
#   V = V[-g2.v]
#   mat = do.call("rbind", t(lapply(V, cbetweenness.ve, G=G, g2.v=g2.v)))
#   # Have to divide by two since we double-counted all the edges
#   return(sum(mat$sigma.st / mat$n) / 2)
# }
# 
# cbetweenness.v.slow = function (G, g2.v) {
#   E = get.edgelist(G$g1, names=FALSE)
# #  cat(paste("\nFound", nrow(E), "edges in G1"))
#   # Should take |E1| * O(cbetweennessEdge)
#   cbtw = mapply(cbetweenness.ve, g1.from=E[,1], g1.to=E[,2], MoreArgs=list(G=G, g2.v=g2.v))
#   return(sum(cbtw))
# }
# 
# cbetweenness.benchmark = function (G) {
#   vIds = sample.int(vcount(G$g1), 10)
#   t = system.time(sapply(vIds, cbetweenness.v.fast, G=G))["elapsed"]
#   cat(paste("\nIt's going to take about", vcount(G$g1) * t / (60 * length(vIds)), "minutes"))
# }
# 
# cbetweenness = function (G, n=vcount(G$g1)) {
#   sapply(1:n, cbetweenness.v.fast, G=G)
# }
# 
# 
# 
# ##############################################################################
# #
# #	Spanning Trees and Broadcast Time
# #
# ##############################################################################
# # 

# 
# getMeanEccentricity = function (G) {
#   mean(apply(G$D2.geodesic, 1, max))
# }
# 
# ##############################################################################
# #
# #	Build an (R)GG over a vertex set
# #
# #############################################################################
# 
# setParentCoordinates = function (V) {
#   #  How far is each node away from his parent?
#   lkup = merge(x=V, y=V, by.x="parentId", by.y="id", all.x=TRUE)
#   head(lkup)
#   idx = order(lkup$id)
#   V$parent.x = lkup$x.y[idx]
#   V$parent.y = lkup$y.y[idx]
#   # V$parent.x = V$x[V$parentId + 1]
#   # V$parent.y = V$y[V$parentId + 1]
#   V$d = sqrt((V$x - V$parent.x)^2 + (V$y - V$parent.y)^2)
#   return(V)
# }
# 
# getParameterA = function (V) {
#   #  Fit a model to the deployment, where r(i) = a(h-i)^2
#   h = length(unique(V$i))
#   X.data = (h - V$i)^2
#   plot(X.data, V$d)
#   fm = lm(V$d ~ 0 + X.data)
#   summary(fm)
#   a = fm$coefficients[1]
#   return(a)
# }
# 
# getParameterB = function (V) {
#   a = getParameterA(V)
#   #  Get some information about the actual deployment
#   radii.mean = tapply(V$d, V$i, mean)
#   radii.sd = tapply(V$d, V$i, sd)
#   plot(sort(radii.mean, decreasing = TRUE))
#   curve(a * (h - x)^2, add = TRUE)
# 
#   #	We now have a model for how far apart each child should be from his parent, but what is the spread?
#   plot(radii.mean, radii.sd)
#   fm = lm(radii.sd ~ 0 + radii.mean)
#   summary(fm)
#   b = fm$coefficients[1]
#   #	The SD seems to vary roughly linearly with the mean, as a constant coefficient of variation model is reasonable
#   curve(b * x, add = TRUE)
#   return(b)
# }
# 
# setNumSiblings = function (g) {
#   V = data.frame(id = V(g)$name, parentId = V(g)$parentId)
#   V$numChildren = neighborhood.size(g, 1, mode="out") - 1
#   # Create a lookup table for the parentIds
#   lkup = merge(x=V, y=V, by.x="parentId", by.y="id", all.x=TRUE, sort=FALSE)
#   idx = order(as.numeric(as.character(lkup$id)))
#   V$numSiblings = as.numeric(as.character(lkup$numChildren.y))[idx]
#   V$numSiblings = ifelse(is.na(V$numSiblings), 1, V$numSiblings)
#   V(g)$numSiblings = V$numSiblings
#   return(g)
# }
# 
# labelChildren = function (child.list) {
#   child.vec = unlist(child.list)
#   n = length(child.vec)
#   if (n == 1) {
#     return(NULL);
#   }
#   # leave off the node itself
#   return(data.frame(id = child.vec[2:n], k = 1:(n-1)))
# }
# 
# labelAllChildren = function (g) {
#   children.list = neighborhood(g, 1, mode="out")
#   
#   # k is the sibling index within a family
#   k.list = sapply(children.list, labelChildren)
#   k.mat = do.call("rbind", k.list)
#   V(g)$k = c(1, k.mat[order(k.mat$id),2])
#   return(g)
# }
# 
# updateLocation = function (tree, vId) {
# #  cat(paste("\nvId=", vId))
#   if (!is.connected(tree) & vcount(tree) == ecount(tree) + 1) {
#     cat("\nThis is not a tree!")
#   }
#   parentId = neighbors(tree, vId, mode="in")
#   if (length(parentId) > 1) {
#     cat(paste("\nNode", vId, "has more than one parent! Aborting..."))
#     return(tree)
#   }
#   if (length(parentId) == 0) {
# #    cat(paste("\nNode", vId, "is the root node!"))
#   } else {
#     parent.x = V(tree)$x[parentId]
#     parent.y = V(tree)$y[parentId]
#     V(tree)$x[vId] = parent.x + V(tree)$offset.x[vId]
#     V(tree)$y[vId] = parent.y + V(tree)$offset.y[vId]
#   }
#   
#   children = neighbors(tree, vId, mode="out")
#   if (length(children) < 1) {
#     return(tree)
#   } else {
#     for (childId in children) {
#       tree = updateLocation(tree, childId)
#     }
#     return(tree)
#   }
# }
# 
# random.tree.deploy = function (V, tree) {	
#   cat("\nRandomly deploying based on the tree you gave me...")
#   # General a random complex variable
#   V$spin = runif(nrow(V))
#   # Spin the wheel uniformly at random
#   V$w = complex(modulus = V$r, argument = (2*pi * (V$k + V$spin) / V$numSiblings))	
#   #  Draw the random variables for the (x,y) offsets
#   V(tree)$offset.x = rnorm(nrow(V), mean = Re(V$w), sd = V$sd)
#   V(tree)$offset.y = rnorm(nrow(V), mean = Im(V$w), sd = V$sd)
# #  plot(y ~ x, data = offsets)
#   #  Recursively set the locations based on the parent's RANDOM location!
# 
#   rootId = which(V$i == 0)
#   tree = updateLocation(tree, rootId)
#     
# #  V$rand.d = sqrt((V$x - V$x[V$parentId + 1])^2 + (V$rand.y - V$rand.y[V$parentId + 1])^2)
#   return(as.matrix(cbind(V(tree)$x, V(tree)$y)))
# }
# 
# 
# getGGEdgeList = function (V, rgg=FALSE, tree=NULL, c = 1, scale = 1) {
#   # Note that V *must* have an "id" column!
#   V = as.data.frame(V)
#   n = nrow(V)
# 
#   #	By default, use the given (x,y) locations
#   X = as.matrix(cbind(V$x, V$y))
#   
#   if (rgg) {
#     #	Uniformly at random generate the (x,y) locations
#     X = matrix(runif(2 * nrow(V)), ncol = 2) * scale
#   }
#   
#   if (!is.null(tree)) {
#     if (is.igraph(tree) & is.connected(tree) & ecount(tree) == vcount(tree) - 1) {
#       #	Do the random tree deployment
#       X = random.tree.deploy(V, tree)
#     }
#   }
# 
#   xlim = range(X[,1], na.rm = TRUE)
#   ylim = range(X[,2], na.rm = TRUE)
#   r = sqrt(c * (log(n)/n) * (xlim[2] - xlim[1]) * (ylim[2] - ylim[1]) )
# 
#   D.euclidean = as.matrix(dist(X))
#   # summary(D)
#   E.ids = which(D.euclidean > 0 & D.euclidean < r, arr.ind = TRUE)
#   E = cbind("src" = V$id[E.ids[,1]], "dest" = V$id[E.ids[,2]])
#   return(list("E" = E, "r" = r, "layout" = X, "D.euclidean" = D.euclidean))
# }
# 
# graph.GG = function (V, name = "G", rgg=FALSE, tree=NULL, c = 1, ...) {
#   edgeList = getGGEdgeList(V, rgg, tree, c, ...)
#   g = graph.data.frame(edgeList$E, directed = FALSE, vertices = V)
#   g = set.graph.attribute(g, "name", name)
#   g$r = edgeList$r
#   g$layout = edgeList$layout
#   g$D.euclidean = edgeList$D.euclidean
#   return(g)
# }
# 
# 
# 

# ######################################################################################
# #
# #  Testing
# #
# ########################################################################################
# 
# 
# trial.rgg = function (g1, V, prefix="us", save.plot=FALSE) {
#   xlim = range(V$x, na.rm = TRUE)
#   ylim = range(V$y, na.rm = TRUE)
#   area = (xlim[2] - xlim[1]) * (ylim[2] - ylim[1])
#   g2 = graph.GG(V, rgg=TRUE, name = "Random Geometric Graph", scale = sqrt(area))
#   G = compgraph(g1, g2, "RGG")
#   if (save.plot) {
#     pdf(paste("gfx/", prefix, "_rgg_", sample.int(100,1), ".pdf", sep=""), width=8, height=8, fonts=c("serif", "Palatino", "Helvetica"))
#     plot.g2(G)
#     dev.off()
#   }
#   return(stats(G))
# }
# 
# trial.tree = function (g1, V, c=1, prefix="us", save.plot=FALSE) {
#   xlim = range(V$x, na.rm = TRUE)
#   ylim = range(V$y, na.rm = TRUE)
#   area = (xlim[2] - xlim[1]) * (ylim[2] - ylim[1])
#   g2 = graph.GG(V, tree=g1, name = "Random Correlated Geometric Graph", scale = sqrt(area))
#   G = compgraph(g1, g2, "Correlated Deployment")
#   if (save.plot) {
#     pdf(paste("gfx/", prefix, "_tree_", sample.int(100,1), ".pdf", sep=""), width=8, height=8, fonts=c("serif", "Palatino", "Helvetica"))
#     plot.g2(G)
#     dev.off()
#   }
#   return(stats(G))
# }
# 
# trial.shuffle = function (G) {
#   G = shuffle(G)
#   #  pdf(paste(plot_dir, "random_permutation_", sample.int(100,1), ".pdf", sep=""), width=8, height=8, fonts=c("serif", "Palatino", "Helvetica"))
#   #  plot.g2(G)
#   #  dev.off()
#   return(stats(G))
# }
# 
# test.shuffle = function (G, numTrials=100, benchmark=FALSE, prefix="us") {
#   if (benchmark) {
#     numTrials = 10
#     return(system.time(data.frame(t(replicate(numTrials, trial.shuffle(G.given))))))
#   }
#   df = data.frame(t(replicate(numTrials, trial.shuffle(G.given))))
#   write.table(df, paste(prefix, "_test_shuffle_", numTrials, ".csv", sep=""))
#   return(df)
# }
# 
# test.rgg = function (g1, V, numTrials=100, benchmark=FALSE, prefix="us") {
#   if (benchmark) {
#     numTrials = 10
#     return(system.time(data.frame(t(replicate(numTrials, trial.rgg(g1, V, prefix=prefix))))))
#   }
#   df = data.frame(t(replicate(numTrials, trial.rgg(g1, V, prefix=prefix))))
#   write.table(df, paste(prefix, "_test_rgg_", numTrials, ".csv", sep=""))
#   return(df)
# }
# 
# test.tree = function (g1, V, c=1, numTrials=100, benchmark=FALSE, prefix="us") {
#   if (benchmark) {
#     numTrials = 10
#     return(system.time(data.frame(t(replicate(numTrials, trial.tree(g1, V, c=c, prefix=prefix))))))
#   }
#   df = data.frame(t(replicate(numTrials, trial.tree(g1, V, c=c, prefix=prefix))))
#   write.table(df, paste(prefix, "_test_tree_", numTrials, ".csv", sep=""))
#   return(df)
# }
# 
# test.benchmark = function (G, V, numTrials=20000) {
#   time.shuffle = test.shuffle(G, benchmark=TRUE)
#   time.rgg = test.rgg(G$g1, V, benchmark=TRUE)
#   time.tree = test.tree(G$g1, V, benchmark=TRUE)
#   time = rbind(time.shuffle, time.rgg, time.tree) * numTrials / (10 * 60)
#   return(sum(time[,"elapsed"]))
# }
# 
# # time = rbind(time.shuffle, time.rgg) * numTrials / (testTrials * 60)
# # sum(time[,"elapsed"])
# #  About 1 minute for 100 trials of each, so it should take around n/100 minutes
# 
# 
# ######################################################################################
# #
# #	Function to export TikZ code
# #
# ########################################################################################
# 
# 
# 
# tikz <- function (graph, layout) {
#   ## Here we get the matrix layout
#   if (class(layout) == "function")
#     layout <- layout(graph)
# 
#   layout <- layout / max(abs(layout))
#   clos <- closeness(graph)
# 
#   ##TikZ initialisation and default options (see pgf/TikZ manual)
#   cat("\\tikzset{\n")
#   cat("\tnode/.style={circle,inner sep=1mm,minimum size=0.8cm,draw,very thick,black,fill=red!20,text=black},\n")
#   cat("\tnondirectional/.style={very thick,black},\n")
#   cat("\tunidirectional/.style={nondirectional,shorten >=2pt,-stealth},\n")
#   cat("\tbidirectional/.style={unidirectional,bend right=10}\n")
#   cat("}\n")
#   cat("\n")
# 
#   ##Size
#   cat("\\begin{tikzpicture}[scale=5]\n")
# 
#   for (vertex in V(graph)) {
#     label <- V(graph)[vertex]$label
#     if (is.null(label))
#       label <- ""
# 
#   ##drawing vertices
#     cat (sprintf ("\t\\node [node] (v%d) at (%f, %f)\t{%s};\n", vertex, layout[vertex+1,1], layout[vertex+1,2], label))
#   }
#   cat("\n")
# 
#   adj = get.adjacency(graph)
#   bidirectional = adj & t(adj)
# 
#   if (!is.directed(graph)) ##undirected case
#       for (line in 1:nrow(adj)) {
#       for (col in line:ncol(adj)) {
#            if (adj[line,col]&col>line) {
#             cat (sprintf ("\t\\path [nondirectional] (v%d) edge (v%d);\n", line-1, col-1)) ##edges drawing
#           }
#         }
#       }
#   else ##directed case
#       for (line in 1:nrow(adj)) {
#       for (col in 1:ncol(adj)) {
#           if (bidirectional[line,col]&line > col)
#             cat (sprintf ("\t\\path [bidirectional] (v%d) edge (v%d);\n", line-1, col-1),
#             sprintf ("\t\\path [bidirectional] (v%d) edge (v%d);\n", col-1, line-1)) ##edges drawing
#         else if (!bidirectional[line,col]&adj[line,col]) 
#             cat (sprintf ("\t\\path [unidirectional] (v%d) edge (v%d);\n", line-1, col-1)) ##edges drawing
#       }
#     }
# 
#   cat("\\end{tikzpicture}\n")
# }
