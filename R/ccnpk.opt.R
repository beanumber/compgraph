#' @title ccnpk.opt
#' @aliases brute.force
#' 
#' @description Compute the optimal solution to the CCNP-k problem using brute force
#' 
#' @details Tries all possible subsets of edges
#' @author Ben Baumer
#' 
#' @param ccg a ccgraph object
#' @param ... Arguments to be passed to compgraph
#' 
#' @return An optimal solution, or NULL is the tasks are unsolvable
#' 
#' @export
#' @export brute.force
#' @examples
#' n = 20
#' p = 1/2
#' ccg = ccg.game(n, p)
#' ccnpk.opt(ccg)
#' 
#' 

ccnpk.opt = function(ccg, ...) {
  message("Computing the optimal solution...")
  E = find.potential.assignments(ccg)
  if (nrow(E) == 0) {
    warning("You have already made all possible assignments!")
    return(ccg)
  }
  # Find the number of unsolved tasks
  k = length(unique(E$to))
  
  # Todo: optimize this loop
  for(i in k:nrow(E)) {
    win = brute.force(ccg, E, i)
    if (!is.null(win)) {
      #      cat(win)
      return(win)
    }
  }
}


brute.force = function(ccg, E, k = 1) {
  message(paste("Computing all", choose(nrow(E), k) , "subsets of size", k, "..."))
  x = combn(nrow(E),k)
  
  # which tasks remain unsolved?
  solvable = sapply(1:vcount(ccg$g2), is.solvable, ccg=ccg)
  unsolved = which(!solvable)
  
  # include only those possibilities which contain at least one edge to each unsolved task
  is.possible = apply(x, 2, is.feasible.solution, E=E, ccg=ccg, unsolved=unsolved)
  if (sum(is.possible) < 1) {
    warning("No feasible solutions!")
    return(NULL)
  }
  y = as.matrix(x[,is.possible])
  message(paste("...but only", ncol(y) , "are feasible solutions"))
  
  for(j in 1:ncol(y)) {
    queue = E[y[,j],]
    # Add the edges
    ccg2 = ccg
    ccg2$R = ccg2$R + edges(as.vector(t(queue[,1:2])))
    # Make sure that you haven't exceeded the capacity of the researchers!
#    if (sum(degree(ccg2$R)[1:vcount(ccg$g1)] > V(ccg$g1)$capacity)) {
#      warning("This selection exceeds capacity...discarding...")
#    } else {
      if(is.completed(ccg2)) {
        cat("\n...found a solution!")
        #      cat(queue)
        return(queue);
      }     
      
#    }
    #    cat(nrow(queue))
    if(log(j, 10) == round(log(j, 10))) {
      cat(paste("\n", j))
    }
    
  }
  return(NULL)
}

is.feasible.solution = function (idxs, E, ccg, unsolved) {
  q = E[idxs,]
  # consider only solutions that do not exceed capacity
  R.new = count(c(1:vcount(ccg$g1), q$from))
  R.new = transform(R.new, degree = freq - 1)
  if (sum(R.new$degree > V(ccg$g1)$capacity)) {
    return(FALSE)
  } else {
    return(length(intersect(q$to - vcount(ccg$g1), unsolved)) == length(unsolved))
  }
}
