#' @title ccnpk.lp
#' 
#' @description Compute the optimal solution to the CCNP-k problem using LP relaxation
#' 
#' @details A linear program
#' @author Ben Baumer
#' 
#' @param ccg a ccgraph object
#' @param ... Arguments to be passed to compgraph
#' 
#' @return The optimal fractional solution
#' 
#' @export
#' @examples
#' n = 20
#' p = 1/2
#' ccg = ccg.game(n, p)
#' ccnpk.lp(ccg)
#' 
#' 

ccnpk.lp = function(ccg, ...) {
  message("Computing an LP relaxation solution...")
  E = find.potential.assignments(ccg)
  if (nrow(E) == 0) {
    warning("You have already made all possible assignments!")
    return(ccg)
  }
  # Find the number of unsolved tasks
  k = length(unique(E$to))
  
  # number of potential assigments
  n = vcount(ccg$g1)
  m = vcount(ccg$g2)
  k = n * m
  
  xvec = expand.grid(workerId = 1:vcount(ccg$g1), taskId = 1:vcount(ccg$g2))
  workers = get.data.frame(ccg$g1, what="vertices")
  workers = transform(workers, id = 1:nrow(workers))
  tasks = get.data.frame(ccg$g2, what="vertices")
  tasks = transform(tasks, id = 1:nrow(tasks))
  x.df = merge(x=xvec, y=workers, by.x = "workerId", by.y = "id")
  x.df = merge(x=x.df, y=tasks, by.x="taskId", by.y="id")
  
  Amat.capacity = matrix(0, nrow=n, ncol = k)
  # capacity constraints
  for (i in 1:n) {
    for (j in 1:k) {
      if(x.df[j, "workerId"] == i) {
        Amat.capacity[i,j] = 1
      }
    }
  }
  # difficulty constraints
  Amat.completion = matrix(0, nrow=m, ncol = k)
  for (i in 1:m) {
    for (j in 1:k) {
      if(x.df[j, "taskId"] == i) {
        Amat.completion[i,j] = x.df[j, "expertise"]
      }
    }
  }
  # assignment constraints
  # and IP constraints
  Amat = rbind(Amat.capacity, Amat.completion, diag(k))
  bvec = c(workers$capacity, tasks$difficulty, rep(1, k))
  cvec = rep(1, nrow(x.df))
  direction = c(rep("<=", n), rep(">=", m), rep("<=", k))
  
  require(linprog)
  sol = solveLP(cvec, bvec, Amat, const.dir = direction)
  x.df$solution = sol$solution
  x.df = transform(x.df, applied = expertise * solution)
  return(sol$opt)
}
