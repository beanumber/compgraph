\name{ccnpk.greedy}
\alias{ccnpk.greedy}
\title{ccnpk.greedy}
\usage{
  ccnpk.greedy(ccg, ...)
}
\arguments{
  \item{ccg}{a ccgraph object}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  A greedy solution, or NULL is the tasks are unsolvable
}
\description{
  Compute a greedy solution to the CCNP-k problem
}
\details{
  Tries complete all tasks
}
\examples{
n = 20
p = 1/2
ccg = ccg.game(n, p)
ccnpk.greedy(ccg)
}
\author{
  Ben Baumer
}

