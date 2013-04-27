\name{ccnpk}
\alias{ccnpk}
\title{ccnp-k}
\usage{
  ccnpk(ccg, alg = "greedy", ...)
}
\arguments{
  \item{ccg}{a ccgraph object}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  A solution, or NULL is the tasks are unsolvable
}
\description{
  Compute solutions to the CCNP-k problem
}
\details{
  Calls various algorithms
}
\examples{
n = 20
p = 1/2
ccg = ccg.game(n, p, capacity=2)
ccnpk(ccg)
}
\author{
  Ben Baumer
}

