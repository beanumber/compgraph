\name{opt}
\alias{brute.force}
\alias{opt}
\title{opt}
\usage{
  opt(ccg, ...)
}
\arguments{
  \item{ccg}{a ccgraph object}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  An optimal solution, or NULL is the tasks are unsolvable
}
\description{
  Compute the optimal solution to the CCNP-k problem using
  brute force
}
\details{
  Tries all possible subsets of edges
}
\examples{
n = 20
p = 1/2
ccg = ccg.game(n, p)
opt(ccg)
}
\author{
  Ben Baumer
}

