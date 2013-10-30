\name{ccnpk.lp}
\alias{ccnpk.lp}
\title{ccnpk.lp}
\usage{
  ccnpk.lp(ccg, ...)
}
\arguments{
  \item{ccg}{a ccgraph object}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  The optimal fractional solution
}
\description{
  Compute the optimal solution to the CCNP-k problem using
  LP relaxation
}
\details{
  A linear program
}
\examples{
n = 20
p = 1/2
ccg = ccg.game(n, p)
ccnpk.lp(ccg)
}
\author{
  Ben Baumer
}

