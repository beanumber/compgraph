\name{assign.random}
\alias{assign.random}
\title{assign.random}
\usage{
  assign.random(ccg, ...)
}
\arguments{
  \item{ccg}{a composite graph object}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  A ccg with all tasks complete, or FALSE
}
\description{
  Randomly assign researchers to tasks until all tasks are
  complete
}
\details{
  Randomly assign researchers to tasks until all tasks are
  complete
}
\examples{
n = 20
p = 1/2
ccg = ccg.game(n, p, n2=2)
is.completed(ccg)
ccg = assign.random(ccg)
is.completed(ccg)
}
\author{
  Ben Baumer
}

