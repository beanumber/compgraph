\name{is.solvable}
\alias{is.solvable}
\title{is.solvable}
\usage{
  is.solvable(ccg, wId = 3, ...)
}
\arguments{
  \item{ccg}{collaborative composite graph}

  \item{w}{vertex id corresponding to the task}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  TRUE or FALSE
}
\description{
  Determines whether a task is solvable
}
\details{
  Determines whether a task is solvable
}
\examples{
n = 20
p = 1/2
G = ccg.game(n, p)
is.solvable(G, 3)
}
\author{
  Ben Baumer
}

