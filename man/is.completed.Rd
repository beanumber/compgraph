\name{is.completed}
\alias{is.completed}
\title{is.completed}
\usage{
  is.completed(G, wId = 3, ...)
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
  Determine whether a task will be completed
}
\details{
  A recursive function that will traverse a tree and check
  whether tasks are completed.
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

