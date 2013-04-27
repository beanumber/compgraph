\name{add.assignments}
\alias{add.assignments}
\title{add.assignments}
\usage{
  add.assignments(ccg, alg = "random", blind = FALSE,
    n = 1)
}
\arguments{
  \item{ccg}{a ccgraph object}

  \item{alg}{the type of algorithm to use}

  \item{blind}{a logical indicating if the algorithm is
  blind to task completion}

  \item{n}{the number of assignments to add simultaneously}
}
\value{
  a ccgraph object
}
\description{
  Adds assignments of researchers to tasks
}
\details{
  Augment the mapping between the two vertex sets
}
\examples{
n = 20
p = 1/2
ccg = ccg.game(n, p)
plot(add.assignments(ccg))
}
\author{
  Ben Baumer
}

