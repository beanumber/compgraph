\name{add.random.assignments}
\alias{add.greedy.assignments}
\alias{add.random.assignments}
\title{add.random.assignments}
\usage{
  add.random.assignments(ccg, n = 1)
}
\arguments{
  \item{ccg}{a ccgraph object}

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
plot(add.random.assignments(ccg))
}
\author{
  Ben Baumer
}

