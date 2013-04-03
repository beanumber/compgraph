\name{ccg.game}
\alias{ccg.game}
\title{ccg.game}
\usage{
  ccg.game(n1, p1, n2 = n1, r = 0.5, capacity = 10, ...)
}
\arguments{
  \item{n1}{Number of vertices in g1}

  \item{p1}{Probability of edge in g1}

  \item{n2}{Number of vertices in g2. Default value is n1.}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  A compgraph object
}
\description{
  Generate a random collaborative composite graph
}
\details{
  Generates a random collaborative composite graph then
  returns the resulting composite graph object
}
\examples{
n = 20
p = 1/2
G = ccg.game(n, p)
}
\author{
  Ben Baumer
}

