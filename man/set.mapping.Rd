\name{set.mapping}
\alias{set.mapping}
\title{set.mapping}
\usage{
  set.mapping(G, type = "random", ...)
}
\arguments{
  \item{G}{a composite graph object}

  \item{type}{the type of mapping desired}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  TRUE or FALSE
}
\description{
  Set the relation R between the two graphs
}
\details{
  Fix the mapping between the two vertex sets
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

