\name{cstretch}
\alias{cstretch}
\title{cstretch}
\usage{
  cstretch(G, list.path)
}
\arguments{
  \item{G}{A compgraph object}

  \item{list.path}{A path in g1 represented as a list of
  vertex Ids}
}
\value{
  An integer representing the length of the composite
  stretch of the path in G. That is, the length of the
  corresponding path in g2 for the vertices in the path in
  g1 mapped according to R.
}
\description{
  Compute the composite stretch
}
\details{
  Compute the composite stretch of a path in g1
}
\examples{
n = 20
p = 1/2
g1 = erdos.renyi.game(n, p)
g2 = erdos.renyi.game(n, p)
cg = compgraph(g1, g2, name="myCompGraph")
}
\author{
  Ben Baumer
}

