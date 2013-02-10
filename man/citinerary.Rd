\name{citinerary}
\alias{citinerary}
\title{citinerary}
\usage{
  citinerary(G, g1.path)
}
\arguments{
  \item{G}{A compgraph object}

  \item{g1.path}{A vector of vertex Ids for a path in g1}
}
\description{
  Find the the composite itinerary for a path in g1
}
\details{
  Find the the composite itinerary for a path in g1
}
\examples{
G = ercg(20, 0.5)
citinerary(G, c(1,3))
}

