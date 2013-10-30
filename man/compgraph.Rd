\name{compgraph}
\alias{compgraph}
\title{compgraph}
\arguments{
  \item{g1}{An igraph object}

  \item{g2}{An igraph object}

  \item{name}{The name of the resulting compgraph object}
}
\value{
  A compgraph object, which consists of a list containing
  \item{name}{The name of the compgraph} \item{g1}{The
  social igraph object} \item{g2}{The communication igraph
  object} \item{R}{A vector that provides a 1-1 mapping
  between the vertices in g1 and the vertices in g2}
  \item{D1.geodesic}{A matrix of the geodesic distances in
  g1} \item{D2.geodesic}{A matrix of the geodesic distances
  in g2} \item{T}{A minimum spanning tree for g1}
}
\description{
  Instantiate a compgraph object
}
\details{
  Class function for compgraphs
}
\examples{
# Create a compgraph
cg = compgraph (g1, g2, name="myCompGraph")
cg
}
\author{
  Ben Baumer
}

