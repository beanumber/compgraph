\name{ercg}
\alias{ercg}
\title{ercg}
\usage{
  ercg(n1, p1, n2 = n1, p2 = p1, ...)
}
\arguments{
  \item{n1}{Number of vertices in g1}

  \item{p1}{Probability of edge in g1}

  \item{n2}{Number of vertices in g2. Default value is n1}

  \item{p2}{Probability of edge in g2. Default value is p1}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  A compgraph object
}
\description{
  Generate an Erdos-Renyi random composite graph
}
\details{
  Generates two Erdos-Renyi graphs, and then returns the
  resulting composite graph object
}
\examples{
n = 20
p = 1/2
G = ercg(n, p)
}
\author{
  Ben Baumer
}

