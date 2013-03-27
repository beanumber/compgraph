\name{collaboration}
\alias{collaboration}
\title{collaboration}
\usage{
  collaboration(g, ctype = "social-density", ...)
}
\arguments{
  \item{g}{an igraph object representing the social network
  of the researchers}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  TRUE or FALSE
}
\description{
  Computes the collaboration score for a subgraph of the
  social network
}
\details{
  Computes the collaboration score for a subgraph of the
  social network
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

