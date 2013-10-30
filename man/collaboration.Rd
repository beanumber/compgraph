\name{collaboration}
\alias{collaboration}
\title{collaboration}
\usage{
  collaboration(g, ctype = "social-density", a = 1, ...)
}
\arguments{
  \item{g}{an igraph object representing the social network
  of the researchers}

  \item{ctype}{the type of collaboration function to be
  applied}

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
cg = ccg.game(n, p)
collaboration(cg$g1)
}
\author{
  Ben Baumer
}

