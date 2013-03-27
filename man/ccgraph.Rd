\name{ccgraph}
\alias{ccgraph}
\alias{ccgraph.default}
\title{ccgraph}
\usage{
  ccgraph(cg, ctype = "density", ...)
}
\arguments{
  \item{cg}{A compgraph object}

  \item{...}{Currently ignored}
}
\value{
  A ccgraph object, or an error
}
\description{
  Instantiate a ccgraph object
}
\details{
  Class function for collaborative composite graphs
}
\examples{
# Create a ccgraph
ccg = ccg.game(10, 0.5, 2, r=0.5, name="myCCG")
ccg
}
\author{
  Ben Baumer
}

