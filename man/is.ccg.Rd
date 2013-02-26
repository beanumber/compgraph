\name{is.ccg}
\alias{is.ccg}
\title{is.ccg}
\usage{
  is.ccg(cg)
}
\arguments{
  \item{cg}{a compgraph object}
}
\value{
  TRUE or FALSE
}
\description{
  Check to see whether a compgraph is a ccg
}
\details{
  Checks to see whether a compgrah is a collaborative
  composite graph
}
\examples{
n = 20
p = 1/2
cg = ccg.game(n, p)
is.ccg(cg)
}
\author{
  Ben Baumer
}

