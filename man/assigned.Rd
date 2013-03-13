\name{assigned}
\alias{assigned}
\title{assigned}
\usage{
  assigned(ccg, wId = 1, ...)
}
\arguments{
  \item{ccg}{collaborative composite graph}

  \item{w}{vertex id corresponding to the task}

  \item{...}{Arguments to be passed to compgraph}
}
\value{
  An igraph object consisting of the subgraph of the social
  network induced by those researchers assigned to the task
}
\description{
  Figure out who is assigned to a particular task
}
\details{
  Figure out who is assigned to a particular task
}
\examples{
n = 20
p = 1/2
ccg = ccg.game(n, p)
assigned(ccg, 3)
}
\author{
  Ben Baumer
}

