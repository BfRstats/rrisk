\name{tolatexGraphs.tree}
\alias{tolatexGraphs.tree}
\title{Function that creates LaTeX codes to include a regression tree plot}
\usage{
  tolatexGraphs.tree(rriskModel,file.name)
}
\arguments{
  \item{rriskModel}{is an instance of the class
  \code{modelClass}}

  \item{file.name}{is the name of the TeX file into which
  the codes are to be written}
}
\description{
  A function that creates LaTeX codes to include a
  regression tree plot.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel <- init.Model1()
rriskModel<-run(rriskModel)
tolatexGraphs.tree(rriskModel,file.name="text.tex") }
}
\keyword{report}

