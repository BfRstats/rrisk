\name{tolatexGraphs.convergence}
\alias{tolatexGraphs.convergence}
\title{Function that creates LaTeX codes to include a convergence plot}
\usage{
  tolatexGraphs.convergence(rriskModel,file.name)
}
\arguments{
  \item{rriskModel}{is an instance of the class
  \code{modelClass}}

  \item{file.name}{is the name of the TeX file into which
  the codes are to be written}
}
\description{
  A function that creates LaTeX codes to include a
  convergence plot.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel <- init.Model1()
rriskModel<-run(rriskModel)
tolatexGraphs.convergence(rriskModel,file.name="text.tex") }
}
\keyword{report}

