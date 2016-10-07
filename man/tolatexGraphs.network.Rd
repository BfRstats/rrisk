\name{tolatexGraphs.network}
\alias{tolatexGraphs.network}
\title{Function creating LaTeX code for including model uncertainties graphs}
\usage{
  tolatexGraphs.network(rriskModel,file.name)
}
\arguments{
  \item{rriskModel}{an instance of the \code{modelClass}}

  \item{file.name}{character, name of the tex file where
  code for creating model uncertainties graphs should be
  written}
}
\description{
  Function that creates LaTeX code for including model
  uncertainties graphs
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatexGraphs.network(rriskModel,file.name="figcode.tex")}
}
\keyword{report}

