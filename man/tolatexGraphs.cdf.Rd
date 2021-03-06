% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runFunctions.R
\name{tolatexGraphs.cdf}
\alias{tolatexGraphs.cdf}
\title{Function that creates LaTeX codes to import a cdf plot for the outcome items}
\usage{
tolatexGraphs.cdf(rriskModel,file.name)
}
\arguments{
\item{rriskModel}{is an instance of the class \code{modelClass}}

\item{file.name}{is the name of the TeX file into which the codes are to be written}
}
\description{
A function that creates LaTeX codes to import a cdf plot for the outcome items.
}
\details{
This function is not intended to be called directly but is internally called
during \code{rrisk} session.
}
\examples{
\donttest{rriskModel <- init.Model1()
rriskModel<-run(rriskModel)
tolatexGraphs.cdf(rriskModel,file.name="text.tex") }
}
\keyword{report}

