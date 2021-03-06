% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpFunctions.R
\name{createEmptyModel}
\alias{createEmptyModel}
\title{Function that creates an empty model}
\usage{
createEmptyModel(rriskSessionModels)
}
\arguments{
\item{rriskSessionModels}{the slot models of an instance of the \code{rriskSession} object}
}
\description{
Function that creates an empty model
}
\details{
This function is not intended to be called directly but is internally called
during \code{rrisk} session.
}
\examples{
\donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
createEmptyModel(rriskSession@models)}
}
\keyword{model}

