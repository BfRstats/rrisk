% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/changeFunctions.R
\name{change.graphs}
\alias{change.graphs}
\title{Function for edditing model slot 'graphs'}
\usage{
change.graphs(graphs)
}
\arguments{
\item{graphs}{(model graphs) is an object of class \code{modelGraphsClass}}
}
\value{
This function returns edited model slot 'graphs'.
}
\description{
A change function for \code{\linkS4class{modelGraphsClass}}.
}
\details{
This function is not intended to be called directly but is internally called
during \code{rrisk} session.
}
\note{
This function requires some functions from the package \code{tcltk}.
}
\examples{
\donttest{model<-init.Model1()
model@graphs<-change.graphs(model@graphs)
model@graphs}
}
\keyword{edit}

