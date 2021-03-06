% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/menuFunctions.R
\name{menu.getTopics}
\alias{menu.getTopics}
\title{Function listing all available topics of a rrisk model, from which it offers the user to choose and export to another rrisk model}
\usage{
menu.getTopics(rriskModel,rriskModelFrom,menuLevel=1)
}
\arguments{
\item{rriskModel}{is the \code{rrisk} model where the parts are to be exported to}

\item{rriskModelFrom}{is the \code{rrisk} model from which the items are exported}

\item{menuLevel}{is a numeric value that indicates the load of the current menu}
}
\description{
This function offers the user to import external topics into the current model.
}
\details{
This function is not intended to be called directly but is internally called
during \code{rrisk} session.
}
\examples{
\donttest{emptyModel<-new("modelClass")
rriskModel<-init.Model1()
menu.getTopics(rriskModel=emptyModel,rriskModelFrom=rriskModel,menuLevel=1)}
}
\keyword{menu}

