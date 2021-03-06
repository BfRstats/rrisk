% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initFunctions.R
\name{init.rriskSession}
\alias{init.rriskSession}
\title{Function to initialize the object rriskSession as an instance of 'rriskClass'}
\usage{
init.rriskSession(useDemoModels="all", demomode=FALSE)
}
\arguments{
\item{useDemoModels}{decides which demoModels should be used, \code{useDemoModels=c("no","all","Demo model 1", "Demo model 2", "Demo model 3")}}

\item{demomode}{is a boolean value indicating whether the model should be run under the demo mode, \code{demomode=c(TRUE,FALSE)}}
}
\value{
rriskSession
}
\description{
This initializes the object rriskSession as an instance of \code{\linkS4class{rriskClass}}.
}
\details{
This function is not intended to be called directly but is internally called
during \code{rrisk} session.
}
\examples{
\donttest{rriskSession<-init.rriskSession(useDemoModels="all")}
}
\keyword{init}

