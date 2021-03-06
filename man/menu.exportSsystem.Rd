% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/menuFunctions.R
\name{menu.exportSsystem}
\alias{menu.exportSsystem}
\title{Function listing all available scoring systems and offering them to be exported}
\usage{
menu.exportSsystem(rriskSessionSSystems,menuLevel=1)
}
\arguments{
\item{rriskSessionSSystems}{is the slot scoringsystem from an instance of \code{rriskClass}}

\item{menuLevel}{is a numeric value that indicates the load of the current menu}
}
\description{
This function lists all available scoring systems and offers them to be exported.
}
\details{
This function is not intended to be called directly but is internally called
during \code{rrisk} session.
}
\examples{
\donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
menu.exportSsystem(rriskSession@scoringsystems,menuLevel=1)}
}
\keyword{menu}

