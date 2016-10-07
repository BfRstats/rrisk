\name{menu.handleSsystem}
\alias{menu.handleSsystem}
\title{Function generating user menu for maintaining items}
\usage{
  menu.handleSsystem(rriskModel,rriskSession,menuLevel=1)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{modelClass}}

  \item{rriskSession}{is an instance of the
  \code{rriskClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function generates the user menu for maintaining
  scoring systems.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
menu.handleSsystem(rriskSession@models[[1]],rriskSession,menuLevel=1)}
}
\keyword{menu}

