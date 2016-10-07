\name{menu.loadDemoModels}
\alias{menu.loadDemoModels}
\title{Function generating the menu for loading demo models}
\usage{
  menu.loadDemoModels(rriskSession,demomode=FALSE,menuLevel=1)
}
\arguments{
  \item{rriskSession}{is an instance of \code{rriskClass}}

  \item{demomode}{is a boolean value indicating whether the
  model should be run under the demo mode,
  \code{demomode=c(TRUE,FALSE)}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\value{
  the new rriskSession object
}
\description{
  This function generates the menu for loading demo models.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
menu.loadDemoModels(rriskSession, menuLevel=1)}
}
\keyword{menu}

