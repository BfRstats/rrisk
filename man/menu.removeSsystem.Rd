\name{menu.removeSsystem}
\alias{menu.removeSsystem}
\title{Function listing all available scoring systems and offering them to be removed}
\usage{
  menu.removeSsystem(rriskSessionSSystems,menuLevel=1)
}
\arguments{
  \item{rriskSessionSSystems}{are the slot scoringsystems
  from an instance of \code{rriskClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function lists all available scoring systems and
  offers them to be removed.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
rriskSession@scoringsystems<-menu.removeSsystem(rriskSession@scoringsystems,
menuLevel=1)}
}
\keyword{menu}

