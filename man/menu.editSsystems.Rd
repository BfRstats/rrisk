\name{menu.editSsystems}
\alias{menu.editSsystems}
\title{Function listing all available scoring systems and offering them to be edited}
\usage{
  menu.editSsystems(rriskSessionSSystems,menuLevel=1)
}
\arguments{
  \item{rriskSessionSSystems}{is the slot scoringsystem
  from an instance of \code{rriskClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function lists all available scoring systems and
  offers them to be edited.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSessionSSystems<-init.rriskSession(useDemoModels = "all")@scoringsystems
menu.editSsystems(rriskSessionSSystems,menuLevel=1)}
}
\keyword{menu}

