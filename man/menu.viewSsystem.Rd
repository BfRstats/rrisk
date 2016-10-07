\name{menu.viewSsystem}
\alias{menu.viewSsystem}
\title{Function generating the user menu for viewing all listed scoring systems available}
\usage{
  menu.viewSsystem(rriskSessionSSystems,menuLevel=1)
}
\arguments{
  \item{rriskSessionSSystems}{are the slot scoringsystems
  from an instance of \code{rriskClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function generates the user menu for viewing all
  listed scoring systems available.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
menu.viewSsystem(rriskSession@scoringsystems,menuLevel=1)}
}
\keyword{menu}

