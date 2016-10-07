\name{menu.createModel}
\alias{menu.createModel}
\title{Function generating user menu for creating a new model with various possibilities}
\usage{
  menu.createModel(rriskSessionModels,menuLevel=1)
}
\arguments{
  \item{rriskSessionModels}{are the slot models from an
  instance of \code{rriskClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function generates the user menu for creating a new
  model with various possibilities.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\dontrun{rriskSession<-init.rriskSession(useDemoModels = "all")
menu.createModel(rriskSession@scoringsystems,menuLevel=1)}
}
\keyword{menu}

