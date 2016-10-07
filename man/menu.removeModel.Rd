\name{menu.removeModel}
\alias{menu.removeModel}
\title{Function listing all the models and offering them to be removed}
\usage{
  menu.removeModel(rriskSessionModels,menuLevel=1)
}
\arguments{
  \item{rriskSessionModels}{are the slot models from an
  instance of \code{rriskClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function list all the models and offers them to be
  removed.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
menu.removeModel(rriskSession@models,menuLevel=1)}
}
\keyword{menu}

