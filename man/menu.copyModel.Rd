\name{menu.copyModel}
\alias{menu.copyModel}
\title{Function listing all available models and offers them to be duplicated}
\usage{
  menu.copyModel(rriskSessionModels,menuLevel=1)
}
\arguments{
  \item{rriskSessionModels}{are the slot models from an
  instance of \code{rriskClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function lists all available models and offers them
  to be duplicated.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
menu.copyModel(rriskSession@models,menuLevel=1)}
}
\keyword{menu}

