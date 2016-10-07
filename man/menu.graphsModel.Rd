\name{menu.graphsModel}
\alias{menu.graphsModel}
\title{Function generating user menu for displaying model graphics}
\usage{
  menu.graphsModel(rriskModel,menuLevel=1)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{modelClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function generates the user menu for displaying
  model graphics.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
menu.graphsModel(rriskModel,menuLevel=1)}
}
\keyword{menu}

