\name{menu.editItems}
\alias{menu.editItems}
\title{Function generating user menu for editing model informations}
\usage{
  menu.editItems(rriskModel,menuLevel=1)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{modelClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function generates the user menu for editing item
  informations.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
menu.editItems(rriskModel)}
}
\keyword{menu}

