\name{menu.getChoiceOfItems}
\alias{menu.getChoiceOfItems}
\title{Function listing all available parts of a rrisk model, from which it offers the user to choose and saves the choice of the user}
\usage{
  menu.getChoiceOfItems(rriskModel,rriskModelFrom,menuLevel=1)
}
\arguments{
  \item{rriskModel}{is the \code{rrisk} model where the
  parts are to be exported to}

  \item{rriskModelFrom}{is the \code{rrisk} model from
  which the parts are exported}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function lists all available items of a \code{rrisk}
  model, from which it offers the user to choose and saves
  the choice of the user.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{emptyModel<-new("modelClass")
rriskModel<-init.Model1()
menu.getChoiceOfItems(rriskModel=emptyModel,rriskModelFrom=rriskModel,menuLevel=1)}
}
\keyword{menu}

