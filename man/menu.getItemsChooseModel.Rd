\name{menu.getItemsChooseModel}
\alias{menu.getItemsChooseModel}
\title{Function generating user menu for choosing a model from which items should be
imported}
\usage{
  menu.getItemsChooseModel(rriskModel,rriskSessionModels,menuLevel=1)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{modelClass}}

  \item{rriskSessionModels}{are the slot models of an
  rriskSession object}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function generates the user menu for choosing a
  model from which items should be imported.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
menu.getItemsChooseModel(rriskModel = rriskSession@models[[1]],
rriskSession@models, menuLevel=1)}
}
\keyword{menu}

