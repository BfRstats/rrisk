\name{menu.getTopicsChooseModel}
\alias{menu.getTopicsChooseModel}
\title{Function generating user menu for choosing a model from which topics should be imported}
\usage{
  menu.getTopicsChooseModel(rriskModel,rriskSessionModels,menuLevel=1)
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
  model from which topics should be imported.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
rriskSession<-init.rriskSession(useDemoModels="all")
menu.getTopicsChooseModel(rriskSession@models[[1]],rriskSession@models,menuLevel=1)}
}
\keyword{menu}

