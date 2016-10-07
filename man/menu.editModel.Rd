\name{menu.editModel}
\alias{menu.editModel}
\title{Function generating user menu for editing model informations}
\usage{
  menu.editModel(rriskModel,availableModels,menuLevel=1)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{modelClass}}

  \item{availableModels}{is a character vector containing
  model names that are available in the current rrisk
  session}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\description{
  This function generates the user menu for editing model
  informations.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\dontrun{rriskModel<-init.Model1()
menu.editModel(rriskModel,menuLevel=1)}
}
\keyword{menu}

