\name{getParts}
\alias{getParts}
\title{Function that imports slot 'parts' from another 'modelClass' object}
\usage{
  getParts(rriskModel,rriskModelFrom,
    partsToImportIndex=1:length(rriskModelFrom@parts@parts),onlyItems=FALSE)
}
\arguments{
  \item{rriskModel}{the \code{rrisk} model the parts to be
  exported to}

  \item{rriskModelFrom}{the \code{rrisk} model from which
  the parts are exported}

  \item{partsToImportIndex}{index of all parts in the
  rriskmodelFrom object}

  \item{onlyItems}{whether only items should be imported}
}
\description{
  Function that imports parts from another
  \code{\linkS4class{modelClass}} object
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-new("modelClass")
rriskModelFrom<-init.Model1()
getParts(rriskModel,rriskModelFrom,
  partsToImportIndex=1:length(rriskModelFrom@parts@parts),onlyItems=FALSE)}
}
\keyword{model}

