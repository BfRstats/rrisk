\name{change.item}
\alias{change.item}
\title{Function for edditing model slot 'item'}
\usage{
  change.item(index,rriskModel,menuLevel=1)
}
\arguments{
  \item{index}{...}

  \item{rriskModel}{(risk model) is an object of class
  \code{modelClass}}

  \item{menuLevel}{is a numeric value that indicates the
  load of the current menu}
}
\value{
  This function returns edited model item.
}
\description{
  A change function for \code{\linkS4class{itemClass}}.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\note{
  This function requires some functions from the package
  \code{tcltk}.
}
\keyword{edit}

