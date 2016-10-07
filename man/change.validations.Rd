\name{change.validations}
\alias{change.validations}
\title{Function for edditing model slot 'validations'}
\usage{
  change.validations(validations)
}
\arguments{
  \item{validations}{(model validations) is an object of
  class \code{modelValidationClass}}
}
\value{
  This function returns edited model slot 'validations'.
}
\description{
  Change function for
  \code{\linkS4class{modelValidationClass}}.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\note{
  This function requires some functions from the package
  \code{tcltk}.
}
\examples{
\donttest{model<-init.Model1()
model@validation<-change.validations(model@validation)
model@validation}
}
\keyword{edit}

