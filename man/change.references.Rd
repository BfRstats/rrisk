\name{change.references}
\alias{change.references}
\title{Function for edditing model slot 'references'}
\usage{
  change.references(references)
}
\arguments{
  \item{references}{(model references) is an object of
  class \code{modelReferencesClass}}
}
\value{
  This function returns edited model slot 'references'.
}
\description{
  A change function for
  \code{\linkS4class{modelReferencesClass}}.
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
model@references<-change.references(model@references)
model@references}
}
\keyword{edit}

