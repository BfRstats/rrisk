\name{change.abbreviations}
\alias{change.abbreviations}
\title{Function for edditing model slot 'abbreviations'}
\usage{
  change.abbreviations(abbreviations)
}
\arguments{
  \item{abbreviations}{(model abbreviations) is an object
  of class \code{modelAbbreviationsClass}}
}
\value{
  This function returns edited model slot 'abbreviations'.
}
\description{
  A change function for
  \code{\linkS4class{modelAbbreviationsClass}}.
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
model@abbreviations<-change.abbreviations(model@abbreviations)
model@abbreviations}
}
\keyword{edit}

