\name{change.authors}
\alias{change.authors}
\title{Function for edditing model slot 'authors'}
\usage{
  change.authors(authors)
}
\arguments{
  \item{authors}{(model authors) is an object of class
  \code{modelAuthorsClass}}
}
\value{
  This function returns edited model slot 'authors'.
}
\description{
  Function for edditing model slot 'authors'
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
model@authors<-change.authors(model@authors)
model@authors}
}
\keyword{edit}

