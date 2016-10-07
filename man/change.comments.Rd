\name{change.comments}
\alias{change.comments}
\title{Function for edditing model slot 'comments'}
\usage{
  change.comments(comments)
}
\arguments{
  \item{comments}{(model comments) is an object of class
  \code{modelCommentsClass}}
}
\value{
  This function returns edited model slot 'comments'.
}
\description{
  A change function for
  \code{\linkS4class{modelCommentsClass}}.
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
model@comments<-change.comments(model@comments)
model@comments}
}
\keyword{edit}

