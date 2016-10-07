\name{createEmptySsystem}
\alias{createEmptySsystem}
\title{Function that creates an empty scoring system}
\usage{
  createEmptySsystem(rriskSessionSsystems)
}
\arguments{
  \item{rriskSessionSsystems}{the slot scoringsystems of an
  instance of the \code{rriskSession} object}
}
\description{
  Function that creates an empty scoring system
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
createEmptySsystem(rriskSession@scoringsystems)}
}
\keyword{scoring}

