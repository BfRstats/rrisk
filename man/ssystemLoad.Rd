\name{ssystemLoad}
\alias{ssystemLoad}
\title{Function that loads external scoring systems}
\usage{
  ssystemLoad(rriskSessionSsystems)
}
\arguments{
  \item{rriskSessionSsystems}{the slot scoringsystems of
  the instance of the \code{rriskClass} object}
}
\description{
  Function that loads external scoring systems
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
ssystemLoad(rriskSession@scoringsystems)}
}
\keyword{scoring}

