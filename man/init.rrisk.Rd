\name{init.rrisk}
\alias{init.rrisk}
\title{Function that installs packages required by 'rrisk'}
\usage{
  init.rrisk(lib=.libPaths()[1])
}
\arguments{
  \item{lib}{is the file path of the packages}
}
\description{
  A function that installs packages required by
  \code{rrisk}.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{init.rrisk(lib=.libPaths()[1])}
}
\keyword{init}

