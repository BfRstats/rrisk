\name{rriskAbout}
\alias{rriskAbout}
\title{Function that creates the about-information of the rrisk in a window}
\usage{
  rriskAbout(disclaimer, demomode)
}
\arguments{
  \item{disclaimer}{the slot disclaimer of the instance of
  the \code{rriskClass} object}

  \item{demomode}{whether the function be run under demo
  mode}
}
\description{
  Function that creates the about-information of the
  \code{rrisk} in a window
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
\donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
rriskAbout(rriskSession@disclaimer)}
}
\keyword{rrisk}

