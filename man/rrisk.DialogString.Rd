\name{rrisk.DialogString}
\alias{rrisk.DialogString}
\title{Function that creates a dialog window}
\usage{
  rrisk.DialogString(message="",default="")
}
\arguments{
  \item{message}{the message that should be shown on the
  dialog window}

  \item{default}{a default text}
}
\description{
  Function that creates a dialog window
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
\donttest{rrisk.DialogString(message="Please enter the name of the new model:")}
}
\keyword{rrisk}

