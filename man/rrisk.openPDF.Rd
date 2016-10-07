\name{rrisk.openPDF}
\alias{rrisk.openPDF}
\title{Function that starts a proper PDF viewer according to the system environment}
\usage{
  rrisk.openPDF(file, bg = TRUE)
}
\arguments{
  \item{file}{the file to be opened}

  \item{bg}{parameter for unix systems}
}
\description{
  Function that starts a proper PDF viewer according to the
  system environment
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{file = file.path(getwd(), "rrisk-manual.PDF")
rrisk.openPDF(file, bg = TRUE)}
}
\keyword{rrisk}

