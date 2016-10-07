\name{tolatex.graphics}
\alias{tolatex.graphics}
\title{producing LaTeX codes adding graphics into the documentation including
concept graphics, network graphics and uncertainty graphics
required by the model}
\usage{
  tolatex.graphics(rriskModel,file.name)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{rriskModel}}

  \item{file.name}{is a character value that indicates the
  Tex file where codes to be written into}
}
\description{
  This creates the LaTeX scripts for the third part in the
  appendix - "Graphics".
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatex.graphics(rriskModel=rriskModel,file.name="report.tex") }
}
\keyword{report}

