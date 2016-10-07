\name{tolatex.preamble}
\alias{tolatex.preamble}
\title{LaTeX code for setting the preamble}
\usage{
  tolatex.preamble(rriskModel, enc, file.name)
}
\arguments{
  \item{rriskModel}{is an instance of the \code{rriskModel}
  with slots including \code{sty}}

  \item{enc}{is the encoding variable of the system}

  \item{file.name}{is a character value that indicates the
  Tex file where codes to be written into}
}
\description{
  This creates the preamble for the LaTeX script.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatex.preamble(rriskModel=rriskModel, enc=options()$encoding, file.name="report.tex") }
}
\keyword{report}

