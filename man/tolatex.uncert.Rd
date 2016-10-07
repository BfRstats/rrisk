\name{tolatex.uncert}
\alias{tolatex.uncert}
\title{LaTeX code for producing texts displaying uncertainties of the model}
\usage{
  tolatex.uncert(rriskModel, file.name)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{rriskModel}}

  \item{file.name}{is a character value that indicates the
  Tex file where codes to be written into}
}
\description{
  This creates the LaTeX scripts producing the second part
  in the documentation - "Uncertainties and knowledge
  base".
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatex.uncert(rriskModel=rriskModel, file.name="report.tex") }
}
\keyword{report}

