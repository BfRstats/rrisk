\name{tolatex.modelValidation}
\alias{tolatex.modelValidation}
\title{LaTeX code for producing texts displaying the Verification and
validation of the model}
\usage{
  tolatex.modelValidation(rriskModel, file.name)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{rriskModel}}

  \item{file.name}{is a character value that indicates the
  Tex file where codes to be written into}
}
\description{
  This creates the LaTeX scripts producing the sixth part
  in the documentation - "Verification and Validation".
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatex.modelValidation(rriskModel=rriskModel, file.name="report.tex") }
}
\keyword{report}

