\name{tolatex.references}
\alias{tolatex.references}
\title{LaTeX code for producing refrences page concerning the model}
\usage{
  tolatex.references(rriskModel, file.name)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{rriskModel}}

  \item{file.name}{is a character value that indicates the
  Tex file where codes to be written into}
}
\description{
  This creates the LaTeX scripts producing the eighth part
  in the documentation - "References".
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatex.references(rriskModel=rriskModel, file.name="report.tex") }
}
\keyword{report}

