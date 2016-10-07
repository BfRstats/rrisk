\name{tolatex.itemsAndParts}
\alias{tolatex.itemsAndParts}
\title{LaTeX code for producing texts displaying the structure and contents
of the model in form of items of every available parts}
\usage{
  tolatex.itemsAndParts(rriskModel, file.name)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{rriskModel}}

  \item{file.name}{is a character value that indicates the
  Tex file where codes to be written into}
}
\description{
  This creates the LaTeX scripts producing the third part
  in the documentation - "Model structure and contents".
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatex.itemsAndParts(rriskModel=rriskModel, file.name="report.tex") }
}
\keyword{report}

