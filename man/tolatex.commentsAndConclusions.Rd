\name{tolatex.commentsAndConclusions}
\alias{tolatex.commentsAndConclusions}
\title{LaTeX code for producing texts listing all comments and conclusions
of the model}
\usage{
  tolatex.commentsAndConclusions(rriskModel, file.name)
}
\arguments{
  \item{rriskModel}{is an instance of the
  \code{rriskModel}}

  \item{file.name}{is a character value that indicates the
  Tex file where codes to be written into}
}
\description{
  This creates the LaTeX scripts producing the seventh part
  in the documentation - "Comments and conclusions".
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatex.commentsAndConclusions(rriskModel=rriskModel, file.name="report.tex") }
}
\keyword{report}

