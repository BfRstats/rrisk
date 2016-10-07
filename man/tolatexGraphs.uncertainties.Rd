\name{tolatexGraphs.uncertainties}
\alias{tolatexGraphs.uncertainties}
\title{LaTeX code for displaying model uncertainties graphs}
\usage{
  tolatexGraphs.uncertainties(rriskModel,LatexReport=FALSE,
    file.name,pdfGraph=FALSE)
}
\arguments{
  \item{rriskModel}{an instance of the \code{modelClass}}

  \item{LatexReport}{whether a LaTeX report should be
  created}

  \item{file.name}{file that saves the tex codes to import
  the unvertainties graphics}

  \item{pdfGraph}{whether the pdf graphics should be
  created}
}
\description{
  Create LaTeX code for displaying model uncertainties
  graphs
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatexGraphs.uncertainties(rriskModel=rriskModel,
LatexReport=TRUE,file.name="uncertainties.tex")  }
}
\keyword{report}

