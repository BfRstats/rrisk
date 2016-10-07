\name{tolatexGraphs.concept}
\alias{tolatexGraphs.concept}
\title{Function creating LaTeX code for including model uncertainties graphs}
\usage{
  tolatexGraphs.concept(rriskModel,file.name)
}
\arguments{
  \item{rriskModel}{an instance of the \code{modelClass}}

  \item{file.name}{the file that saves tex codes to import
  the concept graphics}
}
\description{
  Function that creates LaTeX code for displaying model
  uncertainties graphs
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\note{
  This function requires some functions from the package
  \code{png}.
}
\examples{
\donttest{rriskModel<-init.Model1()
tolatexGraphs.concept(rriskModel, file.name="concept.tex")  }
}
\keyword{report}

