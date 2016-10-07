\name{plotOFConvergence}
\alias{plotOFConvergence}
\title{Function that draws a convergence plot}
\usage{
  plotOFConvergence(rriskModel,pdfGraph=FALSE)
}
\arguments{
  \item{rriskModel}{is an instance of the class
  \code{modelClass}}

  \item{pdfGraph}{decides whether the plot is as PDF data
  to be saved}
}
\description{
  A function that creates a convergence plot.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel <- init.Model1()
rriskModel<-run(rriskModel)
plotOFConvergence(rriskModel,pdfGraph=FALSE)  }
}
\keyword{graphs}

