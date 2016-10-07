\name{plotTree}
\alias{plotTree}
\title{Function that draws a regression tree plot}
\usage{
  plotTree(rriskModel,pdfGraph=FALSE)
}
\arguments{
  \item{rriskModel}{is an instance of the class
  \code{modelClass}}

  \item{pdfGraph}{decides whether the plot is as PDF data
  to be saved}
}
\description{
  A function that creates a regression tree plot.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel <- init.Model1()
rriskModel<-run(rriskModel)
plotTree(rriskModel,pdfGraph=FALSE)  }
}
\keyword{graphs}

