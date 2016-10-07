\name{plotOFHistogram}
\alias{plotOFHistogram}
\title{Function that draws a histogram}
\usage{
  plotOFHistogram(rriskModel,pdfGraph=FALSE)
}
\arguments{
  \item{rriskModel}{is an instance of the class
  \code{modelClass}}

  \item{pdfGraph}{decides whether the histogram is as PDF
  data to be saved}
}
\description{
  A function that creates a histogram.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel <- init.Model1()
rriskModel<-run(rriskModel)
plotOFHistogram(rriskModel,pdfGraph=FALSE)  }
}
\keyword{graphs}

