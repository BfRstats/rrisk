\name{plotTornado}
\alias{plotTornado}
\title{Function that draws a Tornado plot}
\usage{
  plotTornado(rriskModel,pdfGraph=FALSE)
}
\arguments{
  \item{rriskModel}{is an instance of the class
  \code{modelClass}}

  \item{pdfGraph}{decides whether the plot is as PDF data
  to be saved}
}
\description{
  A function that creates a Tornado plot.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel <- init.Model1()
rriskModel<-run(rriskModel)
plotTornado(rriskModel,pdfGraph=FALSE)  }
}
\keyword{graphs}

