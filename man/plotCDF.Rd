\name{plotCDF}
\alias{plotCDF}
\title{Function that draws a cumulative distribution of the main outcome function based on 2d-simulation}
\usage{
  plotCDF(rriskModel,pdfGraph=FALSE)
}
\arguments{
  \item{rriskModel}{is an instance of the class
  \code{modelClass}}

  \item{pdfGraph}{decides whether the plot is as PDF data
  to be saved}
}
\description{
  A function that creates a cumulative distribution of the
  main outcome function based on 2d-simulation.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel <- init.Model1()
rriskModel<-run(rriskModel)
plotCDF(rriskModel) }
}
\keyword{graphs}

