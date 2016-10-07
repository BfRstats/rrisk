\name{plotUncertainties}
\alias{plotUncertainties}
\title{Displaying scores of model uncertainties}
\usage{
  plotUncertainties(uncertMatrix,
    maintext="ModelUncertainties",
    modelSsystem,useNotApplicable=TRUE,pdfGraph=FALSE,modelName=NULL)
}
\arguments{
  \item{uncertMatrix}{numerical matrix, output of the
  function \code{\link{uncertainties2matrix}}}

  \item{maintext}{character string, plot title}

  \item{modelSsystem}{an instance of the
  \code{modelScoringClass} containing scoring system of the
  \code{rrisk} model}

  \item{useNotApplicable}{logical defining whether 'not
  applicable' scores should be plotted,
  \code{useNotApplicable=c(TRUE,FALSE)}}

  \item{pdfGraph}{logical defining whether a pdf file
  should be created, \code{pdfGraph=c(TRUE,FALSE)}}

  \item{modelName}{name of the model}
}
\description{
  Displaying scores of model uncertainties
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskModel<-init.Model1()
uncertMatrix<-uncertainties2matrix(rriskModel@uncertainties@uncertainties[1:9],
 rriskModel@scoring)
plotUncertainties(uncertMatrix,modelSsystem=rriskModel@scoring,
 useNotApplicable=FALSE)
plotUncertainties(uncertMatrix,modelSsystem=rriskModel@scoring,
 useNotApplicable=TRUE)
plotUncertainties(uncertMatrix,modelSsystem=rriskModel@scoring,
 useNotApplicable=TRUE,pdfGraph=TRUE)}
}
\keyword{graphs}

