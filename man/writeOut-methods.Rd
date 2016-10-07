\docType{methods}
\name{writeOut}
\alias{writeOut}
\alias{writeOut-methods}
\alias{writeOut,modelClass-method}
\alias{writeOut,modelReferencesClass-method}
\alias{writeOut,modelScoringClass-method}
\title{Generic function and methods for writing specific model informations into a file}

\description{Generic function and methods for writing specific model informations into a file}

\usage{
writeOut(object,\dots)

\S4method{writeOut}{modelReferencesClass}(object,model.name)

\S4method{writeOut}{modelClass}(object) 

\S4method{writeOut}{modelScoringClass}(object) 

}

\arguments{
  \item{object}{an object for which a \code{sumUp} method is desired, it can be an instance of class \code{modelReferencesClass},
  \code{modelClass} or \code{modelScoringClass}}
  \item{model.name}{character string, name of the rrisk model}
  \item{\dots}{additional argument list that might not ever be used}
}

\examples{

\donttest{
rriskModel<-init.Model1()

writeOut(new("modelReferencesClass"))
writeOut(rriskModel@references,model.name=rriskModel@name@name)

writeOut(new("modelClass"))
writeOut(rriskModel)

writeOut(new("modelScoringClass"))
writeOut(rriskModel@scoring)}
}

