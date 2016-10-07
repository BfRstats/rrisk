\docType{methods}
\name{sumUp}
\alias{sumUp}
\alias{sumUp-methods}
\alias{sumUp,modelClass-method}
\alias{sumUp,modelScoringClass-method}
\alias{sumUp,rriskClass-method}

\title{Generic function and methods for summing up specific model informations}

\description{Generic function and methods for summing up specific model informations}

\usage{
sumUp(object,\dots)

\S4method{sumUp}{modelClass}(object,showFullCommand) 

\S4method{sumUp}{modelScoringClass}(object) 

\S4method{sumUp}{rriskClass}(object)

}

\arguments{
  \item{object}{an object for which a \code{sumUp} method is desired. It can be an instance of class \code{modelClass},
  \code{modelScoringClass} or \code{rriskClass}}
  \item{showFullCommand}{a single logical value defining whether item full command will be shown}
  \item{\dots}{additional argument list that might not ever be used}
}

\examples{
\donttest{
rriskModel<-init.Model1()

sumUp(new("modelClass"))
sumUp(rriskModel,showFullCommand=TRUE)
sumUp(rriskModel,showFullCommand=FALSE)

sumUp(new("modelScoringClass"))
sumUp(rriskModel@scoring)

sumUp(new("rriskClass"))
sumUp(init.rriskSession())
}
}

