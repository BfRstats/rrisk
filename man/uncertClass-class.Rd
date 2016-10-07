\name{uncertClass-class}
\Rdversion{1.1}
\docType{class}
\alias{uncertClass-class}

\title{S4 class for saving model uncertainty item}
\description{A S4 class for saving the descriptions of an uncertainty of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("uncertClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{namemain}:}{\code{"character"} string defining the group of model uncertainties, where the current uncertainty belongs to}
    \item{\code{namesub}:}{\code{"character"} string defining name of the current model uncertainty}
    \item{\code{explanation}:}{\code{"character"} string giving the explanation of the current uncertainty}
    \item{\code{scores}:}{Numeric vector giving quantitation of the current uncertainty according to model scoring system (an object of class \code{"numericNULL"})}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "uncertClass")}}
	 }
}

\examples{new("uncertClass")}

\keyword{classes}

