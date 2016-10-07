\name{partClass-class}
\Rdversion{1.1}
\docType{class}
\alias{partClass-class}

\title{S4 class for saving model part item}
\description{A S4 class for saving the descriptions of a part of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("partClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{"character"} string defining name of the current model part}
    \item{\code{explanation}:}{\code{"character"} string giving explanation of the current model part}
    \item{\code{items}:}{\code{"character"} vector specifying items, that belong to the current model part}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "partClass")}}
	 }
}

\examples{new("partClass")}

\keyword{classes}
