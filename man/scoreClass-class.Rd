\name{scoreClass-class}
\Rdversion{1.1}
\docType{class}
\alias{scoreClass-class}

\title{S4 class for saving model score items}
\description{A S4 class for saving the information of a scoring system of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("scoreClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{notation}:}{\code{"character"} string giving notation of the current score}
    \item{\code{name}:}{\code{"character"} string giving name of the current score}
    \item{\code{explanation}:}{\code{"character"} string containing explanation of the current score}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "scoreClass")}}
	 }
}

\examples{new("scoreClass")}
\keyword{classes}
