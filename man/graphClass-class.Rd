\name{graphClass-class}
\Rdversion{1.1}
\docType{class}
\alias{graphClass-class}

\title{S4 class for saving model graph object}
\description{A S4 class for saving a graph used in a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("graphClass", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{explanation}:}{\code{"character"} string containing information about the current model concept graph}
    \item{\code{cover}:}{\code{"logical"} value, giving whether the current model concept graph should be used an a cover picture by model reporting}
    \item{\code{graphdata}:}{Object of class \code{"ANY"} containing data in special format to representing the current model concept graph}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "graphClass")} }
	 }
}
\examples{new("graphClass")}
\keyword{classes}
