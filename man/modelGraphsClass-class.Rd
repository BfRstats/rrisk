\name{modelGraphsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelGraphsClass-class}

\title{S4 class for saving all 'rrisk' model graphs objects}
\description{A S4 class for saving all the graphs used in \code{rrisk} models.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelGraphsClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{graphs}:}{\code{"list"} containing model graphs, that should be initialized instances of the \code{graphClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelGraphsClass")} }
	 }
}

\examples{new("modelGraphsClass")}

\keyword{classes}
