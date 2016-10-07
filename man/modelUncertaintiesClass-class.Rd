\name{modelUncertaintiesClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelUncertaintiesClass-class}

\title{S4 class for saving 'rrisk' model uncertainties}
\description{A S4 class for saving the descriptions of all the uncertainties of a \code{rrisk} 
model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelUncertaintiesClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{note}:}{Object of class \code{"character"} }
    \item{\code{uncertainties}:}{\code{"list"} containing model uncertainties, that should be initialized instances of the \code{uncertClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelUncertaintiesClass")} }
	 }
}

\examples{new(Class="modelUncertaintiesClass")}

\keyword{classes}
