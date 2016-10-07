\name{modelPartsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelPartsClass-class}

\title{S4 class for saving 'rrisk' model parts}
\description{A S4 class for saving the descriptions of all the parts of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelPartsClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{parts}:}{\code{"list"} containing model parts, that should be initialized instances of the \code{partClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelPartsClass")}}
	 }
}

\examples{new("modelPartsClass")}

\keyword{classes}
