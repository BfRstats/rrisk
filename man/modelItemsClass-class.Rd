\name{modelItemsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelItemsClass-class}

\title{S4 class for saving all 'rrisk' model items}
\description{A S4 class for saving all the items of \code{rrisk} models.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelItemsClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{items}:}{\code{"list"} containing model items, that should be initialized instances of the \code{itemClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelItemsClass")}}
	 }
}

\examples{new("modelItemsClass")}

\keyword{classes}
