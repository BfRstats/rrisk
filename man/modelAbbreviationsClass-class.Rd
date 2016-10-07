\name{modelAbbreviationsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelAbbreviationsClass-class}

\title{S4 class for saving 'rrisk' model abbreviations list}
\description{A S4 class for saving all the abbreviations uesed in a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelAbbreviationsClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{abbreviations}:}{\code{"list"} containing model abbreviations, that should be initialized instances of the \code{glossaryClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelAbbreviationsClass")}}
	 }
}

\examples{new("modelAbbreviationsClass")}
\keyword{classes}
