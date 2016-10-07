\name{modelBasicsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelBasicsClass-class}

\title{S4 class for saving 'rrisk' model basic descriptions}
\description{A S4 class for saving basic descriptions of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelBasicsClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{basics}:}{\code{"list"} containing model besic informations, that should be initialized instances of the \code{basicsClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelBasicsClass")}}
	 }
}

\examples{new("modelBasicsClass")}

\keyword{classes}

