\name{validationClass-class}
\Rdversion{1.1}
\docType{class}
\alias{validationClass-class}

\title{S4 class for saving model validity entry}
\description{A S4 class for saving the descriptions of a validity of a \code{rrisck} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("validationClass", ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{"character"} string containing name of the current validation entry}
    \item{\code{explanation}:}{\code{"character"} string containing explanation of the current validation entry}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "validationClass")}}
	 }
}

\examples{new("validationClass")}

\keyword{classes}
