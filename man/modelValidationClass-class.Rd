\name{modelValidationClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelValidationClass-class}

\title{S4 class for saving 'rrisk' model validities}
\description{A S4 class for saving all the validities of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelValidationClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{validation}:}{\code{"list"} containing model validations, that should be initialized instances of the \code{validationClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelValidationClass")} }
	 }
}

\examples{new("modelValidationClass")}

\keyword{classes}
