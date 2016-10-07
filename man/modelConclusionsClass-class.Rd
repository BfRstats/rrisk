\name{modelConclusionsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelConclusionsClass-class}

\title{S4 class for saving all 'rrisk' model conclusions}
\description{A S4 class for saving all the conclusions of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelConclusionsClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{conclusions}:}{\code{"list"} of \code{character} strings with model conclusions}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelConclusionsClass")}}
	 }
}

\examples{new("modelConclusionsClass")}

\keyword{classes}
