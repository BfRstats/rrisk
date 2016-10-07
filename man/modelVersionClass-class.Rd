\name{modelVersionClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelVersionClass-class}

\title{S4 class for saving 'rrisk' model version}
\description{A S4 class for saving the basic information of a \code{rrisk} model version.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelVersionClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{status}:}{\code{"character"} string specifying model status ("Draft" or "Release") }
    \item{\code{minorupdate}:}{\code{"numeric"} value giving minor update}
    \item{\code{majorupdate}:}{\code{"numeric"} value giving major update}
    \item{\code{subtitle}:}{\code{"character"} string containing subtitle for model report}
    \item{\code{editedby}:}{\code{"character"} string representing the author of last update}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelVersionClass")}}
	 }
}

\examples{new("modelVersionClass")}

\keyword{classes}
