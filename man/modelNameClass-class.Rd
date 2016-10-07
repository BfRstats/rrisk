\name{modelNameClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelNameClass-class}

\title{S4 class for saving the names of 'rrisk' models}
\description{A S4 class for saving the names of \code{rrisk} models.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelNameClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{"character"} string defining model name}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelNameClass")}}
	 }
}

\examples{new("modelNameClass")}

\keyword{classes}
