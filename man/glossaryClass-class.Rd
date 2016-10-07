\name{glossaryClass-class}
\Rdversion{1.1}
\docType{class}
\alias{glossaryClass-class}

\title{S4 class for saving model glossary entry}
\description{A S4 class for saving a glossary of a \code{rrisk} model.}


\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("glossaryClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{"character"} string containing name of the current model glossary entry}
    \item{\code{explanation}:}{\code{"character"} string containing explanation of the current model glossary entry}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "glossaryClass")} }
	 }
}

\examples{new("glossaryClass")}

\keyword{classes}
