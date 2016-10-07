\name{basicsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{basicsClass-class}

\title{S4 class for representing basic descriptions}
\description{A S4 class for representing basic descriptions of the \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("basicsClass", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{"character"} string containing name of the current model basic description topic}
    \item{\code{explanation}:}{\code{"character"} string containing explanation of the current model basic description}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "basicsClass")} }
	 }
}

\examples{new("basicsClass")

new("basicsClass",name="Background",explanation="Some basckground information...")}

\keyword{classes}
