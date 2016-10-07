\name{authorClass-class}
\Rdversion{1.1}
\docType{class}
\alias{authorClass-class}

\title{S4 class for saving author's information}
\description{A S4 class that saves the information about authors of the \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("authorClass", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{"character"} string containing author name}
    \item{\code{institution}:}{\code{"character"} string containing institution, where the author is based}
    \item{\code{email}:}{\code{"character"} string containing author e-mail adress}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "authorClass")}}
	 }
}
\examples{new("authorClass")}
\keyword{classes}

