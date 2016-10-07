\name{modelAuthorsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelAuthorsClass-class}

\title{S4 class for saving 'rrisk' model authors}
\description{A S4 class for saving information of all the authors of a \code{rrisk} models.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelAuthorsClass", ...)}. 
}
\section{Slots}{
  \describe{
    \item{\code{authors}:}{any \code{"list"} containing model authors, that should be initialized instances of the \code{authorClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelAuthorsClass")}}
	 }
}

\examples{new("modelAuthorsClass")}

\keyword{classes}
