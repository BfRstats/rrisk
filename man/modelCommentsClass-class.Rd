\name{modelCommentsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelCommentsClass-class}

\title{S4 class for saving all 'rrisk' model comments}
\description{A S4 class for saving all the comments of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelCommentsClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{comments}:}{\code{"list"} of \code{character} srings with model comments}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelCommentsClass")} }
	 }
}

\examples{new("modelCommentsClass")}

\keyword{classes}
