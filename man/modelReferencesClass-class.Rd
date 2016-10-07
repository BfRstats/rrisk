\name{modelReferencesClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelReferencesClass-class}

\title{Class "modelReferencesClass"}
\description{A S4 class for saving the references of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelReferencesClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{references}:}{\code{"character"} string giving model references in BibTeX format}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelReferencesClass")} }
	 }
}
\examples{
new("modelReferencesClass")
}
\keyword{classes}

