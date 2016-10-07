\name{modelGlossaryClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelGlossaryClass-class}

\title{S4 class for saving 'rrisk' model glossary list}
\description{A S4 class for saving all the glossaries of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelGlossaryClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{glossary}:}{\code{"list"} containing model glossary entries, that should be initialized instances of the \code{glossaryClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelGlossaryClass")}}
	 }
}

\examples{new("modelGlossaryClass")}

\keyword{classes}
