\name{rriskClass-class}
\Rdversion{1.1}
\docType{class}
\alias{rriskClass-class}

\title{S4 class for saving 'rrisk' session}
\description{A S4 class for saving the \code{rrisk} session.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rriskClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{models}:}{\code{"list"} containing available rrisk models, that should be initialized instances of the \code{modelClass} class}
    \item{\code{scoringsystems}:}{code{"list"} containing available scoring systems, that should be initialized instances of the \code{modelScoringClass} class}
    \item{\code{disclaimer}:}{\code{"list"} of character string containing some basic rrisk informations}
    \item{\code{glossary}:}{\code{"list"} containing rrisk glossary, that should be initialized instances of the \code{glossaryClass} class}
    \item{\code{abbreviations}:}{\code{"list"} containing rrisk abbreviations, that should be initialized instances of the \code{glossaryClass} class}
    \item{\code{rpackages}:}{\code{"character"} vector giving R packages on which rrisk package depends on}
     \item{\code{rriskversion}:}{\code{"character"} vector giving the version of the 'rrisk' package}
    \item{\code{rversion}:}{\code{"character"} string giving R version under which rrisk was built}
  }
}
\section{Methods}{
No methods defined with class "rriskClass" in the signature.
}

\examples{new("rriskClass")}

\keyword{classes}
