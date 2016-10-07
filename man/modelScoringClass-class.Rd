\name{modelScoringClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelScoringClass-class}

\title{S4 class for saving 'rrisk' model scoring system}
\description{A S4 class for saving the scoring systems of \code{rrisk} models.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelScoringClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{"character"} string giving scoring system name}
    \item{\code{tableheader}:}{\code{"character"} string that will be used as table header by model reporting and describing scoring system}
    \item{\code{explanatory}:}{\code{"character"} string containing explanation of the current scoring system}
    \item{\code{values}:}{\code{"numeric"} vector defining possible score range (for example \code{c(0,1,2,3)})}
    \item{\code{vcolors}:}{\code{"numeric"} numerical vector defining colors of each score value (for example \code{c("blue"=0,"green"=1,"yellow"=2,"red"=3)})}
    \item{\code{vmeanings}:}{\code{"numeric"} numerical vector defining meanings of each score value (for example \code{c("notapplicable"=0,"low"=1,"medium"=2,"high"=3)})}
    \item{\code{systemtype}:}{\code{"character"} string giving the type of scoring system ("rrisk scoring system" oder "user defined")}
    \item{\code{scoring}:}{\code{"list"} containing scores, that should be initialized instances of the \code{scoreClass} class}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelScoringClass")} }
	 }
}
\examples{new("modelScoringClass")}

\keyword{classes}
