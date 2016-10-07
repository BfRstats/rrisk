\name{modelOutputClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelOutputClass-class}

\title{S4 class for saving the output of a rrisk model}
\description{
A S4 class for saving the outputs of a \code{rrisk} model.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelOutputClass", ...)}.

}    
             
\section{Slots}{
  \describe{
    \item{\code{fullout.1d}:}{Object of class \code{"list"} saving results of 1d simulation}
    \item{\code{fullout.2d}:}{Object of class \code{"list"} saving results of 2d simulation}
    \item{\code{OFcdfCI}:}{Object of class \code{"list"} saving confidence intervals of the outcome function}
    \item{\code{OFname.2d}:}{Object of class \code{"list"} saving names of the outcome functions}
    \item{\code{relaxout.1d}:}{Object of class \code{"list"} saving relaxed results of 1d simulation}
    \item{\code{runtime1d}:}{Object of class \code{"list"} saving run time of 1d simulation}
    \item{\code{runtime2d}:}{Object of class \code{"list"} saving run time of 2d simulation}
    \item{\code{summaries}:}{Object of class \code{"list"} saving summaries table about simulation results}
    \item{\code{uncitems.2d}:}{Object of class \code{"list"} saving names of the uncertainty items}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelOutputClass")}: ... }
  }
}
\examples{new("modelOutputClass")}

\keyword{classes}


