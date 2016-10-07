\name{modelSettingsClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelSettingsClass-class}

\title{S4 class for saving 'rrisk' model settings}
\description{A S4 class for saving the information of all the default settings of a \code{rrisk} model.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelSettingsClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{wscores}:}{\code{"character"} string giving how to weight individual scores of model scoring system}
    \item{\code{N}:}{\code{"numeric"} value giving the number of iterations for 1d simulation}
    \item{\code{N2d}:}{\code{"numeric"} value giving the number of iterations for 2d simulation}
    \item{\code{Ntest}:}{\code{"numeric"} value giving the number of iterations for 1d simulation (only for test evaluations)}
    \item{\code{stress}:}{\code{"numeric"} value giving stress test percentile}
    \item{\code{mycol}:}{\code{"character"} string defining the default color of output graphs}
    \item{\code{nwlayout}:}{\code{"character"} string defining the layout of model network graph ("kamadakawai", "fruchtermanreingold","circle")}
    \item{\code{usenotapplicable}:}{\code{"logical"} value specifying whether scores with not applicable quantitation should be plotted in model uncertainties plots}
    \item{\code{coverheader}:}{\code{"character"} string specifying cover picture for model report ("model network","concept graph" or "none")}
    \item{\code{trans}:}{\code{"character"} string defining transformation for sensitivity analysis ("rank", "z-score" or "identity")}
    \item{\code{sens}:}{\code{"character"} string defining model for sensitivity analysis ("correlation" or "regression")}
    \item{\code{sty}:}{\code{"character"} vector containing optional additinal LaTeX style files}
    \item{\code{deleteTeX}:}{\code{"logical"} value specifying whether LaTeX source file should be deleted after creating of model report}
    \item{\code{abserror}:}{\code{"numeric"} value giving the absolute error tolerance relevant for plotting convergence}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelSettingsClass")} }
	 }
}

\examples{new("modelSettingsClass")}

\keyword{classes}

