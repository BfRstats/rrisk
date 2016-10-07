\name{itemClass-class}
\Rdversion{1.1}
\docType{class}
\alias{itemClass-class}

\title{S4 class for saving model items}
\description{A S4 class for saving an Item of \code{rrisk} models.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("itemClass", ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{part}:}{\code{"character"} string giving the model part where the item belongs to}
    \item{\code{name}:}{\code{"character"} string defining the item name}
    \item{\code{title}:}{\code{"character"} string defining the title of the item description}
    \item{\code{stratum}:}{\code{"character"} string defining item stratification}
    \item{\code{stratumevaluated}:}{\code{"logical"} value defining the way the strata item has been created (relevent only for 'strata' items)}
    \item{\code{explanation}:}{\code{"character"} string containing item explanation}
    \item{\code{type}:}{\code{"character"} string containing information about item type}
    \item{\code{typecode}:}{\code{"character"} string containing abbreviation of item type}
    \item{\code{data}:}{\code{"ANY"} data (relevant only for \code{data} items)}
    \item{\code{definition}:}{\code{"character"} giving informal item definition like a formula}
    \item{\code{depitem}:}{\code{"character"} giving dependent items}
    \item{\code{unit}:}{\code{"character"} string giving units by which item is measured}
    \item{\code{role}:}{\code{"character"} string defining the role of the current item in model}
    \item{\code{rolecode}:}{\code{"character"} string containing abbreviation of the item role}
    \item{\code{plausimin}:}{numerical value giving the plausibility minimal limit of numerical item, object of class \code{"numericNULL"}}
    \item{\code{plausimax}:}{numerical value giving the plausibility maximal limit of numerical item, object of class \code{"numericNULL"}}
    \item{\code{scores}:}{numerical vector giving the score weighting of the current item according to the model scoring system, object of class \code{"numericNULL"}}
    \item{\code{assumptions}:}{\code{"character"} string describing item assumptions}
    \item{\code{remark}:}{\code{"character"} string containing any remarks to teh current item}
    \item{\code{reference}:}{\code{"character"} string giving any reference according to model reference list}
    \item{\code{fullc}:}{\code{"character"} string containing full comannd for evaluating current item}
    \item{\code{fullcommand}:}{\code{"character"} string containing full comannd for evaluating current item}
    \item{\code{relaxc}:}{\code{"character"} string containing relax comannd for evaluating current item}
    \item{\code{relaxcommand}:}{\code{"character"} string containing relax comannd for evaluating current item}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "itemClass")}}
	 }
}

\examples{new("itemClass")}
\keyword{classes}
