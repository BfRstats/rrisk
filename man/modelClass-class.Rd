\name{modelClass-class}
\Rdversion{1.1}
\docType{class}
\alias{modelClass-class}

\title{S4 class for saving 'rrisk' models}
\description{A S4 class for saving \code{rrisk} models.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("modelClass", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"modelNameClass"} giving the model name}
    \item{\code{modeltype}:}{Object of class \code{"character"} giving the model type (demo oder user defined model)}
    \item{\code{version}:}{Object of class \code{"modelVersionClass"} giving model version informations (edited by, status, version etc.)}
    \item{\code{authors}:}{Object of class \code{"modelAuthorsClass"} containing information about model authors}
    \item{\code{settings}:}{Object of class \code{"modelSettingsClass"} defining default modle settings}
    \item{\code{scoring}:}{Object of class \code{"modelScoringClass"} describing model scoring system}
    \item{\code{basics}:}{Object of class \code{"modelBasicsClass"} giving model basic informations}
    \item{\code{references}:}{Object of class \code{"modelReferencesClass"} defining model reference list}
    \item{\code{uncertainties}:}{Object of class \code{"modelUncertaintiesClass"} describing model uncertainties}
    \item{\code{parts}:}{Object of class \code{"modelPartsClass"} defining model parts}
    \item{\code{graphs}:}{Object of class \code{"modelGraphsClass"} containing data for model concept graphs and their explanations}
    \item{\code{glossary}:}{Object of class \code{"modelGlossaryClass"} containing model glossary}
    \item{\code{abbreviations}:}{Object of class \code{"modelAbbreviationsClass"} containing model abbreviations list}
    \item{\code{items}:}{Object of class \code{"modelItemsClass"} giving description of model items}
    \item{\code{validation}:}{Object of class \code{"modelValidationClass"} containing model validations}
    \item{\code{comments}:}{Object of class \code{"modelCommentsClass"} containing modle comments}
    \item{\code{conclusions}:}{Object of class \code{"modelConclusionsClass"}  }
    \item{\code{output}:}{Object of class \code{"modelOutputClass"} representing model conclusions}
    \item{\code{archive}:}{Object of class \code{"ANY"} savinf model archive}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "modelClass")} }
	 }
}

\examples{new("modelClass")}

\keyword{classes}

