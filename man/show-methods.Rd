\docType{methods}
\name{show}
\alias{show}
\alias{show-methods}
\alias{show,modelNameClass-method}
\alias{show,modelVersionClass-method}
\alias{show,authorClass-method}
\alias{show,modelAuthorsClass-method}
\alias{show,modelSettingsClass-method}
\alias{show,basicsClass-method}
\alias{show,modelBasicsClass-method}
\alias{show,uncertClass-method}
\alias{show,modelUncertaintiesClass-method}
\alias{show,partClass-method}
\alias{show,modelPartsClass-method}
\alias{show,glossaryClass-method}
\alias{show,modelGlossaryClass-method}
\alias{show,modelAbbreviationsClass-method}
\alias{show,validationClass-method}
\alias{show,modelValidationClass-method}
\alias{show,modelReferencesClass-method}
\alias{show,modelCommentsClass-method}
\alias{show,modelConclusionsClass-method}
\alias{show,scoreClass-method}
\alias{show,modelScoringClass-method}
\alias{show,graphClass-method}
\alias{show,modelGraphsClass-method}
\alias{show,itemClass-method}
\alias{show,modelItemsClass-method}
\alias{show,modelOutputClass-method}
\alias{show,modelClass-method}


\title{show methods}

\description{show methods}

\usage{

\S4method{show}{modelNameClass}(object) 
\S4method{show}{modelVersionClass}(object)
\S4method{show}{authorClass}(object) 
\S4method{show}{modelAuthorsClass}(object)   
\S4method{show}{modelSettingsClass}(object)  
\S4method{show}{basicsClass}(object) 
\S4method{show}{modelBasicsClass}(object) 
\S4method{show}{uncertClass}(object)
\S4method{show}{modelUncertaintiesClass}(object) 
\S4method{show}{partClass}(object)   
\S4method{show}{modelPartsClass}(object)  
\S4method{show}{glossaryClass}(object) 
\S4method{show}{modelGlossaryClass}(object) 
\S4method{show}{modelAbbreviationsClass}(object)
\S4method{show}{validationClass}(object) 
\S4method{show}{modelReferencesClass}(object)   
\S4method{show}{modelCommentsClass}(object)  
\S4method{show}{modelConclusionsClass}(object) 
\S4method{show}{scoreClass}(object) 
\S4method{show}{modelScoringClass}(object) 
\S4method{show}{graphClass}(object) 
\S4method{show}{modelGraphsClass}(object) 
\S4method{show}{itemClass}(object) 
\S4method{show}{modelItemsClass}(object) 
\S4method{show}{modelOutputClass}(object) 
\S4method{show}{modelClass}(object) 

}

\arguments{

  \item{object}{a \code{modelNameClass}, \code{modelVersionClass}, \code{authorClass},
   \code{modelAuthorsClass}, \code{modelSettingsClass}, \code{basicsClass},
   \code{modelBasicsClass}, \code{uncertClass}, \code{modelUncertaintiesClass},
   \code{partClass}, \code{modelPartsClass}, \code{glossaryClass},
   \code{modelGlossaryClass}, \code{modelAbbreviationsClass}, \code{validationClass}, 
   \code{modelReferencesClass}, \code{modelCommentsClass}, \code{modelConclusionsClass},
   \code{scoreClass}, \code{modelScoringClass}, \code{graphClass},
   \code{modelGraphsClass}, \code{itemClass}, \code{modelItemsClass},
   \code{modelOutputClass} or \code{modelClass}  object}
}

\examples{
\donttest{
  # show methods for an empty model
  show(new("modelNameClass"))
  show(new("modelVersionClass"))
  show(new("authorClass"))
  show(new("modelAuthorsClass"))
  show(new("modelSettingsClass"))
  show(new("basicsClass"))
  show(new("modelBasicsClass"))
  show(new("uncertClass"))
  show(new("modelUncertaintiesClass"))
  show(new("partClass"))
  show(new("modelPartsClass"))
  show(new("glossaryClass"))
  show(new("modelGlossaryClass"))
  show(new("modelAbbreviationsClass"))
  show(new("validationClass"))
  show(new("modelValidationClass"))
  show(new("modelReferencesClass"))
  show(new("modelCommentsClass"))
  show(new("modelConclusionsClass")
  show(new("scoreClass")
  show(new("modelScoringClass"))
  new("graphClass")
  show(new("modelGraphsClass"))
  show(new("itemClass"))
  show(new("modelItemsClass"))
  show(new("modelOutputClass")) 
  show(new("modelClass"))
  
  # show methods for demo model
  rriskModel<-init.Model1()
  show(rriskModel@name)
  show(rriskModel@version)
  show(rriskModel@authors@authors[[1]])
  show(rriskModel@autors)
  show(rriskModel@settings)
  show(rriskModel@basics@basics[[3]])
  show(rriskModel@basics)
  show(rriskModel@uncertainties@uncertainties[[1]])
  show(rriskModel@uncertainties)
  show(rriskModel@parts@parts[[1]])
  show(rriskModel@parts)
  show(rriskModel@glossary@glossary[[1]])
  show(rriskModel@glossary)
  show(rriskModel@abbreviations)
  show(rriskModel@validation@validation[[1]])
  show(rriskModel@validation)
  show(rriskModel@references)
  show(rriskModel@comments)
  show(rrikModel@conclusions)
  show(rriskModel@scoring@scoring[[1]])
  show(rriskModel@scoring)
  show(rriskModel@graphs@graphs[[1]])
  show(rriskModel@graphs)
  show(rriskModel@items@items[[1]])
  show(rriskModel@items)
  show(rriskModel@output) 
  show(rriskModel)

}
}

