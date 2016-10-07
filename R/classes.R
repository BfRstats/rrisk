

################################################################################
################################################################################
#' S4 virtual class, a union of classes \code{numeric} and \code{NULL}.
#'
#' @name numericNULL-class
#' @aliases numericNULL
#' @docType class
#' @title S4 virtual class
#' @rdname numericNULL-class
#' @exportClass numericNULL

setClassUnion("numericNULL", c("numeric", "NULL"))

################################################################################
################################################################################
#' A S4 class for representing the names of \code{rrisk} models
#'
#' @name modelNameClass-class
#' @aliases modelNameClass
#' @docType class                                                                                              
#' @title S4 class for representing the names of 'rrisk' models
#' @slot name
#' @rdname modelNameClass-class
#' @exportClass modelNameClass
#' @examples
#' \donttest{new("modelNameClass")}

setClass(Class="modelNameClass",
  representation=representation(name="character"),
  prototype=prototype(name="new rrisk model"))
  
#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{modelNameClass}}
#'
#' @rdname show-methods
#' @aliases show                                         
#' @docType methods
#' @title Show method for 'modelNameClass'
#' @param object a \code{modelNameClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelNameClass"))}

setMethod(f="show",signature=signature(object="modelNameClass"),
  definition=function(object)
  { cat("=========================================================================\n")
    cat("Model name\n")
    cat("=========================================================================\n")
    cat(object@name,"\n\n")
  })

################################################################################
################################################################################
#' A S4 class for representing basic information of a \code{rrisk} model version
#'
#' @name modelVersionClass-class
#' @aliases modelVersionClass
#' @docType class
#' @title S4 class for representing 'rrisk' model version
#' @slot status
#' @slot minorupdate
#' @slot majorupdate
#' @slot subtitle
#' @slot editedby
#' @rdname modelVersionClass-class
#' @exportClass modelVersionClass
#' @examples
#' \donttest{new("modelVersionClass")
#'
#' new("modelVersionClass",status="Draft",minorupdate=0,majorupdate=1,
#' subtitle="Space for subtitle",editedby="Matthias Greiner")}

setClass(Class="modelVersionClass",
  representation=representation(
    status="character",
    minorupdate="numeric",
    majorupdate="numeric",
    subtitle="character",
    editedby="character"),
  prototype=prototype(
    status="Draft",
    minorupdate=0,
    majorupdate=0,
    subtitle="",
    editedby=""))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelVersionClass}}
#'
#' @rdname show-methods
#' @aliases show
#' @docType methods
#' @title Show method for 'modelVersionClass'
#' @param object a \code{modelVersionClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelVersionClass"))}

setMethod(f="show",signature=signature(object="modelVersionClass"),
  definition=function(object)
  { cat("*************************************************************************\n")
    cat("Model version\n")
    cat("*************************************************************************\n")
    cat("Status: ",object@status,"\n") 
    cat("Major update: ",object@majorupdate,"\n")
    cat("Minor update: ",object@minorupdate,"\n") 
    cat("Subtitle: ",object@subtitle,"\n")
    cat("Edited by: ",object@editedby,"\n")
    cat("\n")              
  })

################################################################################
################################################################################
#' S4 class represents information of an author of a \code{rrisk} model
#'
#' @name authorClass-class
#' @aliases authorClass
#' @docType class
#' @title S4 class for representing author's information
#' @slot name name
#' @slot institution institution
#' @slot email email address
#' @rdname authorClass-class
#' @exportClass authorClass
#' @examples
#' \donttest{new("authorClass")
#'
#' new("authorClass",name="Max Mustermann",institution="Example institution",
#' email="example@@email.com")}

setClass(Class="authorClass",
  representation=representation(
    name="character",
    institution="character",
    email="character"),
  prototype=prototype(
    name="",
    institution="",
    email=""))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{authorClass}}
#'
#' @rdname show-methods
#' @aliases show,authorClass-method
#' @docType methods
#' @title Show method for 'authorClass'
#' @param object a \code{authorClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("authorClass")) }

setMethod(f="show",signature=signature(object="authorClass"),
  definition=function(object)
  { cat("Name: ",object@name,"\n")
    cat("Institution: ",object@institution ,"\n")
    cat("Email: ",object@email ,"\n")
  })

################################################################################
################################################################################

#' S4 class for representing information of all the authors of a \code{rrisk} models
#'
#' @name modelAuthorsClass-class
#' @aliases modelAuthorsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model authors
#' @slot authors
#' @rdname modelAuthorsClass-class
#' @exportClass modelAuthorsClass
#' @examples
#' \donttest{new("modelAuthorsClass")
#'
#' new("modelAuthorsClass",authors=list(new("authorClass",name="Max Mustermann",
#' institution="Example institution",email="example@@email.com")))}

setClass(Class="modelAuthorsClass",
  representation=representation(authors="list"),
  prototype=prototype(authors=list()))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelAuthorsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelAuthorsClass-method
#' @docType methods
#' @title Show method for 'modelAuthorsClass'
#' @param object a \code{modelAuthorsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelAuthorsClass"))}

setMethod(f="show",signature=signature(object="modelAuthorsClass"),
  definition=function(object)
  { cat("*************************************************************************\n")
    cat("Model authors\n")
    cat("*************************************************************************\n")
    if(length(object@authors)>0)
    { for(i in 1:length(object@authors))
      { show(object@authors[[i]])
        cat("\n")
      }
    } else cat("Model authors list is empty \n\n")
  })


################################################################################
################################################################################
#' S4 class for representing information of all the default settings of a \code{rrisk} model
#'
#' @name modelSettingsClass-class
#' @aliases modelSettingsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model settings
#' @slot wscores
#' @slot N
#' @slot Ntest
#' @slot N2d
#' @slot stress
#' @slot mycol
#' @slot nwlayout
#' @slot usenotapplicable
#' @slot coverheader
#' @slot trans
#' @slot sens
#' @slot sty
#' @slot deleteTeX
#' @slot abserror
#' @rdname modelSettingsClass-class
#' @exportClass modelSettingsClass
#' @examples
#' \donttest{show(new("modelSettingsClass"))}

setClass(Class="modelSettingsClass",
  representation=representation(
    wscores="character",
    N="numeric",
    Ntest="numeric",
    N2d="numeric",
    stress="numeric",
    mycol="character",
    nwlayout="character",
    usenotapplicable="logical",
    coverheader="character",
    trans="character",
    sens="character",
    sty="character",
    deleteTeX="logical",
    abserror="numeric"),
  prototype=prototype(
    wscores="equal",
    N=1000,
    Ntest=100,
    N2d=50,
    stress=90,
    mycol="lightblue",
    nwlayout="kamadakawai",
    usenotapplicable=TRUE,
    coverheader="model network",
    trans="rank",
    sens="correlation", 
    sty="Sweave",
    deleteTeX=TRUE,
    abserror=0.0001))
    
#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{modelSettingsClass}}
#'
#' @rdname show-methods
#' @aliases show, modelSettingsClass-method
#' @docType methods
#' @title Show method for 'modelSettingsClass'
#' @param object a \code{modelSettingsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelSettingsClass"))}

setMethod(f="show",signature=signature(object="modelSettingsClass"),
  definition=function(object)
  { cat("*************************************************************************\n")
    cat("Model settings\n")
    cat("*************************************************************************\n") 
    cat(paste("Number of iterations (1st dimension): ",object@N,"\n"))
    cat(paste("Number of iterations (2nd dimension): ",object@N2d,"\n"))
    cat(paste("Number of test iterations: ",object@Ntest,"\n"))
    cat(paste("Stress test percentile: ",object@stress,"\n"))
    cat(paste("Absolute error: ",object@abserror,"\n"))
    cat(paste("Weights for qualitative scores: ",object@wscores,"\n"))
    cat(paste("Standard color for graphics: ",object@mycol,"\n"))
    cat(paste("Layout for model graph visualisation: ",object@nwlayout,"\n"))
    cat(paste("Use 'not applicable' by displaying model uncertainties : ",object@usenotapplicable,"\n"))
    cat("Optional laTeX style files: ",paste(object@sty,collapce=" "),"\n")
    cat(paste("Protect source file: ",object@deleteTeX,"\n"))
    cat(paste("Cover picture for model report: ",object@coverheader,"\n"))
    cat(paste("Transformation for sensitivity analysis: ",object@trans,"\n"))
    cat(paste("Model for sensitivity analysis: ",object@sens,"\n"))
    cat("\n")
  })
  

################################################################################
################################################################################

#' S4 class for representing basic descriptions of a \code{rrisk} model
#'
#' @name basicsClass-class
#' @aliases basicsClass
#' @docType class
#' @title S4 class for representing basic descriptions
#' @slot name
#' @slot explanation
#' @rdname basicsClass-class
#' @exportClass basicsClass
#' @examples
#' \donttest{new("basicsClass")
#'
#' new("basicsClass",
#' name="Background",
#' explanation=gsub(x="Escherichia (E.) coli O157:H7 represents a microbial 
#' hazard in food stuffs such as ground beef. It is known that good kitchen
#' hygiene and proper cooking reduces the risk of food-borne outbreaks
#' caused by this bacterium. A model for assessing the risk of illness
#' in children less than...","\n",replacement=""))}

setClass(Class="basicsClass",
  representation=representation(
    name="character",
    explanation="character"),
  prototype=prototype(
    name="",
    explanation=""))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{basicsClass}}
#'
#' @rdname show-methods
#' @aliases show, basicsClass-method
#' @docType methods
#' @title Show method for 'basicsClass'
#' @param object a \code{basicsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("basicsClass"))}

setMethod(f="show",signature=signature(object="basicsClass"),
  definition=function(object)
  { cat("Name: ",object@name,"\n")
    cat("Explanation: ",putLineBreaks(object@explanation) ,"\n")
  })
  
################################################################################
################################################################################

#' S4 class for representing basic descriptions of a \code{rrisk} model
#'
#' @name modelBasicsClass-class
#' @aliases modelBasicsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model basic descriptions
#' @slot basics
#' @rdname modelBasicsClass-class
#' @exportClass modelBasicsClass
#' @examples
#' \donttest{new("modelBasicsClass")}

setClass(Class="modelBasicsClass",
  representation=representation(basics="list"),
  prototype=prototype(
  basics=list(
    new("basicsClass",name="mainDoc",explanation="Please enter here a reference to the main document (e.g., project report) in which the model is described..."),
    new("basicsClass",name="Background",explanation="Please write here about the background of the model..."),
    new("basicsClass",name="Objectives",explanation="Please write here about the purpose and the objectives of the model..."),
    new("basicsClass",name="Scope",explanation="Please write here about the scope and limitations of the scope..."),
    new("basicsClass",name="Description",explanation="Please write here the model itself (type of model, structure (division into parts),the processes modeled and which endpoint(s) will be simulated by the model)...")
  )))

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelBasicsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelBasicsClass-method
#' @docType methods
#' @title Show method for 'modelBasicsClass'
#' @param object a \code{modelBasicsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelBasicsClass"))}

setMethod(f="show",signature=signature(object="modelBasicsClass"),
  definition=function(object)
  { cat("*************************************************************************\n")
    cat("Basic descriptions\n")
    cat("*************************************************************************\n")
    if(length(object@basics)>0)
    { for(i in 1:length(object@basics))
      { show(object@basics[[i]])
        cat("\n")
      }
    } else
    { cat("Model basics list is empty \n\n")
    } 
  })


################################################################################
################################################################################

#' S4 class for representing descriptions of an uncertainty of a \code{rrisk} model
#'
#' @name uncertClass-class
#' @aliases uncertClass
#' @docType class
#' @title S4 class for representing model uncertainty item
#' @slot namemain main category of uncertainty
#' @slot namesub sub category of uncertainty
#' @slot explanation
#' @slot scores
#' @rdname uncertClass-class
#' @exportClass uncertClass
#' @examples
#' \donttest{new("uncertClass")}

setClass(Class="uncertClass",
  representation=representation(
    namemain="character",
    namesub="character",
    explanation="character",
    scores="numericNULL"),
  prototype=prototype(
    namemain="",
    namesub="",
    explanation="",
    scores=c()))
    
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{uncertClass}}
#'
#' @rdname show-methods
#' @aliases show,uncertClass-method
#' @docType methods
#' @title Show method for 'uncertClass'
#' @param object a \code{uncertClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("uncertClass"))}

setMethod(f="show",signature=signature(object = "uncertClass"),
  definition = function(object)
  { cat("Main category: ",object@namemain,"\n")
    cat("Sub category: ",object@namesub,"\n")
    cat("Explanation: ",putLineBreaks(object@explanation) ,"\n")
    cat("Scores:",object@scores,"\n")
  })
  
################################################################################
################################################################################

#' S4 class for representing descriptions of all the uncertainties of a \code{rrisk} 
#' model
#'
#' @name modelUncertaintiesClass-class
#' @aliases modelUncertaintiesClass
#' @docType class
#' @title S4 class for representing 'rrisk' model uncertainties
#' @slot note
#' @slot uncertainties
#' @rdname modelUncertaintiesClass-class
#' @exportClass modelUncertaintiesClass
#' @examples
#' \donttest{new(Class="modelUncertaintiesClass")}

setClass(Class="modelUncertaintiesClass",
  representation=representation(
    note="character",
    uncertainties="list"),
  prototype=prototype(
    note="",
    uncertainties=list()))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelUncertaintiesClass}}
#'
#' @rdname show-methods
#' @aliases show,modelUncertaintiesClass-method
#' @docType methods
#' @title Show method for 'modelUncertaintiesClass'
#' @param object a \code{modelUncertaintiesClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new(Class="modelUncertaintiesClass"))}

setMethod(f="show",signature=signature(object="modelUncertaintiesClass"),
  definition=function(object)
  { cat("*************************************************************************\n")
    cat("Model uncertainties\n")
    cat("*************************************************************************\n")
    if(!(object@note=="" & length(object@uncertainties)==0))
    { cat("Note: ",putLineBreaks(object@note),"\n\n")
      if(length(object@uncertainties)>0)
      { for(i in 1:length(object@uncertainties))
        { show(object@uncertainties[[i]])
          cat("\n")
        }
      } else cat("Uncertainties: \n\n")
    } else
    { cat("Model uncertainties list is empty \n\n")
    }
  })
  

################################################################################
################################################################################

#' S4 class for representing descriptions of a part of a \code{rrisk} model
#'
#' @name partClass-class
#' @aliases partClass
#' @docType class
#' @title S4 class for representing model part item
#' @slot name
#' @slot explanation
#' @slot items
#' @rdname partClass-class
#' @exportClass partClass
#' @examples
#' \donttest{new("partClass")}

setClass(Class="partClass",
  representation=representation(
    name="character",
    explanation="character",
    items="character"),
  prototype=prototype(
    name="",
    explanation="",
    items=""))
    
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{partClass}}
#'
#' @rdname show-methods
#' @aliases show,partClass-method
#' @docType methods
#' @title Show method for 'partClass'
#' @param object a \code{partClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("partClass"))}

setMethod(f="show",signature=signature(object="partClass"),
  definition=function(object)
  { cat("Name: ",object@name,"\n")
    cat("Explanation: ",putLineBreaks(object@explanation) ,"\n")
    cat("Items:",object@items,"\n")    
  })
  
################################################################################
################################################################################

#' S4 class for representing descriptions of all the parts of a \code{rrisk} model
#'
#' @name modelPartsClass-class
#' @aliases modelPartsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model parts
#' @slot parts
#' @rdname modelPartsClass-class
#' @exportClass modelPartsClass
#' @examples
#' \donttest{new("modelPartsClass")}

setClass(Class="modelPartsClass",
  representation=representation(parts="list"),
  prototype=prototype(parts=list()))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelPartsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelPartsClass-method
#' @docType methods
#' @title Show method for 'modelPartsClass'
#' @param object a \code{modelPartsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelPartsClass"))}

setMethod(f="show",signature=signature(object="modelPartsClass"),
  definition=function(object)
  {
    cat("*************************************************************************\n")
    cat("Model parts\n")
    cat("*************************************************************************\n")
    if(length(object@parts)>0)
    { for(i in 1:length(object@parts))
      { show(object@parts[[i]])
        cat("\n")
      }
    } else cat("Model parts list is empty\n\n")
  })

################################################################################
################################################################################

#' S4 class for representing a glossary of a \code{rrisk} model
#'
#' @name glossaryClass-class
#' @aliases glossaryClass
#' @docType class
#' @title S4 class for representing model glossary entry
#' @slot name
#' @slot explanation
#' @rdname glossaryClass-class
#' @exportClass glossaryClass
#' @examples
#' \donttest{new("glossaryClass")}

setClass(Class="glossaryClass",
  representation=representation(
    name="character",
    explanation="character"),
  prototype=prototype(
    name="",
    explanation=""))
    
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{glossaryClass}}
#'
#' @rdname show-methods
#' @aliases show,glossaryClass-method
#' @docType methods
#' @title Show method for 'glossaryClass'
#' @param object a \code{glossaryClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("glossaryClass"))}

setMethod(f="show",signature=signature(object="glossaryClass"),
  definition=function(object)
  { if(nchar(object@name)>0 & nchar(putLineBreaks(object@explanation)>0))
    { cat(object@name," - ",putLineBreaks(object@explanation) ,"\n")
    } else print("Empty glossary entry")
  })

################################################################################
################################################################################

#' S4 class for representing all the glossaries of a \code{rrisk} model
#'
#' @name modelGlossaryClass-class
#' @aliases modelGlossaryClass
#' @docType class
#' @title S4 class for representing 'rrisk' model glossary list
#' @slot glossary
#' @rdname modelGlossaryClass-class
#' @exportClass modelGlossaryClass
#' @examples
#' \donttest{new("modelGlossaryClass")}

setClass(Class="modelGlossaryClass",
  representation=representation(glossary="list"),
  prototype=prototype(glossary=list()))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelGlossaryClass}}
#'
#' @rdname show-methods
#' @aliases show,modelGlossaryClass-method
#' @docType methods
#' @title Show method for 'modelGlossaryClass'
#' @param object a \code{modelGlossaryClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelGlossaryClass"))}

setMethod(f="show",signature=signature(object="modelGlossaryClass"),
  definition=function(object)
  {
    cat("*************************************************************************\n")
    cat("Glossary\n")
    cat("*************************************************************************\n")
    if(length(object@glossary)>0)
    { for(i in 1:length(object@glossary))
      { show(object@glossary[[i]])
        cat("\n")
      }
    } else cat("Model glossary list is empty\n\n")
  })


################################################################################
################################################################################

#' S4 class for representaing all the abbreviations uesed in a \code{rrisk} model
#'
#' @name modelAbbreviationsClass-class
#' @aliases modelAbbreviationsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model abbreviations list
#' @slot abbreviations
#' @rdname modelAbbreviationsClass-class
#' @exportClass modelAbbreviationsClass
#' @examples
#' \donttest{new("modelAbbreviationsClass")
#'
#' new("modelAbbreviationsClass",
#' abbreviations=list(
#' new("glossaryClass",name="cfu", explanation="colony forming units"),
#' new("glossaryClass",name="cfu", explanation="colony forming units")))}
  
setClass(Class="modelAbbreviationsClass",
  representation=representation(abbreviations="list"),
  prototype=prototype(abbreviations=list()))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelAbbreviationsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelAbbreviationsClass-method
#' @docType methods
#' @title Show method for 'modelAbbreviationsClass'
#' @param object a \code{modelAbbreviationsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelAbbreviationsClass"))}

setMethod(f="show",signature=signature(object="modelAbbreviationsClass"),
  definition=function(object)
  {
    cat("*************************************************************************\n")
    cat("Abbreviations\n")
    cat("*************************************************************************\n")
    if(length(object@abbreviations)>0)
    { for(i in 1:length(object@abbreviations))
      { show(object@abbreviations[[i]])
        cat("\n")
      }
    } else cat("Model abbreviations list is empty\n\n")
  })


################################################################################
################################################################################
#' A S4 class for representing descriptions of a validity of a \code{rrisck} model
#'
#' @name validationClass-class
#' @aliases validationClass
#' @docType class
#' @title S4 class for representing model validity entry
#' @slot name
#' @slot explanation
#' @rdname validationClass-class
#' @exportClass validationClass
#' @examples
#' \donttest{new("validationClass")
#'
#' new("validationClass",
#' name="Model concept",
#' explanation=gsub(x="The model concept is a reasonable representation of
#' the key processes that are to be considered to answer the risk
#' question. The concept has been verified from a microbiological
#' and modelling viewpoint.","\n",replacement=""))}

setClass(Class="validationClass",
  representation=representation(
    name="character",
    explanation="character"),
  prototype=prototype(
    name="",
    explanation=""))
    
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{validationClass}}
#'
#' @rdname show-methods
#' @aliases show,validationClass-method
#' @docType methods
#' @title Show method for 'validationClass'
#' @param object a \code{validationClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("validationClass"))}

setMethod(f="show",signature=signature(object="validationClass"),
  definition=function(object)
  { cat("Name: ",object@name,"\n")
    cat("Explanation: ",putLineBreaks(object@explanation),"\n")
  })

################################################################################
################################################################################
#' S4 class for representing all the validities of a \code{rrisk} model
#'
#' @name modelValidationClass-class
#' @aliases modelValidationClass
#' @docType class
#' @title S4 class for representing 'rrisk' model validities
#' @slot validation
#' @rdname modelValidationClass-class
#' @exportClass modelValidationClass
#' @examples
#' \donttest{new("modelValidationClass")
#'
#' new("modelValidationClass",
#' validation=list(
#' new("validationClass",name="Model concept",
#' explanation=gsub(x="The model concept is a reasonable representation of 
#' the key processes that are to be considered to answer the risk question. 
#' The concept has been verified from a microbiological and modelling 
#' viewpoint.","\n",replacement="")),
#' new("validationClass",
#' name="Model implementation and verification",
#' explanation=gsub(x="The correctness of the translation of the model
#' concept into the mathematical model has been checked. 
#' The appropriateness of the model parameterisation---and in 
#' particular of the choice of distributions for stochastic model 
#' input---has been commented in the item descriptions. The unit of 
#' observation of each model input quantity was carefully considered 
#' when combining inputs by mathematical expressions...","\n",
#' replacement="")))) }

setClass(Class="modelValidationClass",
  representation=representation(validation="list"),
  prototype=prototype(validation=list()))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelValidationClass}}
#'
#' @rdname show-methods
#' @aliases show,modelValidationClass-method
#' @docType methods
#' @title Show method for 'modelValidationClass'
#' @param object a \code{modelValidationClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelValidationClass"))}

setMethod(f="show",signature=signature(object="modelValidationClass"),
  definition=function(object)
  { cat("*************************************************************************\n")
    cat("Validation\n")
    cat("*************************************************************************\n")
    if(length(object@validation)>0)
    { for(i in 1:length(object@validation))
      { show(object@validation[[i]])
        cat("\n")
      }
    } else cat("Model validation list is empty\n\n")
  })

################################################################################
################################################################################
#' S4 class for representing all the references of a \code{rrisk} model
#'
#' @name modelReferencesClass-class
#' @aliases modelReferencesClass
#' @docType class
#' @title S4 class for representing 'rrisk' modeo references
#' @slot references
#' @rdname modelReferencesClass-class
#' @exportClass modelReferencesClass
#' @examples
#'  \donttest{new("modelReferencesClass")}

setClass(Class="modelReferencesClass",
  representation=representation(references="character"),
  prototype=prototype(references=""))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelReferencesClass}}
#'
#' @rdname show-methods
#' @aliases show,modelReferencesClass-method
#' @docType methods
#' @title Show method for 'modelReferencesClass'
#' @param object a \code{modelReferencesClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelReferencesClass"))}

setMethod(f="show",signature=signature(object="modelReferencesClass"),
  definition=function(object)
  {
    cat("*************************************************************************\n")
    cat("References\n")
    cat("*************************************************************************\n")
    if(length(object@references)>1)
    { temp<-object@references
      output<-""
      for(i in 1:length(temp))
      { temp.i<-temp[i]
        if(temp.i!="") temp.i<-paste(putLineBreaks(temp.i),"\n",sep="")
        if(temp.i=="") temp.i<-"\n"
        output<-paste(output,temp.i,sep="")   
      }
      cat(output,"\n")
    } else cat("empty\n\n")
  })


################################################################################
################################################################################
#' S4 class for representing all the comments of a \code{rrisk} model
#'
#' @name modelCommentsClass-class
#' @aliases modelCommentsClass
#' @docType class
#' @title S4 class for representing all 'rrisk' model comments
#' @slot comments
#' @rdname modelCommentsClass-class
#' @exportClass modelCommentsClass
#' @examples
#'  \donttest{new("modelCommentsClass")
#'
#' new("modelCommentsClass",
#' comments=list(gsub(x="The structure of the model follows the scenario 
#' pathway. The grouping of items into parts could be re-arranged to 
#' be consistent with the structure of risk assessments according to 
#' Codex Alimentarius if necessary.","\n",replacement=""),
#' gsub(x="The sensitivity analysis indicates that among the uncertain model 
#' inputs, the probability of illness due to exposure to a single 
#' E.~coli cell ($r$) has a great impact on the outcome risk estimate.
#' The variability of the outcome is largely dependent on the number 
#' of viable bacteria in the dish at time of consumption ($n$)...",
#' "\n",replacement=""))) }

setClass(Class="modelCommentsClass",
  representation=representation(comments="list"),
  prototype=prototype(comments=list()))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelCommentsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelCommentsClass-method
#' @docType methods
#' @title Show method for 'modelCommentsClass'
#' @param object a \code{modelCommentsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelCommentsClass"))}

setMethod(f="show",signature=signature(object="modelCommentsClass"),
  definition=function(object)
  {
    cat("*************************************************************************\n")
    cat("Comments\n")
    cat("*************************************************************************\n")
    if(length(object@comments)>0)
    { for(i in 1:length(object@comments))
      { cat(putLineBreaks(object@comments[[i]]),"\n\n")
      }
    } else cat("Mode comments list is empty\n\n")
  })


################################################################################
################################################################################
#' S4 class for representing all the conclusions of a \code{rrisk} model
#'
#' @name modelConclusionsClass-class
#' @aliases modelConclusionsClass
#' @docType class
#' @title S4 class for representing all 'rrisk' model conclusions
#' @slot conclusions
#' @rdname modelConclusionsClass-class
#' @exportClass modelConclusionsClass
#' @examples
#'  \donttest{new("modelConclusionsClass")
#'
#' new("modelConclusionsClass",
#' conclusions=list(gsub(x="This illustrative model predicts that the population
#' average probability of illness due to E.coli O157:H7 in child below 
#' age of three years attributable to consumption of one serving prepared 
#' from ground beef is around...","\n",replacement=""),
#' gsub(x="The probabilistic modelling approach indicates that, taking into 
#' account variability and uncertainty, this probability could be much 
#' higher in rare cases...","\n",replacement=""))) }

setClass(Class="modelConclusionsClass",
  representation=representation(conclusions="list"),
  prototype=prototype(conclusions=list()))
  
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelConclusionsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelConclusionsClass-method
#' @docType methods
#' @title Show method for 'modelConclusionsClass'
#' @param object a \code{modelConclusionsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelConclusionsClass"))
#'
#' new("modelConclusionsClass",
#' conclusions=list(gsub(x="This illustrative model predicts that the population
#' average probability of illness due to E.coli O157:H7 in child below 
#' age of three years attributable to consumption of one serving prepared 
#' from ground beef is around...","\n",replacement=""),
#' gsub(x="The probabilistic modelling approach indicates that, taking into 
#' account variability and uncertainty, this probability could be much 
#' higher in rare cases...","\n",replacement="")))}

setMethod(f="show",signature=signature(object="modelConclusionsClass"),
  definition=function(object)
  { cat("*************************************************************************\n")
    cat("Conclusions\n")
    cat("*************************************************************************\n")
    if(length(object@conclusions)>0)
    { for(i in 1:length(object@conclusions))
      { cat(putLineBreaks(object@conclusions[[i]]),"\n\n")
      }
    } else cat("Model conclusions list is empty\n\n")
  })


################################################################################
#' S4 class for representing information of a scoring system of a \code{rrisk} model
#'
#' @name scoreClass-class
#' @aliases scoreClass
#' @docType class
#' @title S4 class for representing model score items
#' @slot notation
#' @slot name
#' @slot explanation
#' @slot values
#' @rdname scoreClass-class
#' @exportClass scoreClass
#' @examples
#'  \donttest{new("scoreClass")}

setClass(Class="scoreClass",
  representation=representation(
    notation="character",
    name="character",
    explanation="character"),
  prototype=prototype(
    notation="",
    name="",
    explanation=""))
  
#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{scoreClass}}
#'
#' @rdname show-methods
#' @aliases show, scoreClass-method
#' @docType methods
#' @title Show method for 'scoreClass'
#' @param object a \code{scoreClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("scoreClass"))
#'
#' new(Class="scoreClass",
#' notation="U1",
#' name="Plausibility",
#' values=c(0,1,2,3),
#' explanation=gsub(x="1=high (highly plausible despite absence of factual
#' evidence), 2=medium (plausible despite absence of factual evidence),
#' 3=low (plausibility questionable or not assessed;
#' fictive or speculative assertion)","\n",replacement=""))}

setMethod(f="show",signature=signature(object="scoreClass"),
  definition=function(object)
  { cat("Notation: ",object@notation,"\n")
    cat("Name: ", object@name,"\n")
    cat("Explanation: ",putLineBreaks(object@explanation),"\n")  
  })

################################################################################
################################################################################
#' S4 class for representing scoring systems of \code{rrisk} models
#'
#' @name modelScoringClass-class
#' @aliases modelScoringClass
#' @docType class
#' @title S4 class for representing 'rrisk' model scoring system
#' @slot name
#' @slot tableheader
#' @slot explanatory
#' @slot values
#' @slot vcolors
#' @slot vmeanings
#' @slot systemtype
#' @slot scoring
#' @rdname modelScoringClass-class
#' @exportClass modelScoringClass
#' @examples
#'  \donttest{new("modelScoringClass")}

setClass(Class="modelScoringClass",
  representation=representation(
    name="character",
    tableheader="character",
    explanatory="character",
    values="numeric",
    vcolors="numeric",
    vmeanings="numeric",
    systemtype="character",
    scoring="list"),
  prototype=prototype(
    name="empty scoring system",
    tableheader="",
    explanatory="",
    values=c(),
    vcolors=c(),
    vmeanings=c(),
    systemtype="user defined",
    scoring=list()))
  
#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{modelScoringClass}}
#'
#' @rdname show-methods
#' @aliases show,modelScoringClass-method
#' @docType methods
#' @title Show method for 'modelScoringClass'
#' @param object a \code{modelScoringClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelScoringClass"))}

setMethod(f="show",signature=signature(object="modelScoringClass"),
  definition=function(object)
  {
    cat("*************************************************************************\n")
    cat("Scoring system\n")
    cat("*************************************************************************\n")
    cat("Name: ", object@name,"\n")
    cat("System type: ", object@systemtype,"\n")
    cat("Table header: ",putLineBreaks(object@tableheader),"\n")
    cat("Explanatory: ",putLineBreaks(object@explanatory),"\n")
    cat("Values: ",object@values,"\n")
    #---------------------------------------------------------------------------
    if(length(object@vcolors)>0)
    { for(i in 1:length(object@vcolors))
      { if(i==1) vcolors<-c()
        vcolorsTemp<-paste(object@vcolors[i],"=",names(object@vcolors)[i],sep="")
        vcolors<-c(vcolors,vcolorsTemp)
      }
    } else vcolors=""
    cat("Colors: ",vcolors,"\n")
    #---------------------------------------------------------------------------
    if(length(object@vmeanings)>0)
    { for(i in 1:length(object@vmeanings))
      { if(i==1) vmeanings<-c()
        vmeaningsTemp<-paste(object@vmeanings[i],"=",names(object@vmeanings)[i],sep="")
        vmeanings<-c(vmeanings,vmeaningsTemp)
      }
    } else vmeanings=""
    cat("Meanings: ",vmeanings,"\n\n")
    #---------------------------------------------------------------------------
    if(length(object@scoring)>0)
    { for(i in 1:length(object@scoring))
      { show(object@scoring[[i]])
        cat("\n")
      }
    } 
  })


################################################################################
################################################################################
#' S4 class for representing a graph used in a \code{rrisk} model
#'
#' @name graphClass-class
#' @aliases graphClass
#' @docType class
#' @title S4 class for representing model graph object
#' @slot notation
#' @slot explanation
#' @slot cover
#' @slot graphdata
#' @rdname graphClass-class
#' @exportClass graphClass
#' @examples
#'  \donttest{new("graphClass")}

setClass(Class="graphClass",
  representation=representation(
    #notation="character",
    explanation="character",
    cover="logical",
    graphdata="ANY"),
  prototype=prototype(
    #notation="",
    explanation="",
    cover=TRUE,
    graphdata=NULL))
#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{graphClass}}
#'
#' @rdname show-methods
#' @aliases show, graphClass-method
#' @docType methods
#' @title Show method for 'graphClass'
#' @param object a \code{graphClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("graphClass"))}

setMethod(f="show",signature=signature(object="graphClass"),
  definition=function(object)
  { #cat("Notation: ",object@notation,"\n")
    cat("Explanation: ",putLineBreaks(object@explanation),"\n")
    cat("Cover: ",object@cover,"\n")
    cat("Graph data: ",ifelse(!is.null(object@graphdata),"graph data available","no graph data available"),"\n")
  })


################################################################################
################################################################################
#' S4 class for representing all the graphs used in \code{rrisk} models
#'
#' @name modelGraphsClass-class
#' @aliases modelGraphsClass
#' @docType class
#' @title S4 class for representing all 'rrisk' model graphs objects
#' @slot graphs
#' @rdname modelGraphsClass-class
#' @exportClass modelGraphsClass
#' @examples
#'  \donttest{new("modelGraphsClass")}

setClass(Class="modelGraphsClass",
  representation=representation(graphs="list"),
  prototype=prototype(graphs=list()))
  
#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{modelGraphsClass}}
#'
#' @rdname show-methods
#' @aliases show, modelGraphsClass-method
#' @docType methods
#' @title Show method for 'modelGraphsClass'
#' @param object a \code{modelGraphsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelGraphsClass"))}

setMethod(f="show",signature=signature(object="modelGraphsClass"),
  definition=function(object)
  {
    cat("*************************************************************************\n")
    cat("Model graphs\n")
    cat("*************************************************************************\n")
    if(length(object@graphs)>0)
    { for(i in 1:length(object@graphs))
      { show(object@graphs[[i]])
        cat("\n")
      }
      #temp<-new("modelClass")
      #temp@graphs<-object  
      #displayConceptGraphs(temp)
    } else cat("Model graphs list is empty\n\n")
  })
  
  
################################################################################
################################################################################
#' S4 class for representing an Item of \code{rrisk} models
#'
#' @name itemClass-class
#' @aliases itemClass
#' @docType class
#' @title S4 class for representing model items
#' @slot part
#' @slot name
#' @slot title
#' @slot stratum
#' @slot explanation
#' @slot type
#' @slot typecode
#' @slot stratumevaluated
#' @slot data
#' @slot definition
#' @slot depitem
#' @slot unit
#' @slot role
#' @slot rolecode
#' @slot plausimin
#' @slot plausimax
#' @slot scores
#' @slot assumptions
#' @slot remark
#' @slot reference
#' @slot fullc
#' @slot relaxc
#' @slot fullcommand
#' @rdname itemClass-class
#' @exportClass itemClass
#' @examples
#' \donttest{new("itemClass")}

setClass(Class="itemClass",
  representation=representation(
    part="character",
    name="character",
    title="character",
    stratum="character",
    explanation="character",
    type="character",
    typecode="character",
    stratumevaluated="ANY",
    data="ANY",
    definition="character",
    depitem="character",
    unit="character",
    role="character",
    rolecode="character",
    plausimin="numericNULL",
    plausimax="numericNULL",
    scores="numericNULL",
    assumptions="character",
    remark="character",
    reference="character",
    fullc="character",
    relaxc="character",                               
    fullcommand="numericNULL", 
    relaxcommand="numericNULL"),
  prototype=prototype(
    part="",
    name="",
    title="",
    stratum="",
    stratumevaluated="",
    explanation="",
    type="",
    typecode="",
    data=NULL,
    definition="",
    depitem="",
    unit="",
    role="Not defined (nd)",
    rolecode="nd",
    plausimin=NULL,
    plausimax=NULL,
    scores=NULL,
    assumptions="",
    remark="",
    reference="",
    fullc="",
    relaxc="", 
    fullcommand=NULL, 
    relaxcommand=NULL))
    
#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{itemClass}}
#'
#' @rdname show-methods
#' @aliases show, itemClass-method
#' @docType methods
#' @title Show method for 'itemClass'
#' @param object a \code{itemClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("itemClass"))}

setMethod(f="show",signature=signature(object="itemClass"),
  definition=function(object)
  { cat("Name:\t\t",object@name,"\n") 
    cat("Part:\t\t",object@part,"\n") 
    cat("Title:\t\t",object@title,"\n")
    cat("Stratum:\t",object@stratum,"\n")
    cat("Type:\t\t",object@type,"\n")
    cat("Type code:\t",object@typecode,"\n")
    if(object@typecode=="stra") cat("Stratum evaluated:\t",object@stratumevaluated,"\n")
    cat("Data:\n")
    if(!is.null(object@data) & object@typecode=="numv")
    { print(object@data)
    } else if(!is.null(object@data) & object@typecode=="mcrv") {
      print(object@data)
    } else if(!is.null(object@data) & object@typecode=="fnrv"){
      print(object@data) 
    } else if(!is.null(object@data) & object@typecode=="stra") {
      print(object@data)
    } else if (!is.null(object@data) & object@typecode=="rsrv"){
      print(object@data)
    } else if(!is.null(object@data) & object@typecode=="bsrv"){
      print(object@data)
    } else if (!is.null(object@data) & object@typecode=="data"){
      print(head(object@data))
      cat("...\n")
      print(summary(object@data))
    } else if(!is.null(object@data) & object@typecode=="fdoc"){
      print(object@data)
    } else if (!is.null(object@data) & object@typecode=="bdjp"){
      print(object@data)
    } else if (!is.null(object@data) & object@typecode=="mxrv"){
      print(object@data)
    } 
    cat("Definition:\t",object@definition,"\n")
    cat("Dependent items:",object@depitem,"\n")
    cat("Unit:\t\t",object@unit,"\n")
    cat("Role:\t\t",object@role,"\n")
    cat("Role code:\t",object@rolecode,"\n")
    cat(paste("Plausimin:\t",object@plausimin,"\n",sep=""))
    cat(paste("Plausimax:\t",object@plausimax,"\n",sep=""))
    if(is.null(object@scores))
    { cat("Scores:\t\t\n")
    } else cat("Scores:\t\t",object@scores,"\n")
    cat("Explanation:\t",putLineBreaks(object@explanation),"\n")
    cat("Assumptions:\t",putLineBreaks(object@assumptions),"\n")
    cat("Remark:\t\t",putLineBreaks(object@remark),"\n")
    cat("Reference:\t",object@reference,"\n")
    cat(paste("Full command:\t",object@fullc,"\n",sep=""))
    cat(paste("Relax command:\t",object@relaxc,"\n",sep=""))
    # cat(paste("Full command after run:\t\t", as.character(object@fullcommand), "\n", sep = ""))
  })
  
  
################################################################################
################################################################################
#' S4 class for representing all the Items of \code{rrisk} models
#'
#' @name modelItemsClass-class
#' @aliases modelItemsClass
#' @docType class
#' @title S4 class for representing all 'rrisk' model items
#' @slot items
#' @rdname modelItemsClass-class
#' @exportClass modelItemsClass
#' @examples
#'  \donttest{new("modelItemsClass")}

setClass(Class="modelItemsClass",
  representation=representation(items="list"),          
  prototype=prototype(items=list()))
  
#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{modelItemsClass}}
#'
#' @rdname show-methods
#' @aliases show, modelItemsClass-method
#' @docType methods
#' @title Show method for 'modelItemsClass'
#' @param object a \code{modelItemsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelItemsClass"))}

setMethod(f="show",signature=signature(object="modelItemsClass"),
  definition=function(object)
  { cat("*************************************************************************\n")
    cat("Model items\n")
    cat("*************************************************************************\n")
    if(length(object@items)>0)
    { for(i in 1:length(object@items))
      { show(object@items[[i]])
        if(i!=length(object@items))
        {cat("-----------------------------------------------------------------------------\n")
        } else cat("\n")
      }
    } else cat("Model items list is empty\n\n")
  })
  
  
################################################################################
################################################################################
#' S4 class for representing \code{rrisk} modelOutputClass
#'
#' @name modelOutputClass-class
#' @aliases modelOutputClass
#' @docType class
#' @title S4 class for representing 'rrisk' modelOutputClass
#' @slot fullout.1d  list-object
#' @slot relaxout.1d list-object
#' @slot fullout2d character-vector
#' @slot uncitems.2d character-vector
#' @slot OFname.2d character
#' @slot summaries character-vector
#' @slot runtime1d numeric
#' @slot runtime2d numeric
#' @slot OFcdfCI numeric
#' @rdname modelOutputClass-class
#' @exportClass modelOutputClass
#' @examples
#'  \donttest{new("modelOutputClass")}

setClass(Class="modelOutputClass",
  representation=representation(
    fullout.1d="list",
    relaxout.1d="list",
    fullout.2d="ANY",
    uncitems.2d="ANY",
    OFname.2d="character",
    summaries="ANY",
    runtime1d="numericNULL",
    runtime2d="numericNULL", 
    OFcdfCI = "ANY"),
  prototype=prototype(
    fullout.1d=list(),
    relaxout.1d=list(),
    fullout.2d=c(),
    uncitems.2d=c(),
    OFname.2d="",
    summaries=c(),
    runtime1d=NULL,
    runtime2d=NULL, 
    OFcdfCI=c()))  
    
#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{modelOutputClass}}
#'
#' @rdname show-methods
#' @aliases show,modelOutputClass-method
#' @docType methods
#' @title Show method for 'modelOutputClass'
#' @param object a \code{modelOutputClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelOutputClass"))    }
 
setMethod(f = "show", 
  signature = signature(object = "modelOutputClass"), 
  definition = function(object)
  { cat("*************************************************************************\n")
    cat("Model run results \n")
    cat("*************************************************************************\n")
    cat("\nEvaluated outcome function(s)\n")
    show(object@summaries)
    cat("\n")
    if(object@OFname.2d!="") cat("2d similation has been evaluated for the outcome function ",object@OFname.2d,"\n",sep="")
    if(!is.null(object@runtime1d)) cat("Run time of the 1d simulation ",object@runtime1d," sec.\n",sep="")
    if(!is.null(object@runtime2d)) cat("Run time of the 2d simulation ",object@runtime2d," sec.\n",sep="")
    cat("\n")
  }
)
    
    
    
################################################################################
################################################################################
#' S4 class for representing \code{rrisk} models
#'
#' @name modelClass-class
#' @aliases modelClass
#' @docType class
#' @title S4 class for representing 'rrisk' models
#' @slot name
#' @slot modeltype
#' @slot version
#' @slot authors
#' @slot settings
#' @slot scoring
#' @slot basics
#' @slot references
#' @slot uncertainties
#' @slot parts
#' @slot graphs
#' @slot glossary
#' @slot abbreviations
#' @slot items
#' @slot validation
#' @slot comments
#' @slot conclusions
#' @slot output
#' @slot archive
#' @rdname modelClass-class
#' @exportClass modelClass
#' @examples
#'  \donttest{new("modelClass")}

setClass(Class="modelClass",
  representation=representation(
    name="modelNameClass",
    modeltype="character",
    version="modelVersionClass",
    authors="modelAuthorsClass",
    settings="modelSettingsClass",
    scoring="modelScoringClass",
    basics="modelBasicsClass",
    references="modelReferencesClass",
    uncertainties="modelUncertaintiesClass",
    parts="modelPartsClass",
    graphs="modelGraphsClass",
    glossary="modelGlossaryClass",
    abbreviations="modelAbbreviationsClass",
    items="modelItemsClass",
    validation="modelValidationClass",
    comments="modelCommentsClass",
    conclusions="modelConclusionsClass",
    output="modelOutputClass",
    archive="ANY"),
  prototype=prototype(
    name=new("modelNameClass"),
    modeltype="user defined",
    version=new("modelVersionClass"),
    authors=new("modelAuthorsClass"),
    settings=new("modelSettingsClass"),
    scoring=new("modelScoringClass"),
    basics=new("modelBasicsClass"),
    references=new("modelReferencesClass"),
    uncertainties=new("modelUncertaintiesClass"),
    parts=new("modelPartsClass"),
    graphs=new("modelGraphsClass"),
    glossary=new("modelGlossaryClass"),
    abbreviations=new("modelAbbreviationsClass"),
    items=new("modelItemsClass"),
    validation=new("modelValidationClass"),
    comments=new("modelCommentsClass"),
    conclusions=new("modelConclusionsClass"),
    output=new("modelOutputClass"),
    archive=NULL))
    
#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelClass}}
#'
#' @rdname show-methods
#' @aliases show,modelClass-method
#' @docType methods
#' @title Show method for 'modelClass'
#' @param object a \code{modelClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelClass"))}

setMethod(f="show",
  signature=signature(object="modelClass"),
  definition=function(object)
  { show(object@name)
    cat("*************************************************************************\n")
    cat("Model type\n")
    cat("*************************************************************************\n")
    cat(object@modeltype,"\n\n")
    show(object@version)
    show(object@authors)
    show(object@settings)
    show(object@scoring)
    show(object@basics)
    show(object@references)
    show(object@uncertainties)
    show(object@parts)
    show(object@items)
    show(object@graphs)
    show(object@glossary)
    show(object@abbreviations)
    show(object@output)
    show(object@validation)
    show(object@comments)
    show(object@conclusions)
    #---------------------------------------------------------------------------
    cat("*************************************************************************\n")
    cat("Archive\n")
    cat("*************************************************************************\n")
    if(!is.null(object@archive))
    { cat("There is one old model version in the archive...\n\n")
    } else cat("The archive is empty...\n\n")
  })
  

 
################################################################################
################################################################################
#' S4 class for representing \code{rrisk} session
#'
#' @name rriskClass-class
#' @aliases rriskClass
#' @docType class
#' @title S4 class for representing 'rrisk' session
#' @slot models
#' @slot scoringsystems
#' @slot disclaimer
#' @slot glossary
#' @slot abbreviations
#' @slot rpackages
#' @slot rversion
#' @rdname rriskClass-class
#' @exportClass rriskClass
#' @examples
#'  \donttest{new("rriskClass")}

setClass(Class="rriskClass",
  representation=representation(
    models="list",
    scoringsystems="list",
    disclaimer="list",
    glossary="list",
    abbreviations="list",
    rpackages="character",
    rversion="character",
    rriskversion="character"),
  prototype=prototype(
    models=list(),
    scoringsystems=list(),
    disclaimer=list(
      ABOUT="rrisk is is a set of functions running in the distributed rrisk-workspace using the R software package (R Development Core Team (2007). R: A Language and Environment forStatistical Computing, R Foundation for Statistical Computing, Vienna, Austria, ISBN 3-900051-07-0, http://www.R-project.org.).",
      COPYRIGHT="Copyright 2009 [under open source GNU licensing agreement of R] \nFederal Institute for Risk Assessment (BfR), Germany",
      CONTACT="matthias.greiner@bfr.bund.de\nwww.bfr.bund.de/cd/52162",
      AUTHORS="Matthias Greiner (BfR) \nKristin Tolksdorf (BfR) \nChristine Mueller-Graf (BfR) \nJuliane Braeunig (BfR) \nNatalia Belgorodski (STAT-UP) \nYinchong Yang (STAT-UP) \nKatharina Schueller (STAT-UP) \nMarkus Junker (MaSt services)",
      NOTES="rrisk is a project under development. Contributions are welcome!",
      DISCLAIMER="THE rrisk SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."),
    glossary=list(),
    abbreviations=list(),
    rpackages=c(),
    rversion="2.11.0",
    rriskversion="1.4"
))
 