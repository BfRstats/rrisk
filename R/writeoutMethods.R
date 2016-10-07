

################################################################################
################################################################################
#' @description Generic function for writing model informations list into a file        
#' @rdname writeOut-methods
#' @aliases writeOut-methods
#' @title Generic function and methods for writing specific model informations into a file
#' @usage writeOut(object, ...)
#' @param object An object
#' @exportMethod writeOut

setGeneric(name="writeOut",def=function(object,...)standardGeneric("writeOut"))


################################################################################
################################################################################
#' @description writeOut method for \code{\linkS4class{modelReferencesClass}}
#' @rdname writeOut-methods
#' @aliases writeOut,modelReferencesClass-method
#' @usage \S4method{writeOut}{modelReferencesClass}(object) 
#' @docType methods
#' @title writeOut method for \code{modelReferencesClass}
#' @param object a \code{modelReferencesClass} object   
#' @param model.name name of the model                                                     
#' @examples
#' \donttest{writeOut(new("modelReferencesClass"))}

setMethod(f="writeOut",
  signature=signature(object="modelReferencesClass"),
  definition=function(object,model.name)
  {
    object<-object@references
    if(object=="")
    {
      tkmessageBox(message=paste("The references list is empty and could not be written into the file!"),icon="error",type="ok")
    } else
    { file.name<-gsub(x=model.name," ",replacement="")
      file.name<-tclvalue(tkgetSaveFile(filetypes=" {{} {.bib}} ",initialfile=paste(file.name,".bib",sep="")))
      if(nchar(file.name)>0)
      {
        suppressWarnings(file.remove(file.name))
        try.result<-try(writeLines(text=object,file.name))
        if (inherits(try.result, "try-error"))
        { tkmessageBox(message=paste("The references list could NOT be written in file '",file.name,"'!"),icon="error",type="ok")
        } else
        { tkmessageBox(message=paste("The references list has been written into the file '",file.name,"' successfully!"),icon="info",type="ok")
        }
      }
    }
  }
) # end of function


################################################################################
################################################################################
#' @description writeOut method for \code{\linkS4class{modelClass}}
#' @rdname writeOut-methods
#' @aliases writeOut,modelClass-method
#' @usage \S4method{writeOut}{modelClass}(object) 
#' @docType methods
#' @title writeOut method for \code{modelClass}
#' @param object a \code{modelClass} object                                                         
#' @examples
#' \donttest{
#' writeOut(new("modelClass"))
#' writeOut(init.Model1())}

setMethod(f="writeOut",
  signature=signature(object="modelClass"),
  definition=function(object)
  {
    if(!is.null(object@name))
    {
      file.name<-paste(gsub(x=object@name@name," ",replacement=""))
    } else
    {
      file.name<-"rriskModel"
    }
    file.name<-tclvalue(tkgetSaveFile(filetypes=" {{} {.Rmdt}} ",initialfile=paste(file.name,".Rmdt",sep="")))
    if(nchar(file.name)>0)
    { 
      suppressWarnings(file.remove(file.name))
      try.result<-try(writeOutModelClass(object,file.name))
      if (inherits(try.result, "try-error"))
      { suppressWarnings(file.remove(file.name))
        tkmessageBox(message=paste("The model could NOT be written in file '",file.name,"'!"),icon="error",type="ok")
      } else
      { tkmessageBox(message=paste("The model has been written into the file '",file.name,"' successfully!"),icon="info",type="ok")
      }
    }
  })
  
  
################################################################################
################################################################################
#' @description writeOut method for \code{\linkS4class{modelScoringClass}}
#' @rdname writeOut-methods
#' @aliases writeOut,modelScoringClass-method
#' @usage \S4method{writeOut}{modelScoringClass}(object) 
#' @docType methods
#' @title writeOut method for \code{modelScoringClass}
#' @param object a \code{modelScoringClass} object                                                                           
#' @examples
#' \donttest{writeOut(new("modelScoringClass"))
#' writeOut(init.scoresystem1())}

setMethod(f="writeOut",
  signature=signature(object="modelScoringClass"),
  definition=function(object)
  { 
    if(!is.null(object@name))
    { file.name<-paste(gsub(x=object@name," ",replacement=""))
    } else
    { file.name<-"rriskScoringSystem"
    }
    file.name<-tclvalue(tkgetSaveFile(filetypes=" {{} {.Rsdt}} ",initialfile=paste(file.name,".Rsdt",sep="")))
    if(nchar(file.name)>0)
    { suppressWarnings(file.remove(file.name))
      try.result<-try(writeOutScoringsystemClass(object,file.name))
      if (inherits(try.result, "try-error"))
      { suppressWarnings(file.remove(file.name))
        tkmessageBox(message=paste("The scoring system could NOT be written in file '",file.name,"'!"),icon="error",type="ok")
      } else
      { tkmessageBox(message=paste("The scoring system has been written into the file '",file.name,"' successfully!"),icon="info",type="ok")
      }
    }
  })





