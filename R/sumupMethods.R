
################################################################################
################################################################################
#' @description Generic function for summing up objects
#' @rdname sumUp-methods
#' @aliases sumUp-methods
#' @title Generic function and methods for object summing up
#' @usage sumUp(object,...)
#' @param object An object
#' @exportMethod sumUp

setGeneric(name="sumUp",def=function(object,...)standardGeneric("sumUp"))

################################################################################
################################################################################
#' @description \code{sumUp} method for \code{\linkS4class{modelClass}}
#' @rdname sumUp-methods
#' @aliases sumUp,modelClass-method
#' @docType methods
#' @title sumUp method for modelClass
#' @param object an \code{\linkS4class{modelClass}} object
#' @exportMethod sumUp
#' @examples
#' \donttest{sumUp(new("modelClass"))
#' sumUp(init.Model1())}

setMethod(f="sumUp",
  signature=signature(object="modelClass"),
  definition=function(object,showFullCommand=FALSE)
  { cat("************************************************************************************************************************************************************************************************\n")
    cat("Model name: ",object@name@name,"\n")
    cat("************************************************************************************************************************************************************************************************\n")
    cat("Version: ",paste(object@version@majorupdate,".",object@version@minorupdate," (",object@version@status,")",sep=""),"\n")
    cat("------------------------------------------------------------------------\n")
    cat("Model type: ",object@modeltype,"\n")
    cat("------------------------------------------------------------------------\n")
    cat("Scoring system: ",object@scoring@name,"\n")
    cat("------------------------------------------------------------------------\n")
    cat("Authors: ")
    if(length(object@authors@authors)>0)
    { for(i in 1:length(object@authors@authors))
      { if(i==1)
        { cat(object@authors@authors[[i]]@name)
        } else  cat("\n\t",object@authors@authors[[i]]@name)      
      }
    } 
    cat("\n------------------------------------------------------------------------\n")
    cat("Parts and items:\n")
    output<-data.frame("Part"=NA,"Item"=NA,"Stratum"=NA,Dep.items=NA,"Type"=NA,"Definition"=NA,"Role"=NA,"Explanation"=NA,"Full command"=NA)
    if(length(object@items@items)>0)# & length(object@parts@parts))
    {  for(i in 1:length(object@items@items))
      { temp<-object@items@items[[i]]
        if(temp@explanation!="")
        { temp.explanation<-strsplit(temp@explanation,split=" ")[[1]]
          temp.explanation<-temp.explanation[1:4]
          temp.explanation<-paste(temp.explanation,collapse=" ")
          temp.explanation<-paste(temp.explanation,"...",sep="")
        } else temp.explanation<-temp@explanation
        if(length(temp@depitem)==0)
        { temp.depitem<-""
        } else temp.depitem<-temp@depitem
        temp<-c(temp@part,temp@name,temp@stratum,temp.depitem,temp@typecode,temp@definition,temp@rolecode,temp.explanation,temp@fullc)
        output[i,]<-temp  
      } 
      if(showFullCommand==FALSE) output<-output[,-7]
      print(output)
      cat("\n")   
    } 
  }
)



################################################################################
################################################################################
#' @description \code{sumUp} method for \code{\linkS4class{modelScoringClass}}
#' @rdname sumUp-methods
#' @aliases sumUp,modelScoringClass-method
#' @docType methods
#' @title sumUp method for modelScoringClass
#' @param object an \code{\linkS4class{modelScoringClass}} object
#' @exportMethod sumUp
#' @examples
#' \donttest{sumUp(new("modelScoringClass"))
#' sumUp(init.scoresystem1())}

setMethod(f="sumUp",
  signature=signature(object="modelScoringClass"),
  definition=function(object)
  { cat("************************************************************************************************************************************************************************************************\n")
    cat("Summing up of scoring system\n")
    cat("************************************************************************************************************************************************************************************************\n")
    cat("Name: ",object@name,"\n")
    cat("------------------------------------------------------------------------\n")
    cat("Table header: ",putLineBreaks(object@tableheader),"\n")
    cat("------------------------------------------------------------------------\n")
    cat("Explanatory: ",putLineBreaks(object@explanatory),"\n")
    cat("------------------------------------------------------------------------\n")
    cat("Type: ",object@systemtype,"\n")
    cat("------------------------------------------------------------------------\n")
    cat("Values: ",object@values,"\n")
    cat("------------------------------------------------------------------------\n")
    #---------------------------------------------------------------------------
    if(length(object@vcolors)>0)
    { for(i in 1:length(object@vcolors))
      { if(i==1) vcolors<-c()
        vcolorsTemp<-paste(object@vcolors[i],"=",names(object@vcolors)[i],sep="")
        vcolors<-c(vcolors,vcolorsTemp)
      }
    } else vcolors=""
    cat("Colors: ",vcolors,"\n")
    cat("------------------------------------------------------------------------\n")
    #---------------------------------------------------------------------------
    if(length(object@vmeanings)>0)
    { for(i in 1:length(object@vmeanings))
      { if(i==1) vmeanings<-c()
        vmeaningsTemp<-paste(object@vmeanings[i],"=",names(object@vmeanings)[i],sep="")
        vmeanings<-c(vmeanings,vmeaningsTemp)
      }
    } else vmeanings=""
    cat("Meanings: ",vmeanings,"\n")
    cat("------------------------------------------------------------------------\n")
    #---------------------------------------------------------------------------
    cat("Scores:\n")
    if(length(object@scoring)>0)
    { for(i in 1:length(object@scoring))
      { temp<-object@scoring[[i]]
        cat("\t",paste("(",temp@notation,")",sep=""))
        cat(" ",temp@name,"\n")
      }
      cat("\n")
    } #else cat("empty\n\n") 
  }
   #cat("------------------------------------------------------------------------\n")
)




################################################################################
################################################################################
#' @description \code{sumUp} method for \code{\linkS4class{rriskClass}}
#' @rdname sumUp-methods
#' @aliases sumUp,rriskClass-method
#' @docType methods
#' @title sumUp method for rriskClass
#' @param object an \code{\linkS4class{rriskClass}} object
#' @exportMethod sumUp
#' @examples
#' \donttest{sumUp(new("rriskClass"))
#' sumUp(init.rriskSession())}

setMethod(f="sumUp",
  signature=signature(object="rriskClass"),
  definition=function(object)
  { if(length(object@models)>0)
    { cat("*********************************************************************\n")
      cat("Available models:\n")
      cat("*********************************************************************\n")
      for(i in 1:length(object@models))
      cat(paste("(",i,")",sep=""),object@models[[i]]@name@name,paste("(",object@models[[i]]@modeltype,")",sep=""),"\n")
    } else print("The rrisk model list is empty!")
    #-----------
    cat("\n")
    #-----------
    if(length(object@scoringsystems)>0)
    { cat("*********************************************************************\n")
      cat("Scoring systems:\n")
      cat("*********************************************************************\n")
      for(i in 1:length(object@scoringsystems))
      { cat(paste("(",i,")",sep=""),object@scoringsystems[[i]]@name,paste("(",object@scoringsystems[[i]]@systemtype,")",sep=""),"\n")
        if(length(object@scoringsystems[[i]]@scoring)>0)
        { for(j in 1:length(object@scoringsystems[[i]]@scoring))
          { temp<-object@scoringsystems[[i]]@scoring[[j]]
            cat("\t",paste("(",temp@notation,")",sep=""),temp@name,"\n")
          }
        } else cat("There are no scores in the scoring system!\n")
        cat("\n\n")
      }
    } else print("There are no scoring systems available in the current rrisk session!")
    cat("*********************************************************************\n")
    cat("Use menu to select other information to display\n")
    cat("*********************************************************************\n")   
  })
  


