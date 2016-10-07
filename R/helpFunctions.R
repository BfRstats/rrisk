

################################################################################
################################################################################
#' @name database
#' @aliases datasets sdata zipdata NorthSouth
#' @title Data sets
#' @keywords dataset
#' @examples
#' \donttest{
#' data(sdata)
#' sdata
#' 
#' data(zipdata)
#' zipdata
#'
#' data(NorthSouth)
#' NorthSouth
#' }
NULL


################################################################################
################################################################################
#' @name openUserGuide
#' @aliases openUserGuide
#' @title Opend the 'rrisk' User Guide (pdf docoment)
#' @usage openUserGuide()
#' @keywords manip
#' @export

openUserGuide<-function(){
  rrisk.openPDF(file=system.file("doc","rriskUserGuide.pdf", package = "rrisk"), bg = TRUE)
} # end of function openUserGuide()


################################################################################
################################################################################
#' @description Sampling from dara frame column by predefined weights
#' @name rrisksample
#' @aliases rrisksample
#' @title Non-executable auxiliary function
#' @usage rrisksample(data,weights)
#' @param data data frame
#' @param weights vector with weights for sampling from data frame columns
#' @keywords manip
#' @export

rrisksample<-function(data,weights){
  results<-c()
  for(i in 1:nrow(data)){
    results<-c(results,data[i,weights[i]])
  }
  return(results)
} # end of function rrisksample()


################################################################################
################################################################################
#' @name setDepItems
#' @aliases setDepItems
#' @title Non-executable auxiliary function
#' @usage setDepItems(item,items)
#' @param item ...
#' @param items ... 
#' @keywords manip
#' @export

setDepItems<-function(item,items){

  #allTerms1<-all.vars(parse(text=item@fullc))
  allTerms1<-all.names(parse(text=item@fullc))

  allTerms2<-c()
  if(is.element(item@typecode,c("bsrv","mcrv","bdjp"))){
    helpvar<-as.list(strsplit(item@definition,split="; fitted to")[[1]])
    for(i in 1:length(helpvar)){
      try.result<-try(allTerms2.temp<-all.vars(parse(text=helpvar[[i]])),silent=TRUE)
      if (!inherits(try.result, "try-error")){
        allTerms2<-c(allTerms2,allTerms2.temp)
      }
    } # end for
  } else if(item@typecode=="mxrv"){
    helpvar<-as.list(strsplit(item@definition,split=" based on: ")[[1]])
    for(i in 1:length(helpvar)){
      try.result<-try(allTerms2.temp<-all.vars(parse(text=helpvar[[i]])),silent=TRUE)
      if (!inherits(try.result, "try-error")){
        allTerms2<-c(allTerms2,allTerms2.temp)
      }
    } # end for
  } # end if(is.element(item@typecode,c("bsrv","mcrv","bdjp"))){
  
  allTerms3<-item@stratum

  allTerms<-c(allTerms1,allTerms2,allTerms3)
  allTerms<-setdiff(allTerms,"")

  if(length(items)>0){
    for(i in 1:length(items)){
      if(is.element(items[[i]]@name,allTerms)){
        items[[i]]@depitem<-paste(items[[i]]@depitem,item@name,collapse=" ")
        depitem<-strsplit(x=items[[i]]@depitem,split=" ")
        if (length(depitem)>0){
          depitem<-depitem[[1]]
          depitem<-unique(depitem)
          items[[i]]@depitem<-paste(depitem,collapse=" ")
        }
      }
    } # end for(i in 1:length(items)){
  } # end if(length(items)>0){
  return(items)
} # end of function setDepItems()



################################################################################
################################################################################
#' @description Function rudiscrete
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name rudiscrete
#' @aliases rudiscrete
#' @title rudiscrete
#' @usage rudiscrete(n=100,min,max)
#' @param n n
#' @param min min
#' @param max max
#' @export
#' @keywords manip


rudiscrete<-function(n=100,min,max)
{ output<-c()
  for(j in 1:n/length(min)){
    for(i in 1:length(min)){
      output.temp<-sample(min[i]:max[i], size=1, replace=TRUE)
      output<-c(output,output.temp)
    } # end i-loop
  } # end j-loop
  return<-output
}
    
    
################################################################################
################################################################################
#' @description Function rdiscrete
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name rdiscrete
#' @aliases rdiscrete
#' @title rdiscrete
#' @usage rdiscrete(n,values,probs)
#' @param n n
#' @param values values
#' @param probs probs
#' @export
#' @keywords manip
    
rdiscrete<-function(n=100,values=1:length(probs),probs)
{
  output<-sample(values, size=n, replace=TRUE, prob=probs)
  return<-output
} # end of function rdiscrete() 


################################################################################
################################################################################
#' @description Function that loads all packages, initializes \code{demoModels} and starts user menu
#'
#' @details This function starts the rrisk session, it is the main function.
#'
#' @name rrisk.start
#' @aliases rrisk.start
#' @title Function that loads all packages, initializes demoModels and starts user menu
#' @usage rrisk.start(useDemoModels="all", demomode=FALSE)  
#' @param useDemoModels which demoModels should be used. Possible values are 
#' \code{useDemoModels=c("no","all", "Demo model 1", "Demo model 2")}.
#' @param demomode a boolean value indicating whether model should be run under the demo mode, \code{demomode=c(TRUE,FALSE)}
#' @keywords rrisk
#' @export
#' @examples
#' \donttest{rrisk.start(useDemoModels="all")}

rrisk.start<-function(useDemoModels="all", demomode=FALSE)    
{ # useDemoModels=c("no","all","Demo model 1", "Demo model 2")
  if(demomode){
    require(tcltk)
    require(png)
  }
  options(width=500)
  accept<-FALSE
  accept<-rriskAgreement(demomode=demomode)
  
  if (accept==FALSE){
    cat("\n Yout have not accepted the rrisk licence  --> Goodbye...\n")  
  } else if(accept==TRUE){
    # Welcome information
    cat("----------------------------------------------------------------------------------------------\n")
    cat("Welcome to rrisk!\n")
    if(demomode==TRUE){
      cat("You are using\t\t rrisk version 2.7 (last update 17.09.2012)\n")
    } else {
      cat("You are using\t\t rrisk version", packageDescription("rrisk")["Version"]$Version ,"( last update",packageDescription("rrisk")["Date"]$Date,")\n")
    }
    cat("Working directory is\t", getwd(),"\n")
    if(demomode==FALSE){
      cat("Type 'openUserGuide()' in to open the rrisk User Guide\n")
    }
    cat("----------------------------------------------------------------------------------------------\n")
    #-------------------------------------------------------------------------------
    # falls pakete fehlen
    #-------------------------------------------------------------------------------
    #init.rrisk()
  
    #-------------------------------------------------------------------------------
    # load required packages
    #-------------------------------------------------------------------------------
    if(demomode==TRUE){
      require(tcltk)
      require(png)
      require(rgdal)
      require(grDevices)
      require(network)
      require(sna)
      require(tools)
      require(tree)
      require(mgcv)
      require(rriskBayes)
      require(rriskDistributions)
      require(xtable)
    } # end if(demomode)
  
    #-------------------------------------------------------------------------------
    # create default rriskClass object
    #-------------------------------------------------------------------------------
    rriskSession<-init.rriskSession(useDemoModels=useDemoModels,demomode)
    
    #-----------------------------------------------------------------------------
    # start main rrisk menu
    #-----------------------------------------------------------------------------
    menu.mainMenu(rriskSession,demomode)
  } # end if accept-if-block
} # end of fucntion rrisk()




################################################################################
################################################################################
#' @description Function that starts a proper PDF viewer according to the system environment
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name rrisk.openPDF
#' @aliases rrisk.openPDF
#' @title Function that starts a proper PDF viewer according to the system environment
#' @usage rrisk.openPDF(file, bg = TRUE)  
#' @param file the file to be opened
#' @param bg parameter for unix systems
#' @keywords rrisk
#' @export
#' @examples
#' \donttest{file = file.path(getwd(), "rrisk-manual.PDF")
#' rrisk.openPDF(file, bg = TRUE)}

rrisk.openPDF<-function (file, bg = TRUE) 
{ OST <- .Platform$OS.type
  if (OST == "windows")
  { shell.exec(file)
  }
  else if (OST == "unix")
  { bioCOpt <- getOption("BioC")
    pdf <- getOption("pdfviewer")
    msg <- NULL
    if (is.null(pdf)) 
      msg <- "getOption('pdfviewer') is NULL"
    else if (length(pdf) == 1 && nchar(pdf[[1]]) == 0) 
      msg <- "getOption('pdfviewer') is ''"
    if (!is.null(msg)) 
      stop(msg, "; please use 'options(pdfviewer=...)'")
    cmd <- paste(pdf, file)
    if (bg) 
      cmd <- paste(cmd, "&")
    system(cmd)
  }
  return(TRUE)
} # end of function rrisk.openPDF()




################################################################################
################################################################################
#' @description Function that imports parts from another \code{\linkS4class{modelClass}} object
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name getParts
#' @aliases getParts
#' @title Function that imports slot 'parts' from another 'modelClass' object
#' @usage getParts(rriskModel,rriskModelFrom,
#'    partsToImportIndex=1:length(rriskModelFrom@@parts@@parts),onlyItems=FALSE)
#' @param rriskModel the \code{rrisk} model the parts to be exported to 
#' @param rriskModelFrom the \code{rrisk} model from which the parts are exported
#' @param partsToImportIndex index of all parts in the rriskmodelFrom object
#' @param onlyItems whether only items should be imported
#' @keywords model
#' @export
#' @examples
#' \donttest{rriskModel<-new("modelClass")
#' rriskModelFrom<-init.Model1()
#' getParts(rriskModel,rriskModelFrom,
#'   partsToImportIndex=1:length(rriskModelFrom@@parts@@parts),onlyItems=FALSE)}

getParts<-function(rriskModel,rriskModelFrom,partsToImportIndex=1:length(rriskModelFrom@parts@parts),onlyItems=FALSE)
{
  if(length(rriskModelFrom@parts@parts)>0)
      { partsNamesLeft<-c()
        if(length(rriskModel@parts@parts)>0)
        { for(i in 1:length(rriskModel@parts@parts))
          { partsNamesLeft<-c(partsNamesLeft,rriskModel@parts@parts[[i]]@name)
          }
        }
        #-----------------
        partsNamesRight<-c()
        partsToImport<-list()
        for(i in 1:length(rriskModelFrom@parts@parts))
        { if(is.element(i,partsToImportIndex))
          { temp<-rriskModelFrom@parts@parts[[i]]
            partsToImport<-c(partsToImport,temp)
            partsNamesRight<-c(partsNamesRight,temp@name)    
          }
        }
        #--------------
        duplicatedParts<-intersect(partsNamesLeft,partsNamesRight)
        if(length(duplicatedParts)>0)
        { substituteParts<-tkmessageBox(title="Duplicated parts",icon="question",type="yesno",
            message="There are some duplicated parts, do you wish to substitute them?")
          if(tclvalue(substituteParts)=="yes")
          { partsNamesRight<-setdiff(partsNamesRight,duplicatedParts)
            partsToImportOld<-partsToImport
            partsToImport<-list()
            for(i in 1:length(partsToImportOld))
            { if(!is.element(partsToImportOld[[i]]@name,duplicatedParts))
              { partsToImport<-c(partsToImport,partsToImportOld[[i]])
              }
            }
          }     
        }
        #-----------------------------------------------------------------------   
        if(length(partsToImport)>0)
        { itemsToImport<-list()
          for(i in 1:length(rriskModelFrom@items@items))
          { if(is.element(rriskModelFrom@items@items[[i]]@part,partsNamesRight))
            { itemsToImportTemp<-rriskModelFrom@items@items[[i]]
              if(onlyItems) itemsToImportTemp@part<-""
              scoringSystemLeft<-rriskModel@scoring@name
              scoringSystemRight<-rriskModelFrom@scoring@name
              if(scoringSystemLeft!=scoringSystemRight) itemsToImportTemp@scores<-c()
              itemsToImport<-c(itemsToImport,itemsToImportTemp)
            }
          }
          #--------------------- 
          if(!onlyItems) rriskModel@parts@parts<-c(rriskModel@parts@parts,partsToImport)
          rriskModel@items@items<-c(rriskModel@items@items,itemsToImport)
          #---------------------
          PartItemsText<-""
          for(i in 1:length(partsToImport))
          { if(!onlyItems)
            { PartItemsTextTemp<-paste("\t",partsToImport[[i]]@name," (",partsToImport[[i]]@items,")\n",sep="")
            } else PartItemsTextTemp<-paste(partsToImport[[i]]@items," ",sep="")
            PartItemsText<-c(PartItemsText,PartItemsTextTemp)
          }   
          if(!onlyItems)
          { cat("The following", length(partsNamesRight),"parts have been successfully imported into the model:\n",PartItemsText)
          } else cat("The following", length(itemsToImport),"items have been successfully imported into the model:\n",PartItemsText)
        }      
      } else cat("There are no parts in the model from which the items should be exported!\n")
  return(rriskModel)
} # end of function getParts()


                                          

################################################################################
################################################################################
#' @description Function that adds new items to a \code{\linkS4class{modelClass}} object
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name addNewItem
#' @aliases addNewItem
#' @title Function that adds new slots 'items' to a 'modelClass' object
#' @usage addNewItem(rriskModel,menuLevel=1)
#' @param rriskModel an instance of the \code{modelClass}, i.e. any rrisk model to which new item(s) should be added
#' @param menuLevel ...
#' @keywords misc
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' addNewItem(rriskModel)}

addNewItem<-function(rriskModel,menuLevel=1)
{ on.exit(return(rriskModel))
  newModel <- change.item(index=length(rriskModel@items@items)+1,rriskModel,menuLevel=menuLevel)

  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  choices<-c("yes","no")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Do you want to save this new item?",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1){
      rriskModel<-newModel
      cat(paste("\nThe new item has been successfully saved in the current model.\n",sep=""))
      break()
    } else if(input==2){
      break()
    }
    input<-99
  } # end while
}  # end of fucntion addNewItem()



################################################################################
################################################################################
#' @description Function that imports items from another \code{\linkS4class{modelClass}} object
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name getItems
#' @aliases getItems
#' @title Function that imports slots 'items' from another 'modelClass' object
#' @usage getItems(rriskModel,rriskModelFrom,
#'      itemsToImportIndex=1:length(rriskModelFrom@@items@@items))
#' @param rriskModel the \code{rrisk} model the items to be exported to 
#' @param rriskModelFrom the \code{rrisk} model from which the parts are exported
#' @param itemsToImportIndex index of all items in the rriskModelFrom object
#' @keywords model
#' @export
#' @examples
#' \donttest{rriskModel<-new("modelClass")
#' rriskModelFrom<-init.Model1()
#' getItems(rriskModel,rriskModelFrom)}

getItems<-function(rriskModel,rriskModelFrom,itemsToImportIndex=1:length(rriskModelFrom@items@items))
{  
  if(length(rriskModelFrom@items@items)>0)
    { itemNamesLeft<-c()
        if(length(rriskModel@items@items)>0)
        { for(i in 1:length(rriskModel@items@items))
          { itemNamesLeft<-c(itemNamesLeft,rriskModel@items@items[[i]]@name)
          }
        }
        #-----------------
        itemNamesRight<-c()
        itemsToImport<-list()
        for(i in 1:length(rriskModelFrom@items@items))
        { if(is.element(i,itemsToImportIndex))
          { temp<-rriskModelFrom@items@items[[i]]
            temp@part<-""
            scoringSystemLeft<-rriskModel@scoring@name
            scoringSystemRight<-rriskModelFrom@scoring@name
            if(scoringSystemLeft!=scoringSystemRight) temp@scores<-c()
            itemsToImport<-c(itemsToImport,temp)
            itemNamesRight<-c(itemNamesRight,temp@name)      
          }
        }
        #--------------
        duplicatedItems<-intersect(itemNamesLeft,itemNamesRight)
        if(length(duplicatedItems)>0)
        { substituteItems<-tkmessageBox(title="Duplicated items",icon="question",type="yesno",
            message=paste("There are some duplicated items (",paste(duplicatedItems,collapse=" "),") do you wish to substitute them?",sep=""))
          if(tclvalue(substituteItems)=="yes")
          { indexToRemove<-c()
            for(i in 1:length(rriskModel@items@items))
            { tempItemLeft<-rriskModel@items@items[[i]]
              if(is.element(tempItemLeft@name,duplicatedItems))
              { for(j in 1:length(itemsToImport))
                { if(itemsToImport[[j]]@name==tempItemLeft@name)
                  { rriskModel@items@items[[i]]<-itemsToImport[[j]]
                    indexToRemove<-c(indexToRemove,j)
                  }
                }
              }
            }
            itemsToImport<-itemsToImport[-indexToRemove]
            #itemNamesRight<-setdiff(itemNamesRight,duplicatedItems)
            #itemToImportOld<-itemsToImport
            #itemsToImport<-list()
            #for(i in 1:length(itemToImportOld))
            #{ if(!is.element(itemToImportOld[[i]]@name,duplicatedItems))
             # {  itemsToImport<-list(itemsToImport,itemToImportOld[[i]]@name)
             # }
            #}
          }     
        }
        rriskModel@items@items<-c(rriskModel@items@items,itemsToImport)
        cat("The following", length(itemNamesRight),"items have been successfully imported into the model: ",itemNamesRight,".\n")       
      } else cat("There are no items in the model from which the items should be exported!\n")
  return(rriskModel)
} # end of function getItems()



################################################################################
################################################################################
#' @description Function that sets scoring system of an \code{\linkS4class{modelClass}} object
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name setScoringSystem
#' @aliases setScoringSystem
#' @title Function that sets slots 'scoring system' of an 'modelClass' object
#' @usage setScoringSystem(rriskModel,newScoringSystem)
#' @param rriskModel the \code{rrisk} model whose scoring system is to be set
#' @param newScoringSystem the new scoring system 
#' @keywords scoring
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' newScoringSystem<-new("modelScoringClass")
#' setScoringSystem(rriskModel,newScoringSystem)   }

setScoringSystem<-function(rriskModel,newScoringSystem)
{ on.exit(return(invisible(rriskModel)))
  oldScoringSystemName<-rriskModel@scoring@name
  newScoringSystemName<-newScoringSystem@name
  rriskModel@scoring<-newScoringSystem
  
  if(newScoringSystemName!="empty scoring system" & oldScoringSystemName!="empty scoring system" ){
  if(oldScoringSystemName!=newScoringSystem@name)
  { #---------------------------------------------------------------------------
    # set all item scores to 'not applicable' or remove (for complete empty scoring system)
    #---------------------------------------------------------------------------
    if(length(rriskModel@items@items)>0)
    { cat("Updating scores of model items...\n")
      for(i in 1:length(rriskModel@items@items))
      { if(!is.null(newScoringSystem@vmeanings["notapplicable"]))
        { rriskModel@items@items[[i]]@scores<-as.numeric(rep(newScoringSystem@vmeanings["notapplicable"],length(newScoringSystem@scoring)))
        } else rriskModel@items@items[[i]]@scores<-c()
      }
    }
    #---------------------------------------------------------------------------
    # set all uncertainties scores to 'not applicable' or remove (for complete empty scoring system)
    #---------------------------------------------------------------------------
    if(length(rriskModel@uncertainties@uncertainties)>0)
    { cat("Updating scores of model uncertainties...\n")
      for(i in 1:length(rriskModel@uncertainties@uncertainties))
      { if(!is.null(newScoringSystem@vmeanings["notapplicable"]))
        { notApplicableValue<-newScoringSystem@vmeanings["notapplicable"]
          newScores<-rep(notApplicableValue,length(newScoringSystem@scoring))
          rriskModel@uncertainties@uncertainties[[i]]@scores<-newScores
        } else rriskModel@uncertainties@uncertainties[[i]]@scores<-c()
      }
    }
  } else cat("New scoring system has the same name as the old scoring system --> items and uncertainties scores have not been updated!\n")
  }
} # end of function setScoringSystem()





################################################################################
################################################################################
#' @description Function that sets the path according to system environment
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name rrisk.chooseDir
#' @aliases rrisk.chooseDir
#' @title Function that sets the path according to system environment
#' @usage rrisk.chooseDir(default=getwd())
#' @param default the default path
#' @keywords rrisk
#' @export
#' @examples
#' \donttest{rrisk.chooseDir(default=getwd()) }

rrisk.chooseDir<-function(default=getwd())
{ OST <- .Platform$OS.type
  directory<-""
  if (OST == "windows")
  {  directory<-choose.dir(default=default)                                                      
     if(is.na(directory)) directory<-""
  }
  else if (OST == "unix")
  {  directory<-tclvalue(tkchooseDirectory(initialdir=default))
  }
  return(directory)
} # end of function rrisk.chooseDir()


################################################################################
################################################################################
#' @description Function that creates a dialog window
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name rrisk.DialogString
#' @aliases rrisk.DialogString
#' @title Function that creates a dialog window
#' @usage rrisk.DialogString(message="",default="")
#' @param message the message that should be shown on the dialog window
#' @param default a default text
#' @keywords rrisk
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{rrisk.DialogString(message="Please enter the name of the new model:")}

rrisk.DialogString<-function(message="",default="")
{
  onOk<-function()
  { output.temp<-tclvalue(tkget(output.entry))
    if(gsub(x=output.temp," ",replacement="")=="") output.temp<-NULL
    assign("output.temp",value=output.temp,envir=tempEnvir)
    tkdestroy(DialogWindow)
  }
  #-----------------------------------------------------------------------------
  onCancel<-function()
  { tkdestroy(DialogWindow)
  }
  #-----------------------------------------------------------------------------
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("output.temp",value=NULL,envir=tempEnvir)
  
  #-----------------------------------------------------------------------------
  DialogWindow<-tktoplevel()
  tkwm.title(DialogWindow,"Question")
  tkwm.resizable(DialogWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(DialogWindow)
  #-----------------------------------------------------------------------------

  textFrame<-tkframe(allFrame)
  output.label<-tklabel(textFrame,text=message)
  #print(default)
  output.entry <- tkentry(textFrame,width=70,text=tclVar(default))
  
  #output.entry <- tkentry(textFrame,width=70)
  #tkconfigure(output.entry,text=default)

  tkgrid(output.label,sticky="nw")
  tkgrid(output.entry)
  tkpack(textFrame,side="top")

  #-----------------------------------------------------------------------------
  buttonsFrame<-tkframe(allFrame)
  okButton<-ttkbutton(buttonsFrame,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(30,30))
  #-----------------------------------------------------------------------------

  tkpack(buttonsFrame,side="bottom",pady=c(15,0))
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))
  #-----------------------------------------------------------------------------
  tkfocus(DialogWindow)
  #tkfocus(output.entry)
  tkwait.window(DialogWindow)
  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-get("output.temp",tempEnvir)
  return(output)
} # end of function rrisk.DialogString()

#bla<-rrisk.DialogString(message="Please enter the name of the new model:")


################################################################################
################################################################################
#' @description This function wraps a text in lines with definite length.
#'
#' @details The argument \code{object} denotes the text to be displayed. \cr
#' If \code{show.output=TRUE} the text will be displayed in lines with a definite 
#' length, otherweise blank. \cr
#' The argument \code{width.output} denotes the to be defined length of a line 
#'
#' @name putLineBreaks
#' @aliases putLineBreaks
#' @title function for wraping text by definite length
#' @author Matthias Greiner \email{matthias.greiner@@bfr.bund.de}(BfR), \cr
#' Katharina Schueller \email{schueller@@stat-up.de}(\acronym{STAT-UP} Statistical Consulting), \cr
#' Natalia Belgorodski \email{belgorodski@@stat-up.de}(\acronym{STAT-UP} statistical Consulting)
#' @usage putLineBreaks(object,show.output=FALSE,width.output)
#' @param object character string, text to be wraped
#' @param show.output logical, if \code{TRUE} the text will be wraped in lines with definite length
#' @param width.output numeric, single positive integer defining the length of a line 
#' @return Returns the wraped text in lines with user-defined length if 
#' \code{show.output=TRUE}. Otherwise no text will be returned.
#' @keywords misc
#' @export
#' @examples
#' \donttest{object<-"This is a lang text to be wraped. This is a lang text 
#'  to be wraped. This is a lang text to be wraped. This is a lang text to be 
#'  wraped. This is a lang text to be wraped. This is a lang text to be wraped. 
#'  This is a lang text to be wraped. This is a lang text to be wraped."
#' object<-gsub(x=object,"\n",replacement="")
#' putLineBreaks(object=object,show.output=TRUE)       }

putLineBreaks<-function(object,show.output=FALSE,width.output=70)
{ if(nchar(object)>width.output)
  { object<-strsplit(object," ")[[1]]
    output<-object[1]
    index<-1
    if(length(object)>1)
    { for(i in 2:length(object))
      {
        output<-paste(output,object[i],sep=" ")
        if(nchar(output)>(width.output*index) & i!=length(object))
        {
          output<-paste(output,"\n",sep="")
          index<-index+1
        }
      }
    }
    } else output<-object 
  if(show.output) cat(output)
  return(invisible(output)) 
}


################################################################################
################################################################################
#' @description Function that shows data of a \code{\linkS4class{modelClass}} 
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name showModelData
#' @aliases showModelData
#' @title Function that shows item-data of a 'modelClass'
#' @usage showModelData(object)
#' @param object a \code{modelClass} object
#' @keywords model
#' @export
#' @examples
#' \donttest{object <- init.Model1()
#' showModelData(object@@items)  }

showModelData<-function(object)
{ if(length(object@items)>0)
  { for(i in 1:length(object@items))
    { itemTemp<-object@items[[i]]
      if(itemTemp@typecode=="data")
      { if(!is.null(itemTemp@data))
        { cat("***************************************************************\n")
          cat( paste("Model related data set (item: ",itemTemp@name,")\n",sep=""))
          cat("***************************************************************\n")
          print(head(itemTemp@data))
          cat("only first rows shown...\n")
        } else
        { cat("***************************************************************\n")
          cat( "Model related data set\n")
          cat("***************************************************************\n") 
          cat("No data set(s) available for the current model!\n")
        }
      } 
    }
 } else
 {  cat("***************************************************************\n")
    cat( "Model related data set\n")
    cat("***************************************************************\n") 
    cat("No data set(s) available for the current model!\n")
 }
} # end of fucntion showModelData()



################################################################################
################################################################################
#' @description Function that exports model graphs
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name exportModelGraphs
#' @aliases exportModelGraphs
#' @title Function that exports model graphs
#' @usage exportModelGraphs(rriskModel)
#' @param rriskModel an instance of the \code{modelClass} 
#' @keywords model
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' exportModelGraphs(rriskModel)}

exportModelGraphs<-function(rriskModel)
{ 
  oldDirectory<-getwd()
  on.exit(setwd(oldDirectory))
  
  # choose directory where model graphs shoul be written
  #directory<-tclvalue(tkchooseDirectory())
  directory<-rrisk.chooseDir()
  if(nchar(directory)>0)
  { setwd(directory)
    # export concept graphs
    displayConceptGraphs(rriskModel,exportGraph=TRUE) 
    # export network graph
    plotModelNetwork(rriskModel,pdfGraph=TRUE)  
    # export uncertainties graphs
    tolatexGraphs.uncertainties(rriskModel,LatexReport=FALSE,pdfGraph=TRUE)
  }
}


################################################################################
################################################################################
#' @description Function that exports model data
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name exportModelData
#' @aliases exportModelData
#' @title Function that exports model data
#' @usage exportModelData(rriskModel)
#' @param rriskModel an instance of the \code{modelClass} 
#' @keywords model
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' exportModelData(rriskModel)}

exportModelData<-function(rriskModel)
{ oldDirectory<-getwd()
  on.exit(setwd(oldDirectory))
  modelItems<-rriskModel@items@items
  if(length(modelItems)>0)
  { # choose directory where model graphs shoul be written
    directory<-rrisk.chooseDir()
    if(nchar(directory)>0)
    { setwd(directory)
      for(i in 1:length(modelItems))
      { if(modelItems[[i]]@typecode=="data")
        { if(!is.null(modelItems[[i]]@data))
          { fileName<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_",modelItems[[i]]@name,".txt",sep="")
            write.table(x=modelItems[[i]]@data,file=fileName,row.names=FALSE,quote=FALSE)
          } else cat("Data item does not contain any data set!\n")
        } #else cat("There is no data sets available in the current model!\n")
      }
    }
   } else cat("Model items list is empty --> No data set(s) can be exported!\n")
} # end of function exportModelData() 


    
################################################################################
################################################################################   
#' @description Function that displays the uncertaintiy information in a matrix
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name uncertainties2matrix
#' @aliases uncertainties2matrix
#' @title Function that displays the uncertaintiy information in a matrix
#' @usage uncertainties2matrix(modelUncertainties,modelSsystem)
#' @param modelUncertainties the slot unvertainties@@uncertainties of an instance of the \code{modelClass} 
#' @param modelSsystem the slot scoring of an instance of the \code{modelClass} 
#' @keywords misc
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' res<-uncertainties2matrix(rriskModel@@uncertainties@@uncertainties,rriskModel@@scoring)}
 
uncertainties2matrix<-function(modelUncertainties,modelSsystem)
{ for(i in 1:length(modelSsystem@scoring))
  { if(i==1) colNames<-c()
    colNamesTemp<-paste("(",modelSsystem@scoring[[i]]@notation,")",sep="")
    colNames<-c(colNames,colNamesTemp)
  } 
  len<-length(modelUncertainties)
  if(len>0)
  { uncertMatrix<-matrix(rep(0,len*length(modelSsystem@scoring)),ncol=length(modelSsystem@scoring))
    colnames(uncertMatrix)<-colNames
    for(i in 1:length(modelUncertainties))
    { if(i==1) rowNames<-c()
      rowNames<-c(rowNames,modelUncertainties[[i]]@namesub)
      uncertMatrix[i,]<-modelUncertainties[[i]]@scores
      if(i==length(modelUncertainties)) rownames(uncertMatrix)<-rowNames
    }
    return(uncertMatrix)
  } else
  { return(NA)
  }
} # end of function uncertainties2matrix() 


################################################################################
################################################################################
#' @description Displaying scores of model uncertainties
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotUncertainties
#' @aliases plotUncertainties
#' @title Displaying scores of model uncertainties
#' @usage plotUncertainties(uncertMatrix, maintext="ModelUncertainties",
#' modelSsystem,useNotApplicable=TRUE,pdfGraph=FALSE,modelName=NULL)
#' @param uncertMatrix numerical matrix, output of the function \code{\link{uncertainties2matrix}}
#' @param maintext character string, plot title
#' @param modelSsystem an instance of the \code{modelScoringClass} 
#'  containing scoring system of the \code{rrisk} model
#' @param useNotApplicable logical defining whether 'not applicable' scores should be plotted, \code{useNotApplicable=c(TRUE,FALSE)}
#' @param pdfGraph logical defining whether a pdf file should be created, \code{pdfGraph=c(TRUE,FALSE)}
#' @param modelName name of the model
#' @export
#' @keywords graphs
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' uncertMatrix<-uncertainties2matrix(rriskModel@@uncertainties@@uncertainties[1:9],
#'  rriskModel@@scoring) 
#' plotUncertainties(uncertMatrix,modelSsystem=rriskModel@@scoring, 
#'  useNotApplicable=FALSE)
#' plotUncertainties(uncertMatrix,modelSsystem=rriskModel@@scoring, 
#'  useNotApplicable=TRUE)
#' plotUncertainties(uncertMatrix,modelSsystem=rriskModel@@scoring, 
#'  useNotApplicable=TRUE,pdfGraph=TRUE)}

plotUncertainties<-function(uncertMatrix,maintext="ModelUncertainties",
  modelSsystem,useNotApplicable=TRUE,pdfGraph=FALSE,modelName=NULL)
{ output<-list(removed=c(),values=c(),vcolors=c(),vmeanings=c())
  
  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  scoreValues<-modelSsystem@values
  vcolors<-modelSsystem@vcolors
  vmeanings<-modelSsystem@vmeanings
  
  if(maintext=="ModelUncertainties") maintext<-"Model uncertainties"
  
  #-----------------------------------------------------------------------------
  #remove columns with all not-applicable entries
  #-----------------------------------------------------------------------------
  notapplicable<-vmeanings[which(names(vmeanings)=="notapplicable")]
  if(!useNotApplicable){
    for(i in 1:ncol(uncertMatrix)){
    if(i==1) colToRemove<-c()
      if(all(uncertMatrix[,i]==notapplicable)) colToRemove<-c(colToRemove,i)    
    } # end for
    output$removed=colnames(uncertMatrix)[colToRemove]
    uncertMatrix<-uncertMatrix[,-colToRemove]
        
    scoreValues<-setdiff(scoreValues,notapplicable)
    vcolors<-vcolors[which(vcolors!=notapplicable)]
    vmeanings<-vmeanings[which(vmeanings!=notapplicable)]
  } # end if
  
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  output$values<-scoreValues
  output$vcolors<-vcolors
  output$vmeanings<-vmeanings
  on.exit(return(invisible(output)))
  
  #-----------------------------------------------------------------------------
  # compute min, max and color vector
  #-----------------------------------------------------------------------------
  min<-min(scoreValues)
  max<-max(scoreValues)
  matVector<-as.vector(t(uncertMatrix))
  colVector=rep(NA,length(matVector))
  for(i in 1:length(vcolors)){
    colVector[which(matVector==vcolors[i])]<-names(vcolors)[i]
  }# end for 
  
  #-----------------------------------------------------------------------------
  # help computations
  #-----------------------------------------------------------------------------
  n.col <- ncol(uncertMatrix)
  if(length(min) == 1) min <- rep(min,n.col)
  if(length(max) == 1) max <- rep(max,n.col)
  if(length(min) != n.col | length(max) != n.col) stop("min or max incorrect length")
  z <- uncertMatrix
  for (j in 1:n.col){
    b <- lm(c(1,20)~c(min[j],max[j]))$coeff
    z[,j] <- round(b[[1]] + b[[2]]*z[,j])
  } # end for
  z <- as.numeric(t(z))
  r <- nrow(uncertMatrix)
  c <-ncol(uncertMatrix)
  y <- rep(r:1,each=c)
  x <- rep(1:c,r)+.2
  min.x <- min(x)
  max.x <- max(x) + 3
  min.y <- min(y)
  max.y <- max(y)  + 1
 
  #-----------------------------------------------------------------------------
  # create graph
  #-----------------------------------------------------------------------------
  if(pdfGraph==FALSE){
    X11(width=ncol(uncertMatrix)+1) 
    graphRef <- paste("\\ref{fig: ", gsub(x=modelName," ",replacement=""), "_", gsub(x = maintext, " ", replacement = ""), "}", sep = "")
    cat("Use for referencing to the model uncertainty graph(s) following LaTeX commando(s): ",paste(graphRef,collapse=", "),"\n")
    maintext <- paste(maintext, "\n", graphRef, sep = "")
  } else{
    file.name=gsub(x=maintext," ",replacement="")
    file.name<-paste(file.name,"pdf",sep=".")
    file.name<-paste(gsub(x=modelName," ",replacement=""),"_",file.name,sep="")
    pdf(file=file.name,width=ncol(uncertMatrix)+1)
  } # end if
  par(mfrow=c(1,1),mai=c(1,1,1.5,1)-.9,oma=c(0,0,0,0))
  plot(c(1,max.y)~c(1,max.x), ylim=c(0,max.y),
    main=maintext,type = "n",axes=FALSE,xlab="",ylab="")
  for (i in 1:r) lines(x=c(1.2,c),y=c(i,i),lty="dotted")
  points(y~x,cex=5,pch=21,col=colVector,bg=colVector)
  text(x=(1:c)+.2,y=rep(max.y,c),labels=colnames(uncertMatrix))
  text(x=rep(max.x-2,r),y=r:1,labels=rownames(uncertMatrix),adj=c(0,NA))
  lines(c(max.y,max.y)-0.5~c(0.8,max.x))   # top line
  lines(c(0.5,0.5)~c(0.8,max.x))           # bottom line
  lines(c(max.y-0.5,0.5)~rep(0.8,2))     # left
  lines(c(max.y-0.5,0.5)~rep(max.x,2))     # right
  legend(min(y),0.3,pch=21,col=names(vcolors),
    pt.bg=names(vcolors),text.width=2,x.intersp=2,
    horiz=TRUE,pt.cex=4,bty="n",
    legend=names(vmeanings),border=FALSE)
  if(pdfGraph==TRUE) dev.off()
} # end of function plotUncertainties1()



################################################################################
################################################################################
#' @description Create LaTeX code for displaying model uncertainties graphs
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.uncertainties
#' @aliases tolatexGraphs.uncertainties
#' @title LaTeX code for displaying model uncertainties graphs
#' @usage tolatexGraphs.uncertainties(rriskModel,LatexReport=FALSE,
#'    file.name,pdfGraph=FALSE)
#' @param rriskModel an instance of the \code{modelClass}
#' @param LatexReport whether a LaTeX report should be created
#' @param file.name file that saves the tex codes to import the unvertainties graphics
#' @param pdfGraph whether the pdf graphics should be created
#' @export
#' @keywords report
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatexGraphs.uncertainties(rriskModel=rriskModel,
#' LatexReport=TRUE,file.name="uncertainties.tex")  }

tolatexGraphs.uncertainties<-function(rriskModel,LatexReport=FALSE,file.name,pdfGraph=FALSE)
{ # initialize help variables
  modelUncertainties<-rriskModel@uncertainties@uncertainties
  modelSsystem<-rriskModel@scoring
  useNotApplicable<-rriskModel@settings@usenotapplicable
  #-----------------------------------------------------------------------------
  nameGraphs<-c()
  on.exit(return(invisible(nameGraphs)))
  
  if(length(modelSsystem@scoring)==0)
  { stop("No scoring system is defined for the current model --> model uncertainties graph(s) cannot be created!",call.=FALSE)
  }
  
  #-----------------------------------------------------------------------------
  if(length(modelUncertainties)>0)
  { uncertParts<-modelUncertainties[[1]]@namemain
    for(i in 2:length(modelUncertainties))
    { if(!is.element(modelUncertainties[[i]]@namemain,uncertParts))
      {  uncertParts<-c(uncertParts,modelUncertainties[[i]]@namemain)
      }
    }
    #---------------------------------------------------------------------------
    for(i in 1:length(uncertParts))
    { uncertList<-list()
      for(j in 1:length(modelUncertainties))
      { if(modelUncertainties[[j]]@namemain==uncertParts[i])
        { uncertList[[length(uncertList)+1]]<-modelUncertainties[[j]]
        }
      }
      if(LatexReport)  pdfGraph<-TRUE
      uncertMatrix<-uncertainties2matrix(uncertList,modelSsystem=modelSsystem)
      outputInfo<-suppressWarnings(plotUncertainties(uncertMatrix,modelSsystem=modelSsystem,
        useNotApplicable,maintext=uncertParts[i],pdfGraph,modelName=rriskModel@name@name))
      if(LatexReport==TRUE)
      { nameGraph<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_",gsub(x=uncertParts[[i]]," ",replacement=""),sep="")
        nameGraphs<-c(nameGraphs,nameGraph)
        captionGraph<-paste("Scoring of",tolower(uncertParts[i]),"and knowledge base:")
        
        for(k in 1:length(modelSsystem@scoring))
        { if(k==1) uncertText<-""
          temp<-modelSsystem@scoring[[k]]
          uncertTextTemp<-paste(" ",tolower(temp@name)," (",temp@notation,")",sep="")
          if(k!=length(modelSsystem@scoring))
          { uncertTextTemp<-paste(uncertTextTemp,",",sep="")
          }else  uncertTextTemp<-paste(uncertTextTemp,".",sep="")
          uncertText<-paste(uncertText,uncertTextTemp,sep="") 
        }
        captionGraph<-paste(captionGraph,uncertText)
        captionGraph<-paste(captionGraph,"The qualitative scores are represented as dots using light colour scheme to signal")
        for(k in 1:length(outputInfo$values))
        { if(k==1)uncertText<-c()
          temp1<-names(outputInfo$vmeanings)[which(outputInfo$vmeanings==outputInfo$values[k])]
          temp2<-names(outputInfo$vcolor)[which(outputInfo$vcolors==outputInfo$values[k])]
          uncertTextTemp<-paste(temp1,paste("(",temp2,")",sep=""))
          if(k==1)
          { uncertText<-paste(uncertText,uncertTextTemp,sep="")
          } else uncertText<-paste(uncertText,uncertTextTemp,sep=", ")
        }
        captionGraph<-paste(captionGraph,uncertText,"uncertainty.")
        if(!useNotApplicable)
        {  captionGraph<-paste(captionGraph,"No dots were drawn for criteria that were found not applicable, resulting omission of columns for following criteria",paste(paste(outputInfo$removed,collapse=", "),".",sep=""))
        }
        cat("
\\begin{figure}[p]
\\centering",append=TRUE,file = file.name)
cat(paste("\n\\includegraphics[width=1\\textwidth]{",nameGraph,"}",sep=""),append=TRUE,file = file.name)
cat("\n\\caption{",captionGraph,"}",append=TRUE,file = file.name)
cat(paste("\n\\label{fig:",nameGraph,"} \n\\end{figure}
\\clearpage
",sep=""),append=TRUE,file = file.name)
      }
    }
  } else cat("Model uncertainties list is empty --> No model uncertainties graph can be exported!\n")
} # end of function tolatexGraphs.uncertainties() 


 
################################################################################
################################################################################
#' @description Function that creates model network graph
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotModelNetwork
#' @aliases plotModelNetwork
#' @title Function creating model network graph
#' @usage plotModelNetwork(rriskModel,pdfGraph=FALSE)
#' @param rriskModel an instance of the \code{modelClass}
#' @param pdfGraph whether a PDF graphic should be created
#' @keywords graphs
#' @note This function requires some functions from the packages \code{network}, \code{grDevices} and \code{sna}.
#' @export
#' @examples                                    
#' \donttest{rriskModel<-init.Model1()
#' plotModelNetwork(rriskModel,pdfGraph=FALSE)}

plotModelNetwork<-function(rriskModel,pdfGraph=FALSE)
{ #-----------------------------------------------------------------------------
  # initialize help variables
  #-----------------------------------------------------------------------------
  modelItems<-rriskModel@items@items
  modelSettings<-rriskModel@settings
  modelSsystem<-rriskModel@scoring

  #-----------------------------------------------------------------------------
  # define help function
  #-----------------------------------------------------------------------------
  trafficCol<-function(x,range)
  { # function colorRampPalette() from package "grDevices"
    traffic.col <- colorRampPalette(c("green", "yellow", "red"),space = "rgb")
     min <- min(range[1],min(x))
     max <- max(range[2],max(x))
     b <- lm(c(1,20)~c(min,max))$coeff
     i <- round(b[[1]] + b[[2]]*x) # turns the 20 colour values into index values
     traffic.col <- traffic.col(20)[i]
     (traffic.col)
  } # end of function trafficCol 
  
  len<-length(modelItems)
  if(len>1){
    #---------------------------------------------------------------------------
    #  create model adjacency matrix
    #---------------------------------------------------------------------------
    for(i in 1:len){
      if(i==1) itemNames<-c()
      itemNames<-c(itemNames,modelItems[[i]]@name)
    } # end for
    modelAdjacency<-matrix(rep(0,len*len),ncol=len)
    colnames(modelAdjacency)<-itemNames
    rownames(modelAdjacency)<-itemNames
    for(i in 1:len){
      tempItem<-modelItems[[i]]
      #if(nchar(tempItem@depitem)>0){
        depItems<-strsplit(tempItem@depitem,split=" ") 
        if(length(depItems)>0){
          depItems<-depItems[[1]]
          depItems<-setdiff(depItems,"")
          depItems<-gsub(x=depItems," ",replacement="")
          #modelAdjacency[i,which(depItems==itemNames)]<-1
          modelAdjacency[i,which(is.element(itemNames,depItems))]<-1
        } # end  if(length(depItems)>0){
      #} # end  if(tempItem@depitem!="")
    } # end for
    #---------------------------------------------------------------------------
    # create a network object
    #---------------------------------------------------------------------------
    modelNetwork<-network(modelAdjacency)
   
    #---------------------------------------------------------------------------
    # define graph parameters
    #---------------------------------------------------------------------------
    v.size<-rep(4,len)
    v.shape<-rep(3,len)
    v.rot<-rep(0,len)
    for(i in 1:len)
    { typeTemp<-modelItems[[i]]@typecode
      if(typeTemp=="fnrv"){ # hexaeder
        v.size[i]<-v.size[i]*1.1
        v.shape[i]<-6
      } else if(typeTemp=="mcrv"){ # triangle default
        v.size[i]<-v.size[i]*1.6
        v.rot[i]<-90
      } else if(typeTemp=="numv"){
        v.size[i]<-v.size[i]*1.4
        v.rot[i]<-90
        v.shape[i]<-50 # circle
      } else if(typeTemp=="rsrv"){  # upside down triange
         v.size[i]<-v.size[i]*1.4
         v.rot[i]<-150
      } else if(typeTemp=="bsrv"){ # triangle
        v.size[i]<-v.size[i]*1.6
        v.rot[i]<-90
      } else if(typeTemp=="data"){
        v.size[i]<-v.size[i]*2.0
        v.shape[i]<-4 # square
        v.rot[i]<-45
      } else if(typeTemp=="stra"){ #pentahedron
        v.size[i]<-v.size[i]*1.5
        v.shape[i]<-5
        v.rot[i]<-18
      } else if(typeTemp=="fdoc"){ # rotated square
        v.size[i]<-v.size[i]*1.6
        v.shape[i]<-4
        v.rot[i]<-6 
      } else if (typeTemp=="bdjp"){ #right rotated triangle
        v.size[i]<-v.size[i]*1.6
      } else if (typeTemp=="mxrv") { # left rotated triangle
        v.size[i]<-v.size[i]*1.6
        v.rot<-(-60)
      } # end if(typeTemp=="fnrv"){ # hexaeder
      if(modelItems[[i]]@rolecode=="OF") v.size[i]<-4*1.8
    } # end for
    #---------------------------------------------------------------------------
    # define color for displaying items on the network graph
    #---------------------------------------------------------------------------
    # get colours for summary Scores
    sum.score<-rep(0,len)
    if(modelSettings@wscores == "equal"){
      w<-rep(1,length(modelSsystem@scoring))
    } else {
      w<-suppressWarnings(as.numeric(strsplit(modelSettings@wscores,split=" ")[[1]]))
    } # end if(modelSettings@wscores == "equal") 
    # mean score evaluated using weights and non-zero criteria
    for (i in 1:len){
      asnum<-modelItems[[i]]@scores
      sum.score[i] <- sum(asnum*w)
      if(sum(asnum) > 0) sum.score[i]/sum(asnum > 0)
    }# end for
    v.col<-trafficCol(x=sum.score,range=c(1,max(sum.score))) # minimum 1 or 1*ncol?
    for(i in 1:len){
      if(modelItems[[i]]@rolecode=="OF")v.col[i]<-modelSettings@mycol 
    } # end for 
    #---------------------------------------------------------------------------
    # create network plot
    #---------------------------------------------------------------------------
    if(pdfGraph==FALSE){
      X11()
      titleGraph<-gsub(x=rriskModel@name@name," ",replacement="")
      titleGraph<-paste("\\ref{fig:",titleGraph,"_modelNetwork","}",sep="")
      cat("Use for referencing to the model network graph following LaTeX commando: ",titleGraph,"\n")
      titleGraph<-paste("model network graph \n",titleGraph)
    } else {
      pdf(file=paste(gsub(x=rriskModel@name@name," ",replacement=""),"_modelNetwork.pdf",sep=""))
      titleGraph<-""
    } # end if if(pdfGraph==FALSE){
    par(mai=c(0,0,1,0)+0.1)
    suppressWarnings(plot.network(x=modelNetwork,label=itemNames,
      vertex.cex=v.size, vertex.rot=v.rot,vertex.sides=v.shape,vertex.col=v.col,
      arrowhead.cex=0.9,label.pos=8,boxed.labels=FALSE,label.pad=1,pad=0.2,
      vertices.last=FALSE,mode=modelSettings@nwlayout))
    suppressWarnings(title(titleGraph)) 
    if(pdfGraph==TRUE) dev.off()
  } else cat("The model items list is empty --> No model network graph can be exported!\n")
} # end of function plotModelNetwork()




################################################################################
################################################################################
#' @description Function that displays model concept graphs
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name displayConceptGraphs
#' @aliases displayConceptGraphs
#' @title Function that displays model concept graphs
#' @usage displayConceptGraphs(rriskModel,exportGraph=FALSE)
#' @param rriskModel an instance of the \code{modelClass}
#' @param exportGraph whether the graphic should be exported as well
#' @keywords graphs
#' @note This function requires some functions from the packages \code{png} and \code{rgdal}.
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' displayConceptGraphs(rriskModel) }

displayConceptGraphs<-function(rriskModel,exportGraph=FALSE)
{ #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  modelGraphs<-rriskModel@graphs@graphs

  fileNames<-c()
  on.exit(return(invisible(fileNames)))
  if(length(modelGraphs)>0)
  { for(i in 1:length(modelGraphs))
    { if(!is.null(modelGraphs[[i]]@graphdata))
      { # Idee zum Darstellung von Concept graphs in graphic device
        # (1) write concept graph in pnf file
        # (2) read png file and show fraph in R graphic device
        fileName<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_conceptGraph",i,".PNG",sep="")
        fileNames<-c(fileNames,fileName)
        # function writePNG from package "png"
        writePNG(modelGraphs[[i]]@graphdata,fileName)
        if(!exportGraph)
        { X11()
          # functions GDAL.open, getDriverLongName, getDriver, displayDataset, GDAL.close from package "rgdal"
          h <- GDAL.open(fileName) # returns a file handle
          getDriverLongName(getDriver(h))  # should report "Portable Network Graphics"
          displayDataset(h)
          graphRef<-gsub(x=rriskModel@name@name," ",replacement="")
          graphRef<-paste("\\ref{fig:",graphRef,"_conceptGraph",i,"}",sep="")
          cat("Use for referencing to model concept graph",i,"following LaTeX commando: ",graphRef,"\n")
          suppressWarnings(title(paste("model concept graph",i,"\n",graphRef)))
          GDAL.close(h)
          file.remove(fileName)
        }
      } else print("No graph data available for displaying --> No model concept graph can be exported.")
    }
  } else cat("The model graphs list is empty --> No model concept graphs can be exported.\n")
}



################################################################################
################################################################################
#' @description Function that creates LaTeX code for displaying model uncertainties graphs
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.concept
#' @aliases tolatexGraphs.concept
#' @title Function creating LaTeX code for including model uncertainties graphs
#' @usage tolatexGraphs.concept(rriskModel,file.name)
#' @param rriskModel an instance of the \code{modelClass}
#' @param file.name the file that saves tex codes to import the concept graphics
#' @keywords report
#' @note This function requires some functions from the package \code{png}.
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatexGraphs.concept(rriskModel, file.name="concept.tex")  }

tolatexGraphs.concept<-function(rriskModel,file.name)
{ 
   modelGraphs<-rriskModel@graphs
  #-----------------------------------------------------------------------------
  # create model concept graph in png format (-> output file = conceptGraph.png)
  #-----------------------------------------------------------------------------
  #require(png)
  if(length(modelGraphs@graphs)>0)
  { for(i in 1:length(modelGraphs@graphs))
    { graphName<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_conceptGraph",i,".png",sep="")
      if(!is.null(modelGraphs@graphs[[i]]@graphdata))
      { # function writePNG from package "png"
        try.result<-try(writePNG(modelGraphs@graphs[[i]]@graphdata,graphName),silent=TRUE)
        if (!inherits(try.result, "try-error"))
        {  cat("
\\begin{figure}[p]
\\centering",append=TRUE,file = file.name)
cat(paste("\n\\includegraphics[width=1\\textwidth]{",graphName,"}",sep=""),append=TRUE,file = file.name)
cat("\n\\caption{",modelGraphs@graphs[[i]]@explanation,"}",append=TRUE,file = file.name)
cat(paste("\n\\label{fig:",graphName,"}
\\end{figure}
\\clearpage",sep=""),append=TRUE,file = file.name)
        }
      }     
    } # end for
  } # end if
} # end of function tolatexGraphs.concept() 



################################################################################
################################################################################
#' @description Function that creates LaTeX code for including model uncertainties graphs
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.network
#' @aliases tolatexGraphs.network
#' @title Function creating LaTeX code for including model uncertainties graphs
#' @usage tolatexGraphs.network(rriskModel,file.name)
#' @param rriskModel an instance of the \code{modelClass}
#' @param file.name character, name of the tex file where code for creating model
#' uncertainties graphs should be written
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatexGraphs.network(rriskModel,file.name="figcode.tex")}

tolatexGraphs.network<-function(rriskModel,file.name)
{ #-----------------------------------------------------------------------------
  # initialize help variables
  #-----------------------------------------------------------------------------
  modelItems<-rriskModel@items@items
  modelSettings<-rriskModel@settings
  modelSsystem<-rriskModel@scoring
  
  #-----------------------------------------------------------------------------
  # create model network graph in pdf format (-> output file = modelNetwork.pdf)
  #-----------------------------------------------------------------------------
  if(length(modelItems)>1)
  { plotModelNetwork(rriskModel,pdfGraph=TRUE)
    captionGraph<-"Dependencies among model items visualised as graph. The shape of items corresponds to the item type (square=\\tdata, rotated square=\\tfdoc, triangle=\\tmcrv,
    \\tbsrv, upside-down triangle=\\trsrv, left rotated triangle=\\tmxrv, right rotated triangle=\\tbdjp, circle=\\tnumv, pentahedron=\\tstra-item triplet and hexahedron=\\tfnrv-items). 
    The colours of the items were adjusted between green and red, reflecting the 
    lowest (most uncritical) and highest (most critical) level of uncertainty and knowledge base, respectively. The"
    captionGraph<-paste(captionGraph,length(modelSsystem@scoring),"qualitative scores were combined with")
    weightsInfo<-modelSettings@wscores
    if(gsub(x=weightsInfo," ",replacement="")=="equal")
    { weightsInfo<-"equal weights."
    } else
    { weightsInfoTemp<-paste(weightsInfo,collapse=", ")
      weightsInfo<-paste("equal weights proportional to ",weightsInfoTemp,".",sep="")
    }
    captionGraph<-paste(captionGraph,weightsInfo,"The item representing the outcome function(s) (OF) is shaded in ")
    captionGraph<-paste(captionGraph,modelSettings@mycol,".",sep="")
    graphFileName<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_modelNetwork",sep="")
    cat("
\\begin{figure}[p]
\\centering\n",append=TRUE,file = file.name)
cat(paste("\\includegraphics[width=1\\textwidth]{",graphFileName,"}\n",sep=""),
"\\caption{",captionGraph,"}",
paste("\\label{fig:",graphFileName,"}\n",sep=""),
"\\end{figure}
\\clearpage
",append=TRUE,file = file.name)
  } else cat("The items list is empty, model network graph cannot be created!\n")
} # end of function tolatexGraphs.network() 



################################################################################
################################################################################
#' @description Function that produces menu element and catches the input of the user
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name mymenu1
#' @aliases mymenu1
#' @title Function that produces menu element and catches the input of the user
#' @usage mymenu1(title="Hi",choices="y",codes=1:length(choices),part="NA",
#' help="No further help available",levelTabulator)
#' @param title the title
#' @param choices choice made my the user
#' @param codes vector from 1 to the length of choices
#' @param part part
#' @param help whether further help is available
#' @param levelTabulator numeric indicating the level of the menu
#' @return choice
#' @keywords misc
#' @export
#' @examples
#' \donttest{mymenu1(title="Hi",choices="y",codes=1:length("y"),part="NA",
#' help="No further help available",levelTabulator=1)}

mymenu1<-function(title="Hi",choices="y",codes=1:length(choices),part="NA",help="No further help available",levelTabulator)
{ codes.len <- length(codes)
  cat(paste(levelTabulator,title))
  cat(levelTabulator,rep("-",35))
  for (i in 1: codes.len)
  { cat(levelTabulator,codes[i],"\t",choices[i])
  }       
  condition<-TRUE
  while(condition)
  { cat(levelTabulator,rep("-",35),levelTabulator,"")
    choice <- readline()
    choice<-strsplit(choice,split=" ")[[1]]
    choice<-gsub(x=choice," ",replacement="")
    choice<-setdiff(choice,"")
    choice<-unlist(apply(as.matrix(choice,ncol=1),1,function(x)eval(parse(text=x))))
    #choice<-suppressWarnings(as.numeric(choice))
    if(length(choice)==0)
    { condition<-TRUE
    } else if(length(choice)==1)
    { if(is.element(choice,codes)) condition<-FALSE
    } else if(setequal(intersect(choice,codes[1:(length(codes)-1)]),choice))
    { condition<-FALSE
    }
  }
  return(choice)
} # end of function mymenu1()



################################################################################
################################################################################
#' @description Function mymenu that produces menu element and catches the input of the user
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name mymenu
#' @aliases mymenu
#' @title Function that produces menu element and catches the input of the user
#' @usage mymenu(title="Hi",choices="y",codes=1:length(choices),part="NA",
#'    help="No further help available",levelTabulator)
#' @param title the title
#' @param choices choice made my the user
#' @param codes vector from 1 to the length of choices
#' @param part part
#' @param help whether further help is available
#' @param levelTabulator numeric indicating the level of the menu
#' @export
#' @keywords misc
#' @examples
#' \donttest{mymenu(title="Hi",choices="x",codes=1:length("x"),
#' part="NA",help="No further help available",levelTabulator=1)}

mymenu<-function(title="Hi",choices="y",codes=1:length(choices),part="NA",help="No further help available",levelTabulator)
{ codes.len <- length(codes)
  cat(paste(levelTabulator,title))
  cat(levelTabulator,rep("-",35))
  for (i in 1: codes.len)
  { cat(levelTabulator,codes[i],"\t",choices[i])
  }
  choice <- -1
  while (!is.element(choice,c(codes)))
  { cat(levelTabulator,rep("-",35),levelTabulator,"")
    choice <- readline()
    if(choice == "?")
    { cat("",rep(".",43),"\n Valid entries are:",codes,"?","\n","Help:",help,"\n",rep(".",43),"\n")
    }
  }
  return(choice)
}


################################################################################
################################################################################
#' @description Function that displays all available models of an \code{\linkS4class{rriskClass}} object
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name showAvailableModels
#' @aliases showAvailableModels
#' @title Function that display all available models of an 'rriskClass' object
#' @usage showAvailableModels(rriskSession)
#' @param rriskSession an instance of the \code{rriskClass}
#' @keywords model
#' @export
#' @examples
#' \donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
#' showAvailableModels(rriskSession)}

showAvailableModels<-function(rriskSession)
{ if(length(rriskSession@models)>0)
  { for(i in 1:length(rriskSession@models))
    { sumUp(rriskSession@models[[i]])
      cat("\n\n")
    }
  } else cat("The rrisk models list is empty. \n\n")
} # end of fucntion sumupSession()



################################################################################
################################################################################
#' @description Function that creates the about-information of the \code{rrisk} in a window
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name rriskAbout
#' @aliases rriskAbout
#' @title Function that creates the about-information of the rrisk in a window
#' @usage rriskAbout(disclaimer, demomode)
#' @param disclaimer the slot disclaimer of the instance of the \code{rriskClass} object
#' @param demomode whether the function be run under demo mode
#' @keywords rrisk
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
#' rriskAbout(rriskSession@@disclaimer)}

rriskAbout<-function(disclaimer, demomode = FALSE)
{ 
  if(demomode)
  { graphLogo <- "logoBfR.PNG"
  } else
  { graphLogo <- system.file("img", "logoBfR.PNG", package="rrisk")
  }

  aboutWindow<-tktoplevel(width=600,height=600)
  tkwm.title(aboutWindow,"About rrisk")
  tkwm.resizable(aboutWindow,0,0)  # fixed size, not resizeable
  stickyVar<-"nw"
  allFrame<-tkframe(aboutWindow)

  aboutLabel<-ttklabel(allFrame,text="ABOUT RRISK")
  aboutText<-ttklabel(allFrame,text=disclaimer$ABOUT,wraplength=500)
  tkgrid(aboutLabel,aboutText,sticky="nw",ipadx=10,pady=c(0,15))
  
  copyLabel<-ttklabel(allFrame,text="COPYRIGHT")
  copyText<-ttklabel(allFrame,text=disclaimer$COPYRIGHT,wraplength=500)
  tkgrid(copyLabel,copyText,sticky="nw",ipadx=10,pady=c(0,15))
  
  authorsLabel<-ttklabel(allFrame,text="AUTHORS")
  authorsFrame<-tkframe(allFrame,width=100)
  # BfRlogo<-tkimage.create("photo",file="logoBfR.png")
  BfRlogo<-tkimage.create("photo",file=graphLogo)

  logoLabel<-ttklabel(authorsFrame,image=BfRlogo)
  authorsText<-ttklabel(authorsFrame,text=disclaimer$AUTHORS)
  tkpack(authorsText,side="left")
  tkpack(logoLabel,side="right")
  tkgrid(authorsLabel,authorsFrame,sticky="nw",ipadx=10,pady=c(0,15))
  
  contactLabel<-ttklabel(allFrame,text="CONTACT")
  contactText<-ttklabel(allFrame,text=disclaimer$CONTACT,wraplength=500)
  tkgrid(contactLabel,contactText,sticky="nw",ipadx=10,pady=c(0,15))

  notesLabel<-ttklabel(allFrame,text="NOTES")
  notesText<-ttklabel(allFrame,text=disclaimer$NOTES,wraplength=500)
  tkgrid(notesLabel,notesText,sticky="nw",ipadx=10,pady=c(0,15))

  discLabel<-ttklabel(allFrame,text="DISCLAIMER")
  discText<-ttklabel(allFrame,text=disclaimer$DISCLAIMER,wraplength=500)
  tkgrid(discLabel,discText,sticky="nw",ipadx=10,pady=c(0,15))
  tkpack(allFrame,pady=c(20,0),padx=c(10,10))

  okButton<-ttkbutton(aboutWindow,text="Ok",command=function() tkdestroy(aboutWindow))
  tkpack(okButton,pady=c(0,20))

  tkfocus(aboutWindow)
  tkwait.window(aboutWindow)
} # end of rriskAbout()


################################################################################
################################################################################
#' @description Function that creates the window that asks for users' agreement
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name rriskAgreement
#' @aliases rriskAgreement
#' @title Function that creates the the window that asks for users' agreement
#' @usage rriskAgreement(demomode = FALSE)
#' @param demomode whether the function be run under demo mode
#' @keywords rrisk
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{rriskAgreement()}

rriskAgreement <- function(demomode=FALSE) {
  if(demomode){
    graphLogo1 <- "logoBfR.png"
    #graphLogo2 <- "car.png"
  } else{
    graphLogo1 <- system.file("img", "logoBfR.PNG", package="rrisk")
    #graphLogo2 <- system.file("img", "logoStatUp.PNG", package="rrisk")
  }
  # Umgebung und Umgebungsvariable
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("output.temp",value=FALSE,envir=tempEnvir)
  # Action-Funktionen
  checkbuttonAction<-function() {
    if(tclvalue(acceptedTcl)=="1")  tkconfigure(okButton,state="normal")
    else  tkconfigure(okButton,state="disabled")
  }
  onCancel<-function(){
    tkdestroy(agreementWindow)
    assign("output.temp",value=FALSE,envir=tempEnvir)
  }
  onNext<-function() {
    assign("output.temp",value=TRUE,envir=tempEnvir)
    tkdestroy(agreementWindow)
  }
  
  aboutLabelText <- "rrisk is is a set of functions running in the distributed rrisk-workspace using the R software package (R Development Core Team (2007). R: A Language and Environment forStatistical Computing, R Foundation for Statistical Computing, Vienna, Austria, ISBN 3-900051-07-0, http://www.R-project.org.)."
  copyrightLabelText <- "Copyright 2009 [under open source GNU licensing agreement of R] \nFederal Institute for Risk Assessment (BfR), Germany"
  contactLabelText <- "matthias.greiner@bfr.bund.de\nwww.bfr.bund.de/cd/52162"
  authorsLabelText <- "Matthias Greiner (BfR) \nKristin Tolksdorf (BfR) \nChristine Mueller-Graf (BfR) \nJuliane Braeunig (BfR) \nNatalia Belgorodski (STAT-UP) \nYinchong Yang (STAT-UP) \nKatharina Schueller (STAT-UP) \nMarkus Junker (MaSt services)"
  notesLabelText <- "rrisk is a project under development. Contributions are welcome!"
  disclaimerLabelText <- "THE rrisk SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
  
  # GUI aufbauen
  agreementWindow<-tktoplevel(width=600,height=600)
  tkwm.title(agreementWindow,"About rrisk")
  tkwm.resizable(agreementWindow,0,0)  # fixed size, not resizeable
  stickyVar<-"nw"
  acceptedTcl<-tclVar(FALSE)
  allFrame<-tkframe(agreementWindow)

  aboutLabel<-ttklabel(allFrame,text="ABOUT RRISK")
  aboutText<-ttklabel(allFrame,text=aboutLabelText,wraplength=500)
  tkgrid(aboutLabel,aboutText,sticky="nw",ipadx=10,pady=c(0,15))

  copyLabel<-ttklabel(allFrame,text="COPYRIGHT")
  copyText<-ttklabel(allFrame,text=copyrightLabelText,wraplength=500)
  tkgrid(copyLabel,copyText,sticky="nw",ipadx=10,pady=c(0,15))

  authorsLabel<-ttklabel(allFrame,text="AUTHORS")
  authorsFrame<-tkframe(allFrame,width=100)
  BfRlogo<-tkimage.create("photo",file=graphLogo1)
  #StatUplogo<-tkimage.create("photo",file=graphLogo2)

  logoLabel1<-ttklabel(authorsFrame,image=BfRlogo)
  #logoLabel2<-ttklabel(authorsFrame,image=StatUplogo)
  authorsText<-ttklabel(authorsFrame,text=authorsLabelText)
  tkpack(authorsText,side="left")
  tkpack(logoLabel1,side="right")
  #tkpack(logoLabel2,side="right")
  tkgrid(authorsLabel,authorsFrame,sticky="nw",ipadx=10,pady=c(0,15))

  contactLabel<-ttklabel(allFrame,text="CONTACT")
  contactText<-ttklabel(allFrame,text=contactLabelText,wraplength=500)
  tkgrid(contactLabel,contactText,sticky="nw",ipadx=10,pady=c(0,15))

  notesLabel<-ttklabel(allFrame,text="NOTES")
  notesText<-ttklabel(allFrame,text=notesLabelText,wraplength=500)
  tkgrid(notesLabel,notesText,sticky="nw",ipadx=10,pady=c(0,15))

  discLabel<-ttklabel(allFrame,text="DISCLAIMER")
  discText<-ttklabel(allFrame,text=disclaimerLabelText,wraplength=500)
  tkgrid(discLabel,discText,sticky="nw",ipadx=10,pady=c(0,15))

  tkpack(allFrame,pady=c(20,0),padx=c(10,10))

  acceptedCheckbutton<-tkcheckbutton(allFrame,variable=acceptedTcl,command=checkbuttonAction)
  tkgrid(acceptedCheckbutton,row=6,column=0,sticky=stickyVar)
  tkgrid(ttklabel(allFrame,text="I accept these conditions",wraplength=500),pady=c(0,15),row=6,column=1,sticky=stickyVar)

  buttonsFrame<-tkframe(allFrame)
  okButton<-ttkbutton(buttonsFrame,text="Next",state="disabled",command=onNext)
  tkpack(okButton,side="left",padx=c(60,40))
  cancelButton<-ttkbutton(buttonsFrame,text="Cancel",command=onCancel)
  tkpack(cancelButton,side="right",padx=c(60,40))
  tkgrid(buttonsFrame,row=7,column=1,sticky=stickyVar, pady = c(0, 15))

  tkfocus(agreementWindow)
  tkwait.window(agreementWindow)

  # get Umgebungsvariable
  ret <- get("output.temp",tempEnvir)
  return(ret)
} # end of function rriskAgreement()



################################################################################
################################################################################
#' @description Function loads external reference information
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name referencesLoad
#' @aliases referencesLoad
#' @title Function loads external reference information
#' @usage referencesLoad()
#' @export
#' @keywords model
#' @examples
#' \donttest{referencesLoad()}

referencesLoad<-function() 
  { output<-""
    on.exit(return(output))
    referencesFile<-tclvalue(tkgetOpenFile(filetypes=" {{BibTeX files} {.bib}} "))
    if(nchar(referencesFile)>0)
    {
      try.result<-try(tempReferences<-readLines(referencesFile),silent=TRUE)
      if (inherits(try.result, "try-error"))
      { tkmessageBox(message=paste("The reference list could NOT be loaded from the file '",referencesFile,"'!"),icon="error",type="ok")
      } else
      { tkmessageBox(message="The reference list has been successfully loaded into the rrisk session!")
         output<-tempReferences
      }   
    }
  }  # end of referencesLoad()
  
 
 
 
################################################################################
################################################################################
#' @description Function that loads external models
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name modelLoad
#' @aliases modelLoad
#' @title Function that loads external models
#' @usage modelLoad(rriskSessionModels)
#' @param rriskSessionModels the slot models of the instance of the \code{rriskClass} object
#' @keywords model
#' @export
#' @examples
#' \donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
#' modelLoad(rriskSession@@disclaimer)}

modelLoad<-function(rriskSessionModels)
{ modelLoad.under<-function() # help function
  { on.exit(return(NULL))
    modelFile<-tclvalue(tkgetOpenFile(filetypes=" {{R images} {.Rdata}} "))
    if(nchar(modelFile)>0)
    {
      try.result<-try(tempModel<-load(modelFile))
      if (inherits(try.result, "try-error"))
      { tkmessageBox(message=paste("The model could NOT be loaded from the file '",modelFile,"'!"),icon="error",type="ok")
      } else
      { newModelName<-basename(modelFile)
        newModelName<-sub(x=newModelName,".Rdata",replacement="")
        #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
        assign("tempEnvir",value=new.env())
        assign(make.names(newModelName),value=get(tempModel),envir=tempEnvir)
        output<-get(make.names(newModelName),envir=tempEnvir)
        if(!is(output,"modelClass"))
        { tkmessageBox(message="The specified .Rdata object is not an instance of the required class 'modelClass'!")
          stop("The specified .Rdata object is not an instance of the required class 'modelClass'!",call.=FALSE)
        } else {
          tkmessageBox(message="The model has been successfully loaded into the rrisk session!")
          on.exit(return(output))
        }
      }
    }
  }  # end of modelLoad()
  newModel<-modelLoad.under()
  output<-rriskSessionModels
  if(is(newModel,"modelClass"))
  { newModelName<-newModel@name@name
    modelNamesAvailable<-c()
    if(length(rriskSessionModels)>0)
    { for(i in 1:length(rriskSessionModels))
      { modelNamesAvailable<-c(modelNamesAvailable,rriskSessionModels[[i]]@name@name)
      }
      repeat
      { newModelName<-rrisk.DialogString(message="Please enter the name of the new model:",default=newModel@name@name)
        if(is.null(newModelName)) {
          cat("\nA new model has not been saved into the rrisk session.\n")
          break()
        }
        if(is.element(newModelName,modelNamesAvailable) | gsub(x=newModelName," ",replacement="")==""){
          cat("\nEither the model name is an empty string or there is any other model in the rrisk session this the same name.\n")
          next()
        } else {
          break()
        }
      }
    }
    if(!is.null(newModelName))
    { newModel@name@name<-newModelName
      modelsNumber<-length(rriskSessionModels)
      rriskSessionModels[[modelsNumber+1]]<-newModel
      output<-rriskSessionModels
      cat("\nA new model has been created successfully!\n")
    }
  }
  return(output)
} # end of fucntion modelLoad()



################################################################################
################################################################################
#' @description Function that loads external graphics
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name graphLoad
#' @aliases graphLoad
#' @title Function that loads external graphics
#' @usage graphLoad()
#' @keywords model
#' @export
#' @examples
#' \donttest{graphLoad()}

graphLoad<-function() # help function
  { output<-NULL
    graphFile<-tclvalue(tkgetOpenFile(filetypes=" {{png objects} {.png}} "))
    if(nchar(graphFile)>0)
    {
      try.result<-try(tempGraph<-readPNG(source=graphFile))
      if (inherits(try.result, "try-error"))
      { tkmessageBox(message=paste("The concept graph could NOT be loaded from the file '",graphFile,"'!"),icon="error",type="ok")
      } else
      { tkmessageBox(message="The concept graph has been successfully loaded into the rrisk session!")
        output<-tempGraph
      }   
    }
    return(output)
  }  # end of graphLoad()
  
 # graphLoad()



################################################################################
################################################################################
#' @description Function that loads external scoring systems
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name ssystemLoad
#' @aliases ssystemLoad
#' @title Function that loads external scoring systems
#' @usage ssystemLoad(rriskSessionSsystems)
#' @param rriskSessionSsystems the slot scoringsystems of the instance of the \code{rriskClass} object
#' @keywords scoring
#' @export
#' @examples
#' \donttest{rriskSession <- init.rriskSession(useDemoModels = "all") 
#' ssystemLoad(rriskSession@@scoringsystems)}

ssystemLoad<-function(rriskSessionSsystems)
{ ssystemLoad.under<-function() # help function
  { on.exit(return(NULL))
    ssystemFile<-tclvalue(tkgetOpenFile(filetypes=" {{R images} {.Rdata}} "))
    if(nchar(ssystemFile)>0)
    {
      try.result<-try(tempSsystem<-load(ssystemFile))
      if (inherits(try.result, "try-error"))
      { tkmessageBox(message=paste("The scoring system could NOT be loaded from the file '",ssystemFile,"'!"),icon="error",type="ok")
      } else
      { newSsystemName<-basename(ssystemFile)
        newSsystemName<-sub(x=newSsystemName,".Rdata",replacement="")  
        #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
        assign("tempEnvir",value=new.env())
        assign(make.names(newSsystemName),value=get(tempSsystem),envir=tempEnvir)
        output<-get(make.names(newSsystemName),envir=tempEnvir)
        if(!is(output,"modelScoringClass"))
        { tkmessageBox(message="The specified .Rdata object is not an instance of the required class 'modelScoringClass'!")
          stop("The specified .Rdata object is not an instance of the required class 'modelScoringClass'!",call.=FALSE)
        } else
        { on.exit(return(output))
          tkmessageBox(message="The scoring system has been successfully loaded into the rrisk session!")
        }
      }   
    }
  }  # end of ssystemLoad.under()
  newSsystem<-ssystemLoad.under()
  output<-rriskSessionSsystems
  if(!is.null(newSsystem))
  { newSsystemName<-newSsystem@name
    ssystemNamesAvailable<-c()
    if(length(rriskSessionSsystems)>0)
    { for(i in 1:length(rriskSessionSsystems))
      { ssystemNamesAvailable<-c(ssystemNamesAvailable,rriskSessionSsystems[[i]]@name)
      }
      repeat
      { newSsystemName<-rrisk.DialogString("Please enter the name of the new scoring system:",default=newSsystemName)
        if(is.null(newSsystemName)) break()
        ifelse(is.element(newSsystemName,ssystemNamesAvailable) | gsub(x=newSsystemName," ",replacement="")=="",next(),break())
      }
    }
    if(!is.null(newSsystemName))
    { newSsystem@name<-newSsystemName
      ssystemsNumber<-length(rriskSessionSsystems)
      rriskSessionSsystems[[ssystemsNumber+1]]<-newSsystem
      output<-rriskSessionSsystems
      tkmessageBox(title="User information",icon="info",type="ok",
       message="A new scoring system has been successfully created. Please, use 'Edit scoring system' menu for further modifications!")
    } 
  }
  return(output)
} # end of ssystemLoad()



################################################################################
################################################################################
#' @description Function that saves a model object
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name modelSave
#' @aliases modelSave
#' @title Function that saves a model object 
#' @usage modelSave(rriskModel)
#' @param rriskModel an instance of the \code{modelClass} object
#' @keywords model
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' modelSave(rriskModel)}

modelSave<-function(rriskModel)
{ 
  rriskModel@modeltype<-"user defined"
  modelFile<-rriskModel@name@name
  modelFile<-tclvalue(tkgetSaveFile(filetypes=" {{R images} {.Rdata}} ",initialfile=paste(modelFile,".Rdata",sep="")))
  if(nchar(modelFile)>0)
  { try.result<-try(save(file=modelFile,list="rriskModel"))
    if (inherits(try.result, "try-error")){
      tkmessageBox(message=paste("The model could NOT be written into the file '",modelFile,"'!"),icon="error",type="ok")
      cat("\nThe model could NOT be written into the file '",modelFile,"'!\n")
    } else {
      tkmessageBox(message = paste("The model was successful saved under ",modelFile))
      cat("\nThe model was successful saved under '",modelFile,"'.\n")   
    } # end  if (inherits(try.result, "try-error")){
  } # end if(nchar(modelFile)>0)
}  # end of modelSave()


################################################################################
################################################################################
#' @description Function that saves a scoring system object
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name ssystemSave
#' @aliases ssystemSave
#' @title Function that saves a scoring system object 
#' @usage ssystemSave(rriskSsystem)
#' @param rriskSsystem the slot scoringsystems of an instance of the \code{rriskSession} object
#' @export
#' @keywords scoring
#' @examples
#' \donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
#' ssystemSave(rriskSession@@scoringsystems[[1]])}

ssystemSave<-function(rriskSsystem)
{ 
  rriskSsystem@systemtype<-"user defined"
  ssystemFile<-rriskSsystem@name
  ssystemFile<-tclvalue(tkgetSaveFile(filetypes=" {{R images} {.Rdata}} ",initialfile=paste(ssystemFile,".Rdata",sep="")))
  if(nchar(ssystemFile)>0)
  { try.result<-try(save(file=ssystemFile,list="rriskSsystem"))
    if (inherits(try.result, "try-error"))
    { tkmessageBox(message=paste("The scoring system could NOT be written into the file '",ssystemFile,"'!"),icon="error",type="ok")
    } else
    { tkmessageBox(message = paste("The scoring system was successful saved under ",ssystemFile))   
    }
  }
}  # end of ssystemSave()



################################################################################
################################################################################
#' @description Function that creates an empty model
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name createEmptyModel
#' @aliases createEmptyModel
#' @title Function that creates an empty model
#' @usage createEmptyModel(rriskSessionModels)
#' @param rriskSessionModels the slot models of an instance of the \code{rriskSession} object
#' @export
#' @keywords model
#' @examples
#' \donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
#' createEmptyModel(rriskSession@@models)}

createEmptyModel<-function(rriskSessionModels)
{ on.exit(return(rriskSessionModels))
  #----------------------------------------------
  modelNamesAvailable<-c()
  if(length(rriskSessionModels)>0){
    for(i in 1:length(rriskSessionModels)){
      modelNamesAvailable<-c(modelNamesAvailable,rriskSessionModels[[i]]@name@name)
    } # end for
  } # end if
  #----------------------------------------------
  repeat
  { newModelName<-rrisk.DialogString("Please enter the name of the new model:",default="")
    if(is.null(newModelName)){
      cat("\nA new model has not been saved into the rrisk session.\n")
      break()
    } else if(is.element(newModelName,modelNamesAvailable) | gsub(x=newModelName," ",replacement="")==""){
      cat("\nEither the model name is an empty string or there is any other model in the rrisk session this the same name.\n")
      next()
    } else {
      break()
    }
  }
  #------------------------------------------------
  if(!is.null(newModelName))
  { index<-length(rriskSessionModels)
    rriskSessionModels[[index+1]]<-new("modelClass",name=new("modelNameClass",name=newModelName))
    cat("\nA new model has been created successfully!\n")
  }
}  # end of createEmptyModel()




################################################################################
################################################################################
#' @description Function that creates an empty scoring system 
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name createEmptySsystem
#' @aliases createEmptySsystem
#' @title Function that creates an empty scoring system
#' @usage createEmptySsystem(rriskSessionSsystems)
#' @param rriskSessionSsystems the slot scoringsystems of an instance of the \code{rriskSession} object
#' @keywords scoring
#' @export
#' @examples
#' \donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
#' createEmptySsystem(rriskSession@@scoringsystems)}

createEmptySsystem<-function(rriskSessionSsystems)
{ on.exit(return(rriskSessionSsystems))
  ssystemNamesAvailable<-c()
  for(i in 1:length(rriskSessionSsystems))ssystemNamesAvailable<-c(ssystemNamesAvailable,rriskSessionSsystems[[i]]@name)
  repeat
  { newSsystemName<-rrisk.DialogString("Please enter the name of the new scoring system:",default="")
    if(is.null(newSsystemName)) break()
    ifelse(is.element(newSsystemName,ssystemNamesAvailable) | gsub(x=newSsystemName," ",replacement="")=="",next(),break())
  }
  if(!is.null(newSsystemName))
  { index<-length(rriskSessionSsystems)
    tempSystem<-new("modelScoringClass")
    tempSystem@name<-newSsystemName
    rriskSessionSsystems[[index+1]]<-tempSystem
    tkmessageBox(title="User information",icon="info",type="ok",
      message="A new empty scoring system has been successfully created. Please, use 'Edit scoring system' menu for further modifications!")
  }
}  # end of createEmptySsystem()

#createEmptySsystem(rriskSession@scoringsystems)


################################################################################
################################################################################
#' @description Auxiliary function for the \code{writeOut} method of scoring system object
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name writeOutScoringsystemClass
#' @aliases writeOutScoringsystemClass
#' @title Auxiliary function for the writeOut method of scoring system object
#' @usage writeOutScoringsystemClass(object,file.name)
#' @param object the slot scoringsystems of an instance of the \code{rriskSession} object
#' @param file.name file that saves the write-outs
#' @keywords scoring
#' @export
#' @examples
#' \donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
#' writeOutScoringsystemClass(rriskSession@@scoringsystems[[1]],"ssystem.txt")}

writeOutScoringsystemClass<-function(object,file.name)
{
  suppressWarnings(file.remove(file.name))
  #-------------------------------------------------------------------------------
  # write out file header
  #-------------------------------------------------------------------------------
  cat("################################################################\n",file=file.name,append=TRUE)
  cat(paste("# rrisk Scoring System Definition Template (Rsdt) ", date(),"\n"),file=file.name,append=TRUE)
  cat("# DO NOT CHANGE LINES BEGINNING WITH '#' \n",file=file.name,append=TRUE)
  cat("################################################################\n",file=file.name,append=TRUE)
  cat("\n",file=file.name,append=TRUE)
  #-------------------------------------------------------------------------------
  # write out scoring system
  #-------------------------------------------------------------------------------  
  cat("#~~~ Qualitative scheme for assessing uncertainty and knowledge base ~~~#\n",file=file.name,append=TRUE) 
  cat("name: ",object@name,"\n",file=file.name,append=TRUE)
  cat("tableheader: ",putLineBreaks(object@tableheader),"\n",file=file.name,append=TRUE)
  cat("explanatory: ",putLineBreaks(object@explanatory),"\n",file=file.name,append=TRUE)
  cat("values: ",object@values,"\n",file=file.name,append=TRUE)
  #---------------------------------------------------------------------------
    if(length(object@vcolors)>0)
    { for(i in 1:length(object@vcolors))
      { if(i==1) vcolors<-c()
        vcolorsTemp<-paste(object@vcolors[i],"=",names(object@vcolors)[i],sep="")
        vcolors<-c(vcolors,vcolorsTemp)
      }
    } else vcolors=""
  cat("vcolors: ",vcolors,"\n",file=file.name,append=TRUE)
  #---------------------------------------------------------------------------
  if(length(object@vmeanings)>0)
  { for(i in 1:length(object@vmeanings))
    { if(i==1) vmeanings<-c()
      vmeaningsTemp<-paste(object@vmeanings[i],"=",names(object@vmeanings)[i],sep="")
      vmeanings<-c(vmeanings,vmeaningsTemp)
    }
  } else vmeanings=""
  cat("vmeanings: ",vmeanings,"\n\n",file=file.name,append=TRUE)
  #---------------------------------------------------------------------------
  if(length(object@scoring)>0)
  { for(i in 1:length(object@scoring))
    { temp<-object@scoring[[i]]
      explanation<-temp@explanation
      if(!is.null(explanation)) explanation<-putLineBreaks(explanation)  
      cat("#~~~~~~#\n",file=file.name,append=TRUE) 
      cat("symbol: ",temp@notation,"\n",file=file.name,append=TRUE)
      cat("name: ",temp@name,"\n",file=file.name,append=TRUE)
      cat("explanation: ",explanation,file=file.name,append=TRUE)
      cat("\n\n",file=file.name,append=TRUE)
    }
  } 
}  # end of function writeOutScoringsystemClass()



################################################################################
################################################################################
#' @description Auxiliary function for the \code{writeOut} method of class \code{\linkS4class{modelClass}}.
#'
#' @name writeOutModelClass
#' @aliases writeOutModelClass
#' @title Non-executable auxiliary function
#' @usage writeOutModelClass(object,file.name)
#' @param object an instance of the \code{modelClass} object
#' @param file.name the file which saves the write-outs
#' @keywords model
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' writeOutModelClass(rriskModel,"rriskModel.txt")}

writeOutModelClass<-function(object,file.name)
{
  suppressWarnings(file.remove(file.name))

  #-------------------------------------------------------------------------------
  # write out file header
  #-------------------------------------------------------------------------------
  cat("################################################################\n",file=file.name,append=TRUE)
  cat(paste("# rrisk Model Definition Template (Rmdt) ", date(),"\n"),file=file.name,append=TRUE)
  cat("# DO NOT CHANGE LINES BEGINNING WITH '#' \n",file=file.name,append=TRUE)
  cat("################################################################\n",file=file.name,append=TRUE)
  cat("\n",file=file.name,append=TRUE)

  #-------------------------------------------------------------------------------
  # write out model name
  #-------------------------------------------------------------------------------
  cat("#~~~ Name of the model ~~~#",file=file.name,append=TRUE)
  cat("\n",file=file.name,append=TRUE)
  cat("name: ",object@name@name,"\n",file=file.name,append=TRUE)
  cat("\n",file=file.name,append=TRUE)
  
  #-------------------------------------------------------------------------------
  # write out authors
  #-------------------------------------------------------------------------------
  if(length(object@authors@authors)>0)
  { cat("#~~~ Authors ~~~#\n",file=file.name,append=TRUE)
    for(i in 1:length(object@authors@authors))
    { temp<-object@authors@authors[[i]]
      cat("name: ",temp@name,"\n",file=file.name,append=TRUE)
      cat("institution: ",temp@institution,"\n",file=file.name,append=TRUE)
      cat("email: ",temp@email,"\n",file=file.name,append=TRUE)
      cat("\n",file=file.name,append=TRUE)
    }
  } else
  {  cat("#~~~ Authors ~~~#\n",file=file.name,append=TRUE)
     cat("empty \n\n",file=file.name,append=TRUE)
  }

  #-------------------------------------------------------------------------------
  # write out version
  #-------------------------------------------------------------------------------
  cat("#~~~ Version of the model ~~~#",file=file.name,append=TRUE)
  cat("\n",file=file.name,append=TRUE)
  cat("status: ",object@version@status,"\n",file=file.name,append=TRUE)
  cat("minorupdate: ",object@version@minorupdate,"\n",file=file.name,append=TRUE)
  cat("majorupdate: ",object@version@majorupdate,"\n",file=file.name,append=TRUE)
  cat("subtitle: ",object@version@subtitle,"\n",file=file.name,append=TRUE)
  cat("editedby: ",object@version@editedby,"\n",file=file.name,append=TRUE)
  cat("\n",file=file.name,append=TRUE)
  
  #-------------------------------------------------------------------------------
  # write out settings
  #-------------------------------------------------------------------------------
  cat("#~~~ Model settings ~~~#",file=file.name,append=TRUE)
  cat("\n",file=file.name,append=TRUE)
  cat("wscores: ",object@settings@wscores,"\n",file=file.name,append=TRUE)
  cat("N: ",object@settings@N,"\n",file=file.name,append=TRUE)
  cat("N2d: ",object@settings@N2d,"\n",file=file.name,append=TRUE)
  cat("Ntest: ",object@settings@Ntest,"\n",file=file.name,append=TRUE)
  cat("stress: ",object@settings@stress,"\n",file=file.name,append=TRUE)
  cat("abserror: ",object@settings@abserror,"\n",file=file.name,append=TRUE)
  cat("mycol: ",object@settings@mycol,"\n",file=file.name,append=TRUE)
  cat("nwlayout: ",object@settings@nwlayout,"\n",file=file.name,append=TRUE)
  cat("usenotapplicable: ",object@settings@usenotapplicable,"\n",file=file.name,append=TRUE)
  cat("coverheader: ",object@settings@coverheader,"\n",file=file.name,append=TRUE)
  cat("sty: ",object@settings@sty,"\n",file=file.name,append=TRUE)
  cat("deleteTeX: ",object@settings@deleteTeX,"\n",file=file.name,append=TRUE)
  cat("trans: ",object@settings@trans,"\n",file=file.name,append=TRUE)
  cat("sens: ",object@settings@sens,"\n",file=file.name,append=TRUE)
  cat("\n",file=file.name,append=TRUE)
  
  #-------------------------------------------------------------------------------
  # write out scoring system
  #-------------------------------------------------------------------------------  
  cat("#~~~ Qualitative scheme for assessing uncertainty and knowledge base ~~~#\n",file=file.name,append=TRUE) 
  cat("name: ",object@scoring@name,"\n",file=file.name,append=TRUE)
  cat("tableheader: ",putLineBreaks(object@scoring@tableheader),"\n",file=file.name,append=TRUE)
  cat("explanatory: ",putLineBreaks(object@scoring@explanatory),"\n",file=file.name,append=TRUE)
  cat("values: ",object@scoring@values,"\n",file=file.name,append=TRUE)
  #---------------------------------------------------------------------------
  if(length(object@scoring@vcolors)>0)
    { for(i in 1:length(object@scoring@vcolors))
      { if(i==1) vcolors<-c()
        vcolorsTemp<-paste(object@scoring@vcolors[i],"=",names(object@scoring@vcolors)[i],sep="")
        vcolors<-c(vcolors,vcolorsTemp)
      }
    } else vcolors=""
  cat("vcolors: ",vcolors,"\n",file=file.name,append=TRUE)
  #---------------------------------------------------------------------------
  if(length(object@scoring@vmeanings)>0)
  { for(i in 1:length(object@scoring@vmeanings))
    { if(i==1) vmeanings<-c()
      vmeaningsTemp<-paste(object@scoring@vmeanings[i],"=",names(object@scoring@vmeanings)[i],sep="")
      vmeanings<-c(vmeanings,vmeaningsTemp)
    }
  } else vmeanings=""
  cat("vmeanings: ",vmeanings,"\n\n",file=file.name,append=TRUE)
  #---------------------------------------------------------------------------
  if(length(object@scoring@scoring)>0)
  { for(i in 1:length(object@scoring@scoring))
    { temp<-object@scoring@scoring[[i]]
      explanation<-temp@explanation
      if(!is.null(explanation)) explanation<-putLineBreaks(explanation)  
      cat("#~~~~~~#\n",file=file.name,append=TRUE) 
      cat("symbol: ",temp@notation,"\n",file=file.name,append=TRUE)
      cat("name: ",temp@name,"\n",file=file.name,append=TRUE)
      cat("explanation: ",explanation,file=file.name,append=TRUE)
      cat("\n\n",file=file.name,append=TRUE)
    }
  } 

  #-------------------------------------------------------------------------------
  # write out basic descriptions
  #-------------------------------------------------------------------------------  
  for(i in 1:length(object@basics@basics))
  { temp<-object@basics@basics[[i]]
    explanation<-temp@explanation
    if(!is.null(explanation)) explanation<-putLineBreaks(explanation)
    if(i==1)
    { cat("#~~~ Reference to the main document ~~~#\n",file=file.name,append=TRUE)
    } else if (i==2)
    { cat("#~~~ Background ~~~#\n",file=file.name,append=TRUE)
    } else if(i==3)
    { cat("#~~~ Objectives ~~~#\n",file=file.name,append=TRUE)
    } else if(i==4)
    { cat("#~~~ Scope ~~~#\n",file=file.name,append=TRUE)
    } else if (i==5)
    { cat("#~~~ Model description ~~~#\n",file=file.name,append=TRUE)
    }
    cat(explanation,file=file.name,append=TRUE)
    cat("\n\n",file=file.name,append=TRUE)
  }
 
  #-------------------------------------------------------------------------------
  # write out uncertainties
  #-------------------------------------------------------------------------------
  note<-object@uncertainties@note
  note<-gsub(x=note,"\n",replacement="")
  note<-gsub(x=note,"\t",replacement="")
  if(note!="" & length(object@uncertainties@uncertainties)>0)
  { cat("#~~~ General model uncertainty assessment ~~~\n#",file=file.name,append=TRUE)
    note<-object@uncertainties@note
    if(!is.null(note)) note<-putLineBreaks(note)
    cat("note: ",note,"\n",file=file.name,append=TRUE)
    cat("\n",file=file.name,append=TRUE)
    if(length(object@uncertainties@uncertainties)>0)
    { for(i in 1:length(object@uncertainties@uncertainties))
      { temp<-object@uncertainties@uncertainties[[i]]
        explanation<-temp@explanation
        if(!is.null(explanation)) explanation<-putLineBreaks(explanation)
        cat("#~~~~~~#\n",file=file.name,append=TRUE)
        cat("namemain: ",temp@namemain,"\n",file=file.name,append=TRUE)
        cat("namesub: ",temp@namesub,"\n",file=file.name,append=TRUE)
        cat("explanation: ",explanation,"\n",file=file.name,append=TRUE)
        cat("scores: ",temp@scores,"\n",file=file.name,append=TRUE)
        cat("\n",file=file.name,append=TRUE)
      }
    }
    cat("\n",file=file.name,append=TRUE)
  } else
  { cat("#~~~ General model uncertainty assessment ~~~#\n",file=file.name,append=TRUE)
    cat("empty \n\n",file=file.name,append=TRUE)
  }

  #-------------------------------------------------------------------------------
  # write out model parts
  #-------------------------------------------------------------------------------
  if(length(object@parts@parts)>0)
  { for(i in 1:length(object@parts@parts))
    { temp<-object@parts@parts[[i]]
      explanation<-temp@explanation
      if(!is.null(explanation)) explanation<-putLineBreaks(explanation)
      if(i==1)
      { cat("#~~~ Parts ~~~#\n",file=file.name,append=TRUE)
      } else cat("#~~~~~~#\n",file=file.name,append=TRUE)
      cat("name: ",temp@name,"\n",file=file.name,append=TRUE)
      cat("explanation: ",explanation,"\n",file=file.name,append=TRUE)
      cat("items: ",temp@items,"\n",file=file.name,append=TRUE)
      cat("\n",file=file.name,append=TRUE)
    }
  } else
  { cat("#~~~ Parts ~~~#\n",file=file.name,append=TRUE)
    cat("empty \n\n",file=file.name,append=TRUE)
  }
  
  #-------------------------------------------------------------------------------
  # write out model items
  #-------------------------------------------------------------------------------
  if(length(object@items@items)>0)
  { for(i in 1:length(object@items@items))
    { temp<-object@items@items[[i]]
      explanation<-temp@explanation
      if(!is.null(explanation)) explanation<-putLineBreaks(explanation)
      assumptions<-temp@assumptions
      if(!is.null(assumptions)) assumptions<-putLineBreaks(assumptions)
      remark<-temp@remark
      if(!is.null(remark)) remark<-putLineBreaks(remark)
      if(i==1)
      { cat("#~~~ Items definition ~~~#\n",file=file.name,append=TRUE)
      } else cat("#~~~~~~#\n",file=file.name,append=TRUE)
      cat("name: ",temp@name,"\n",file=file.name,append=TRUE)
      cat("part: ",temp@part,"\n",file=file.name,append=TRUE)
      cat("title: ",temp@title,"\n",file=file.name,append=TRUE)
      cat("stratum: ",temp@stratum,"\n",file=file.name,append=TRUE)
      cat("explanation: ",explanation,"\n",file=file.name,append=TRUE)
      cat("type: ",temp@type,"\n",file=file.name,append=TRUE)
      cat("typecode: ",temp@typecode,"\n",file=file.name,append=TRUE)
      #cat("data: ",temp@typecode,"\n",file=file.name,append=TRUE)
      cat("definition: ",temp@definition,"\n",file=file.name,append=TRUE)
      cat("depitem: ",temp@depitem,"\n",file=file.name,append=TRUE)
      cat("unit: ",temp@unit,"\n",file=file.name,append=TRUE)
      cat("role: ",temp@role,"\n",file=file.name,append=TRUE)
      cat("rolecode: ",temp@rolecode,"\n",file=file.name,append=TRUE)
      cat("stratumevaluated: ",temp@stratumevaluated,"\n",file=file.name,append=TRUE)
      cat("plausimin: ",temp@plausimin,"\n",file=file.name,append=TRUE)
      cat("plausimax: ",temp@plausimax,"\n",file=file.name,append=TRUE)
      cat("scores: ",temp@scores,"\n",file=file.name,append=TRUE)
      cat("assumptions: ",assumptions,"\n",file=file.name,append=TRUE)
      cat("remark: ",remark,"\n",file=file.name,append=TRUE)
      cat("reference: ",temp@reference,"\n",file=file.name,append=TRUE)
      cat("fullc: ",temp@fullc,"\n",file=file.name,append=TRUE)
      cat("relaxc: ",temp@relaxc,"\n",file=file.name,append=TRUE)
      cat("\n",file=file.name,append=TRUE)
    }
  } else
  { cat("#~~~ Items definition ~~~#\n",file=file.name,append=TRUE)
    cat("empty \n\n",file=file.name,append=TRUE)
  }

  #-------------------------------------------------------------------------------
  # write out validation
  #-------------------------------------------------------------------------------
  if(length(object@validation@validation)>0)
  { for(i in 1:length(object@validation@validation))
    { temp<-object@validation@validation[[i]]
      explanation<-temp@explanation
      if(!is.null(explanation)) explanation<-putLineBreaks(explanation)
      if(i==1)
      { cat("#~~~ Validation ~~~#\n",file=file.name,append=TRUE)
      } else cat("#~~~~~~#\n",file=file.name,append=TRUE)
      cat("name: ",temp@name,"\n",file=file.name,append=TRUE)
      cat("explanation: ",explanation,file=file.name,append=TRUE)
      cat("\n\n",file=file.name,append=TRUE)
    }
  } else
  { cat("#~~~ Validation ~~~#\n",file=file.name,append=TRUE)
    cat("empty \n\n",file=file.name,append=TRUE)
  }

  #-------------------------------------------------------------------------------
  # write out model glossary
  #-------------------------------------------------------------------------------
  if(length(object@glossary@glossary)>0)
  { for(i in 1:length(object@glossary@glossary))
    { temp<-object@glossary@glossary[[i]]
      explanation<-temp@explanation
      if(!is.null(explanation)) explanation<-putLineBreaks(explanation)
      if(i==1)
      { cat("#~~~ Glossary ~~~#\n",file=file.name,append=TRUE)
      } else cat("#~~~~~~#\n",file=file.name,append=TRUE) 
      cat("name: ",temp@name,"\n",file=file.name,append=TRUE)
      cat("explanation: ",explanation,file=file.name,append=TRUE)
      cat("\n\n",file=file.name,append=TRUE)
    }
  } else
  { cat("#~~~ Glossary ~~~#\n",file=file.name,append=TRUE)
    cat("empty \n\n",file=file.name,append=TRUE)
  }

  #-------------------------------------------------------------------------------
  # write out model abbreviations
  #-------------------------------------------------------------------------------
  if(length(object@abbreviations@abbreviations)>0)
  { for(i in 1:length(object@abbreviations@abbreviations))
    { temp<-object@abbreviations@abbreviations[[i]]
      explanation<-temp@explanation
      if(!is.null(explanation)) explanation<-putLineBreaks(explanation)
      if(i==1)
      { cat("#~~~ Abbreviations ~~~#\n",file=file.name,append=TRUE)
      } else cat("#~~~~~~#\n",file=file.name,append=TRUE)
      cat("name: ",temp@name,"\n",file=file.name,append=TRUE)
      cat("explanation: ",explanation,file=file.name,append=TRUE)
      cat("\n\n",file=file.name,append=TRUE)
    }
  } else
  { cat("#~~~ Abbreviations ~~~#\n",file=file.name,append=TRUE)
    cat("empty \n\n",file=file.name,append=TRUE)
  }

  #-------------------------------------------------------------------------------
  # write out conclusions
  #-------------------------------------------------------------------------------
  if(length(object@conclusions@conclusions)>0)
  {
    for(i in 1:length(object@conclusions@conclusions))
    { temp<-object@conclusions@conclusions[[i]]
      if(!is.null(temp)) temp<-putLineBreaks(temp)
      if(i==1)
      { cat("#~~~ Conclusions ~~~#\n",file=file.name,append=TRUE)
      } else cat("#~~~~~~#\n",file=file.name,append=TRUE)
      cat(temp,"\n",file=file.name,append=TRUE)
      cat("\n",file=file.name,append=TRUE)
    }
  } else
  {  cat("#~~~ Conclusions ~~~#\n",file=file.name,append=TRUE)
     cat("empty \n\n",file=file.name,append=TRUE)
  }

  #-------------------------------------------------------------------------------
  # write out comments
  #-------------------------------------------------------------------------------
  if(length(object@comments@comments)>0)
  {
    for(i in 1:length(object@comments@comments))
    { temp<-object@comments@comments[[i]]
      if(!is.null(temp)) temp<-putLineBreaks(temp)
      if(i==1)
      { cat("#~~~ Comments ~~~#\n",file=file.name,append=TRUE)
      } else cat("#~~~~~~#\n",file=file.name,append=TRUE)
      cat(temp,"\n",file=file.name,append=TRUE)
      cat("\n",file=file.name,append=TRUE)
    }
  } else
  { cat("#~~~ Comments ~~~#\n",file=file.name,append=TRUE)
    cat("empty \n\n",file=file.name,append=TRUE)
  }
    
  #-------------------------------------------------------------------------------
  # write out graphs information
  #-------------------------------------------------------------------------------
  if(length(object@graphs@graphs)>0)
  {
    for(i in 1:length(object@graphs@graphs))
    { temp<-object@graphs@graphs[[i]]
      explanation<-temp@explanation
      if(!is.null(temp)) explanation<-putLineBreaks(explanation)
      if(i==1)
      { cat("#~~~ Graphs ~~~#\n",file=file.name,append=TRUE)
      } else cat("#~~~~~~#\n",file=file.name,append=TRUE)
      #cat("notation: ",temp@notation,"\n",file=file.name,append=TRUE)
      cat("explanation: ",explanation,"\n",file=file.name,append=TRUE)
      cat("cover: ",temp@cover,"\n",file=file.name,append=TRUE)
      cat("\n",file=file.name,append=TRUE)
    }
  } else
  { cat("#~~~ Graphs ~~~#\n",file=file.name,append=TRUE)
    cat("empty \n\n",file=file.name,append=TRUE)
  }
}  # end of function writeOutModelClass()

