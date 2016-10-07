

################################################################################
################################################################################
#' @description This function offers the user to redefine the order of the model topic parts. 
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.changeOrder
#' @aliases menu.changeOrder
#' @title Function offering the user to redifine the order of the model topic parts 
#' @usage menu.changeOrder(rriskModelObject,menuLevel=1,specification="normal")
#' @param rriskModelObject is the \code{rrisk} model, whose parts are to be reordered
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @param specification specifies which objects are to be reordered
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskModelObject <- init.Model1()@@authors@@authors
#' menu.changeOrder(rriskModelObject)}

menu.changeOrder<-function(rriskModelObject,menuLevel=1,specification="normal")
{ on.exit(return(invisible(rriskModelObject)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  if(length(rriskModelObject)>0)
  { objectNames<-c()
    for(i in 1:length(rriskModelObject))
    { if(specification=="no name slot")
      { temp<-paste(substr(rriskModelObject[[i]],start=1,stop=100),"...",sep="")
      } else if(specification=="items")
      { temp<-paste(rriskModelObject[[i]]@name," (",rriskModelObject[[i]]@part,")",sep="")
      } else if(specification=="graphs")
      { temp<-paste(substr(rriskModelObject[[i]]@explanation,start=1,stop=100),"...",sep="")
      } else if(specification=="uncertainties")
      { temp<-paste(rriskModelObject[[i]]@namesub, " (",rriskModelObject[[i]]@namemain,")",sep="")
      } else temp<-rriskModelObject[[i]]@name
      objectNames<-c(objectNames,temp)
    }
    choices<-c(objectNames,"Exit dialog")
    input<-99
    while(!is.element(input,seq(1:length(choices))))
    { input<-mymenu1(title="Define new order of model topic parts",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
      
      input<-as.numeric(unique(input))
      if(setequal(input,1:length(rriskModelObject)))
      {  rriskModelObject<-rriskModelObject[input]
         cat("The order of model topic parts has been modified!\n")
         # break()
         #----------------------------
         objectNames<-c()
         for(i in 1:length(rriskModelObject))
         { if(specification=="no name slot")
            { temp<-paste(substr(rriskModelObject[[i]],start=1,stop=100),"...",sep="")
            } else if(specification=="items")
            { temp<-paste(rriskModelObject[[i]]@name," (",rriskModelObject[[i]]@part,")",sep="")
            } else if(specification=="graphs")
            { temp<-paste(substr(rriskModelObject[[i]]@explanation,start=1,stop=100),"...",sep="")
            } else if(specification=="uncertainties")
            { temp<-paste(rriskModelObject[[i]]@namesub, " (",rriskModelObject[[i]]@namemain,")",sep="")
            } else temp<-rriskModelObject[[i]]@name
            objectNames<-c(objectNames,temp)
         }
         choices<-c(objectNames,"Exit dialog")
         input<-99
         #---------------------------
      } else if(input==length(choices))
      { 
        break()
      }
      input<-99
    } # end while
  } else cat("The list of model topic parts is empty!!\n")
} # end of function menu.changeOrder()



################################################################################
################################################################################
#' @description This function lists all available parts of a \code{rrisk} model, from which it
#' offers the user to choose and saves the choice of the user.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.getChoiceOfParts
#' @aliases menu.getChoiceOfParts
#' @title Function listing all available parts of a rrisk model, from which it
#' offers the user to choose and saves the choice of the user
#' @usage menu.getChoiceOfParts(rriskModel,rriskModelFrom,menuLevel=1,onlyItems=FALSE)
#' @param rriskModel is the \code{rrisk} model where the parts are to be exported to 
#' @param rriskModelFrom is the \code{rrisk} model from which the parts are exported
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @param onlyItems decides whether only items are to be exported
#' @keywords menu
#' @export
#' @examples
#' \donttest{emptyModel<-new("modelClass")
#' rriskModel<-init.Model1()
#' menu.getChoiceOfParts(emptyModel,rriskModel,menuLevel=1,onlyItems=FALSE) }

menu.getChoiceOfParts<-function(rriskModel,rriskModelFrom,menuLevel=1,onlyItems=FALSE)
{ 
  on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  if(length(rriskModelFrom@parts@parts)>0)
  { partsNamesRight<-c()
    for(i in 1:length(rriskModelFrom@parts@parts))
    { partsNamesRight<-c(partsNamesRight,rriskModelFrom@parts@parts[[i]]@name)
    }
  
    choices<-c(partsNamesRight,"Exit dialog")
    input<-99
    while(!is.element(input,seq(1:length(choices))))
    { input<-mymenu1(title="Choose parts that should be imported into the current model",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
      if(length(input)==1)
      { if(is.element(input,1:length(partsNamesRight)))
        { rriskModel<-getParts(rriskModel,rriskModelFrom,partsToImportIndex=input,onlyItems=onlyItems)
          rriskModelFrom@parts@parts<-rriskModelFrom@parts@parts[-input]
          #-----------
          if(length(rriskModelFrom@parts@parts)>0)
          { partsNamesRight<-c()
            for(i in 1:length(rriskModelFrom@parts@parts))
            { partsNamesRight<-c(partsNamesRight,rriskModelFrom@parts@parts[[i]]@name)
            }
            choices<-c(partsNamesRight,"Exit dialog")
            input<-99
          } else break()
          #------------
        } else if(input==length(choices))
        { break()
        }
      } else if(setequal(intersect(input,1:length(partsNamesRight)),input))
      { rriskModel<-getParts(rriskModel,rriskModelFrom,partsToImportIndex=input,onlyItems=onlyItems)
        rriskModelFrom@parts@parts<-rriskModelFrom@parts@parts[-input]
        #-----------
        if(length(rriskModelFrom@parts@parts)>0)
        { partsNamesRight<-c()
          for(i in 1:length(rriskModelFrom@parts@parts))
          { partsNamesRight<-c(partsNamesRight,rriskModelFrom@parts@parts[[i]]@name)
          }
          choices<-c(partsNamesRight,"Exit dialog")
          input<-99
        } else break()
       #------------
      } 
      input<-99
    } # end while
  } else cat("There are no parts in the model from which the parts should be exported!\n")
} # end of functions()




################################################################################
################################################################################
#' @description This function list all items available and offers them to be removed.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.removeItems
#' @aliases menu.removeItems
#' @title Function listing all items available and offering them to be removed 
#' @usage menu.removeItems(rriskModel,menuLevel=1)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param menuLevel  ...
#' @export
#' @keywords menu
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' menu.removeItems(rriskModel,menuLevel=1)}

menu.removeItems<-function(rriskModel,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  itemNames<-c()
  for(i in 1:length(rriskModel@items@items)){
    itemNamesTemp<-paste("Item ",rriskModel@items@items[[i]]@name,sep="")
    itemNames<-c(itemNames,itemNamesTemp)
  }
  choices<-c(itemNames,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu1(title="Choose item(s) you wish to remove",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(all(input %in% 1:length(itemNames))){
      input<-as.numeric(input)
      temp<-gsub(x=itemNames[input],"Item ",replacement="")
      removeItemConfirm<-tkmessageBox(title="Delete item",icon="question",type="yesno",
          message=paste("Do you really wish to remove item(s) ",paste(temp,collapse=" ")," from the model?",sep=""))
      if(tclvalue(removeItemConfirm)=="yes"){
        itemsToRemove<-rriskModel@items@items[input]
        itemsToRemoveNames<-c()
        for(i in 1:length(itemsToRemove)){
          itemsToRemoveNames<-c(itemsToRemoveNames,itemsToRemove[[i]]@name)
        } # end  for(i in 1:length(itemsToRemove)){
        rriskModel@items@items<-rriskModel@items@items[-input]
        if(length(rriskModel@items@items)>0){
          for(i in 1:length(rriskModel@items@items)){
            depitem<-rriskModel@items@items[[i]]@depitem
            if(any(is.element(depitem,itemsToRemoveNames))){
              depitem<-setdiff(depitem,itemsToRemoveNames)
              rriskModel@items@items[[i]]@depitem<-depitem
            } # end if(is.element(oldItemName,depitem)){
          } # end for(i in 1:length(rriskModel@items@items)){
        } # end   if(length(rriskModel@items@items)>0){
        cat(paste("Item(s) '",itemsToRemoveNames,"' has been successfully removed from the model.\n",sep=""))
        cat("\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
        cat("After deleting some items CHECK the definition, full and relax commands of remaining items!\n")
        cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
        #-----------------
        if(length(rriskModel@items@items)>0){
          itemNames<-c()
          for(i in 1:length(rriskModel@items@items)){
            itemNamesTemp<-paste("Item ",rriskModel@items@items[[i]]@name,sep="")
            itemNames<-c(itemNames,itemNamesTemp)
          }
          choices<-c(itemNames,"Exit dialog")
          input<-99
        } else break()
        #-----------------
      } # end if(tclvalue(removeItemConfirm)=="yes")
    } else if(input==(length(itemNames)+1))
    { break()
    }
    input<-99
  } # end while
} # end of function menu.removeItems()


                                         
################################################################################
################################################################################
#' @description This function generates the user menu for maintaining items.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.handleItems
#' @aliases menu.handleItems
#' @title Function generating user menu for maintaining items
#' @usage menu.handleItems(rriskModel,rriskSession,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param rriskSession is an instance of the \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.handleItems(rriskSession@@models[[1]],rriskSession,menuLevel=1)}

menu.handleItems<-function(rriskModel,rriskSession,menuLevel=1)
{ on.exit(return(rriskModel)) 
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")  
  choices<-c("Edit available items",
              "Get items from other models",
              "Add new item",
              "Remove item(s)",
              "View items",
              "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the action you wish to execute",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { if(length(rriskModel@items@items)>0)
      { rriskModel<-menu.editItems(rriskModel,menuLevel=menuLevel+1)
      } else cat("The list of model items is empty!\n")
    } else if(input==2)
    { rriskModel<-menu.getItemsChooseModel(rriskModel,rriskSessionModels=rriskSession@models,menuLevel=menuLevel+1) 
    } else if (input==3)
    { rriskModel<-addNewItem(rriskModel,menuLevel=menuLevel+1)
    } else if(input==4)
    { if(length(rriskModel@items@items)>0)
      { rriskModel<-menu.removeItems(rriskModel,menuLevel=menuLevel+1)
      } else ("The list of model items is empty!\n")
    } else if(input==5){
      if(length(rriskModel@items@items)>0){
        menu.viewItems(rriskModel,menuLevel=menuLevel+1)
      } else {
        cat("\nThe model does not contain any item.\n")
      }
    } else if(input==6)
    { break()
    }
    input<-99
  }
} # end of function menu.handleItems()



################################################################################
################################################################################
#' @name menu.viewItems
#' @aliases menu.viewItems
#' @title Non-executable auxiliary function
#' @usage menu.viewItems(rriskModel,menuLevel=1)
#' @param rriskModel  ...
#' @param menuLevel  ...
#' @keywords menu
#' @export

menu.viewItems<-function(rriskModel,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  itemNames<-c()
  for(i in 1:length(rriskModel@items@items))
  { itemNamesTemp<-paste("View item ",rriskModel@items@items[[i]]@name,sep="")
    itemNames<-c(itemNames,itemNamesTemp)
  }
  choices<-c(itemNames,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose item yout wish to view",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(itemNames))){
      input<-as.numeric(input)
      print(rriskModel@items@items[[input]])
    } else if(input==(length(itemNames)+1)){
      break()
    }
    input<-99
  } # end while
} # end of function menu.editItems()


################################################################################
################################################################################
#' @description This function generates the user menu for editing item informations.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.editItems
#' @aliases menu.editItems
#' @title Function generating user menu for editing model informations
#' @usage menu.editItems(rriskModel,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' menu.editItems(rriskModel)}

menu.editItems<-function(rriskModel,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  itemNames<-c()
  for(i in 1:length(rriskModel@items@items))
  { #itemNamesTemp<-paste("Edit item ",rriskModel@items@items[[i]]@name,sep="")
    itemNamesTemp<-paste("Edit item ",rriskModel@items@items[[i]]@name," (",rriskModel@items@items[[i]]@title,")",sep="")
    itemNames<-c(itemNames,itemNamesTemp)
  }
  choices<-c(itemNames,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose action yout wish to execute",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)

    if(is.element(input,1:length(itemNames)))
    { input<-as.numeric(input)
      #oldItemName<-rriskModel@items@items[[input]]@name
      #editedItem<-change.item(rriskModel@items@items[[input]],rriskModel,menuLevel=menuLevel+1)
      #change.item.results<-change.item(rriskModel@items@items[[input]],rriskModel,menuLevel=menuLevel+1)
      #rriskModel<-change.item.results$model
      #editedItem<-change.item.results$item
      #rriskModel@items@items[[as.numeric(input)]]<-editedItem
      rriskModel<-change.item(index=input,rriskModel,menuLevel=menuLevel+1)
      #--------------------
      #itemNames<-c()
      #for(i in 1:length(rriskModel@items@items))
      #{ itemNamesTemp<-paste("Edit item ",rriskModel@items@items[[i]]@name,sep="")
      #  itemNames<-c(itemNames,itemNamesTemp)
      #}
      choices<-c(itemNames,"Exit dialog")
      input<-99
      #--------------------
    } else if(input==(length(itemNames)+1))
    { break()
    }
    input<-99
  } # end while
} # end of function menu.editItems()

#menu.editItems(demoModel1)



################################################################################
################################################################################
#' @description This function generates the user menu for editing model informations.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.editModel
#' @aliases menu.editModel
#' @title Function generating user menu for editing model informations
#' @usage menu.editModel(rriskModel,availableModels,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param availableModels is a character vector containing model names that are available in the current rrisk session
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \dontrun{rriskModel<-init.Model1()
#' menu.editModel(rriskModel,menuLevel=1)}
                      
menu.editModel<-function(rriskModel,availableModels,menuLevel=1)
{ # object - Instanz der Klasse "modelClass"
  on.exit(return(rriskModel)) 
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")  
  choices<-c("Edit model authors",
              "Edit model name and version",
              "Edit model settings",
              "Edit basic descriptions",
              "Edit references",
              "Edit uncertainties",
              "Edit model parts",
              "Edit glossary",
              "Edit abbreviations",
              "Edit validations",
              "Edit comments",
              "Edit conclusions",
              "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose model topic for editing",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==2)
    { nameversion.new<-change.nameversion(rriskModel@name,rriskModel@version,rriskModel@authors,availableModels)
      rriskModel@name<-nameversion.new$Name
      rriskModel@version<-nameversion.new$Version
    }else if(input==1)
    { rriskModel@authors<-change.authors(rriskModel@authors)
    } else if (input==3)
    { rriskModel@settings<-change.settings(rriskModel@settings)
    } else if(input==4)
    { rriskModel@basics<-change.basics(rriskModel@basics)
    } else if(input==5)
    { rriskModel@references<-change.references(rriskModel@references)
    } else if(input==6)
    { try(rriskModel@uncertainties<-change.uncertainties(rriskModel@uncertainties,rriskModel@scoring), silent = TRUE) 
    } else if (input==7)
    { PartsItems.new<-change.parts(rriskModel@parts,rriskModel@items)
      rriskModel@parts@parts<-PartsItems.new$parts
      rriskModel@items@items<-PartsItems.new$items
    } else if(input==8)
    { rriskModel@glossary<-change.glossary(rriskModel@glossary)
    } else if(input==9)
    { rriskModel@abbreviations<-change.abbreviations(rriskModel@abbreviations)
    } else if(input==10)
    { rriskModel@validation<-change.validations(rriskModel@validation)
    } else if(input==11)
    { rriskModel@comments<-change.comments(rriskModel@comments)
    } else if(input==12)
    { rriskModel@conclusions<-change.conclusions(rriskModel@conclusions)
    } else if(input==13)
    { break()
    } 
    input<-99
  }
} # end of fucntion menu.editModel()




################################################################################
################################################################################
#' @description This function generates the user menu for viewing models.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.viewModel
#' @aliases menu.viewModel
#' @title Function generating user menu for viewing models
#' @usage menu.viewModel(rriskModel,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' menu.viewModel(rriskModel,menuLevel=1)}

menu.viewModel<-function(rriskModel,menuLevel=1)
{ 
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  choices<-c(   "Model complete description",
                "Model version",
                "Model settings",
                "Model name and authors",
                "Model name, version, authors and items",
                "Model basic description",
                "Scoring system for uncertainty and knowledge base",
                "Assessment of general uncertainties",
                "Model parts",
                "Model items",
                "Model related data",
                "Model validation and verification status",
                "Comments",
                "Conclusions",
                "References",
                "Glossary and abbreviation",
                "Figures provided by user",
                "Exit dialog")
            
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose topic to be viewed",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { show(rriskModel)
    } else if(input==5)
    { sumUp(rriskModel)
    } else if(input==4)
    { show(rriskModel@name)
      show(rriskModel@authors)
    } else if (input==2)
    { show(rriskModel@version)
    } else if(input==3)
    { show(rriskModel@settings)
    } else if(input==7)
    { show(rriskModel@scoring)
    } else if(input==11)
    { showModelData(rriskModel@items)
    } else if (input==6)
    { show(rriskModel@basics)
    } else if(input==15)
    { show(rriskModel@references)
    } else if(input==8)
    { show(rriskModel@uncertainties)
    } else if(input==9)
    { show(rriskModel@parts)
    } else if(input==16)
    { show(rriskModel@glossary)
      show(rriskModel@abbreviations)
    } else if(input==10)
    { show(rriskModel@items)
    } else if(input==12)
    { show(rriskModel@validation)
    } else if(input==13)
    { show(rriskModel@comments)
    } else if(input==14)
    { show(rriskModel@conclusions)
    } else if(input==17)
    { show(rriskModel@graphs)
      displayConceptGraphs(rriskModel)
    } else if(input==18)
    { break()
    } 
    input<-99
  }
} # end of function menu.viewModel()



################################################################################
################################################################################
#' @description This function lists all scoring systems available and offers them to be set to
#' the current model.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.getSsystem
#' @aliases menu.getSsystem
#' @title Function listing all items available and offering them to be removed 
#' @usage menu.getSsystem(rriskModel,rriskSessionSsystems,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param rriskSessionSsystems are the slot models from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels="Demo model 1")
#' menu.getSsystem(rriskModel=rriskSession@@models[[1]],
#' rriskSession@@scoringsystems,menuLevel=1)}

menu.getSsystem<-function(rriskModel,rriskSessionSsystems,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  systemNames<-c()
  for(i in 1:length(rriskSessionSsystems))
  { if(rriskSessionSsystems[[i]]@name!=rriskModel@scoring@name)
    { #temp<-paste(rriskSession@models[[i]]@name@name," (",rriskSession@models[[i]]@modeltype,")",sep="")
      systemNames<-c(systemNames,rriskSessionSsystems[[i]]@name)
    }
  }
  choices<-c(systemNames,"Exit dialog")
  input<-99
  
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose scoring system you wish to set to the current model",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(systemNames)))
    { input<-as.numeric(input)
      for(i in 1:length(rriskSessionSsystems))
      { if(rriskSessionSsystems[[i]]@name==systemNames[input])
        { rriskModel<-setScoringSystem(rriskModel,rriskSessionSsystems[[i]])
        }
      }
      print(rriskModel@scoring)
      cat("\nScoring system has been edited successfully!\n")
    } else if(input==(length(systemNames)+1))
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.getItems()



################################################################################
################################################################################
#' @description This function generates the user menu for maintaining scoring systems.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.handleSsystem
#' @aliases menu.handleSsystem
#' @title Function generating user menu for maintaining items
#' @usage menu.handleSsystem(rriskModel,rriskSession,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param rriskSession is an instance of the \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.handleSsystem(rriskSession@@models[[1]],rriskSession,menuLevel=1)}

menu.handleSsystem<-function(rriskModel,rriskSession,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  choices<-c("Set to an empty scoring system",
             "Get scoring system from the list of available scoring systems",
             "Edit scoring system",
             "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose action to be executed",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { rriskModel<-setScoringSystem(rriskModel,newScoringSystem=new("modelScoringClass"))
      print(rriskModel@scoring)
      cat("\nScoring system has been edited successfully!\n")
    } else if(input==2)
    { rriskModel<-menu.getSsystem(rriskModel,rriskSession@scoringsystems,menuLevel=menuLevel+1)
      #print(rriskModel@scoring)
      #cat("\nScoring system has been edited successfully!\n")
    } else if(input==3)
    { rriskModel@scoring<-change.scoring(scoring=rriskModel@scoring)
       print(rriskModel@scoring)
       cat("\nScoring system has been edited successfully!\n")
    } else if (input==4)
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.handleSsystem() 



################################################################################
################################################################################
#' @description This function generates the user menu for choosing a model from which items should
#' be imported.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.getItemsChooseModel
#' @aliases menu.getItemsChooseModel
#' @title Function generating user menu for choosing a model from which items should be 
#' imported
#' @usage menu.getItemsChooseModel(rriskModel,rriskSessionModels,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param rriskSessionModels are the slot models of an rriskSession object                                 
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
#' menu.getItemsChooseModel(rriskModel = rriskSession@@models[[1]], 
#' rriskSession@@models, menuLevel=1)}

menu.getItemsChooseModel<-function(rriskModel,rriskSessionModels,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  modelNames<-c()
  for(i in 1:length(rriskSessionModels))
  { #if(rriskSessionModels[[i]]@name@name!=rriskModel@name@name){
      #temp<-paste(rriskSession@models[[i]]@name@name," (",rriskSession@models[[i]]@modeltype,")",sep="")
      modelNames<-c(modelNames,rriskSessionModels[[i]]@name@name)
    #}
  }
  choices<-c(modelNames,"Exit dialog")
  input<-99
  
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the model from which you wish to import items",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(modelNames)))
    { input<-as.numeric(input)
      for(i in 1:length(rriskSessionModels))
      { if(rriskSessionModels[[i]]@name@name==modelNames[input])
        { rriskModelFrom<-rriskSessionModels[[i]]
        }
      }
      #rriskModelFrom<-rriskSession@models[[input]]
      rriskModel<-menu.getItems(rriskModel,rriskModelFrom,menuLevel=menuLevel+1)
    } else if(input==(length(modelNames)+1))
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.getItems()




################################################################################
################################################################################
#' @description This function generates the user menu for choosing a model from which topics should
#' be imported.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.getTopicsChooseModel
#' @aliases menu.getTopicsChooseModel
#' @title Function generating user menu for choosing a model from which topics should be imported
#' @usage menu.getTopicsChooseModel(rriskModel,rriskSessionModels,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param rriskSessionModels are the slot models of an rriskSession object         
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' rriskSession<-init.rriskSession(useDemoModels="all")
#' menu.getTopicsChooseModel(rriskSession@@models[[1]],rriskSession@@models,menuLevel=1)}

menu.getTopicsChooseModel<-function(rriskModel,rriskSessionModels,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  modelNames<-c()
  for(i in 1:length(rriskSessionModels))
  { if(rriskSessionModels[[i]]@name@name!=rriskModel@name@name)
    { modelNames<-c(modelNames,rriskSessionModels[[i]]@name@name)
    }
  }
  choices<-c(modelNames,"Exit dialog")
  input<-99
  
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the model from which you wish to import topics",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(modelNames)))
    { input<-as.numeric(input)
      for(i in 1:length(rriskSessionModels))
      { if(rriskSessionModels[[i]]@name@name==modelNames[input])
        { rriskModelFrom<-rriskSessionModels[[i]]
        }
      }
      rriskModel<-menu.getTopics(rriskModel,rriskModelFrom,menuLevel=menuLevel+1)
    } else if(input==(length(modelNames)+1))
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.getTopicsChooseModel()



################################################################################
################################################################################
#' @description This function lists all available items of a \code{rrisk} model, from which it
#' offers the user to choose and saves the choice of the user.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.getChoiceOfItems
#' @aliases menu.getChoiceOfItems
#' @title Function listing all available parts of a rrisk model, from which it offers the user to choose and saves the choice of the user
#' @usage menu.getChoiceOfItems(rriskModel,rriskModelFrom,menuLevel=1)
#' @param rriskModel is the \code{rrisk} model where the parts are to be exported to 
#' @param rriskModelFrom is the \code{rrisk} model from which the parts are exported 
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{emptyModel<-new("modelClass")
#' rriskModel<-init.Model1()
#' menu.getChoiceOfItems(rriskModel=emptyModel,rriskModelFrom=rriskModel,menuLevel=1)}

menu.getChoiceOfItems<-function(rriskModel,rriskModelFrom,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  if(length(rriskModelFrom@items@items)>0)
  { itemNamesRight<-c()
    for(i in 1:length(rriskModelFrom@items@items))
    { itemNamesRight<-c(itemNamesRight,rriskModelFrom@items@items[[i]]@name)
    }
    choices<-c(itemNamesRight,"Exit dialog")
    input<-99
    while(!is.element(input,seq(1:length(choices))))
    { input<-mymenu1(title="Choose items that should be imported into the current model",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
      if(length(input)==1)
      { if(is.element(input,1:length(itemNamesRight)))
        { rriskModel<-getItems(rriskModel,rriskModelFrom,itemsToImportIndex=input)
          rriskModelFrom@items@items<-rriskModelFrom@items@items[-input]
          #-----------
          if(length(rriskModelFrom@items@items)>0)
          { itemNamesRight<-c()
            for(i in 1:length(rriskModelFrom@items@items))
            { itemNamesRight<-c(itemNamesRight,rriskModelFrom@items@items[[i]]@name)
            }
            choices<-c(itemNamesRight,"Exit dialog")
            input<-99
          } else break()
          #------------
        } else if(input==length(choices))
        { break()
        }
      } else if(setequal(intersect(input,1:length(itemNamesRight)),input))
      { rriskModel<-getItems(rriskModel,rriskModelFrom,itemsToImportIndex=input)
        rriskModelFrom@items@items<-rriskModelFrom@items@items[-input]
        #-----------
        if(length(rriskModelFrom@items@items)>0)
        { itemNamesRight<-c()
          for(i in 1:length(rriskModelFrom@items@items))
          { itemNamesRight<-c(itemNamesRight,rriskModelFrom@items@items[[i]]@name)
          }
          choices<-c(itemNamesRight,"Exit dialog")
          input<-99
        } else break()
       #------------
      } 
      input<-99
    } # end while
  } else cat("There are no items in the model from which the items should be exported!\n")
} # end of function menu.getItems()




################################################################################
################################################################################
#' @description This function offers the user to import external items into the current model.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.getItems
#' @aliases menu.getItems
#' @title Function listing all available parts of a rrisk model, from which it
#' offers the user to choose and export to another rrisk model
#' @usage menu.getItems(rriskModel,rriskModelFrom,menuLevel=1)
#' @param rriskModel is the \code{rrisk} model where the parts are to be exported to
#' @param rriskModelFrom is the \code{rrisk} model from which the items are exported 
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{emptyModel<-new("modelClass")
#' rriskModel<-init.Model1()
#' menu.getItems(rriskModel=emptyModel,rriskModelFrom=rriskModel,menuLevel=1)}

menu.getItems<-function(rriskModel,rriskModelFrom,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  choices<-c("Get a choice of items",
             "Get a choice of parts inclusive items",
             "Get all items from part (without part definition)",
             "Get all items",
             "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the way how items should be imported into the current model",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { rriskModel<-menu.getChoiceOfItems(rriskModel,rriskModelFrom,menuLevel=menuLevel+1)
    } else if(input==2)
    { rriskModel<-menu.getChoiceOfParts(rriskModel,rriskModelFrom,menuLevel=menuLevel+1)
    } else if(input==3)
    { rriskModel<-menu.getChoiceOfParts(rriskModel,rriskModelFrom,menuLevel=menuLevel+1,onlyItems=TRUE)
    } else if(input==4)
    { rriskModel<-getItems(rriskModel,rriskModelFrom)
    } else if (input==5)
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.getItems() 



################################################################################
################################################################################
#' @description This function generates the user menu for editing sequences.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.changeSequences
#' @aliases menu.changeSequences
#' @title Function generating user menu for editing sequences items
#' @usage menu.changeSequences(rriskModel,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' menu.changeSequences(rriskModel,menuLevel=1)}

menu.changeSequences<-function(rriskModel,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  choices<-c("Order of authors",
             "Order of parts",
             "Order of items",
             "Order of uncertainties",
             "Order of validations",
             "Order of comments",
             "Order of conclusions",
             "Order of glossary",
             "Order of abbreviations",
             "Order of concept graphs",
             "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the model topic where you wish to change the order",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { rriskModel@authors@authors<-menu.changeOrder(rriskModel@authors@authors,menuLevel=menuLevel+1)
    } else if(input==2)
    { rriskModel@parts@parts<-menu.changeOrder(rriskModel@parts@parts,menuLevel=menuLevel+1)
    } else if(input==3)
    { cat("\nSorry, this option is currently not possible.\n")
      #rriskModel@items@items<-menu.changeOrder(rriskModel@items@items,menuLevel=menuLevel+1,specification="items")
    } else if(input==4)
    { rriskModel@uncertainties@uncertainties<-menu.changeOrder(rriskModel@uncertainties@uncertainties,menuLevel=menuLevel+1,specification="uncertainties")
    } else if(input==5)
    { rriskModel@validation@validation<-menu.changeOrder(rriskModel@validation@validation,menuLevel=menuLevel+1)
    } else if(input==6)
    { rriskModel@comments@comments<-menu.changeOrder(rriskModel@comments@comments,menuLevel=menuLevel+1,specification="no name slot")
    } else if(input==7)
    { rriskModel@conclusions@conclusions<-menu.changeOrder(rriskModel@conclusions@conclusions,menuLevel=menuLevel+1,specification="no name slot")
    }else if(input==8)
    { rriskModel@glossary@glossary<-menu.changeOrder(rriskModel@glossary@glossary,menuLevel=menuLevel+1)
    } else if(input==9)
    { rriskModel@abbreviations@abbreviations<-menu.changeOrder(rriskModel@abbreviations@abbreviations,menuLevel=menuLevel+1)
    } else if(input==10)
    { rriskModel@graphs@graphs<-menu.changeOrder(rriskModel@graphs@graphs,menuLevel=menuLevel+1,specification="graphs")
    } else if (input==11)
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.changeSequences() 


################################################################################
################################################################################
#' @description This function offers the user to import external topics into the current model.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.getTopics
#' @aliases menu.getTopics
#' @title Function listing all available topics of a rrisk model, from which it offers the user to choose and export to another rrisk model
#' @usage menu.getTopics(rriskModel,rriskModelFrom,menuLevel=1)
#' @param rriskModel is the \code{rrisk} model where the parts are to be exported to
#' @param rriskModelFrom is the \code{rrisk} model from which the items are exported 
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{emptyModel<-new("modelClass")
#' rriskModel<-init.Model1()
#' menu.getTopics(rriskModel=emptyModel,rriskModelFrom=rriskModel,menuLevel=1)}

menu.getTopics<-function(rriskModel,rriskModelFrom,menuLevel=1)
{ on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  choices<-c("Get basic descriptions",
             "Get general model uncertainties",
             "Get validations",
             "Get comments",
             "Get conclusions",
             "Get glossary",
             "Get abbreviations",
             "Get concept graphs",
             "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the topic that you wish to import into the current model",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { rriskModel@basics<-rriskModelFrom@basics
      cat("Basics descriptions have been successfully imported into the current model.")
    } else if(input==2)
    { rriskModel@uncertainties<-rriskModelFrom@uncertainties
      cat("Model uncertainties and knowledge base have been successfully imported into the current model.")
    } else if(input==3)
    { rriskModel@validation<-rriskModelFrom@validation
      cat("Validations have been successfully imported into the current model.")
    } else if(input==4)
    { rriskModel@comments<-rriskModelFrom@comments
      cat("Comments have been successfully imported into the current model.")
    } else if(input==5)
    { rriskModel@conclusions<-rriskModelFrom@conclusions
      cat("Conclusions have been successfully imported into the current model.")
    } else if(input==6)
    { rriskModel@glossary<-rriskModelFrom@glossary
      cat("Glossary have been successfully imported into the current model.")
    } else if(input==7)
    { rriskModel@abbreviations<-rriskModelFrom@abbreviations
      cat("Abbreviations have been successfully imported into the current model.")
    } else if(input==8)
    { rriskModel@graphs<-rriskModelFrom@graphs
      cat("Model concept graphs have been successfully imported into the current model.")
    } else if (input==9)
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.getItems() 


################################################################################
################################################################################
#' @description This function generates the user menu for maintaining models.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.handleModel
#' @aliases menu.handleModel
#' @title Function generating user menu for maintaining models
#' @usage menu.handleModel(rriskModel,rriskSession,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param rriskSession is an instance of the \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' rriskModel.new<-menu.handleModel(rriskSession@@models[[1]],rriskSession)}

menu.handleModel<-function(rriskModel,rriskSession,menuLevel=1)
{
  on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  # Get names of available models
  availableModels<-c()
  for(i in 1:length(rriskSession@models))
  { temp<-rriskSession@models[[i]]@name@name
    availableModels<-c(availableModels,temp)
  }
  
  choices<-c("Summing up",
              "View model",
              "Show model graphs",
              "Edit model topics",
              "Get topics from other models",
              "Model items",
              "Load reference list",
              "Load/edit concept graph(s)",
              "Export model objects and definition",
              "Scoring system",
              "Change order in model topics",
              "Run model",
              "Create model report",
              "Save model",
              "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose action to be executed",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { sumUp(rriskModel)
    }else if(input==2)
    { menu.viewModel(rriskModel,menuLevel=menuLevel+1)
    } else if(input==3)
    { menu.graphsModel(rriskModel,menuLevel=menuLevel+1)
    }else if(input==4)
    { availableModels<-setdiff(availableModels,rriskModel@name@name)
      if(rriskModel@modeltype=="user defined")
      { rriskModel<-menu.editModel(rriskModel,availableModels,menuLevel=menuLevel+1)
      } else cat("This action is not allowed for the rrisk model of type 'rrisk demo model', only rrisk model of the type 'user defined' can be edited!\n")
    } else if(input==5)
    { if(rriskModel@modeltype=="user defined")
      { rriskModel<-menu.getTopicsChooseModel(rriskModel,rriskSession@models,menuLevel=menuLevel+1)
      } else cat("This action is not allowed for the rrisk model of type 'rrisk demo model', only rrisk model of the type 'user defined' can be edited!\n")
    } else if(input==6)
    { if(rriskModel@modeltype=="user defined")
      { rriskModel<-menu.handleItems(rriskModel,rriskSession,menuLevel=menuLevel+1)
      } else cat("This action is not allowed for the rrisk model of type 'rrisk demo model', only rrisk model of the type 'user defined' can be edited!\n")
    }else if(input==7)
    { if(rriskModel@modeltype=="user defined")
      { rriskModel@references@references<-referencesLoad()
      } else cat("This action is not allowed for the rrisk model of type 'rrisk demo model', only rrisk model of the type 'user defined' can be edited!\n")
    } else if(input==8)
    { if(rriskModel@modeltype=="user defined")
      { rriskModel@graphs<-change.graphs(rriskModel@graphs)
      } else cat("This action is not allowed for the rrisk model of type 'rrisk demo model', only rrisk model of the type 'user defined' can be edited!\n")
    } else if(input==9)
    { menu.exportModel(rriskModel,menuLevel=menuLevel+1)
    } else if(input==10)
    { if(rriskModel@modeltype=="user defined")
      { rriskModel<-menu.handleSsystem(rriskModel,rriskSession,menuLevel=menuLevel+1)
      } else cat("This action is not allowed for the rrisk model of type 'rrisk demo model', only rrisk model of the type 'user defined' can be edited!\n") 
    } else if(input==11)
    { rriskModel<-menu.changeSequences(rriskModel,menuLevel=menuLevel+1)
    } else if(input==12)
    { rriskModel<-menu.runModel(rriskModel,menuLevel=menuLevel+1)
    } else if (input==13)
    { createModelReport(rriskModel,rriskSession,menuLevel=menuLevel+1)
    } else if(input==14)
    { modelSave(rriskModel)
    } else if(input==15)
    { break()
    } 
    input<-99
  }
} # end of fucntion menu.handleModel()


################################################################################
################################################################################
#' @description This function generates the user menu for running simulations on the model.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.runModel
#' @aliases menu.runModel
#' @title Function generating user menu for running simulations on the model
#' @usage menu.runModel(rriskModel,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' menu.runModel(rriskModel)}

menu.runModel<-function(rriskModel,menuLevel=1)
{ # object - Instanz der Klasse "modelClass"
  on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  choices<-c("Run model (1d)",
              "Run model (1d + 2d)",
              "Show outcome function(s)",
              "Run sensitivity analysis",
              "Reset simulation results",
              "Close all graphic devices",
              "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose action to be executed",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1){
      rriskModel<-run(rriskModel,sim.2d=FALSE,menuLevel=menuLevel+1)
    }else if(input==2){
      rriskModel<-run(rriskModel,sim.2d=TRUE,menuLevel=menuLevel+1)
    } else if(input==3){
      if(length(rriskModel@output@fullout.1d)==0 | length(rriskModel@output@relaxout.1d)==0){
        cat("\nPlots of outcome function(s) are omitted because 1d simulation was not conducted.\n")
        #break()
      } else {
        plotOFHistogram(rriskModel,pdfGraph=FALSE)
        plotOFConvergence(rriskModel,pdfGraph=FALSE)
        plotCDF(rriskModel,pdfGraph=FALSE) 
      } # end if(length(rriskModel@output@fullout.1d)==0 | length(rriskModel@output@relaxout.1d)==0){
    } else if(input==4){
      menu.runSensitivityModel(rriskModel,menuLevel=menuLevel+1)
    } else if(input==5){
      rriskModel@output<-new("modelOutputClass")
      cat("\nThe model simulation results have been reset!\n")
    } else if(input==6){
      graphics.off()
    } else if(input==7){
      break()
    } # enmd if(input==1)
    input<-99
  } # end while
} # end of function menu.runModel()




################################################################################
################################################################################
#' @description This function generates the user menu for viewing graphics.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.runSensitivityModel
#' @aliases menu.runSensitivityModel
#' @title Function generating user menu for viewing graphics
#' @usage menu.runSensitivityModel(rriskModel,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()   
#' menu.runSensitivityModel(rriskModel)}

menu.runSensitivityModel<-function(rriskModel,menuLevel=1)
{ 
  on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  choices<-c("Tornado charts",
              "Nonparametric regression",
              "Traffic light matrix of mcrv-items",
              "Regression tree",
              "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Run sesitivity analysis using ...",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { plotTornado(rriskModel,pdfGraph=FALSE)
    }else if(input==2)
    { plotGAM(rriskModel,pdfGraph=FALSE)
    } else if(input==3)
    { plotTrafficLights(rriskModel,pdfGraph=FALSE)
    } else if(input==4)
    { plotTree(rriskModel,pdfGraph=FALSE) 
    } else if(input==5)
    { break()
    } 
    input<-99
  }
} # end of function menu.runSensitivityModel()



################################################################################
################################################################################
#' @description This function generates the user menu for displaying model graphics.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.graphsModel
#' @aliases menu.graphsModel
#' @title Function generating user menu for displaying model graphics
#' @usage menu.graphsModel(rriskModel,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' menu.graphsModel(rriskModel,menuLevel=1)}

menu.graphsModel<-function(rriskModel,menuLevel=1)
{ # object - Instanz der Klasse "modelClass"
  on.exit(return(invisible(rriskModel)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  choices<-c("Concept graph(s)",
             "Network graph",
             "Model uncertainties",
             "Close all graphic devices",
             "Exit")
             
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose model graph that should be shown",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { displayConceptGraphs(rriskModel)
    } else if(input==2)
    { plotModelNetwork(rriskModel)
    } else if(input==3)
    { tolatexGraphs.uncertainties(rriskModel,LatexReport=FALSE) 
    } else if(input==4)
    { graphics.off()
    }else if(input==5)
    { break()
    } 
    input<-99
  }
} # end of fucntion menu.graphsModel()



################################################################################
################################################################################
#' @description This function generates the user menu for exporting models.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.exportModel
#' @aliases menu.exportModel
#' @title Function generating user menu for exporting models
#' @usage menu.exportModel(rriskModel,menuLevel=1)
#' @param rriskModel is an instance of the \code{modelClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' menu.exportModel(rriskModel,menuLevel=1)}

menu.exportModel<-function(rriskModel,menuLevel=1)
{ # object - Instanz der Klasse "modelClass"  
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  choices<-c("Export model definition template (.Rmdt file)",
              "Export scoring system definition template (.Rsdt file)",
              "Export scoring system as R object (.Rdata file)",  
              "Export model related data sets",
              "Export reference list",
              "Export model graphs",
              "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose action to be executed",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { writeOut(object=rriskModel)
    } else if(input==2)
    { writeOut(rriskModel@scoring)
    } else if (input==3)
    { ssystemSave(rriskModel@scoring)
    }else if(input==4)
    { exportModelData(rriskModel)
    } else if (input==5)
    { referencesCheck<-paste(rriskModel@references@references,collapse="")
      referencesCheck<-gsub(x=referencesCheck," ",replacement="")
      if(referencesCheck!="")
      { writeOut(rriskModel@references,model.name=rriskModel@name@name)
      }  else cat("The reference list of the current model is empty!\n") 
    } else if(input==6)
    { exportModelGraphs(rriskModel)
    } else if(input==7)
    { break()
    }
    input<-99 
    }
} # end of function menu.exportModel()



################################################################################
################################################################################
#' @description This function generates the user menu for saving models.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.saveModel
#' @aliases menu.saveModel
#' @title Function generating user menu for saving models
#' @usage menu.saveModel(rriskSessionModels,menuLevel=1)
#' @param rriskSessionModels are the slot models from the \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels="all")
#' menu.saveModel(rriskSession@@models,menuLevel=1)}

menu.saveModel<-function(rriskSessionModels,menuLevel=1)
{ #on.exit(return(invisible(rriskSessionModels)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  modelNames<-c()
  for(i in 1:length(rriskSessionModels))
  { temp<-paste(rriskSessionModels[[i]]@name@name," (",rriskSessionModels[[i]]@modeltype,")",sep="")
    modelNames<-c(modelNames,temp)
  }
  choices<-c(modelNames,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the model you wish to save",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(modelNames)))
    { modelToSave<-rriskSessionModels[[as.numeric(input)]]
      modelToSave@modeltype<-"user defined"
      modelSave(modelToSave)
    } else if(input==(length(modelNames)+1))
    { break()
    } 
    input<-99
  }
}



################################################################################
################################################################################
#' @description This function lists all available models to be chosen by the user.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.chooseModel
#' @aliases menu.chooseModel
#' @title Function listing all available models to be chosen by the user
#' @usage menu.chooseModel(rriskSession,menuLevel=1)
#' @param rriskSession is a \code{rriskClass} object
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.chooseModel(rriskSession,menuLevel=1)}


menu.chooseModel<-function(rriskSession,menuLevel=1)
{ rriskSessionModels<-rriskSession@models
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  on.exit(return(invisible(rriskSessionModels))) 
  modelNames<-c()
  for(i in 1:length(rriskSessionModels))
  { temp<-paste(rriskSessionModels[[i]]@name@name," (",rriskSessionModels[[i]]@modeltype,")",sep="")
    modelNames<-c(modelNames,temp) 
  }
  choices<-c(modelNames,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the model you wish to use",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(modelNames)))
    { input<-as.numeric(input)
      rriskSessionModels[[input]]<-menu.handleModel(rriskSessionModels[[input]],rriskSession,menuLevel=menuLevel+1)
      modelNames<-c()
      for(i in 1:length(rriskSessionModels)){
        temp<-paste(rriskSessionModels[[i]]@name@name," (",rriskSessionModels[[i]]@modeltype,")",sep="")
        modelNames<-c(modelNames,temp) 
      }
      choices<-c(modelNames,"Exit dialog")
    } else if(input==(length(modelNames)+1))
    { break()
    } 
    input<-99
  }
} # end of function menu.chooseModel()



################################################################################
################################################################################
#' @description This function lists all available models and offers them to be duplicated.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.copyModel
#' @aliases menu.copyModel
#' @title Function listing all available models and offers them to be duplicated
#' @usage menu.copyModel(rriskSessionModels,menuLevel=1)
#' @param rriskSessionModels are the slot models from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.copyModel(rriskSession@@models,menuLevel=1)}

menu.copyModel<-function(rriskSessionModels,menuLevel=1)
{ levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  on.exit(return(invisible(rriskSessionModels)))
  modelNamesAvailable<-c()
  for(i in 1:length(rriskSessionModels))
  { temp<-paste(rriskSessionModels[[i]]@name@name," (",rriskSessionModels[[i]]@modeltype,")",sep="")
    modelNamesAvailable<-c(modelNamesAvailable,temp)
  }
  choices<-c(modelNamesAvailable,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the model you wish to duplicate",
    choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(modelNamesAvailable)))
    { input<-as.numeric(input)
      modelToAdd<-rriskSessionModels[[input]]
      modelToAdd@modeltype<-"user defined"
      newModelName<-modelToAdd@name@name
      repeat
      { newModelName<-rrisk.DialogString("Please enter the name of the new model:")
        modelNamesAvailable<-c()
        for(i in 1:length(rriskSessionModels)) modelNamesAvailable<-c(modelNamesAvailable,rriskSessionModels[[i]]@name@name)
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
      if(!is.null(newModelName))
      {
        modelToAdd@name@name<-newModelName
        rriskSessionModels[[length(modelNamesAvailable)+1]]<-modelToAdd
        #tkmessageBox(title="User information",icon="info",type="ok",
        #  message="A new model has been successfully created. Please, use 'View/edit/execute model' menu for further modifications!")
        cat("\nNew model has been created successfully!\n")
      }
      #------------------------
      modelNamesAvailable<-c()
      for(i in 1:length(rriskSessionModels))
      { temp<-paste(rriskSessionModels[[i]]@name@name," (",rriskSessionModels[[i]]@modeltype,")",sep="")
        modelNamesAvailable<-c(modelNamesAvailable,temp)
      }
      choices<-c(modelNamesAvailable,"Exit dialog")
      input<-99
      #-----------------------
    } else if(input==(length(modelNamesAvailable)+1))
    { break()
    }
    input<-99
  }
} # end of function menu.copyModel()




################################################################################
################################################################################
#' @description This function lists all available scoring systems and offers them to be duplicated.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.copySsystem
#' @aliases menu.copySsystem
#' @title Function listing all available scoring systems and offering them to be duplicated
#' @usage menu.copySsystem(rriskSessionSsystems,menuLevel=1)
#' @param rriskSessionSsystems is the slot scoringsystem from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.copySsystem(rriskSession@@scoringsystems,menuLevel=1)}

menu.copySsystem<-function(rriskSessionSsystems,menuLevel=1)
{ levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  on.exit(return(invisible(rriskSessionSsystems)))
  ssystemNamesAvailable<-c()
  for(i in 1:length(rriskSessionSsystems))
  { temp<-paste(rriskSessionSsystems[[i]]@name," (",rriskSessionSsystems[[i]]@systemtype,")",sep="")
    ssystemNamesAvailable<-c(ssystemNamesAvailable,temp)
  }
  choices<-c(ssystemNamesAvailable,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the scoring system you wish to duplicate",
    choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(ssystemNamesAvailable)))
    { input<-as.numeric(input)
      systemToAdd<-rriskSessionSsystems[[input]]
      systemToAdd@systemtype<-"user defined"
      newSsystemName<-systemToAdd@name
      repeat
      { newSsystemName<-rrisk.DialogString("Please enter the name of the new scoring system:")
        if(is.null(newSsystemName)) break()
        ssystemNamesAvailable<-c()
        for(i in 1:length(rriskSessionSsystems)) ssystemNamesAvailable<-c(ssystemNamesAvailable,rriskSessionSsystems[[i]]@name)
        ifelse(is.element(newSsystemName,ssystemNamesAvailable) | gsub(x=newSsystemName," ",replacement="")=="",next(),break())
      }
      if(!is.null(newSsystemName))
      { systemToAdd@name<-newSsystemName
        rriskSessionSsystems[[length(ssystemNamesAvailable)+1]]<-systemToAdd
        tkmessageBox(title="User information",icon="info",type="ok",
          message="A new scoring system has been successfully created. Please, use 'Edit scoring system' menu for further modifications!")
      }
      #------------------------------------
      ssystemNamesAvailable<-c()
      for(i in 1:length(rriskSessionSsystems))
      { temp<-paste(rriskSessionSsystems[[i]]@name," (",rriskSessionSsystems[[i]]@systemtype,")",sep="")
        ssystemNamesAvailable<-c(ssystemNamesAvailable,temp)
      }
      choices<-c(ssystemNamesAvailable,"Exit dialog")
      input<-99
      #------------------------------------
    } else if(input==(length(ssystemNamesAvailable)+1))
    { break()
    }
    input<-99
  }
} # end of function menu.copySsystem()



################################################################################
################################################################################
#' @description This function generates the user menu for creating a new model with various possibilities.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.createModel
#' @aliases menu.createModel
#' @title Function generating user menu for creating a new model with various possibilities
#' @usage menu.createModel(rriskSessionModels,menuLevel=1)
#' @param rriskSessionModels are the slot models from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \dontrun{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.createModel(rriskSession@@scoringsystems,menuLevel=1)}

menu.createModel<-function(rriskSessionModels,menuLevel=1)
{ levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="") 
  on.exit(return(invisible(rriskSessionModels))) 
  choices<-c("Empty model",
            "Load model definition template (Rmdt.txt file)",
            "Load rrisk model object (.Rdata file)",
            "Duplicate an available model",
            "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the way for creating a new model",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { rriskSessionModels<-createEmptyModel(rriskSessionModels)
    } else if(input==2)
    { cat("\nSorry, this option is still under evaluation...\n")
    } else if(input==3)
    { rriskSessionModels<-modelLoad(rriskSessionModels)
    } else if(input==4)
    { if(length(rriskSessionModels)>0)
      { rriskSessionModels<-menu.copyModel(rriskSessionModels,menuLevel=menuLevel+1)
      } else cat("The list of 'rrisk' models is empty!\n")
    } else if(input==5)
    { break()
    }
    input<-99
  }
} # end of function menu.createModel()



################################################################################
################################################################################
#' @description This function list all the models and offers them to be removed.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.removeModel
#' @aliases menu.removeModel
#' @title Function listing all the models and offering them to be removed 
#' @usage menu.removeModel(rriskSessionModels,menuLevel=1)
#' @param rriskSessionModels are the slot models from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.removeModel(rriskSession@@models,menuLevel=1)}

menu.removeModel<-function(rriskSessionModels,menuLevel=1)
{ on.exit(return(invisible(rriskSessionModels)))  
  
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  if(length(rriskSessionModels)>0)
  { modelNames<-c()
    for(i in 1:length(rriskSessionModels))
    { #if(rriskSessionModels[[i]]@modeltype=="user defined")
      #{ 
      modelNamesTemp<-paste(rriskSessionModels[[i]]@name@name," (",rriskSessionModels[[i]]@modeltype,")",sep="")
      modelNames<-c(modelNames,modelNamesTemp)    
      #}
    }
    choices<-c(modelNames,"Exit dialog")
    input<-99
    while(!is.element(input,seq(1:length(choices))) & length(choices)>1)
    { input<-mymenu(title="Choose model(s) you wish to remove",choices=choices,
      part="NA",help="No further help available",levelTabulator=levelTabulator)
      if(is.element(input,1:length(modelNames)))
      { input<-as.numeric(input)
        removeModelConfirm<-tkmessageBox(title="Delete model",icon="question",type="yesno",
          message=paste("Do you really wish to remove model '",modelNames[input],"' from the 'rrisk' session?",sep=""))
        if(tclvalue(removeModelConfirm)=="yes")
        { for(i in 1:length(rriskSessionModels))
          { temp<-paste(rriskSessionModels[[i]]@name@name," (",rriskSessionModels[[i]]@modeltype,")",sep="")
            if(temp==modelNames[input]) indexToRemove<-i
          }
          nameOfRemovedModel<-modelNames[input]
          rriskSessionModels<-rriskSessionModels[-indexToRemove]
          cat(paste("The model '",modelNames[input],"' has been successfully removed from the 'rrisk' session.\n",sep=""))
          #---------------------
          if(length(rriskSessionModels)>0)
          {  modelNames<-c()
            for(i in 1:length(rriskSessionModels))
            { #if(rriskSessionModels[[i]]@modeltype=="user defined")
              #{ 
              modelNamesTemp<-paste(rriskSessionModels[[i]]@name@name," (",rriskSessionModels[[i]]@modeltype,")",sep="")
              modelNames<-c(modelNames,modelNamesTemp)    
              #}
            }
            choices<-c(modelNames,"Exit dialog")
          } else break()
          #---------------------
        } #end if(tclvalue(removeModelConfirm)=="yes")
      } else if (input==(length(modelNames)+1))
      { break()
      }
      input<-99
    }
  } 
} # end of function menu.removeModel()




################################################################################
################################################################################
#' @description This function generates the user menu for creating scoring systems with various possibilities.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.createSsystem
#' @aliases menu.createSsystem
#' @title Function generating the user menu for creating scoring systems with various possibilities
#' @usage menu.createSsystem(rriskSessionSSystems,menuLevel=1)
#' @param rriskSessionSSystems is the slot scoringsystem from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.createSsystem(rriskSession@@scoringsystems,menuLevel=1)}

menu.createSsystem<-function(rriskSessionSSystems,menuLevel=1)
{ levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="") 
  on.exit(return(invisible(rriskSessionSSystems))) 
  choices<-c("Empty scoring system",
            "Load scoring system definition template (Rsdt.txt file)",
            "Load rrisk scoring system object (.Rdata file)",
            "Duplicate an available scoring system",
            "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the way for creating/loading a new scoring system",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { rriskSessionSSystems<-createEmptySsystem(rriskSessionSSystems)
    } else if(input==2)
    { cat("\nSorry, this option is still under evaluation...\n")
    } else if(input==3)
    { rriskSessionSSystems<-ssystemLoad(rriskSessionSSystems)
    } else if(input==4)
    { rriskSessionSSystems<-menu.copySsystem(rriskSessionSSystems,menuLevel=menuLevel+1)
    } else if(input==5)
    { break()
    }
    input<-99
  }
} # end of function menu.createSsystem()




################################################################################
################################################################################
#' @description This function lists all available scoring systems and offers them to be edited.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.editSsystems
#' @aliases menu.editSsystems
#' @title Function listing all available scoring systems and offering them to be edited
#' @usage menu.editSsystems(rriskSessionSSystems,menuLevel=1)
#' @param rriskSessionSSystems is the slot scoringsystem from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSessionSSystems<-init.rriskSession(useDemoModels = "all")@@scoringsystems
#' menu.editSsystems(rriskSessionSSystems,menuLevel=1)}

menu.editSsystems<-function(rriskSessionSSystems,menuLevel=1)
{ levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  on.exit(return(invisible(rriskSessionSSystems)))
  ssystemsNames<-c()
  availableSsystems<-c()
  for(i in 1:length(rriskSessionSSystems))
  { availableSsystems<-c(availableSsystems,rriskSessionSSystems[[i]]@name) 
    if(rriskSessionSSystems[[i]]@systemtype=="user defined")
    { temp<-paste(rriskSessionSSystems[[i]]@name," (",rriskSessionSSystems[[i]]@systemtype,")",sep="")  
      ssystemsNames<-c(ssystemsNames,temp)
    }
  }
  choices<-c(ssystemsNames,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the scoring system you wish to edit",
      choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(ssystemsNames)))
    { input<-as.numeric(input)
      for(i in 1:length(rriskSessionSSystems))
      { temp<-paste(rriskSessionSSystems[[i]]@name," (",rriskSessionSSystems[[i]]@systemtype,")",sep="")  
        if(temp==ssystemsNames[input])
        { 
          availableSsystems<-setdiff(availableSsystems,temp)
          rriskSessionSSystems[[i]]<-change.scoring(scoring = rriskSessionSSystems[[i]],availableSsystems)                                                         ## object1 entfernt
        }
      }
    } else if(input==(length(ssystemsNames)+1))
    { break()
    } 
    input<-99
  }
} # end of function menu.editSsystem()



################################################################################
################################################################################
#' @description This function lists all available scoring systems and offers them to be exported.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.exportSsystem
#' @aliases menu.exportSsystem
#' @title Function listing all available scoring systems and offering them to be exported
#' @usage menu.exportSsystem(rriskSessionSSystems,menuLevel=1)
#' @param rriskSessionSSystems is the slot scoringsystem from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.exportSsystem(rriskSession@@scoringsystems,menuLevel=1)}

menu.exportSsystem<-function(rriskSessionSSystems,menuLevel=1)
{ levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  ssystemsNames<-c()
  for(i in 1:length(rriskSessionSSystems))
  { temp<-paste(rriskSessionSSystems[[i]]@name," (",rriskSessionSSystems[[i]]@systemtype,")",sep="")  
    ssystemsNames<-c(ssystemsNames,temp) 
  }
  choices<-c(ssystemsNames,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the scoring system you wich to export",
      choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(ssystemsNames)))
    { input<-as.numeric(input)
      menu.exportSsystemFormat(rriskSsystem=rriskSessionSSystems[[input]],menuLevel=menuLevel+1)
    } else if(input==(length(ssystemsNames)+1))
    { break()
    } 
    input<-99
  }
} # end of function menu.exportSsystem()



################################################################################
################################################################################
#' @description This function generates the user menu for exporting a scoring system with various possibilities. 
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.exportSsystemFormat
#' @aliases menu.exportSsystemFormat
#' @title Function generating the user menu for exporting a scoring system with various possibilities
#' @usage menu.exportSsystemFormat(rriskSsystem,menuLevel=1)
#' @param rriskSsystem is a sigle one slot of the slot scoringsystems from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.exportSsystemFormat(rriskSession@@scoringsystems[[1]],menuLevel=1)}

menu.exportSsystemFormat<-function(rriskSsystem,menuLevel=1)
{ levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  choices<-c("Export scoring system definition template (.Rsdt file)",
            "Export rrisk scoring system object (.Rdata file)",
            "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the way how you wish to export the scoring sysrtem",
      choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { writeOut(rriskSsystem)
    } else if(input==2)
    { ssystemSave(rriskSsystem)
    } 
    else if(input==3)
    { break()
    } 
    input<-99
  }
} # end of function menu.exportSsystemFormat()



################################################################################
################################################################################
#' @description This function generates the user menu for viewing all listed scoring systems available.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.viewSsystem
#' @aliases menu.viewSsystem
#' @title Function generating the user menu for viewing all listed scoring systems available
#' @usage menu.viewSsystem(rriskSessionSSystems,menuLevel=1)
#' @param rriskSessionSSystems are the slot scoringsystems from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.viewSsystem(rriskSession@@scoringsystems,menuLevel=1)}

menu.viewSsystem<-function(rriskSessionSSystems,menuLevel=1)
{ levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  ssystemsNames<-c()
  for(i in 1:length(rriskSessionSSystems))
  { temp<-paste(rriskSessionSSystems[[i]]@name," (",rriskSessionSSystems[[i]]@systemtype,")",sep="")
    ssystemsNames<-c(ssystemsNames,temp) 
  }
  choices<-c(ssystemsNames,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose scoring system you wich to view",
      choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(ssystemsNames)))
    { input<-as.numeric(input)
      show(rriskSessionSSystems[[input]])
    } else if(input==(length(ssystemsNames)+1))
    { break()
    } 
    input<-99
  }
} # end of function menu.viewSsystem()



################################################################################
################################################################################
#' @description This function lists all available scoring systems and offers them to be removed.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.removeSsystem
#' @aliases menu.removeSsystem
#' @title Function listing all available scoring systems and offering them to be removed
#' @usage menu.removeSsystem(rriskSessionSSystems,menuLevel=1)
#' @param rriskSessionSSystems are the slot scoringsystems from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' rriskSession@@scoringsystems<-menu.removeSsystem(rriskSession@@scoringsystems,
#' menuLevel=1)}

menu.removeSsystem<-function(rriskSessionSSystems,menuLevel=1)
{ on.exit(return(invisible(rriskSessionSSystems)))  
  
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  #require(tcltk)
  
  if(length(rriskSessionSSystems)>0)
  { ssystemsNames<-c()
    for(i in 1:length(rriskSessionSSystems))
    { if(rriskSessionSSystems[[i]]@systemtype=="user defined")
      { ssystemsNamesTemp<-paste(rriskSessionSSystems[[i]]@name," (",rriskSessionSSystems[[i]]@systemtype,")",sep="")
        ssystemsNames<-c(ssystemsNames,ssystemsNamesTemp)    
      }
    }
    choices<-c(ssystemsNames,"Exit dialog")
    input<-99
    while(!is.element(input,seq(1:length(choices))) & length(choices)>1)
    { input<-mymenu(title="Choose the scoring system you wish to remove",choices=choices,
        part="NA",help="No further help available",levelTabulator=levelTabulator)
      if(is.element(input,1:length(ssystemsNames)))
      { input<-as.numeric(input)
        removeSsystemConfirm<-tkmessageBox(title="Delete scoring system",icon="question",type="yesno",
          message=paste("Do you really wish to remove '",ssystemsNames[input],"' from the 'rrisk' session?",sep=""))
        if(tclvalue(removeSsystemConfirm)=="yes")
        { for(i in 1:length(rriskSessionSSystems))
          { temp<-paste(rriskSessionSSystems[[i]]@name," (",rriskSessionSSystems[[i]]@systemtype,")",sep="")
            if(temp==ssystemsNames[input]) indexToRemove<-i
          }
          nameOfRemovedSsystem<-ssystemsNames[input]
          rriskSessionSSystems<-rriskSessionSSystems[-indexToRemove]
          cat(paste("The scoring system '",ssystemsNames[input],"' has been successfully removed from the 'rrisk' session.\n",sep=""))
          #--------------
          ssystemsNames<-c()
          for(i in 1:length(rriskSessionSSystems))
          { if(rriskSessionSSystems[[i]]@systemtype=="user defined")
            { ssystemsNamesTemp<-paste(rriskSessionSSystems[[i]]@name," (",rriskSessionSSystems[[i]]@systemtype,")",sep="")
              ssystemsNames<-c(ssystemsNames,ssystemsNamesTemp)    
            }
          }
          choices<-c(ssystemsNames,"Exit dialog")
          #--------------
        } # end if(tclvalue(removeSsystemConfirm)=="yes")
      } else if (input==(length(ssystemsNames)+1))
      { break()
      }
      input<-99
    }
  } else
  { tkmessageBox(message=paste("The list of scorings systems is empty!",sep=""),icon="info",type="ok")
  }
} # end of function menu.removeSsystem()




################################################################################
################################################################################
#' @description This function generates the user menu for maintaining scoring systems.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.Ssystems
#' @aliases menu.Ssystems
#' @title Function generating the user menu for maintaining scoring systems
#' @usage menu.Ssystems(rriskSessionSSystems,menuLevel=1)
#' @param rriskSessionSSystems are the slot scoringsystems from an instance of \code{rriskClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords menu
#' @export
#' @examples
#' \donttest{rriskSessionSSystems<-init.rriskSession(useDemoModels = "all")@@scoringsystems
#' menu.Ssystems(rriskSessionSSystems,menuLevel=1)}

menu.Ssystems<-function(rriskSessionSSystems,menuLevel=1)
{ on.exit(return(invisible(rriskSessionSSystems)))    
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  #require(tcltk)    
  choices<-c( "View scoring systems (summed up)",
              "View scoring systems (full info)",
              "Load/new scoring system",
              "Edit scoring system",
              "Save/Export scoring system",
              "Remove scoring system",    
              "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose the action you wish to execute",choices=choices,
      part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { if(length(rriskSessionSSystems)>0)
      { for(i in 1:length(rriskSessionSSystems))
        sumUp(rriskSessionSSystems[[i]])
        #cat("\n") 
      } else print("There are no scoring systems available in the current rrisk session!")
    } else if(input==2)
    { menu.viewSsystem(rriskSessionSSystems,menuLevel=menuLevel+1)
    } else if (input==3)
    { rriskSessionSSystems<-menu.createSsystem(rriskSessionSSystems,menuLevel=menuLevel+1)
    } else if (input==4)
    { rriskSessionSSystems<-menu.editSsystems(rriskSessionSSystems,menuLevel=menuLevel+1)
    } else if(input==5)
    { menu.exportSsystem(rriskSessionSSystems,menuLevel=menuLevel+1)
    } else if(input==6)
    { rriskSessionSSystems<-menu.removeSsystem(rriskSessionSSystems,menuLevel=menuLevel+1)
    } else if(input==7)
    { break()
    }
    input<-99
  }
} # end of function menu.scoringSystems()




################################################################################
################################################################################
#' @description This function generates the menu for loading demo models.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.loadDemoModels
#' @aliases menu.loadDemoModels
#' @title Function generating the menu for loading demo models
#' @usage menu.loadDemoModels(rriskSession,demomode=FALSE,menuLevel=1)
#' @param rriskSession is an instance of \code{rriskClass}
#' @param demomode is a boolean value indicating whether the model should be run under the demo mode, \code{demomode=c(TRUE,FALSE)}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @return the new rriskSession object
#' @keywords menu
#' @export              
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.loadDemoModels(rriskSession, menuLevel=1)}


menu.loadDemoModels <- function(rriskSession, demomode=FALSE, menuLevel = 1) {
  on.exit(return(invisible(rriskSession)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  checkExistingModels <- function(rriskSession, modelName) {
    flag <- FALSE
    if (length(rriskSession@models) == 0) {
      flag <- FALSE
    } else {
      for (i in c(1:length(rriskSession@models))){
        if ( rriskSession@models[[i]]@name@name == modelName ) flag <- TRUE
      } # end for (i in c(1:length(rriskSession@models))){
    } # end if (length(rriskSession@models) == 0) {
    return(flag)
  } # end of function checkExistingModels
  
  #-----------------------------------------------------------------------------
  # function for loading demo model 1
  #-----------------------------------------------------------------------------
  load.demomodel1<-function(rriskSession,demomode=FALSE){
    on.exit(return(invisible(rriskSession)))
    if(checkExistingModels(rriskSession, "Demo model 1")) {
      cat("Demo model 1 already exists.\n")
    } else {
      try.result<-try(demomodel1<-init.Model1(demomode),silent=TRUE)
      if (inherits(try.result, "try-error")){
        cat("An arror occur during loading Demo model 1!\n")
      } else {
        rriskSession@models <- c(rriskSession@models, demomodel1)
        cat("Demo model 1 has been successfully loaded.\n")
      } # end if (inherits(try.result, "try-error")){  
    } # end if(checkExistingModels(rriskSession, "Demo model 1")) {
  } # end of function load.demomodel1()
  
  #-----------------------------------------------------------------------------
  # function for loading demo model 2
  #-----------------------------------------------------------------------------
  load.demomodel2<-function(rriskSession,demomode=FALSE){
    on.exit(return(invisible(rriskSession)))
    if(checkExistingModels(rriskSession, "Demo model 2")) {
      cat("Demo model 2 already exists.\n")
    } else {
      if(demomode==TRUE){
        try.result<-try(load("demomodel2.Rdata"),silent=TRUE)
      } else {
        try.result<-try(load(file=system.file("extdata","demomodel2.Rdata", package = "rrisk")),silent=TRUE)
      } # end if(demomode==TRUE){ 
      
      if (inherits(try.result, "try-error")){
        cat("An arror occur during loading Demo model 2!\n")
      } else {
        demomodel2<-rriskModel
        demomodel2@name@name<-"Demo model 2"
        rriskSession@models <- c(rriskSession@models, demomodel2)
        cat("Demo model 2 has been successfully loaded.\n")
      } # end if (inherits(try.result, "try-error")){   
    } # end if(checkExistingModels(rriskSession, "Demo model 2")) {  
  } # end of function load.demomodel2()
  
  #-----------------------------------------------------------------------------
  # function for loading demo model 3
  #-----------------------------------------------------------------------------
  load.demomodel3<-function(rriskSession,demomode=FALSE){
    on.exit(return(invisible(rriskSession)))
    if(checkExistingModels(rriskSession, "Demo model 3")) {
      cat("Demo model 3 already exists.\n")
    } else {
      if(demomode==TRUE){
        try.result<-try(load("Demomodel3.Rdata"),silent=TRUE)
      } else {
        try.result<-try(load(file=system.file("extdata","Demomodel3.Rdata", package = "rrisk")),silent=TRUE)
      }
      if (inherits(try.result, "try-error")){
        cat("An arror occur during loading Demo model 3!\n")
      } else {
        demomodel3<-rriskModel
        demomodel3@name@name<-"Demo model 3"
        rriskSession@models <- c(rriskSession@models, demomodel3)
        cat("Demo model 3 has been successfully loaded.\n")
      } # end if (inherits(try.result, "try-error")){   
    } # end if(checkExistingModels(rriskSession, "Demo model 3")) {
  } # end of function load.demomodel3()


  choices <- c(
    "Demo model 1",
    "Demo model 2",
    "Demo model 3",
    "All demo models",
    "Exit dialog"
  )

  input <- 99
  while(!is.element(input,seq(1:length(choices)))) {
    input<-mymenu(title="Choose the demo model you wish to load",choices=choices,
      part="NA",help="No further help available",levelTabulator=levelTabulator)
    if (input == 1) {
      rriskSession<-load.demomodel1(rriskSession,demomode=demomode)    
    } else if (input == 2) {
      rriskSession<-load.demomodel2(rriskSession,demomode=demomode)   
    } else if (input == 3) {
      rriskSession<-load.demomodel3(rriskSession,demomode=demomode)
    } else if (input == 4) {
      rriskSession<-load.demomodel1(rriskSession,demomode=demomode)
      rriskSession<-load.demomodel2(rriskSession,demomode=demomode)
      rriskSession<-load.demomodel3(rriskSession,demomode=demomode)
    } else if (input == 5) {
      break()
    }
    input <- 99
  }
} # end of function menu.loadDemoModels



################################################################################
#menu.loadDemoModels.old <- function(rriskSession, demomode=FALSE, menuLevel = 1) {
#  # on.exit(return(invisible(rriskSession)))
#  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
#  levelTabulator<-paste("\n",levelTabulator,sep="")
#
#  checkExistingModels <- function(rriskSession, modelName) {
#    flag <- FALSE
#    if (length(rriskSession@models) == 0) {
#      flag <- FALSE
#    } else {
#      for (i in c(1:length(rriskSession@models))){
#        if ( rriskSession@models[[i]]@name@name == modelName ) flag <- TRUE
#      } # end for (i in c(1:length(rriskSession@models))){
#    } # end if (length(rriskSession@models) == 0) {
#    return(flag)
#  } # end of function checkExistingModels
#
#  #loadModels <- function(rriskSession, input) {
#  #  existingDemoModelNames <- c()
#  #  demoModel1 <- init.Model1()
#  #  demoModel2 <- init.Model1()
#  #  demoModel2@name@name <- "Demo model 2"
#  #  toLoadModelNames <- c(demoModel1@name@name, demoModel2@name@name)
#  #  #for (i in c(1:length(rriskSession@models))) {
#  #  #  existingDemoModelNames <- c(existingDemoModelNames, rriskSession@models[[i]]@name@name)
#  #  #}
#  #  exist <- checkExistingModels(rriskSession, toLoadModelNames[input])
#  #  if (!exist){
#  #    rriskSession@models <- c(rriskSession@models, toLoadModels[input])
#  #    print("Demo models have been successfully loaded into the rrisk session!\n")
#  #  } else print(paste(toLoadModels[[input]]@name@name, "already exists."))
#  #}
#
#  choices <- c(
#    "Demo model 1",
#    "Demo model 2",
#    "Both demo models",
#    "Exit dialog"
#  )
#
#  input <- 99
#  while(!is.element(input,seq(1:length(choices)))) {
#    input<-mymenu(title="Choose the demo model you wish to load",choices=choices,
#      part="NA",help="No further help available",levelTabulator=levelTabulator)
#    if (input == 1) {
#      # loadModels(rriskSession, 2)
#      if(checkExistingModels(rriskSession, "Demo model 1")) {
#        cat("Demo model 1 already exists.\n")
#      } else {
#        rriskSession@models <- c(rriskSession@models, init.Model1(demomode))
#        cat("Demo model 1 has been successfully loaded.\n")
#      }
#    } else if (input == 2) {
#      # loadModels(rriskSession, 2)
#
#      if(checkExistingModels(rriskSession, "Demo model 2")) {
#        cat("Demo model 2 already exists.\n")
#      } else {
#        demoModel2 <- init.Model1(demomode)
#        demoModel2@name@name <- "Demo model 2"
#        rriskSession@models <- c(rriskSession@models, demoModel2)
#        cat("Demo model 1 has been successfully loaded.\n")
#      }
#    } else if (input == 4) {
#      # loadModels(rriskSession, 1)
#      # loadModels(rriskSession, 2)
#      if(checkExistingModels(rriskSession, "Demo model 1")) {
#        cat("Demo model 1 already exists.\n")
#      } else {
#        rriskSession@models <- c(rriskSession@models, init.Model1(demomode))
#        cat("Demo model 1 has been successfully loaded.\n")
#      }
#
#      if(checkExistingModels(rriskSession, "Demo model 2")) {
#        cat("Demo model 2 already exists.\n")
#      } else {
#        demoModel2 <- init.Model1(demomode)
#        demoModel2@name@name <- "Demo model 2"
#        rriskSession@models <- c(rriskSession@models, demoModel2)
#        cat("Demo model 2 has been successfully loaded.\n")
#      }
#    } else if (input == 4) {
#      break()
#    }
#    input <- 99
#  }
#  return(rriskSession)
#} # end of function menu.loadDemoModels



################################################################################
################################################################################
#' @description This function generates the main menu of rrisk.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.mainMenu
#' @aliases menu.mainMenu
#' @title Function generating the main menu of rrisk
#' @usage menu.mainMenu(rriskSession,demomode=FALSE)
#' @param rriskSession is an instance of \code{rriskClass}
#' @param demomode is a boolean value indicating whether the model should be run under the demo mode, \code{demomode=c(TRUE,FALSE)}
#' @keywords menu
#' @export              
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' menu.mainMenu(rriskSession)}

menu.mainMenu<-function(rriskSession,demomode=FALSE)
{ menuLevel<-1
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  on.exit(return(invisible(rriskSession)))
  choices<-c("Session infos",
              "Available models",
              "Scoring systems",
              "Create new model",
              "Save model",
              "Remove model",
              "Load demo models",
              "View/edit/run model",
              "Set working directory",
              "Show working directory",
              "About rrisk",
              "Exit rrisk")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="rrisk main menu",choices=choices,part="NA",
      help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { sumUp(rriskSession)
    } else if(input==2)
    { showAvailableModels(rriskSession)
    } else if(input==3)
    { rriskSession@scoringsystems<-menu.Ssystems(rriskSession@scoringsystems,menuLevel=menuLevel+1)
    } else if(input==4)
    { rriskSession@models<-menu.createModel(rriskSession@models,menuLevel=menuLevel+1)
    } else if (input==5)
    { if(length(rriskSession@models)>0)
      { menu.saveModel(rriskSession@models,menuLevel=menuLevel+1)
      } else cat("The list of 'rrisk' models is empty!\n")
    } else if(input==6)
    { if(length(rriskSession@models)>0)
      { rriskSession@models<-menu.removeModel(rriskSession@models,menuLevel=menuLevel+1)
      } else cat("The list of 'rrisk' models is empty!\n")
    } else if(input==7)
    { # rriskSession@models<-c(rriskSession@models,init.Model1())
      rriskSession <- menu.loadDemoModels(rriskSession, demomode,menuLevel = menuLevel + 1)
      # cat("Demo models have been successfully loaded into the rrisk session!\n")
    } else if(input==8)
    { if(length(rriskSession@models)>0)
      { rriskSession@models<-menu.chooseModel(rriskSession,menuLevel=menuLevel+1)
      } else cat("The list of 'rrisk' models is empty!\n")
    } else if(input==9)
    { workingdir<-rrisk.chooseDir()
      if(nchar(workingdir)>0)
      { setwd(workingdir)
      }
    } else if(input==10)
    { workdir<-getwd()
      cat(workdir,"\n")
    } else if(input==11)
    { rriskAbout(disclaimer=rriskSession@disclaimer)
    } else if(input==12)
    { mess<-paste("Do you wish to save 'user defined' models and scoring systems befor closing 'rrisk' session? If no, all user created models and scoring systems will get lost!",sep="")
      exitSure<-tkmessageBox(message=mess,icon="question",type="yesno")
      if(tclvalue(exitSure)=="no") break()
    }
    input<-99
  }
} # end of function menu.mainMenu()
