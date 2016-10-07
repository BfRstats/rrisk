


################################################################################
################################################################################
#' @name get.dataForWith
#' @aliases get.dataForWith
#' @title Non-executable auxiliary function
#' @usage get.dataForWith(item,rriskModel,catResults=FALSE,fullc=TRUE,run=FALSE,run2d=FALSE,catEval=TRUE)
#' @param item ...
#' @param rriskModel  ...
#' @param catResults  ...
#' @param fullc  ...
#' @param run  ...
#' @param run2d  ...
#' @param catEval ...
#' @keywords items
#' @export

get.dataForWith<-function(item,rriskModel,catResults=FALSE,fullc=TRUE,run=FALSE,run2d=FALSE,catEval=TRUE){
  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  items<-rriskModel@items@items
  availableItems<-c()
  availableFdocs<-c()
  valuesOfEvaluatedItems<-c()
  dataForWith<-list()
  strata.names<-list() # names of strata of all available stra-items

  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(dataForWith))

  #-----------------------------------------------------------------------------
  # Evaluiere jeden item
  #-----------------------------------------------------------------------------
  if(length(items)>0){
  for(i in 1:length(items)){
    if(!is.null(items[[i]])){
      if(items[[i]]@name!=item@name){
        #-----------------------------------------------------------------------
        # set number of test evaluation to rriskModel@settings@Ntest
        #-----------------------------------------------------------------------
        if(run==FALSE){
          items[[i]]@fullc<-gsub(x=items[[i]]@fullc,pattern="rriskModel@settings@N",replacement="rriskModel@settings@Ntest")
          items[[i]]@relaxc<-gsub(x=items[[i]]@relaxc,pattern="rriskModel@settings@N",replacement="rriskModel@settings@Ntest")
        } # end  if(run==FALSE){
        #-----------------------------------------------------------------------
        availableItems<-c(availableItems,items[[i]]@name)
        if(items[[i]]@typecode=="stra"){
          strata.names[[length(strata.names)+1]]<-as.character(items[[i]]@data$stratum$stid)
          names(strata.names)[length(strata.names)]<-items[[i]]@name
          dataForWith[[length(dataForWith)+1]]<-items[[i]]@data
          names(dataForWith)[length(dataForWith)]<-items[[i]]@name
          if(catEval==TRUE)cat(paste("\nEvaluating ",items[[i]]@name," item...OK\n",sep=""))
        } else if(items[[i]]@typecode=="data"){   # sammele alle Datensätze
          dataForWith[[length(dataForWith)+1]]<-items[[i]]@data
          names(dataForWith)[length(dataForWith)]<-items[[i]]@name
          if(catEval==TRUE)cat(paste("\nEvaluating ",items[[i]]@name," item...OK\n",sep=""))
        } else if(items[[i]]@typecode=="fdoc"){ # sammele alle benutzerdefinierte Funktionen
          availableFdocs<-c(availableFdocs,items[[i]]@name)
          assign(items[[i]]@name,items[[i]]@data,envir=.GlobalEnv)
          if(catEval==TRUE)cat(paste("\nEvaluating ",items[[i]]@name," item...OK\n",sep=""))
        } else if(items[[i]]@typecode!="bdjp" & items[[i]]@typecode!="stra"){
          if(fullc==TRUE){
            textForWith<-items[[i]]@fullc
          } else {
            textForWith<-items[[i]]@relaxc
          }
          # versuche, Item-Kommado zu evaluieren
          tryResult<-try(with(data=dataForWith,eval(parse(text=textForWith))),silent=TRUE)
          if (inherits(tryResult, "try-error") | is.null(tryResult)){
            cat(paste("\nEvaluating ",items[[i]]@name," item...ERROR\n",sep=""))
            #stop("Item ",items[[i]]@name," could not be evaluated or has been evaluated to NULL!\n",call.=FALSE)
            cat("Item ",items[[i]]@name," could not be evaluated or has been evaluated to NULL!\n")
            next()
          } else { # falls keine Fehlermeldung -> Item erfolgreich evaluiert
            if(items[[i]]@stratum!="" & items[[i]]@typecode!="mxrv"){
              stratum<-items[[i]]@stratum
              corresponding.strata.item<-which(names(strata.names)==stratum)
              if(length(corresponding.strata.item)>0){ # also falls es einen strata-item gibt
                if(items[[i]]@typecode=="numv"){
                  names(tryResult)<-strata.names[[corresponding.strata.item]]
                } else {
                  colnames(tryResult)<-strata.names[[corresponding.strata.item]]
                  tryResult<-data.frame(tryResult)
                }
                if(catEval==TRUE)cat(paste("\nEvaluating ",items[[i]]@name," item...OK\n",sep=""))
              } else {
                cat(paste("\nEvaluating ",items[[i]]@name," item...ERROR\n",sep=""))
                warning(paste("There is no strata item that corresponds to the stratified item '",items[[i]]@name,"'!",sep=""),call.=FALSE,immediate.=TRUE)
              }
            } else {
              if(catEval==TRUE)cat(paste("\nEvaluating ",items[[i]]@name," item...OK\n",sep=""))
            }
            dataForWith[[length(dataForWith)+1]]<-tryResult
            names(dataForWith)[length(dataForWith)]<-items[[i]]@name
            #-------------------------------------------------------------------
            #
            #-------------------------------------------------------------------
            if(run2d==TRUE){
              if(items[[i]]@typecode=="mcrv" & (items[[i]]@rolecode=="u" | items[[i]]@rolecode=="uv")){
                dataForWith[[length(dataForWith)]]<-as.vector(dataForWith[[length(dataForWith)]])
              }
            }
            if(catResults==TRUE){
              if(fullc==TRUE){
                cat("Full command = ",items[[i]]@fullc,"\n")
              } else {
                cat("Relax command = ",items[[i]]@relaxc,"\n\n")
              }
            }
          } # end  if (inherits(tryResult, "try-error"))
        }
      } else break()
      }
    } # end for
  } # end if(length(items)>0)
}# end of function get.dataForWith()




################################################################################
################################################################################
#' @name menu.define.mxrv
#' @aliases menu.define.mxrv
#' @title Non-executable auxiliary function
#' @usage menu.define.mxrv(item,rriskModel,menuLevel=1)
#' @param item  ...
#' @param rriskModel  ...
#' @param menuLevel  ...
#' @keywords items
#' @export

menu.define.mxrv<-function(item,rriskModel,menuLevel=1){
  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  #-----------------------------------------------------------------------------
  # go into teh while-loop
  #-----------------------------------------------------------------------------
  choices<-c("by sampling from stratified mcrv item","by sampling from several non-stratified items","Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices)))){
  input<-mymenu(title="Please choose the way how you wish to define the mxrv item",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input==1){
      #--------------------------------------------------------------------------
      #by sampling from stratified mcrv item
      #-------------------------------------------------------------------------
      item<-menu.define.mxrv.stramcrv(item,rriskModel,menuLevel=menuLevel+1)
      break()
    } else if (input==2){
      #-------------------------------------------------------------------------
      # by sampling from several non-stratified items
      #-------------------------------------------------------------------------
      item<-menu.define.mxrv.nonstramcrv(item,rriskModel,menuLevel=menuLevel+1)
      break()
    } else if (input==length(choices)){
      break()
    }
    input<-99
  } # end while
} # end of fucntion menu.define.mxrv



################################################################################
################################################################################
#' @name menu.define.mxrv.stramcrv
#' @aliases menu.define.mxrv.stramcrv
#' @title Non-executable auxiliary function
#' @usage menu.define.mxrv.stramcrv(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel  ...
#' @param menuLevel  ...
#' @keywords items
#' @export

menu.define.mxrv.stramcrv<-function(item,rriskModel,menuLevel=1){
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  items<-rriskModel@items@items
  mcrv.items<-c()
  mcrv.items.index<-c()
  stratum.item.index<-c()
  
  #-----------------------------------------------------------------------------
  # get all mcrv items with the same statum as current mxrv item
  #-----------------------------------------------------------------------------
  if(length(items)>0){
    items<-items[1:(length(items)-1)]
    for(i in 1:length(items)){
      if(items[[i]]@typecode=="mcrv" & items[[i]]@stratum==item@stratum){
        mcrv.items<-c(mcrv.items,items[[i]]@name)
        mcrv.items.index<-c(mcrv.items.index,i)
      } else if(items[[i]]@typecode=="stra" & items[[i]]@name==item@stratum){
        stratum.item.index<-i
      }# end if(items[[i]]@typecode=="mcrv" & items[[i]]@stratum==item@stratum){
    } # end for(i in 1:length(items)){
    if(length(mcrv.items)==0){
      stop("\nThe model does not contain any mcrv items with the same stratum as the current mxrv item!\n",call.=FALSE)
    } # end if(length(mcrv.items)==0){
  } else {
    stop("\nThe model does not contain any items!\n",call.=FALSE)
  }# end if(length(items)>0){
  
  #-----------------------------------------------------------------------------
  # show corresponding strata
  #-----------------------------------------------------------------------------
  cat("\nThe corresponding strata are:\n")
  print(items[[stratum.item.index]]@data$stratum)
  
  #-----------------------------------------------------------------------------
  # falls einige mcrv items mit der gleichen schichtung wie mxrv vorhanden gehe weiter
  #-----------------------------------------------------------------------------
  choices<-c(mcrv.items,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices)))){
  input<-mymenu(title="Please choose stratified mcrv item to define the mxrv item",choices=choices,help="No further help available",levelTabulator=levelTabulator)
  input<-as.numeric(input)
    if(input!=length(choices)){
      item<-define.mxrv.stramcrv(item,rriskModel,mcrv.item.name=mcrv.items[input])
      break()
    } else if (input==length(choices)){
      break()
    }
    input<-99
  } # end while
  
} # end of function menu.define.mxrv.stramcrv



################################################################################
################################################################################
#' @name define.mxrv.stramcrv
#' @aliases define.mxrv.stramcrv
#' @title Non-executable auxiliary function
#' @usage define.mxrv.stramcrv(item,rriskModel,mcrv.item.name)
#' @param item  ...
#' @param rriskModel  ...
#' @param mcrv.item.name   ...
#' @keywords items
#' @export

define.mxrv.stramcrv<-function(item,rriskModel,mcrv.item.name){
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(invisible(item)))

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  weights.string<-paste(item@stratum,"$strv",sep="")
  fullc<-paste("matrix(ncol=1,","data=","rrisksample(data=",mcrv.item.name,",weights=",weights.string,"),byrow=TRUE)",sep="")
  item.temp<-item
  item.temp@fullc<-fullc
  definition<-paste("Mixed distribution based on: ",mcrv.item.name,sep="")
  
  cat("\n----------------------------------------------------------------------\nEvaluating item values with predefined distribution parameters...\n----------------------------------------------------------------------\n")
  fullcValues<-itemsEvaluation(item.temp,rriskModel,run=TRUE)# hier wird die Ausführbarkeit von fullc geprüft
  cat("\n***************************************\nFull command for",item@name,"\n***************************************\n")
   if (inherits(fullcValues, "try-error")){
    cat("Full command expression",fullc,"could not be evaluated !\n")
   } else { # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
     colnames(fullcValues)<-item@name
     # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
     cat("\nDimension of test evaluation: ");cat(dim(fullcValues));cat("\n")
     cat("\nitem@fullc=",fullc,"\n")
     cat("\nGenerated item values (full command):\n");print(head(data.frame(fullcValues)));cat("...\n")
     cat("\nSummary statistics of item values (full command):\n"); print(summary(fullcValues),digits=4);cat("\n")
     item@fullc<-fullc
     cat("\n***************************************\nSymbolical definition for",item@name,"\n***************************************\n")
     cat("\nitem@definition=",definition,"\n")
     item@definition<-definition
     #--------------------------------------------------------------------------
     # save simulated item value in data-slot
     #--------------------------------------------------------------------------
     item@data<-summary(fullcValues)
   }
   #-----------------------------------------------------------------------------
   # hier wird die Ausführbarkeit von 'relaxc' geprüft
   #----------------------------------------------------------------------------
   plausimin<-item@plausimin
   plausimax<-item@plausimax
   relaxc<-paste("matrix(ncol=1,data=rudiscrete(n=rriskModel@settings@N,min=",plausimin,",max=",plausimax,"),byrow=TRUE)",sep="")
   relaxc.test<-paste("matrix(ncol=1,data=rudiscrete(n=rriskModel@settings@Ntest,min=",plausimin,",max=",plausimax,"),byrow=TRUE)",sep="")
   relaxcValues<-try(eval(parse(text=relaxc.test)),silent=TRUE)
   if (inherits(relaxcValues, "try-error"))
   { cat("Relax command expression",relaxc,"could not be evaluated !\n")
   }else
   { colnames(relaxcValues)<-item@name
     cat("\n***************************************\nRelax command for",item@name,"\n***************************************\n")
     cat("\nDimension of test evaluation: ");cat(dim(relaxcValues));cat("\n")
     cat("\nitem@relaxc=",relaxc,"\n")
     cat("\nGenerated item values (relax command):\n");print(head(data.frame(relaxcValues)));cat("...\n")
     cat("\nSummary statistics of item values (relax command):\n"); print(summary(relaxcValues),digits=4);cat("\n")
     item@relaxc<-relaxc
   } 
} # end of function define.mxrv.stramcrv()




################################################################################
################################################################################
#' @name menu.define.mxrv.nonstramcrv
#' @aliases menu.define.mxrv.nonstramcrv
#' @title Non-executable auxiliary function
#' @usage menu.define.mxrv.nonstramcrv(item,rriskModel,menuLevel=1)
#' @param item  ...
#' @param rriskModel ...
#' @param menuLevel   ...
#' @keywords items
#' @export

menu.define.mxrv.nonstramcrv<-function(item,rriskModel,menuLevel=1){
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  items.nonstra.names<-c()
  items.nonstra.index<-c()
  items<-rriskModel@items@items
  stratum.item<-c()
  
  #-----------------------------------------------------------------------------
  # get all non-stratified items and corresponding stratum-Item
  #-----------------------------------------------------------------------------
  if(length(items)>0){
    for(i in 1:length(items)){
      if(items[[i]]@typecode=="mcrv" & items[[i]]@stratum==""){
        items.nonstra.names<-c(items.nonstra.names,items[[i]]@name)
        items.nonstra.index<-c(items.nonstra.index,i)
      } else if (items[[i]]@typecode=="stra" & items[[i]]@name==item@stratum) {
        stratum.item<-items[[i]]
      }#  end if(items[[i]]@typecode=="mcrv" & items[[i]]@stratum==""){
    } # end for
    #---------------------------------------------------------------------------
    # falls kein stratum-Item gefunden
    #---------------------------------------------------------------------------
    if(length(stratum.item)==0){
      stop("\nThe model does not contain corresponding stratum item!\n",call.=FALSE)
    } else {
      stratum.length<-nrow(stratum.item@data$stratum)
    }# end if(length(stratum.item)==0){
    #---------------------------------------------------------------------------
    # falls keine nicht-geschichtete mcrv-Items vorhanden, breche ab!
    #---------------------------------------------------------------------------
    if(length(items.nonstra.names)==0){
      stop("\nThe model does not contain any non-stratified mcrv items!\n",call.=FALSE)
    } # end if(length(mcrv.items)==0){
  } else {
    stop("\nThe model does not contain any items!\n",call.=FALSE)
  }# end if(length(items)>0){
  
  #-----------------------------------------------------------------------------
  # show corresponding strata
  #-----------------------------------------------------------------------------
  cat("\nThe corresponding strata are:\n")
  print(stratum.item@data$stratum)
  
  #-----------------------------------------------------------------------------
  # falls einige mcrv items mit der gleichen schichtung wie mxrv vorhanden gehe weiter
  #-----------------------------------------------------------------------------
  choices<-c(items.nonstra.names,"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices)))){
    input<-mymenu1(title=paste("Please choose ",stratum.length," non-stratified mcrv items to define the mxrv item",sep=""),choices=choices,help="No further help available",levelTabulator=levelTabulator)
    input<-as.numeric(unique(input))
    if(!setequal(input,length(choices))){ # also falls nicht Exit gewählt
      if(length(input)==stratum.length){
        item<-define.mxrv.nonstramcrv(item,rriskModel,items.nonstra.index=items.nonstra.index[input],items.nonstra.names=items.nonstra.names[input],stratum.item=stratum.item)
        break()
      } else {
        cat("\nThe number of chosen non-stratified mcrv items does not correspond to the number of strata!\n")
        #next()
      }
    } else { 
      break()
    } # end if(setequal(input,1:length(rriskModelObject))){
    input<-99
  } # end while
} # end of function menu.define.mxrv.nonstramcrv



################################################################################
################################################################################
#' @name define.mxrv.nonstramcrv
#' @aliases define.mxrv.nonstramcrv
#' @title Non-executable auxiliary function
#' @usage define.mxrv.nonstramcrv(item,rriskModel,items.nonstra.index,items.nonstra.names,stratum.item)
#' @param item  ...
#' @param rriskModel  ...
#' @param items.nonstra.index  ...
#' @param items.nonstra.names  ...
#' @param stratum.item ...  
#' @keywords items
#' @export

define.mxrv.nonstramcrv<-function(item,rriskModel,items.nonstra.index,items.nonstra.names,stratum.item){
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(item))
  
  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  weights.string<-paste(item@stratum,"$strv",sep="")
  namesfordataframe<-paste(paste(stratum.item@data$stratum$stid,items.nonstra.names,sep="="),collapse=",")
  fullc<-paste("matrix(ncol=1,","data=","rrisksample(data=data.frame(",namesfordataframe,"),weights=",weights.string,"),byrow=TRUE)",sep="")
  item.temp<-item
  item.temp@fullc<-fullc
  definition<-paste("Mixed distribution based on: ",namesfordataframe,sep="")
  
  cat("\n----------------------------------------------------------------------\nEvaluating item values with predefined distribution parameters...\n----------------------------------------------------------------------\n")
  fullcValues<-itemsEvaluation(item.temp,rriskModel,run=TRUE)# hier wird die Ausführbarkeit von fullc geprüft
  cat("\n***************************************\nFull command for",item@name,"\n***************************************\n")
   if (inherits(fullcValues, "try-error")){
    cat("Full command expression",fullc,"could not be evaluated !\n")
   } else { # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
     colnames(fullcValues)<-item@name
     # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
     cat("\nDimension of test evaluation: ");cat(dim(fullcValues));cat("\n")
     cat("\nitem@fullc=",fullc,"\n")
     cat("\nGenerated item values (full command):\n");print(head(data.frame(fullcValues)));cat("...\n")
     cat("\nSummary statistics of item values (full command):\n"); print(summary(fullcValues),digits=4);cat("\n")
     item@fullc<-fullc
     cat("\n***************************************\nSymbolical definition for",item@name,"\n***************************************\n")
     cat("\nitem@definition=",definition,"\n")
     item@definition<-definition
     #--------------------------------------------------------------------------
     # save simulated item value in data-slot
     #--------------------------------------------------------------------------
     item@data<-summary(fullcValues)
   }
   #-----------------------------------------------------------------------------
   # hier wird die Ausführbarkeit von 'relaxc' geprüft
   #----------------------------------------------------------------------------
   plausimin<-item@plausimin
   plausimax<-item@plausimax
   relaxc<-paste("matrix(ncol=1,data=rudiscrete(n=rriskModel@settings@N,min=",plausimin,",max=",plausimax,"),byrow=TRUE)",sep="")
   relaxc.test<-paste("matrix(ncol=1,data=rudiscrete(n=rriskModel@settings@Ntest,min=",plausimin,",max=",plausimax,"),byrow=TRUE)",sep="")
   relaxcValues<-try(eval(parse(text=relaxc.test)),silent=TRUE)
   if (inherits(relaxcValues, "try-error"))
   { cat("Relax command expression",relaxc,"could not be evaluated !\n")
   }else
   { colnames(relaxcValues)<-item@name
     cat("\n***************************************\nRelax command for",item@name,"\n***************************************\n")
     cat("\nDimension of test evaluation: ");cat(dim(relaxcValues));cat("\n")
     cat("\nitem@relaxc=",relaxc,"\n")
     cat("\nGenerated item values (relax command):\n");print(head(data.frame(relaxcValues)));cat("...\n")
     cat("\nSummary statistics of item values (relax command):\n"); print(summary(relaxcValues),digits=4);cat("\n")
     item@relaxc<-relaxc
   } 
} # end of function define.mxrv.nonstramcrv




################################################################################
################################################################################
#' @name menu.define.stra
#' @aliases menu.define.stra
#' @title Non-executable auxiliary function
#' @usage menu.define.stra(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.define.stra<-function(item,rriskModel,menuLevel=1)
{ 
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(invisible(item)))
  
  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  choices<-c("Weights derived from existing items","Weights entered manually")
  
  #-----------------------------------------------------------------------------
  # go into the while-loop
  #-----------------------------------------------------------------------------
  choices<-c(choices, "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Please choose the way to define stratum item",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input==1){
      stratum<-menu.define.stra.rcode(item,rriskModel,menuLevel=menuLevel+1)
      if(length(stratum)>0){
        strv<-sample(1:nrow(stratum),replace=TRUE,prob=stratum$stdi,size=rriskModel@settings@N)
        item@data<-list(stratum=stratum,strv=strv)
        item@fullc<-"sample(1:nrow(stratum),replace=TRUE,prob=stratum$stdi,size=rriskModel@settings@N)"
        item@relaxc<-"sample(1:nrow(stratum),replace=TRUE,prob=stratum$stdi,size=rriskModel@settings@N)"
        item@stratumevaluated<-TRUE
      }
      break()
    } else if (input==2){
      stratum<-menu.define.stra.fix(item,rriskModel,menuLevel=menuLevel+1)
      strv<-sample(1:nrow(stratum),replace=TRUE,prob=stratum$stdi,size=rriskModel@settings@N)
      item@data<-list(stratum=stratum,strv=strv)
      item@fullc<-"sample(1:nrow(stratum),replace=TRUE,prob=stratum$stdi,size=rriskModel@settings@N)"
      item@relaxc<-"sample(1:nrow(stratum),replace=TRUE,prob=stratum$stdi,size=rriskModel@settings@N)"
      item@stratumevaluated<-FALSE
      break()
    } else if (input==length(choices)){
      break()
    }
    input<-99
  } # end while
} # end of function menu.define.stra()


################################################################################
################################################################################
#' @name menu.define.stra.fix
#' @aliases menu.define.stra.fix
#' @title Non-executable auxiliary function
#' @usage menu.define.stra.fix(item,rriskModel,menuLevel=1)
#' @param item  ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.define.stra.fix<-function(item,rriskModel,menuLevel=1)
{ 
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  stratum<-data.frame()
  on.exit(return(invisible(stratum)))

  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  cat("\nPlease enter in the label names and the relative weights of the strata in the first and second column, respectively\n")
  
  #-----------------------------------------------------------------------------
  # öffne input-fenster bis valid data eingetragen wird
  #-----------------------------------------------------------------------------
  validInput<-FALSE
  
  while(!validInput){
    stratum<-data.frame(stid=c("category1","category2","category3"),stdi=c("weight1","weight2","weight3"))
    stratum<-suppressWarnings(edit(stratum,title="Please enter in the label names and the relative weights of the strata in the first and second column, respectively"))
    stratum<-stratum[,c("stid","stdi")]
    # entferne Zeilen, die nur aus fehlenden Werten bestehen
    for(i in 1:nrow(stratum)){
      if(i==1) rowToRemove<-c()
      if(all(is.na(stratum[i,]))) rowToRemove<-c(rowToRemove,i)
      if(i==nrow(stratum) & length(rowToRemove)>0) stratum<-stratum[-rowToRemove,]
    }
    # Überprüfe ob, stid nur aus fehlenden Werten besteht
    if(all(is.na(stratum$stid)) | all(is.na(stratum$stdi))){
      cat("Stratum data frame contains in columns 'stid' or 'stdi' anly missing values. Please, repeat input prozess!\n")
      next()
    }
     # Überprüfe ob, einzelne fehlende Werte vorhanden sind
    if(any(is.na(stratum))){
      cat("Stratum data frame contains missing values. Please, repeat input prozess!\n")
      next()
    }
    stratum$stid<-as.character(stratum$stid)
    stratum$stdi<-as.character(stratum$stdi)
    if(nrow(stratum)<=1){
      cat("Stratum data frame contains only one row. Please, repeat input prozess!\n")
      next()
    }
    #-----------------------------------------------------------------------------
    # check input data
    #-----------------------------------------------------------------------------
     stratum$stdi<-suppressWarnings(as.numeric(stratum$stdi))
     try.result<-try(na.fail(stratum),silent=TRUE)
     if (inherits(try.result, "try-error")){
      cat("Some entries in the column 'stdi' cannot be converted to numerical values\n")
      } else validInput<-TRUE
  } # end while-loop

  #-----------------------------------------------------------------------------
  # prepare output
  #-----------------------------------------------------------------------------
  cat("\nStratum data frame has been evaluated to\n")
  print(stratum)
  output<-list(validInput<-validInput,stratum.evaluated=stratum)
  return(invisible(stratum))
} # end of function menu.define.stra.rcode()


################################################################################
################################################################################
#' @name menu.define.stra.rcode
#' @aliases menu.define.stra.rcode
#' @title Non-executable auxiliary function
#' @usage menu.define.stra.rcode(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.define.stra.rcode<-function(item,rriskModel,menuLevel=1)
{ 
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  stratum<-data.frame()
  on.exit(return(invisible(stratum)))

  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  #-----------------------------------------------------------------------------
  # öffne input-fenster bis valid data eingetragen wird
  #-----------------------------------------------------------------------------
  stratumnames<-myvalue.stratumNames(item,rriskModel,menuLevel=menuLevel)
  if(length(stratumnames)>0){
    cat("Stratum names:\n")
    print(stratumnames)
    stratumweights<-myvalue.stratumNames(item,rriskModel,stratumNames=stratumnames,menuLevel=menuLevel)
    cat("Stratum weights:\n")
    print(stratumweights)
    stratum<-data.frame(stratumnames,stratumweights)
    names(stratum)<-c("stid","stdi")
  
    cat("\nStratum data frame has been evaluated to\n")
    print(stratum)
  } # end  if(length(stratumnames)>0)

  return(invisible(stratum))
} # end of function menu.define.stra.rcode()



################################################################################
################################################################################
#' @name myvalue.stratumNames
#' @aliases myvalue.stratumNames
#' @title Non-executable auxiliary function
#' @usage myvalue.stratumNames(item,rriskModel,menuLevel=1,stratumNames)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @param stratumNames ...
#' @keywords items
#' @export

myvalue.stratumNames<-function(item,rriskModel,menuLevel=1,stratumNames){
  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  stratumInputs<-c()
  dataForWith<-get.dataForWith(item,rriskModel,catEval=FALSE)
  if(missing(stratumNames)){
    ttitle<-"Please enter names of stratum levels (should evaluate to required number of character values)"
  } else {
    ttitle<-"Enter weights of stratum levels (should evaluate to required number of numeric values)"
  }
  
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(stratumInputs))
  
  #-----------------------------------------------------------------------------
  # read string = names of stratum
  #-----------------------------------------------------------------------------
  if(length(dataForWith)>0){
    try.again <- TRUE
    while(try.again){
      cat(levelTabulator,ttitle,levelTabulator,rep("-",35),levelTabulator)
      txt<-readline()
      try.result<-try(value<-with(data=dataForWith,suppressWarnings(eval(parse(text=txt)))),silent=TRUE)
      if(inherits(try.result, "try-error")){
        cat(levelTabulator,"The expression cannot be evaluated because of unknown variable, data oder function!","\n")
        next()
      } else {
        stratumInputs<-value
        if(!is.vector(stratumInputs)){
          cat("Your input were evaluated to the following expression that is not a vector. Please, repeate your input.\n")
          print(stratumInputs)
          next()
        } else {
          if(!missing(stratumNames)){ # wenn es also um stratum weight geht
            if(length(stratumInputs)!=length(stratumNames)){
              cat("Length of evaluated stratum weights vector does not correspond to the length of defined stratum names!\n")
              cat("Stratum names:");print(stratumNames)
              cat("Stratum weights:");print(stratumInputs)
              next()
            } else if (!all(is.numeric(stratumInputs))){
              cat("Entered values could not be converted to numerical values\n")
              print(stratumInputs)
              next()
            }
          }
          try.again<-FALSE
        }
      }
    } # end while
  } else {
    cat("\nThe current model does not contain any data set or evaluated item(s). It is not possible to define weights derived from existing items!\n")
  }# end if(length(dataForWith)>0)
}# end of function myvalue.stratumNames()





################################################################################
################################################################################
#' @name menu.define.bdjp
#' @aliases menu.define.bdjp
#' @title Non-executable auxiliary function
#' @usage menu.define.bdjp(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.define.bdjp<-function(item,rriskModel,menuLevel=1)
{ bdjpItem<-item
  bdjpDataitem<-NULL
  on.exit(return(invisible(list(bdjpItem=bdjpItem,bdjpDataitem=bdjpDataitem))))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  choices<-c("Bayesian prevalence estimation under misclassification",
            "Bayes estimation of a zero inflated Poisson (ZIP) model",
            "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Please choose MCMC estimation method",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { try.result<-try(bayesResults<-PEMGUI(),silent=TRUE)
      #try.result<-try(bayesResults<-lm(),silent=TRUE)# das war nur zum testen
      #-------------------------------------------------------------------------
      # falls mit fehlermeldung abgebrochen rufe stop, falls nicht gehe weiter
      #-------------------------------------------------------------------------
      #if (inherits(try.result, "try-error")) {
      if(!isClass(bayesResults," bayesmodelClass ")){
        cat("\n\n ---> Execution terminated with an error message!!!\n\n")
        stop("Could not fit bayesian prevalence model",call.=FALSE)
      } else {
        if(bayesResults@convergence==TRUE) # falls konvergiert, dann gehe weiter
        { # bei konvergenz erzeugt bdjp item ein data item, wo posteriors abgespeichert werden
          itemdef<-"Bayesian prevalence estimation"
          items<-bdjp.saveResults(bayesResults,item,itemdef,
            rriskModel,menuLevel=menuLevel+1)
          print(items)
          bdjpItem<-items[["bdjpItem"]]
          bdjpDataitem<-items[["bdjpDataitem"]]   
        }
        break()
      }
    } else if(input==2)
    { itemdef<-"Bayesian estimation of ZIP model"
      try.result<-try(items<-menu.bdjp.ZIPGUI(item,itemdef,rriskModel,menuLevel=menuLevel+1),silent=TRUE)
      #-------------------------------------------------------------------------
      # falls mit fehlermeldung abgebrochen rufe stop, falls nicht gehe weiter
      #-------------------------------------------------------------------------
      if (inherits(try.result, "try-error")) {
        stop("Could not fit bayesian ZIP model",call.=FALSE)
      } else {
        print(items)
        bdjpItem<-items[["bdjpItem"]]
        bdjpDataitem<-items[["bdjpDataitem"]]
      break()
      }
    } else if (input==length(choices))
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.define.bdjp()


################################################################################
################################################################################
#' @name menu.bdjp.ZIPGUI
#' @aliases menu.bdjp.ZIPGUI
#' @title Non-executable auxiliary function
#' @usage menu.bdjp.ZIPGUI(item,itemdef,rriskModel,menuLevel=1)
#' @param item ...
#' @param itemdef ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.bdjp.ZIPGUI<-function(item,itemdef,rriskModel,menuLevel=1)
{ bdjpItem<-item
  bdjpDataitem<-NULL
  on.exit(return(invisible(list(bdjpItem=bdjpItem,bdjpDataitem=bdjpDataitem))))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  # get all data sets assosiated with the current model
  availableData.names<-c()
  availableData.data<-list()
  items<-rriskModel@items@items
  if(length(items)>0)
  { for(i in 1:length(items))
    { # Betrachte data-Items, bei denen data-Slot nicht leer ist
      if(items[[i]]@typecode=="data" & !is.null(items[[i]]@data))
      { item.name<-items[[i]]@name
        var.names<-names(items[[i]]@data)
        availableData.temp<-paste(item.name,var.names,sep="$")
        availableData.names<-c(availableData.names,availableData.temp)
        availableData.data<-c(availableData.data,as.list(items[[i]]@data))
      }
    }
  } 
  choices<-c(availableData.names, "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Please choose data set for bayesian estimation",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input<=(length(choices)-1))
    { 
      ZIPGUI.data<-availableData.data[[as.numeric(input)]]
      View(ZIPGUI.data)
      ZIPGUI.data<-as.vector(unlist(ZIPGUI.data)) # wandele in ein vektor um
      try.result<-try(bayesResults<-ZIPGUI(data=ZIPGUI.data),silent=TRUE)
      #if (inherits(try.result, "try-error")) {
      if(!isClass(bayesResults," bayesmodelClass ")){
        cat("\n\n ---> Execution terminated with an error message!!!\n\n")
        stop("Could not fit bayesian ZIP models to the selected data set",call.=FALSE)
      } else {
        if(bayesResults@convergence==TRUE) # falls konvergiert, dann gehe weiter
        { itemdef<-paste(itemdef,"; fitted to ",availableData.names[as.numeric(input)],sep="")
          items<-bdjp.saveResults(bayesResults,item,itemdef,rriskModel,menuLevel=menuLevel+1)   
          bdjpItem<-items[["bdjpItem"]]
          bdjpDataitem<-items[["bdjpDataitem"]]
          break()
        }
      }
    } else if (input==length(choices))
    { break()
    } 
    input<-99
  } # end while
} #  end of function menu.bdjp.ZIPGUI()



################################################################################
################################################################################
#' @name bdjp.saveResults
#' @aliases bdjp.saveResults
#' @title Non-executable auxiliary function
#' @usage bdjp.saveResults(bayesResults,item,itemdef="",rriskModel,menuLevel=1)
#' @param bayesResults ...
#' @param item ...
#' @param itemdef ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

bdjp.saveResults<-function(bayesResults,item,itemdef="",rriskModel,menuLevel=1)
{ 
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  #-----------------------------------------------------------------------------
  # fill slot of created bdjp item
  #-----------------------------------------------------------------------------
  item@definition<-itemdef
  item@data<-bayesResults@results
  
  #-----------------------------------------------------------------------------
  # create new data item and save bdjp posteriors
  #-----------------------------------------------------------------------------
  bdjp.dataitem<-new("itemClass")
  bdjp.dataitem@name<-paste(item@name,".data",sep="")
  bdjp.dataitem@title<-paste("Posteriors of the bdjp-item '",item@name,"'",sep="")
  bdjp.dataitem@type<-"Data item (data)"
  bdjp.dataitem@typecode<-"data"
  bdjp.dataitem@part<-item@part
  bdjp.dataitem@data<-bayesResults@jointpost
  bdjp.dataitem@role<-"Not defined (nd)"
  bdjp.dataitem@rolecode<-"nd"
  bdjp.dataitem@scores<-item@scores
  bdjp.dataitem@depitem<-""
  item@depitem<-bdjp.dataitem@name

  cat(paste("Simulation results has been successfully saved to the bdjp data item '",bdjp.dataitem@name,"'",sep=""),"\n")
  
  #-----------------------------------------------------------------------------
  # create output
  #-----------------------------------------------------------------------------
  output<-list(bdjpItem=item,bdjpDataitem=bdjp.dataitem)
  return(output)
} # end of function bdjp.saveResults()




################################################################################
################################################################################
#' @name menu.define.rsrv
#' @aliases menu.define.rsrv
#' @title Non-executable auxiliary function
#' @usage menu.define.rsrv(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.define.rsrv<-function(item,rriskModel,menuLevel=1)
{
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(invisible(item)))

  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  # get all data sets assosiated with the current model
  availableData<-c()
  availableDataRCODE<-c()
  availableMcrvFnrv.names<-c()
  availableMcrvFnrv.data<-c()
  items<-rriskModel@items@items
  if(length(items)>0){
    for(i in 1:length(items)){
      if(items[[i]]@typecode=="data"){
        #-----------------------------------------------------------------------
        # get all data items
        #-----------------------------------------------------------------------
        item.name<-items[[i]]@name
        var.names<-names(items[[i]]@data)
        availableData.temp<-paste(item.name,var.names,sep="$")
        availableDataRCODE.temp<-paste("rriskModel@items@items[[",i,"]]@data$",var.names,sep="")
        availableData<-c(availableData,availableData.temp)
        availableDataRCODE<-c(availableDataRCODE,availableDataRCODE.temp)
        names(availableDataRCODE)<-availableData.temp
      } else if (is.element(items[[i]]@typecode,c("mcrv","fnrv"))){
        #-----------------------------------------------------------------------
        # evaluate/get all mcrv and fnrv items
        #-----------------------------------------------------------------------
        if(!is.null(items[[i]]@data) | items[[i]]@fullc!=""){
          availableMcrvFnrv.names<-c(availableMcrvFnrv.names,items[[i]]@name)
        }
      }
    } # end for
    #---------------------------------------------------------------------------
    # get evaluated values of all mcrv and fnrv items
    #---------------------------------------------------------------------------
   # item<-new(itemClass)
    dataForWith<-get.dataForWith(item,rriskModel,run=FALSE,catEval=FALSE)
    if (inherits(dataForWith, "try-error")){
      cat("Full command expression",item@fullc,"could not be evaluated !\n")
    } else { # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
      availableMcrvFnrv.data<-dataForWith[availableMcrvFnrv.names]
    } # end if (inherits(dataForWith, "try-error")){
  } # end  if(length(items)>0){

  if(length(c(availableData, names(availableMcrvFnrv.data)))==0){
    stop("The model does not contain any data, mcrv or fnrv item!",call.=FALSE)
  } # end if(length(availableData, names(availableMcrvFnrv.data))==0){

  choices<-c(availableData, names(availableMcrvFnrv.data),"Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices)))){
    input<-mymenu(title="Please choose data set for resampling",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    input.old<-as.numeric(input)
    input<-choices[as.numeric(input)]
    if(input %in% availableData){
      data2fitname<-input
      data2fit<-eval(parse(text=availableDataRCODE[input.old]))
      #View(data2fit)
      #print(head(data2fit))
      cat(data2fitname,": ",head(data2fit),"...\n")
      #item<-rsrv.resample(data2fit,data2fitname,item,rriskModel)
      item<-rsrv.resample(data2fitname,item,rriskModel)
      break()
    } else if( input %in% availableMcrvFnrv.names) {
      data2fit<-availableMcrvFnrv.data[[input]]
      data2fitname<-input
      cat(data2fitname,": ",head(data2fit),"...\n")
      item<-rsrv.resample(data2fitname,item,rriskModel)
      break()
    } else if (input.old==length(choices)){
      break()
    }
   # input<-99
  } # end while
} # end of function menu.define.rsrv()




################################################################################
################################################################################
#' @name rsrv.resample
#' @aliases rsrv.resample
#' @title Non-executable auxiliary function
#' @usage rsrv.resample(data2fitname,item,rriskModel)
#' @param data2fitname ...
#' @param item ...
#' @param rriskModel ...
#' @keywords items
#' @export

rsrv.resample<-function(data2fitname,item,rriskModel)
{ on.exit(return(item))
  fullc<-paste("sample(x=",data2fitname,",size=rriskModel@settings@N,replace=TRUE)",sep="")
  definition<-paste("resample(",data2fitname,")",sep="")
  
  #-----------------------------------------------------------------------------
   # hier wird die Ausführbarkeit von 'fullc' geprüft
   #-----------------------------------------------------------------------------
   item.temp<-item
   item.temp@fullc<-fullc
   fullcValues<-itemsEvaluation(item.temp,rriskModel)# hier wird die Ausführbarkeit von fullc geprüft
   cat("\n***************************************\nFull command for",item@name,"\n***************************************\n")
   if (inherits(fullcValues, "try-error"))
   { cat("Full command expression",fullc,"could not be evaluated !\n")
   }else 
   { # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
     item@fullc<-fullc
     fullcValues<-as.data.frame(fullcValues)
     names(fullcValues)<-item@name
     # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
     cat("\nDimension of test evaluation: ",dim(fullcValues),"\n")
     cat("\nitem@fullc=",fullc,"\n")
     cat("\nGenerated item values (full command):\n");print(head(fullcValues));cat("...\n")
     cat("\nSummary statistics of item values (full command):\n"); print(summary(fullcValues),digits=4);cat("\n")
     item@fullc<-fullc
     item@data<-summary(fullcValues)
     cat("\n***************************************\nSymbolical definition for",item@name,"\n***************************************\n")
     cat("\nitem@definition=",definition,"\n")
     item@definition<-definition
     cat("\n***************************************\nRelax command for",item@name,"\n***************************************\n")
     cat("\nitem@relaxc=",fullc,"\n")
     item@relaxc<-fullc
   }  
   #----------------------------------------------------------------------------
   # definiere Output
   #----------------------------------------------------------------------------
   return(item)
} # end of function  rsrv.resample()



################################################################################
################################################################################
#' @name menu.define.bsrv
#' @aliases menu.define.bsrv
#' @title Non-executable auxiliary function
#' @usage menu.define.bsrv(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.define.bsrv<-function(item,rriskModel,menuLevel=1)
{ on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  choices<-c("Parametric (sampling from fitted Normal distribution)",
             "Nonparametric (sampling from data)",
             "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose bootstrap method",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input==1) # for parametric method
    { item<-menu.bsrv(item,rriskModel,method="param",menuLevel=menuLevel+1)  # method=c("param","nonparam")
      break()
    } else if(input==2) # for nonparametric method                                                                                 
    { item<-menu.bsrv(item,rriskModel,method="nonparam",menuLevel=menuLevel+1)
      break()
    } else if (input==3)
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.define.bsrv()


################################################################################
################################################################################
#' @name menu.bsrv
#' @aliases menu.bsrv
#' @title Non-executable auxiliary function
#' @usage menu.bsrv(item,rriskModel,method="param",menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param method character string defining bootstrapping method, \code{method=c("param","nonparam")}
#' @param menuLevel ...
#' @keywords items
#' @export

menu.bsrv<-function(item,rriskModel,method="param",menuLevel=1)
{ on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  # get all data sets assosiated with the current model
  availableData<-c()
  items<-rriskModel@items@items
  if(length(items)>0)
  { for(i in 1:length(items))
    { if(items[[i]]@typecode=="data" & !is.null(items[[i]]@data))
      { item.name<-items[[i]]@name
        var.names<-names(items[[i]]@data)
        availableData.temp<-paste(item.name,var.names,sep="$")
        availableData<-c(availableData,availableData.temp)
      }
    }
  } 
  choices<-c(availableData, "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Please choose data for bootstraping",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input<=(length(choices)-1))
    { data2fitname<-availableData[as.numeric(input)]
      item<-bsrv.fittodata(data2fitname,item,rriskModel,method)
      break()
    } else if (input==length(choices))
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.bsrv.nonparam()


################################################################################
################################################################################
#' @name bsrv.fittodata
#' @aliases bsrv.fittodata
#' @title Non-executable auxiliary function
#' @usage bsrv.fittodata(data2fitname,item,rriskModel,method)
#' @param data2fitname ...
#' @param item ...
#' @param rriskModel ...
#' @param method character string giving the method for bootstrapping, \code{method=c("param","nonparam")}
#' @keywords items
#' @export

bsrv.fittodata<-function(data2fitname,item,rriskModel,method)
{ #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(item))
  
  #-----------------------------------------------------------------------------
  # define some help varibles
  #-----------------------------------------------------------------------------
  if(method=="param"){
    definition<-paste("mean, sd (param. bootstrap); fitted to ",data2fitname,sep="")
    fullc<-paste("bootdist(fitdist(",data2fitname,",'norm',method='mle'),bootmethod='param',niter=rriskModel@settings@N)$estim",sep="")
  } else if(method=="nonparam"){
    definition<-paste("mean, sd (nonparam. bootstrap); fitted to ",data2fitname,sep="")
    fullc<-paste("bootdist(fitdist(",data2fitname,",'norm',method='mle'),bootmethod='nonparam',niter=rriskModel@settings@N)$estim",sep="")
  }
    
  #-----------------------------------------------------------------------------
   # hier wird die Ausführbarkeit von 'fullc' geprüft
   #-----------------------------------------------------------------------------
   item.temp<-item
   item.temp@fullc<-fullc
   fullcValues<-itemsEvaluation(item.temp,rriskModel)# hier wird die Ausführbarkeit von fullc geprüft
   cat("\n***************************************\nFull command for",item@name,"\n***************************************\n")
   if (inherits(fullcValues, "try-error"))
   { cat("Full command expression",fullc,"could not be evaluated !\n")
   }else 
   { # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
     item@fullc<-fullc
     # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
     cat("\nDimension of test evaluation: ",dim(fullcValues),"\n")
     cat("\nitem@fullc=",fullc,"\n")
     cat("\nGenerated item values (full command):\n");print(head(data.frame(fullcValues)));cat("...\n")
     cat("\nSummary statistics of item values (full command):\n"); print(summary(fullcValues),digits=4);cat("\n")
     item@fullc<-fullc
     item@data<-summary(fullcValues)
     cat("\n***************************************\nSymbolical definition for",item@name,"\n***************************************\n")
     cat("\nitem@definition=",definition,"\n")
     item@definition<-definition
     cat("\n***************************************\nRelax command for",item@name,"\n***************************************\n")
     cat("\nitem@relaxc=",fullc,"\n")
     item@relaxc<-fullc
   }  
   #----------------------------------------------------------------------------
   # definiere Output
   #----------------------------------------------------------------------------
   return(item)
} # end of function bsrv.fittodata()


################################################################################
################################################################################
#' @description This function generates the user menu for maintaining scoring systems
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.define.mcrv
#' @aliases menu.define.mcrv
#' @title Non-executable auxiliary function
#' @usage menu.define.mcrv(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel an instance of the \code{modelClass}
#' @param menuLevel numeric value indicating menu level
#' @keywords items
#' @export

menu.define.mcrv<-function(item,rriskModel,menuLevel=1)
{ on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  choices<-c("discrete",
             "continuous",
             "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose type of probability density",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { item<-menu.mcrv.disc(item,rriskModel,menuLevel=menuLevel+1)
      break()
    } else if(input==2)                                                                                   
    { item<-menu.mcrv.cont(item,rriskModel,menuLevel=menuLevel+1)
      break()
    } else if (input==3)
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.define.mcrv() 



################################################################################
################################################################################
#' @name menu.mcrv.disc
#' @aliases menu.mcrv.disc
#' @title Non-executable auxiliary function
#' @usage menu.mcrv.disc(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.mcrv.disc<-function(item,rriskModel,menuLevel=1)
{ on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
    
  choices<-c("Fit discrete distribution to data",
             "Fit discrete distribution to given percentiles",
             "Bernoulli (bern)",
             "binomial (binom)",
             "discrete (discrete)",
             "geometric (geom)",
             "hypergeometric (hyper)",
             "multinomial (multinom)",
             "negative binomial (nbinom)",
             "Poisson (pois)",
             "uniform discrete (udiscrete)",
             "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose family of discrete probability density function",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input==1)# Fit discrete distribution to data 
    { if(item@stratum!=""){
        cat("This option is not allowed for stratified item\n")
      } else {
        cat("Sorry, this option is still under evaluation\n")
        #break()
      }
    } else if(input==2)# Fit discrete distribution to percentiles
    { if(item@stratum!=""){
        cat("This option is not allowed for stratified item\n")
      } else {
        cat("Sorry, this option is still under evaluation\n")
        #break()
      }
    } else if(input==3) # Bernoulli (bern)
    { item<-menu.mcrv.dfamily(item,rriskModel,dfamily="bern",menuLevel=menuLevel)
      break()
    } else if(input==4) # binomial (binom)
    { item<-menu.mcrv.dfamily(item,rriskModel,dfamily="binom",menuLevel=menuLevel)
      break()
    } else if(input==5) # discrete (discrete)
    { if(item@stratum!=""){
        cat("This option is not allowed for stratified item\n")
      } else {
        item<-menu.mcrv.dfamily(item,rriskModel,dfamily="discrete",menuLevel=menuLevel)
        break()
      }
    } else if(input==6) # geometric (geom)
    { item<-menu.mcrv.dfamily(item,rriskModel,dfamily="geom",menuLevel=menuLevel)
      break()
    } else if(input==7) # hypergeometric (hyper)
    { item<-menu.mcrv.dfamily(item,rriskModel,dfamily="hyper",menuLevel=menuLevel)
      break()
    } else if(input==8) # multinomial (multinom)
    { cat("\nSorry, this option is still under evaluation...\n")
      #if(item@stratum!=""){
      #  cat("This option is not allowed for stratified item\n")
      #} else {
      #  item<-menu.mcrv.dfamily(item,rriskModel,dfamily="multinom",menuLevel=menuLevel)
      #break()
      #}
    } else if(input==9) # negative binomial (nbinom)
    { item<-menu.mcrv.dfamily(item,rriskModel,dfamily="nbinom",menuLevel=menuLevel)
      break()
    } else if(input==10) # Poisson (pois)
    { item<-menu.mcrv.dfamily(item,rriskModel,dfamily="pois",menuLevel=menuLevel)
      break()
    } else if(input==11) # uniform discrete (udiscrete)
    { item<-menu.mcrv.dfamily(item,rriskModel,dfamily="udiscrete",menuLevel=menuLevel)
      break()
    } else if (input==12) # Exit
    { break()
    } 
    input<-99
  } # end while
} # end of fucntion menu.mcrv.disc()


################################################################################
################################################################################
#' @name menu.mcrv.dfamily
#' @aliases menu.mcrv.dfamily
#' @title Non-executable auxiliary function
#' @usage menu.mcrv.dfamily(item,rriskModel,dfamily="pois",menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param dfamily ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.mcrv.dfamily<-function(item,rriskModel,dfamily="pois",menuLevel=1)
{ on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  batch<-1
  
  #-----------------------------------------------------------------------------
  # ist ein geschichteter Item?
  #-----------------------------------------------------------------------------
  if(item@stratum!=""){
    if(length(rriskModel@items@items)>0){
      for(i in 1:length(rriskModel@items@items)){
        if(rriskModel@items@items[[i]]@typecode=="stra"){
          stratum.item<-rriskModel@items@items[[i]]
          batch<-nrow(stratum.item@data$stratum)
          stid<-as.character(stratum.item@data$stratum$stid)                                  
        } # end if
      } # end for
    } # end if
  } # end if
  
  #-----------------------------------------------------------------------------
  # now enter distribution parameters
  #-----------------------------------------------------------------------------
  if(dfamily=="bern")
  { prob.mess<-paste("Enter ",batch," value(s) for Bernoulli distribution parameter prob (0 < prob < 1)",sep="")
    prob<-myvalue.distrparams(item,rriskModel,title=prob.mess,menuLevel=menuLevel+1,minmax=c(0,1),batch=batch)$output
    definition<-paste("binom(size=1",",prob=",prob,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rbinom(n=",batch,"*rriskModel@settings@N,size=1,","prob=",prob,"),byrow=TRUE)",sep="")
  } else if(dfamily=="binom")
  { size.mess<-paste("Enter ",batch," value(s) for binomial distribution parameter size (integer value)",sep="")
    size<-myvalue.distrparams(item,rriskModel,title=size.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),asInteger=TRUE,batch=batch)$output
    prob.mess<-paste("Enter ",batch," value(s) for binomial distribution parameter prob (0 < prob < 1)",sep="")
    prob<-myvalue.distrparams(item,rriskModel,title=prob.mess,menuLevel=menuLevel+1,minmax=c(0,1),batch=batch)$output
    definition<-paste("binom(size=",size,",prob=",prob,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rbinom(n=",batch,"*rriskModel@settings@N,size=",size,",prob=",prob,"),byrow=TRUE)",sep="")
  } else if(dfamily=="discrete")
  { #-----------------------------------------------------------------------------
    # öffne input-fenster bis valid data eingetragen wird
    #-----------------------------------------------------------------------------
      validInput<-FALSE
      while(!validInput){
        valuesprobs<-data.frame(values=c("value1","value2","value3"),probs=c("prob1","prob2","prob3"))
        valuesprobs<-suppressWarnings(edit(valuesprobs))
        valuesprobs$values<-as.numeric(as.character(valuesprobs$values))
        valuesprobs$probs<-as.numeric(as.character(valuesprobs$probs))
        #-----------------------------------------------------------------------------
        # check input data
        #-----------------------------------------------------------------------------
        try.result<-try(na.fail(valuesprobs),silent=TRUE)
        if (inherits(try.result, "try-error")){
          cat("Invalid input data")
        } else validInput<-TRUE
      } # end while-loop
      values.def<-paste(valuesprobs$values,collapse=",")
      probs.def<-paste(valuesprobs$probs,collapse=",")
      definition<-paste("discrete(values=c(",values.def,"),probs=c(",probs.def,"))",sep="")
      fullc<-paste("matrix(ncol=",batch,",data=","rdiscrete(n=",batch,"*rriskModel@settings@N,values=c(",values.def,"),probs=c(",probs.def,")),byrow=TRUE)",sep="")
  } else if(dfamily=="geom")
  { prob.mess<-paste("Enter ",batch," value(s) for geometrical distribution parameter prob (0 < prob < 1)",sep="")
    prob<-myvalue.distrparams(item,rriskModel,title=prob.mess,menuLevel=menuLevel+1,minmax=c(0,1),batch=batch)$output
    definition<-paste("geom(prob=",prob,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rgeom(n=",batch,"*rriskModel@settings@N,prob=",prob,"),byrow=TRUE)",sep="")
  } else if(dfamily=="hyper")
  { m.mess<-paste("Enter ",batch," value(s) for hypergeometric distribution parameter m (integer value)",sep="")
    m<-myvalue.distrparams(item,rriskModel,title=m.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),asInteger=TRUE,batch=batch)$output
    n.mess<-paste("Enter ",batch," value(s) for hypergeometric distribution parameter n (integer value)",sep="")
    n<-myvalue.distrparams(item,rriskModel,title=n.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),asInteger=TRUE,batch=batch)$output
    k.mess<-paste("Enter ",batch," value(s) for hypergeometric distribution parameter k (integer value)",sep="")
    k<-myvalue.distrparams(item,rriskModel,title=k.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),asInteger=TRUE,batch=batch)$output
    definition<-paste("hyper(m=",m,",n=",n,",k=",k,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rhyper(nn=",batch,"*rriskModel@settings@N,m=",m,",n=",n,",k=",k,"),byrow=TRUE)",sep="")
  } else if(dfamily=="multinom")
  { #m<-myvalue.distrparams(item,rriskModel,title="Enter value for hypergeometric distribution parameter m (integer value)",menuLevel=menuLevel+1,minmax=c(0,Inf),asInteger=TRUE)$output
    #prob<-myvalue.distrparams(item,rriskModel,title="Enter value for binomial distribution parameter prob (0 < prob < 1)",menuLevel=menuLevel+1,minmax=c(0,1))$output
    #definition<-paste("multinom(m=",m,",n=",n,",k=",k,")",sep="")
    #fullc<-paste("matrix(ncol=",batch,",data=","rhyper(nn=",batch,"*rriskModel@settings@N,m=",m,",n=",n,",k=",k,"),byrow=TRUE)",sep="")
  } else if(dfamily=="nbinom")
  { size.mess<-paste("Enter ",batch," value(s) for negative binomial distribution parameter size (integer value)",sep="")
    size<-myvalue.distrparams(item,rriskModel,title=size.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),asInteger=TRUE,batch=batch)$output
    prob.mess<-paste("Enter ",batch," value(s) for negative binomial distribution parameter prob (0 < prob < 1)",sep="")
    prob<-myvalue.distrparams(item,rriskModel,title=prob.mess,menuLevel=menuLevel+1,minmax=c(0,1),batch=batch)$output
    definition<-paste("nbinom(size=",size,",prob=",prob,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rnbinom(n=",batch,"*rriskModel@settings@N,size=",size,",prob=",prob,"),byrow=TRUE)",sep="")
  } else if(dfamily=="pois")
  { lambda.mess<-paste("Enter ",batch," value(s) for Poisson distribution parameter lambda (positive)",sep="")
    lambda<-myvalue.distrparams(item,rriskModel,title=lambda.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("pois(lambda=",lambda,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rpois(n=",batch,"*rriskModel@settings@N,lambda=",lambda,"),byrow=TRUE)",sep="")
  } else if(dfamily=="udiscrete")
  { min.mess<-paste("Enter ",batch," value(s) for uniform discrete distribution parameter min (integer)",sep="")
    minu<-myvalue.distrparams(item,rriskModel,title=min.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),asInteger=TRUE,batch=batch)$output
    max.mess<-paste("Enter ",batch," value(s) for uniform discrete distribution parameter max (integer)",sep="")
    maxu<-myvalue.distrparams(item,rriskModel,title=max.mess,menuLevel=menuLevel+1,minmax=c(minu,Inf),asInteger=TRUE,batch=batch)$output
    definition<-paste("udiscrete(min=",minu,",max=",maxu,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rudiscrete(n=",batch,"*rriskModel@settings@N,min=",minu,",max=",maxu,"),byrow=TRUE)",sep="")
  } 
  #-----------------------------------------------------------------------------
   # hier wird die Ausführbarkeit von 'fullc' geprüft
   #-----------------------------------------------------------------------------
   item.temp<-item
   item.temp@fullc<-fullc
   
 cat("\n----------------------------------------------------------------------\nEvaluating item values with predefined distribution parameters...\n----------------------------------------------------------------------\n")
   fullcValues<-itemsEvaluation(item.temp,rriskModel)# hier wird die Ausführbarkeit von fullc geprüft
   cat("\n***************************************\nFull command for",item@name,"\n***************************************\n")
   if (inherits(fullcValues, "try-error"))
   { cat("Full command expression",fullc,"could not be evaluated !\n")
   }else 
   { # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
     if(batch>1){
      colnames(fullcValues)<-stid
     } else { colnames(fullcValues)<-item@name}     
     item@fullc<-fullc
     # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
     cat("\nDimension of test evaluation: ");cat(dim(fullcValues));cat("\n")
     cat("\nitem@fullc=",fullc,"\n")
     cat("\nGenerated item values (full command):\n");print(head(data.frame(fullcValues)));cat("...\n")
     cat("\nSummary statistics of item values (full command):\n"); print(summary(fullcValues),digits=4);cat("\n")
     item@fullc<-fullc
     cat("\n***************************************\nSymbolical definition for",item@name,"\n***************************************\n")
     cat("\nitem@definition=",definition,"\n")
     item@definition<-definition
     #--------------------------------------------------------------------------
     # save simulated item value in data-slot
     #--------------------------------------------------------------------------
     item@data<-summary(fullcValues)
   }
   #-----------------------------------------------------------------------------
   # hier wird die Ausführbarkeit von 'relaxc' geprüft
   #----------------------------------------------------------------------------
   plausimin<-item@plausimin
   plausimax<-item@plausimax
   relaxc<-paste("matrix(ncol=",batch,",data=rudiscrete(n=",batch,"*rriskModel@settings@N,min=",plausimin,",max=",plausimax,"),byrow=TRUE)",sep="")
   relaxc.test<-paste("matrix(ncol=",batch,",data=rudiscrete(n=",batch,"*rriskModel@settings@Ntest,min=",plausimin,",max=",plausimax,"),byrow=TRUE)",sep="")
   relaxcValues<-try(eval(parse(text=relaxc.test)),silent=TRUE)
   if (inherits(relaxcValues, "try-error"))
   { cat("Relax command expression",relaxc,"could not be evaluated !\n")
   }else 
   { if(batch>1){
      colnames(relaxcValues)<-stid
     } else { colnames(relaxcValues)<-item@name}    
     cat("\n***************************************\nRelax command for",item@name,"\n***************************************\n")
     cat("\nDimension of test evaluation: ");cat(dim(relaxcValues));cat("\n")
     cat("\nitem@relaxc=",relaxc,"\n")
     cat("\nGenerated item values (relax command):\n");print(head(data.frame(relaxcValues)));cat("...\n")
     cat("\nSummary statistics of item values (relax command):\n"); print(summary(relaxcValues),digits=4);cat("\n")
     item@relaxc<-relaxc
   }
   #----------------------------------------------------------------------------
   # definiere Output
   #----------------------------------------------------------------------------
   return(item)
} # end of function  menu.mcrv.dfamily()



################################################################################
################################################################################
#' @name itemsEvaluation
#' @aliases itemsEvaluation
#' @title Non-executable auxiliary function
#' @usage itemsEvaluation(item,rriskModel,run=FALSE,catEval=FALSE)
#' @param item ...
#' @param rriskModel ...
#' @param run a single logical value
#' @param catEval a single logical value
#' @keywords items
#' @export

itemsEvaluation<-function(item,rriskModel,run=FALSE,catEval=FALSE){
  #-----------------------------------------------------------------------------
  # calculate data for item evaluation
  #-----------------------------------------------------------------------------
  dataForWith<-get.dataForWith(item,rriskModel,run=run,catEval=catEval)
  
  #-----------------------------------------------------------------------------
  # set number of test evaluation to rriskModel@settings@Ntest
  #-----------------------------------------------------------------------------
  if(run==FALSE){
    item@fullc<-gsub(x=item@fullc,pattern="rriskModel@settings@N",replacement="rriskModel@settings@Ntest")
  }
  
  #---------------------------------------------------------------------------
  # versuche fullc-expression zu evaluieren
  #---------------------------------------------------------------------------
  if(length(dataForWith)>0){
    try.result<-try(with(data=dataForWith,suppressWarnings(eval(parse(text=item@fullc)))),silent=TRUE)
  } else{
    try.result<-try(suppressWarnings(eval(parse(text=item@fullc))),silent=TRUE)
  }
  return(try.result)
} # end of function itemsEvaluation()



################################################################################
################################################################################
#' @name myvalue.distrparams
#' @aliases myvalue.distrparams
#' @title Non-executable auxiliary function
#' @usage myvalue.distrparams(item,rriskModel,
#'        title="Tryme",menuLevel=1,
#'        minmax,asInteger=FALSE,batch=1)
#' @param item ...
#' @param rriskModel ...
#' @param title ...
#' @param menuLevel ...
#' @param minmax ...
#' @param asInteger boolean value giving whether entered value should be an integer, \code{asInteger=c(TRUE,FALSE)}
#' @param batch ...
#' @keywords items
#' @export

myvalue.distrparams<-function(item,rriskModel,title="Tryme",menuLevel=1,minmax,asInteger=FALSE,batch=1)
{ #-------------------------------------------------------------------------------
  # define help variables
  #-------------------------------------------------------------------------------
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  length.error <- paste(levelTabulator,"Entry does not evaluate to required length",batch,levelTabulator)
  output<-NA
  values<-NA

  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(list(output=output,values=values)))

  #-------------------------------------------------------------------------------
  try.again<-TRUE
  while (try.again){
    cat(levelTabulator,title,levelTabulator,rep("-",35),levelTabulator)
    txt <- readline()
    #-----------------------------------------------------------------------------
    # get data to evaluate current item
    #-----------------------------------------------------------------------------
    dataForWith<-get.dataForWith(item,rriskModel,run=FALSE,catEval=FALSE)

    #---------------------------------------------------------------------------
    # versuche txt-expression zu evaluieren
    #---------------------------------------------------------------------------
    if(length(dataForWith)>0){
      try.result<-try(values<-with(data=dataForWith,suppressWarnings(eval(parse(text=txt)))),silent=TRUE)
      #print(values)
    } else{
      try.result<-try(values<-suppressWarnings(eval(parse(text=txt))),silent=TRUE)
      #print(values)
    }
    if(inherits(try.result, "try-error")){
      cat(levelTabulator,"The expression cannot be evaluated because of unknown variable, data oder function!","\n")
      next()
    } else{
      #-----------------------------------------------------------------------
      # values muss im vordefinierten range liegen
      #-----------------------------------------------------------------------
      if(!missing(minmax)){
        if(all(!is.na(minmax)) & length(values)==1){
          if(values < minmax[1] | values > minmax[2]){
            cat(levelTabulator,"Your input is out of range: ",paste( "c(",minmax[1],",",minmax[2],")",sep=""),"\n")
            next()
          } # end if(values < minmax[1] | values > minmax[2])
        } # end if(all(!is.na(minmax)) & length(values)==1)
      } # end  if(!missing(minmax))
      #-------------------------------------------------------------------------
      # muss values eine ganze zahl sein?
      #-------------------------------------------------------------------------
      if(asInteger==TRUE){
        if(abs(values-round(values))!=0){
          cat(levelTabulator,"Your input is not integer","\n")
          next()
        }
      }
      #-----------------------------------------------------------------------
      # und die Länge von values muss auch stimmen
      #-----------------------------------------------------------------------
      if(length(values)!=batch){
        cat(levelTabulator,"Entry does not evaluate to required length of ",batch,"\n")
        next()
      }

      #-------------------------------------------------------------------------
      # Lasse die (gerundete) Ergebnisse anzeigen
      #-------------------------------------------------------------------------
      if(item@stratum==""){
        cat("\nEntry evaluates to:",paste(try(round(values,digits=2)),collapse=", "),"\n")
      } else {
        cat("\nEntry evaluates to:",paste(try(round(values,digits=2)),collapse=", "),"\n")
      }
      output<-txt
      try.again<-FALSE
    } # end if(inherits(try.result, "try-error"))
  } # end while
  return(list(output=output,values=values))
} # end function myvalue.distrparams()



################################################################################
################################################################################
#' @name menu.mcrv.cont
#' @aliases menu.mcrv.cont
#' @title Non-executable auxiliary function
#' @usage menu.mcrv.cont(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.mcrv.cont<-function(item,rriskModel,menuLevel=1)
{ on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  choices<-c("Fit continuous distribution to data",
             "Fit continuous distribution to given percentiles",
             "beta (beta)",
             "beta Pert (pert)",
             "Cauchy (cauchy)",
             "chi-square (chisq)",
             "chi-square, non-central (chisqnc)",
             "exponential (exp)",
             "F (f)",
             "gamma (gamma)",
             "logistic (logis)",
             "lognormal (lnorm)",
             "Normal (norm)",
             "Truncated Normal (tnorm)",
             "t (t)",
             "triangular (triang)",
             "uniform (unif)",
             "Weibull (weibull)",
             "Gompertz (gompertz)", 
             "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Choose family of continuous probability density function",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input==1)# Fit continuous distribution to data or percentiles
    { if(item@stratum!=""){
        cat("This option is not allowed for stratified item\n")
      } else {
        item<-menu.mcrv.fittodata(item,rriskModel,menuLevel=menuLevel+1)
        break()
      }
    } else if(input==2)
    { if(item@stratum!=""){
      cat("This option is not allowed for stratified item\n")
      } else {
        item<-menu.mcrv.fittoperc(item,rriskModel,menuLevel=menuLevel+1)
        break()
      }
    } else if(input==3) # beta (beta)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="beta",menuLevel=menuLevel)
      break()
    } else if(input==4) # beta Pert (pert)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="pert",menuLevel=menuLevel)
      break()
    } else if(input==5) # Cauchy (cauchy)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="cauchy",menuLevel=menuLevel)
      break()
    } else if(input==6) # chi-square (chisq)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="chisq",menuLevel=menuLevel)
      break()
    } else if(input==7) # chi-square, non-central (chisqnc)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="chisqnc",menuLevel=menuLevel)
      break()
    } else if(input==8) # exponential (exp)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="exp",menuLevel=menuLevel)
      break()
    } else if(input==9) # F (f)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="f",menuLevel=menuLevel)
      break()
    } else if(input==10) # gamma (gamma)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="gamma",menuLevel=menuLevel)
      break()
    } else if(input==11) # logistic (logistic)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="logis",menuLevel=menuLevel)
      break()
    } else if(input==12) # lognormal (lorm)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="lnorm",menuLevel=menuLevel)
      break()
    } else if(input==13) #  Normal (norm)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="norm",menuLevel=menuLevel)
      break()
    } else if(input==14) # Truncated Normal (tnorm)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="tnorm",menuLevel=menuLevel)
      break()
    } else if(input==15) #  t (t)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="t",menuLevel=menuLevel)
      break()
    } else if(input==16) # triangular (triang)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="triang",menuLevel=menuLevel)
      break()
    } else if(input==17) # uniform (unif)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="unif",menuLevel=menuLevel)
      break()
    } else if(input==18) # Weibull (weibull)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="weibull",menuLevel=menuLevel)
      break()
    } else if(input==19) # Gompertz (gompertz)
    { item<-menu.mcrv.cfamily(item,rriskModel,dfamily="gompertz",menuLevel=menuLevel)
      break()
    } else if (input==20) # Exit
    { break()
    } 
    input<-99
  } # end while
} # end of function menu.mcrv.cont()


################################################################################
################################################################################
#' @name menu.mcrv.fittodata
#' @aliases menu.mcrv.fittodata
#' @title Non-executable auxiliary function
#' @usage menu.mcrv.fittodata(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.mcrv.fittodata<-function(item,rriskModel,menuLevel=1)
{ on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  # get all data sets assosiated with the current model
  availableData.names<-c()
  availableData.data<-list()
  items<-rriskModel@items@items
  if(length(items)>0)
  { for(i in 1:length(items))
    { # Betrachte data-Items, bei denen data-Slot nicht leer ist
      if(items[[i]]@typecode=="data" & !is.null(items[[i]]@data))
      { item.name<-items[[i]]@name
        var.names<-names(items[[i]]@data)
        availableData.temp<-paste(item.name,var.names,sep="$")
        availableData.names<-c(availableData.names,availableData.temp)
        availableData.data<-c(availableData.data,as.list(items[[i]]@data))
      }
    }
  }
  choices<-c(availableData.names, "Exit dialog")
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Please choose data set for fitting",choices=choices,help="No further help available",levelTabulator=levelTabulator)
    if(input<=(length(choices)-1))
    { data2fitname<-availableData.names[as.numeric(input)]
      data2fit<-availableData.data[[as.numeric(input)]]
      View(data2fit)
      data2fit<-as.vector(unlist(data2fit)) # wandele in ein vektor um
      cat(data2fitname,": ",head(data2fit),"...\n")
      item<-mcrv.fittodata(data2fit,data2fitname,item,rriskModel)
      break()
    } else if (input==length(choices))
    { break()
    }
    input<-99
  } # end while
} # end of function menu.mcrv.fittodata()



################################################################################
################################################################################
#' @name mcrv.fittodata
#' @aliases mcrv.fittodata
#' @title Non-executable auxiliary function
#' @usage mcrv.fittodata(data2fit,data2fitname,item,rriskModel)
#' @param data2fit ...
#' @param data2fitname ...
#' @param item ...
#' @param rriskModel ...
#' @keywords items
#' @export

mcrv.fittodata<-function(data2fit,data2fitname,item,rriskModel)
{ #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(item))
  
  #-----------------------------------------------------------------------------
  # ist ein geschichteter Item?
  #-----------------------------------------------------------------------------
  batch<-1
  if(item@stratum!=""){
    if(length(rriskModel@items@items)>0){
      for(i in 1:length(rriskModel@items@items)){
        if(rriskModel@items@items[[i]]@typecode=="stra"){
          stratum.item<-rriskModel@items@items[[i]]
          batch<-nrow(stratum.item@data$stratum)
          stid<-as.character(stratum.item@data$stratum$stid)                                  
        } # end if
      } # end for
    } # end if
  } # end if
  
  #-----------------------------------------------------------------------------
  # fit several distributions to data
  #-----------------------------------------------------------------------------
  fitResults<-fit.cont(data2fit)
  if(!is.na(fitResults$chosenDistr))
  { paramNames<-names(fitResults$fittedParams) # gefittete Parameter
    textParams<-c() # Parameter-Infos für 'definition' and 'fullc' Slots
    for(i in 1:length(paramNames)){
      textParams.temp<-paste(paramNames[i],"=",round(fitResults$fittedParams[i],digits=3),sep="")
      textParams<-c(textParams,textParams.temp)
    } # end for
    textParams<-paste(textParams,collapse=",")
    chosenDistr<-fitResults$chosenDistr  # geschätze Verteilungsfamilie

    #---------------------------------------------------------------------------
    # aktualisiere Item Slots
    #---------------------------------------------------------------------------
    definition<-paste(chosenDistr,"(",textParams,"); fitted to ",data2fitname,sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","r",chosenDistr,"(n=",batch,"*rriskModel@settings@N,",textParams,"),byrow=TRUE)",sep="")
    
    #-----------------------------------------------------------------------------
    # hier wird die Ausführbarkeit von 'fullc' geprüft
    #-----------------------------------------------------------------------------
    item.temp<-item
    item.temp@fullc<-fullc
    fullcValues<-itemsEvaluation(item.temp,rriskModel)# hier wird die Ausführbarkeit von fullc geprüft

    cat("\n***************************************\nFull command for ",item@name,"\n***************************************\n")
    if (inherits(fullcValues, "try-error")){
      cat("Full command expression",fullc,"could not be evaluated !\n")
    } else {
      # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
      item@fullc<-fullc
      fullcValues<-data.frame(fullcValues)
      names(fullcValues)<-item@name
      # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
      cat("\nDimension of test evaluation: ",dim(fullcValues),"\n")
      cat("\nitem@fullc=",fullc,"\n")
      cat("\nGenerated item values (full command):\n");print(head(fullcValues));cat("...\n")
      cat("\nSummary statistics of item values (full command):\n"); print(summary(fullcValues),digits=4);cat("\n")
      item@fullc<-fullc
      cat("\n***************************************\nSymbolical definition for",item@name,"\n***************************************\n")
      cat("\nitem@definition=",definition,"\n")
      item@definition<-definition
      item@data<-summary(fullcValues)
    } # end if
    #-----------------------------------------------------------------------------
    # hier wird die Ausführbarkeit von 'relaxc' geprüft
    #----------------------------------------------------------------------------
    relaxc<-paste("matrix(ncol=",batch,",data=runif(rriskModel@settings@N,min=",item@plausimin,",max=",item@plausimax,"),byrow=TRUE)",sep="")
    relaxc.test<-paste("matrix(ncol=",batch,",data=runif(rriskModel@settings@Ntest,min=",item@plausimin,",max=",item@plausimax,"),byrow=TRUE)",sep="")
    relaxcValues<-try(eval(parse(text=relaxc.test)),silent=TRUE)
    if (inherits(relaxcValues, "try-error")){
      cat("Relax command expression",relaxc,"could not be evaluated !\n")
    } else {
      cat("\n***************************************\nRelax command for",item@name,"\n***************************************\n")
      cat("\nDimension of test evaluation: ",dim(relaxcValues),"\n")
      cat("\nitem@relaxc=",relaxc,"\n")
      relaxcValues<-data.frame(relaxcValues)
      names(relaxcValues)<-item@name
      cat("\nGenerated item values (relax command):\n");print(head(relaxcValues));cat("...\n")
      cat("\nSummary statistics of item values (relax command):\n"); print(summary(relaxcValues),digits=4);cat("\n")
      item@relaxc<-relaxc
    }
    #----------------------------------------------------------------------------
    # definiere Output
    #----------------------------------------------------------------------------
    return(item)
    } # end if
} # end of function mcrv.fittodata()


  
################################################################################
################################################################################
#' @name menu.mcrv.fittoperc
#' @aliases menu.mcrv.fittoperc
#' @title Non-executable auxiliary function
#' @usage menu.mcrv.fittoperc(item,rriskModel,menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.mcrv.fittoperc<-function(item,rriskModel,menuLevel=1)
{ #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(item))
  
  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  batch<-1
  if(item@stratum!=""){
    if(length(rriskModel@items@items)>0){
      for(i in 1:length(rriskModel@items@items)){
        if(rriskModel@items@items[[i]]@typecode=="stra"){
          stratum.item<-rriskModel@items@items[[i]]
          batch<-nrow(stratum.item@data$stratum)
          stid<-as.character(stratum.item@data$stratum$stid)                                  
        } # end if
      } # end for
    } # end if
  } # end if
  #p<-c(0.2,0.5,0.8)
  #q<-qf(p,1,2,7,2)
  
  #-----------------------------------------------------------------------------
  # fit several distributions to percentiles
  #-----------------------------------------------------------------------------
  fitResults<-fit.perc()
  if(!is.na(fitResults$chosenDistr)){
    paramNames<-names(fitResults$fittedParams) # gefittete Parameter
    textParams<-c() # Parameter-Infos für 'definition' and 'fullc' Slots
    for(i in 1:length(paramNames)){
      textParams.temp<-paste(paramNames[i],"=",round(fitResults$fittedParams[i],digits=3),sep="")
      textParams<-c(textParams,textParams.temp)
    } # end for
    textParams<-paste(textParams,collapse=",")
    chosenDistr<-fitResults$chosenDistr  # geschätze Verteilungsfamilie

    #definition<-paste(chosenDistr,"(",textParams,"); fitted to percentiles (fit.perc)",sep="")
    definition<-fitResults$fitSummary
    fullc<-paste("matrix(ncol=",batch,",data=","r",chosenDistr,"(n=",batch,"*rriskModel@settings@N,",textParams,"),byrow=TRUE)",sep="")
    
    #-----------------------------------------------------------------------------
    # hier wird die Ausführbarkeit von 'fullc' geprüft
    #-----------------------------------------------------------------------------
    item.temp<-item
    item.temp@fullc<-fullc
    fullcValues<-itemsEvaluation(item.temp,rriskModel)# hier wird die Ausführbarkeit von fullc geprüft
    cat("\n***************************************\nFull command for",item@name,"\n***************************************\n")
    if (inherits(fullcValues, "try-error")){
      cat("Full command expression",fullc,"could not be evaluated !\n")
    } else {
      # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
      item@fullc<-fullc
      fullcValues<-data.frame(fullcValues)
      names(fullcValues)<-item@name
      # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
      cat("\nDimension of test evaluation: ",dim(fullcValues),"\n")
      cat("\nitem@fullc=",fullc,"\n")
      cat("\nGenerated item values (full command):\n");print(head(fullcValues));cat("...\n")
      cat("\nSummary statistics of item values (full command):\n"); print(summary(fullcValues),digits=4);cat("\n")
      item@fullc<-fullc
      cat("\n***************************************\nSymbolical definition for",item@name,"\n***************************************\n")
      cat("\nitem@definition=",definition,"\n")
      item@definition<-definition
      item@data<-summary(fullcValues)
    }
    #-----------------------------------------------------------------------------
    # hier wird die Ausführbarkeit von 'relaxc' geprüft
    #----------------------------------------------------------------------------
    relaxc<-paste("matrix(ncol=",batch,",data=runif(rriskModel@settings@N,min=",item@plausimin,",max=",item@plausimax,"),byrow=TRUE)",sep="")
    relaxc.test<-paste("matrix(ncol=",batch,",data=runif(rriskModel@settings@Ntest,min=",item@plausimin,",max=",item@plausimax,"),byrow=TRUE)",sep="")
    relaxcValues<-try(eval(parse(text=relaxc.test)),silent=TRUE)
    if (inherits(relaxcValues, "try-error")){
      cat("Relax command expression",relaxc,"could not be evaluated !\n")
    } else {
      cat("\n***************************************\nRelax command for",item@name,"\n***************************************\n")
      cat("\nDimension of test evaluation: ",dim(relaxcValues),"\n")
      cat("\nitem@relaxc=",relaxc,"\n")
      relaxcValues<-data.frame(relaxcValues)
      names(relaxcValues)<-item@name
      cat("\nGenerated item values (relax command):\n");print(head(relaxcValues));cat("...\n")
      cat("\nSummary statistics of item values (relax command):\n"); print(summary(relaxcValues),digits=4);cat("\n")
      item@relaxc<-relaxc
   } # end if
   #----------------------------------------------------------------------------
   # definiere Output
   #----------------------------------------------------------------------------
   return(item)
  } # end if(!is.na(fitResults$chosenDistr))
} # end of function mcrv.fit.perc()  
  


################################################################################
################################################################################
#' @name menu.mcrv.cfamily
#' @aliases menu.mcrv.cfamily
#' @title Non-executable auxiliary function
#' @usage menu.mcrv.cfamily(item,rriskModel,dfamily="beta",menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param dfamily ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.mcrv.cfamily<-function(item,rriskModel,dfamily="beta",menuLevel=1)
{ #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(invisible(item)))
  
  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")

  #-----------------------------------------------------------------------------
  # ist ein geschichteter Item?
  #-----------------------------------------------------------------------------
  batch<-1
  if(item@stratum!=""){
    if(length(rriskModel@items@items)>0){
      for(i in 1:length(rriskModel@items@items)){
        if(rriskModel@items@items[[i]]@typecode=="stra"){
          stratum.item<-rriskModel@items@items[[i]]
          batch<-nrow(stratum.item@data$stratum)
          stid<-as.character(stratum.item@data$stratum$stid)
        }
      }
    }
  }
    
  #-----------------------------------------------------------------------------
  # enter distribution parameter values
  #-----------------------------------------------------------------------------  
  if(dfamily=="beta"){
    shape1.mess<-paste("Enter ",batch," value(s) for beta distribution parameter shape1 (positive)",sep="")
    shape1<-myvalue.distrparams(item,rriskModel,title=shape1.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    shape2.mess<-paste("Enter ",batch," value(s) for beta distribution parameter shape2 (positive)",sep="")
    shape2<-myvalue.distrparams(item,rriskModel,title=shape2.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("beta(shape1=",shape1,",shape2=",shape2,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rbeta(n=",batch,"*rriskModel@settings@N,shape1=",shape1,",shape2=",shape2,"),byrow=TRUE)",sep="")
  } else if(dfamily=="cauchy"){
    location.mess<-paste("Enter ",batch," value(s) for Cauchy distribution parameter location",sep="")
    location<-myvalue.distrparams(item,rriskModel,title=location.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    scale.mess<-paste("Enter ",batch," value(s) for Cauchy distribution parameter scale (positive)",sep="")
    scale<-myvalue.distrparams(item,rriskModel,title=scale.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("cauchy(scale=",scale,",location=",location,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rcauchy(n=",batch,"*rriskModel@settings@N,location=",location,",scale=",scale,"),byrow=TRUE)",sep="")
  } else if(dfamily=="chisq"){
    df.mess<-paste("Enter ",batch," value(s) for chi-squared distribution parameter location (positive integer)",sep="")
    df<-myvalue.distrparams(item,rriskModel,title=df.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),asInteger=TRUE,batch=batch)$output
    definition<-paste("chisq(df=",df,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rchisq(n=",batch,"*rriskModel@settings@N,df=",df,"),byrow=TRUE)",sep="")
  } else if(dfamily=="chisqnc"){
    df.mess<-paste("Enter ",batch," value(s) for non-central chi-squared distribution parameter df (positive)",sep="")
    df<-myvalue.distrparams(item,rriskModel,title=df.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    #df<-round(df,digits=0)
    ncp.mess<-paste("Enter ",batch," value(s) for non-central chi-squared distribution parameter ncp (positive)",sep="")
    ncp<-myvalue.distrparams(item,rriskModel,title=ncp.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("chisqnc(df=",df,",ncp=",ncp,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rchisq(n=",batch,"*rriskModel@settings@N,df=",df,",ncp=",ncp,"),byrow=TRUE)",sep="")
  } else if(dfamily=="exp"){
    rate.mess<-paste("Enter ",batch," value(s) for exponential distribution parameter rate (positive)",sep="")
    rate<-myvalue.distrparams(item,rriskModel,title=rate.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("exp(rate=",rate,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rexp(n=",batch,"*rriskModel@settings@N,rate=",rate,"),byrow=TRUE)",sep="")
  } else if(dfamily=="f"){
    df1.mess<-paste("Enter ",batch," value(s) for F distribution parameter df1 (positive)",sep="")
    df1<-myvalue.distrparams(item,rriskModel,title=df1.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    df2.mess<-paste("Enter ",batch," value(s) for F distribution parameter df2 (positive)",sep="")
    df2<-myvalue.distrparams(item,rriskModel,title=df2.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("f(df1=",df1,",df2=",df2,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rf(n=",batch,"*rriskModel@settings@N,df=",df1,",df2=",df2,"),byrow=TRUE)",sep="")
  } else if(dfamily=="gamma"){
    shape.mess<-paste("Enter ",batch," value(s) for gamma distribution parameter shape (positive)",sep="")
    shape<-myvalue.distrparams(item,rriskModel,title=shape.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    rate.mess<-paste("Enter ",batch," value(s) for gamma distribution parameter rate (positive)",sep="")
    rate<-myvalue.distrparams(item,rriskModel,title=rate.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("gamma(shape=",shape,",rate=",rate,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rgamma(n=",batch,"*rriskModel@settings@N,shape=",shape,",rate=",rate,"),byrow=TRUE)",sep="")
  } else if(dfamily=="logis"){
    location.mess<-paste("Enter ",batch," value(s) for logistic distribution parameter location",sep="")
    location<-myvalue.distrparams(item,rriskModel,title=location.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    scale.mess<-paste("Enter ",batch," value(s) for logistic distribution parameter scale (positive)",sep="")
    scale<-myvalue.distrparams(item,rriskModel,title=scale.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("logis(location=",location,",scale=",scale,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rlogis(n=",batch,"*rriskModel@settings@N,location=",location,",scale=",scale,"),byrow=TRUE)",sep="")
  } else if(dfamily=="lnorm"){
    meanlog.mess<-paste("Enter ",batch," value(s) for log-normal distribution parameter meanlog",sep="")
    meanlog<-myvalue.distrparams(item,rriskModel,title=meanlog.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    sdlog.mess<-paste("Enter ",batch," value(s) for log-normal distribution parameter sdlog (positive)",sep="")
    sdlog<-myvalue.distrparams(item,rriskModel,title=sdlog.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("lnorm(meanlog=",meanlog,",sdlog=",sdlog,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rlnorm(n=",batch,"*rriskModel@settings@N,meanlog=",meanlog,",sdlog=",sdlog,"),byrow=TRUE)",sep="")
  } else if(dfamily=="norm"){
    mean.mess<-paste("Enter ",batch," value(s) for normal distribution parameter mean",sep="")
    mean<-myvalue.distrparams(item,rriskModel,title=mean.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    sd.mess<-paste("Enter ",batch," value(s) for normal distribution parameter sd (positive)",sep="")
    sd<-myvalue.distrparams(item,rriskModel,title=sd.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("norm(mean=",mean,",sd=",sd,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rnorm(n=",batch,"*rriskModel@settings@N,mean=",mean,",sd=",sd,"),byrow=TRUE)",sep="")
  } else if(dfamily=="tnorm"){
    mean.mess<-paste("Enter ",batch," value(s) for truncated normal distribution parameter mean",sep="")
    mean<-myvalue.distrparams(item,rriskModel,title=mean.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    sd.mess<-paste("Enter ",batch," value(s) for truncated normal distribution parameter sd (positive)",sep="")
    sd<-myvalue.distrparams(item,rriskModel,title=sd.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    lower.mess<-paste("Enter ",batch," value(s) for truncated normal distribution parameter lower (< mean)",sep="")
    lower<-myvalue.distrparams(item,rriskModel,title=lower.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    upper.mess<-paste("Enter ",batch," value(s) for truncated normal distribution parameter upper (> mean)",sep="")
    upper<-myvalue.distrparams(item,rriskModel,title=upper.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    definition<-paste("tnorm(mean=",mean,",sd=",sd,",lower=",lower,",upper=",upper,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rtnorm(n=",batch,"*rriskModel@settings@N,mean=",mean,",sd=",sd,",lower=",lower,",upper=",upper,"),byrow=TRUE)",sep="")
  } else if(dfamily=="t"){
    df.mess<-paste("Enter ",batch," value(s) for t distribution parameter t (positive)",sep="")
    df<-myvalue.distrparams(item,rriskModel,title=df.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    definition<-paste("t(df=",df,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rt(n=",batch,"*rriskModel@settings@N,df=",df,"),byrow=TRUE)",sep="")
  } else if(dfamily=="triang"){
    min.mess<-paste("Enter ",batch," value(s) for triangular distribution parameter min",sep="")
    min<-myvalue.distrparams(item,rriskModel,title=min.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    mode.mess<-paste("Enter ",batch," value(s) for triangular distribution parameter mode (> min)",sep="")
    mode<-myvalue.distrparams(item,rriskModel,title=mode.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    max.mess<-paste("Enter ",batch," value(s) for triangular distribution parameter max (> mode)",sep="")
    max<-myvalue.distrparams(item,rriskModel,title=max.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    definition<-paste("triang(min=",min,",mode=",mode,",max=",max,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rtriang(n=",batch,"*rriskModel@settings@N,min=",min,",mode=",mode,",max=",max,"),byrow=TRUE)",sep="")
  } else if(dfamily=="unif"){
    min.mess<-paste("Enter ",batch," value(s) for uniform distribution parameter min",sep="")
    min<-myvalue.distrparams(item,rriskModel,title=min.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    max.mess<-paste("Enter ",batch," value(s) for uniform distribution parameter max",sep="")
    max<-myvalue.distrparams(item,rriskModel,title=max.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    definition<-paste("unif(min=",min,",max=",max,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","runif(n=",batch,"*rriskModel@settings@N,min=",min,",max=",max,"),byrow=TRUE)",sep="")
  } else if(dfamily=="weibull"){
    shape.mess<-paste("Enter ",batch," value(s) for Weibull distribution parameter shape (positive)",sep="")
    shape<-myvalue.distrparams(item,rriskModel,title=shape.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    scale.mess<-paste("Enter ",batch," value(s) for Weibull distribution parameter scale (positive)",sep="")
    scale<-myvalue.distrparams(item,rriskModel,title=scale.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    definition<-paste("weibull(shape=",shape,",scale=",scale,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rweibull(n=",batch,"*rriskModel@settings@N,shape=",shape,",scale=",scale,"),byrow=TRUE)",sep="")
  } else if(dfamily=="pert"){
    min.mess<-paste("Enter ",batch," value(s) for beta pert distribution parameter min",sep="")
    min<-myvalue.distrparams(item,rriskModel,title=min.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    mode.mess<-paste("Enter ",batch," value(s) for beta pert distribution parameter mode (> min)",sep="")
    mode<-myvalue.distrparams(item,rriskModel,title=mode.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    max.mess<-paste("Enter ",batch," value(s) for beta pert distribution parameter max (> mode)",sep="")
    max<-myvalue.distrparams(item,rriskModel,title=max.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    shape.mess<-paste("Enter ",batch," value(s) for beta pert distribution parameter shape",sep="")
    shape<-myvalue.distrparams(item,rriskModel,title=shape.mess,menuLevel=menuLevel+1,minmax=c(-Inf,Inf),batch=batch)$output
    definition<-paste("pert(min=",min,",mode=",mode,",max=",max,",shape=",shape,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rpert(n=",batch,"*rriskModel@settings@N,min=",min,",mode=",mode,",max=",max,",shape=",shape,"),byrow=TRUE)",sep="")
  } else if(dfamily=="gompertz"){
    shape.mess<-paste("Enter ",batch," value(s) for Gompertz distribution parameter shape (positive)",sep="")
    shape<-myvalue.distrparams(item,rriskModel,title=shape.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    scale.mess<-paste("Enter ",batch," value(s) for Gompertz distribution parameter scale (positive)",sep="")
    scale<-myvalue.distrparams(item,rriskModel,title=scale.mess,menuLevel=menuLevel+1,minmax=c(0,Inf),batch=batch)$output
    definition<-paste("gompertz(shape=",shape,",scale=",scale,")",sep="")
    fullc<-paste("matrix(ncol=",batch,",data=","rgompertz(n=",batch,"*rriskModel@settings@N,shape=",shape,",scale=",scale,"),byrow=TRUE)",sep="")
  } 
  #-----------------------------------------------------------------------------
   # hier wird die Ausführbarkeit von 'fullc' geprüft
   #-----------------------------------------------------------------------------
   item.temp<-item
   item.temp@fullc<-fullc
   cat("\n----------------------------------------------------------------------\nEvaluating item values with predefined distribution parameters...\n----------------------------------------------------------------------\n")
   fullcValues<-itemsEvaluation(item.temp,rriskModel)# hier wird die Ausführbarkeit von fullc geprüft
   cat("\n***************************************\nFull command for",item@name,"\n***************************************\n")
   if (inherits(fullcValues, "try-error")){
    cat("Full command expression",fullc,"could not be evaluated !\n")
   } else {
    # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
     item@fullc<-fullc
     if(batch>1){
      colnames(fullcValues)<-stid
     } else {
      colnames(fullcValues)<-item@name
     }     
     # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
     cat("\nDimension of test evaluation: ");cat(dim(fullcValues));cat("\n")
     cat("\nitem@fullc=",fullc,"\n")
     cat("\nGenerated item values (full command):\n");print(head(data.frame(fullcValues)));cat("...\n")
     cat("\nSummary statistics of item values (full command):\n"); print(summary(fullcValues),digits=4);cat("\n")
     item@fullc<-fullc
     cat("\n***************************************\nSymbolical definition for",item@name,"\n***************************************\n")
     cat("\nitem@definition=",definition,"\n")
     item@definition<-definition
     #--------------------------------------------------------------------------
     # save simulated item value in data-slot
     #--------------------------------------------------------------------------
     item@data<-summary(fullcValues)
   }
   #-----------------------------------------------------------------------------
   # hier wird die Ausführbarkeit von 'relaxc' geprüft
   #----------------------------------------------------------------------------
   relaxc<-paste("matrix(ncol=",batch,",data=runif(",batch,"*rriskModel@settings@N,min=",item@plausimin,",max=",item@plausimax,"),byrow=TRUE)",sep="")
   relaxc.test<-paste("matrix(ncol=",batch,",data=runif(",batch,"*rriskModel@settings@Ntest,min=",item@plausimin,",max=",item@plausimax,"),byrow=TRUE)",sep="")
   relaxcValues<-try(eval(parse(text=relaxc.test)),silent=TRUE)
   if (inherits(relaxcValues, "try-error")){
    cat("Relax command expression",relaxc,"could not be evaluated !\n")
   } else { 
     if(batch>1){
      colnames(relaxcValues)<-stid
     } else { colnames(relaxcValues)<-item@name}   
     cat("\n***************************************\nRelax command for",item@name,"\n***************************************\n")
     cat("\nDimension of test evaluation: ");cat(dim(relaxcValues));cat("\n")
     cat("\nitem@relaxc=",relaxc,"\n")
     cat("\nGenerated item values (relax command):\n");print(head(data.frame(relaxcValues)));cat("...\n")
     cat("\nSummary statistics of item values (relax command):\n"); print(summary(relaxcValues),digits=4);cat("\n")
     item@relaxc<-relaxc
   }
   #----------------------------------------------------------------------------
   # definiere Output
   #----------------------------------------------------------------------------
   return(item)
} # end of function  menu.mcrv.dfamily()




################################################################################
################################################################################
#' @description This function generates the user menu for maintaining scoring systems
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name menu.define.data
#' @aliases menu.define.data
#' @title Non-executable auxiliary function
#' @usage menu.define.data(item,menuLevel=1)
#' @param item ...
#' @param menuLevel numeric value indicating menu level
#' @keywords items
#' @export

menu.define.data<-function(item,menuLevel=1)
{ on.exit(return(invisible(item)))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  
  availableData<-list.files(system.file("data", package="rrisk"))
  availableData<-apply(matrix(availableData,ncol=1),1,function(x)gsub(x=x,".rda",replacement=""))
  rriskData<-paste(availableData,"(rrisk data set)")
  
  if(length(availableData)>0){
    choices<-c(rriskData,
             "load user-defined data set",
             "Exit dialog")
    input<-99
    while(!is.element(input,seq(1:length(choices)))){
      input<-mymenu(title="Choose the data set you wish add to the current model item",choices=choices,help="No further help available",levelTabulator=levelTabulator)
      if(input<=(length(choices)-2)){
        datasetName<-availableData[as.numeric(input)]
        data(list=datasetName,package="rrisk",envir = .GlobalEnv)
        item@data<-get(datasetName,envir = .GlobalEnv)
        assign(item@name,value=item@data,envir = .GlobalEnv)
        print(head(get(item@name,envir = .GlobalEnv))) # show head of the data set
        cat("...\n\n")
        cat("Summary statistics:\n")
        print(summary(get(item@name,envir = .GlobalEnv)),digits=4)
        cat("\nData set has been successfully added to the current model item.\n")
        View(get(item@name,envir = .GlobalEnv),title="Loaded data set") 
        break()
      } else if(input==(length(choices)-1)){
        item<-dataLoad(item)
        break()
      } else if (input==length(choices)){
        break()
      } 
      input<-99
    } # end while
  } else {
    choices<-c("load user-defined data set","Exit dialog")
    input<-99
    while(!is.element(input,seq(1:length(choices)))){
      input<-mymenu(title="Choose the data set you wish add to the current model item",choices=choices,help="No further help available",levelTabulator=levelTabulator)
      if(input==(length(choices)-1)){
        item<-dataLoad(item)
        break()
      } else if (input==length(choices)){
        break()
      } 
      input<-99
    } # end while
  }
} # end of function menu.chooseDataset() 





################################################################################
################################################################################
#' @description Function that loads data sets
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name dataLoad
#' @aliases dataLoad
#' @title Non-executable auxiliary function
#' @usage dataLoad(item)
#' @param item ...
#' @keywords items
#' @export

dataLoad<-function(item)
{ on.exit(return(item))
  dataFile<-tclvalue(tkgetOpenFile(filetypes=" {{Data file} {.txt}} "))
  if(nchar(dataFile)>0)
  { try.result<-try(dataset<-read.table(dataFile,header=TRUE))
    if (inherits(try.result, "try-error"))
    { tkmessageBox(message=paste("The data set could NOT be loadet. Please, check the data format!"),icon="error",type="ok")
    } else
    { item.name<-item@name
      item@data<-dataset
      item@definition<- paste("Variables: ",paste(names(dataset),collapse=" "))
      assign(item.name,value=dataset,envir = .GlobalEnv) # activate the data set 
      tkmessageBox(message="The data set has been successfully loaded!")  
      print(head(get(item.name,envir = .GlobalEnv))) # show head of the data set
      cat("...\n\n")
      cat("Summary statistics:\n")
      print(summary(get(item.name,envir = .GlobalEnv)),digits=4) 
      View(get(item.name,envir = .GlobalEnv),title="Loaded data set")   
    }
  } # end if(nchar(modelFile)>0)
} # end function  dataLoad()      
      


################################################################################
################################################################################
#' @description Function that evaluates and saves the input value for item objects from the
#' rriskModel class
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name myvalue
#' @aliases myvalue
#' @title Non-executable auxiliary function
#' @usage myvalue(item,rriskModel,title="Tryme",menuLevel=1,batch=1,run=FALSE)
#' @param item ...
#' @param rriskModel ...
#' @param title ... 
#' @param menuLevel ...
#' @param batch ...
#' @param run boolean value defining whether this function nwill be calls during 1d or 2d simulation, \code{run=c(TRUE,FALSE)}
#' @keywords items
#' @export

myvalue<-function(item,rriskModel,title="Tryme",menuLevel=1,batch=1,run=FALSE)
{ #-------------------------------------------------------------------------------
  # define help variables
  #-------------------------------------------------------------------------------
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")
  length.error <- paste(levelTabulator,"Entry does not evaluate to required length",batch,levelTabulator)
  output<-NA
  values<-NA

  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(list(output=output,values=values)))
  
  #-----------------------------------------------------------------------------
  # get r-code for current item
  #-----------------------------------------------------------------------------
  try.again <- TRUE
  while (try.again){
    cat(levelTabulator,title,levelTabulator,rep("-",35),levelTabulator)
    txt <- readline()
    #-----------------------------------------------------------------------------
    # get data to evaluate current item
    #-----------------------------------------------------------------------------
    dataForWith<-get.dataForWith(item,rriskModel,run=run,catEval=FALSE)
    
    #---------------------------------------------------------------------------
    # versuche txt-expression zu evaluieren
    #---------------------------------------------------------------------------
    if(length(dataForWith)>0){
      try.result<-try(values<-with(data=dataForWith,suppressWarnings(eval(parse(text=txt)))),silent=TRUE)
    } else{
     try.result<-try(values<-suppressWarnings(eval(parse(text=txt))),silent=TRUE)
    }
    #---------------------------------------------------------------------------
    # falls mit einer Fehlermeldung abgebrochen, zeige entsprechende Meldung
    #---------------------------------------------------------------------------
    if(inherits(try.result, "try-error")){
     cat(levelTabulator,"The expression cannot be evaluated because of unknown variable, data oder function!","\n")
     next()
    } else {
      #-----------------------------------------------------------------------
      # und die Länge von values muss auch stimmen
      #-----------------------------------------------------------------------
      if(item@typecode=="numv")
      { if(length(values)!=batch){
          cat(levelTabulator,"Entry does not evaluate to required length of ",batch,"\n")
          next()
        }
      } else if(item@typecode=="fnrv"){
        requiredRows<-rriskModel@settings@N
        if(nrow(values)!=requiredRows | ncol(values)!=batch){
          cat(levelTabulator,"Entry does not evaluate to required number of rows ",requiredRows,"and",batch,"columns\n")
          next()
        }
      }
      #-------------------------------------------------------------------------
      # Lasse die (gerundete) Ergebnisse als Zwischenausgabe anzeigen
      #-------------------------------------------------------------------------
      if(item@stratum==""){
        cat("\nEntry evaluates to:",paste(try(round(values,digits=2)),collapse=", "),"\n")
      } else {
        cat("\nEntry evaluates to:\n");print(head(values));cat("...\n")
      }
      output<-txt
      try.again<-FALSE
    } # end if(inherits(try.result, "try-error"))
  } # end while
  return(list(output=output,values=values))
} # end function myvalue()



################################################################################
################################################################################
#' @description Function that evaluates and saves the input numeric value
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name define.numv.fnrv
#' @aliases define.numv.fnrv
#' @title Non-executable auxiliary function
#' @usage define.numv.fnrv(item,rriskModel,menuLevel=1,type="numv")
#' @param item ...
#' @param rriskModel an instance of the class \code{modelClass}
#' @param menuLevel ...
#' @param type character string defining item type, \code{type=c("numv","fnrv")}
#' @keywords items
#' @export

define.numv.fnrv <- function(item,rriskModel,menuLevel=1,type="numv")
{ #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(item))
  
  #-----------------------------------------------------------------------------
  # define some help varisbles
  #-----------------------------------------------------------------------------
  item.name<-item@name
  batch<-1
  
  #-----------------------------------------------------------------------------
  # ist es ein geschichteter item?
  #-------------------------------------------- ---------------------------------
  if(item@stratum!=""){
    if(length(rriskModel@items@items)>0){
      for(i in 1:length(rriskModel@items@items)){
        stratum.item<-rriskModel@items@items[[i]]
        if(stratum.item@name==item@stratum){
          batch<-nrow(stratum.item@data$stratum)
          stid<-as.character(stratum.item@data$stratum$stid) # only for fnrv items
        }
      }
    }
  } # end
  
  #-----------------------------------------------------------------------------
  # define console message
  #-----------------------------------------------------------------------------
  if(type=="fnrv"){
    title <- paste("Enter expression that could be evaluated to data frame consisting of",batch,"columns and",rriskModel@settings@N,"rows")
  } else if (type=="numv"){
    title <- paste("Enter expression that could be evaluated to",batch,"numerical values(s)")
  }
   
  #----------------------------------------------------------------------------
  # Hier wird 'fullc' definiert und als Zeichenkette zurückgegeben
  #----------------------------------------------------------------------------
  myvalue.results<-myvalue(item,rriskModel=rriskModel,title,menuLevel=menuLevel,batch=batch,run=TRUE)
  
  #-----------------------------------------------------------------------------
  # save evaluated results
  #-----------------------------------------------------------------------------
  fullc<-myvalue.results$output
  values<-myvalue.results$values
   
  #-----------------------------------------------------------------------------
  # hier werden die Ergebnise angezeit
  #-----------------------------------------------------------------------------
  cat("\n***************************************\nFull command for",item@name,"\n***************************************\n")
  item@fullc<-fullc # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
  cat("\nDimension of test evaluation: ",dim(values),"\n")
  cat("\nitem@fullc=",fullc,"\n") # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen
  if(type=="numv" & batch==1){
    cat("\nGenerated item value:",values,"\n")
  } else if(type=="numv" & batch>1){
     names(values)<-stid
     cat("\nGenerated item values (full command):\n");print(values);cat("\n")
  } else if(type=="fnrv"){
    if(batch==1){
      colnames(values)<-item@name
    } else {
      colnames(values)<-stid
    }
    cat("\nGenerated item values (full command):\n\n");print(head(values));cat("...\n")
    cat("\nSummary statistics of item values (full command):\n"); print(summary(values),digits=4);cat("\n")
  }
  item@fullc<-fullc
  cat("\n***************************************\nSymbolical definition for",item@name,"\n***************************************\n")
  cat("\nitem@definition=",fullc,"\n")
  item@definition<-fullc
  cat("\n***************************************\nRelax command for",item@name,"\n***************************************\n")
  item@relaxc<-fullc
  cat("\nitem@relaxc=",fullc,"\n") # Falls 'fullc' ausführbar ist, lasse die Zwischenergebnisse anzeigen 
  cat("\n")
  
  #--------------------------------------------------------------------------
  # save simulated item value in data-slot
  #--------------------------------------------------------------------------
  if(type=="numv"){
    item@data<-values
  } else if (type=="fnrv"){
    item@data<-summary(values)
  }
  
  #----------------------------------------------------------------------------
  # definiere Output
  #----------------------------------------------------------------------------
  return(item)
} # end of function define.numv.fnrv()





################################################################################
################################################################################
#' @description Function that loads external models
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name define.fdoc
#' @aliases define.fdoc
#' @title Non-executable auxiliary function
#' @usage define.fdoc(item)
#' @param item ...
#' @keywords items
#' @export

define.fdoc<-function(item)
{ on.exit(return(item))
  if(is.null(item@data))
  { tempfun<-fix(tempfun)
    assign(item@name,tempfun)
    item@data<-get(item@name)
  } else
  { assign("fdocedited",item@data)
    fdocedited<-fix(fdocedited)
    item@data<-get("fdocedited")
  }
  assign(item@name,item@data,envir=.GlobalEnv)
  temp1<-paste(item@name,"<-",deparse(item@data)[1],collapse="")
  temp2<-paste(deparse(item@data)[-1],collapse="\n")
  temp3<-paste(temp1,temp2,collapse="\n")
  item@definition<-temp3
  return(item)
} # end of function define.fdoc()






################################################################################
################################################################################
#' @description This function activates all functions documented in 'fdoc' items, 
#' i.e. this function add all fdoc function to the workspace.
#'
#' @name activateFDOCs
#' @aliases activateFDOCs
#' @title Non-executable auxiliary function
#' @usage activateFDOCs(items)
#' @param items ...
#' @keywords items
#' @export

activateFDOCs<-function(items)
{
  items<-items@items
  if(length(items)>0)
  { for(i in 1:length(items))
    { if(items[[i]]@typecode=="fdoc")  assign(items[[i]]@name,items[[i]]@data,envir=.GlobalEnv)
    }
  }
} # end of function activateFDOC()

