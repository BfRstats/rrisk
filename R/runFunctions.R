

################################################################################
################################################################################
#' @name resampleSTRATA
#' @aliases resampleSTRATA
#' @title Non-executable auxiliary function
#' @usage resampleSTRATA(rriskModel)
#' @param rriskModel  ...
#' @keywords run
#' @export

resampleSTRATA<-function(rriskModel){
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(rriskModel))
  
  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  items<-rriskModel@items@items
  
  #-----------------------------------------------------------------------------
  # loop over all items
  #-----------------------------------------------------------------------------
  if(length(items)>0){
    for(i in 1:length(items)){
      if(items[[i]]@typecode=="stra"){
        cat("\n---> Resample strv of strata-Item",items[[i]]@name,"\n")
        cat("\tOld strv values:",items[[i]]@data$strv[1:30],"...\n")
        stratumData<-list(items[[i]]@data)
        names(stratumData)[[1]]<-items[[i]]@name
        items[[i]]@data$strv<-with(stratumData,eval(parse(text=items[[i]]@fullc)))      
        cat("\tNew strv values:",items[[i]]@data$strv[1:30],"...\n")
      } # end if(items[[i]]@typecode=="stra"){
    } # end for
  } # end if(length(items)>0){
  
  #-----------------------------------------------------------------------------
  # save modified items to the model
  #-----------------------------------------------------------------------------
  rriskModel@items@items<-items
} # end of function resampleSTRATA()


################################################################################
################################################################################
#' @name run2dsimulation
#' @aliases run2dsimulation
#' @title Non-executable auxiliary function
#' @usage run2dsimulation(rriskModel,menuLevel=1)
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords run
#' @export

run2dsimulation<-function(rriskModel,menuLevel=1){

  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(rriskModel))
  
  #-----------------------------------------------------------------------------
  # reset simulation results
  #-----------------------------------------------------------------------------
  #rriskModel<-resetSimulationResults(rriskModel)
  
  #-----------------------------------------------------------------------------
  # run 1d simulation
  #-----------------------------------------------------------------------------
  rriskModel<-run1dsimulation(rriskModel)

  #-----------------------------------------------------------------------------
  # compute some help variables
  #-----------------------------------------------------------------------------
  N2d<-rriskModel@settings@N2d
  items<-rriskModel@items@items

  #-------------------------------------------------------------------------------
  # get OF, v and uv items
  #-------------------------------------------------------------------------------
  OF.items<-c()
  v.items<-c()
  uv.items<-c()
  if(length(items)>0){
    for(i in 1:length(items)){
      #-------------------------------------------------------------------------
      # collect outcome- and remain items
      #-------------------------------------------------------------------------
      if(items[[i]]@rolecode=="OF"){
        OF.items<-c(OF.items,items[[i]]@name)
      } # end if(items[[i]]@rolecode=="OF"){
      if(items[[i]]@typecode=="mcrv") {
        #-----------------------------------------------------------------------
        # collect variability and uncertainty items
        #-----------------------------------------------------------------------
        if(items[[i]]@rolecode=="v"){
          v.items<-c(v.items,items[[i]]@name)
        } else if(items[[i]]@rolecode=="u" | items[[i]]@rolecode=="uv"){
          uv.items<-c(uv.items,items[[i]]@name)
          items[[i]]@fullc<-gsub(x=items[[i]]@fullc,pattern="rriskModel@settings@N",replacement="1")
        } # end if(items[[i]]@rolecode=="v"){
      } # end  if(items[[i]]@rolecode=="mcrv") {
    } # end for loop
    rriskModel.temp<-rriskModel
    rriskModel.temp@items@items<-items
  } else {
    stop("Current model cannot be run, the list of model items is empty!",call.=FALSE)
  } # end if(length(items)>0)


  #-----------------------------------------------------------------------------
  # run 2d simulation
  #-----------------------------------------------------------------------------
  if(length(v.items)>0 & length(uv.items)>0 & N2d>0){
    
    #---------------------------------------------------------------------------
    # get OF items f?r 2d simulation
    #---------------------------------------------------------------------------
    if(length(OF.items)>1){
      levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
      levelTabulator<-paste("\n",levelTabulator,sep="")
      choices<-c(OF.items)
      input<-mymenu(title="Please choose the the outcome item for executing 2d simulation",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
      OF<-OF.items[[as.numeric(input)]]
    } else {
      OF<-OF.items[1]
    } # end if(length(OF.items)>1){
    
    start.clock2d <- proc.time()[3]    # start clock

    cat("*************************************************************************\n")
    cat("Begin 2d simulation...\n")
    cat("*************************************************************************\n")

    #-------------------------------------------------------------------------------
    # define some help variables
    #-------------------------------------------------------------------------------
    fullout.2d<-c()
    uncertainty.2d<-c()     
        
    #---------------------------------------------------------------------------
    # OF.q collects statistics of all outcome function items for display
    # it has 13 rows for quantiles and cols equal to number of OF items
    #---------------------------------------------------------------------------
    OF2d.q <- as.data.frame(matrix(nrow=13,ncol=3,data=NA))
    rownames(OF2d.q) <- c("Qt0.01","Qt0.1","Qt1.0","Qt2.5","Qt5.0","Qt10","Qt50","Qt90","Qt95","Qt97.5","Qt99","Qt99.9","Qt99.99")   ##"Type","Part", have been taken out here
    colnames(OF2d.q) <-c(paste(OF,"(50)",sep=""),paste(OF,"(2.5)",sep=""),paste(OF,"(97.5)",sep=""))

    # test
    OF.cdf       <- matrix(nrow=N2d,ncol=114)
    OF.cdf.CI    <- matrix(nrow=2,ncol=114)
    colnames(OF.cdf.CI) <- paste("Q",c(100*seq(0,1,.01),100*c(.0001,.001,.01,.025,.05,.1,.5,.9,.95,.975,.99,.999,.9999)),sep="")
    # end test

    #---------------------------------------------------------------------------
    # 2d simulation = run 1d simulations N2d times
    #---------------------------------------------------------------------------
    for(i in 1:N2d){
      cat("Iteration ", i,"...\n") 
      results.2d<-get.dataForWith(item=new("itemClass"),rriskModel.temp,catResults=FALSE,fullc=TRUE,run=TRUE,run2d=TRUE,catEval=FALSE)
      fullout.2d<-cbind(fullout.2d,results.2d[[OF]])
      uncertainty.2d<-rbind(uncertainty.2d,results.2d[uv.items])    
      OF.cdf[i,] <- signif(quantile(fullout.2d[,i],c(seq(0,1,.01),c(.0001,.001,.01,.025,.05,.1,.5,.9,.95,.975,.99,.999,.9999)),na.rm=TRUE,type=7),digits=4)
    } # end for(i in 1:N2d) 
          
    #---------------------------------------------------------------------------
    # calculate quantile of the outcome function
    #---------------------------------------------------------------------------
    OF2d.q[,1]<-signif(quantile(apply(fullout.2d,2,median),
          c(0.0001,0.001,0.01,0.025,0.05,0.1,0.5,0.9,0.95,0.975,0.99,0.999,0.9999),na.rm=TRUE,type=7),digits=4)
    OF2d.q[,2]<-signif(quantile(apply(fullout.2d,2,function(x)quantile(x,0.025)),
          c(0.0001,0.001,0.01,0.025,0.05,0.1,0.5,0.9,0.95,0.975,0.99,0.999,0.9999),na.rm=TRUE,type=7),digits=4)
    OF2d.q[,3]<-signif(quantile(apply(fullout.2d,2,function(x)quantile(x,0.95)),
          c(0.0001,0.001,0.01,0.025,0.05,0.1,0.5,0.9,0.95,0.975,0.99,0.999,0.9999),na.rm=TRUE,type=7),digits=4)
    
    # test
    for (perc in 1:114) OF.cdf.CI[,perc] <- quantile(OF.cdf[,perc],probs=c(.025,.975))
    # if(test.mode) print(OF.cdf.CI)
    # end test

    #-----------------------------------------------------------------------------
    OF1d.q<-rriskModel@output@summaries
    OFindex<-which(names(OF1d.q)==OF)
    if(OFindex < ncol(OF1d.q)){
      OF.q<-cbind(OF1d.q[,1:OFindex],OF2d.q,OF1d.q[,(OFindex+1):ncol(OF1d.q)])
      names(OF.q)[1:OFindex]<-names(OF1d.q)[1:OFindex]
      names(OF.q)[(OFindex+1):ncol(OF1d.q)+ncol(OF2d.q)]<-names(OF1d.q)[(OFindex+1):ncol(OF1d.q)]
    } else{ 
      OF.q<-cbind(OF1d.q[,1:OFindex],OF2d.q)
      names(OF.q)[1:OFindex]<-names(OF1d.q)[1:OFindex]
    } # end if(OFindex < ncol(OF1d.q)){
    
    #---------------------------------------------------------------------------
    # save simulation results to the rrisk model
    #---------------------------------------------------------------------------    
    rriskModel@output@OFname.2d<-OF
    rriskModel@output@fullout.2d<-fullout.2d
    rriskModel@output@uncitems.2d<-uncertainty.2d
    rriskModel@output@OFcdfCI <- OF.cdf.CI
    rriskModel@output@summaries<-OF.q
    
    #-----------------------------------------------------------------------------
    # speichere runtime
    #-----------------------------------------------------------------------------
    runtime2d<-proc.time()[3]-start.clock2d
    rriskModel@output@runtime2d<-runtime2d
    
    #-----------------------------------------------------------------------------
    # Zeige Ergebnisse der 2d Simulation                                                    .
    #-----------------------------------------------------------------------------
    cat("---------------------------------------------------------------------------\n")
    cat("Quantiles of the simulated outcome function of the model\n")
    cat("---------------------------------------------------------------------------\n")
    print(rriskModel@output@summaries)
    cat("\nThe quantiles are calculated using the empirical probabilities
    p(k)=(k-1)/(n-1) (n ordered data x_1, ..., x_k, ..., x_ n).
    Type 'help(quantiles)' for further information.
    Quantiles having less than 5 data points below are stated with NA to represent their inaccuracy.
    (50)= Estimate of quantile based on 2d simulation.
    (2.5) = Lower limit of 95% uncertainty inverval of quantile based on 2d order simulation.
    (97.5) = Upper limit of 95% uncertainty inverval of quantile based on 2d order simulation.")
    cat("\n\n")
  
    #-------------------------------------------------------------------------------
    # create cdf of outcome function(s)
    #-------------------------------------------------------------------------------
    cat("Plotting cdf of the outcome function(s)...\n\n")
    plotCDF(rriskModel)

    cat("*************************************************************************\n")
    cat("End 2d simulation...")
    cat("run time of 2d simulation is ",runtime2d," seconds\n",sep="")
    cat("*************************************************************************\n")

  } else cat("Model does not contain any 'uv', 'u' or any 'v' and the length of the 2d simulation is probably equal to zero!\n")

} # end of function run2dsimulation()



################################################################################
################################################################################
#' @name run1dsimulation
#' @aliases run1dsimulation
#' @title Non-executable auxiliary function
#' @usage run1dsimulation(rriskModel)
#' @param rriskModel ...
#' @keywords run
#' @export


run1dsimulation<-function(rriskModel){
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(rriskModel))
  
  #-----------------------------------------------------------------------------
  # reset simulation results
  #-----------------------------------------------------------------------------
  rriskModel<-resetSimulationResults(rriskModel)
  
  #-----------------------------------------------------------------------------
  # resample strv of stra-Items
  #-----------------------------------------------------------------------------
  rriskModel<-resampleSTRATA(rriskModel)
  
  #-----------------------------------------------------------------------------
  # compute some help variables
  #-----------------------------------------------------------------------------
  items<-rriskModel@items@items
  
  #-------------------------------------------------------------------------------
  # get OF items
  #-------------------------------------------------------------------------------
  availableOFs<-c()
  if(length(items)>0){
    for(i in 1:length(items)){
      #-------------------------------------------------------------------------
      # collect outcome- and remain items
      #-------------------------------------------------------------------------
      if(items[[i]]@rolecode=="OF"){
        availableOFs<-c(availableOFs,items[[i]]@name)
      }
    } # end for loop
    #-----------------------------------------------------------------------------
    # breche ab falls die Anzahl von Outcome-Items null ist
    #-----------------------------------------------------------------------------
    if(length(availableOFs)==0){
      stop("At least one outcome function (OF) needs to be defined before the model can be run!\nThe current model does not contain any 'OF' item.",call.=FALSE)
    } # end if(length(availableOFs)==0){
  } else {
    stop("Current model cannot be run, the list of model items is empty!",call.=FALSE)
  } # end if(length(items)>0)

  #-------------------------------------------------------------------------------
  # run 1d simulation
  #-------------------------------------------------------------------------------
  fullout.1d<-list()
  relaxout.1d<-list()
  iterat<-rriskModel@settings@N

  start.clock1d <- proc.time()[3]    # start clock

  cat("\n*************************************************************************\n")
  cat("Begin 1d simulation...\n")
  cat("*************************************************************************\n")

  cat("\n-----------------------------------------------------------------------\n")
  cat("Computing full command values")
  cat("\n-----------------------------------------------------------------------\n")
  fullout.1d<-get.dataForWith(item=new("itemClass"),rriskModel,catResults=TRUE,fullc=TRUE,run=TRUE)

  cat("\n-----------------------------------------------------------------------\n")
  cat("Computing relax command values")
  cat("\n-----------------------------------------------------------------------\n")
  relaxout.1d<-get.dataForWith(item=new("itemClass"),rriskModel,catResults=TRUE,fullc=FALSE,run=TRUE)

  #-----------------------------------------------------------------------------
  # OF.q collects statistics of all outcome function items for display
  # it has 13 rows for quantiles and cols equal to number of OF items
  #-----------------------------------------------------------------------------
  OF1d.q <- as.data.frame(matrix(nrow=13,ncol=length(availableOFs),data=NA))
  rownames(OF1d.q) <- c("Qt0.01","Qt0.1","Qt1.0","Qt2.5","Qt5.0","Qt10","Qt50","Qt90","Qt95","Qt97.5","Qt99","Qt99.9","Qt99.99")   ##"Type","Part", have been taken out here

  for(i in 1:length(availableOFs)){
    availableOFs.temp<-availableOFs[[i]]
    fullout.1d.OFtemp<-fullout.1d[[availableOFs.temp]]
    OF1d.q[1:nrow(OF1d.q),i] <- signif(quantile(fullout.1d.OFtemp,c(0.0001,0.001,0.01,0.025,0.05,0.1,0.5,0.9,0.95,0.975,0.99,0.999,0.9999),na.rm=TRUE,type=7),digits=4)
    if(iterat < 50000){
      OF1d.q[nrow(OF1d.q),i] <- NA
      if(iterat<40001)    OF1d.q[1,i] <- NA
      if(iterat<5000)     OF1d.q[12,i] <- NA
      if(iterat<4001)     OF1d.q[2,i] <- NA
      if(iterat<500)      OF1d.q[11,i] <- NA
      if(iterat<401)      OF1d.q[3,i] <- NA
      if(iterat<200)      OF1d.q[10,i] <- NA
      if(iterat<161)      OF1d.q[4,i] <- NA
      if(iterat<100)      OF1d.q[9,i] <- NA
      if(iterat<81)       OF1d.q[5,i] <- NA
      if(iterat<50)       OF1d.q[8,i] <- NA
      if(iterat<41)       OF1d.q[6,i] <- NA
      if(iterat<10)       OF1d.q[5,i] <- NA
    } # end if(iterat < 50000){
    if(i==length(availableOFs))  colnames(OF1d.q)<-availableOFs
  } # end for

  #-------------------------------------------------------------------------------
  # save 1d simualtion results into the model object
  #-------------------------------------------------------------------------------
  rriskModel@output@fullout.1d<-fullout.1d
  rriskModel@output@relaxout.1d<-relaxout.1d
  rriskModel@output@summaries<-OF1d.q

  #-----------------------------------------------------------------------------
  # speichere runtime
  #-----------------------------------------------------------------------------
  runtime1d<-proc.time()[3]-start.clock1d
  rriskModel@output@runtime1d<-runtime1d

  #-----------------------------------------------------------------------------
  # Zeige Ergebnisse der 1d Simulation
  #-----------------------------------------------------------------------------
  cat("---------------------------------------------------------------------------\n")
  cat("Quantiles of the simulated outcome function of the model\n")
  cat("---------------------------------------------------------------------------\n")
  print(rriskModel@output@summaries)
    cat("\nThe quantiles are calculated using the empirical probabilities
    p(k)=(k-1)/(n-1) (n ordered data x_1, ..., x_k, ..., x_ n).
    Type 'help(quantiles)' for further information.
    Quantiles having less than 5 data points below are stated with NA to represent their inaccuracy.")
    cat("\n\n")

  #-------------------------------------------------------------------------------
  # create convergence plot(s)
  #-------------------------------------------------------------------------------
  cat("Plotting convergence diagramm(s) of the outcome function(s)...\n")
  plotOFConvergence(rriskModel)

  #-------------------------------------------------------------------------------
  # create histogram of outcome function(s)
  #-------------------------------------------------------------------------------
  cat("Plotting histogram(s) of the outcome function(s)...\n\n")
  plotOFHistogram(rriskModel)

  cat("*************************************************************************\n")
  cat("End 1d simulation...")
  cat("run time of 1d simulation is ",runtime1d," seconds\n",sep="")
  cat("*************************************************************************\n")
  
  #-----------------------------------------------------------------------------
  # save evaluated item results in data-Slot (if possible)
  #-----------------------------------------------------------------------------
  if(length(fullout.1d)>0){
    for(i in 1:length(rriskModel@items@items)){
      itemname<-rriskModel@items@items[[i]]@name
      dataslot<-rriskModel@items@items[[i]]@data
      if(is.element(itemname,names(fullout.1d))){
        fullout.1d.item<-fullout.1d[[itemname]]
        if(rriskModel@items@items[[i]]@typecode=="numv"){
          dataslot<-fullout.1d.item
        } else if(is.element(rriskModel@items@items[[i]]@typecode,c("fnrv","rsrv"))){
          fullout.1d.item<-data.frame(fullout.1d.item)
          colnames(fullout.1d.item)<-itemname
          dataslot<-summary(fullout.1d.item)
        } else if(rriskModel@items@items[[i]]@typecode=="mcrv"){
          fullout.1d.item<-data.frame(fullout.1d.item)
          if(length(fullout.1d.item)==1){
            colnames(fullout.1d.item)<-itemname
          } 
          dataslot<-summary(fullout.1d.item)
        } else if(rriskModel@items@items[[i]]@typecode=="bsrv"){
          dataslot<-summary(fullout.1d.item)
        }
        #-----------------------------------------------------------------------
        # aktualisiere data-Slot
        #-----------------------------------------------------------------------
        rriskModel@items@items[[i]]@data<-dataslot  
      } # end if(is.element(itemname,names(fullout.1d))){  
    } # end for
  } # end if

} # end of function run1dsimulation




################################################################################
################################################################################
#' @name resetSimulationResults
#' @aliases resetSimulationResults
#' @title Non-executable auxiliary function
#' @usage resetSimulationResults(rriskModel)
#' @param rriskModel ...
#' @keywords run
#' @export

resetSimulationResults<-function(rriskModel){
  cat("\nReset simulation results...\n\n")
  rriskModel@output@fullout.1d<-list()
  rriskModel@output@relaxout.1d<-list()
  rriskModel@output@runtime1d<-NULL
  rriskModel@output@fullout.2d<-c()
  rriskModel@output@uncitems.2d<-c()
  rriskModel@output@OFname.2d<-""
  rriskModel@output@summaries<-c()
  rriskModel@output@OFcdfCI<-c()
  rriskModel@output@runtime2d<-NULL
  return(rriskModel)
}  # end of function resetSimulationResults()
 

################################################################################
################################################################################
#' @description A function that runs multidimensional simulations on the \code{\linkS4class{modelClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name run
#' @aliases run
#' @title Function that runs multidimensional simulations on the 'modelClass'
#' @usage run(rriskModel,sim.2d=TRUE,menuLevel=1)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param sim.2d decides whether a 2-dimensional or 1-dimensional simulation to run
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @keywords run
#' @export

run<-function(rriskModel,sim.2d=TRUE,menuLevel=1)
{ #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  on.exit(return(invisible(rriskModel)))
  
  #-------------------------------------------------------------------------------
  # check number of items
  #-------------------------------------------------------------------------------
  if(length(rriskModel@items@items)==0){
    stop("Current model cannot be run, the list of model items is empty!",call.=FALSE)
  }
  
  #-----------------------------------------------------------------------------
  # executing 2d simulation
  #-----------------------------------------------------------------------------
  if(sim.2d==FALSE){
    #-----------------------------------------------------------------------------
    # executing 1d simulation
    #-----------------------------------------------------------------------------
    rriskModel<-run1dsimulation(rriskModel) 
  } else if(sim.2d==TRUE){
    #-----------------------------------------------------------------------------
    # executing 2d simulation
    #-----------------------------------------------------------------------------
    rriskModel<-run2dsimulation(rriskModel,menuLevel=menuLevel+1) 
  }

} # end of function run()




################################################################################
################################################################################   
#' @description A function that creates a cumulative distribution of the main outcome function based on 2d-simulation.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotCDF
#' @aliases plotCDF
#' @title Function that draws a cumulative distribution of the main outcome function based on 2d-simulation
#' @usage plotCDF(rriskModel,pdfGraph=FALSE)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the plot is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotCDF(rriskModel) }


plotCDF<-function(rriskModel,pdfGraph=FALSE){
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  pdffilenames<-c()
  on.exit(return(pdffilenames))

  if(length(rriskModel@items@items)>0){

    OFitems<-c()
    OFunits<-c()

    #---------------------------------------------------------------------------
    # get OF items
    #---------------------------------------------------------------------------
    for(i in 1:length(rriskModel@items@items)){
      if(rriskModel@items@items[[i]]@rolecode=="OF"){
        OFitems<-c(OFitems,rriskModel@items@items[[i]]@name)
        OFunits<-c(OFunits,rriskModel@items@items[[i]]@unit)
      } # end  if(rriskModel@items@items[[i]]@rolecode=="OF"){
    } # end for

    #---------------------------------------------------------------------------
    # create cdf plot of each OF item
    #---------------------------------------------------------------------------
    if(length(OFitems)>0) {
        for(i in 1:length(OFitems)){
          if(rriskModel@output@OFname.2d==OFitems[i]){ # cdf plot for 2d simulations
            OF.resampled <- sample(t(rriskModel@output@fullout.2d),1000,replace=TRUE)
          } else { # cdf plot for 2d simulations
            OF.resampled <-sample(rriskModel@output@fullout.1d[OFitems[i]][[1]],1000,replace=TRUE)
          } # end   if(rriskModel@output@OFname.2d==OFitems[i]){
          xlab<-OFitems[i]
          OFunit<-OFunits[which(OFitems==xlab)]
          xlab <- paste(xlab," [",OFunit,"]",sep="")

          #---------------------------------------------------------------------
          # open graph device
          #---------------------------------------------------------------------
          if (pdfGraph == FALSE){
            X11(width=8,height=9)
          } else {
            pdffilenames.temp<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_cdf_",OFitems[i],sep="")
            pdffilenames<-c(pdffilenames,pdffilenames.temp) 
            pdf(file=paste(pdffilenames.temp,".pdf",sep=""),width=8,height=9)
          } # end if (pdfGraph == FALSE){

          #---------------------------------------------------------------------
          # define graph reference for model report
          #---------------------------------------------------------------------
          graphRef<-gsub(x=rriskModel@name@name," ",replacement="")
          graphRef<-paste("\\ref{fig:",graphRef,"_cdf_", OFitems[i],"}",sep="")
          graphName <- paste("cdf graph of the outcome function", OFitems[i], sep = " ")
          suppressWarnings(text <- ifelse(pdfGraph, "", paste(graphName, "\n", graphRef, sep = "")))

          #---------------------------------------------------------------------
          # create plot
          #---------------------------------------------------------------------
          suppressWarnings(plot(ecdf(OF.resampled),main=text,
              ylab=paste("cdf(",OFitems[i],")",sep=""),
              xlab=xlab,do.points=FALSE,verticals=TRUE,lty=2))
          if (rriskModel@output@OFname.2d==OFitems[i]){ # cdf plot for 2d simulations
            for (j in 1:101){
              suppressWarnings(lines(rriskModel@output@OFcdfCI[,j],rep((j-1)/100,2),col=rriskModel@settings@mycol,lend=1,lwd=5))
            } # end for
            suppressWarnings(lines(ecdf(OF.resampled),pch="."))
          } # end if (rriskModel@output@OFname.2d==OFitems[i]){
        } # end for(i in 1:length(OFitems)){
       if (pdfGraph == TRUE) graphics.off()
    } else {
      ("There are no outcome functions in the model.\n")
    } # if(length(OFitems)>0) {
  }  else {
    cat( "No convergence plot can be created, the list of model items is empty!\n" )
  } # end if(length(rriskModel@items@items)>0){
} # end of function plotCDF()



################################################################################
################################################################################
#' @description A function that creates LaTeX codes to import a cdf plot for the outcome items.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.cdf
#' @aliases tolatexGraphs.cdf
#' @title Function that creates LaTeX codes to import a cdf plot for the outcome items
#' @usage tolatexGraphs.cdf(rriskModel,file.name)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param file.name is the name of the TeX file into which the codes are to be written 
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' tolatexGraphs.cdf(rriskModel,file.name="text.tex") }

tolatexGraphs.cdf<-function(rriskModel,file.name){
   try.result<-try(plotCDF(rriskModel,pdfGraph=TRUE),silent=TRUE)
   if (!inherits(try.result, "try-error")){
    if(length(try.result)>0){ # also wenn OF-Items >0
      #-------------------------------------------------------------------------
      # get names of OF-Items
      #-------------------------------------------------------------------------
      OFnames<-as.matrix(try.result,ncol=1)
      OFnames<-apply(OFnames,1,function(x){
                                            output<-strsplit(x,split="_")[[1]]
                                            output<-output[length(output)]})
      #-----------------------------------------------------------------------------
      # create LatexCode
      #-----------------------------------------------------------------------------
      for(i in 1:length(OFnames)){
        cdfName<-paste(try.result[i],".pdf",sep="")
        if(rriskModel@output@OFname.2d!=""){
          cdfCaption<-paste("Cumulative distribution function (cdf) of the main outcome function '",OFnames[i],"' based on 2d-simulation
              (with superimposed 95\\% unvertainty intervals for the quantiles using 1000 and 50 iterations for 1st (variability)
              and 2nd (uncertainty) order simulation, respectively",sep="")
        } else {
          cdfCaption<-paste("Cumulative distribution function (cdf) of the main outcome function '",OFnames[i],"' based on 1d-simulation",sep="")
        }
 cat("
\\begin{figure}[p]
\\centering",append=TRUE,file = file.name)
cat(paste("\n\\includegraphics[width=1\\textwidth]{",cdfName,"}",sep=""),append=TRUE,file = file.name)
cat("\n\\caption{",cdfCaption,"}",append=TRUE,file = file.name)
cat(paste("\n\\label{fig:",try.result[i],"}
\\end{figure}
\\clearpage",sep=""),append=TRUE,file = file.name)
        } # end for schleife
    } else {
      cat("Cumulative distribution of the outcome function be created, there is no 'OF' item(s) defined in the model!\n")
    }# end if(length(try.result)>0){
   } else {
    cat("Cumulative distribution of the outcome function cannot be created!\n")
   }# end if (!inherits(try.result, "try-error")){
} # end of function tolatexGraphs.cdf





################################################################################
################################################################################
#' @description A function that creates a traffic lights plot for the outcome items.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotTrafficLights
#' @aliases plotTrafficLights
#' @title Function that creates a traffic lights plot for the outcome items
#' @usage plotTrafficLights(rriskModel,pdfGraph=FALSE)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the plot is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotTrafficLights(rriskModel,useNotApplicable=TRUE, pdfGraph=FALSE)}

plotTrafficLights <- function(rriskModel,pdfGraph=FALSE) {
  
  useNotApplicable<-rriskModel@settings@usenotapplicable
  mcrv.unc.items<-c()  
  numbers <- c()

  if(length(rriskModel@items@items)>0) {
    for(i in 1:length(rriskModel@items@items)) {
      if(rriskModel@items@items[[i]]@typecode=="mcrv") {
        if(rriskModel@items@items[[i]]@rolecode=="u" | rriskModel@items@items[[i]]@rolecode=="uv") {
          mcrv.unc.items<-c(mcrv.unc.items,rriskModel@items@items[[i]]@name)
		      numbers <- c(numbers, i)
        }
      }
    }
  }

  unc.matrix <- c()
  for (i in c(1:length(numbers))) {
  	unc.matrix <- rbind(unc.matrix, rriskModel@items@items[[numbers[i]]]@scores)
  }
  # unc.matrix <- unc.matrix[,c(1, 2, 3, 4, 6, 8)]
  colnames(unc.matrix) <- c("(U1)","(U2)","(U3)","(U4)","(U5)","(U6)","(K1)","(K2)","(K3)","(K4)")
  rownames(unc.matrix) <- mcrv.unc.items

  uncertMatrix <- unc.matrix
  modelSsystem <- rriskModel@scoring
  graphName <- paste(gsub(x=rriskModel@name@name," ",replacement=""),"_Sensitivityeffects}",sep="")
  graphRef  <- paste("\\ref{fig:",graphName, sep = "")
  maintext <- ifelse(pdfGraph, "", paste(graphName, "\n", graphRef))

  modelName=NULL

  output<-list(removed=c(),values=c(),vcolors=c(),vmeanings=c())
  
  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  scoreValues<-modelSsystem@values
  vcolors<-modelSsystem@vcolors
  vmeanings<-modelSsystem@vmeanings
  
  #-----------------------------------------------------------------------------
  #remove columns with all not-applicable entries
  #-----------------------------------------------------------------------------
  notapplicable<-vmeanings[which(names(vmeanings)=="notapplicable")]
  if(!useNotApplicable)
  { for(i in 1:ncol(uncertMatrix)) {
      if(i==1) colToRemove<-c()
      if(all(uncertMatrix[,i]==notapplicable)) colToRemove<-c(colToRemove,i)    
    }
    if (length(colToRemove) != 0) {
      # output$removed=colnames(uncertMatrix)[colToRemove]
      output$removed=colToRemove
      uncertMatrix<-uncertMatrix[,-colToRemove]
        
      scoreValues<-setdiff(scoreValues,notapplicable)
      vcolors<-vcolors[which(vcolors!=notapplicable)]
      vmeanings<-vmeanings[which(vmeanings!=notapplicable)]
    }
    #scoreValues<-scoreValues[-1]
    #vcolors<-vcolors[-1]
    #vmeanings<-vmeanings[-1]
  }
  
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
  for(i in 1:length(vcolors))
  { colVector[which(matVector==vcolors[i])]<-names(vcolors)[i]
  } 
  
  #-----------------------------------------------------------------------------
  # help computations
  #-----------------------------------------------------------------------------
  n.col <- ncol(uncertMatrix)
  if(length(min) == 1) min <- rep(min,n.col)
  if(length(max) == 1) max <- rep(max,n.col)
  if(length(min) != n.col | length(max) != n.col) stop("min or max incorrect length")
  z <- uncertMatrix
  for (j in 1:n.col)
  { b <- lm(c(1,20)~c(min[j],max[j]))$coeff
    z[,j] <- round(b[[1]] + b[[2]]*z[,j])
  }
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
  if(pdfGraph==FALSE)
  { X11(width=ncol(uncertMatrix)+1)
  } else
  { file.name=gsub(x=maintext," ",replacement="")
    file.name<-paste(file.name,"pdf",sep=".")
    file.name<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_Sensitivityeffects",file.name,sep="")
    pdf(file=file.name,width=ncol(uncertMatrix)+1)
  }
  par(mfrow=c(1,1),mai=c(1,1,ifelse(pdfGraph, 1.2, 2),1)-.9,oma=c(0,0,0,0))
  suppressWarnings(plot(c(1,max.y)~c(1,max.x), ylim=c(0,max.y),
    main=maintext,type = "n",axes=FALSE,xlab="",ylab=""))
  for (i in 1:r){
    suppressWarnings(lines(x=c(1.2,c),y=c(i,i),lty="dotted"))
  } # end for-loop
  suppressWarnings(points(y~x,cex=5,pch=21,col=colVector,bg=colVector))
  suppressWarnings(text(x=(1:c)+.2,y=rep(max.y,c),labels=colnames(uncertMatrix)))
  suppressWarnings(text(x=rep(max.x-2,r),y=r:1,labels=rownames(uncertMatrix),adj=c(0,NA)))
  suppressWarnings(lines(c(max.y,max.y)-0.5~c(0.8,max.x)))   # top line
  suppressWarnings(lines(c(0.5,0.5)~c(0.8,max.x)))           # bottom line
  suppressWarnings(lines(c(max.y-0.5,0.5)~rep(0.8,2)))     # left
  suppressWarnings(lines(c(max.y-0.5,0.5)~rep(max.x,2)))     # right
  legend(min(y),0.3,pch=21,col=names(vcolors),
    pt.bg=names(vcolors),text.width=1.5,x.intersp=2,
    horiz=TRUE,pt.cex=4,bty="n",
    legend=names(vmeanings),border=FALSE)
  if(pdfGraph==TRUE) dev.off()
 
  return(mcrv.unc.items)
} # end of function plotTrafficLights



################################################################################
################################################################################
#' @description A function that creates LaTeX codes to import a trafficlights plot for the outcom items.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.trafficLights
#' @aliases tolatexGraphs.trafficLights
#' @title Function that creates LaTeX codes to import a trafficLights plot for the outcome items
#' @usage tolatexGraphs.trafficLights(rriskModel,file.name)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param file.name is the name of the TeX file into which the codes are to be written 
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' tolatexGraphs.trafficLights(rriskModel,file.name="text.tex")}

tolatexGraphs.trafficLights<-function(rriskModel,file.name) {
  try.result<-try(mcrv.unc.items<-plotTrafficLights(rriskModel,pdfGraph=TRUE),silent=TRUE)
  name <- paste(gsub(x=rriskModel@name@name," ",replacement=""),"_Sensitivityeffects",sep="")
  if (!inherits(try.result, "try-error")) {
    #-----------------------------------------------------------------------------
    # create LatexCode
    #-----------------------------------------------------------------------------
    if(length(mcrv.unc.items)>0) {
    
      captionGraph <- c()
      inames <- c()
      modelSsystem <- rriskModel@scoring
        for(k in 1:length(modelSsystem@scoring))
       
        { if(k==1) uncertText<-""
          temp<-modelSsystem@scoring[[k]]
          uncertTextTemp<-paste(" ",tolower(temp@name)," (",temp@notation,")",sep="")
          inames[k] <- temp@notation
          if(k!=length(modelSsystem@scoring))
          { uncertTextTemp<-paste(uncertTextTemp,",",sep="")
          }else  uncertTextTemp<-paste(uncertTextTemp,".",sep="")
          uncertText<-paste(uncertText,uncertTextTemp,sep="") 
        }
        captionGraph<-paste(captionGraph,uncertText)
        # captionGraph<-paste(captionGraph,"The qualitative scores are represented as dots using a traffic light colour scheme to signal")
      
      removed <- mcrv.unc.items$removed
      removed <- inames[removed]
      if (length(removed) != 0) {
        li <- c()
        for (i in c(1:length(removed))) {
          li <- paste(li, removed[i]) 
        }
        removed <- paste("No dots were plotted if the qualitative assessment was 'not applicable', resulting in the omission of columns for some criteria (", li, ")", sep = "")
      }   

 cat("\n
\\begin{figure}[p]
\\centering",append=TRUE,file = file.name)
cat(paste("\n\\includegraphics[width=1\\textwidth]{", name,".pdf}",sep=""),append=TRUE,file = file.name)
cat("\n\\caption{
Uncertainty, knowledge base and effect of uncertain model items represented as dots using a traffic light 
colour scheme to signal non-critical (green) or critical (red) assessments and effects. 
Left part: qualitative assessment of ", captionGraph ,"
right part: effect on outcome function in the full and relaxed model.", removed, "}",append=TRUE,file = file.name)
cat(paste("\n\\label{fig:",name,"}
\\end{figure}
\\clearpage
",sep=""),append=TRUE,file = file.name)

    } else cat("Traffic lights plots can't be created because quality assessment not available.")
  } # end if 
} # end of function tolatexGraphs.trafficLights() 



################################################################################
################################################################################   
#' @description A function that creates nonparamtric regression plots for the outcome items.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotGAM
#' @aliases plotGAM
#' @title Function that draws nonparamtric regression plots for the outcome items
#' @usage plotGAM(rriskModel,pdfGraph=FALSE)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the plot is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotGAM(rriskModel,pdfGraph=FALSE)}

plotGAM<-function(rriskModel,pdfGraph=FALSE)
{ mcrv.unc.items<-c()
  on.exit(return(invisible(mcrv.unc.items)))
  if(rriskModel@output@OFname.2d!="")
  {
   #-----------------------------------------------------------------------------
    # identify available OF items and available uncert (mcrv and 'u', 'uv') mcrv items
    #-----------------------------------------------------------------------------
    if(length(rriskModel@items@items)>0)
    { for(i in 1:length(rriskModel@items@items))
      { if(rriskModel@items@items[[i]]@typecode=="mcrv")
        { if(rriskModel@items@items[[i]]@rolecode=="u" | rriskModel@items@items[[i]]@rolecode=="uv")
          { mcrv.unc.items<-c(mcrv.unc.items,rriskModel@items@items[[i]]@name)
          }
        }                                                                       
      }
    } else stop("Regression tree plot cannot be created, the list of model items is empty!",call.=FALSE)
    #-----------------------------------------------------------------------------
    # define help functions
    #-----------------------------------------------------------------------------
    npreg<-function(y,x,xlab,ylab,mainText,col)
    { ylab<-paste("Smoothed effect on the outcome function ",ylab,sep="")
      plot(gam(y~s(x)),main=mainText,shade=TRUE,xlab=xlab,ylab=ylab,residuals=FALSE,rug=FALSE,shade.col=col)
    }

    for(i in 1:length(mcrv.unc.items))
    { item.temp<-mcrv.unc.items[i]
      if(pdfGraph==FALSE)
      { X11(width=8,height=9)
        #par(mai=c(1,1,1.5,1))
        graphRef <- paste("\\ref{fig:", gsub(x=rriskModel@name@name," ",replacement=""),"_npreg_",rriskModel@output@OFname.2d,item.temp, "} \n", sep="")
        # mainText=paste("Regression tree of the outcome item '",mcrv.unc.items[i],"'",sep="")
      } else
      { # mainText="Regression tree"
        pdf(file=paste(gsub(x=rriskModel@name@name," ",replacement=""),"_npreg_",rriskModel@output@OFname.2d,item.temp,".pdf",sep=""),width=8,height=9)
        graphRef <- ""
      }
      #-----------------------------------------------------------------------------
      # non-parametric regression for 1d simulation
      #-----------------------------------------------------------------------------
      par(mfrow=c(2,1), mai = c(1, 1, 1, 1))
      x.1d<-rriskModel@output@fullout.1d[[item.temp]]
      y.1d<-rriskModel@output@fullout.1d[[rriskModel@output@OFname.2d]]
      suppressWarnings(npreg(y.1d,x.1d,xlab=item.temp,ylab="P",mainText=paste(ifelse(pdfGraph, "", graphRef), "1st order simulation", sep = ""),col=rriskModel@settings@mycol))
      suppressWarnings(abline(h=0,lty=2))
      #-----------------------------------------------------------------------------
      # non-parametric regression for 2d simulation
      #-----------------------------------------------------------------------------
      y.2d<-as.numeric(t(rriskModel@output@fullout.2d))
      x.2d<-rriskModel@output@uncitems.2d[,item.temp]
      x.2d<-as.numeric(rep(x.2d,rriskModel@settings@N))
      suppressWarnings(npreg(y.2d,x.2d,xlab=item.temp,ylab="P",mainText="2nd order simulation",col=rriskModel@settings@mycol))
      suppressWarnings(mtext("AA", side = 3, outer = TRUE))
      suppressWarnings(abline(h=0,lty=2))
      if(pdfGraph==TRUE) dev.off() 
    }
  } else cat("Non-parametric regression plots are omitted because 2d simulation was not conducted.\n")
} # end of function plotGAM()



################################################################################
################################################################################
#' @description A function that creates LaTeX codes to import nonparamtric regression plots for the outcom items.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.gam
#' @aliases tolatexGraphs.gam
#' @title Function that creates LaTeX codes to import nonparamtric regression plots for the outcome items
#' @usage tolatexGraphs.gam(rriskModel,file.name)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param file.name is the name of the TeX file into which the codes are to be written 
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' tolatexGraphs.gam(rriskModel,file.name="text.tex")}

tolatexGraphs.gam<-function(rriskModel,file.name)
{ if(rriskModel@output@OFname.2d!="")
  {
      try.result<-try(mcrv.unc.items<-plotGAM(rriskModel,pdfGraph=TRUE),silent=TRUE)
      if (!inherits(try.result, "try-error"))
      { #-----------------------------------------------------------------------------
        # create LatexCode
        #-----------------------------------------------------------------------------
        if(length(mcrv.unc.items)>0)
        { for(i in 1:length(mcrv.unc.items))
          { gamName<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_npreg_",rriskModel@output@OFname.2d,mcrv.unc.items[i],sep="")
            gamCaption<-paste("Nonparametric regression using generalized additive models (GAM)
            of the outcome function and uncertain \\tmcrv-items according to the full model.",sep="")
 cat("\n
\\begin{figure}[p]
\\centering",append=TRUE,file = file.name)
cat(paste("\n\\includegraphics[width=1\\textwidth]{",gamName,"}",sep=""),append=TRUE,file = file.name)
cat("\n \\caption{",gamCaption,"}",append=TRUE,file = file.name)
cat(paste("\n\\label{fig:",gamName,"}", sep = ""), append = TRUE, file = file.name)
cat(paste("
\\end{figure}
\\clearpage
",sep=""),append=TRUE,file = file.name)
         } # end for
        } # end if
      } # end if
    } else cat("Non-parametric regression plots are omitted because 2d simulation was not conducted.\n")  
} # end of function tolatexGraphs.gam() 




################################################################################
################################################################################
#' @description A function that creates a regression tree plot.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotTree
#' @aliases plotTree
#' @title Function that draws a regression tree plot
#' @usage plotTree(rriskModel,pdfGraph=FALSE)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the plot is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotTree(rriskModel,pdfGraph=FALSE)  }

plotTree<-function(rriskModel,pdfGraph=FALSE)
{ #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  pdffilenames<-c()
  on.exit(return(pdffilenames))
   
  #-----------------------------------------------------------------------------
  # identify available mcrv items, available OF items and available uncert (mcrv and 'u', 'uv') items
  #-----------------------------------------------------------------------------
  OF.items<-c()
  mcrv.items<-c()
  unc.items<-c()
  
  #-----------------------------------------------------------------------------
  # loop over all model items
  #-----------------------------------------------------------------------------
  if(length(rriskModel@items@items)>0){
    for(i in 1:length(rriskModel@items@items)){
      #-------------------------------------------------------------------------
      # get all OF items
      #-------------------------------------------------------------------------
      if(rriskModel@items@items[[i]]@rolecode=="OF"){
        OF.items<-c(OF.items,rriskModel@items@items[[i]]@name)
      } # end if(rriskModel@items@items[[i]]@rolecode=="OF"){
      #-------------------------------------------------------------------------
      # get all mcrv items of type "uv" and "u"
      #-------------------------------------------------------------------------
      if(rriskModel@items@items[[i]]@typecode=="mcrv" & rriskModel@items@items[[i]]@stratum==""){
        if(!is.null(rriskModel@items@items[[i]]@data) & rriskModel@items@items[[i]]@fullc!=""){
          mcrv.items<-c(mcrv.items,rriskModel@items@items[[i]]@name)
          if(rriskModel@items@items[[i]]@rolecode=="u" | rriskModel@items@items[[i]]@rolecode=="uv"){
            unc.items<-c(unc.items,rriskModel@items@items[[i]]@name)
          } # end if(rriskModel@items@items[[i]]@rolecode=="u" | rriskModel@items@items[[i]]@rolecode=="uv"){
        } else {
          cat(paste("Not evaluated mcrv item '",rriskModel@items@items[[i]]@name,"' will be ignored.\n",sep=""))
        }
      } else if(rriskModel@items@items[[i]]@typecode=="mcrv" & rriskModel@items@items[[i]]@stratum!="") {
        cat(paste("Stratified mcrv item '",rriskModel@items@items[[i]]@name,"' will be ignored.\n",sep=""))
      }# end if(rriskModel@items@items[[i]]@typecode=="mcrv"){
    } # end for
    
    if(length(OF.items)==0){
      stop("Regression tree plot cannot be created, there is no 'OF' item defined in the model!",call.=FALSE)
    }
    
    if(length(mcrv.items)==0){
      stop("Regression tree plot cannot be created, there is no uncertainty mcrv item(s) defined in the model!",call.=FALSE)
    }
    
  } else {
    stop("Regression tree plot cannot be created, the list of model items is empty!",call.=FALSE)
  } # end if(length(rriskModel@items@items)>0){
  #-----------------------------------------------------------------------------
  # evaluate data for tornado charts
  #-----------------------------------------------------------------------------
  if(length(rriskModel@output@fullout.1d)==0 | length(rriskModel@output@relaxout.1d)==0){
    stop("Regression tree plot cannot be created, the list of 1d full and/or relax model simulation results is empty!",call.=FALSE)
  } else {
    #--------------------------------------------------------------------------
    # get data of 1d siomulation for full model
    #--------------------------------------------------------------------------
    #fullout.1d<-as.data.frame(rriskModel@output@fullout.1d)
    #fullout.1d<-fullout.1d[,which(names(fullout.1d) %in% c(mcrv.items,OF.items))]
     
    fullout.1d<-rriskModel@output@fullout.1d
    fullout.1d<-fullout.1d[c(mcrv.items,OF.items)]
    fullout.1d<-as.data.frame(fullout.1d)
     
    #--------------------------------------------------------------------------
    # get data of 1d simulation for relaxed model
    #--------------------------------------------------------------------------
    #relaxout.1d<-as.data.frame(rriskModel@output@relaxout.1d)
    #relaxout.1d<-relaxout.1d[,which(names(relaxout.1d) %in% c(mcrv.items,OF.items))]
     
    relaxout.1d<-rriskModel@output@relaxout.1d
    relaxout.1d<-relaxout.1d[c(mcrv.items,OF.items)]
    relaxout.1d<-as.data.frame(relaxout.1d)
     
    #--------------------------------------------------------------------------
    # Create tornado plot for each 'OF' item
    #--------------------------------------------------------------------------    
    for(i in 1:length(OF.items)){
      OF <- OF.items[i]
     
      #OF.index<-which(colnames(fullout.1d)==OF)
      OF.full<-fullout.1d[,OF]
      indexToRemove<-which(is.element(names(fullout.1d),OF.items))
      #data.full<-fullout.1d[,-indexToRemove]
      data.full<-as.data.frame(fullout.1d[,-indexToRemove])
      names(data.full)<-names(fullout.1d)[-indexToRemove]
     
      OF.relax<-relaxout.1d[,OF]
      indexToRemove<-which(is.element(names(relaxout.1d),OF.items))
      #data.relax<-relaxout.1d[,-indexToRemove]
      data.relax<-as.data.frame(relaxout.1d[,-indexToRemove])
      names(data.relax)<-names(relaxout.1d)[-indexToRemove]
      
      #-------------------------------------------------------------------------
      #### plot CART tree
      #-------------------------------------------------------------------------
      if(pdfGraph==FALSE){
        X11(width=12,height=15)
        mainText=paste("Regression tree of the outcome item '",OF,"'",sep="")
        graphRef <- paste("\\ref{fig:", gsub(x=rriskModel@name@name," ",replacement=""),"_tree_",OF,"}",sep="")
        mainText <- paste(mainText, "\n" ,graphRef)
      } else { 
        pdffilenames.temp<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_tree_",OF,sep="")
        pdffilenames<-c(pdffilenames,pdffilenames.temp)
        pdf(file=paste(pdffilenames.temp,".pdf",sep=""),height=8,width=8)
        mainText=paste("\n Regression tree of the outcome item '",OF,"'",sep="")
      } # end if(pdfGraph==FALSE){
  
      par(mfrow=c(2,1),oma=c(0,0,ifelse(pdfGraph,2, 4),0))#,omi=c(1,1,2,1))
      full.set <- round(as.data.frame(cbind(OF.full,data.full)),digits=2)
      colnames(full.set)[which(names(full.set)=="OF.full")]<-OF
      f <- paste(OF,"~",paste(colnames(data.full),collapse="+"))
      full.tree <- tree(formula=f, data=full.set)
      full.sum <- summary(full.tree)
      suppressWarnings(plot(full.tree))
      suppressWarnings(title("Full model"))
      suppressWarnings(text(full.tree))
    
      relax.set <- round(as.data.frame(cbind(OF.relax,data.relax)),digits=2)
      colnames(relax.set)[which(names(relax.set)=="OF.relax")]<-OF
      #colnames(relax.set) <- c(OF,colnames(data.relax))
      f <- paste(OF,"~",paste(colnames(data.relax),collapse="+"))
      relax.tree <- tree(formula=f, data=relax.set)
      relax.sum <- summary(relax.tree)
      suppressWarnings(plot(relax.tree))
      suppressWarnings(title("Relaxed model"))
      suppressWarnings(text(relax.tree))
        
      suppressWarnings(mtext(mainText,side=3,outer=TRUE,cex=1.4))
        
      #-----------------------------------------------------------------------
      # close graph device
      #-----------------------------------------------------------------------
      if(pdfGraph==TRUE) dev.off()  
     }  # end for schleife
   }  # end for
} # end of function plotTree()



################################################################################
################################################################################
#' @description A function that creates LaTeX codes to include a regression tree plot.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.tree
#' @aliases tolatexGraphs.tree
#' @title Function that creates LaTeX codes to include a regression tree plot
#' @usage tolatexGraphs.tree(rriskModel,file.name)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param file.name is the name of the TeX file into which the codes are to be written 
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' tolatexGraphs.tree(rriskModel,file.name="text.tex") }

tolatexGraphs.tree<-function(rriskModel,file.name)
{  try.result<-try(plotTree(rriskModel,pdfGraph=TRUE),silent=TRUE)
   if (!inherits(try.result, "try-error")){
    if(length(try.result)>0){ # also wenn OF-Items >0
      #-------------------------------------------------------------------------
      # get names of OF-Items
      #-------------------------------------------------------------------------
      OFnames<-as.matrix(try.result,ncol=1)
      OFnames<-apply(OFnames,1,function(x){
                                            output<-strsplit(x,split="_")[[1]]
                                            output<-output[length(output)]})
      #-----------------------------------------------------------------------------
      # create LatexCode                                                                      
      #-----------------------------------------------------------------------------
      for(i in 1:length(OFnames)){
        treeName<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_tree_",OFnames[i],sep="")
          treeCaption<-paste("Regression tree for analysing the effect of model input \\tmcrv-items 
          on the outcome function (",OFnames[i],") according to the full model (left) and relaxed model (right).",sep="")
 cat("
\\begin{figure}[p]
\\centering",append=TRUE,file = file.name)
cat(paste("\n\\includegraphics[width=1\\textwidth]{",treeName,"}",sep=""),append=TRUE,file = file.name)
cat("\n\\caption{",treeCaption,"}",append=TRUE,file = file.name)
cat(paste("\n\\label{fig:",treeName,"}
\\end{figure}
\\clearpage",sep=""),append=TRUE,file = file.name)
        } # end for schleife
    } else {
      cat("Regression tree plot of the outcome function be created, there is no 'OF' item(s) defined in the model!\n")
    }# end if(length(try.result)>0){
   } else {
    cat("Regression tree plot of the outcome function cannot be created!\n")
   }# end if (!inherits(try.result, "try-error")){
} # end of function tolatexGraphs.tree




################################################################################
################################################################################
#' @description A function that creates a Tornado plot.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotTornado
#' @aliases plotTornado
#' @title Function that draws a Tornado plot
#' @usage plotTornado(rriskModel,pdfGraph=FALSE)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the plot is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotTornado(rriskModel,pdfGraph=FALSE)  }

plotTornado<-function(rriskModel,pdfGraph=FALSE)
{ # Problem: tornado-charts f?r geschichtete mcrv items...

  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  pdffilenames<-c()
  on.exit(return(pdffilenames))

  #-----------------------------------------------------------------------------
  # define help variables and functions
  #-----------------------------------------------------------------------------
  zscore <- function(x) (x-mean(x))/sd(x)
  mycol<-rriskModel@settings@mycol
  
  #-----------------------------------------------------------------------------
  # identify available mcrv items, available OF items and available uncert (mcrv and 'u', 'uv') items
  #-----------------------------------------------------------------------------
  OF.items<-c()
  mcrv.items<-c()
  unc.items<-c()
  mcrv.col<-c()
  mcrv.border<-c()
  
  #-----------------------------------------------------------------------------
  # get OF, mcrv, u and uv items
  #-----------------------------------------------------------------------------
  if(length(rriskModel@items@items)>0){
    for(i in 1:length(rriskModel@items@items)){
      #-------------------------------------------------------------------------
      # get OF items
      #-------------------------------------------------------------------------
      if(rriskModel@items@items[[i]]@rolecode=="OF"){
        OF.items<-c(OF.items,rriskModel@items@items[[i]]@name)
      } # end if(rriskModel@items@items[[i]]@rolecode=="OF"){
      #-------------------------------------------------------------------------
      # get mcrv items of type "u" oder "uv"
      #-------------------------------------------------------------------------
      if(rriskModel@items@items[[i]]@typecode=="mcrv" & rriskModel@items@items[[i]]@stratum==""){
        if(!is.null(rriskModel@items@items[[i]]@data) & rriskModel@items@items[[i]]@fullc!=""){
          # collect mcrv items
          mcrv.items<-c(mcrv.items,rriskModel@items@items[[i]]@name)
          mcrv.col<-c(mcrv.col,"white")
          mcrv.border<-c(mcrv.border,rriskModel@settings@mycol)
          if(rriskModel@items@items[[i]]@rolecode=="u" | rriskModel@items@items[[i]]@rolecode=="uv"){
            # collect uncertainty mcrv items
            unc.items<-c(unc.items,rriskModel@items@items[[i]]@name)
            mcrv.col[length(mcrv.col)]<-rriskModel@settings@mycol
            mcrv.border[length(mcrv.border)]<-"white"
          } # end  if(rriskModel@items@items[[i]]@rolecode=="u" | rriskModel@items@items[[i]]@rolecode=="uv"){
        } else {
          cat(paste("Not evaluated mcrv item '", rriskModel@items@items[[i]]@name,"' will be ignored.\n",sep=""))
        } # end !is.null(rriskModel@items@items[[i]]@data) & rriskModel@items@items[[i]]@fullc!="" 
      } else if (rriskModel@items@items[[i]]@typecode=="mcrv" & rriskModel@items@items[[i]]@stratum!="") {
        cat(paste("Statified mcrv item '", rriskModel@items@items[[i]]@name,"' will be ignored.\n",sep=""))
      }# end  if(rriskModel@items@items[[i]]@typecode=="mcrv"){
    } # end for
    
    if(length(OF.items)==0){
      stop("Tornado plot cannot be created, there is no 'OF' item defined in the model!",call.=FALSE)
    } # end  if(length(OF.items)==0){
    
    if(length(mcrv.items)==0){
      stop("Regression tree plot cannot be created, there is no uncertainty mcrv item(s) defined in the model!",call.=FALSE)
    }
    
    #if(length(mcrv.items)==0){
    #  stop("Regression tree plot cannot be created, there is no uncertainty mcrv item(s) defined in the model!",call.=FALSE)
    #}
    
  } else {
    stop("Tornado plot cannot be created, the list of model items is empty!",call.=FALSE)
  } # end  if(length(rriskModel@items@items)>0){
  
  #-----------------------------------------------------------------------------
  # evaluate data for tornado charts
  #-----------------------------------------------------------------------------
  if(length(rriskModel@output@fullout.1d)==0 | length(rriskModel@output@relaxout.1d)==0){
    stop("Tornado plot cannot be created, the list of 1d full and/or relax model simulation results is empty!",call.=FALSE)
  } else {
    #--------------------------------------------------------------------------
    # get data of 1d simulation for full model
    #--------------------------------------------------------------------------
    fullout.1d<-rriskModel@output@fullout.1d
    fullout.1d<-fullout.1d[c(mcrv.items,OF.items)]
    fullout.1d<-as.data.frame(fullout.1d)
     
    #--------------------------------------------------------------------------
    # get data of 1d simulation for relaxed model
    #--------------------------------------------------------------------------
    relaxout.1d<-rriskModel@output@relaxout.1d
    relaxout.1d<-relaxout.1d[c(mcrv.items,OF.items)]
    relaxout.1d<-as.data.frame(relaxout.1d)
                            
    #--------------------------------------------------------------------------
    # transform 1d simulation data accoring to model settings
    #--------------------------------------------------------------------------
    if(rriskModel@settings@trans=="rank"){
      fullout.1d<-apply(fullout.1d,2,rank)
      relaxout.1d<-apply(relaxout.1d,2,rank)
      tornado.xlab0 <- "using rank transformated data"
    } else if(rriskModel@settings@trans=="z-score"){
      fullout.1d<-apply(fullout.1d,2,zscore)
      relaxout.1d<-apply(relaxout.1d,2,zscore)
      tornado.xlab0 <- "using z-transformed data"
    } else if(rriskModel@settings@trans=="identity"){
      tornado.xlab0 <- "using original data"
    } # end if(rriskModel@settings@trans=="rank"){
    
    #--------------------------------------------------------------------------
    # Create tornado plot for each 'OF' item
    #--------------------------------------------------------------------------    
    for(i in 1:length(OF.items)){
      OF <- OF.items[i]
     
      OF.index<-which(colnames(fullout.1d)==OF)
      OF.full<-fullout.1d[,OF.index]
      #data.full<-fullout.1d[,-c(OF.index)]
      data.full<-as.data.frame(fullout.1d[,-c(OF.index)])
     
      OF.index<-which(colnames(fullout.1d)==OF)
      OF.relax<-relaxout.1d[,OF.index]
      #data.relax<-relaxout.1d[,-c(OF.index)]
      data.relax<-as.data.frame(relaxout.1d[,-c(OF.index)])
               
      effect.full <- rep(NA,length(mcrv.items))
      effect.relax <- rep(NA,length(mcrv.items))
     
      #-------------------------------------------------------------------------
      # model for sensitivity analysis, choices by settings: "correlation" (default), "regression"
      #-------------------------------------------------------------------------
      if(rriskModel@settings@sens == "correlation"){
        for(i in 1:length(mcrv.items)){
          effect.full[i] <- suppressWarnings(cor(data.full[,i],OF.full))
          effect.relax[i]<- suppressWarnings(cor(data.relax[,i],OF.relax))
        } # end for
        tornado.xlab <- paste("Correlation coefficients",tornado.xlab0)
      } else if(rriskModel@settings@sens == "regression"){
        effect.full <- lm(OF.full ~ -1 + data.full)$coeff
        effect.relax <- lm(OF.relax ~ -1 + data.relax)$coeff
        tornado.xlab <- paste("Regression coefficients",tornado.xlab0)
      } # end if(rriskModel@settings@sens == "correlation"){
      
      # sic to redefine colnames here
      names(effect.full) <- mcrv.items
      names(effect.relax) <- mcrv.items
     
      #effect.full2 <- effect.full^2
      #effect.relax2 <- effect.relax^2
      #max.effect.full2 <- max(effect.full2)
      #max.effect.relax2 <- max(effect.relax2)
      #r.h <- 1+ 1*length(effect.full)
      xlim <- c(min(effect.full,effect.relax,na.rm=TRUE),max(effect.full,effect.relax,na.rm=TRUE))
      if(xlim[1]==xlim[2] & xlim[1]>0){
        xlim<-c(0,xlim[2])
      } else if(xlim[1]==xlim[2] & xlim[2]<0){
        xlim<-c(xlim[2],0)
      }

      #-------------------------------------------------------------------------
      # create tornado plots
      #-------------------------------------------------------------------------
      graphRef<-gsub(x=rriskModel@name@name," ",replacement="")
      graphRef <- paste("\\ref{fig:", graphRef, "_tornado_", OF, "}", sep = "")
      if(pdfGraph==FALSE){
        X11(width=10,height=10)
        mainText=paste("\n Tornado chart of the outcome item '",OF,"'\n",graphRef, sep="")
      } else { mainText="Tornado chart"
        pdffilenames.temp<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_tornado_",OF,sep="")
        pdffilenames<-c(pdffilenames,pdffilenames.temp)
        pdf(file=paste(pdffilenames.temp,".pdf",sep=""),width=10,height=10)
        mainText=paste("\n Tornado chart of the outcome item '",OF,"'",sep="")
      } # end if(pdfGraph==FALSE){
      
      par(mfrow=c(1,2),oma=c(2,1,ifelse(pdfGraph, 5, 5),1))
      suppressWarnings(barplot(effect.full[length(effect.full):1], horiz=TRUE, main="Full model", 
        col=rev(mcrv.col),xlim=xlim,border=rev(mcrv.border),xlab="",xaxt="s",las=1,lwd=2))
      suppressWarnings(abline(v=0))
      
      suppressWarnings(barplot(effect.relax[length(effect.relax):1], horiz=TRUE, main="Relaxed model", 
        col=rev(mcrv.col),xlim=xlim,border=rev(mcrv.border),xlab="",xaxt="s",las=1,lwd=2))
      suppressWarnings(abline(v=0))
      suppressWarnings(mtext(tornado.xlab,side=1,outer=TRUE))
      suppressWarnings(mtext(mainText,side=3,outer=TRUE,cex=1.4))
      
      #-------------------------------------------------------------------------
      # close graph devices
      #-------------------------------------------------------------------------
      if(pdfGraph==TRUE) dev.off()
     } # end for schleife
  } # end   if(length(rriskModel@output@fullout.1d)==0 | length(rriskModel@output@relaxout.1d)==0){
} # end of function plotTornado()




################################################################################
################################################################################
#' @description A function that creates LaTeX codes to include a Tornado plot.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.tornado
#' @aliases tolatexGraphs.tornado
#' @title Function that creates LaTeX codes to include a Tornado plot
#' @usage tolatexGraphs.tornado(rriskModel,file.name)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param file.name is the name of the TeX file into which the codes are to be written 
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' tolatexGraphs.tornado(rriskModel,file.name="text.tex")}

tolatexGraphs.tornado<-function(rriskModel,file.name)
{ try.result<-try(plotTornado(rriskModel,pdfGraph=TRUE),silent=TRUE)
   if (!inherits(try.result, "try-error")){
    if(length(try.result)>0){ # also wenn OF-Items >0
      #-------------------------------------------------------------------------
      # get names of OF-Items
      #-------------------------------------------------------------------------
      OFnames<-as.matrix(try.result,ncol=1)
      OFnames<-apply(OFnames,1,function(x){
                                            output<-strsplit(x,split="_")[[1]]
                                            output<-output[length(output)]})
      #-----------------------------------------------------------------------------
      # create LatexCode
      #-----------------------------------------------------------------------------
      for(i in 1:length(OFnames)){
        tornadoName<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_tornado_",OFnames[i],sep="")
          tornadoCaption<-paste("Tornado charts of the effect of stochastic (\\tmcrv) items on the outcome function (",OFnames[i],"). 
          The left figure refers to the full risk model as it was used to estimate the outcome function 
          whereas the right figure refers to a relaxed model, where all distributional assumptions where
          replaced with uniform distributions spanning the absolute plausible ranges of each input parameter.
          Items representing uncertainty are shared in ",rriskModel@settings@mycol,".",sep="")
 cat("
\\begin{figure}[p]
\\centering",append=TRUE,file = file.name)
cat(paste("\n\\includegraphics[width=1\\textwidth]{",tornadoName,"}",sep=""),append=TRUE,file = file.name)
cat("\n\\caption{",tornadoCaption,"}",append=TRUE,file = file.name)
cat(paste("\n\\label{fig:",tornadoName,"}
\\end{figure}
\\clearpage",sep=""),append=TRUE,file = file.name)
        } # end for schleife
    } else {
      cat("Tornado plot of the outcome function be created, there is no 'OF' item(s) defined in the model!\n")
    }# end if(length(try.result)>0){
   } else {
    cat("Tornado plot of the outcome function cannot be created!\n")
   }# end if (!inherits(try.result, "try-error")){
} # end of function tolatexGraphs.convergence


    
    
################################################################################
################################################################################
#' @description A function that creates a histogram.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotOFHistogram
#' @aliases plotOFHistogram
#' @title Function that draws a histogram
#' @usage plotOFHistogram(rriskModel,pdfGraph=FALSE)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the histogram is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotOFHistogram(rriskModel,pdfGraph=FALSE)  }

plotOFHistogram<-function(rriskModel,pdfGraph=FALSE){
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  pdffilenames<-c()
  on.exit(return(pdffilenames))

  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  OFitems<-c()
  OFunits<-c()

  #-----------------------------------------------------------------------------
  # get OF items and OF units
  #-----------------------------------------------------------------------------
  if(length(rriskModel@items@items)>0){
    for(i in 1:length(rriskModel@items@items)){
      if(rriskModel@items@items[[i]]@rolecode=="OF"){
        OFitems<-c(OFitems,rriskModel@items@items[[i]]@name)
        OFunits<-c(OFunits,rriskModel@items@items[[i]]@unit)
      } # end if(rriskModel@items@items[[i]]@rolecode=="OF"){
    } # end for
  } else {
    stop("No convergence plot can be created, the list of model items is empty!",call.=FALSE)
  } # end if(length(rriskModel@items@items)>0){

  #-----------------------------------------------------------------------------
  # create histogram of each OF item
  #-----------------------------------------------------------------------------
  if(length(rriskModel@output@fullout.1d)>0){
    OFnumber<-0
    for(i in 1:length(OFitems)){
      if(is.element(OFitems[i],names(rriskModel@output@fullout.1d))){
        OF.values<-rriskModel@output@fullout.1d[[OFitems[i]]]
        OFnumber<-OFnumber+1
        #-----------------------------------------------------------------------
        # create hist as pdf plot or as R plot
        #-----------------------------------------------------------------------
        if(pdfGraph==FALSE){
          X11()
          graphRef <- paste("\\ref{fig:", gsub(x=rriskModel@name@name," ",replacement=""),"_histogram_",OFitems[i],"}",sep="")
          mainText=paste("Histogram of the outcome item '",OFitems[i],"'", "\n", graphRef, sep="")
        } else {
          mainText=paste("Histogram of the outcome function '",OFitems[i],"'",sep="" )
          pdffilenames.temp<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_histogram_",OFitems[i],sep="")
          pdffilenames<-c(pdffilenames,pdffilenames.temp)
          pdf(file=paste(pdffilenames.temp,".pdf",sep=""))
        } # end if(pdfGraph==FALSE){
        xlabText<-paste(OFitems[i]," [",OFunits[i],"]",sep="")
        suppressWarnings(hist(OF.values,freq=FALSE,main=mainText,xlab=xlabText,col=rriskModel@settings@mycol,border=rriskModel@settings@mycol))
        if(pdfGraph==TRUE) dev.off()
      } # end
    }
    if(OFnumber==0){
      stop("There is no outcome items in the 1d simulation results!",call.=FALSE)
    } # end if(OFnumber==0){
  } else {
    stop("1d simulation of the full command has not be run!",call.=FALSE)
  } # end if(length(rriskModel@output@fullout.1d)>0){
} # end of function plotOFHistogram()



################################################################################
################################################################################
#' @description A function that creates LaTeX codes to include a histogram.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.histogram
#' @aliases tolatexGraphs.histogram
#' @title Function that creates LaTeX codes to include a histogram
#' @usage tolatexGraphs.histogram(rriskModel,file.name)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param file.name is the name of the TeX file into which the codes are to be written 
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' tolatexGraphs.histogram(rriskModel,file.name="text.tex") }

tolatexGraphs.histogram<-function(rriskModel,file.name)
{
   try.result<-try(plotOFHistogram(rriskModel,pdfGraph=TRUE),silent=TRUE)
   if (!inherits(try.result, "try-error")){
    if(length(try.result)>0){ # also wenn OF-Items >0
      #-------------------------------------------------------------------------
      # get names of OF-Items
      #-------------------------------------------------------------------------
      OFnames<-as.matrix(try.result,ncol=1)
      OFnames<-apply(OFnames,1,function(x){
                                            output<-strsplit(x,split="_")[[1]]
                                            output<-output[length(output)]})
      #-----------------------------------------------------------------------------
      # create LatexCode
      #-----------------------------------------------------------------------------
      for(i in 1:length(OFnames)){
        histName<-try.result[i]
        histCaption<-paste("Histogram on the outcome function (",OFnames[i],") using ",rriskModel@settings@N," iterations (variablility and uncertainty combined).",sep="")
 cat("
\\begin{figure}[p]
\\centering",append=TRUE,file = file.name)
cat(paste("\n\\includegraphics[width=1\\textwidth]{",histName,"}",sep=""),append=TRUE,file = file.name)
cat("\n\\caption{",histCaption,"}",append=TRUE,file = file.name)
cat(paste("\n\\label{fig:",histName,"}
\\end{figure}
\\clearpage",sep=""),append=TRUE,file = file.name)
        } # end for schleife
    } else {
      cat("Histogram of the outcome function be created, there is no 'OF' item(s) defined in the model!\n")
    }# end if(length(try.result)>0){
   } else {
    cat("Histogram of the outcome function cannot be created!\n")
   }# end if (!inherits(try.result, "try-error")){
} # end of function tolatexGraphs.histogram




################################################################################
################################################################################
#' @description A function that creates a convergence plot.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotOFConvergence
#' @aliases plotOFConvergence
#' @title Function that draws a convergence plot
#' @usage plotOFConvergence(rriskModel,pdfGraph=FALSE)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the plot is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotOFConvergence(rriskModel,pdfGraph=FALSE)  }

plotOFConvergence<-function(rriskModel,pdfGraph=FALSE)
{ #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  pdffilenames<-c()
  on.exit(return(pdffilenames))

  #-----------------------------------------------------------------------------
  # define some help variables
  #-----------------------------------------------------------------------------
  settings<-rriskModel@settings
  N<-settings@N

  #-----------------------------------------------------------------------------
  # get OF items
  #-----------------------------------------------------------------------------
  OFitems<-c()
  if(length(rriskModel@items@items)>0){
    for(i in 1:length(rriskModel@items@items)){
      if(rriskModel@items@items[[i]]@rolecode=="OF"){
        OFitems<-c(OFitems,rriskModel@items@items[[i]]@name)
      } # end
    } # end for
  } else {
    stop("No convergence plot can be created, the list of model items is empty!",call.=FALSE)
  } # end  if(length(rriskModel@items@items)>0){
  #-----------------------------------------------------------------------------
  # create convergence output
  #-----------------------------------------------------------------------------
  if(length(rriskModel@output@fullout.1d)>0){
    OFnumber<-0
    for(j in 1:length(OFitems)){
      if(is.element(OFitems[j],names(rriskModel@output@fullout.1d))){
        OFnumber<-OFnumber+1
        OF.values<-rriskModel@output@fullout.1d[[OFitems[j]]]
        k<-round(N/100)
        Iteration<-c(seq(k,N,k),N)
        m<-rep(NA,length(Iteration))
        for(i in 1:length(Iteration)) m[i]<-median(OF.values[1:Iteration[i]])
        ymin<-min(m)
        ymax<-max(m)
        ybar<-m[length(Iteration)]
        yrange<-c(min(ymin,ybar-settings@abserror),max(ymax,ybar+settings@abserror))
        tolerance<-ybar+c(-1,1)*settings@abserror
        #-----------------------------------------------------------------------------
        # open pdf or R graph device
        #-----------------------------------------------------------------------
        if(pdfGraph==FALSE){
          X11()
          graphRef <- paste("\\ref{fig:", gsub(x=rriskModel@name@name," ",replacement=""),"_convergence_",OFitems[j],"}",sep="")
        } else {
           pdffilenames.temp<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_convergence_",OFitems[j],sep="")
           pdffilenames<-c(pdffilenames,pdffilenames.temp)
           pdf(file=paste(pdffilenames.temp,".pdf",sep=""))
        } # end  if(pdfGraph==FALSE){
        #-----------------------------------------------------------------------
        # create plot
        #-----------------------------------------------------------------------
        ylabText=paste("median(",OFitems[j],")",sep="")
        mainText=paste("Convergence of the outcome item '",OFitems[j],"'", ifelse(pdfGraph, "", paste("\n", graphRef)),sep="")
        suppressWarnings(plot(m~Iteration,type="l",ylim=yrange,main=mainText,xlab="Iteration",ylab=ylabText,col=settings@mycol))
        suppressWarnings(abline(h=ybar,lty=3,col=settings@mycol))
        suppressWarnings(abline(h=tolerance,lty=2))
        #-----------------------------------------------------------------------
        # close graph device
        #-----------------------------------------------------------------------
        if(pdfGraph==TRUE) dev.off()
      }
    } # end for
    if(OFnumber==0) stop("There is no outcome items in the 1d simulation results!",call.=FALSE)
  } else{
    stop("1d simulation of the full command has not be run!",call.=FALSE)
  }
} # end of function plotOFConvergence()



################################################################################
################################################################################
#' @description A function that creates LaTeX codes to include a convergence plot.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatexGraphs.convergence
#' @aliases tolatexGraphs.convergence
#' @title Function that creates LaTeX codes to include a convergence plot
#' @usage tolatexGraphs.convergence(rriskModel,file.name)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param file.name is the name of the TeX file into which the codes are to be written 
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' tolatexGraphs.convergence(rriskModel,file.name="text.tex") }

tolatexGraphs.convergence<-function(rriskModel,file.name)
{  try.result<-try(plotOFConvergence(rriskModel,pdfGraph=TRUE),silent=TRUE)
   if (!inherits(try.result, "try-error")){
    if(length(try.result)>0){ # also wenn OF-Items >0
      #-------------------------------------------------------------------------
      # get names of OF-Items
      #-------------------------------------------------------------------------
      OFnames<-as.matrix(try.result,ncol=1)
      OFnames<-apply(OFnames,1,function(x){
                                            output<-strsplit(x,split="_")[[1]]
                                            output<-output[length(output)]})
      #-----------------------------------------------------------------------------
      # create LatexCode
      #-----------------------------------------------------------------------------
      for(i in 1:length(OFnames)){
        convName<-try.result[i]
        convCaption<-paste("Convergence of the full model of the output item (",OFnames[i],") using the cumulative median of the outcome function using up to ",
          rriskModel@settings@N," iterations. The final median estimate and the absolute error tolerance are shown as dotted and dashed lines, respectively.",sep="")
 cat("
\\begin{figure}[p]
\\centering",append=TRUE,file = file.name)
cat(paste("\n\\includegraphics[width=1\\textwidth]{",convName,"}",sep=""),append=TRUE,file = file.name)
cat("\n\\caption{",convCaption,"}",append=TRUE,file = file.name)
cat(paste("\n\\label{fig:",convName,"}
\\end{figure}
\\clearpage
",sep=""),append=TRUE,file = file.name)
        } # end for schleife
    } else {
      cat("Convergence plot of the outcome function be created, there is no 'OF' item(s) defined in the model!\n")
    }# end if(length(try.result)>0){
   } else {
    cat("Convergence plot of the outcome function cannot be created!\n")
   }# end if (!inherits(try.result, "try-error")){
} # end of function tolatexGraphs.convergence

