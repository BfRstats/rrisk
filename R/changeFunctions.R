

################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{itemClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.item
#' @aliases change.item
#' @title Function for edditing model slot 'item'
#' @usage change.item(index,rriskModel,menuLevel=1)
#' @param index ...
#' @param rriskModel (risk model) is an object of class \code{modelClass}
#' @param menuLevel is a numeric value that indicates the load of the current menu
#' @return This function returns edited model item.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export

change.item<-function(index,rriskModel,menuLevel=1)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  { availableItems<-get("availableItems",envir=tempEnvir)
    item<-get("item",envir=tempEnvir)
    item.original<-get("item.original",envir=tempEnvir)
    scoring<-get("scoring",tempEnvir)
    possibleValues<-get("possibleValues",tempEnvir)
    #---------------------------------------------------------------------------
    # getting new values
    #---------------------------------------------------------------------------
    name<-tclvalue(tkget(name.entry))
    unit<-tclvalue(tkget(unit.entry))
    reference<-tclvalue(tkget(reference.entry))
    scores<-tclvalue(tkget(scores.entry))
    plausimin<-tclvalue(tkget(min.entry))
    plausimax<-tclvalue(tkget(max.entry))
    depitem<-tclvalue(tkget(depitem.entry))
    title<-tclvalue(tkget(title.txt,"0.0","end"))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))    
    assumptions<-tclvalue(tkget(assumptions.txt,"0.0","end"))
    remark<-tclvalue(tkget(remark.txt,"0.0","end")) 
    stratum<-tclvalue(tkget(stratum.combobox))
    part<-tclvalue(tkget(part.combobox))
    type<-tclvalue(tkget(type.combobox))
    role<-tclvalue(tkget(role.combobox))
    
    #---------------------------------------------------------------------------
    # formatting new data
    #---------------------------------------------------------------------------   
    title<-gsub(title,pattern="\n",replacement="")
    title<-gsub(title,pattern="\t",replacement="")
    explanation<-gsub(explanation,pattern="\n",replacement="")
    explanation<-gsub(explanation,pattern="\t",replacement="")    
    assumptions<-gsub(assumptions,pattern="\n",replacement="")
    assumptions<-gsub(assumptions,pattern="\t",replacement="")
    remark<-gsub(remark,pattern="\n",replacement="")
    remark<-gsub(remark,pattern="\t",replacement="")
  
    #---------------------------------------------------------------------------
    # setting typecode
    #---------------------------------------------------------------------------
    if(type=="Data item (data)")
    { typecode<-"data"
    } else if (type=="User-defined R type function (fdoc)")
    { typecode="fdoc"
    } else if(type=="Stratum triplet (stid, stdi, strv)")
    { typecode<-"stra"
    } else if(type=="Numerical value(s) (numv)")
    { typecode<-"numv"
    } else if(type=="Monte-Carlo random variate (mcrv)")
    { typecode<-"mcrv"
    } else if(type=="Function of mcrv and other item(s) (fnrv)")
    { typecode<-"fnrv"
    } else if(type=="Mixture distribution item (mxrv)")
    { typecode<-"mxrv"
    } else if(type=="Resampling item (rsrv)")
    { typecode<-"rsrv"
    } else if(type=="Bootsrap item (bsrv)")
    { typecode<-"bsrv"
    } else if(type=="Bayes domain joint posterior distribution item (bdjp)")
    { typecode<-"bdjp"
    } else typecode<-""
    
    # mit corv-Items
    #if(type=="Data item (data)")
    #{ typecode<-"data"
    #} else if (type=="User-defined R type function (fdoc)")
    #{ typecode="fdoc"
    #} else if(type=="Stratum triplet (stid, stdi, strv)")
    #{ typecode<-"stra"
    #} else if(type=="Numerical value(s) (numv)")
    #{ typecode<-"numv"
    #} else if(type=="Monte-Carlo random variate (mcrv)")
    #{ typecode<-"mcrv"
    #} else if(type=="Function of mcrv and other item(s) (fnrv)")
    #{ typecode<-"fnrv"
    #} else if(type=="Correlated random variate (corv)")
    #{ typecode<-"corv"
    #} else if(type=="Mixture distribution item (mxrv)")
    #{ typecode<-"mxrv"
    #} else if(type=="Resampling item (rsrv)")
    #{ typecode<-"rsrv"
    #} else if(type=="Bootsrap item (bsrv)")
    #{ typecode<-"bsrv"
    #} else if(type=="Bayes domain joint posterior distribution item (bdjp)")
    #{ typecode<-"bdjp"
    #} else typecode<-""
    
    if(gsub(x=typecode," ",replacement="")=="")
    { tkmessageBox(message="Empty 'Type' field!",icon="error")
      tkfocus(type.combobox)
      stop("INVALID INPUT, empty 'Type' field...",call.=FALSE)
    }

    #---------------------------------------------------------------------------
    # setting rolecode
    #---------------------------------------------------------------------------
    if(role=="This variable represents uncertainty only (u)")
    { rolecode<-"u"
    } else if(role=="Not defined (nd)")
    { rolecode<-"nd"
    } else if(role=="This variable represents both uncertainty and variability (uv)")
    { rolecode<-"uv"
    } else if(role=="This variable represents variability only (v)")
    { rolecode<-"v"
    } else if(role=="This item has been defined as outcome function (OF)")
    { rolecode<-"OF"
    } else rolecode<-""

    #---------------------------------------------------------------------------
    # check item name
    #---------------------------------------------------------------------------
    if(is.element(name,availableItems))
    { tkmessageBox(message="There is another item with the same name in the current model, item name should be unique!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, there is another item with the same name in the current model, item name should be unique...",call.=FALSE)
    } else if(gsub(x=name," ",replacement="")=="")
    { tkmessageBox(message="Empty 'Name' field!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty 'Name' field...",call.=FALSE)
    }
    
    #---------------------------------------------------------------------------
    # check plausimin and plausimax
    #---------------------------------------------------------------------------
    plausimin<-gsub(x=plausimin," ",replacement="")
    plausimax<-gsub(x=plausimax," ",replacement="")
    if(is.element(typecode,c("mcrv","mxrv"))){
      # check plausimin
      plausimin<-suppressWarnings(as.numeric(plausimin))
      if(is.na(plausimin)){
        tkmessageBox(message="Plausible min value could not be converted to numerical value!",icon="error")
        tkconfigure(min.entry,text=tclVar(""))
        tkfocus(min.entry)
        stop("INVALID INPUT, Plausible min value could not be converted to numerical value...",call.=FALSE)
      }
      # check plausimax
      plausimax<-suppressWarnings(as.numeric(plausimax))
      if(is.na(plausimax)){
        tkmessageBox(message="Plausible max value could not be converted to numerical value!",icon="error")
        tkconfigure(max.entry,text=tclVar(""))
        tkfocus(max.entry)
        stop("INVALID INPUT, Plausible max value could not be converted to numerical value...",call.=FALSE)
      }
    } else {
      plausimin<-NULL
      plausimax<-NULL
    }
    
    #---------------------------------------------------------------------------
    # check dependent items
    #---------------------------------------------------------------------------
    depitem<-strsplit(depitem,split=" ")[[1]]
    depitem<-setdiff(depitem,unique(c("",name)))
    depitem<-gsub(x=depitem," ",replacement="")
    if(!all(is.element(depitem,availableItems)))
    { index<-which(is.element(depitem,availableItems)==FALSE)
      if(length(index)>0)
      { mess<-paste("Following items are not defined in the current model and will be removed from the list of dependent items: ",paste(depitem[index],collapse=", "),".",sep="")
        tkmessageBox(message=mess,icon="warning")
        depitem<-depitem[-index]
      }
    }
    
    #---------------------------------------------------------------------------
    # check scores: überprüfe scores auf konsistenz mit dem scoring system
    #---------------------------------------------------------------------------
    if(length(possibleValues)>0 & length(scoring)>0)
    { scores<-tclvalue(tkget(scores.entry))
      scores<-suppressWarnings(as.numeric(strsplit(scores,split=" ")[[1]]))
      if(any(is.na(scores)))
      { tkmessageBox(message="The scores entries could not be converted to numerical values!",icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, not numerical entries in field 'Scores'...",call.=FALSE)
      } else if(length(scores)!=length(scoring))
      { mess<-paste("The vector of scores entries should be of length ",length(scoring),"!",sep="")
        tkmessageBox(message=mess,icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
      }
      for(i in 1:length(scores))
      { if(!is.element(scores[i],possibleValues))
        { mess<-paste("The values of the score ",scoring[[i]]@notation,"is out of range!")
          tkmessageBox(message=mess,icon="error")
          tkfocus(scores.entry)
          stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
        } 
      } # end for
    } else scores<-NULL
    
    
    #---------------------------------------------------------------------------
    # put new values into the global help variables
    #---------------------------------------------------------------------------
    item@name<-name
    item@title<-title
    item@stratum<-stratum
    item@part<-part
    item@explanation<-explanation
    item@type<-type
    item@typecode<-typecode
    item@depitem<-depitem
    item@unit<-unit
    item@role<-role
    item@rolecode<-rolecode
    item@plausimin<-plausimin
    item@plausimax<-plausimax
    item@scores<-scores
    item@assumptions<-assumptions
    item@remark<-remark
    item@reference<-reference
    assign("item",value=item,envir=tempEnvir)
    
    
    assign("button",value="ok",envir=tempEnvir)  
    # destroy the dialog window
    tkdestroy(itemChangeWindow)
  } # end of the function onOk()

  #-----------------------------------------------------------------------------
  # what happends by pressing "clear" button
  #-----------------------------------------------------------------------------
  onClear<-function(...)
  { tkdelete(title.txt,"0.0","end")
    tkdelete(explanation.txt,"0.0","end")
    tkconfigure(unit.entry,text=tclVar(""))
    tkdelete(assumptions.txt,"0.0","end")
    tkdelete(remark.txt,"0.0","end")
    tkconfigure(scores.entry,text=tclVar(""))
    tkset(role.combobox,itemrole[1])
    tkset(type.combobox,itemtype[1])
    tkset(stratum.combobox,stratums[1])
    tkset(part.combobox,parts[1])
    #tkconfigure(definition.txt,state="normal")
    #tkdelete(definition.txt,"0.0","end")
    #tkconfigure(definition.txt,state="disabled")
    tkconfigure(min.entry,text=tclVar(""),state="disabled")
    tkconfigure(max.entry,text=tclVar(""),state="disabled")
    tkconfigure(depitem.entry,text=tclVar(""))
    tkconfigure(reference.entry,text=tclVar(""))
  } # end of the function onClear()

  #-----------------------------------------------------------------------------
  # what happends by pressing "reset" button
  #-----------------------------------------------------------------------------
  onReset<-function(...)
  { item.original<-get("item.original",envir=tempEnvir)
  
    #---------------------------------------------------------------------------
    # Felder name, title, explanation, unit, assumptions, remark zurücksetzen
    #---------------------------------------------------------------------------
    tkconfigure(name.entry,text=tclVar(item.original@name))
    tkdelete(title.txt,"0.0","end");        tkinsert(title.txt,"0.0",item.original@title)
    tkdelete(explanation.txt,"0.0","end");  tkinsert(explanation.txt,"0.0",item.original@explanation)
    tkconfigure(unit.entry,text=tclVar(item.original@unit))
    tkdelete(assumptions.txt,"0.0","end");  tkinsert(assumptions.txt,"0.0",item.original@assumptions)
    tkdelete(remark.txt,"0.0","end");       tkinsert(remark.txt,"0.0",item.original@remark)
    #---------------------------------------------------------------------------
    # Definition Feld zurücksetzen
    #---------------------------------------------------------------------------
    #tkconfigure(definition.txt,state="normal")
    #tkdelete(definition.txt,"0.0","end");        tkinsert(definition.txt,"0.0",item.original@definition)
    #tkconfigure(definition.txt,state="disabled")
    #---------------------------------------------------------------------------
    # Scores Feld zurücksetzen
    #---------------------------------------------------------------------------
    if(!is.null(item.original@scores))
    { scoresText<-tclVar(item@scores)
    } else  scoresText<-tclVar("")
    tkconfigure(scores.entry,state="normal")
    tkconfigure(scores.entry,text=scoresText)
    if(is.null(item.original@scores))tkconfigure(scores.entry,state="disabled")
    #---------------------------------------------------------------------------
    # Comboboxe zuücksetzen
    #---------------------------------------------------------------------------
    tkset(role.combobox,item.original@role)
    tkset(type.combobox,item.original@type)
    tkset(stratum.combobox,item.original@stratum)
    tkset(part.combobox,item.original@part)
    #---------------------------------------------------------------------------
    # Plausimin Feld zurücksetzen
    #---------------------------------------------------------------------------
    if(!is.null(item.original@plausimin))
    { plausiminText<-tclVar(item@plausimin)
    } else  plausiminText<-tclVar("")
    tkconfigure(min.entry,text=plausiminText)
    #---------------------------------------------------------------------------
    # Plausimax Feld zurücksetzen
    #---------------------------------------------------------------------------
    if(!is.null(item.original@plausimax))
    { plausimaxText<-tclVar(item@plausimax)
    } else  plausimaxText<-tclVar("")
    tkconfigure(max.entry,text=plausimaxText)
    #---------------------------------------------------------------------------
    # Plausimin und Plausimax für mcrv und mxrv aktivieren
    #---------------------------------------------------------------------------
    if(item.original@type=="Monte-Carlo random variate (mcrv)" | item.original@type=="Mixture distribution item (mxrv)"){
      tkconfigure(min.entry,state="normal")
      tkconfigure(max.entry,state="normal")
    } else {
      tkconfigure(min.entry,state="disabled")
      tkconfigure(max.entry,state="disabled")
    }   
    #----------
    #tkconfigure(depitem.entry,text=tclVar(item.original@depitem))
    #---------------------------------------------------------------------------
    # Reference Feld zurücksetzen
    #---------------------------------------------------------------------------
    tkconfigure(reference.entry,text=tclVar(item.original@reference))
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    item.original<-get("item.original",envir=tempEnvir)
    assign("item",value=item.original,envir=tempEnvir)  
    tkdestroy(itemChangeWindow)
  } # end of the function onCancel()

  #-----------------------------------------------------------------------------
  # create GUI window and help variables
  #-----------------------------------------------------------------------------
  #rriskModel<-demoModel1
  #rriskModel@scoring<-emptyModel@scoring
  #item<-rriskModel@items@items[[1]]
  #itemtype<-rriskSession@itemtype
  #itemrole<-rriskSession@itemrole
  
  #item<-object1
  #rriskModel<-object2
  
  if(index>length(rriskModel@items@items))
  { item<-new("itemClass")
    nameEntryState<-"normal"
  } else
  { item<-rriskModel@items@items[[index]]
    nameEntryState<-"readonly"
  }
  
  itemtype=c("Data item (data)",
      "User-defined R type function (fdoc)",
      "Stratum triplet (stid, stdi, strv)",
      "Numerical value(s) (numv)",
      "Monte-Carlo random variate (mcrv)",
      "Function of mcrv and other item(s) (fnrv)",  
      "Mixture distribution item (mxrv)",
      "Resampling item (rsrv)",
      "Bootsrap item (bsrv)",
      "Bayes domain joint posterior distribution item (bdjp)")
      #"Correlated random variate (corv)")
  
  itemrole=c( "Not defined (nd)",
      "This variable represents uncertainty only (u)", 
      "This variable represents both uncertainty and variability (uv)",
      "This variable represents variability only (v)",
      "This item has been defined as outcome function (OF)")
  
  
  scoring<-rriskModel@scoring  
  possibleValues<-scoring@values
  scoring<-scoring@scoring
  #-----------------------------------------------------------------------------
  parts<-c("")
  if(length(rriskModel@parts@parts)>0)
  { for(i in 1:length(rriskModel@parts@parts))
    { parts<-c(parts,rriskModel@parts@parts[[i]]@name)
    }
  }
  #-----------------------------------------------------------------------------
  stratums<-c("")
  if(length(rriskModel@items@items)>0){
    for( i in 1:length(rriskModel@items@items)){
      item.temp<-rriskModel@items@items[[i]]
      if(item.temp@typecode=="stra"){
        stratums<-c(stratums,item.temp@name)
      }
    }
  }
  #-----------------------------------------------------------------------------
  availableItems<-c()
  if(length(rriskModel@items@items)>0)
  { for(i in 1:length(rriskModel@items@items))
    { availableItems<-c(availableItems,rriskModel@items@items[[i]]@name)
    }
    availableItems<-setdiff(availableItems,item@name)
  }
  #-----------------------------------------------------------------------------
  if(length(possibleValues)>0 & length(scoring)>0)
  { scoresState="normal"
    if(is.null(item@scores)) item@scores<-as.numeric(rep(rriskModel@scoring@vmeanings["notapplicable"],length(scoring)))
  } else
  { scoresState="disabled"
    item@scores<-NULL
  }
  #-----------------------------------------------------------------------------
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv) 
  assign("tempEnvir",value=new.env())
  assign("scoring",value=scoring,envir=tempEnvir)
  assign("parts",value=parts,envir=tempEnvir)
  assign("availableItems",value=availableItems,envir=tempEnvir)
  assign("possibleValues",value=possibleValues,envir=tempEnvir)
  assign("item",value=item,envir=tempEnvir)  
  assign("item.original",value=item,envir=tempEnvir)
  assign("button",value="",envir=tempEnvir)  
  #-----------------------------------------------------------------------------
  itemChangeWindow<-tktoplevel()
  window.title<-paste("Editing item of model '",rriskModel@name@name,"'",sep="")
  #tkwm.title(itemChangeWindow,"Edit item")
  tkwm.title(itemChangeWindow,window.title)
  tkwm.resizable(itemChangeWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(itemChangeWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)

  # changing name entry
  name.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=tclVar(item@name),font=combo.font,state=nameEntryState)
  name.label<-tklabel(allFrame,font=label.font,text="Name")
  tkgrid(name.label,name.entry,sticky="nw",padx=c(0,5),pady=c(0,5))

  # changing title entry
  title.scrollbar<-tkscrollbar(allFrame,repeatinterval=1,command=function(...)tkyview(title.txt,...))
  title.txt<-tktext(allFrame,relief="sunken",borderwidth=3,width=70,height=2,
    yscrollcommand=function(...)tkset(title.scrollbar,...),wrap="word",font=combo.font)
  title.label<-tklabel(allFrame,font=label.font,text="Title")
  tkinsert(title.txt,"0.0",item@title)
  tkgrid(title.label,title.txt,title.scrollbar,sticky="nw",padx=c(0,5),pady=c(0,5))
  tkgrid.configure(title.scrollbar,sticky="news")
  
  # changing role entry
  role.combobox<-ttkcombobox(allFrame,values=itemrole,width=67,state="readonly",font=combo.font)
  tkset(role.combobox,item@role)
  role.label<-tklabel(allFrame,font=label.font,text="Role")
  tkgrid(role.label,role.combobox,sticky="nw",padx=c(0,5),pady=c(0,5))
  #----------------------------------------------------
  onRoleSelection<-function(...){
    role<-tclvalue(tkget(role.combobox))
    #---------------------------------------------------------------------------
    # Aktiviere/deaktiviere das Feld 'Strtum'
    #---------------------------------------------------------------------------
    if(role=="This item has been defined as outcome function (OF)"){
      # bei OF-Item keine Schichtung möglich!
      tkconfigure(stratum.combobox,state="readonly")
      tkset(stratum.combobox,stratums[1])
      tkconfigure(stratum.combobox,state="disabled")
      # OF-Items sind nur vom Typ "fnrv" (nicht geschichtet) erlaubt!
      tkconfigure(type.combobox,state="readonly")
      tkset(type.combobox,"Function of mcrv and other item(s) (fnrv)")
      tkconfigure(type.combobox,state="disabled")
    } else {
      #tkset(stratum.combobox,stratums[1])
      tkconfigure(stratum.combobox,state="readonly")
      tkconfigure(type.combobox,state="readonly")
    }
  }
  tkbind(role.combobox,"<<ComboboxSelected>>",onRoleSelection)
  
  
  # changing type entry
  type.combobox<-ttkcombobox(allFrame,values=itemtype,width=67,state="readonly",font=combo.font)
  tkset(type.combobox,item@type)
  type.label<-tklabel(allFrame,font=label.font,text="Type")
  tkgrid(type.label,type.combobox,sticky="nw",padx=c(0,5),pady=c(0,5))
  #----------------------------------------------------
  onTypeSelection<-function(...){
    type<-tclvalue(tkget(type.combobox))
    #---------------------------------------------------------------------------
    # Aktiviere/deaktiviere das Feld 'Stratum'
    #---------------------------------------------------------------------------
    if(type=="Numerical value(s) (numv)" |
       type=="Function of mcrv and other item(s) (fnrv)" |
       type=="Mixture distribution item (mxrv)" |
       type=="Monte-Carlo random variate (mcrv)"){
      tkconfigure(stratum.combobox,state="readonly")
    } else {
      tkset(stratum.combobox,stratums[1])
      tkconfigure(stratum.combobox,state="disabled")
    } # end if(type=="Numerical value(s) (numv)" | 
    #---------------------------------------------------------------------------
    # Aktiviere/deaktiviere und leere Felder 'Plausimin' und 'Plausimax'
    #---------------------------------------------------------------------------
    if(type=="Monte-Carlo random variate (mcrv)" |
       type=="Mixture distribution item (mxrv)" ){
       #type=="Resampling item (rsrv)" |
       #type=="Bootsrap item (bsrv)"){
       tkconfigure(min.entry,state="normal")
       tkconfigure(max.entry,state="normal")
    } else {
      tkconfigure(min.entry,state="disabled",text=tclVar(""))
      tkconfigure(max.entry,state="disabled",text=tclVar(""))
    } # end  if(type=="Monte-Carlo random variate (mcrv)" |
    #---------------------------------------------------------------------------
    # Setze Role-Feld auf 'u'
    #---------------------------------------------------------------------------
    if(type=="Bootsrap item (bsrv)"){
       tkset(role.combobox,itemrole[2])
       tkconfigure(role.combobox,state="disabled")
    } else {
      tkconfigure(role.combobox,state="readonly")
    } # end  if(type=="Bootsrap item (bsrv)"){
  } # end of fucntion onTypeSelection()
  
  tkbind(type.combobox,"<<ComboboxSelected>>",onTypeSelection)
  
  # changing stratum entry
  stratum.combobox<-ttkcombobox(allFrame,values=stratums,width=67,state="disabled",font=combo.font)
  tkset(stratum.combobox,item@stratum)
  stratum.label<-tklabel(allFrame,font=label.font,text="Stratum")
  tkgrid(stratum.label,stratum.combobox,sticky="nw",padx=c(0,5),pady=c(0,5))

  # changing part entry
  part.combobox<-ttkcombobox(allFrame,values=parts,width=67,state="readonly",font=combo.font)
  tkset(part.combobox,item@part)
  part.label<-tklabel(allFrame,font=label.font,text="Report part")
  tkgrid(part.label,part.combobox,sticky="nw",padx=c(0,5),pady=c(0,5))

  # changing explanation entry
  explanation.scrollbar<-tkscrollbar(allFrame,repeatinterval=1,command=function(...)tkyview(explanation.txt,...))
  explanation.txt<-tktext(allFrame,borderwidth=2,width=70,height=3,relief="sunken",
    yscrollcommand=function(...)tkset(explanation.scrollbar,...),wrap="word",font=combo.font)
  explanation.label<-tklabel(allFrame,font=label.font,text="Explanation")
  tkinsert(explanation.txt,"0.0",item@explanation)
  tkgrid(explanation.label,explanation.txt,explanation.scrollbar,sticky="nw",padx=c(0,5),pady=c(0,5))
  tkgrid.configure(explanation.scrollbar,sticky="news")
   
  # changing definition entry
  #definition.scrollbar<-tkscrollbar(allFrame,repeatinterval=1,command=function(...)tkyview(definition.txt,...))
  #definition.txt<-tktext(allFrame,relief="sunken",borderwidth=3,width=70,height=2,
  #  yscrollcommand=function(...)tkset(definition.scrollbar,...),wrap="word",font=combo.font)
  #tkinsert(definition.txt,"0.0",item@definition)
  #tkconfigure(definition.txt,state="disabled")
  #definition.label<-tklabel(allFrame,font=label.font,text="Definition")
  #tkgrid(definition.label,definition.txt,definition.scrollbar,sticky="nw",padx=c(0,5),pady=c(0,5))
  #tkgrid.configure(definition.scrollbar,sticky="news")
  
  # changing unit entry
  unit.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=tclVar(item@unit),font=combo.font)
  unit.label<-tklabel(allFrame,font=label.font,text="Unit")
  tkgrid(unit.label,unit.entry,sticky="nw",padx=c(0,5),pady=c(0,5))
  
  # changing min
  if(!is.null(item@plausimin))
  { plausiminText<-tclVar(item@plausimin)
  } else plausiminText<-tclVar("")
  if(tclvalue(tkget(type.combobox))=="Monte-Carlo random variate (mcrv)" |
     tclvalue(tkget(type.combobox))=="Mixture distribution item (mxrv)" ){
    min.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=plausiminText,font=combo.font,state="normal")
  } else {
    min.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=plausiminText,font=combo.font,state="disabled")
  }
  min.label<-tklabel(allFrame,font=label.font,text="Plausible min")
  tkgrid(min.label,min.entry,sticky="nw",padx=c(0,5),pady=c(0,5))
  
  # changing max
  if(!is.null(item@plausimax))
  { plausimaxText<-tclVar(item@plausimax)
  } else plausimaxText<-tclVar("")
  if(tclvalue(tkget(type.combobox))=="Monte-Carlo random variate (mcrv)" |
     tclvalue(tkget(type.combobox))=="Mixture distribution item (mxrv)" ){
    max.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=plausimaxText,font=combo.font,state="normal")
  } else {
    max.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=plausimaxText,font=combo.font,state="disabled")
  }
  #max.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=plausimaxText,font=combo.font,state="disabled")
  max.label<-tklabel(allFrame,font=label.font,text="Plausible max")
  tkgrid(max.label,max.entry,sticky="nw",padx=c(0,5),pady=c(0,5))
  
  # dependent items
  depitem.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=tclVar(item@depitem),font=combo.font,state="disabled")
  depitem.label<-tklabel(allFrame,font=label.font,text="Dependent items")
  tkgrid(depitem.label,depitem.entry,sticky="nw",padx=c(0,5),pady=c(0,5))
  
  # changing scores entry
  if(!is.null(item@scores))
  { scoresText<-tclVar(item@scores)
  } else  scoresText<-tclVar("")
  scores.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=scoresText,font=combo.font,state=scoresState)
  scores.label<-tklabel(allFrame,font=label.font,text="Scores")
  tkgrid(scores.label,scores.entry,sticky="nw",padx=c(0,5),pady=c(0,5))
  
  # changing assumptions entry
  assumptions.scrollbar<-tkscrollbar(allFrame,repeatinterval=1,command=function(...)tkyview(assumptions.txt,...))
  assumptions.txt<-tktext(allFrame,relief="sunken",borderwidth=3,width=70,height=3,
    yscrollcommand=function(...)tkset(assumptions.scrollbar,...),wrap="word",font=combo.font)
  tkinsert(assumptions.txt,"0.0",item@assumptions)
  assumptions.label<-tklabel(allFrame,font=label.font,text="Assumptions")
  tkgrid(assumptions.label,assumptions.txt,assumptions.scrollbar,sticky="nw",padx=c(0,5),pady=c(0,5))
  tkgrid.configure(assumptions.scrollbar,sticky="news")
  
  # changing remark entry
  remark.scrollbar<-tkscrollbar(allFrame,repeatinterval=1,command=function(...)tkyview(remark.txt,...))
  remark.txt<-tktext(allFrame,relief="sunken",borderwidth=3,width=70,height=3,
    yscrollcommand=function(...)tkset(remark.scrollbar,...),wrap="word",font=combo.font)
  tkinsert(remark.txt,"0.0",item@remark)
  remark.label<-tklabel(allFrame,font=label.font,text="Remark")
  tkgrid(remark.label,remark.txt,remark.scrollbar,sticky="nw",padx=c(0,5),pady=c(0,5))
  tkgrid.configure(remark.scrollbar,sticky="news")
  
  # changing reference
  reference.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=tclVar(item@reference),font=combo.font)
  reference.label<-tklabel(allFrame,font=label.font,text="Reference")
  tkgrid(reference.label,reference.entry,sticky="nw",padx=c(0,5),pady=c(0,5))
  
  # full command
  #fullc.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=tclVar(item@fullc),font=combo.font,state="disabled")
  #fullc.label<-tklabel(allFrame,font=label.font,text="Full command")
  #tkgrid(fullc.label,fullc.entry,sticky="nw",padx=c(0,5),pady=c(0,5))
  
  # relax command
  #relaxc.entry<-tkentry(allFrame,relief="sunken",borderwidth=3,width=70,text=tclVar(item@relaxc),font=combo.font,state="disabled")
  #relaxc.label<-tklabel(allFrame,font=label.font,text="relax command")
  #tkgrid(relaxc.label,relaxc.entry,sticky="nw",padx=c(0,5),pady=c(0,5))
  
  # defining buttons
  buttonsFrame<-tkframe(allFrame)
  okButton<-ttkbutton(buttonsFrame,width=10,text="Ok",command=onOk)
  clearButton<-ttkbutton(buttonsFrame,width=10,text="Clear",command=onClear)
  resetButton<-ttkbutton(buttonsFrame,width=10,text="Reset",command=onReset)
  cancelButton<-ttkbutton(buttonsFrame,width=10,text="Cancel",command=onCancel)
  tkpack(okButton,clearButton,resetButton,cancelButton,side="left",padx=c(0,15))
  tkgrid(buttonsFrame,pady=c(10,0),column=1)

  tkpack(allFrame,padx=c(15,15),pady=c(15,15))

  tkfocus(itemChangeWindow)
  tkfocus(okButton)
  tkwait.window(itemChangeWindow)

  #-------------------------------------------------------------------------------
  # generate output, after the window has been destroyed
  #-------------------------------------------------------------------------------
  # hole Item-Objektmit geänderten Slots außer 'fullc', 'relaxc' und 'definition'
  item<-get("item",envir=tempEnvir)
  button<-get("button",envir=tempEnvir)
  
  if(button=="ok")
  {
    # Füge geänderten Item zum rriskModel hinzu (die drei Slots 'fullc', 'relaxc' und 'definition' sind NOCH NICHT geändert worden!)
    rriskModel@items@items[[index]]<-item
  
    # Nun werden Funktionen aufgerufen, die Slots 'fullc', 'relaxc' und 'definition' entsprechend dem Item-Type ändern
    if(item@typecode=="fdoc")
    { item<-define.fdoc(item)
      rriskModel@items@items[[index]]<-item
    } else if(item@typecode=="numv")
    { item<-define.numv.fnrv(item,rriskModel,menuLevel=menuLevel,type="numv")
      rriskModel@items@items[[index]]<-item
    } else if(item@typecode=="fnrv")
    { item<-define.numv.fnrv(item,rriskModel,menuLevel=menuLevel,type="fnrv")
      rriskModel@items@items[[index]]<-item
    } else if(item@typecode=="data")          
    { item<-menu.define.data(item,menuLevel=menuLevel)
      rriskModel@items@items[[index]]<-item
    } else if(item@typecode=="mcrv")
    { item<-menu.define.mcrv(item,rriskModel,menuLevel=menuLevel)
      rriskModel@items@items[[index]]<-item
    } else if (item@typecode=="bsrv")
    { item<-menu.define.bsrv(item,rriskModel,menuLevel=menuLevel)
      rriskModel@items@items[[index]]<-item
    } else if(item@typecode=="rsrv")
    { item<-menu.define.rsrv(item,rriskModel,menuLevel=menuLevel)
      rriskModel@items@items[[index]]<-item
    } else if(item@typecode=="bdjp")
    { # hier werden zwei Items zurückgeliefert: der neue bdjp-Item und der data-Items, in dem die Ergebnisse abgespeichert wurden
      items<-menu.define.bdjp(item,rriskModel,menuLevel=menuLevel)
      rriskModel@items@items[[index]]<-items[["bdjpItem"]]
      if(!is.null(items[["bdjpDataitem"]])){
        rriskModel@items@items[[length(rriskModel@items@items)+1]]<-items[["bdjpDataitem"]]
      }
    } else if(item@typecode=="stra") {
      item<-menu.define.stra(item,rriskModel,menuLevel=menuLevel)
      rriskModel@items@items[[index]]<-item
    } else if(item@typecode=="mxrv"){
      if(item@stratum!=""){
        item<-menu.define.mxrv(item,rriskModel,menuLevel=menuLevel)
      } else if(item@stratum==""){
        cat("\nStratum slot of your 'mxrv' item is empty, further definition is not possible!\n")
      }
      rriskModel@items@items[[index]]<-item
    } # end if(item@typecode=="fdoc")  
    #else if(item@typecode=="corv"){
       #cat("\nSorry, this option is still under evaluation\n")        
    #}
    
    #---------------------------------------------------------------------------
    # und nun werden die depitem-Slots von allen voranstehenden Items aktualisiert
    #---------------------------------------------------------------------------
    if(item@typecode!="bdjp"){
      items<-rriskModel@items@items[1:(index-1)]
      items<-setDepItems(item,items)
      rriskModel@items@items[1:(index-1)]<-items
    } else {
      #items<-rriskModel@items@items[1:(index-2)]
      items<-rriskModel@items@items[1:(index-1)]
      items<-setDepItems(item=rriskModel@items@items[[index]],items)
      rriskModel@items@items[1:(index-1)]<-items
    } # end if(item@typecode!="bdjp"){ 
  
    #---------------------------------------------------------------------------
    # und hier der Slot "item" des entsprechenden Part aktualisiert
    #---------------------------------------------------------------------------
    if(length(rriskModel@parts@parts)>0 & item@part!=""){
      for(i in 1:length(rriskModel@parts@parts)){
        if(rriskModel@parts@parts[[i]]@name==item@part){
          rriskModel@parts@parts[[i]]@items<-paste(rriskModel@parts@parts[[i]]@items,item@name,sep=" ") 
        } # end if(rriskModel@parts@parts[[i]]@name==item@part){
      } # end for(i in 1:length(rriskModel@parts@parts)){
    }# end   if(length(rriskModel@parts@parts)>0 und item@part!="")
  } # end if(button=="ok")
  
  return(rriskModel)
} # end of function change.item()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelScoringClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.scoring
#' @aliases change.scoring
#' @title Function for editing model slot 'scoring'
#' @usage change.scoring(scoring,availableSsystems)
#' @param scoring (model slot \code{scoring}) is an object of the class \code{modelScoringClass}
#' @param availableSsystems is a character vector containing scoring system names that are available in the current rrisk session
#' @return This function returns edited model scoring system (slot \code{scoring}).
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{scoringSystem<-init.scoresystem1()
#' # It is not possible to call current scoring system by "my scoring system"
#' scoringSystem<-change.scoring(scoringSystem,availableSsystems=c("my scoring system"))
#' scoringSystem}

change.scoring<-function(scoring,availableSsystems=c())
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("scoring.temp",get("scoring.original",tempEnvir),tempEnvir)
    tkdestroy(changeScoringWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  { index<-get("index",tempEnvir)
    scoring.temp<-get("scoring.temp",envir=tempEnvir)
    scores.temp<-get("scores.temp",envir=tempEnvir)
    availableSsystems<-get("availableSsystems",envir=tempEnvir)

    #---------------------------------------------------------------------------
    # check system name input
    #---------------------------------------------------------------------------
    systemname<-tclvalue(tkget(systemname.entry))
    systemname.check<-gsub(x=systemname," ",replacement="")
    if(systemname.check=="")
    { tkmessageBox(message="The 'Scoring system name' field is empty, please enter here the scoring system name!",icon="error")
      tkfocus(systemname.entry)
      stop("INVALID INPUT, empty field 'Scoring system name'...",call.=FALSE)
    } else if(is.element(systemname,availableSsystems))
    { tkmessageBox(message="The name of the scoring system should be different to names of existing scoring systems!",icon="error")
      tkfocus(systemname.entry)
      stop("INVALID INPUT, the name of the scoring system should be different to names of existing scoring systems...",call.=FALSE)
    }
    
    #---------------------------------------------------------------------------
    # check table header
    #---------------------------------------------------------------------------
    tableheader<-tclvalue(tkget(tableheader.txt,"0.0","end"))
    tableheader<-gsub(x=tableheader,"\n",replacement="")
    tableheader<-gsub(x=tableheader,"\t",replacement="")
    tableheader.check<-gsub(x=tableheader," ",replacement="")
    if(tableheader.check=="")
    { tkmessageBox(message="The 'Table header' field is empty, please enter here table header!",icon="error")
      tkfocus(tableheader.txt)
      stop("INVALID INPUT, empty field 'Table header'...",call.=FALSE)
    }
    
    #---------------------------------------------------------------------------
    # check explanatory
    #---------------------------------------------------------------------------
    explanatory<-tclvalue(tkget(explanatory.txt,"0.0","end"))
    explanatory<-gsub(x=explanatory,"\n",replacement="")
    explanatory<-gsub(x=explanatory,"\t",replacement="")
    
    #---------------------------------------------------------------------------
    # check values input
    #---------------------------------------------------------------------------
    values<-tclvalue(tkget(values.entry))
    values.check<-gsub(x=values," ",replacement="")
    if(values.check=="")
    { tkmessageBox(message="The 'Scoring system values' field is empty, please enter here the scoring system values (separated by blank)!",icon="error")
      tkfocus(values.entry)
      stop("INVALID INPUT, empty field 'Scoring system values'...",call.=FALSE)
    } else
    { values<-strsplit(values,split=" ")[[1]]
      values<-setdiff(values,"")
      values<-as.numeric(values)
    }
    
    #---------------------------------------------------------------------------
    # check vcolors input
    #---------------------------------------------------------------------------
    vcolors<-tclvalue(tkget(vcolors.entry))
    vcolors.check<-gsub(x=vcolors," ",replacement="")
    if(vcolors.check=="")
    { tkmessageBox(message="The 'Scoring system colors' field is empty, please enter here the scoring system colors (separated by blank)!",icon="error")
      tkfocus(vcolors.entry)
      stop("INVALID INPUT, empty field 'Scoring system colors'...",call.=FALSE)
    } else
    { vcolors<-strsplit(vcolors,split=" ")[[1]]
      vcolors<-setdiff(vcolors,"")
      if(length(values)!=length(vcolors))
      { tkmessageBox(message="Inputs in fields 'Scoring system colors' and 'Scoring system values' are not of the same length!",icon="error")
        tkfocus(vcolors.entry)
        stop("INVALID INPUT, Inputs in fields 'Scoring system colors' and 'Scoring system values' are not of the same length...",call.=FALSE)
      } else
      vcolors.temp<-vcolors
      vcolors<-values
      names(vcolors)<-vcolors.temp
    }
    
    #---------------------------------------------------------------------------
    # check vcolors input
    #---------------------------------------------------------------------------
    vmeanings<-tclvalue(tkget(vmeanings.entry))
    vmeanings.check<-gsub(x=vmeanings," ",replacement="")
    if(vmeanings.check=="")
    { tkmessageBox(message="The 'Scoring system meanings' field is empty, please enter here the scoring system colors (separated by blank)!",icon="error")
      tkfocus(vmeanings.entry)
      stop("INVALID INPUT, empty field 'Scoring system meanings'...",call.=FALSE)
    } else
    { vmeanings<-strsplit(vmeanings,split=" ")[[1]]
      vmeanings<-setdiff(vmeanings,"")
      if(length(values)!=length(vmeanings))
      { tkmessageBox(message="Inputs in fields 'Scoring system meanings' and 'Scoring system values' are not of the same length!",icon="error")
        tkfocus(vmeanings.entry)
        stop("INVALID INPUT, Inputs in fields 'Scoring system meanings' and 'Scoring system values' are not of the same length...",call.=FALSE)
      } else
      vmeanings.temp<-vmeanings
      vmeanings<-values
      names(vmeanings)<-vmeanings.temp
    }

    #---------------------------------------------------------------------------
    # fill scoring with new values
    #---------------------------------------------------------------------------
    scoring.temp@name<-systemname
    scoring.temp@values<-values
    scoring.temp@vcolors<-vcolors
    scoring.temp@vmeanings<-vmeanings
    scoring.temp@tableheader<-tableheader
    scoring.temp@explanatory<-explanatory
    
    #---------------------------------------------------------------------------
    # check score data
    #---------------------------------------------------------------------------
    notation<-tclvalue(tkget(notation.entry))
    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    #--------
    notation.check<-gsub(x=notation," ",replacement="")
    name.check<-gsub(x=name," ",replacement="")
    #-------
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")
    if(notation.check=="" & name.check=="" & explanation.check=="")
    { tkmessageBox(message="All score corresponding fields are empty. This score item will be removed from the list of scores!",icon="warning")
      scores.temp<-scores.temp[-index]
    } else if(notation.check=="" & any(name.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Score symbol' field is empty, please enter here score symbol!",icon="error")
      tkfocus(notation.entry)
      stop("INVALID INPUT, empty field 'Score notation'...",call.=FALSE)
    } else if(name.check=="" & any(notation.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Score name' field is empty, please enter here score name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Score name'...",call.=FALSE)
    } else if(explanation.check=="" & any(notation.check!="",name.check!=""))
    { tkmessageBox(message="The 'Score explanation' field is empty, please enter here score explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Score explanation'...",call.=FALSE)
    } else
    {
      if(index>length(scores.temp))
      { scores.temp[[index]]<-new("scoreClass",
        notation=notation,
        name=name,
        explanation=explanation)
      } else
      { scores.temp[[index]]@notation<-notation
        scores.temp[[index]]@name<-name
        scores.temp[[index]]@explanation<-explanation
      }
    }
    if(length(scores.temp)==0)  scores.temp<-list()

    assign("scores.temp",scores.temp,tempEnvir)
    assign("scoring.temp",scoring.temp,tempEnvir)
    tkdestroy(changeScoringWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { index<-get("index",tempEnvir)
    scores.temp<-get("scores.temp",envir=tempEnvir)

    notation<-tclvalue(tkget(notation.entry))
    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    notation.check<-gsub(x=notation," ",replacement="")
    name.check<-gsub(x=name," ",replacement="")
    
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(notation.check=="" & name.check=="" & explanation.check=="")
    { tkmessageBox(message="All score corresponding fields are empty. This score item will be removed from the list of scores!",icon="warning")
      scores.temp<-scores.temp[-index]
    } else if(notation.check=="" & any(name.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Score symbol' field is empty, please enter here score symbol!",icon="error")
      tkfocus(notation.entry)
      stop("INVALID INPUT, empty field 'Score notation'...",call.=FALSE)
    } else if(name.check=="" & any(notation.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Score name' field is empty, please enter here score name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Score name'...",call.=FALSE)
    } else if(explanation.check=="" & any(notation.check!="",name.check!=""))
    { tkmessageBox(message="The 'Score explanation' field is empty, please enter here score explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Score explanation'...",call.=FALSE)
    } else
    { if(index>length(scores.temp))
      { scores.temp[[index]]<-new("scoreClass",
        notation=notation,
        name=name,
        explanation=explanation)
      } else
      { scores.temp[[index]]@notation<-notation
        scores.temp[[index]]@name<-name
        scores.temp[[index]]@explanation<-explanation
      }
    }

    index<-index-1
    assign("scores.temp",value=scores.temp,envir=tempEnvir)
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(notation.entry,text=tclVar(scores.temp[[index]]@notation))
    tkconfigure(name.entry,text=tclVar(scores.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",scores.temp[[index]]@explanation)


    if(index==length(scores.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(scores.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(scores.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(scores.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(scores.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    scores.temp<-get("scores.temp",envir=tempEnvir)

    notation<-tclvalue(tkget(notation.entry))
    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
 
    notation.check<-gsub(x=notation," ",replacement="") 
    name.check<-gsub(x=name," ",replacement="")

    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(notation.check=="" & name.check=="" & explanation.check=="")
    { tkmessageBox(message="All score corresponding fields are empty. This score item will be removed from the list of scores!",icon="warning")
      scores.temp<-scores.temp[-index]
    } else if(notation.check=="" & any(name.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Score symbol' field is empty, please enter here score symbol!",icon="error")
      tkfocus(notation.entry)
      stop("INVALID INPUT, empty field 'Score notation'...",call.=FALSE)
    } else if(name.check=="" & any(notation.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Score name' field is empty, please enter here score name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Score name'...",call.=FALSE)
    } else if(explanation.check=="" & any(notation.check!="",name.check!=""))
    { tkmessageBox(message="The 'Score explanation' field is empty, please enter here score explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Score explanation'...",call.=FALSE)
    } else
    { if(index>length(scores.temp))
      { scores.temp[[index]]<-new("scoreClass",
        notation=notation,
        name=name,
        explanation=explanation)
      } else
      { scores.temp[[index]]@notation<-notation
        scores.temp[[index]]@name<-name
        scores.temp[[index]]@explanation<-explanation
      }
    }

    assign("scores.temp",value=scores.temp,envir=tempEnvir)
    index<-index+1
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(notation.entry,text=tclVar(scores.temp[[index]]@notation))
    tkconfigure(name.entry,text=tclVar(scores.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",scores.temp[[index]]@explanation)

     if(index==length(scores.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(scores.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(scores.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(scores.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(scores.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  { index<-get("index",tempEnvir)
    scores.temp<-get("scores.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    notation<-tclvalue(tkget(notation.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    notation.check<-gsub(x=notation," ",replacement="")
    name.check<-gsub(x=name," ",replacement="")
    
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

   if(notation.check=="" & name.check=="" & explanation.check=="")
    { tkmessageBox(message="All score corresponding fields are empty. This score item will be removed from the list of scores!",icon="warning")
      scores.temp<-scores.temp[-index]
    } else if(notation.check=="" & any(name.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Score symbol' field is empty, please enter here score symbol!",icon="error")
      tkfocus(notation.entry)
      stop("INVALID INPUT, empty field 'Score notation'...",call.=FALSE)
    } else if(name.check=="" & any(notation.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Score name' field is empty, please enter here score name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Score name'...",call.=FALSE)
    } else if(explanation.check=="" & any(notation.check!="",name.check!=""))
    { tkmessageBox(message="The 'Score explanation' field is empty, please enter here score explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Score explanation'...",call.=FALSE)
    } else
    { if(index>length(scores.temp))
      { scores.temp[[index]]<-new("scoreClass",
        notation=notation,
        name=name,
        explanation=explanation)
      } else
      { scores.temp[[index]]@notation<-notation
        scores.temp[[index]]@name<-name
        scores.temp[[index]]@explanation<-explanation
      }
      assign("scores.temp",value=scores.temp,envir=tempEnvir)
      assign("index",value=index+1,envir=tempEnvir)
    }

    tkconfigure(notation.entry,text=tclVar(""))
    tkconfigure(name.entry,text=tclVar(""))
    tkdelete(explanation.txt,"0.0","end")

    if(index==length(scores.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(scores.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(scores.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(scores.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(scores.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  {
    sureDelete<-tkmessageBox(message="Are you sure to remove this score from the scoring system?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no")
    { stop("",call.=FALSE)
    } else if(sureDelete=="yes")
    { index<-get("index",tempEnvir)
      scores.temp<-get("scores.temp",tempEnvir)

      if(index>1)
      { scores.temp<-scores.temp[-index]
        index<-index-1
        tkconfigure(notation.entry,text=tclVar(scores.temp[[index]]@notation))
        tkconfigure(name.entry,text=tclVar(scores.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",scores.temp[[index]]@explanation)
      } else if(index==1 & index!=length(scores.temp))
      { scores.temp<-scores.temp[-index]
        tkconfigure(notation.entry,text=tclVar(scores.temp[[index]]@notation))
        tkconfigure(name.entry,text=tclVar(scores.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",scores.temp[[index]]@explanation)
      } else if (index==1 & index==length(scores.temp))
      { scores.temp<-list()
        tkconfigure(notation.entry,text=tclVar(""))
        tkconfigure(name.entry,text=tclVar(""))
        tkdelete(explanation.txt,"0.0","end")
      }

      assign("scores.temp",scores.temp,tempEnvir)
      assign("index",index,tempEnvir)

      if(index==length(scores.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(scores.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(scores.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(scores.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(scores.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object1<-demoModel1@scoring
  #scoring.temp<-object1
  
  scoring.temp<-scoring
  scores.temp<-scoring.temp@scoring
  
  #-----------------------------------------------------------------------------
  # get names of available scoring systems
  #-----------------------------------------------------------------------------
  #availableSsystems<-c()
  #for(i in 1:length(rriskSession@scoringsystems))
  #{ availableSsystemsTemp<-rriskSession@scoringsystems[[i]]@name
  #  availableSsystems<-c(availableSsystems,availableSsystemsTemp)
  #}
  #availableSsystems<-setdiff(availableSsystems,scoring.temp@name)
  
  if(length(scores.temp)==0) scores.temp<-list(new("scoreClass"))

  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("scoring.temp",value=scoring.temp,envir=tempEnvir)
  assign("scores.temp",value=scores.temp,envir=tempEnvir)
  assign("scoring.original",value=scoring.temp,envir=tempEnvir)
  assign("availableSsystems",value=availableSsystems,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeScoringWindow<-tktoplevel()
  tkwm.title(changeScoringWindow,"Edit scoring system")
  tkwm.resizable(changeScoringWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeScoringWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)
  index<-get("index",envir=tempEnvir)
  fixedFrame<-tkframe(allFrame)
  flexFrame<-tkframe(allFrame)

  # changing name entry
  systemname.entry<-tkentry(fixedFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(scoring.temp@name),font=combo.font)
  systemname.label<-tklabel(fixedFrame,font=label.font,text="Name of the scoring system")
  tkgrid(systemname.label,systemname.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  
  # changing values entry 
    # changing min
  if(!is.null(scoring.temp@values))
  { scoringvalues<-tclVar(scoring.temp@values)
  } else scoringvalues<-tclVar("")
  values.entry<-tkentry(fixedFrame,borderwidth=2,width=70,relief="sunken",text=scoringvalues,font=combo.font)
  #values.entry<-tkentry(fixedFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(scoring.temp@values),font=combo.font)
  values.label<-tklabel(fixedFrame,font=label.font,text="Scoring values")
  tkgrid(values.label,values.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  
  # changing vcolors entry
  if(!is.null(scoring.temp@vcolors))
  { scoringvcolors<-tclVar(names(scoring.temp@vcolors))
  } else scoringvcolors<-tclVar("")
  vcolors.entry<-tkentry(fixedFrame,borderwidth=2,width=70,relief="sunken",text=scoringvcolors,font=combo.font)
  #vcolors.entry<-tkentry(fixedFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(names(scoring.temp@vcolors)),font=combo.font)
  vcolors.label<-tklabel(fixedFrame,font=label.font,text="Scoring colors")
  tkgrid(vcolors.label,vcolors.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  
  # changing vmeanings entry
  if(!is.null(scoring.temp@vmeanings))
  { scoringvmeanings<-tclVar(names(scoring.temp@vmeanings))
  } else scoringvmeanings<-tclVar("")
  vmeanings.entry<-tkentry(fixedFrame,borderwidth=2,width=70,relief="sunken",text=scoringvmeanings,font=combo.font)
  #vmeanings.entry<-tkentry(fixedFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(names(scoring.temp@vmeanings)),font=combo.font)
  vmeanings.label<-tklabel(fixedFrame,font=label.font,text="Scoring meanings")
  tkgrid(vmeanings.label,vmeanings.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  
  # explanation for scoring system
  explanatory.scrollbar<-tkscrollbar(fixedFrame,repeatinterval=1,command=function(...)tkyview(explanatory.txt,...))
  explanatory.txt<-tktext(fixedFrame,borderwidth=2,width=70,height=5,relief="sunken",
    yscrollcommand=function(...)tkset(explanatory.scrollbar,...),wrap="word",font=combo.font)
  tkinsert(explanatory.txt,"0.0",scoring.temp@explanatory)
  explanatory.label<-tklabel(fixedFrame,font=label.font,text="Explanation")
  tkgrid(explanatory.label,explanatory.txt,explanatory.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,25))
  tkgrid.configure(explanatory.scrollbar,sticky="news")

  # table header for scoring system
  tableheader.scrollbar<-tkscrollbar(fixedFrame,repeatinterval=1,command=function(...)tkyview(tableheader.txt,...))
  tableheader.txt<-tktext(fixedFrame,borderwidth=2,width=70,height=5,relief="sunken",
    yscrollcommand=function(...)tkset(tableheader.scrollbar,...),wrap="word",font=combo.font)
  tkinsert(tableheader.txt,"0.0",scoring.temp@tableheader)
  tableheader.label<-tklabel(fixedFrame,font=label.font,text="Table header")
  tkgrid(tableheader.label,tableheader.txt,tableheader.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,25))
  tkgrid.configure(tableheader.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------
  
  # changing score notation
  notation.entry<-tkentry(fixedFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(scores.temp[[1]]@notation),font=combo.font)
  notation.label<-tklabel(fixedFrame,font=label.font,text="Score symbol")
  tkgrid(notation.label,notation.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
   
  # changing score name
  name.entry<-tkentry(fixedFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(scores.temp[[1]]@name),font=combo.font)
  name.label<-tklabel(fixedFrame,font=label.font,text="Score name")
  tkgrid(name.label,name.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  
  # changing score explanation
  explanation.scrollbar<-tkscrollbar(fixedFrame,repeatinterval=1,command=function(...)tkyview(explanation.txt,...))
  explanation.txt<-tktext(fixedFrame,borderwidth=2,width=70,height=5,relief="sunken",
    yscrollcommand=function(...)tkset(explanation.scrollbar,...),wrap="word",font=combo.font)
  tkinsert(explanation.txt,"0.0",scores.temp[[1]]@explanation)
  explanation.label<-tklabel(fixedFrame,font=label.font,text="Score explanation")
  tkgrid(explanation.label,explanation.txt,explanation.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkgrid.configure(explanation.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------

  tkpack(fixedFrame,side="top")
  #-----------------------------------------------------------------------------
  
  # button frame
  if(length(scores.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(scores.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(prevButton,nextButton,addButton,deleteButton,padx=c(15,0))

  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))
  #-----------------------------------------------------------------------------

  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))
  #-----------------------------------------------------------------------------
  
  tkfocus(changeScoringWindow)
  tkwait.window(changeScoringWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-get("scoring.temp",tempEnvir)
  output@scoring<-get("scores.temp",tempEnvir)
  return(output)
} # end of function change.scoring()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelGraphsClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.graphs
#' @aliases change.graphs
#' @title Function for edditing model slot 'graphs'
#' @usage change.graphs(graphs)
#' @param graphs (model graphs) is an object of class \code{modelGraphsClass}
#' @return This function returns edited model slot 'graphs'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@graphs<-change.graphs(model@@graphs)
#' model@@graphs}

change.graphs<-function(graphs)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("graphs.temp",get("graphs.original",tempEnvir),tempEnvir)
    tkdestroy(changeGraphsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  { index<-get("index",tempEnvir)
    graphs.temp<-get("graphs.temp",envir=tempEnvir)
    graphData<-get("graphData",envir=tempEnvir)

    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(explanation.check=="")
    { tkmessageBox(message="The field 'Explanation' is empty. This concept graph will be removed from the graphs list!",icon="warning")
      #tkfocus(explanation.txt)
      tkdestroy(changeGraphsWindow)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)    
    } else
    { if(index>length(graphs.temp))
      { graphs.temp[[index]]<-new("graphClass",
        explanation=explanation,
        graphdata=graphData)
      } else if(index <= length(graphs.temp) & length(graphs.temp)>0)
      { graphs.temp[[index]]@explanation<-explanation
        if(!is.null(graphData)) graphs.temp[[index]]@graphdata<-graphData
      }
    }

    if(length(graphs.temp)==0)
    { graphs.temp<-list()
    } else
    { graphs.temp<-lapply(graphs.temp,function(x){
        explanation.temp<-x@explanation
        graphdata.temp<-x@graphdata
        explanation.temp<-gsub(x=explanation.temp,"\n",replacement="")
        explanation.temp<-gsub(x=explanation.temp,"\t",replacement="")
        new("graphClass",explanation=explanation.temp,graphdata=graphdata.temp)
      })
    }

    assign("graphs.temp",graphs.temp,tempEnvir)
    assign("graphData",NULL,tempEnvir)
    tkdestroy(changeGraphsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { index<-get("index",tempEnvir)
    graphs.temp<-get("graphs.temp",envir=tempEnvir)
    graphData<-get("graphData",envir=tempEnvir)

    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(explanation.check=="" )
    { tkmessageBox(message="The field 'Explanation' is empty. This concept graph will be removed from the graphs list!",icon="warning")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(graphs.temp))
      { graphs.temp[[index]]<-new("graphClass",
        explanation=explanation,
        graphdata=graphData)
      } else
      { graphs.temp[[index]]@explanation<-explanation
        if(!is.null(graphData)) graphs.temp[[index]]@graphdata<-graphData
      }
    }
    index<-index-1
    assign("graphs.temp",value=graphs.temp,envir=tempEnvir)
    assign("index",value=index,envir=tempEnvir)
    assign("graphData",NULL,tempEnvir)

    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",graphs.temp[[index]]@explanation)

    if(index==length(graphs.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(graphs.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(graphs.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(graphs.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(graphs.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    graphs.temp<-get("graphs.temp",envir=tempEnvir)
    graphData<-get("graphData",envir=tempEnvir)

    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(explanation.check=="" )
    { tkmessageBox(message="The field 'Explanation' is empty. This concept graph will be removed from the graphs list!",icon="warning")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(graphs.temp))
      { graphs.temp[[index]]<-new("graphClass",
        explanation=explanation,
        graphdata=graphData)
      } else
      { graphs.temp[[index]]@explanation<-explanation
        if(!is.null(graphData)) graphs.temp[[index]]@graphdata<-graphData
      }
    }

    assign("graphs.temp",value=graphs.temp,envir=tempEnvir)
    index<-index+1
    assign("index",value=index,envir=tempEnvir)
    assign("graphData",NULL,tempEnvir)

    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",graphs.temp[[index]]@explanation)

     if(index==length(graphs.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(graphs.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(graphs.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(graphs.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(graphs.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  {
    index<-get("index",tempEnvir)
    graphs.temp<-get("graphs.temp",envir=tempEnvir)
    graphData<-get("graphData",envir=tempEnvir)

    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(explanation.check=="")
    { tkmessageBox(message="The field 'Explanation' is empty. This concept graph will be removed from the graphs list!",icon="warning")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(graphs.temp))
      { graphs.temp[[index]]<-new("graphClass",
        explanation=explanation,
        graphdata=graphData)
      } else
      { graphs.temp[[index]]@explanation<-explanation
        if(!is.null(graphData)) graphs.temp[[index]]@graphdata<-graphData
      }

    }
     assign("graphs.temp",value=graphs.temp,envir=tempEnvir)
     assign("index",value=index+1,envir=tempEnvir)
     assign("graphData",NULL,tempEnvir)

    tkdelete(explanation.txt,"0.0","end")

    if(index==length(graphs.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(graphs.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(graphs.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(graphs.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(graphs.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  {
    sureDelete<-tkmessageBox(message="Are you sure to delete this concept graph?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no"){
      stop("",call.=FALSE)
    } else if(sureDelete=="yes"){
      index<-get("index",tempEnvir)
      graphs.temp<-get("graphs.temp",tempEnvir)

      if(index>1)
      { graphs.temp<-graphs.temp[-index]
        index<-index-1
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",graphs.temp[[index]]@explanation)
      } else if(index==1 & index!=length(graphs.temp))
      { graphs.temp<-graphs.temp[-index]
        if(length(graphs.temp)>0){
          tkdelete(explanation.txt,"0.0","end")
          tkinsert(explanation.txt,"0.0",graphs.temp[[index]]@explanation)
        }
      } else if (index==1 & index==length(graphs.temp))
      { graphs.temp<-list()
        tkdelete(explanation.txt,"0.0","end")
      }

      assign("graphs.temp",graphs.temp,tempEnvir)
      assign("index",index,tempEnvir)
      assign("graphData",NULL,tempEnvir)

      if(index==length(graphs.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(graphs.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(graphs.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(graphs.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(graphs.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }
  
  #-----------------------------------------------------------------------------
  # what happends by pressing "load picture" button
  #-----------------------------------------------------------------------------
  onLoad<-function(...)
  {
    graphData<-graphLoad()
    assign("graphData",value=graphData,tempEnvir)

  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object1<-demoModel1@graphs
  #object<-object1

  graphs.temp<-graphs@graphs
  if(length(graphs.temp)==0) graphs.temp<-list(new("graphClass",explanation=""))
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("graphData",value=NULL,envir=tempEnvir)
  assign("graphs.temp",value=graphs.temp,envir=tempEnvir)
  assign("graphs.original",value=graphs.temp,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeGraphsWindow<-tktoplevel()
  tkwm.title(changeGraphsWindow,"Load and edit description of model concept graph(s)")
  tkwm.resizable(changeGraphsWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeGraphsWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  textFrame<-tkframe(allFrame)

  # changing explanation entry
  explanation.txt<-tktext(textFrame,borderwidth=2,width=70,height=15,relief="sunken",wrap="word")
  explanation.label<-tklabel(textFrame,font=label.font,text="Legend")
  tkinsert(explanation.txt,"0.0",graphs.temp[[1]]@explanation)
  tkgrid(explanation.label,explanation.txt,sticky="nw",padx=c(0,10),pady=c(0,5))

  tkpack(textFrame,side="top")

  # button frame
  if(length(graphs.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(graphs.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  loadButton<-ttkbutton(buttonsFrame1,text="load picture",width=12,command=onLoad)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(loadButton,prevButton,nextButton,addButton,deleteButton,padx=c(15,0))

  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))

  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))

  tkfocus(changeGraphsWindow)
  tkwait.window(changeGraphsWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  graphs.output<-get("graphs.temp",tempEnvir)
  # nur die erste grafik soll als cover-grafik verwendet werden
  if(length(graphs.output)>0){
    for(i in 1:length(graphs.output)){
      if(i==1){
        graphs.output[[i]]@cover<-TRUE
      } else {
        graphs.output[[i]]@cover<-FALSE
      }
    }
  }
  output<-new("modelGraphsClass",graphs=graphs.output)
  return(output)
} # end of function change.graphs()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelPartsClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.parts
#' @aliases change.parts
#' @title Function for edditing model slots 'parts' and 'items'
#' @usage change.parts(parts,items)
#' @param parts (model parts) is an object of class \code{modelPartsClass}
#' @param items (model items) is an object of class \code{modelItemsClass}
#' @return This function returns a list containing edited model parts and items.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' results<-change.parts(model@@parts,model@@items)
#' results}




change.parts<-function(parts,items)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("parts.temp",get("parts.original",tempEnvir),tempEnvir)
    assign("items.temp",get("items.original",tempEnvir),tempEnvir)
    tkdestroy(changePartsWindow)
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  {
    index<-get("index",tempEnvir)
    parts.temp<-get("parts.temp",envir=tempEnvir)
    items.temp<-get("items.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    items<-tclvalue(tkget(items.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")
    items<-gsub(x=items,"\n",replacement="")
    items<-gsub(x=items,"\t",replacement="")
    items.check<-gsub(x=items," ",replacement="")

    if(name.check=="" & explanation.check=="")
    { tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      parts.temp<-parts.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the part name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the part explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'explanation'...",call.=FALSE)
    } else
    {
      if(index>length(parts.temp))
      { parts.temp[[index]]<-new("partClass",
        name=name,
        explanation=explanation,
        items=items)
      } else
      { parts.temp[[index]]@name<-name
        parts.temp[[index]]@explanation<-explanation
        parts.temp[[index]]@items<-items
      }
      #-------------------------------------------------------------------------
      if(length(items.temp)>0)
      { partItems<-strsplit(parts.temp[[index]]@items,split=" ")[[1]]
        partItems<-gsub(x=partItems," ",replacement="")
        for(i in 1:length(items.temp))
        { if(is.element(items.temp[[i]]@name,partItems)) items.temp[[i]]@part<-name
        }
        tkselection.clear(items.listbox,0,length(items.temp))
      }
      #-------------------------------------------------------------------------
      for(i in 1:length(parts.temp)){
        if(i==1) allPartsItems<-c()
        allPartsItems<-c(allPartsItems,strsplit(parts.temp[[i]]@items,split=" ")[[1]])
      }
      if(length(items.temp)>0){
        for(i in 1:length(items.temp)){
          if(!is.element(items.temp[[i]]@name,allPartsItems)) items.temp[[i]]@part<-""
        } # end  for(i in 1:length(items.temp)){
      } # end if(length(items.temp)>0){
    }

    if(length(parts.temp)==0)
    { parts.temp<-list()
    }

    assign("parts.temp",parts.temp,tempEnvir)
    assign("items.temp",items.temp,tempEnvir)
    tkdestroy(changePartsWindow)
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { index<-get("index",tempEnvir)
    parts.temp<-get("parts.temp",envir=tempEnvir)
    items.temp<-get("items.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    items<-tclvalue(tkget(items.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")
    items<-gsub(x=items,"\n",replacement="")
    items<-gsub(x=items,"\t",replacement="")
    items.check<-gsub(x=items," ",replacement="")

    if(name.check=="" & explanation.check=="")
    { tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      parts.temp<-parts.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the part name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the part explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'explanation'...",call.=FALSE)
    } else
    {
      if(index>length(parts.temp)){
        parts.temp[[index]]<-new("partClass",
        name=name,
        explanation=explanation,
        items=items)
      } else{
        parts.temp[[index]]@name<-name
        parts.temp[[index]]@explanation<-explanation
        parts.temp[[index]]@items<-items
      } # end if(index>length(parts.temp)){

      if(length(items.temp)>0)
      { partItems<-strsplit(parts.temp[[index]]@items,split=" ")[[1]]
        partItems<-gsub(x=partItems," ",replacement="")
        for(i in 1:length(items.temp)){
          if(is.element(items.temp[[i]]@name,partItems)) items.temp[[i]]@part<-name
        } # end for
        tkselection.clear(items.listbox,0,length(items.temp))
      } # end  if(length(items.temp)>0)
    }

    index<-index-1
    assign("parts.temp",value=parts.temp,envir=tempEnvir)
    assign("items.temp",items.temp,tempEnvir)
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(parts.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",parts.temp[[index]]@explanation)
    tkconfigure(items.txt,state="normal")
    tkdelete(items.txt,"0.0","end")
    if(!is.null(parts.temp[[index]]@items))tkinsert(items.txt,"0.0",parts.temp[[index]]@items)
    tkconfigure(items.txt,state="disabled")

    if(index==length(parts.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(parts.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(parts.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(parts.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(parts.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    parts.temp<-get("parts.temp",envir=tempEnvir)
    items.temp<-get("items.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    items<-tclvalue(tkget(items.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")
    items<-gsub(x=items,"\n",replacement="")
    items<-gsub(x=items,"\t",replacement="")
    items.check<-gsub(x=items," ",replacement="")

    if(name.check=="" & explanation.check=="")
    { tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      parts.temp<-parts.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the part name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the part explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'explanation'...",call.=FALSE)
    } else
    {
      if(index>length(parts.temp))
      { parts.temp[[index]]<-new("partClass",
        name=name,
        explanation=explanation,
        items=items)
      } else
      { parts.temp[[index]]@name<-name
        parts.temp[[index]]@explanation<-explanation
        parts.temp[[index]]@items<-items
      }

      if(length(items.temp)>0)
      { partItems<-strsplit(parts.temp[[index]]@items,split=" ")[[1]]
        partItems<-gsub(x=partItems," ",replacement="")
        for(i in 1:length(items.temp)){
          if(is.element(items.temp[[i]]@name,partItems)) items.temp[[i]]@part<-name
        } # end for
        tkselection.clear(items.listbox,0,length(items.temp))
      } # end if(length(items.temp)>0)
    }

    assign("parts.temp",value=parts.temp,envir=tempEnvir)
    assign("items.temp",items.temp,tempEnvir)

    index<-index+1
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(parts.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",parts.temp[[index]]@explanation)
    tkconfigure(items.txt,state="normal")
    tkdelete(items.txt,"0.0","end")
    if(!is.null(parts.temp[[index]]@items)){
      tkinsert(items.txt,"0.0",parts.temp[[index]]@items)
    } # end if(!is.null(parts.temp[[index]]@items)){
    tkconfigure(items.txt,state="disabled")

     if(index==length(parts.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(parts.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(parts.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(parts.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(parts.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  { index<-get("index",tempEnvir)
    parts.temp<-get("parts.temp",envir=tempEnvir)
    items.temp<-get("items.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    items<-tclvalue(tkget(items.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")
    items<-gsub(x=items,"\n",replacement="")
    items<-gsub(x=items,"\t",replacement="")
    items.check<-gsub(x=items," ",replacement="")

    if(name.check=="" & explanation.check=="")
    { tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      parts.temp<-parts.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the part name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the part explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'explanation'...",call.=FALSE)
    } else
    {
      if(index>length(parts.temp)){
        parts.temp[[index]]<-new("partClass",
        name=name,
        explanation=explanation,
        items=items)
      } else{
        parts.temp[[index]]@name<-name
        parts.temp[[index]]@explanation<-explanation
        parts.temp[[index]]@items<-items
      }
      if(length(items.temp)>0){
        partItems<-strsplit(parts.temp[[index]]@items,split=" ")[[1]]
        partItems<-gsub(x=partItems," ",replacement="")
        for(i in 1:length(items.temp)){
          if(is.element(items.temp[[i]]@name,partItems)) items.temp[[i]]@part<-name
        } # end for
        tkselection.clear(items.listbox,0,length(items.temp))
      } # end if(length(items.temp)>0){
    }

    assign("parts.temp",value=parts.temp,envir=tempEnvir)
    assign("items.temp",items.temp,tempEnvir)
    assign("index",value=index+1,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(""))
    tkdelete(explanation.txt,"0.0","end")
    tkconfigure(items.txt,state="normal")
    tkdelete(items.txt,"0.0","end")
    tkconfigure(items.txt,state="disabled")

    if(index==length(parts.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(parts.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(parts.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(parts.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(parts.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  { sureDelete<-tkmessageBox(message="Are you sure to delete this model part and all corresponding items?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no"){
      stop("",call.=FALSE)
    } else if(sureDelete=="yes"){
      index<-get("index",tempEnvir)
      parts.temp<-get("parts.temp",tempEnvir)
      items.temp<-get("items.temp",tempEnvir)

      if(index<=length(parts.temp) & index !=0){
        # delete corresponding items
        partToRemove<-parts.temp[[index]]@name
        items<-tclvalue(tkget(items.txt,"0.0","end"))
        items<-gsub(x=items,"\n",replacement="")
        items<-gsub(x=items,"\t",replacement="")
        items<-strsplit(items,split=" ")[[1]]
        items<-setdiff(items,"")
        if(length(items.temp)>0){
          for(i in 1:length(items.temp)){
            if(i==1) itemsToRemove<-c()
            if(is.element(items.temp[[i]]@name,items)) itemsToRemove<-c(itemsToRemove,i)
          } # end for
          if(length(itemsToRemove)>0) items.temp<-items.temp[-itemsToRemove]
        } # end  if(length(items.temp)>0){
        
      # variable containing available model items
        if(length(items.temp)>0)
        {
          for(i in 1:length(items.temp))
          { if(i==1)
            { items.list1<-c("")
              items.list2<-c("")
            }
            itemTemp1<-items.temp[[i]]@name
            items.list1<-c(items.list1,itemTemp1)
            itemTemp2<-paste(itemTemp1,"(",items.temp[[i]]@title,")",sep="")
            items.list2<-c(items.list2,itemTemp2)
          }
          tkselection.clear(items.listbox,0,length(items.temp))
        } else items.list2<-""
        items.list2<-tclVar(items.list2)
        assign("items.list2",value=items.list2,envir=tempEnvir)
        assign("items.list1",value=items.list1,envir=tempEnvir)
        tkconfigure(items.listbox,listvariable=get("items.list2",envir=tempEnvir))
       }
      if(index>1)
      { parts.temp<-parts.temp[-index]
        index<-index-1
        tkconfigure(name.entry,text=tclVar(parts.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",parts.temp[[index]]@explanation)
        tkconfigure(items.txt,state="normal")
        tkdelete(items.txt,"0.0","end")
        if(!is.null(parts.temp[[index]]@items))tkinsert(items.txt,"0.0",parts.temp[[index]]@items)
      } else if(index==1 & index!=length(parts.temp))
      { parts.temp<-parts.temp[-index]
        tkconfigure(name.entry,text=tclVar(parts.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",parts.temp[[index]]@explanation)
        tkconfigure(items.txt,state="normal")
        tkdelete(items.txt,"0.0","end")
        if(!is.null(parts.temp[[index]]@items))tkinsert(items.txt,"0.0",parts.temp[[index]]@items)
        tkconfigure(items.txt,state="disabled")
      } else if (index==1 & index==length(parts.temp))
      { parts.temp<-list()
        tkconfigure(name.entry,text=tclVar(""))
        tkdelete(explanation.txt,"0.0","end")
        tkconfigure(items.txt,state="normal")
        tkdelete(items.txt,"0.0","end")
        tkconfigure(items.txt,state="disabled")
      }

      assign("parts.temp",parts.temp,tempEnvir)
      assign("items.temp",items.temp,tempEnvir)
      assign("index",index,tempEnvir)

      if(index==length(parts.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(parts.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(parts.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(parts.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(parts.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }
  #-----------------------------------------------------------------------------
  # function for removing items from part to part
  #-----------------------------------------------------------------------------
  onSelection<-function(...)
  {  items.list1<-get("items.list1",envir=tempEnvir)
    selectedItems<-as.numeric(tkcurselection(items.listbox))+1
    selectedItems<-items.list1[selectedItems]
    selectedItems<-setdiff(selectedItems,"")
    tkconfigure(items.txt,state="normal")
    tkdelete(items.txt,"0.0","end")
    tkconfigure(items.txt,state="normal")
    tkinsert(items.txt,"0.0",selectedItems)
    tkconfigure(items.txt,state="disabled")

    items<-tclvalue(tkget(items.txt,"0.0","end"))
    items<-gsub(x=items,"\n",replacement="")
    items<-gsub(x=items,"\t",replacement="")
    items.check<-gsub(x=items," ",replacement="")
    if(items.check!="")
    { items.temp<-get("items.temp",envir=tempEnvir)
      index<-get("index",tempEnvir)
      parts.temp<-get("parts.temp",envir=tempEnvir)

      items<-strsplit(items,split=" ")[[1]]
      if(length(items)>0)
      { for(i in 1:length(parts.temp))
        { if (i==index)
          { parts.temp[[i]]@items<-paste(items,collapse=" ")
            for(j in 1:length(items.temp))
            { if(is.element(items.temp[[j]]@name,items))
              { newPart<-tclvalue(tkget(name.entry))
                newPart<-gsub(x=newPart,"\n",replacement="")
                newPart<-gsub(x=newPart,"\t",replacement="")
                items.temp[[j]]@part<-newPart
              }
            }
          } else
          { parts.temp[[i]]@items<-paste(setdiff(strsplit(parts.temp[[i]]@items,split=" ")[[1]],items),collapse=" ")
          }
        }
      }
      assign("parts.temp",parts.temp,tempEnvir)
      assign("items.temp",items.temp,tempEnvir)
    }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object1<-rriskModel@parts
  #object2<-rriskModel@items

  #parts.temp<-object1@parts
  #items.temp<-object2@items

  parts.temp<-parts@parts
  items.temp<-items@items

  #if(length(items.temp)==0){
  #  tkmessageBox(message="Sorry, the execution of his function is not possible: there are no items in the current model!",icon="error")
  #  stop("INVALID INPUT, there are no items in the current model...",call.=FALSE)
  #}
  if(length(parts.temp)==0){
    parts.temp<-list(new("partClass",name="",explanation="",items=""))
  } # end if(length(parts.temp)==0){

  # variable containing available model items
  items.list1<-c("")
  items.list2<-c("")
  if(length(items.temp)>0){
    for(i in 1:length(items.temp)){
      itemTemp1<-items.temp[[i]]@name
      items.list1<-c(items.list1,itemTemp1)
      itemTemp2<-paste(itemTemp1,"(",items.temp[[i]]@title,")",sep="")
      items.list2<-c(items.list2,itemTemp2)
      if(i==length(items.temp))items.list2<-tclVar(items.list2)
    } # end for(i in 1:length(items.temp)){
  } # end if(length(items.temp)>0){
  
  #-----------------------------------------------------------------------------
  # create help variable that are visible inside of onACTION(...) functions
  #-----------------------------------------------------------------------------
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("parts.temp",value=parts.temp,envir=tempEnvir)
  assign("items.temp",value=items.temp,envir=tempEnvir)
  assign("items.list1",value=items.list1,envir=tempEnvir)
  assign("items.list2",value=items.list2,envir=tempEnvir)
  assign("parts.original",value=parts.temp,envir=tempEnvir)
  assign("items.original",value=items.temp,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changePartsWindow<-tktoplevel()
  tkwm.title(changePartsWindow,"Edit model parts")
  tkwm.resizable(changePartsWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changePartsWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)
  textFrame<-tkframe(allFrame)
  #-----------------------------------------------------------------------------
  # changing name entry
  name.entry<-tkentry(textFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(parts.temp[[1]]@name),font=combo.font)
  name.label<-tklabel(textFrame,font=label.font,text="Name")
  tkgrid(name.label,name.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  # changing explanation entry
  explanation.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(explanation.txt,...))
  explanation.txt<-tktext(textFrame,borderwidth=2,width=70,height=15,relief="sunken",
    yscrollcommand=function(...)tkset(explanation.scrollbar,...),wrap="word",font=combo.font)
  explanation.label<-tklabel(textFrame,font=label.font,text="Explanation")
  tkinsert(explanation.txt,"0.0",parts.temp[[1]]@explanation)
  tkgrid(explanation.label,explanation.txt,explanation.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkgrid.configure(explanation.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------
  # items entry
  items.txt<-tktext(textFrame,borderwidth=2,width=70,height=1,relief="sunken",wrap="word",font=combo.font)
  tkinsert(items.txt,"0.0",parts.temp[[1]]@items)
  tkconfigure(items.txt,state="disabled")
  items.label<-tklabel(textFrame,font=label.font,text="Items")
  tkgrid(items.label,items.txt,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  # items listbox
  items.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(items.listbox,...))
  items.listbox<-tklistbox(textFrame,height=6,selectmode="multiple",
    yscrollcommand=function(...)tkset(items.scrollbar,...),background="white",font=combo.font,
    relief="sunken",listvariable=get("items.list2",envir=tempEnvir),width=70,font=tkfont.create(family="courier",size=10))
  itemslist.label<-tklabel(textFrame,font=label.font,text="Items list")
  tkgrid(itemslist.label,items.listbox,items.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkgrid.configure(items.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------
  tkbind(items.listbox,"<<ListboxSelect>>",onSelection)
  #-----------------------------------------------------------------------------
  tkpack(textFrame,side="top")
  #-----------------------------------------------------------------------------
  # button frame
  if(length(parts.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(parts.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(prevButton,nextButton,addButton,deleteButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))
  #-----------------------------------------------------------------------------
  tkfocus(changePartsWindow)
  tkwait.window(changePartsWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-list(parts=get("parts.temp",tempEnvir),items=get("items.temp",tempEnvir))
  return(output)
} # end of function change.parts()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelAbbreviationsClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.abbreviations
#' @aliases change.abbreviations                                                                 
#' @title Function for edditing model slot 'abbreviations'
#' @usage change.abbreviations(abbreviations)
#' @param abbreviations (model abbreviations) is an object of class \code{modelAbbreviationsClass}
#' @return This function returns edited model slot 'abbreviations'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@abbreviations<-change.abbreviations(model@@abbreviations)
#' model@@abbreviations}

change.abbreviations<-function(abbreviations)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("abbr.temp",get("abbr.original",tempEnvir),tempEnvir)
    tkdestroy(changeAbbreviationsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  {
    index<-get("index",tempEnvir)
    abbr.temp<-get("abbr.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This abbreviation will be removed from the abbreviations list!",icon="warning")
      abbr.temp<-abbr.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the abbreviation!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the abbreviation explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(abbr.temp))
      { abbr.temp[[index]]<-new("glossaryClass",
        name=name,
        explanation=explanation)
      } else
      { abbr.temp[[index]]@name<-name
        abbr.temp[[index]]@explanation<-explanation
      }
    }

    if(length(abbr.temp)==0)
    { abbr.temp<-list()
    } 
    #else
    #{ abbr.temp<-lapply(abbr.temp,function(x){
    #    name.temp<-x@name
    #    explanation.temp<-x@explanation
    #    name.temp<-gsub(x=name.temp,"\n",replacement="")
    #    name.temp<-gsub(x=name.temp,"\t",replacement="")
    #    explanation.temp<-gsub(x=explanation.temp,"\n",replacement="")
    #    explanation.temp<-gsub(x=explanation.temp,"\t",replacement="")
    #    new("glossaryClass",name=name.temp,explanation=explanation.temp)
    #  })
    #}

    assign("abbr.temp",abbr.temp,tempEnvir)
    tkdestroy(changeAbbreviationsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { index<-get("index",tempEnvir)
    abbr.temp<-get("abbr.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This abbreviation will be removed from the abbreviations list!",icon="warning")
      abbr.temp<-abbr.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the abbreviation!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the abbreviation explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(abbr.temp))
      { abbr.temp[[index]]<-new("glossaryClass",
        name=name,
        explanation=explanation)
      } else
      { abbr.temp[[index]]@name<-name
        abbr.temp[[index]]@explanation<-explanation
      }
    }

    index<-index-1
    assign("abbr.temp",value=abbr.temp,envir=tempEnvir)
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(abbr.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",abbr.temp[[index]]@explanation)

    if(index==length(abbr.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(abbr.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(abbr.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(abbr.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(abbr.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    abbr.temp<-get("abbr.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This abbreviation will be removed from the abbreviations list!",icon="warning")
      abbr.temp<-abbr.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the abbreviation!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the abbreviation explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(abbr.temp))
      { abbr.temp[[index]]<-new("glossaryClass",
        name=name,
        explanation=explanation)
      } else
      { abbr.temp[[index]]@name<-name
        abbr.temp[[index]]@explanation<-explanation
      }
    }

    assign("abbr.temp",value=abbr.temp,envir=tempEnvir)
    index<-index+1
    assign("index",value=index,envir=tempEnvir)
   
    tkconfigure(name.entry,text=tclVar(abbr.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",abbr.temp[[index]]@explanation)

     if(index==length(abbr.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(abbr.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(abbr.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(abbr.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(abbr.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  {
    index<-get("index",tempEnvir)
    abbr.temp<-get("abbr.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This abbreviation will be removed from the abbreviations list!",icon="warning")
      abbr.temp<-abbr.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the abbreviation!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the abbreviation explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(abbr.temp))
      { abbr.temp[[index]]<-new("glossaryClass",
        name=name,
        explanation=explanation)
      } else
      { abbr.temp[[index]]@name<-name
        abbr.temp[[index]]@explanation<-explanation
      }
    }

    assign("abbr.temp",value=abbr.temp,envir=tempEnvir)
    assign("index",value=index+1,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(""))
    tkdelete(explanation.txt,"0.0","end")

    if(index==length(abbr.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(abbr.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(abbr.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(abbr.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(abbr.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  {
    sureDelete<-tkmessageBox(message="Are you sure to delete this abbreviation?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no")
    { stop("",call.=FALSE)
    } else if(sureDelete=="yes")
    { index<-get("index",tempEnvir)
      abbr.temp<-get("abbr.temp",tempEnvir)
      
      if(index>length(abbr.temp))
      { abbr.temp[[index]]<-new("glossaryClass",
        name=tclvalue(tkget(name.entry)),
        explanation=tclvalue(tkget(explanation.txt,"0.0","end")))
      } else
      { abbr.temp[[index]]@name<-tclvalue(tkget(name.entry))
        abbr.temp[[index]]@explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
      }
      
      #if(index<=length(abbr.temp) & index>1)
      if(index>1)
      { abbr.temp<-abbr.temp[-index]
        index<-index-1
        tkconfigure(name.entry,text=tclVar(abbr.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",abbr.temp[[index]]@explanation)
      } else if(index==1 & index!=length(abbr.temp))
      { abbr.temp<-abbr.temp[-index]
        tkconfigure(name.entry,text=tclVar(abbr.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",abbr.temp[[index]]@explanation)
      } else if (index==1 & index==length(abbr.temp))
      { abbr.temp<-list()
        tkconfigure(name.entry,text=tclVar(""))
        tkdelete(explanation.txt,"0.0","end")
      }
      
      assign("abbr.temp",abbr.temp,tempEnvir)
      assign("index",index,tempEnvir)
      
      if(index==length(abbr.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(abbr.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(abbr.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(abbr.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(abbr.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object<-demoModel1@abbreviations
  #object<-object1
  #abbr.temp<-object@abbreviations
  
  abbr.temp<-abbreviations@abbreviations
  if(length(abbr.temp)==0) abbr.temp<-list(new("glossaryClass",name="",explanation=""))
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("abbr.temp",value=abbr.temp,envir=tempEnvir)
  assign("abbr.original",value=abbr.temp,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeAbbreviationsWindow<-tktoplevel()
  tkwm.title(changeAbbreviationsWindow,"Edit abbreviations")
  tkwm.resizable(changeAbbreviationsWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeAbbreviationsWindow)
  textFrame<-tkframe(allFrame)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)
  #-----------------------------------------------------------------------------
  # changing name entry
  name.entry<-tkentry(textFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(abbr.temp[[1]]@name),font=combo.font)
  name.label<-tklabel(textFrame,font=label.font,text="Name")
  tkgrid(name.label,name.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  # changing explanation entry
  explanation.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(explanation.txt,...))
  explanation.txt<-tktext(textFrame,borderwidth=2,width=70,height=15,relief="sunken",
    yscrollcommand=function(...)tkset(explanation.scrollbar,...),wrap="word",font=combo.font)
  explanation.label<-tklabel(textFrame,font=label.font,text="Explanation")
  tkinsert(explanation.txt,"0.0",abbr.temp[[1]]@explanation)
  tkgrid(explanation.label,explanation.txt,explanation.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkgrid.configure(explanation.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------
  tkpack(textFrame,side="top")
  #-----------------------------------------------------------------------------
  # button frame
  if(length(abbr.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(abbr.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  #-----------------------------------------------------------------------------
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(prevButton,nextButton,addButton,deleteButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))
  #-----------------------------------------------------------------------------
  tkfocus(changeAbbreviationsWindow)
  tkwait.window(changeAbbreviationsWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-new("modelAbbreviationsClass",abbreviations=get("abbr.temp",tempEnvir))
  return(output)
} # end of function change.abbreviations()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelGlossaryClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.glossary
#' @aliases change.glossary
#' @title Function for edditing model slot 'glossary'
#' @usage change.glossary(glossary)
#' @param glossary (model glossary) is an object of class \code{modelGlossaryClass}
#' @return This function returns edited model slot 'glossary'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@glossary<-change.glossary(model@@glossary)
#' model@@glossary}

change.glossary<-function(glossary)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("gloss.temp",get("gloss.original",tempEnvir),tempEnvir)
    tkdestroy(changeGlossaryWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  {
    index<-get("index",tempEnvir)
    gloss.temp<-get("gloss.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This glossary item will be removed from the glossary list!",icon="warning")
      gloss.temp<-gloss.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the name of the glossary item!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the explanation of the glossary item!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(gloss.temp))
      { gloss.temp[[index]]<-new("glossaryClass",
        name=name,
        explanation=explanation)
      } else
      { gloss.temp[[index]]@name<-name
        gloss.temp[[index]]@explanation<-explanation
      }
    }

    if(length(gloss.temp)==0)
    { gloss.temp<-list()
    } 

    assign("gloss.temp",gloss.temp,tempEnvir)
    tkdestroy(changeGlossaryWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { index<-get("index",tempEnvir)
    gloss.temp<-get("gloss.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This glossary item will be removed from the glossary list!",icon="warning")
      gloss.temp<-gloss.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the name of the glossary item!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the explanation of the glossary item!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(gloss.temp))
      { gloss.temp[[index]]<-new("glossaryClass",
        name=name,
        explanation=explanation)
      } else
      { gloss.temp[[index]]@name<-name
        gloss.temp[[index]]@explanation<-explanation
      }
    }

    index<-index-1
    assign("gloss.temp",value=gloss.temp,envir=tempEnvir)
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(gloss.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",gloss.temp[[index]]@explanation)

    if(index==length(gloss.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(gloss.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(gloss.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(gloss.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(gloss.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }


  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    gloss.temp<-get("gloss.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This glossary item will be removed from the glossary list!",icon="warning")
      gloss.temp<-gloss.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the name of the glossary item!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the explanation of the glossary item!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(gloss.temp))
      { gloss.temp[[index]]<-new("glossaryClass",
        name=name,
        explanation=explanation)
      } else
      { gloss.temp[[index]]@name<-name
        gloss.temp[[index]]@explanation<-explanation
      }
    }

    assign("gloss.temp",value=gloss.temp,envir=tempEnvir)
    index<-index+1
    assign("index",value=index,envir=tempEnvir)
    
    tkconfigure(name.entry,text=tclVar(gloss.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",gloss.temp[[index]]@explanation)

     if(index==length(gloss.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(gloss.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(gloss.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(gloss.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(gloss.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  {
    index<-get("index",tempEnvir)
    gloss.temp<-get("gloss.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This glossary item will be removed from the glossary list!",icon="warning")
      gloss.temp<-gloss.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the name of the glossary item!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the explanation of the glossary item!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(gloss.temp))
      { gloss.temp[[index]]<-new("glossaryClass",
        name=name,
        explanation=explanation)
      } else
      { gloss.temp[[index]]@name<-name
        gloss.temp[[index]]@explanation<-explanation
      }
    }

    assign("gloss.temp",value=gloss.temp,envir=tempEnvir)
    assign("index",value=index+1,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(""))
    tkdelete(explanation.txt,"0.0","end")

    if(index==length(gloss.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(gloss.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(gloss.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(gloss.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(gloss.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  {
    sureDelete<-tkmessageBox(message="Are you sure to delete this glossary item?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no")
    { stop("",call.=FALSE)
    } else if(sureDelete=="yes")
    { index<-get("index",tempEnvir)
      gloss.temp<-get("gloss.temp",tempEnvir)
      
      if(index>1)
      { gloss.temp<-gloss.temp[-index]
        index<-index-1
        tkconfigure(name.entry,text=tclVar(gloss.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",gloss.temp[[index]]@explanation)
      } else if(index==1 & index!=length(gloss.temp))
      { gloss.temp<-gloss.temp[-index]
        tkconfigure(name.entry,text=tclVar(gloss.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",gloss.temp[[index]]@explanation)
      } else if (index==1 & index==length(gloss.temp))
      { gloss.temp<-list()
        tkconfigure(name.entry,text=tclVar(""))
        tkdelete(explanation.txt,"0.0","end")
      }
      
      assign("gloss.temp",gloss.temp,tempEnvir)
      assign("index",index,tempEnvir)
      
      if(index==length(gloss.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(gloss.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(gloss.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(gloss.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(gloss.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object<-demoModel1@glossary
  #object<-object1
  #gloss.temp<-object@glossary
  
  gloss.temp<-glossary@glossary
  if(length(gloss.temp)==0) gloss.temp<-list(new("glossaryClass",name="",explanation=""))
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("gloss.temp",value=gloss.temp,envir=tempEnvir)
  assign("gloss.original",value=gloss.temp,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeGlossaryWindow<-tktoplevel()
  tkwm.title(changeGlossaryWindow,"Edit glossary")
  tkwm.resizable(changeGlossaryWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeGlossaryWindow)
  textFrame<-tkframe(allFrame)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)
  #-----------------------------------------------------------------------------
  # changing name entry
  name.entry<-tkentry(textFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(gloss.temp[[1]]@name),font=combo.font)
  name.label<-tklabel(textFrame,font=label.font,text="Name")
  tkgrid(name.label,name.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  # changing explanation entry
  explanation.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(explanation.txt,...))
  explanation.txt<-tktext(textFrame,borderwidth=2,width=70,height=15,relief="sunken",
    yscrollcommand=function(...)tkset(explanation.scrollbar,...),wrap="word",font=combo.font)
  explanation.label<-tklabel(textFrame,font=label.font,text="Explanation")
  tkinsert(explanation.txt,"0.0",gloss.temp[[1]]@explanation)
  tkgrid(explanation.label,explanation.txt,explanation.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkgrid.configure(explanation.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------
  tkpack(textFrame,side="top")
  #-----------------------------------------------------------------------------
  # button frame
  if(length(gloss.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(gloss.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(prevButton,nextButton,addButton,deleteButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))
  #-----------------------------------------------------------------------------
  tkfocus(changeGlossaryWindow)
  tkwait.window(changeGlossaryWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-new("modelGlossaryClass",glossary=get("gloss.temp",tempEnvir))
  return(output)
} # end of function change.glossary()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelBasicsClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.basics
#' @aliases change.basics
#' @title Function for edditing model slot 'basics'
#' @usage change.basics(basics)
#' @param basics (model basics) is an object of class \code{modelBasicsClass}
#' @return This function returns edited model slot 'basics'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@basics<-change.basics(model@@basics)
#' model@@basics}

change.basics<-function(basics)
{

  #-----------------------------------------------------------------------------
  # what happends by pressing "help" button
  #-----------------------------------------------------------------------------
  onHelp<-function(...)
  {
    tkmessageBox(icon="info",type="ok",
      message=
"MainDoc: please enter here a reference to the main document (e.g., project report) in which the model is described.
\n
Background: please write here about the background of the model
\n
Objectives: please write here about the purpose and the objectives of the model
\n
Scope: please write here about the scope and limitations of the scope
\n
Description: please write here the model itself (type of model, structure (division into parts),the processes modeled and which endpoint(s) will be simulated by the model)")
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("object.temp",get("object.original",tempEnvir),tempEnvir)
    tkdestroy(changeBasicsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  {
    object.temp<-get("object.temp",tempEnvir)
    index<-get("index",tempEnvir)
    object.temp[[index]]@explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    object.temp<-lapply(object.temp,function(x){
        name.temp<-x@name
        explanation.temp<-x@explanation
        explanation.temp<-gsub(x=explanation.temp,"\n",replacement="")
        explanation.temp<-gsub(x=explanation.temp,"\t",replacement="")
        new("basicsClass",name=name.temp,explanation=explanation.temp)
    })
    assign("object.temp",object.temp,tempEnvir)
    tkdestroy(changeBasicsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  {
    index<-get("index",tempEnvir)
    object.temp<-get("object.temp",envir=tempEnvir)
    object.temp[[index]]@explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    assign("object.temp",value=object.temp,envir=tempEnvir)
    assign("index",value=index-1,envir=tempEnvir)
    index<-get("index",envir=tempEnvir)

    tkconfigure(explanation.label,text=object.temp[[index]]@name)
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",object.temp[[index]]@explanation)

    if(index==length(object.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(object.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(object.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(object.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(object.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  {
     index<-get("index",tempEnvir)
     object.temp<-get("object.temp",envir=tempEnvir)
     object.temp[[index]]@explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
     assign("object.temp",value=object.temp,envir=tempEnvir)
     index<-index+1
     assign("index",value=index,envir=tempEnvir)
     tkconfigure(explanation.label,text=object.temp[[index]]@name)
     tkdelete(explanation.txt,"0.0","end")
     tkinsert(explanation.txt,"0.0",object.temp[[index]]@explanation)

     if(index==length(object.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(object.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(object.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(object.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(object.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object<-object1
  #basics.temp<-object@basics
  
  basics.temp<-basics@basics
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("object.temp",value=basics.temp,envir=tempEnvir)
  assign("object.original",value=basics.temp,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeBasicsWindow<-tktoplevel()
  tkwm.title(changeBasicsWindow,"Edit basic descriptions")
  tkwm.resizable(changeBasicsWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeBasicsWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)

  # changing explanation entry
  textFrame<-tkframe(allFrame)
  explanation.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(explanation.txt,...))
  explanation.txt<-tktext(textFrame,borderwidth=2,width=70,height=15,relief="sunken",
    yscrollcommand=function(...)tkset(explanation.scrollbar,...),wrap="word",font=combo.font)
  explanation.label<-tklabel(textFrame,font=label.font,text=basics.temp[[1]]@name,width=10)
  tkinsert(explanation.txt,"0.0",basics.temp[[1]]@explanation)
  # rm(object)
  tkgrid(explanation.label,explanation.txt,explanation.scrollbar,sticky="nw",padx=c(0,5),pady=c(0,5))
  tkgrid.configure(explanation.scrollbar,sticky="news")
  tkpack(textFrame,side="top")

  # button frame
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  tkgrid(prevButton,nextButton,padx=c(15,0))

  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  helpButton<-ttkbutton(buttonsFrame2,text="help",width=10,command=onHelp)
  tkgrid(okButton,cancelButton,helpButton,padx=c(15,0))

  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))

  tkfocus(changeBasicsWindow)
  tkwait.window(changeBasicsWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-new("modelBasicsClass",basics=get("object.temp",tempEnvir))
  return(output)
} # end of function change.basics()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelReferencesClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.references
#' @aliases change.references
#' @title Function for edditing model slot 'references'
#' @usage change.references(references)
#' @param references (model references) is an object of class \code{modelReferencesClass}
#' @return This function returns edited model slot 'references'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@references<-change.references(model@@references)
#' model@@references}

change.references<-function(references)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("refs.temp",get("refs.original",tempEnvir),tempEnvir)
    tkdestroy(changeReferencesWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  { refs.temp<-tclvalue(tkget(refs.txt,"0.0","end"))
    refs.temp<-strsplit(refs.temp,split="\n")[[1]]
    assign("refs.temp",refs.temp,tempEnvir)
    tkdestroy(changeReferencesWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onReset<-function(...)
  { refs.original<-get("refs.original",tempEnvir)
    for(i in 1:length(refs.original))
    { if(i==1) refs<-""
      refs<-paste(refs,object[i],sep="\n")
    }
    tkdelete(refs.txt,"0.0","end")
    tkinsert(refs.txt,"0.0",refs)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onClear<-function(...)
  { tkdelete(refs.txt,"0.0","end")
    tkfocus(refs.txt)
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object<-object1
  #object<-references
  
  object<-references@references
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("refs.temp",value=object,envir=tempEnvir)
  assign("refs.original",value=object,envir=tempEnvir)

  for(i in 1:length(object))
  { if(i==1) refs<-""
    refs<-paste(refs,object[i],sep="\n")
  }

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeReferencesWindow<-tktoplevel()
  tkwm.title(changeReferencesWindow,"Edit references")
  tkwm.resizable(changeReferencesWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeReferencesWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)

  # changing explanation entry
  refsFrame<-tkframe(allFrame)
  refs.scrollbary<-tkscrollbar(refsFrame,repeatinterval=1,command=function(...)tkyview(refs.txt,...))
  refs.scrollbarx<-tkscrollbar(refsFrame,orient="horizontal",repeatinterval=1,command=function(...)tkxview(refs.txt,...))
  refs.txt<-tktext(refsFrame,borderwidth=2,width=70,height=30,relief="sunken",
    yscrollcommand=function(...)tkset(refs.scrollbary,...),
    xscrollcommand=function(...)tkset(refs.scrollbarx,...),wrap="none",font=combo.font)
  tkinsert(refs.txt,"0.0",refs)
  tkgrid(refs.txt,refs.scrollbary,sticky="nw",padx=c(0,0),pady=c(0,5))
  tkgrid(refs.scrollbarx)
  tkgrid.configure(refs.scrollbary,sticky="ns")
  tkgrid.configure(refs.scrollbarx,sticky="ew")
  tkgrid(refsFrame)

  # button frame
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  resetButton<-ttkbutton(buttonsFrame1,text="reset",width=10,command=onReset)
  clearButton<-ttkbutton(buttonsFrame1,text="clear",width=10,command=onClear)
  tkgrid(resetButton,clearButton,padx=c(15,0))

  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))

  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkgrid(buttonsFrame,pady=c(15,0))
  tkpack(allFrame,padx=c(25,25),pady=c(25,25))

  tkfocus(changeReferencesWindow)
  tkwait.window(changeReferencesWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-new("modelReferencesClass",references=get("refs.temp",tempEnvir))
  return(output)
} # end of function change.references()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelSettingsClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.settings
#' @aliases change.settings
#' @title Function for edditing model slot 'settings'
#' @usage change.settings(settings)
#' @param settings (model settings) is an object of class \code{modelSettingsClass}
#' @return This function returns edited model slot 'settings'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@settings<-change.settings(model@@settings)
#' model@@settings}

change.settings<-function(settings)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOK<-function(...)
  { object.temp<-get("object.temp",tempEnvir)

    abserror.temp<-suppressWarnings(as.numeric(tclvalue(tkget(abserror.entry))))
    N.temp<-suppressWarnings(as.numeric(tclvalue(tkget(N.entry))))
    N2d.temp<-suppressWarnings(as.numeric(tclvalue(tkget(N2d.entry))))
    stress.temp<-suppressWarnings(as.numeric(tclvalue(tkget(stress.entry))))

    # validity check
    if(is.na(abserror.temp) | is.na(N.temp) | is.na(N2d.temp) | is.na(stress.temp))
    {  tkmessageBox(message="INVALID INPUT (not numeric) in one of the fields: \n
        'Number of iterations (1st dimension)'
        'Number of iterations (2nd dimension)'
        'Stress test percentile'
        'Absolute error tolerance relevant for plotting convergence'.
        \n Please correct the input!",icon="error")
        stop("INVALID INPUT...",call.=FALSE)
    }
    if(abserror.temp<=0 | N.temp<=0 | N2d.temp<=0)
    {  tkmessageBox(message="INVALID INPUT (out of range) in one of the fields: \n
        'Number of iterations (1st dimension)'
        'Number of iterations (2nd dimension)'
        'Absolute error tolerance relevant for plotting convergence'.
        \n Please correct the input!",icon="error")
        stop("INVALID INPUT...",call.=FALSE)
    }
    if(abs(N.temp-trunc(N.temp))>0 | abs(N2d.temp-trunc(N2d.temp))>0)
    {  tkmessageBox(message="INVALID INPUT (not integer) in one of the fields: \n
        'Number of iterations (1st dimension)'
        'Number of iterations (2nd dimension)'.
        \n Please correct the input!",icon="error")
        stop("INVALID INPUT...",call.=FALSE)
    }
    if(stress.temp<=0 | stress.temp>=100)
    {  tkmessageBox(message="INVALID INPUT in field 'Stress test percentile' (outside of the intervall (0,100)).
        \n Please correct the input!",icon="error")
        tkfocus(stress.entry)
        stop("INVALID INPUT...",call.=FALSE)
    }

    object.temp@abserror<-abserror.temp
    object.temp@N<-N.temp
    object.temp@N2d<-N2d.temp
    object.temp@stress<-stress.temp
    object.temp@deleteTeX<-as.logical(tclvalue(tkget(latex.combobox)))
    object.temp@usenotapplicable<-as.logical(tclvalue(tkget(notapplicable.combobox)))
    object.temp@mycol<-as.character(tclvalue(tkget(mycol.combobox)))
    object.temp@nwlayout<-as.character(tclvalue(tkget(nwlayout.combobox)))
    object.temp@trans<-as.character(tclvalue(tkget(trans.combobox)))
    object.temp@sens<-as.character(tclvalue(tkget(sens.combobox)))
    object.temp@coverheader<-as.character(tclvalue(tkget(coverheader.combobox)))
    sty<-tclvalue(tkget(latexsty.txt,"0.0","end"))
    sty<-gsub(x=sty,"\n",replacement="")
    sty<-gsub(x=sty,"\t",replacement="")
    sty<-gsub(x=sty," ",replacement="")
    sty<-strsplit(sty,split=",")[[1]]
    object.temp@sty<-sty
    assign("object.temp",object.temp,tempEnvir)
    tkdestroy(changeSettingsWindow)
  } # end of onOk()

  #-----------------------------------------------------------------------------
  # what happends by pressing "reset" button
  #-----------------------------------------------------------------------------
  onReset<-function(...)
  {
    object.original<-get("object.original",tempEnvir)
    tkconfigure(abserror.entry,text=tclVar(object.original@abserror))
    tkconfigure(N.entry,text=tclVar(object.original@N))
    tkconfigure(N2d.entry,text=tclVar(object.original@N2d))
    tkconfigure(stress.entry,text=tclVar(object.original@stress))
    tkconfigure(latex.combobox,text=tclVar(as.character(object.original@deleteTeX)))
    tkconfigure(notapplicable.combobox,text=tclVar(as.character(object.original@usenotapplicable)))
    tkconfigure(mycol.combobox,text=tclVar(object.original@mycol))
    tkconfigure(sens.combobox,text=tclVar(object.original@sens))
    tkconfigure(trans.combobox,text=tclVar(object.original@trans))
    tkconfigure(coverheader.combobox,text=tclVar(object.original@coverheader))
    tkconfigure(nwlayout.combobox,text=tclVar(object.original@nwlayout))
    tkdelete(latexsty.txt,"0.0","end")
    tkinsert(latexsty.txt,"0.0",object.original@sty)
  } # end of onReset()

  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("object.temp",get("object.original",tempEnvir),tempEnvir)
    tkdestroy(changeSettingsWindow)
  }  # end of onCancel()

  #-----------------------------------------------------------------------------
  # load required tcltk tools
  #-----------------------------------------------------------------------------
  tclRequire("BWidget")

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object1<-demoModel1@settings
  #object<-object1
  
  object<-settings
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("object.temp",value=object,envir=tempEnvir)
  assign("object.original",value=object,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeSettingsWindow<-tktoplevel()
  tkwm.title(changeSettingsWindow,"Edit model settings")
  tkwm.resizable(changeSettingsWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeSettingsWindow)
  combo.font<-tkfont.create(family="courier",size=10)

  # N
  N.entry<-tkentry(allFrame,width=8,textvariable=tclVar(object@N),font=combo.font)
  N.label<-tklabel(allFrame,text="Number of iterations (1st dimension)")
  tkgrid(N.label,N.entry,sticky="nw",padx=c(0,15),pady=c(0,10))

  # N2d
  N2d.entry<-tkentry(allFrame,width=8,textvariable=tclVar(object@N2d),font=combo.font)
  N2d.label<-tklabel(allFrame,text="Number of iterations (2nd dimension)")
  tkgrid(N2d.label,N2d.entry,sticky="nw",padx=c(0,15),pady=c(0,10))
  
  # stress
  stress.entry<-tkentry(allFrame,width=8,textvariable=tclVar(object@stress),font=combo.font)
  stress.label<-tklabel(allFrame,text="Stress test percentile")
  tkgrid(stress.label,stress.entry,sticky="nw",padx=c(0,15),pady=c(0,10))

  # abserror
  abserror.entry<-tkentry(allFrame,width=8,textvariable=tclVar(object@abserror),font=combo.font)
  abserror.label<-tklabel(allFrame,text="Absolute error tolerance relevant for plotting convergence")
  tkgrid(abserror.label,abserror.entry,sticky="nw",padx=c(0,15),pady=c(0,10))

  # trans
  trans.combobox<-ttkcombobox(allFrame,values=c("rank","z-score","identity"),
    width=max(nchar(c("rank","z-score","identity")))+2,state="readonly",font=combo.font)
  trans.label<-tklabel(allFrame,text="Transformation for sensitivity analysis")
  tkset(trans.combobox,object@trans)
  tkgrid(trans.label,trans.combobox,sticky="nw",padx=c(0,15),pady=c(0,10))

  # sens
  sens.combobox<-ttkcombobox(allFrame,values=c("correlation","regression"),
    width=max(nchar(c("correlation","regression")))+2,state="readonly",font=combo.font)
  tkset(sens.combobox,object@sens)
  sens.label<-tklabel(allFrame,text="Model for sensitivity analysis")
  tkgrid(sens.label,sens.combobox,sticky="nw",padx=c(0,15),pady=c(0,10))
  
  # coverheader
  coverheader.combobox<-ttkcombobox(allFrame,values=c("model network","concept graph","none"),
    width=max(nchar(c("model network","concept graph","none")))+2,state="readonly",font=combo.font)
  tkset(coverheader.combobox,object@coverheader)
  coverheader.label<-tklabel(allFrame,text="Cover picture for model report")
  tkgrid(coverheader.label,coverheader.combobox,sticky="nw",padx=c(0,15),pady=c(0,10))

  # deletetex
  latex.combobox<-ttkcombobox(allFrame,values=c("TRUE","FALSE"),width=6,state="readonly",font=combo.font)
  latex.label<-tklabel(allFrame,text="Protect LaTeX source file")
  tkset(latex.combobox,as.character(object@deleteTeX))
  tkgrid(latex.label,latex.combobox,sticky="nw",padx=c(0,15),pady=c(0,10))
  
  # not applicable
  notapplicable.combobox<-ttkcombobox(allFrame,values=c("TRUE","FALSE"),width=6,state="readonly",font=combo.font)
  notapplicable.label<-tklabel(allFrame,text="Use 'not applicable' by displaying model uncertainties")
  tkset(notapplicable.combobox,as.character(object@usenotapplicable))
  tkgrid(notapplicable.label,notapplicable.combobox,sticky="nw",padx=c(0,15),pady=c(0,10))

  # mycol
  mycol.combobox<-ttkcombobox(allFrame,values=colors(),width=max(nchar(colors())),state="readonly",font=combo.font)
  mycol.label<-tklabel(allFrame,text="Standard colour for graphics")
  mycolcolor.label<-tklabel(allFrame,width=3,background=object@mycol)
  tkset(mycol.combobox,object@mycol)
  tkbind(mycol.combobox,"<FocusIn>",function(){tkconfigure(mycolcolor.label,background=tclvalue(tkget(mycol.combobox)))})
  tkbind(mycol.combobox,"<MouseWheel>",function(){tkconfigure(mycolcolor.label,background=tclvalue(tkget(mycol.combobox)))})
  tkgrid(mycol.label,mycol.combobox,mycolcolor.label,sticky="nw",padx=c(0,15),pady=c(0,10))

  # layout
  nwlayout.combobox<-ttkcombobox(allFrame,values=c("circle","fruchtermanreingold","kamadakawai"),
    width=max(nchar(c("circle","fruchtermanreingold","kamadakawai"))),state="readonly",font=combo.font)
  nwlayout.label<-tklabel(allFrame,text="Layout for model graph vizualisation")
  tkset(nwlayout.combobox,object@nwlayout)
  tkgrid(nwlayout.label,nwlayout.combobox,sticky="nw",padx=c(0,15),pady=c(0,10))

  # changing latex entry
  latexsty.scrollbar<-tkscrollbar(allFrame,repeatinterval=1,command=function(...)tkyview(latexsty.txt,...))
  latexsty.txt<-tktext(allFrame,borderwidth=2,width=max(nchar(colors())),height=3,relief="sunken",
    yscrollcommand=function(...)tkset(latexsty.scrollbar,...),wrap="word",font=combo.font)
  latexsty.label<-tklabel(allFrame,text="Optional additional LaTeX style files (komma separated)")
  tkinsert(latexsty.txt,"0.0",object@sty)
  tkgrid(latexsty.label,latexsty.txt,latexsty.scrollbar,sticky="nw",padx=c(0,15),pady=c(0,0))
  tkgrid.configure(latexsty.scrollbar,sticky="ns")

  # buttons
  buttonsFrame<-tkframe(changeSettingsWindow)
  okButton<-ttkbutton(buttonsFrame,width=8,text="ok",command=onOK)
  resetButton<-ttkbutton(buttonsFrame,width=8,text="reset",command=onReset)
  cancelButton<-ttkbutton(buttonsFrame,width=8,text="cancel",command=onCancel)
  tkgrid(resetButton,okButton,cancelButton,padx=c(10,10))
  tkpack(buttonsFrame,side="bottom",pady=c(0,10))

  tkpack(allFrame,padx=c(20,20),pady=c(20,20))

  tkfocus(changeSettingsWindow)
  tkwait.window(changeSettingsWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-get("object.temp",tempEnvir)
  return(output)
} # end of function change.settings()



################################################################################
################################################################################
#' @descripotion A change function for \code{\linkS4class{modelAuthorsClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.authors
#' @aliases change.authors
#' @title Function for edditing model slot 'authors'
#' @usage   change.authors(authors)
#' @param authors (model authors) is an object of class \code{modelAuthorsClass}
#' @return This function returns edited model slot 'authors'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@authors<-change.authors(model@@authors)
#' model@@authors}

change.authors<-function(authors)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("authors.temp",get("authors.original",tempEnvir),tempEnvir)
    tkdestroy(changeAuthorsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  {
    index<-get("index",tempEnvir)
    authors.temp<-get("authors.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    institution<-tclvalue(tkget(institution.txt,"0.0","end"))
    email<-tclvalue(tkget(email.entry))

    email<-gsub(x=email," ",replacement="")
    name.check<-gsub(x=name," ",replacement="")
    institution<-gsub(x=institution,"\n",replacement="")
    institution<-gsub(x=institution,"\t",replacement="")
    institution.check<-gsub(x=institution," ",replacement="")

    if(name.check=="" & institution.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Institution' are empty. This author will be removed from the authors list!",icon="warning")
      authors.temp<-authors.temp[-index]
    } else if(name.check=="" & institution.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the author name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(institution.check=="" & name.check!="")
    { tkmessageBox(message="The 'institution' field is empty, please enter here the isntitution where the author is from!",icon="error")
      tkfocus(institution.txt)
      stop("INVALID INPUT, empty field 'Institution'...",call.=FALSE)
    } else
    { if(index>length(authors.temp))
      { authors.temp[[index]]<-new("authorClass",
        name=name,
        institution=institution,
        email=email)
      } else
      { authors.temp[[index]]@name<-name
        authors.temp[[index]]@institution<-institution
        authors.temp[[index]]@email<-email
      }
    }

    if(length(authors.temp)==0)
    { authors.temp<-list()
    } 

    assign("authors.temp",authors.temp,tempEnvir)
    tkdestroy(changeAuthorsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { index<-get("index",tempEnvir)
    authors.temp<-get("authors.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    institution<-tclvalue(tkget(institution.txt,"0.0","end"))
    email<-tclvalue(tkget(email.entry))

    email<-gsub(x=email," ",replacement="")
    name.check<-gsub(x=name," ",replacement="")
    institution<-gsub(x=institution,"\n",replacement="")
    institution<-gsub(x=institution,"\t",replacement="")
    institution.check<-gsub(x=institution," ",replacement="")
    
    if(name.check=="" & institution.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Institution' are empty. This author will be removed from the authors list!",icon="warning")
      authors.temp<-authors.temp[-index]
    } else if(name.check=="" & institution.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the author name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(institution.check=="" & name.check!="")
    { tkmessageBox(message="The 'institution' field is empty, please enter here the isntitution where the author is from!",icon="error")
      tkfocus(institution.txt)
      stop("INVALID INPUT, empty field 'Institution'...",call.=FALSE)
    } else
    { if(index>length(authors.temp))
      { authors.temp[[index]]<-new("authorClass",
        name=name,
        institution=institution,
        email=email)
      } else
      { authors.temp[[index]]@name<-name
        authors.temp[[index]]@institution<-institution
        authors.temp[[index]]@email<-email
      }
    }

    index<-index-1
    assign("authors.temp",value=authors.temp,envir=tempEnvir)
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(authors.temp[[index]]@name))
    tkdelete(institution.txt,"0.0","end")
    tkinsert(institution.txt,"0.0",authors.temp[[index]]@institution)
    tkconfigure(email.entry,text=tclVar(authors.temp[[index]]@email))
    #if((authors.temp[[index]]@email)!="")tkinsert(email.txt,"0.0",authors.temp[[index]]@email)

    if(index==length(authors.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(authors.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(authors.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(authors.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(authors.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    authors.temp<-get("authors.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    institution<-tclvalue(tkget(institution.txt,"0.0","end"))
    email<-tclvalue(tkget(email.entry))
    
    email<-gsub(x=email," ",replacement="") 
    name.check<-gsub(x=name," ",replacement="")
    institution<-gsub(x=institution,"\n",replacement="")
    institution<-gsub(x=institution,"\t",replacement="")
    institution.check<-gsub(x=institution," ",replacement="")

    if(name.check=="" & institution.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Institution' are empty. This author will be removed from the authors list!",icon="warning")
      authors.temp<-authors.temp[-index]
    } else if(name.check=="" & institution.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the author name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(institution.check=="" & name.check!="")
    { tkmessageBox(message="The 'institution' field is empty, please enter here the isntitution where the author is from!",icon="error")
      tkfocus(institution.txt)
      stop("INVALID INPUT, empty field 'Institution'...",call.=FALSE)
    } else
    { if(index>length(authors.temp))
      { authors.temp[[index]]<-new("authorClass",
        name=name,
        institution=institution,
        email=email)
      } else
      { authors.temp[[index]]@name<-name
        authors.temp[[index]]@institution<-institution
        authors.temp[[index]]@email<-email
      }
    }

    assign("authors.temp",value=authors.temp,envir=tempEnvir)
    index<-index+1
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(authors.temp[[index]]@name))
    tkdelete(institution.txt,"0.0","end")
    tkinsert(institution.txt,"0.0",authors.temp[[index]]@institution)
    tkconfigure(email.entry,text=tclVar(authors.temp[[index]]@email))
    #if(!(authors.temp[[index]]@email)=="")tkinsert(email.txt,"0.0",authors.temp[[index]]@email)

     if(index==length(authors.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(authors.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(authors.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(authors.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(authors.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  { index<-get("index",tempEnvir)
    authors.temp<-get("authors.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    institution<-tclvalue(tkget(institution.txt,"0.0","end"))
    email<-tclvalue(tkget(email.entry))

    email<-gsub(x=email," ",replacement="")

    name.check<-gsub(x=name," ",replacement="")
    institution<-gsub(x=institution,"\n",replacement="")
    institution<-gsub(x=institution,"\t",replacement="")
    institution.check<-gsub(x=institution," ",replacement="")

    if(name.check=="" & institution.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Institution' are empty. This author will be removed from the authors list!",icon="warning")
      authors.temp<-authors.temp[-index]
    } else if(name.check=="" & institution.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the author name!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(institution.check=="" & name.check!="")
    { tkmessageBox(message="The 'institution' field is empty, please enter here the isntitution where the author is from!",icon="error")
      tkfocus(institution.txt)
      stop("INVALID INPUT, empty field 'Institution'...",call.=FALSE)
    } else
    { if(index>length(authors.temp))
      { authors.temp[[index]]<-new("authorClass",
        name=name,
        institution=institution,
        email=email)
      } else
      { authors.temp[[index]]@name<-name
        authors.temp[[index]]@institution<-institution
        authors.temp[[index]]@email<-email
      }
    }
    
    assign("authors.temp",value=authors.temp,envir=tempEnvir)
    assign("index",value=index+1,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(""))
    tkdelete(institution.txt,"0.0","end")
    tkconfigure(email.entry,text=tclVar(""))

    if(index==length(authors.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(authors.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(authors.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(authors.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(authors.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  {
    sureDelete<-tkmessageBox(message="Are you sure to delete this model author?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no")
    { stop("",call.=FALSE)
    } else if(sureDelete=="yes")
    { index<-get("index",tempEnvir)
      authors.temp<-get("authors.temp",tempEnvir)

      if(index>1)
      { authors.temp<-authors.temp[-index]
        index<-index-1
        tkconfigure(name.entry,text=tclVar(authors.temp[[index]]@name))
        tkdelete(institution.txt,"0.0","end")
        tkinsert(institution.txt,"0.0",authors.temp[[index]]@institution)
        tkconfigure(email.entry,text=tclVar(authors.temp[[index]]@email))
        #if(!is.null(authors.temp[[index]]@email))tkinsert(email.entry,text=tclauthors.temp[[index]]@email)
      } else if(index==1 & index!=length(authors.temp))
      { authors.temp<-authors.temp[-index]
        tkconfigure(name.entry,text=tclVar(authors.temp[[index]]@name))
        tkdelete(institution.txt,"0.0","end")
        tkinsert(institution.txt,"0.0",authors.temp[[index]]@institution)
        tkconfigure(email.entry,text=tclVar(authors.temp[[index]]@email))
        #if(!is.null(authors.temp[[index]]@email))tkinsert(email.txt,"0.0",authors.temp[[index]]@email)
      } else if (index==1 & index==length(authors.temp))
      { authors.temp<-list()
        tkconfigure(name.entry,text=tclVar(""))
        tkdelete(institution.txt,"0.0","end")
        tkconfigure(email.entry,text=tclVar(""))
      }

      assign("authors.temp",authors.temp,tempEnvir)
      assign("index",index,tempEnvir)

      if(index==length(authors.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(authors.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(authors.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(authors.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(authors.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object<-object1
  
  authors.temp<-authors@authors
  if(length(authors.temp)==0) authors.temp<-list(new("authorClass",name="",institution="",email=""))
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("authors.temp",value=authors.temp,envir=tempEnvir)
  assign("authors.original",value=authors.temp,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeAuthorsWindow<-tktoplevel()
  tkwm.title(changeAuthorsWindow,"Edit model authors")
  tkwm.resizable(changeAuthorsWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeAuthorsWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)
  textFrame<-tkframe(allFrame)
  #-----------------------------------------------------------------------------
  # changing name entry
  name.entry<-tkentry(textFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(authors.temp[[1]]@name),font=combo.font)
  name.label<-tklabel(textFrame,font=label.font,text="Name")
  tkgrid(name.label,name.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  # changing explanation entry
  institution.txt<-tktext(textFrame,borderwidth=2,width=70,height=5,relief="sunken",wrap="word",font=combo.font)
  institution.label<-tklabel(textFrame,font=label.font,text="Institution")
  tkinsert(institution.txt,"0.0",authors.temp[[1]]@institution)
  tkgrid(institution.label,institution.txt,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  # changing email entry
  email.entry<-tkentry(textFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(authors.temp[[1]]@email),font=combo.font)
  email.label<-tklabel(textFrame,font=label.font,text="Email")
  tkgrid(email.label,email.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  tkpack(textFrame,side="top")
  #-----------------------------------------------------------------------------
  # button frame
  if(length(authors.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(authors.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  #-----------------------------------------------------------------------------
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(prevButton,nextButton,addButton,deleteButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))
  #-----------------------------------------------------------------------------
  tkfocus(changeAuthorsWindow)
  tkwait.window(changeAuthorsWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-new("modelAuthorsClass",authors=get("authors.temp",tempEnvir))
  return(output)
} # end of function change.authors()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelNameClass}} and \code{\linkS4class{modelVersionClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.nameversion
#' @aliases change.nameversion
#' @title Function for edditing model slots 'name' and 'version'
#' @usage change.nameversion(name,version,authors,availableModels)
#' @param name (model name) is an object of class \code{modelNameClass}
#' @param version (model version) is an object of class \code{modelVersionClass}
#' @param authors (model authors) is an object of class \code{modelAuthorsClass}
#' @param availableModels is a character vector containing model names that are available in the current rrisk session
#' @return This function returns edited model slots 'name' and 'version'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' # It is not possible to change model name to "my model"
#' newvalues<-change.nameversion(model@@name,model@@version,model@@authors,
#'   availableModels=c("my model"))
#' newvalues}

change.nameversion<-function(name,version,authors,availableModels)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("Version.temp",get("Version.original",tempEnvir),tempEnvir)
    assign("Name.temp",get("Name.original",tempEnvir),tempEnvir)
    tkdestroy(changeNameVersionWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  { availableModel<-get("availableModels",envir=tempEnvir)
  
    name.temp<-tclvalue(tkget(name.txt))
    status.temp<-tclvalue(tkget(status.combobox))
    minorupdate.temp<-tclvalue(tkget(minorupdate.txt))
    majorupdate.temp<-tclvalue(tkget(majorupdate.txt))
    subtitle.temp<-tclvalue(tkget(subtitle.txt))
    editedby.temp<-tclvalue(tkget(editedby.combobox))
    
    #---------------------------------------------------------------------------
    # check model name
    #---------------------------------------------------------------------------
    name.temp<-gsub(x=name.temp,"\n",replacement="")
    name.temp<-gsub(x=name.temp,"\t",replacement="")
    name.check<-gsub(x=name.temp," ",replacement="")
    if(name.check=="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the model name that should be different from the names of existing models!",icon="error")
      tkfocus(name.txt)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(is.element(name.temp,availableModels))
    { tkmessageBox(message="There is another model this the same name in the rrisk session, the model name should be unique!",icon="error")
      tkfocus(name.txt)
      stop("INVALID INPUT, there is another model this the same name in the rrisk session, the model name should be unique...",call.=FALSE)
    }
    
    #---------------------------------------------------------------------------
    # check subtitle
    #---------------------------------------------------------------------------
    subtitle.temp<-gsub(x=subtitle.temp,"\n",replacement="")
    subtitle.temp<-gsub(x=subtitle.temp,"\t",replacement="")
    
    Name.temp<-new("modelNameClass",name=name.temp)
    Version.temp<-new("modelVersionClass",
      status=status.temp,
      minorupdate=as.numeric(minorupdate.temp),
      majorupdate=as.numeric(majorupdate.temp),
      subtitle=subtitle.temp,
      editedby=gsub(x=editedby.temp,"_",replacement=" "))

    assign("Name.temp",Name.temp,tempEnvir)
    assign("Version.temp",Version.temp,tempEnvir)

    tkdestroy(changeNameVersionWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "reset" button
  #-----------------------------------------------------------------------------
  onReset<-function(...)
  { Name.original<-get("Name.original",envir=tempEnvir)
    Version.original<-get("Version.original",envir=tempEnvir)
    authors<-get("authors",envir=tempEnvir)
    
    tkconfigure(name.txt,text=tclVar(Name.original@name))
    tkconfigure(subtitle.txt,text=tclVar(Version.original@subtitle))
    tkset(status.combobox,Version.original@status)
    tkset(editedby.combobox,authors[1])
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object1=demoModel1@name
  #object2=demoModel1@version
  #object3=demoModel1@authors

  objectName<-name
  objectVersion<-version
  objectAuthors<-authors@authors
  if(length(objectAuthors)>0)
  { authors<-c()
    for(i in 1:length(objectAuthors))
    { authors[i]<-objectAuthors[[i]]@name
      authors[i]<-gsub(x=authors[i]," ",replacement="_")
    }
  } else authors<-c("")
  #-----------------------------------------------------------------------------
  # get names of available models
  #-----------------------------------------------------------------------------
  #availableModels<-c()
  #for(i in 1:length(rriskSession@models))
  #{ availableModelsTemp<-rriskSession@models[[i]]@name@name
  #  availableModels<-c(availableModels,availableModelsTemp)
  #}
  #availableModels<-setdiff(availableModels,objectName@name)
  
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("authors",value=authors,envir=tempEnvir)
  assign("Name.temp",value=objectName,envir=tempEnvir)
  assign("Version.temp",value=objectVersion,envir=tempEnvir)
  assign("Name.original",value=objectName,envir=tempEnvir)
  assign("Version.original",value=objectVersion,envir=tempEnvir)
  assign("availableModels",value=availableModels,envir=tempEnvir)

  objectName<-tclVar(objectName@name)
  objectStatus<-objectVersion@status
  objectMinorupdate<-tclVar(objectVersion@minorupdate)
  objectMajorupdate<-tclVar(objectVersion@majorupdate)
  objectSubtitle<-tclVar(objectVersion@subtitle)
  objectEditedby<-tclVar(objectVersion@editedby)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeNameVersionWindow<-tktoplevel()
  #tkwm.title(changeNameVersionWindow,"Edit model name and version")
  window.title<-paste("Edit model name and version of '",tclvalue(objectName),"'",sep="")
  tkwm.title(changeNameVersionWindow,window.title)
  tkwm.resizable(changeNameVersionWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeNameVersionWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)
  #-----------------------------------------------------------------------------
  # changing explanation entry
  textFrame<-tkframe(allFrame)
  name.txt<-tkentry(textFrame,borderwidth=2,width=60,relief="sunken",text=objectName,font=combo.font)
  name.label<-tklabel(textFrame,font=label.font,text="Name")
  minorupdate.txt<-tkentry(textFrame,borderwidth=2,width=60,relief="sunken",text=objectMinorupdate,state="disabled",font=combo.font)
  minorupdate.label<-tklabel(textFrame,font=label.font,text="Minor update")
  majorupdate.txt<-tkentry(textFrame,borderwidth=2,width=60,relief="sunken",text=objectMajorupdate,state="disabled",font=combo.font)
  majorupdate.label<-tklabel(textFrame,font=label.font,text="Major update")
  subtitle.txt<-tkentry(textFrame,borderwidth=2,width=60,relief="sunken",text=objectSubtitle,font=combo.font)
  subtitle.label<-tklabel(textFrame,font=label.font,text="Subtitle")
  #-----------------------------------------------------------------------------
  status.combobox<-ttkcombobox(textFrame,values=c("Draft","Release"),width=58,state="readonly",font=combo.font)
  tkset(status.combobox,objectStatus)
  tkconfigure(status.combobox,state="disabled")
  status.label<-tklabel(textFrame,font=label.font,text="Status")
  #-----------------------------------------------------------------------------
  editedby.combobox<-ttkcombobox(textFrame,values=authors,width=58,state="readonly",font=combo.font)
  tkset(editedby.combobox,objectVersion@editedby)
  #tkconfigure(editedby.combobox,state="disabled")
  editedby.label<-tklabel(textFrame,font=label.font,text="Edited by")
  #-----------------------------------------------------------------------------
  tkgrid(name.label,name.txt,padx=c(0,10),pady=c(0,20),sticky="nw")
  tkgrid(status.label,status.combobox,padx=c(0,10),pady=c(0,20),sticky="nw")
  tkgrid(majorupdate.label,majorupdate.txt,padx=c(0,10),pady=c(0,20),sticky="nw")
  tkgrid(minorupdate.label,minorupdate.txt,padx=c(0,10),pady=c(0,20),sticky="nw")
  tkgrid(subtitle.label,subtitle.txt,padx=c(0,10),pady=c(0,20),sticky="nw")
  tkgrid(editedby.label,editedby.combobox,padx=c(0,10),pady=c(0,0),sticky="nw")
  tkgrid(textFrame)
  #-----------------------------------------------------------------------------
  # button frame
  buttonsFrame<-tkframe(allFrame)
  okButton<-ttkbutton(buttonsFrame,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame,text="cancel",width=10,command=onCancel)
  resetButton<-ttkbutton(buttonsFrame,text="reset",width=10,command=onReset)
  tkgrid(resetButton,okButton,cancelButton,padx=c(15,0))
  tkgrid(buttonsFrame,pady=c(15,0))
  #-----------------------------------------------------------------------------
  tkpack(allFrame,padx=c(25,25),pady=c(25,25))
  #-----------------------------------------------------------------------------
  tkfocus(changeNameVersionWindow)
  tkwait.window(changeNameVersionWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-list(Name=get("Name.temp",tempEnvir),Version=get("Version.temp",tempEnvir))
  return(output)
} # end of fucntion change.nameversion()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelUncertaintiesClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.uncertainties
#' @aliases change.uncertainties
#' @title Function for edditing model slot 'uncertainties'
#' @usage change.uncertainties(uncertainties,scoring)
#' @param uncertainties (model uncertainties) is an object of class \code{modelUncertaintiesClass}
#' @param scoring (scoring system of the current model) is an object of class \code{modelScoringClass}
#' @return This function returns edited model slot 'uncertainties'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@uncertainties<-change.uncertainties(model@@uncertainties, model@@scoring)
#' model@@uncertainties}



change.uncertainties<-function(uncertainties,scoring)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("uncert.temp",get("object.original",tempEnvir)@uncertainties,tempEnvir)
    assign("note.temp",get("object.original",tempEnvir)@note,tempEnvir)
    tkdestroy(changeUncertaintiesWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  { scoring<-get("scoring",tempEnvir)
    possibleValues<-get("possibleValues",tempEnvir)
    index<-get("index",tempEnvir)
    uncert.temp<-get("uncert.temp",envir=tempEnvir)

    namemain<-tclvalue(tkget(namemain.entry))
    namesub<-tclvalue(tkget(namesub.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    scores<-tclvalue(tkget(scores.entry))

    namemain.check<-gsub(x=namemain," ",replacement="")
    namesub.check<-gsub(x=namesub," ",replacement="")
    scores.check<-gsub(x=scores," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(namemain.check=="" & namesub.check=="" & explanation.check=="" & scores.check=="")
    { tkmessageBox(message="All uncertainty corresponding fields are empty. This uncertainty be removed from the uncertainties list!",icon="warning")
      uncert.temp<-uncert.temp[-index]
    } else if(namemain.check=="" & any( namesub.check!="",explanation.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Main category' field is empty, please enter here the name of main category!",icon="error")
      tkfocus(namemain.entry)
      stop("INVALID INPUT, empty field 'Main category'...",call.=FALSE)
    } else if(namesub.check=="" & any( namemain.check!="",explanation.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Sub category' field is empty, please enter here the name of sub category!",icon="error")
      tkfocus(namesub.entry)
      stop("INVALID INPUT, empty field 'Sub category'...",call.=FALSE)
    } else if(explanation.check=="" & any( namemain.check!="",namesub.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here uncertainty explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else if(scores.check=="" & any( namemain.check!="",namesub.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Scores' field is empty, please enter here uncertainty scores!",icon="error")
      tkfocus(scores.entry)
      stop("INVALID INPUT, empty field 'Scores'...",call.=FALSE)
    } else
    { # überprüfe scores auf konsistenz mit dem scoring system
      scores<-gsub(x=scores,"\n",replacement="")
      scores<-suppressWarnings(as.numeric(strsplit(scores,split=" ")[[1]]))
      if(any(is.na(scores)))
      { tkmessageBox(message="The scores entries could not be converted to numerical values!",icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, not numerical entries in field 'Scores'...",call.=FALSE)
      }else if(length(scores)!=length(scoring))
      { mess<-paste("The vector of scores entries should be of length ",length(scoring),"!",sep="")
        tkmessageBox(message=mess,icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
      }
      for(i in 1:length(scores))
      { if(!is.element(scores[i],possibleValues))
        { mess<-paste("The values of the score ",scoring[[i]]@notation,"is out of range!")
          tkmessageBox(message=mess,icon="error")
          tkfocus(scores.entry)
          stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
        }
      }
      if(index>length(uncert.temp))
      { uncert.temp[[index]]<-new("uncertClass",
        namemain=namemain,
        namesub=namesub,
        explanation=explanation,
        scores=scores)
      } else
      { uncert.temp[[index]]@namemain<-namemain
        uncert.temp[[index]]@namesub<-namesub
        uncert.temp[[index]]@explanation<-explanation
        uncert.temp[[index]]@scores<-scores
      }
    }

    if(length(uncert.temp)==0)
    { uncert.temp<-list()
    } #else
    #{ uncert.temp<-lapply(uncert.temp,function(x){
    #    namemain.temp<-x@namemain
    #    namesub.temp<-x@namesub
    #    explanation.temp<-x@explanation
    #    scores.temp<-x@scores
    #    namemain.temp<-gsub(x=namemain.temp,"\n",replacement="")
    #    namemain.temp<-gsub(x=namemain.temp,"\t",replacement="")
    #    namesub.temp<-gsub(x=namesub.temp,"\n",replacement="")
    #    namesub.temp<-gsub(x=namesub.temp,"\t",replacement="")
    #    explanation.temp<-gsub(x=explanation.temp,"\n",replacement="")
    #    explanation.temp<-gsub(x=explanation.temp,"\t",replacement="")
    #    new("uncertClass",namemain=namemain.temp,namesub=namesub.temp,
    #      explanation=explanation.temp,scores=scores.temp)
    #  })
    #}

    note.temp<-tclvalue(tkget(note.txt,"0.0","end"))
    note.temp<-gsub(x=note.temp,"\n",replacement="")
    note.temp<-gsub(x=note.temp,"\t",replacement="")

    assign("uncert.temp",uncert.temp,tempEnvir)
    assign("note.temp",note.temp,tempEnvir)
    tkdestroy(changeUncertaintiesWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { scoring<-get("scoring",tempEnvir)
    index<-get("index",tempEnvir)
    uncert.temp<-get("uncert.temp",envir=tempEnvir)

    namemain<-tclvalue(tkget(namemain.entry))
    namesub<-tclvalue(tkget(namesub.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    scores<-tclvalue(tkget(scores.entry))

    namemain.check<-gsub(x=namemain," ",replacement="")
    namesub.check<-gsub(x=namesub," ",replacement="")
    scores.check<-gsub(x=scores," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(namemain.check=="" & namesub.check=="" & explanation.check=="" & scores.check=="")
    { tkmessageBox(message="All uncertainty corresponding fields are empty. This uncertainty item will be removed from the list of uncertainties!",icon="warning")
      uncert.temp<-uncert.temp[-index]
    } else if(namemain.check=="" & any( namesub.check!="",explanation.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Main category' field is empty, please enter here the name of main category!",icon="error")
      tkfocus(namemain.entry)
      stop("INVALID INPUT, empty field 'Main category'...",call.=FALSE)
    } else if(namesub.check=="" & any( namemain.check!="",explanation.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Sub category' field is empty, please enter here the name of sub category!",icon="error")
      tkfocus(namesub.entry)
      stop("INVALID INPUT, empty field 'Sub category'...",call.=FALSE)
    } else if(explanation.check=="" & any( namemain.check!="",namesub.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here uncertainty explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else if(scores.check=="" & any( namemain.check!="",namesub.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Scores' field is empty, please enter here uncertainty scores!",icon="error")
      tkfocus(scores.entry)
      stop("INVALID INPUT, empty field 'Scores'...",call.=FALSE)
    } else
    { # überprüfe scores auf konsistenz mit dem scoring system
      scores<-gsub(x=scores,"\n",replacement="")
      scores<-suppressWarnings(as.numeric(strsplit(scores,split=" ")[[1]]))
      if(any(is.na(scores)))
      { tkmessageBox(message="The scores entries could not be converted to numerical values!",icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, not numerical entries in field 'Scores'...",call.=FALSE)
      }else if(length(scores)!=length(scoring))
      { mess<-paste("The vector of scores entries should be of length ",length(scoring),"!",sep="")
        tkmessageBox(message=mess,icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
      }
      #heck.consistence<-TRUE
      for(i in 1:length(scores))
      { if(!is.element(scores[i],possibleValues))
        { mess<-paste("The values of the score ",scoring[[i]]@notation,"is out of range!")
          tkmessageBox(message=mess,icon="error")
          tkfocus(scores.entry)
          stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
        }
      }
      if(index>length(uncert.temp))
      { uncert.temp[[index]]<-new("uncertClass",
        namemain=namemain,
        namesub=namesub,
        explanation=explanation,
        scores=scores)
      } else
      { uncert.temp[[index]]@namemain<-namemain
        uncert.temp[[index]]@namesub<-namesub
        uncert.temp[[index]]@explanation<-explanation
        uncert.temp[[index]]@scores<-scores
      }
    }

    index<-index-1
    assign("uncert.temp",value=uncert.temp,envir=tempEnvir)
    assign("index",value=index,envir=tempEnvir)

    #tkconfigure(explanation.entry,text=tclVar(uncert.temp[[index]]@explanation))
    #tkconfigure(namemain.entry,text=tclVar(uncert.temp[[index]]@namemain))
    #tkconfigure(scores.entry,text=tclVar(uncert.temp[[index]]@scores))
    #tkdelete(namesub.txt,"0.0","end")
    #tkinsert(namesub.txt,"0.0",uncert.temp[[index]]@namesub)
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",uncert.temp[[index]]@explanation)
    tkconfigure(namemain.entry,text=tclVar(uncert.temp[[index]]@namemain))
    tkconfigure(namesub.entry,text=tclVar(uncert.temp[[index]]@namesub))
    tkconfigure(scores.entry,text=tclVar(uncert.temp[[index]]@scores))

    if(index==length(uncert.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(uncert.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(uncert.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(uncert.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(uncert.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    scoring<-get("scoring",tempEnvir)
    uncert.temp<-get("uncert.temp",envir=tempEnvir)

    namemain<-tclvalue(tkget(namemain.entry))
    namesub<-tclvalue(tkget(namesub.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    scores<-tclvalue(tkget(scores.entry))

    namemain.check<-gsub(x=namemain," ",replacement="")
    namesub.check<-gsub(x=namesub," ",replacement="")
    scores.check<-gsub(x=scores," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(namemain.check=="" & namesub.check=="" & explanation.check=="" & scores.check=="")
    { tkmessageBox(message="All uncertainty corresponding fields are empty. This uncertainty item will be removed from the list of uncertainties!",icon="warning")
      uncert.temp<-uncert.temp[-index]
    } else if(namemain.check=="" & any( namesub.check!="",explanation.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Main category' field is empty, please enter here the name of main category!",icon="error")
      tkfocus(namemain.entry)
      stop("INVALID INPUT, empty field 'Main category'...",call.=FALSE)
    } else if(namesub.check=="" & any( namemain.check!="",explanation.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Sub category' field is empty, please enter here the name of sub category!",icon="error")
      tkfocus(namesub.entry)
      stop("INVALID INPUT, empty field 'Sub category'...",call.=FALSE)
    } else if(explanation.check=="" & any( namemain.check!="",namesub.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here uncertainty explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else if(scores.check=="" & any( namemain.check!="",namesub.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Scores' field is empty, please enter here uncertainty scores!",icon="error")
      tkfocus(scores.entry)
      stop("INVALID INPUT, empty field 'Scores'...",call.=FALSE)
    } else
    { # überprüfe scores auf konsistenz mit dem scoring system
      scores<-gsub(x=scores,"\n",replacement="")
      scores<-suppressWarnings(as.numeric(strsplit(scores,split=" ")[[1]]))
      if(any(is.na(scores)))
      { tkmessageBox(message="The scores entries could not be converted to numerical values!",icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, not numerical entries in field 'Scores'...",call.=FALSE)
      }else if(length(scores)!=length(scoring))
      { mess<-paste("The vector of scores entries should be of length ",length(scoring),"!",sep="")
        tkmessageBox(message=mess,icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
      }
      #heck.consistence<-TRUE
      for(i in 1:length(scores))
      { if(!is.element(scores[i],possibleValues))
        { mess<-paste("The values of the score ",scoring[[i]]@notation,"is out of range!")
          tkmessageBox(message=mess,icon="error")
          tkfocus(scores.entry)
          stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
        }
      }
      # übenehme neue values
      if(index>length(uncert.temp))
      { uncert.temp[[index]]<-new("uncertClass",
        namemain=namemain,
        namesub=namesub,
        explanation=explanation,
        scores=scores)
      } else
      { uncert.temp[[index]]@namemain<-namemain
        uncert.temp[[index]]@namesub<-namesub
        uncert.temp[[index]]@explanation<-explanation
        uncert.temp[[index]]@scores<-scores
      }
    }

    assign("uncert.temp",value=uncert.temp,envir=tempEnvir)
    index<-index+1
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(namemain.entry,text=tclVar(uncert.temp[[index]]@namemain))
    tkconfigure(namesub.entry,text=tclVar(uncert.temp[[index]]@namesub))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",uncert.temp[[index]]@explanation)
    tkconfigure(scores.entry,text=tclVar(uncert.temp[[index]]@scores))

     if(index==length(uncert.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(uncert.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(uncert.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(uncert.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(uncert.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }  # end onNext()

  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  { scoring<-get("scoring",tempEnvir)
    index<-get("index",tempEnvir)
    uncert.temp<-get("uncert.temp",envir=tempEnvir)

    namemain<-tclvalue(tkget(namemain.entry))
    namesub<-tclvalue(tkget(namesub.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))
    scores<-tclvalue(tkget(scores.entry))

    namemain.check<-gsub(x=namemain," ",replacement="")
    namesub.check<-gsub(x=namesub," ",replacement="")
    scores.check<-gsub(x=scores," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(namemain.check=="" & namesub.check=="" & explanation.check=="" & scores.check=="")
    { tkmessageBox(message="All uncertainty corresponding fields are empty. This uncertainty item will be removed from the list of uncertainties!",icon="warning")
      uncert.temp<-uncert.temp[-index]
    } else if(namemain.check=="" & any( namesub.check!="",explanation.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Main category' field is empty, please enter here the name of main category!",icon="error")
      tkfocus(namemain.entry)
      stop("INVALID INPUT, empty field 'Main category'...",call.=FALSE)
    } else if(namesub.check=="" & any( namemain.check!="",explanation.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Sub category' field is empty, please enter here the name of sub category!",icon="error")
      tkfocus(namesub.entry)
      stop("INVALID INPUT, empty field 'Sub category'...",call.=FALSE)
    } else if(explanation.check=="" & any( namemain.check!="",namesub.check!="",scores.check!=""))
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here uncertainty explanation!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else if(scores.check=="" & any( namemain.check!="",namesub.check!="",explanation.check!=""))
    { tkmessageBox(message="The 'Scores' field is empty, please enter here uncertainty scores!",icon="error")
      tkfocus(scores.entry)
      stop("INVALID INPUT, empty field 'Scores'...",call.=FALSE)
    } else
    { # überprüfe scores auf konsistenz mit dem scoring system
      scores<-gsub(x=scores,"\n",replacement="")
      scores<-suppressWarnings(as.numeric(strsplit(scores,split=" ")[[1]]))
      if(any(is.na(scores)))
      { tkmessageBox(message="The scores entries could not be converted to numerical values!",icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, not numerical entries in field 'Scores'...",call.=FALSE)
      }else if(length(scores)!=length(scoring))
      { mess<-paste("The vector of scores entries should be of length ",length(scoring),"!",sep="")
        tkmessageBox(message=mess,icon="error")
        tkfocus(scores.entry)
        stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
      }
      #heck.consistence<-TRUE
      for(i in 1:length(scores))
      { if(!is.element(scores[i],possibleValues))
        { mess<-paste("The values of the score ",scoring[[i]]@notation,"is out of range!")
          tkmessageBox(message=mess,icon="error")
          tkfocus(scores.entry)
          stop("INVALID INPUT, no scores consistency with the scoring system...",call.=FALSE)
        }
      }
      if(index>length(uncert.temp))
      { uncert.temp[[index]]<-new("uncertClass",
        namemain=namemain,
        namesub=namesub,
        explanation=explanation,
        scores=scores)
      } else
      { uncert.temp[[index]]@namemain<-namemain
        uncert.temp[[index]]@namesub<-namesub
        uncert.temp[[index]]@explanation<-explanation
        uncert.temp[[index]]@scores<-scores
      }
      assign("uncert.temp",value=uncert.temp,envir=tempEnvir)
      assign("index",value=index+1,envir=tempEnvir)
    }

    tkdelete(explanation.txt,"0.0","end")
    tkconfigure(namemain.entry,text=tclVar(""))
    tkconfigure(namesub.entry,text=tclVar(""))
    tkconfigure(scores.entry,text=tclVar(""))

    if(index==length(uncert.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(uncert.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(uncert.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(uncert.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(uncert.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  {
    sureDelete<-tkmessageBox(message="Are you sure to delete this uncertainty from the model?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no")
    { stop("",call.=FALSE)
    } else if(sureDelete=="yes")
    { index<-get("index",tempEnvir)
      uncert.temp<-get("uncert.temp",tempEnvir)

      if(index>1)
      { uncert.temp<-uncert.temp[-index]
        index<-index-1
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",uncert.temp[[index]]@explanation)
        tkconfigure(namemain.entry,text=tclVar(uncert.temp[[index]]@namemain))
        tkconfigure(namesub.entry,text=tclVar(uncert.temp[[index]]@namesub))
        tkconfigure(scores.entry,text=tclVar(uncert.temp[[index]]@scores))
      } else if(index==1 & index!=length(uncert.temp))
      { uncert.temp<-uncert.temp[-index]
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",uncert.temp[[index]]@explanation)
        tkconfigure(namemain.entry,text=tclVar(uncert.temp[[index]]@namemain))
        tkconfigure(namesub.entry,text=tclVar(uncert.temp[[index]]@namesub))
        tkconfigure(scores.entry,text=tclVar(uncert.temp[[index]]@scores))
      } else if (index==1 & index==length(uncert.temp))
      { uncert.temp<-list()
        tkdelete(explanation.txt,"0.0","end")
        tkconfigure(namemain.entry,text=tclVar(""))
        tkconfigure(namesub.entry,text=tclVar(""))
        tkconfigure(scores.entry,text=tclVar(""))
      }

      assign("uncert.temp",uncert.temp,tempEnvir)
      assign("index",index,tempEnvir)

      if(index==length(uncert.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(uncert.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(uncert.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(uncert.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(uncert.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object1<-demoModel1@uncertainties
  #object2<-demoModel1@scoring

  #note.temp<-object1@note
  #uncert.temp<-object1@uncertainties
  #scoring<-object2@scoring
  #possibleValues<-object2@values

  scoringvmeanings <- scoring@vmeanings

  note.temp<-uncertainties@note
  uncert.temp<-uncertainties@uncertainties
  possibleValues<-scoring@values
  scoring<-scoring@scoring                                                      # Reihenfolge...
  #-----------------------------------------------------------------------------
  if(length(scoring)==0){
    tkmessageBox(message="Sorry, the execution of this function is not possible: model does not contain any scoring system!",icon="error")
    stop("INVALID INPUT, there are no model uncertainties in the current model...",call.=FALSE)
  } # end if(length(scoring)==0){
  #-----------------------------------------------------------------------------
  if(length(uncert.temp)==0){
    uncert.temp<-list(
      new("uncertClass",namemain="",namesub="",explanation="",
      scores=rep(scoringvmeanings[which(names(scoringvmeanings)=="notapplicable")],length(scoring))))
  } # end   if(length(uncert.temp)==0){
  #-----------------------------------------------------------------------------
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("scoring",value=scoring,envir=tempEnvir)
  assign("possibleValues",value=possibleValues,envir=tempEnvir)
  assign("uncert.temp",value=uncert.temp,envir=tempEnvir)
  assign("note.temp",value=note.temp,envir=tempEnvir)
  assign("object.original",value=uncertainties,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create text for scoring feld
  #-----------------------------------------------------------------------------
  scoring.text<-""
  for(i in 1:length(scoring))
  { temp<-paste("(",scoring[[i]]@notation,")",
                " ",scoring[[i]]@name,": ",
                scoring[[i]]@explanation,"\n",
                "Not applicable: ",scoringvmeanings[which(names(scoringvmeanings)=="notapplicable")],"\n\n",sep="")                      # korrigiert
    scoring.text<-paste(scoring.text,temp,sep="")
  }

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeUncertaintiesWindow<-tktoplevel()
  tkwm.title(changeUncertaintiesWindow,"Edit model uncertainties")
  tkwm.resizable(changeUncertaintiesWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeUncertaintiesWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)
  scoring.font<-tkfont.create(size=8,family="courier")
  objectUncert<-get("uncert.temp",envir=tempEnvir)
  index<-get("index",envir=tempEnvir)
  textFrame<-tkframe(allFrame)
  #-----------------------------------------------------------------------------
  # changing note entry
  note.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(note.txt,...))
  note.txt<-tktext(textFrame,borderwidth=2,width=70,height=7,relief="sunken",
    yscrollcommand=function(...)tkset(note.scrollbar,...),wrap="word",font=combo.font)
  tkinsert(note.txt,"0.0",note.temp)
  note.label<-tklabel(textFrame,font=label.font,text="Note")
  tkgrid(note.label,note.txt,note.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkgrid.configure(note.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------
  # viewing scoring system
  scoring.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(scoring.txt,...))
  scoring.txt<-tktext(textFrame,borderwidth=2,width=80,height=7,relief="sunken",
    yscrollcommand=function(...)tkset(scoring.scrollbar,...),wrap="word",font=scoring.font)
  tkinsert(scoring.txt,"0.0",scoring.text)
  tkconfigure(scoring.txt,state="disabled")
  scoring.label<-tklabel(textFrame,font=label.font,text="Scoring system")
  tkgrid(scoring.label,scoring.txt,scoring.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkgrid.configure(scoring.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------
  # changing namemain entry
  namemain.entry<-tkentry(textFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(uncert.temp[[1]]@namemain),font=combo.font)
  namemain.label<-tklabel(textFrame,font=label.font,text="Main category")
  tkgrid(namemain.label,namemain.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  # changing namesub entry
  namesub.entry<-tkentry(textFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(uncert.temp[[1]]@namesub),font=combo.font)
  namesub.label<-tklabel(textFrame,font=label.font,text="Sub category")
  tkgrid(namesub.label,namesub.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  # changing explanation entry
  explanation.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(explanation.txt,...))
  explanation.txt<-tktext(textFrame,borderwidth=2,width=70,height=10,relief="sunken",
    yscrollcommand=function(...)tkset(explanation.scrollbar,...),wrap="word",font=combo.font)
  explanation.label<-tklabel(textFrame,font=label.font,text="Explanation")
  tkinsert(explanation.txt,"0.0",uncert.temp[[1]]@explanation)
  tkgrid(explanation.label,explanation.txt,explanation.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkgrid.configure(explanation.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------
  # scores entry
  scores.entry<-tkentry(textFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(uncert.temp[[1]]@scores),font=combo.font)
  scores.label<-tklabel(textFrame,font=label.font,text="Scores")
  tkgrid(scores.label,scores.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkpack(textFrame,side="top")
  #-----------------------------------------------------------------------------
  # button frame
  if(length(uncert.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(uncert.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  #-----------------------------------------------------------------------------
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(prevButton,nextButton,addButton,deleteButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))
  #-----------------------------------------------------------------------------
  tkfocus(changeUncertaintiesWindow)
  tkwait.window(changeUncertaintiesWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-new("modelUncertaintiesClass",
    uncertainties=get("uncert.temp",tempEnvir),
    note=get("note.temp",tempEnvir))
  return(output)
} # end of function change.uncertainties()



################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelCommentsClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.comments
#' @aliases change.comments
#' @title Function for edditing model slot 'comments'
#' @usage   change.comments(comments)
#' @param comments (model comments) is an object of class \code{modelCommentsClass}
#' @return This function returns edited model slot 'comments'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@comments<-change.comments(model@@comments)
#' model@@comments}

change.comments<-function(comments)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("comm.temp",get("comm.original",tempEnvir),tempEnvir)
    tkdestroy(changeCommentsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  {
    index<-get("index",tempEnvir)
    comm.temp<-get("comm.temp",envir=tempEnvir)
    aktual<-tclvalue(tkget(comm.txt,"0.0","end"))
    aktual.check<-gsub(x=aktual,"\n",replacement="")
    aktual.check<-gsub(x=aktual.check," ",replacement="")

    if(aktual.check=="")
    { #tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      comm.temp<-comm.temp[-index]
    } else comm.temp[[index]]<-aktual

    if(length(comm.temp)==0)
    { comm.temp<-list()
    } else
    { comm.temp<-lapply(comm.temp,function(x){
        x<-gsub(x=x,"\n",replacement="")
        x<-gsub(x=x,"\t",replacement="")
      })
    }
    assign("comm.temp",comm.temp,tempEnvir)
    tkdestroy(changeCommentsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { index<-get("index",tempEnvir)
    comm.temp<-get("comm.temp",envir=tempEnvir)
    aktual<-tclvalue(tkget(comm.txt,"0.0","end"))
    aktual.check<-gsub(x=aktual,"\n",replacement="")
    aktual.check<-gsub(x=aktual.check," ",replacement="")

    if(aktual.check=="")
    { #tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      comm.temp<-comm.temp[-index]
    } else comm.temp[[index]]<-aktual

    index<-index-1
    assign("comm.temp",value=comm.temp,envir=tempEnvir)
    assign("index",value=index,envir=tempEnvir)

    tkdelete(comm.txt,"0.0","end")
    tkinsert(comm.txt,"0.0",comm.temp[[index]])
    tkconfigure(comm.label,text=paste("Comment ",index,sep=""))

    if(index==length(comm.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(comm.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(comm.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(comm.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(comm.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    comm.temp<-get("comm.temp",envir=tempEnvir)
    aktual<-tclvalue(tkget(comm.txt,"0.0","end"))
    aktual.check<-gsub(x=aktual,"\n",replacement="")
    aktual.check<-gsub(x=aktual.check," ",replacement="")

    if(aktual.check=="")
    { #tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      comm.temp<-comm.temp[-index]
    } else comm.temp[[index]]<-aktual

    assign("comm.temp",value=comm.temp,envir=tempEnvir)
    index<-index+1
    assign("index",value=index,envir=tempEnvir)
    tkdelete(comm.txt,"0.0","end")
    tkinsert(comm.txt,"0.0",comm.temp[[index]])
    tkconfigure(comm.label,text=paste("Comment ",index,sep=""))

     if(index==length(comm.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(comm.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(comm.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(comm.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(comm.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  { index<-get("index",tempEnvir)
    comm.temp<-get("comm.temp",envir=tempEnvir)
    aktual<-tclvalue(tkget(comm.txt,"0.0","end"))
    aktual.check<-gsub(x=aktual,"\n",replacement="")
    aktual.check<-gsub(x=aktual.check," ",replacement="")

    if(aktual.check=="")
    { #tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      comm.temp<-comm.temp[-index]
      stop("INVALID INPUT: 'comment' field is empty...",call.=FALSE)
    } else comm.temp[[index]]<-aktual

    assign("comm.temp",value=comm.temp,envir=tempEnvir)
    assign("index",value=index+1,envir=tempEnvir)
    tkdelete(comm.txt,"0.0","end")
    tkconfigure(comm.label,text=paste("Comment ",index+1,sep=""))

    if(index==length(comm.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(comm.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(comm.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(comm.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(comm.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  {
    sureDelete<-tkmessageBox(message="Are you sure to delete this model comment?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no")
    { stop("",call.=FALSE)
    } else if(sureDelete=="yes")
    { index<-get("index",tempEnvir)
      comm.temp<-get("comm.temp",tempEnvir)
      
      #if(index<=length(comm.temp) & index>1)
      if(index>1)
      { comm.temp<-comm.temp[-index]
        index<-index-1
        tkdelete(comm.txt,"0.0","end")
        tkinsert(comm.txt,"0.0",comm.temp[[index]])
        tkconfigure(comm.label,text=paste("Comment ",index,sep=""))
      } else if(index==1 & index!=length(comm.temp))
      { comm.temp<-comm.temp[-index]
        tkdelete(comm.txt,"0.0","end")
        tkinsert(comm.txt,"0.0",comm.temp[[index]])
        tkconfigure(comm.label,text=paste("Comment ",index,sep=""))
      } else if (index==1 & index==length(comm.temp))
      { comm.temp<-list()
        tkconfigure(comm.label,text=paste("Comment ",index,sep=""))
      }
      
      assign("comm.temp",comm.temp,tempEnvir)
      assign("index",index,tempEnvir)
      
      if(index==length(comm.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(comm.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(comm.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(comm.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(comm.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object<-object1
  #comm.temp<-object@comments
  
  comm.temp<-comments@comments
  if(length(comm.temp)==0) comm.temp<-list("")
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("comm.temp",value=comm.temp,envir=tempEnvir)
  assign("comm.original",value=comm.temp,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeCommentsWindow<-tktoplevel()
  tkwm.title(changeCommentsWindow,"Edit comments")
  tkwm.resizable(changeCommentsWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeCommentsWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)

  # changing explanation entry
  textFrame<-tkframe(allFrame)
  comm.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(comm.txt,...))
  comm.txt<-tktext(textFrame,borderwidth=2,width=70,height=15,relief="sunken",
    yscrollcommand=function(...)tkset(comm.scrollbar,...),wrap="word",font=combo.font)
  comm.label<-tklabel(textFrame,font=label.font,text="Comment 1")
  tkinsert(comm.txt,"0.0",comm.temp[[1]])
  tkgrid(comm.label,comm.txt,comm.scrollbar,sticky="nw",padx=c(0,5),pady=c(0,5))
  tkgrid.configure(comm.scrollbar,sticky="news")
  tkpack(textFrame,side="top")

  # button frame
  if(length(comm.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(comm.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(prevButton,nextButton,addButton,deleteButton,padx=c(15,0))

  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))

  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))

  tkfocus(changeCommentsWindow)
  tkwait.window(changeCommentsWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-new("modelCommentsClass",comments=get("comm.temp",tempEnvir))
  return(output)
} # end of function change.comments()


################################################################################
################################################################################
#' @description A change function for \code{\linkS4class{modelConclusionsClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.conclusions
#' @aliases change.conclusions
#' @title Function for edditing model slot 'conclusions'
#' @usage change.conclusions(conclusions)
#' @param conclusions (model conclusions) is an object of class \code{modelConclusionsClass}
#' @return This function returns edited model slot 'conclusions'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@conclusions<-change.conclusions(model@@conclusions)
#' model@@conclusions}

change.conclusions<-function(conclusions)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("concl.temp",get("comm.original",tempEnvir),tempEnvir)
    tkdestroy(changeConclusionsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  {
    index<-get("index",tempEnvir)
    concl.temp<-get("concl.temp",envir=tempEnvir)
    aktual<-tclvalue(tkget(comm.txt,"0.0","end"))
    aktual.check<-gsub(x=aktual,"\n",replacement="")
    aktual.check<-gsub(x=aktual.check," ",replacement="")

    if(aktual.check=="")
    { #tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      concl.temp<-concl.temp[-index]
    } else concl.temp[[index]]<-aktual

    if(length(concl.temp)==0)
    { concl.temp<-list()
    } else
    { concl.temp<-lapply(concl.temp,function(x){
        x<-gsub(x=x,"\n",replacement="")
        x<-gsub(x=x,"\t",replacement="")
      })
    }
    assign("concl.temp",concl.temp,tempEnvir)
    tkdestroy(changeConclusionsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { index<-get("index",tempEnvir)
    concl.temp<-get("concl.temp",envir=tempEnvir)
    aktual<-tclvalue(tkget(comm.txt,"0.0","end"))
    aktual.check<-gsub(x=aktual,"\n",replacement="")
    aktual.check<-gsub(x=aktual.check," ",replacement="")

    if(aktual.check=="")
    { #tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      concl.temp<-concl.temp[-index]
    } else concl.temp[[index]]<-aktual

    index<-index-1
    assign("concl.temp",value=concl.temp,envir=tempEnvir)
    assign("index",value=index,envir=tempEnvir)

    tkdelete(comm.txt,"0.0","end")
    tkinsert(comm.txt,"0.0",concl.temp[[index]])
    tkconfigure(comm.label,text=paste("Conclusion ",index,sep=""))

    if(index==length(concl.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(concl.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(concl.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(concl.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(concl.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    concl.temp<-get("concl.temp",envir=tempEnvir)
    aktual<-tclvalue(tkget(comm.txt,"0.0","end"))
    aktual.check<-gsub(x=aktual,"\n",replacement="")
    aktual.check<-gsub(x=aktual.check," ",replacement="")

    if(aktual.check=="")
    { #tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      concl.temp<-concl.temp[-index]
    } else concl.temp[[index]]<-aktual

    assign("concl.temp",value=concl.temp,envir=tempEnvir)
    index<-index+1
    assign("index",value=index,envir=tempEnvir)
    tkdelete(comm.txt,"0.0","end")
    tkinsert(comm.txt,"0.0",concl.temp[[index]])
    tkconfigure(comm.label,text=paste("Conclusion ",index,sep=""))

     if(index==length(concl.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(concl.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(concl.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(concl.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(concl.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  { index<-get("index",tempEnvir)
    concl.temp<-get("concl.temp",envir=tempEnvir)
    aktual<-tclvalue(tkget(comm.txt,"0.0","end"))
    aktual.check<-gsub(x=aktual,"\n",replacement="")
    aktual.check<-gsub(x=aktual.check," ",replacement="")

    if(aktual.check=="")
    { #tkmessageBox(message="All part corresponding fields are empty. This part will be removed from the parts list!",icon="warning")
      concl.temp<-concl.temp[-index]
      stop("INVALID INPUT: 'conclusion' field is empty...",call.=FALSE)
    } else concl.temp[[index]]<-aktual

    assign("concl.temp",value=concl.temp,envir=tempEnvir)
    assign("index",value=index+1,envir=tempEnvir)
    tkdelete(comm.txt,"0.0","end")
    tkconfigure(comm.label,text=paste("Conclusion ",index+1,sep=""))

    if(index==length(concl.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(concl.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(concl.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(concl.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(concl.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }
  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  {
    sureDelete<-tkmessageBox(message="Are you sure to delete this model conclusion?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no")
    { stop("",call.=FALSE)
    } else if(sureDelete=="yes")
    { index<-get("index",tempEnvir)
      concl.temp<-get("concl.temp",tempEnvir)
      
      #if(index<=length(concl.temp) & index>1)
      if(index>1)
      { concl.temp<-concl.temp[-index]
        index<-index-1
        tkdelete(comm.txt,"0.0","end")
        tkinsert(comm.txt,"0.0",concl.temp[[index]])
        tkconfigure(comm.label,text=paste("Conclusion ",index,sep=""))
      } else if(index==1 & index!=length(concl.temp))
      { concl.temp<-concl.temp[-index]
        tkdelete(comm.txt,"0.0","end")
        tkinsert(comm.txt,"0.0",concl.temp[[index]])
        tkconfigure(comm.label,text=paste("Conclusion ",index,sep=""))
      } else if (index==1 & index==length(concl.temp))
      { concl.temp<-list()
        tkconfigure(comm.label,text=paste("Conclusion ",index,sep=""))
      }
      
      assign("oncl.temp",concl.temp,tempEnvir)
      assign("index",index,tempEnvir)
      
      if(index==length(concl.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(concl.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(concl.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(concl.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(concl.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object<-object1
  #concl.temp<-object@conclusions
  
  concl.temp<-conclusions@conclusions
  if(length(concl.temp)==0) concl.temp<-list("")
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("concl.temp",value=concl.temp,envir=tempEnvir)
  assign("comm.original",value=concl.temp,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeConclusionsWindow<-tktoplevel()
  tkwm.title(changeConclusionsWindow,"Edit conclusions")
  tkwm.resizable(changeConclusionsWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeConclusionsWindow)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)

  # changing explanation entry
  textFrame<-tkframe(allFrame)
  comm.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(comm.txt,...))
  comm.txt<-tktext(textFrame,borderwidth=2,width=70,height=15,relief="sunken",
    yscrollcommand=function(...)tkset(comm.scrollbar,...),wrap="word",font=combo.font)
  comm.label<-tklabel(textFrame,font=label.font,text="Conclusion 1")
  tkinsert(comm.txt,"0.0",concl.temp[[1]])
  tkgrid(comm.label,comm.txt,comm.scrollbar,sticky="nw",padx=c(0,5),pady=c(0,5))
  tkgrid.configure(comm.scrollbar,sticky="news")
  tkpack(textFrame,side="top")

  # button frame
  if(length(concl.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(concl.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(prevButton,nextButton,addButton,deleteButton,padx=c(15,0))

  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))

  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))

  tkfocus(changeConclusionsWindow)
  tkwait.window(changeConclusionsWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-new("modelConclusionsClass",conclusions=get("concl.temp",tempEnvir))
  return(output)
} # end of function change.conclusions()



################################################################################
################################################################################
#' @description Change function for \code{\linkS4class{modelValidationClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name change.validations
#' @aliases change.validations
#' @title Function for edditing model slot 'validations'
#' @usage   change.validations(validations)
#' @param validations (model validations) is an object of class \code{modelValidationClass}
#' @return This function returns edited model slot 'validations'.
#' @keywords edit
#' @note This function requires some functions from the package \code{tcltk}.
#' @export
#' @examples
#' \donttest{model<-init.Model1()
#' model@@validation<-change.validations(model@@validation)
#' model@@validation}

change.validations<-function(validations)
{
  #-----------------------------------------------------------------------------
  # what happends by pressing "cancel" button
  #-----------------------------------------------------------------------------
  onCancel<-function(...)
  {
    assign("validations.temp",get("validations.original",tempEnvir),tempEnvir)
    tkdestroy(changeValidationsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "ok" button
  #-----------------------------------------------------------------------------
  onOk<-function(...)
  {
    index<-get("index",tempEnvir)
    validations.temp<-get("validations.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This glossary item will be removed from the glossary list!",icon="warning")
      validations.temp<-validations.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the name of the glossary item!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the explanation of the glossary item!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(validations.temp))
      { validations.temp[[index]]<-new("validationClass",
        name=name,
        explanation=explanation)
      } else
      { validations.temp[[index]]@name<-name
        validations.temp[[index]]@explanation<-explanation
      }
    }

    if(length(validations.temp)==0)
    { validations.temp<-list()
    }

    assign("validations.temp",validations.temp,tempEnvir)
    tkdestroy(changeValidationsWindow)
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "previous" button
  #-----------------------------------------------------------------------------
  onPrev<-function(...)
  { index<-get("index",tempEnvir)
    validations.temp<-get("validations.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This glossary item will be removed from the glossary list!",icon="warning")
      validations.temp<-validations.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the name of the glossary item!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the explanation of the glossary item!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(validations.temp))
      { validations.temp[[index]]<-new("validationClass",
        name=name,
        explanation=explanation)
      } else
      { validations.temp[[index]]@name<-name
        validations.temp[[index]]@explanation<-explanation
      }
    }

    index<-index-1
    assign("validations.temp",value=validations.temp,envir=tempEnvir)
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(validations.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",validations.temp[[index]]@explanation)

    if(index==length(validations.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(validations.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(validations.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(validations.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(validations.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }


  #-----------------------------------------------------------------------------
  # what happends by pressing "next" button
  #-----------------------------------------------------------------------------
  onNext<-function(...)
  { index<-get("index",tempEnvir)
    validations.temp<-get("validations.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This glossary item will be removed from the glossary list!",icon="warning")
      validations.temp<-validations.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the name of the glossary item!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the explanation of the glossary item!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(validations.temp))
      { validations.temp[[index]]<-new("validationClass",
        name=name,
        explanation=explanation)
      } else
      { validations.temp[[index]]@name<-name
        validations.temp[[index]]@explanation<-explanation
      }
    }

    assign("validations.temp",value=validations.temp,envir=tempEnvir)
    index<-index+1
    assign("index",value=index,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(validations.temp[[index]]@name))
    tkdelete(explanation.txt,"0.0","end")
    tkinsert(explanation.txt,"0.0",validations.temp[[index]]@explanation)

     if(index==length(validations.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(validations.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(validations.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(validations.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(validations.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "add" button
  #-----------------------------------------------------------------------------
  onAdd<-function(...)
  {
    index<-get("index",tempEnvir)
    validations.temp<-get("validations.temp",envir=tempEnvir)

    name<-tclvalue(tkget(name.entry))
    explanation<-tclvalue(tkget(explanation.txt,"0.0","end"))

    name.check<-gsub(x=name," ",replacement="")
    explanation<-gsub(x=explanation,"\n",replacement="")
    explanation<-gsub(x=explanation,"\t",replacement="")
    explanation.check<-gsub(x=explanation," ",replacement="")

    if(name.check=="" & explanation.check=="" )
    { tkmessageBox(message="The fields 'Name' and 'Explanation' are empty. This glossary item will be removed from the glossary list!",icon="warning")
      validations.temp<-validations.temp[-index]
    } else if(name.check=="" & explanation.check!="")
    { tkmessageBox(message="The 'Name' field is empty, please enter here the name of the glossary item!",icon="error")
      tkfocus(name.entry)
      stop("INVALID INPUT, empty field 'Name'...",call.=FALSE)
    } else if(explanation.check=="" & name.check!="")
    { tkmessageBox(message="The 'Explanation' field is empty, please enter here the explanation of the glossary item!",icon="error")
      tkfocus(explanation.txt)
      stop("INVALID INPUT, empty field 'Explanation'...",call.=FALSE)
    } else
    { if(index>length(validations.temp))
      { validations.temp[[index]]<-new("validationClass",
        name=name,
        explanation=explanation)
      } else
      { validations.temp[[index]]@name<-name
        validations.temp[[index]]@explanation<-explanation
      }
    }

    assign("validations.temp",value=validations.temp,envir=tempEnvir)
    assign("index",value=index+1,envir=tempEnvir)

    tkconfigure(name.entry,text=tclVar(""))
    tkdelete(explanation.txt,"0.0","end")

    if(index==length(validations.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(validations.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(validations.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(validations.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(validations.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
  }

  #-----------------------------------------------------------------------------
  # what happends by pressing "delete" button
  #-----------------------------------------------------------------------------
  onDelete<-function(...)
  {
    sureDelete<-tkmessageBox(message="Are you sure to delete this validation entry?",icon="question",type="yesno",default="no")
    sureDelete<-tclvalue(sureDelete)
    if(sureDelete=="no")
    { stop("",call.=FALSE)
    } else if(sureDelete=="yes")
    { index<-get("index",tempEnvir)
      validations.temp<-get("validations.temp",tempEnvir)

      if(index>1)
      { validations.temp<-validations.temp[-index]
        index<-index-1
        tkconfigure(name.entry,text=tclVar(validations.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",validations.temp[[index]]@explanation)
      } else if(index==1 & index!=length(validations.temp))
      { validations.temp<-validations.temp[-index]
        tkconfigure(name.entry,text=tclVar(validations.temp[[index]]@name))
        tkdelete(explanation.txt,"0.0","end")
        tkinsert(explanation.txt,"0.0",validations.temp[[index]]@explanation)
      } else if (index==1 & index==length(validations.temp))
      { validations.temp<-list()
        tkconfigure(name.entry,text=tclVar(""))
        tkdelete(explanation.txt,"0.0","end")
      }

      assign("validations.temp",validations.temp,tempEnvir)
      assign("index",index,tempEnvir)

      if(index==length(validations.temp) & index !=1)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="normal")
      } else if (index>1 & index<length(validations.temp))
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="normal")
      } else if(index==1 & length(validations.temp)>1)
      { tkconfigure(nextButton,state="normal")
        tkconfigure(addButton,state="disabled")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & 1==length(validations.temp))
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      } else if(index==1 & length(validations.temp)==0)
      { tkconfigure(nextButton,state="disabled")
        tkconfigure(addButton,state="normal")
        tkconfigure(prevButton,state="disabled")
      }
    }
  }

  #-----------------------------------------------------------------------------
  # define help variables
  #-----------------------------------------------------------------------------
  #object<-demoModel1@glossary
  #object<-object1
  #validations.temp<-object@validation
  
  validations.temp<-validations@validation
  if(length(validations.temp)==0) validations.temp<-list(new("validationClass",name="",explanation=""))
  #assign("tempEnvir",value=new.env(),envir=.GlobalEnv)
  assign("tempEnvir",value=new.env())
  assign("index",value=1,envir=tempEnvir)
  assign("validations.temp",value=validations.temp,envir=tempEnvir)
  assign("validations.original",value=validations.temp,envir=tempEnvir)

  #-----------------------------------------------------------------------------
  # create GUI window and frames
  #-----------------------------------------------------------------------------
  changeValidationsWindow<-tktoplevel()
  tkwm.title(changeValidationsWindow,"Edit validations")
  tkwm.resizable(changeValidationsWindow,0,0)  # fixed size, not resizeable
  allFrame<-tkframe(changeValidationsWindow)
  textFrame<-tkframe(allFrame)
  label.font<-tkfont.create(weight="bold",size=10)
  combo.font<-tkfont.create(family="courier",size=10)
  #-----------------------------------------------------------------------------
  # changing name entry
  name.entry<-tkentry(textFrame,borderwidth=2,width=70,relief="sunken",text=tclVar(validations.temp[[1]]@name),font=combo.font)
  name.label<-tklabel(textFrame,font=label.font,text="Name")
  tkgrid(name.label,name.entry,sticky="nw",padx=c(0,10),pady=c(0,5))
  #-----------------------------------------------------------------------------
  # changing explanation entry
  explanation.scrollbar<-tkscrollbar(textFrame,repeatinterval=1,command=function(...)tkyview(explanation.txt,...))
  explanation.txt<-tktext(textFrame,borderwidth=2,width=70,height=15,relief="sunken",
    yscrollcommand=function(...)tkset(explanation.scrollbar,...),wrap="word",font=combo.font)
  explanation.label<-tklabel(textFrame,font=label.font,text="Explanation")
  tkinsert(explanation.txt,"0.0",validations.temp[[1]]@explanation)
  tkgrid(explanation.label,explanation.txt,explanation.scrollbar,sticky="nw",padx=c(0,10),pady=c(0,5))
  tkgrid.configure(explanation.scrollbar,sticky="news")
  #-----------------------------------------------------------------------------
  tkpack(textFrame,side="top")
  #-----------------------------------------------------------------------------
  # button frame
  if(length(validations.temp)>1)
  { nextButtonState<-"normal"
    addButtonState<-"disabled"
  } else if (length(validations.temp)==1)
  { nextButtonState<-"disabled"
    addButtonState<-"normal"
  }
  buttonsFrame<-tkframe(allFrame)
  buttonsFrame1<-tkframe(buttonsFrame)
  nextButton<-ttkbutton(buttonsFrame1,text="next",width=10,command=onNext,state=nextButtonState)
  addButton<-ttkbutton(buttonsFrame1,text="add new",width=10,command=onAdd,state=addButtonState)
  prevButton<-ttkbutton(buttonsFrame1,text="previous",width=10,command=onPrev,state="disabled")
  deleteButton<-ttkbutton(buttonsFrame1,text="delete",width=10,command=onDelete,state="normal")
  tkgrid(prevButton,nextButton,addButton,deleteButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  # ok cancel button frame
  buttonsFrame2<-tkframe(buttonsFrame)
  okButton<-ttkbutton(buttonsFrame2,text="ok",width=10,command=onOk)
  cancelButton<-ttkbutton(buttonsFrame2,text="cancel",width=10,command=onCancel)
  tkgrid(okButton,cancelButton,padx=c(15,0))
  #-----------------------------------------------------------------------------
  tkgrid(buttonsFrame1,buttonsFrame2,padx=c(15,0))
  tkpack(buttonsFrame,side="bottom")
  tkpack(allFrame,padx=c(15,15),pady=c(15,15))
  #-----------------------------------------------------------------------------
  tkfocus(changeValidationsWindow)
  tkwait.window(changeValidationsWindow)

  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  output<-new("modelValidationClass",validation=get("validations.temp",tempEnvir))
  return(output)
} # end of function change.validations()


