
################################################################################
################################################################################
#' @description A function that installs packages required by \code{rrisk}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name init.rrisk
#' @aliases init.rrisk
#' @title Function that installs packages required by 'rrisk'
#' @usage init.rrisk(lib=.libPaths()[1])
#' @param lib is the file path of the packages
#' @keywords init
#' @export
#' @examples
#' \donttest{init.rrisk(lib=.libPaths()[1])}

init.rrisk<-function(lib=.libPaths()[1])
{ 
  cat("Checking whether all required packages are installed...\n")
  #-----------------------------------------------------------------------------
  installedPackages<-installed.packages()[,1]
  requiredPackages<-sort(c(                                
    "eha",  # for Gompertz distribution
    "mc2d", # for triangular and pert distributions
    "xtable", # for latex output of tables
    "gdata",
    "mgcv",
    "network", # for dependencies among model items visualised as graph
    "msm",
    "tcltk", # for dialog windows
    "methods",
    "tkrplot",  # for dialog windows
    "tools",
    "tree",
    "rgdal",
    "png",     # for png files
    "sna",
    "grDevices", # for different graph outputs
    "fitdistrplus", # for fitting distributions, bootdist(), fitdist()
    "rriskDistributions",  # fitting distributions to data or given percentiles
    "rriskBayes" # for Bayxes models
    ))   
  notInstalled<-setdiff(requiredPackages,installedPackages)
  
  if(length(notInstalled)){
    cat("Following packages required by 'rrisk' are not installed: ",notInstalled,"\n")
    #-----------------------------------------------------------------------------  
    for(i in 1:length(notInstalled)){
      cat("Installing ",notInstalled[i]," package...\n" )
      install.packages(notInstalled[i],dependencies=TRUE,lib=lib)    
    }
  }
  # Au?erdem muss noch bwidget installiert werden!!!!
   #try.result<-try(require(BWidget)),silent=TRUE)
   # if (!inherits(try.result, "try-error"))
} # end function init.rrisk()


################################################################################
################################################################################
#' @description This initializes the object rriskSession as an instance of \code{\linkS4class{rriskClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name init.rriskSession
#' @aliases init.rriskSession
#' @title Function to initialize the object rriskSession as an instance of 'rriskClass'
#' @usage init.rriskSession(useDemoModels="all", demomode=FALSE)
#' @param demomode is a boolean value indicating whether the model should be run under the demo mode, \code{demomode=c(TRUE,FALSE)}
#' @param useDemoModels decides which demoModels should be used, \code{useDemoModels=c("no","all","Demo model 1", "Demo model 2", "Demo model 3")}
#' @return rriskSession
#' @keywords init
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels="all")}

init.rriskSession<-function(useDemoModels="all", demomode=FALSE)
{ 
  #-----------------------------------------------------------------------------
  # initialize default scoring systems
  #-----------------------------------------------------------------------------
  scoringSystem1<-init.scoresystem1()
  scoringSystem2<-new("modelScoringClass")
  scoringSystem2@name<-"empty scoring system"
  
  #-----------------------------------------------------------------------------
  # initialize a list R pacvkages on which rrisk depends
  #-----------------------------------------------------------------------------
  rpackages<-sort(c("xtable","tcltk", "methods", "png", "rgdal", "network", "sna", "tools",
   "tree", "gdata", "mgcv", "msm", "tkrplot", "grDevices","fitdistrplus","rriskDistributions","rriskBayes"))
  
  #-----------------------------------------------------------------------------
  # initialize default glossary items
  #-----------------------------------------------------------------------------
  gloss1.rrisk<-new("glossaryClass",
    name="Items",
    explanation="Data set or element or the rrisk model, which contains numerical input or evaluated functions.")
  gloss2.rrisk<-new("glossaryClass",
    name="Parts",
    explanation="A set of model items that describe one given subsystem in the real world risk scenario. Parts can also be used to structure the assessment according to its components.")
  gloss3.rrisk<-new("glossaryClass",
    name="Variability",
    explanation="(Syn.~aleatory uncertainty, irrucuble uncertainty, inherent uncertainty, objective uncertainty, stochastic uncertainty) Concept to describe the true variability of real world features, phenomena or processes.")
  gloss4.rrisk<-new("glossaryClass",
    name="Uncertainty",
    explanation="(Syn.~epistemic uncertainty, reducible uncertainty, subjective uncertainty) Concept to describe our ignorance of real world features, phenomena or processes.")
  gloss5.rrisk<-new("glossaryClass",
    name="Model uncertainty (MU)",
    explanation="Ignorance about the correct mathematical representation of a real world feature, phenomena or process. In the context of rrisk analysis, MU is handled by stratified evaluation of the outcome function.")
  gloss6.rrisk<-new("glossaryClass",
    name="Parameter uncertainty (PU)",
    explanation="Ignorance about the correct mathematical representation of a population parameter. In the context of rrisk analysis, PU is represented using Monte Carlo random variates.")
  gloss7.rrisk<-new("glossaryClass",
    name="Outcome function (OF)",
    explanation="One or more evaluated expressions that qualify the relevant endpoint(s) of the risk assessment.")  
   gloss8.rrisk<-new("glossaryClass",
    name="Two-dimensional simulation",
    explanation="Monte Carlo method for differentiating the effect of variability and ancertainty in qualitative risk assessment. Also known as second-order modeling.")  
   gloss9.rrisk<-new("glossaryClass",
    name="Iteration",
    explanation="One realisation of random numbers drawn from each of the specified sampling distributions for probabilistic (stochastic) model input quantities and used to establish one value of the outcome function.")
   gloss10.rrisk<-new("glossaryClass",
    name="Full model",
    explanation="Terminology in rrisk for teh probabilistic (stochastic) model, which is the basis for the risk estimation.")
  gloss11.rrisk<-new("glossaryClass",
    name="Relaxed model",
    explanation="Terminology in rrisk for the probabilistic (stochastic) model, which is identical with the full model except that all distributions that reflect uncertainty are replaced with uniform distributions spanning the absolute plausible boundaries for the given quantity provided by the user.")
  
  #-----------------------------------------------------------------------------
  # initialize default abbreviations
  #-----------------------------------------------------------------------------
  abbr1.rrisk<-new("glossaryClass",
    name="N1d",
    explanation="Number of iterations for Monte Carlo simulation (1st dimension)")
  abbr2.rrisk<-new("glossaryClass",
    name="N2d",
    explanation="Number of iterations for Monte Carlo simulation (2nd dimension)")
  abbr3.rrisk<-new("glossaryClass",
    name="data",
    explanation="item type: data")
  abbr4.rrisk<-new("glossaryClass",
    name="stid",
    explanation="item type: stratum identification")
  abbr5.rrisk<-new("glossaryClass",
    name="stdi",
    explanation="item type: stratum distribution")
  abbr6.rrisk<-new("glossaryClass",
    name="strv",
    explanation="item type: stratum random variate for assigning each iteration to one of the defined strata")
  abbr7.rrisk<-new("glossaryClass",
    name="numv",
    explanation="item type: numerical value(s)")
  abbr8.rrisk<-new("glossaryClass",
    name="mcrv",
    explanation="item type: Monte Carlo random variate")
  abbr9.rrisk<-new("glossaryClass",
    name="fnrv",
    explanation="item type: Function of mcrv and other item(s)") 
  abbr10.rrisk<-new("glossaryClass",
    name="corv",
    explanation="item type: correlated random variate") 
  abbr11.rrisk<-new("glossaryClass",
    name="mxrv",
    explanation="item type: mixture distribution variate")
  abbr12.rrisk<-new("glossaryClass",
    name="rsrv",
    explanation="item type: bootstrap (distribution of mean and standard deviation) random variate")             
  abbr13.rrisk<-new("glossaryClass",
    name="GAM",
    explanation="generalized additive model")  
  abbr14.rrisk<-new("glossaryClass",
    name="v",
    explanation="role of a model item to represent variability") 
  abbr15.rrisk<-new("glossaryClass",
    name="u",
    explanation="role of a model item to represent uncertainty") 
  abbr16.rrisk<-new("glossaryClass",
    name="uv",
    explanation="role of a model item to represent uncertainty and variability") 
  abbr17.rrisk<-new("glossaryClass",
    name="OF",
    explanation="role of a model item to represent an outcome function")         
  abbr18.rrisk<-new("glossaryClass",
    name="pdf",
    explanation="probability density function")
  abbr19.rrisk<-new("glossaryClass",
    name="cdf",
    explanation="cumulative probability density function")
  abbr20.rrisk<-new("glossaryClass",
    name="2d",
    explanation="two-dimensional")
  #-----------------------------------------------------------------------------
  # create output
  #-----------------------------------------------------------------------------
  rriskSession<-new("rriskClass",
    scoringsystems=list(scoringSystem1,scoringSystem2),
    glossary=list(gloss1.rrisk,gloss2.rrisk,gloss3.rrisk,gloss4.rrisk,
      gloss5.rrisk,gloss6.rrisk,gloss7.rrisk,gloss8.rrisk,gloss9.rrisk,
      gloss10.rrisk,gloss11.rrisk),
    abbreviations=list(abbr1.rrisk,abbr2.rrisk,abbr3.rrisk,abbr4.rrisk,
      abbr5.rrisk,abbr6.rrisk,abbr7.rrisk,abbr8.rrisk,abbr9.rrisk,
      abbr10.rrisk,abbr11.rrisk,abbr12.rrisk,abbr13.rrisk,abbr14.rrisk,
      abbr15.rrisk,abbr16.rrisk,abbr17.rrisk,abbr18.rrisk,abbr19.rrisk,
      abbr20.rrisk),
    rpackages=rpackages)
                                                    
  if(demomode==FALSE){     
    rriskSession@rriskversion<-packageDescription("rrisk")["Version"]$Version
    depends<-packageDescription("rrisk")["Depends"]$Depends
    depends<-gsub(x=depends,pattern="\n",replacement="")
    depends<-strsplit(depends,split=",")[[1]]
    rriskSession@rpackages<-depends[-1]
    rriskSession@rversion<-depends[1]
  } # end if(demomode==FALSE)
  
  #-----------------------------------------------------------------------------
  # add Demo models to the rrisk session
  #-----------------------------------------------------------------------------  
  if(useDemoModels=="all"){
    demomodel1<-init.Model1(demomode)
    if(demomode==TRUE){
      load("demomodel2.Rdata")
      demomodel2<-rriskModel
      demomodel2@name@name<-"Demo model 2"
      load("Demomodel3.Rdata")
      demomodel3<-rriskModel
      demomodel3@name@name<-"Demo model 3"
    } else {
      load(file=system.file("extdata","demomodel2.Rdata", package = "rrisk"))
      demomodel2<-rriskModel
      demomodel2@name@name<-"Demo model 2"
      load(file=system.file("extdata","Demomodel3.Rdata", package = "rrisk"))
      demomodel3<-rriskModel
      demomodel3@name@name<-"Demo model 3"
    }
    rriskSession@models<-list(demomodel1,demomodel2,demomodel3)
  } else if(useDemoModels=="Demo model 1"){
    demomodel1<-init.Model1(demomode)
    rriskSession@models<-list(demomodel1)
  } else if(useDemoModels=="Demo model 2"){
    if(demomode==TRUE){
      load("demomodel2.Rdata")
    } else {
      load(file=system.file("extdata","demomodel2.Rdata", package = "rrisk"))
    }
    demomodel2<-rriskModel
    demomodel2@name@name<-"Demo model 2"
    rm(rriskModel)
    rriskSession@models<-list(demomodel2)
  } else if(useDemoModels=="Demo model 3"){
    if(demomode==TRUE){
      load("Demomodel3.Rdata")
    } else {
      load(file=system.file("extdata","Demomodel3.Rdata", package = "rrisk"))
    }
    demomodel3<-rriskModel
    demomodel3@name@name<-"Demo model 3"
    rm(rriskModel)
    rriskSession@models<-list(demomodel3)
  } else if(useDemoModels=="no"){
    rriskSession@models<-list()
  }
  
  #----------------------------------------------------------------------------- 
  return(rriskSession)
} # end of function init.rriskSession()


################################################################################
################################################################################
#' @description This initializes the object scoringSystem as an instance of \code{\linkS4class{modelScoringClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name init.scoresystem1
#' @aliases init.scoresystem1
#' @title Function to initialize the object scoringSystem as an instance of 'modelScoringClass-class'
#' @usage init.scoresystem1()
#' @return scoringSystem1
#' @keywords init
#' @export
#' @examples
#' \donttest{scoringSystem<-init.scoresystem1()}

init.scoresystem1<-function()
{
  score1.system1<-new(Class="scoreClass",
    notation="U1",
    name="Plausibility",
    explanation="1=high (highly plausible despite absence of factual evidence), 2=medium (plausible despite absence of factual evidence), 3=low (plausibility questionable or not assessed; fictive or speculative assertion)"
  )
  #-------------------------------------------------------------------------------
  score2.system1<-new(Class="scoreClass",
    notation="U2",
    name="Intersubjectivity",
    explanation="1=high (expert opinions are consistent among peers), 2=medium (expert opinions vary among peers to some extent; deviating minority opinions exist), 3=low (expert opinions vary considerably among peers; no clear majority opinion exists)"
  )
  #-------------------------------------------------------------------------------
  score3.system1<-new(Class="scoreClass",
    notation="U3",
    name="Choice space",
    explanation="1=limited (hardly any choice from alternative assumptions), 2=moderately limited (limited choice from alternative assumptions), 3=wide (ample choice from alternative assumptions)"
  )
  #-------------------------------------------------------------------------------
  score4.system1<-new(Class="scoreClass",
    notation="U4",
    name="Sensitivity to limitations",
    explanation="1=hardly sensitive (more resources such as time, money, etc.~would not markedly reduce the uncertainty), 2=moderately sensitive (more resources would markedly reduce the uncertainty), 3=very sensitive (more resources could ressolve this aspect of uncertainty)"
  )
  #-------------------------------------------------------------------------------
  score5.system1<-new(Class="scoreClass",
    notation="U5",
    name="Sensitivity to interests",
    explanation="1=hardly sensitive (conflict of interest (COI) can be excluded), 2=moderately sensitive (no declared interest or COI unlikely to occur), 3=sensitive (declared interest or COI likely to occur)"
  )
  #-------------------------------------------------------------------------------
  score6.system1<-new(Class="scoreClass",
    notation="U6",
    name="Influence",
    explanation="1=low (local effect in the model, few pathways affected), 2=medium (several pathways affected), 3=high (many pathways affected)"
  )
  #-------------------------------------------------------------------------------
  score7.system1<-new(Class="scoreClass",
    notation="K1",
    name="Proxy",
    explanation="1=high (exact measure or good fit for purpose), 2=medium (well correlated), 3=low (weak correlation or not clearly correlated)"
  )
  #-------------------------------------------------------------------------------
  score8.system1<-new(Class="scoreClass",
    notation="K2",
    name="Strength of evidence",
    explanation="1=high (strong evidence, systematic review and meta-analysis of suitable studies, official statistics, census data, controlled randomised trial, observational study with appropriate study design and adjustment for confounding), 2=medium (some evidence, like level 1 but constraints of internal or external validity apply), 3=low (no evidence, expert opinion, assumption)"
  )
  #-------------------------------------------------------------------------------
  score9.system1<-new(Class="scoreClass",
    notation="K3",
    name="Internal validity",
    explanation="1=high (internal validity assessed with positive outcome, measurement methods validated and considered best-suited for the purpose), 2=medium (internal validity assessed with acceptable outcome, measurement methods validated and considered acceptable for the purpose), 3=low (internal validity not assessed or assessed with questionable outcome)"
  )
  #-------------------------------------------------------------------------------
  score10.system1<-new(Class="scoreClass",
    notation="K4",
    name="External validity",
    explanation="1=high (external validity for the purpose of this assessment can be presupposed), 2=medium (external validity for the purpose of this assessment can be presupposed with limitations), 3=low (external validity for the purpose of this assessment may not be given)"
  )
  #-------------------------------------------------------------------------------
  scoringSystem1<-new("modelScoringClass",
    name="default scoring system",
    tableheader="Default criteria and qualitative scores for uncertainty (U) and knowledge (K) base (modified after van der Sluijs et al., Risk Analysis 25: 481-492, 2005)",
    explanatory="Depending on the aspect of the model assessed, either the criteria for expressing uncertainty (U) or those expressing knowledge base (K) may be more relevant.",
    values=c(0,1,2,3),
    vcolors=c("blue"=0,"green"=1,"yellow"=2,"red"=3),
    vmeanings=c("notapplicable"=0,"low"=1,"medium"=2,"high"=3),
    systemtype="rrisk scoring system",
    scoring=list(score1.system1,score2.system1,score3.system1,score4.system1,
    score5.system1,score6.system1,score7.system1,score8.system1,score9.system1,
    score10.system1))
  #-------------------------------------------------------------------------------
  return(scoringSystem1)
} # end of function init.scoresystem1



################################################################################
################################################################################
#' @description This initializes an object as the instance of \code{\linkS4class{modelClass}}.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name init.Model1
#' @aliases init.Model1
#' @title Function to initialize an object as the instance of 'modelClass'
#' @usage init.Model1(demomode=FALSE)
#' @param demomode is a boolean value indicating whether the model should be run under the demo mode
#' @return demoModel1 auxiliary boolean, \code{demomode=c(TRUE,FALSE)}
#' @keywords init
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()}

init.Model1<-function(demomode=FALSE)
{
  #-----------------------------------------------------------------------------
  # define model name
  #-----------------------------------------------------------------------------
  name.Model1<-new("modelNameClass",name="Demo model 1")

  #-----------------------------------------------------------------------------
  # define model version
  #-----------------------------------------------------------------------------
  version.Model1<-new("modelVersionClass",
    status="Draft",
    minorupdate=0,
    majorupdate=1,
    subtitle="Space for subtitle",
    editedby="Matthias Greiner")

  #-----------------------------------------------------------------------------
  # define model authors
  #-----------------------------------------------------------------------------
  author1.Model1<-new("authorClass",
    name="Matthias Greiner",
    institution="Federal Institute for Risk Assessment (BfR)",
    email="matthias.greiner@bfr.bund.de")

  author2.Model1<-new("authorClass",
    name="Kristin Tolksdorf",
    institution="Federal Institute for Risk Assessment (BfR)")

  author3.Model1<-new("authorClass",
    name="Juliane Braeunig",
    institution="Federal Institute for Risk Assessment (BfR)")

  author4.Model1<-new("authorClass",
    name="Christine Mueller-Graf",
    institution="Federal Institute for Risk Assessment (BfR)")
  #-----------------------------------------------------------------------------
  authors.Model1<-new("modelAuthorsClass",
    authors=list(author1.Model1,author2.Model1,author3.Model1,author4.Model1))

  #-----------------------------------------------------------------------------
  # define model settings
  #-----------------------------------------------------------------------------
  settings.Model1<-new("modelSettingsClass")

  #-----------------------------------------------------------------------------
  # define model basic descriptions
  #-----------------------------------------------------------------------------
  basic0.Model1<-new("basicsClass",
    name="mainDoc",
    explanation="")

  basic1.Model1<-new("basicsClass",
    name="Background",
    explanation="Escherichia (E.) coli O157:H7 represents a microbial hazard in food stuffs such as ground beef. It is known that good kitchen hygiene and proper cooking reduces the risk of food-borne outbreaks caused by this bacterium. A model for assessing the risk of illness in children less than three years of age due to E.coli O157:H7 in ground beef has been used as simple example for the application of the R-based package mc2d for two-dimensional Monte-Carlo  modelling  \\citep{pouillot2009mp}. We have adapted this example to illustrate the use of \\trrisk.")

  basic2.Model1<-new("basicsClass",
    name="Objectives",
    explanation="The objective of the model is to assess the risk of illness in children below age of three years due to consumption of a single serving prepared from ground beef contaminated with E.coli O157:H7. The effect of uncertainty in input variables should be taken into account.")

  basic3.Model1<-new("basicsClass",
    name="Scope",
    explanation="The model does not consider pre-harvest processes such as infection of beef cattle and associated risk factors including on-farm management options. The assessment should provide preliminary information about E. coli O157:H7 infection risk of the specified age group under German conditions. However, it is anticipated that German food consumption data for the target group do not exist and that extrapolation from other data sources may be necessary. An assessment of exposure frequency is beyond the scope of this model.")

  basic4.Model1<-new("basicsClass",
    name="Model concept",
    explanation="The model considers the occurrence of E. coli O157:H7 in raw beef, its fate during storage and processing and the resulting average risk of illness for children below age of three related to consumption of a single serving of dish prepared from ground beef. The relevant real-life processes reflected in the model were divided into four parts, \\emph{Occurrence}, \\emph{Processing and storage}, \\emph{Exposure} and \\emph{Consequences}. Probabilistic (stochastic) modelling was used to capture variability and uncertainty in model inputs.")
  #-------------------------------------------------------------------------------
  basics.Model1<-new("modelBasicsClass",
    basics=list(basic0.Model1,basic1.Model1,basic2.Model1,basic3.Model1,basic4.Model1))

  #-----------------------------------------------------------------------------
  # define model references
  #-----------------------------------------------------------------------------
  references.Model1<-new("modelReferencesClass",references=c("% This file was created with JabRef 2.5.",
    "% Encoding: Cp1252",
    "",
    "@ARTICLE{ec2004ren,",
    "  author = {EC},",
    "  title = {{REGULATION EC No 853/2004 OF THE EUROPEAN PARLIAMENT AND OF THE COUNCIL of 29 April 2004 laying down specific hygiene rules for food of animal}},",
    "  journal = {Official Journal of the European Communities},",
    "  year = {2004},",
    "  owner = {mgr},",
    "  timestamp = {2010.03.15},",
    "  url = {origin. http://europa.eu.int/eurlex/pri/de/oj/dat/2004/l_139/l_13920040430de00550205.pdf}",
    "}",
    "",
    "@TECHREPORT{pouillot2009mp,",
    "  author = {Pouillot, R. and Delignette-Muller, ML and Kelly, DL and Dennis,JB},",
    "  title = {{The mc2d package.}},",
    "  year = {2009},",
    "  file = {:pouillot2009mp.pdf:PDF},",
    "  keywords = {rrisk_demo},",
    "  url = {http://riskassessment.r-forge.r-project.org/docmcEnglish.pdf}",
    "}",
    "",
    "@ARTICLE{van2005cqa,",
    "  author = {Van der Sluijs, J.P. and Craye, M. and Funtowicz, S. and Kloprogge,P. and Ravetz, J. and Risbey, J. and Fermi, V.E. and Campus, C.},",
    "  title = {{Combining quantitative and qualitative measures of uncertainty in model-based environmental assessment: the NUSAP system}},",
    "  journal = {Risk Analysis},",
    "  year = {2005},",
    "  volume = {25},",
    "  pages = {481--492},",
    "  number = {2},",
    "  file = {:van2005cqa.pdf:PDF},",
    "  keywords = {rrisk_demo}",
    "}",
    "",
    "@TECHREPORT{who2009rco,",
    "  author = {WHO},",
    "  title = {Risk Characterization of Microbiological Hazards in Food},",
    "  institution = {World health Organization (WHO)},",
    "  year = {2009},",
    "  file = {:I\\:\\\\public\\\\JabRef\\\\pdf\\\\who2009rco.pdf:PDF},",
    "  owner = {mgr},",
    "  timestamp = {2010.03.05},",
    "  url = {https://www.who.int/foodsafety/publications/micro/MRA17.pdf}",
    "}",
    "",
    "@TECHREPORT{who2008uad,",
    "  author = {WHO},",
    "  title = {Uncertainty and data quality in exposure assessment. Part 1: Guidance document on characterizing and communicating uncertainty in exposure assessment. Part 2: Hallmarks of data quality in chemical exposure assessment},",
    "  institution = {World Health Organization and International Labour Organization and the United Nations Environment Programme},",
    "  year = {2008},",
    "  file = {:who2008uad.pdf:PDF},",
    "  owner = {mgr},",
    "  timestamp = {2009.03.18},",
    "  url = {http://www.who.int/ipcs/publications/methods/harmonization/exposure_assessment.pdf}",
    "}",
    "",
    "@TECHREPORT{who2008hpd,",
    "  author = {{WHO}},",
    "  title = {Harmonization Project Document No. 6: PART 1: GUIDANCE DOCUMENT ON CHARACTERIZING AND COMMUNICATING UNCERTAINTY IN EXPOSURE ASSESSMENT},",
    "  institution = {World Health Organization},",
    "  year = {2008},",
    "  file = {:who2008hpd.pdf:PDF},",
    "  keywords = {uncertainty,},",
    "  owner = {mgr},",
    "  timestamp = {2008.09.01},",
    "  url = {http://www.who.int/ipcs/publications/methods/harmonization/exposure_assessment.pdf}",
    "}",
    ""))

  #-----------------------------------------------------------------------------
  # define model uncertainties
  #-----------------------------------------------------------------------------
  uncert1.Model1<-new("uncertClass",
    namemain="Scenario uncertainty",
    namesub="Agents",
    explanation="The agent (hazard in the context of this microbial risk assessment) is defined in the risk question. The role of E. coli O157:H7 as food-borne hazard is well-documented. Other E. coli serotypes found in beef may cause food-borne outbreaks but are typically associated with less severe clinical signs.  All data (when available) will refer to E. coli O157:H7. We stipulate there is no relevant uncertainty related to the agent.",
    scores=c(1,2,2,1,1,1,0,0,0,0))

  uncert2.Model1<-new("uncertClass",
    namemain="Scenario uncertainty",
    namesub="Sources",
    explanation="The source of the hazard is ground beef as per the risk question. Outbreaks with E. coli O157:H7 may be attributed to other sources but those are beyond the scope of this assessment. We see no important uncertainty related to the sources. Uncertainty due to the origin of the raw product will be described under Spatial aspects. ",
    scores=c(1,1,1,1,1,3,0,0,0,0))

  uncert3.Model1<-new("uncertClass",
    namemain="Scenario uncertainty",
    namesub="Populations",
    explanation="The target subpopulation is young children aged below three years. It is likely that habits in food preparation and consumption vary considerably among families with different social, ethnic or religious backgrounds. The assumptions underlying the model do not fully capture this anticipated variability. Therefore, we see the populations as a relevant source of uncertainty. ",
    scores=c(3,3,2,3,1,3,0,0,0,0))

  uncert4.Model1<-new("uncertClass",
    namemain="Scenario uncertainty",
    namesub="Microenvironments",
    explanation="Growth and survival of E.coli O157:H7 in ground beef is influenced by the chemo-physical conditions such as temperature, water activity, pH and tissue texture. These conditions may be summarised as matrix effects and are expected to change during processing and storage. The model assumes that ground beef remains frozen and that no concentration change occurs. We belief that the simplification in our model results in substantial uncertainty due to neglecting matrix effects.",
    scores=c(3,3,2,3,1,3,0,0,0,0))

  uncert5.Model1<-new("uncertClass",
    namemain="Scenario uncertainty",
    namesub="Temporal",
    explanation="The growth of E. coli O157:H7 is temperature dependent. Seasonal peaks in the occurrence of food-borne outbreaks including those caused by E. coli O157:H7 are known to occur and may be attributed to seasonal changes of the ambient temperature. The model does not consider temporal effects in general nor change of temperature effects in particular. The main purpose of the model is to demonstrate the health risk in relation to cooking practices. Having in regard this purpose, we belief that the model is robust against uncertainty arising from temporal factors.",
    scores=c(2,2,2,2,1,3,0,0,0,0))

  uncert6.Model1<-new("uncertClass",
    namemain="Scenario uncertainty",
    namesub="Spatial",
    explanation="Raw beef may be sourced from different production and marketing systems within the country, from EU or from third countries. The contamination level may vary among those sources as well as their share on the market. This may lead to substantial uncertainty related to the source. ",
    scores=c(2,2,3,3,1,3,0,0,0,0))

  uncert7.Model1<-new("uncertClass",
    namemain="Scenario uncertainty",
    namesub="Measures",
    explanation="The risk question does not include any aspect of risk mitigation. We deal with the effect of cooking in the context of human consumption patterns and activities. Thus, the model does not have to consider any particular risk mitigation measure and we see no uncertainty related to measures.",
    scores=c(1,1,1,1,1,2,0,0,0,0))

  uncert8.Model1<-new("uncertClass",
    namemain="Scenario uncertainty",
    namesub="Activities",
    explanation="It is well-known that heat treatment is an efficient way to reduce -- to zero if done properly -- the concentration of infectious E. coli O157:H7 in beef dishes. There is also evidence that the lack of kitchen hygiene may result in cross-contamination, e.g., from raw ground beef to lettuce. The model captures the cooking practices in a straightforward and simplified manner (see details in the Exposure part). We assert some uncertainty related to quantification of the heat-treatment effect under household conditions and substantial scenario uncertainty related to omission from the model of kitchen practices that may result in cross-contamination.",
    scores=c(2,2,3,3,1,3,0,0,0,0))

  uncert9.Model1<-new("uncertClass",
    namemain="Scenario uncertainty",
    namesub="Pathways",
    explanation="Other pathways than consumption of cooked (or heat-treated) ground beef may occur in reality. The uncertainty due to omission of the cross-contamination pathway is assessed above under Scenario uncertainty: Activities. Future extensions of the model may take different types of dishes into account along with respective consumption patterns and heat inactivation characteristics.",
    scores=c(1,1,1,1,1,2,0,0,0,0))

  uncert10.Model1<-new("uncertClass",
    namemain="Modelling approach uncertainty",
    namesub="Concept",
    explanation="This model is a reflection of the relevant processes that may lead to and modify the occurrence of E. coli O157:H7 in ground beef, exposure by consumption and eventually illness. We are certain about this model concept.",
    scores=c(1,1,1,1,1,2,0,0,0,0))

  uncert11.Model1<-new("uncertClass",
    namemain="Modelling approach uncertainty",
    namesub="Structure",
    explanation="A low level of uncertainty due to model structure can be asserted under the condition that the chosen model is parsimonious and the parameters can be derived from available data. The current model contains only essential input variables which could be supported by empirical evidence. In this sense we do not see the model structure as relevant source of uncertainty.",
    scores=c(1,1,1,1,1,2,0,0,0,0))

  uncert12.Model1<-new("uncertClass",
    namemain="Modelling approach uncertainty",
    namesub="Type",
    explanation="The type of model can be described as probabilistic risk scenario model. It is commonly accepted that this type of model is capable to reflect a known or plausible deterministic model structure as well as variability and uncertainty in model input quantities. We do not attribute the type of model as a source of uncertainty.",
    scores=c(1,1,1,1,1,2,0,0,0,0))

  uncert13.Model1<-new("uncertClass",
    namemain="Modelling approach uncertainty",
    namesub="Resolution",
    explanation="The level of detail in this model is low. Risk factors that may impact any of the key parameters are not included in the model. We belief that the model will provide usefull preliminary risk estimates and identify the most influential parameters, which could be more refined if necessary. We ascribe a medium level of uncertainty due to the level of detail in this model. ",
    scores=c(2,1,3,3,1,3,0,0,0,0))

  uncert14.Model1<-new("uncertClass",
    namemain="Modelling approach uncertainty",
    namesub="Dependencies",
    explanation="The distributions of model input data such as animal-level prevalences within herds, concentration of infectious units and productivity level (to mention only a few) may reveal a complex dependency structure. Dependencies may be estimated from empirical joint distributions or be derived from expert opinion. When the dependencies are not accounted for, the simulated outcome function may be biased towards less weight for high-risk scenarios. The model does not consider any correlated input distributions. We propose that the current model contains no critically dependent input parameters and that the uncertainty related to (omission of) dependency structures is negligible.",
    scores=c(1,1,1,1,1,1,0,0,0,0))

  uncert15.Model1<-new("uncertClass",
    namemain="Modelling approach uncertainty",
    namesub="Dose-response",
    explanation="The distributions of model input data such as animal-level prevalences within herds, concentration of infectious units and productivity level (to mention only a few) may reveal a complex dependency structure. Dependencies may be estimated from empirical joint distributions or be derived from expert opinion. When the dependencies are not accounted for, the simulated outcome function may be biased towards less weight for high-risk scenarios. The model does not consider any correlated input distributions. We propose that the current model contains no critically dependent input parameters and that the uncertainty related to (omission of) dependency structures is negligible.",
    scores=c(2,2,3,3,1,3,0,0,0,0))

  uncert16.Model1<-new("uncertClass",
    namemain="Modelling approach uncertainty",
    namesub="Outcome",
    explanation="The probability of E.coli O157:H7 induced illness due to consumption of a single dish prepared from ground beef is considered as outcome function in accordance with the risk question. This outcome describes a risk conditional on a defined exposure. This risk estimate is an unusual construction because the frequency of exposure is not part of its definition. Therefore, despite internal validity of the outcome with regard to the risk question, we ascribe some uncertainty and potential for improvement to the outcome function.",
    scores=c(3,2,2,3,1,3,0,0,0,0))
  #-----------------------------------------------------------------------------
  uncertainties.Model1<-new(Class="modelUncertaintiesClass",
    note="The assessed sources of uncertainty have been proposed in a guidance paper on uncertainty and data quality in exposure assessment \\cite{who2008hpd} and guidelines for risk characterization of microbial hazards in food \\cite{who2009rco}. Two uncertainty aspects mentioned in the latter document were merged here under the subcategory resolution under the main category of model uncertainty. An assessment of uncertainties related to the outcome function is part of the extended documentation standard of \\trrisk.",
    uncertainties=list(uncert1.Model1,uncert2.Model1,uncert3.Model1,uncert4.Model1,
    uncert5.Model1,uncert6.Model1,uncert7.Model1,uncert8.Model1,uncert9.Model1,
    uncert10.Model1,uncert11.Model1,uncert12.Model1,uncert13.Model1,uncert14.Model1,
    uncert15.Model1,uncert16.Model1))

  #-----------------------------------------------------------------------------
  # define model parts
  #-----------------------------------------------------------------------------
  part1.Model1<-new("partClass",
    name="Occurrence",
    explanation="This part considers all processes leading to the presence of viable E. coli O157:H7 in raw beef. For the purpose of this (illustrative) model, all beef is considered contaminated with a given mean concentration.",
    items="c0")

  part2.Model1<-new("partClass",
    name="Processing and storage",
    explanation="This part considers the growth and survival of E. coli O157:H7 in the food chain. The slaughter and the end of the storage period of ground beef in private households are defined as start and end point of this part, respectively. The original model presupposed that ground beef remains frozen and that no change of the occurrence and concentration of the hazard occurs \\citep{pouillot2009mp}. For the purpose of illustration, our model contains a simple exponential growth model ($c_t$). The growth parameter of this model is set to $k=0$ for consitency with the original model. In reality, the growth parameter is temperature-dependent.",
    items="k t ct")

  part3.Model1<-new("partClass",
    name="Exposure",
    explanation="This part of the model reflects the exposure of children below age of three to viable E. coli O157:H7 due to consumption of dishes prepared with ground beef. The current model considers the inactivation of the hazard by cooking.",
    items="i sdata s n zipdata")

  part4.Model1<-new("partClass",
    name="Consequence",
    items="r P",
    explanation="In this part of the model, the relevant outcome function is established to answer the risk question, which is the probability of illness due to consumption of a dish containing ground beef. The dose-response function is the key biological process considered in this part.")
  #-------------------------------------------------------------------------------
  parts.Model1<-new("modelPartsClass",
    parts=list(part1.Model1,part2.Model1,part3.Model1,part4.Model1))

  #-----------------------------------------------------------------------------
  # define model glossary
  #-----------------------------------------------------------------------------
  gloss1.Model1<-new("glossaryClass",
    name="Ground beef",
    explanation="Boned meat that has been minced into fragments and contains less than 1\\% salt according to \\cite{ec2004ren}, sourced from cattle.")
  #-------------------------------------------------------------------------------
  glossary.Model1<-new("modelGlossaryClass",glossary=list(gloss1.Model1))

  #-----------------------------------------------------------------------------
  # define model abbreviations
  #-----------------------------------------------------------------------------
  abbr1.Model1<-new("glossaryClass",name="cfu",explanation="colony forming units")
  #-------------------------------------------------------------------------------
  abbreviations.Model1<-new("modelAbbreviationsClass",abbreviations=list(abbr1.Model1))

  #-----------------------------------------------------------------------------
  # define model validation
  #-----------------------------------------------------------------------------
  valid1.Model1<-new("validationClass",
    name="Model concept",
    explanation="The model concept is a reasonable representation of the key processes that are to be considered to answer the risk question. The concept has been verified from a microbiological and modelling viewpoint.")

  valid2.Model1<-new("validationClass",
    name="Model implementation and verification",
    explanation="The correctness of the translation of the model concept into the mathematical model has been checked. The appropriateness of the model parameterisation---and in particular of the choice of distributions for stochastic model input---has been commented in the item descriptions. The unit of observation of each model input quantity was carefully considered when combining inputs by mathematical expressions. The correctness of the mathematical algorithms has been assessed by comparing our results with the published results by \\cite{pouillot2009mp}, who also used R for computations. In addition, the full model (1st order simulation) was also implemented in EXCEL@RISK with consistent results. The automatic documentation of \\trrisk\\ ensures that the model for generating results is identical with the documented model.")

  valid3.Model1<-new("validationClass",
    name="Data validity",
    explanation="Only one of the model parameters ($s$) is supported by data and even for these data the external validity is questionable. Therefore, the model should not be regarded as validated in terms of its underlying data.")

  valid4.Model1<-new("validationClass",
    name="Model output plausibility",
    explanation="The numerical range of the final risk estimate from the full model was in an order of magnitude as expected and the direction of correlation between stochastic model input (\\tmcrv-items) and outcome ($P$) in the full model was as expected. Therefore, the models output was found plausible with regard to our knowldge of the system.")

  valid5.Model1<-new("validationClass",
    name="Model predictive validity",
    explanation="We could have estimated the annual incidence in Germany as further output from the model. Theoretically, this output could be used to calibrate or validate the model through comparison with estimates of the observed number of outbreaks in the target population attributable to E.coli O157:H7 in ground beef. However, such empirical estimates were not available.")    
  #-------------------------------------------------------------------------------  
  validation.Model1<-new("modelValidationClass",
    validation=list(valid1.Model1,valid2.Model1,valid3.Model1,valid4.Model1,valid5.Model1))

  #-----------------------------------------------------------------------------
  # define model comments
  #-----------------------------------------------------------------------------
  comments.Model1<-new("modelCommentsClass",
    comments=list("The structure of the model follows the scenario pathway. The grouping of items into parts could be re-arranged to be consistent with the structure of risk assessments according to Codex Alimentarius if necessary.",
  "The sensitivity analysis indicates that among the uncertain model inputs, the probability of illness due to exposure to a single E.~coli cell ($r$) has a great impact on the outcome risk estimate. The variability of the outcome is largely dependent on the number of viable bacteria in the dish at time of consumption ($n$). The effect of this variable is due to the fact that this variable captures also the effect of other important variable such as the number of viable bacteria before preparation ($c_t$), the heat inactivation ($i$) and the size of the dish ($s$)."))

  #-----------------------------------------------------------------------------
  # define model conclusions
  #-----------------------------------------------------------------------------
  conclusions.Model1<-new("modelConclusionsClass",
    conclusions=list("This illustrative model predicts that the population average probability of illness due to E.coli O157:H7 in child below age of three years attributable to consumption of one serving prepared from ground beef is around 1.4 \\% (based on 2d simulation with 95\\% credible interval of 0.6 -- 2.5 ]\\%).",
    "The probabilistic modelling approach indicates that, taking into account variability and uncertainty, this probability could be much higher in rare cases ($>$ 61 \\% as derived from the upper 95\\% credible interval of the 99th precentile).",
    "The dose-response model and in particular the uncertain probability ($r$) of illness due to a single E.coli cell is one of the priorities for improvements of the model.",
    "This model is only for illustrative purposes and all input information should be verified or updated before drawing conclusions."))

  #-----------------------------------------------------------------------------
  # define model archive
  #-----------------------------------------------------------------------------
  archive.Model1<-new("modelClass")

  #-----------------------------------------------------------------------------
  # define model graphics
  #-----------------------------------------------------------------------------
  if(demomode==TRUE){
    graphdata1<-readPNG(source="EcoliBeefChildren.PNG")
  } else{
    graphdata1<-readPNG(source=system.file("img", "EcoliBeefChildren.PNG", package="rrisk"))
  } # end if(demomode==TRUE)
  
  graph1.Model1<-new("graphClass",
    explanation="Conceptual pathway for the model to assess the probability of illness due to E.coli 0157:H7 in children below age of three years after consumption of a dish prepared from ground beef.",
    cover=TRUE,
    graphdata=graphdata1)  
  #-----------------------------------------------------------------------------
  graphs.Model1<-new("modelGraphsClass",graphs=list(graph1.Model1))

  #-----------------------------------------------------------------------------
  # define model data
  #-----------------------------------------------------------------------------
  #dishsize<-c(35,27,31,65,64,21,78,77,110,29,44,44,82,41,53,46,31,39,38,18,
  #  20,32,64,32,31,19,52,50,45,75,27,47,37,52,28,62,38,47,65,38,18,46,52,28,61,49,
  #  55,21,48,18,80,65,46,36,82,56,76,46,39,33,54,49,49,57,62,60,45,34,50,70,89,35,
  #  44,65,43,57,41,120,42,31,28,74,50,85,38,68,56,26,33,160,41,24,42,48,57,58,87,
  #  60,14,35,27,36,57,59,28,100,51,29,11,70,31,87,82,54,57,55,13,49,69,17,63,56,
  #  53,41,47,60,110,110,58,51,97,40,37,52,49,43,49,48,97,43,63,140,63,25,45,11,49,
  #  23,52,15,45,34,13,58,24,41,70,47,29,16,36,29,30,120,26,89,66,14,32,29,79,90,
  #  15,60,7,24,15,21,81,71,58,25,41,59,27,44,51,47,110,96,82,58,140,25,77,54,47,
  #  130,57,72,47,70,36,39,38,27,36,15,16,54,22,66,14,20,19,80,20,54,30,24,82,9,36,
  #  100,41,34,34,79,29,35,39,21,23,61,67,29,42,80,56,68,44,26,73,57,42,34,29,70,
  #  21,71,56,33,42,32,97,42,54,19,96,67,14,33,22,45,12,30,33,83,65,66,27,36,37,17,
  #  50,21,52,41,31,83,110,25,38,71,59,27,25,28,72,98,46,36,30,18,54,4,19,46,82,71,
  #  15,50,65,36,14,76,66,91,29,39,39,45,27,63,69,41,20,37,98,38,41,23,80,53,27,32,
  #  46,18,51,82,49,37,52,43,56,61,18,38,9,48,47,57,65,91,55,34,42,72,62,31,84,23,
  #  46,29,51,46,62,25,86,37,14,56,57,36,48,45,86,99,38,26,53,19,91,25)
  #  dishsize<-data.frame(dishsize=)
  
  #-----------------------------------------------------------------------------
  # define model items
  #-----------------------------------------------------------------------------
  c0<-matrix(ncol=1,data=rnorm(n=1* settings.Model1@N,mean=10,sd=2),byrow=TRUE)
  colnames(c0)<-"c0"
  c0.item<-new(Class="itemClass",
    part="Occurrence",
    name="c0",
    title="Mean concentration of the bacteria in ground beef",
    explanation="The mean concentration of E. coli O175:H7 in ground beef in not known with certainty. Microbiologistics think that the uncertainty around this estimate can be represented by a normal distribution with parameters m=10 and s=2.",
    type="Monte-Carlo random variate (mcrv)",
    typecode="mcrv",
    definition="norm(mean=10,sd=2)",
    depitem="ct",
    unit="cfu/g; applies to 1g raw beef",
    role="This variable represents uncertainty only (u)",
    rolecode="u",
    plausimin=0,
    plausimax=1000000,
    scores=c(1,2,3,3,0,2,0,3,0,0),
    assumptions="Uniform random mix and negligible variability is assumed (homogeneity). The model makes a simplifying assumptions that E. coli O157:H7 occurs with a fixed mean average concentration. We are uncertain whether the assumed concentration reflects realistic conditions.",
    remark="The condentration of the hazard c0 is used to establish the Poisson mean value (see item n below). The Normal distribution was chosen to represent the uncertainty about the mean. Only positive values for c0 are plausible. negative values are very unlikely to occur with the given parametrisation (Pr(c0<0)=2.8E-7). Replacing the assumed distribution fitted to an empirical sample mean estimate will remedy this issue of model uncertainty.",
    reference="\\cite{pouillot2009mp}",
    fullc="matrix(ncol=1,data=rnorm(n=1*rriskModel@settings@N,mean=10,sd=2),byrow=TRUE)",
    relaxc="matrix(ncol=1,data=runif(n=1*rriskModel@settings@N,min=0,max=1e+06),byrow=TRUE)",
    data=summary(c0))
  #-------------------------------------------------------------------------------
  k<-0
  k.item<-new(Class="itemClass",
    part="Processing and storage",
    name="k",
    title="Exponential growth rate",
    explanation="The specific growth rate $k$ of E. coli =157:H7 according to the simple model $c_t=c_0 e^{kt}$, where $c_t$ and $c_0$ is the number of bacteria at time $t$ and $t=0$, respectiveliy.",
    type="Numerical value(s) (numv)",
    typecode="numv",
    definition="0",
    depitem="ct",
    data=0,
    unit="$h^{-1}$; applies to ground beef",
    role="Not defined (nd)",
    rolecode="other",
    plausimin=NULL,
    plausimax=NULL,
    scores=c(3,1,3,1,1,3,0,3,0,0),
    assumptions="The parameter $k=0$ expresses the assumption that ground beef is kept frozen during processing and storage and that no growth occures during this phase.",
    remark="In the original model it is assumed that no growth occurs during processing (i.e., implicitly growth rate of $k=0$ is assumed). The actual processing and storage conditions should be reviewed. If growth occurs, $k>0$ can be chosen.",
    reference="\\cite{pouillot2009mp}",
    fullc="0",
    relaxc="0"
  )
  #-------------------------------------------------------------------------------
  t<-matrix(ncol=1,data=rpert(n=1*settings.Model1@N,min=6,mode=12,max=24),byrow=TRUE)
  colnames(t)<-"t"
  t.item<-new(Class="itemClass",
    part="Processing and storage",
    name="t",
    title="Processing time",
    explanation="Time under processing and storage (freezing in this case)",
    type="Monte-Carlo random variate (mcrv)",
    typecode="mcrv",
    definition="pert(min=6,mode=12,max=24)",
    depitem="ct",
    unit="$h$, applies to ground beef",
    role="This variable represents both uncertainty and variability (uv)",
    rolecode="uv",
    plausimin=4,
    plausimax=48,
    scores=c(1,2,2,2,0,3,0,3,0,0),
    assumptions="It is assumed that - at least in some cases - abusive storage times of ground beef may occur.",
    remark="This item has currently no impact on the outcome because no growth over time is assumed (due to $k=0$). Therefore, this variable was not considered by Poullot et. al. (2009). The beta Pert distrubution is based on hypothetical expert opinion about minimum, maximum and most likely value of the time $t$ accuring in reality. It is not possible to differentiate between variability and uncertainty in this case.",
    reference="No information given",
    fullc="matrix(ncol=1,data=rpert(n=1*rriskModel@settings@N,min=6,mode=12,max=24),byrow=TRUE)",
    relaxc="matrix(ncol=1,data=runif(n=1*rriskModel@settings@N,min=4,max=48),byrow=TRUE)",
    data=summary(t)
  )
  #-------------------------------------------------------------------------------
  ct<-c0*exp(k*t)
  colnames(ct)<-"ct"
  ct.item<-new(Class="itemClass",
    part="Processing and storage",
    name="ct",
    title="Concentration after processing",
    explanation="The number of E. coli O157:H7 after time $t$ according to a simple growth model as defined for the item $k$ and with starting concentration $c_0$.",
    type="Function of mcrv and other item(s) (fnrv)",
    typecode="fnrv",
    definition="c0*exp(k*t)",
    depitem="n",
    unit="cfu/g; applies to ground beef",
    role="Not defined (nd)",
    rolecode="other",
    plausimin=NULL,
    plausimax=NULL,
    scores=c(3,3,3,3,1,3,0,0,0,0),
    assumptions="This primary growth model assumes exponential growth as function of time only.",
    remark="In the current model, no growth is considered because $k=0$ and thus $c_t=c_0$ for all $t$. There is uncertainty about the growth function. For example, the simple model would only apply to time periods with steady growth.",
    reference="\\cite{pouillot2009mp}",
    fullc="c0*exp(k*t)",
    relaxc="c0*exp(k*t)",
    data=summary(ct)
  )
  #-------------------------------------------------------------------------------
  i<-matrix(ncol=1,data=rdiscrete(n=1*settings.Model1@N,values=c(1,1/5,1/50),probs=c(0.027,0.373,0.6)),byrow=TRUE)
  colnames(i)<-"i"
  i.item<-new(Class="itemClass",
    part="Exposure",
    name="i",
    title="Fraction surviving the preparation",
    explanation="The surviving fraction of E. coli O157:H7 during cooking and corresponding percentage of dishes prepared accordingla. No inactivation ($i=1$) is expected for rare cooking and $i=1/5$ or $i=1/50$ surviving bacteria are expected with medium or well-done cooking, respectively. For example, 60% of the dishes are cooked well-done with $1/50$ surviving fraction.",
    type="Monte-Carlo random variate (mcrv)",
    typecode="mcrv",
    definition="discrete(values=c(1,1/5,1/50),probs=c(0.027,0.373,0.6))",
    depitem="n",
    unit="",
    role="This variable represents variability only (v)",
    rolecode="v",
    plausimin=0,
    plausimax=1,
    scores=c(0,0,0,0,0,0,0,2,1,3),
    assumptions="The three levels of heat treatment are sufficient to reflect actual cooking habits in the target population.",
    remark="This items has in fact two independent aspects of information, i.i. the inactivation rate at three (implicit) levels of temperature/time of cooking and the relative frequency of such temperature/time",
    reference="\\cite{pouillot2009mp}",
    fullc="matrix(ncol=1,data=rdiscrete(n=1*rriskModel@settings@N,values=c(1,1/5,1/50),probs=c(0.027,0.373,0.6)),byrow=TRUE)",
    relaxc="matrix(ncol=1,data=runif(n=1*rriskModel@settings@N,min=0,max=1),byrow=TRUE)" ,
    data=summary(i)
  )
  #-----------------------------------------------------------------------------
  # read the data sets assosiated with the current demo model
  #-----------------------------------------------------------------------------
  if(demomode==TRUE)
  { dataset<-read.table(file="sdata.txt",header=TRUE)
    assign("sdata",value=dataset,envir=.GlobalEnv)
  } else
  { data(sdata)
  }
  #-----------------------------------------------------------------------------
  sdata.item<-new(Class="itemClass",
    part="Exposure",
    name="sdata",
    title="Consumption data",
    explanation="Hypothetical data of serving size of a steak dish prepared from ground beef for French children below three years of age.",
    type="Data (data)",
    typecode="data",
    data=sdata,
    definition="variables: dishsize",
    depitem="s",
    unit="g",
    role="Not defined (nd)",
    rolecode="other",
    plausimin=NULL,
    plausimax=NULL,
    scores=c(0,0,0,0,0,0,1,2,1,3),
    assumptions="It is assumed that the children eat the entire serving.",
    remark="The data is hypothetical and has been generated for illustration. The extrapolation from French to German consumption leads to extrapolation uncertainty of this item. With real data it will be nessesary to assess heterogeneity.",
    reference="No information given",
    fullc="",
    relaxc=""
  )
  #-------------------------------------------------------------------------------
  s<-matrix(ncol=1,data=rgamma(n=1*settings.Model1@N,shape=3.93,rate=0.0803),byrow=TRUE)
  colnames(s)<-"s"
  s.item<-new(Class="itemClass",
    part="Exposure",
    name="s",
    title="Serving size",
    explanation="The serving size of a steak dish prepared from ground beef.",
    type="Monte-Carlo random variate (mcrv)",
    typecode="mcrv",
    definition="gamma(shape=3.93,rate=0.083); fitted to sdata$dishsize",
    depitem="n",
    unit="g; applies to one serving",
    role="This variable represents variability only (v)",
    rolecode="v",
    plausimin=0.001,
    plausimax=200,
    scores=c(0,0,0,0,0,0,1,2,1,3),
    assumptions="It is assumed that the children eat the entire serving.",
    remark="The distribution model was fitted to the data sdata. The gamma model fitted the data best (no results shown). The uncertainty assessment is optimistic as no original data sources were described. The xtrapolation from French to German consumption data increases the uncertainty of this item.",
    reference="\\cite{pouillot2009mp}",
    fullc="matrix(ncol=1,data=rgamma(n=1*rriskModel@settings@N,shape=3.93,rate=0.0803),byrow=TRUE)",
    relaxc="matrix(ncol=1,data=runif(n=1*rriskModel@settings@N,min=0.001,max=200),byrow=TRUE)",
    data=summary(s)
  )
  #-------------------------------------------------------------------------------
  n<-matrix(ncol=1,data=rpois(n=1*settings.Model1@N,lambda=ct*i*s),byrow=TRUE)
  colnames(n)<-"n"
  n.item<-new(Class="itemClass",
    part="Exposure",
    name="n",
    title="Exposure concentration",
    explanation="The number of viable E. coli O157:H7 in a ready-to-eat serving is expressed using the Poisson distribution model. The parameter of this distribution is the product of two random variates ($i$ and $s$) and one function of random variates ($c_t$). Therefore, the Poisson parameter is in fact a sequance of random numbers with same length as the number of iterations. Consequently, the number $n$ of bacterial cells per serving can be thought as random with an expected value given by the product of the median values of the three input quantities. The variance of this Poisson distribution is inflated due to variability captured in its parameter. Other sources of overdispersion, such as clustering of bacteria, are not considered.",
    type="Monte-Carlo random variate (mcrv)",
    typecode="mcrv",
    definition="pois(lambda=ct*i*s)",
    depitem="P",
    unit="cfu/serving; applies to one serving",
    role="This variable represents variability only (v)",
    rolecode="v",
    plausimin=0,
    plausimax=100000,
    scores=c(0,0,0,0,0,0,0,0,0,0),
    assumptions="Random mix of bacteria in the meat product is presupposed.",
    remark="The dispersion of bacteria in the food matrix, which is a phenomenon of variability, is described as Poisson process. It is noted that the outcome function $P$ depends algebraically on only two input quantities, namly $n$  and $r$, the latter being an mcrv-item expressing uncertainty. Therefore, it is also technically required that $n$ is assigned to be an expression of variability because otherwise the two-dimensional simulation would not result in any possible resolution of variability and uncertainty for this model. N0o additional data were used to define this item. Therefore, the assessment of evidence and uncertainty is not applicable.",
    reference="\\cite{pouillot2009mp}",
    fullc="matrix(ncol=1,data=rpois(n=1*rriskModel@settings@N,lambda=ct*i*s),byrow=TRUE)",
    relaxc="matrix(ncol=1,data=sample(x=0:1e+05,size=1*rriskModel@settings@N,replace=TRUE),byrow=TRUE)",
    data=summary(n)
  )
  #-------------------------------------------------------------------------------
  r<-matrix(ncol=1,data=runif(n=1*settings.Model1@N,min=0.0005,max=0.0015),byrow=TRUE)
  colnames(r)<-"r"
  r.item<-new(Class="itemClass",
    part="Consequence",
    name="r",
    title="Probability of illness per bacterial cell",
    explanation="The theoretical probability of illness due to exposure to a single cell of E. coli O157:H7 (single hit model).",
    type="Monte-Carlo random variate (mcrv)",
    typecode="mcrv",
    definition="unif(min=0.0005,max=0.0015)",
    depitem="P",
    unit="",
    role="This variable represents uncertainty only (u)",
    rolecode="u",
    plausimin=0,
    plausimax=0.5,
    scores=c(3,3,3,3,0,3,0,3,0,0),
    assumptions="The single-hit model presupposed that the bacteria are randomly distributed and that a constant interaction between the bacterial cell and the host applies. The model also implies that there is no safe limit of concentration below which no illness is triggered.",
    remark="This value is uncertain as reflected by the uniform distribution. The uniform distribution was chosen bevcause only minimum and maximum values were available from experts.",
    reference="\\cite{pouillot2009mp}",
    fullc="matrix(ncol=1,data=runif(n=1*rriskModel@settings@N,min=0.0005,max=0.0015),byrow=TRUE)",
    relaxc="matrix(ncol=1,data=runif(n=1*rriskModel@settings@N,min=0,max=0.05),byrow=TRUE)",
    data=summary(r)
  )
  #-------------------------------------------------------------------------------
  P<-1-(1-r)^n
  colnames(P)<-"P"
  P.item<-new(Class="itemClass",
    part="Consequence",
    name="P",
    title="Probability of illness per serving",
    explanation="The probability of illness in children below age of three years associated with exposure to E. coli =157:H7 in one serving of a dish made of ground beef.",
    type="Function of mcrv and other item(s) (fnrv)",
    typecode="fnrv",
    definition="1-(1-r)^n",
    depitem="",
    unit="applies to one serving",
    role="This item has been defined as outcome function (OF)",
    rolecode="OF",
    plausimin=NULL,
    plausimax=NULL,
    scores=c(0,0,0,0,0,0,0,3,0,0),
    assumptions="Response is not affected by immunological or other physological status.",
    remark="Expressing the risk per serving is suitable if there is relatively little variability in the serving sizes consumed. There was no evidence available to establish the risk for hight intake consumers.",
    reference="\\cite{pouillot2009mp}",
    fullc="1-(1-r)^n",
    relaxc="1-(1-r)^n",
    data=summary(P)
  )
  #-----------------------------------------------------------------------------
  items.Model1<-new("modelItemsClass",items=list(c0.item,k.item,t.item,ct.item,
    i.item,sdata.item,s.item,n.item,r.item,P.item))

  #-----------------------------------------------------------------------------
  # initialize demo model 1
  #-----------------------------------------------------------------------------
  demoModel1<-new("modelClass",
    name=name.Model1,
    modeltype="rrisk demo model",
    version=version.Model1,
    authors=authors.Model1,
    settings=settings.Model1,
    scoring=init.scoresystem1(),
    basics=basics.Model1,
    references=references.Model1,
    uncertainties=uncertainties.Model1,
    parts=parts.Model1,
    graphs=graphs.Model1,
    glossary=glossary.Model1,
    abbreviations=abbreviations.Model1,
    items=items.Model1,
    validation=validation.Model1,
    conclusions=conclusions.Model1,
    comments=comments.Model1,
    archive=archive.Model1)

  return(demoModel1)
} # end of function init.Model1()
