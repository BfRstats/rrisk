
################################################################################
##                   Function tolatex.preamble                                ##
################################################################################
#' @description This creates the preamble for the LaTeX script.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.preamble
#' @aliases tolatex.preamble
#' @title LaTeX code for setting the preamble
#' @usage tolatex.preamble(rriskModel, enc, file.name)
#' @param rriskModel is an instance of the \code{rriskModel} with slots including \code{sty}
#' @param enc is the encoding variable of the system
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @export
#' @keywords report
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.preamble(rriskModel=rriskModel, enc=options()$encoding, file.name="report.tex") }

## \\usepackage[latin1]{inputenc}
tolatex.preamble<-function(rriskModel, enc, file.name)
{
cat("Writing preambel of the LaTeX document...\n") 
myPackages <- rriskModel@settings@sty
packages<-"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             The preamble                                   %%

\\documentclass[a4paper,authoryear,10pt]{article}

%\\usepackage[ngerman]{babel}
\\usepackage{hyperref}
\\usepackage{multirow}
\\usepackage{natbib}
\\usepackage{threeparttable}
\\usepackage{setspace}
\\usepackage{longtable}
\\usepackage{color} 
\\usepackage{fancyhdr}
\\usepackage{textcomp}
\\usepackage{graphicx}
\\usepackage{lscape}
\\usepackage{type1cm}
\\usepackage{eso-pic}
\\usepackage{here}
\\usepackage{listings}
\\lstset{ 
  language=S, 
  breaklines=true, 
  breakatwhitespace=false
}

\\usepackage[babel,english=british]{csquotes}
\\usepackage[margin=1pt,format=plain,font=small,labelfont=bf]{caption}
"
if (enc == "MAC") {
  packages <- append(packages, "\\usepackage[applemac]{inputenc}")
} else {
  packages <- append(packages, "\\usepackage[latin1]{inputenc}")
}

preamble <- "
\\definecolor{darkred}{rgb}{0.5,0,0}
\\definecolor{darkgreen}{rgb}{0,0.5,0}
\\definecolor{darkblue}{rgb}{0,0,0.5}

\\hypersetup{colorlinks,linkcolor=darkblue,filecolor=darkgreen, urlcolor=darkred ,citecolor=darkblue}
     
\\newcommand{\\trrisk}{{\\tt rrisk}}
\\newcommand{\\tdata}{{\\tt data}}
\\newcommand{\\tstra}{{\\tt stra}}
\\newcommand{\\tstrv}{{\\tt strv}}
\\newcommand{\\tnumv}{{\\tt numv}}
\\newcommand{\\tmcrv}{{\\tt mcrv}}
\\newcommand{\\tfnrv}{{\\tt fnrv}}
\\newcommand{\\tcorv}{{\\tt corv}}
\\newcommand{\\tmxrv}{{\\tt mxrv}}
\\newcommand{\\trsrv}{{\\tt rsrv}}
\\newcommand{\\tbsrv}{{\\tt bsrv}}
\\newcommand{\\tbdjp}{{\\tt bdjp}}
\\newcommand{\\tstid}{{\\tt stid}}
\\newcommand{\\tfdoc}{{\\tt fdoc}}
\\newcommand{\\tRF}{{\\tt RF}}
\\newcommand{\\tMU}{{\\tt MU}}
\\newcommand{\\tPU}{{\\tt PU}}
\\newcommand{\\tOF}{{\\tt OF}}
\\newcommand{\\tA}{{\\tt A}}
\\newcommand{\\tB}{{\\tt B}}
\\newcommand{\\tC}{{\\tt C}}
\\newcommand{\\tY}{{\\tt Y}}


\\newcommand\\xtable[6]{\\begin{table}[bht]
    \\begin{minipage}{#2}\\caption{\\it #3}
    \\bigskip \\begin{tabular}{#4}
    \\hline\\\\[-.2cm]#5
    \\\\[.15cm]\\hline\\label{#1}\\end{tabular}
    {\\footnotesize\\begin{flushleft}\\vspace{-.3cm}#6\\end{flushleft}}
    \\end{minipage}\\end{table}}
\\newcommand\\midline{\\\\[.2cm]\\hline \\\\[-.2cm]}

\\newcommand{\\xjpg}[3]{
    \\begin{figure}[hbt] \\begin{center}
    \\begin{minipage}{#2}
    \\includegraphics[width=#2, keepaspectratio] {#1.jpg}
    \\caption{#3}\\label{f.#1}
    \\end{minipage}\\end{center}\\end{figure}}

\\newcommand{\\xpng}[3]{
    \\begin{figure}[hbt] \\begin{center}
    \\begin{minipage}{#2}
    \\includegraphics[width=#2, keepaspectratio] {#1.png}
    \\caption{#3}\\label{f.#1}
    \\end{minipage}\\end{center}\\end{figure}}

\\newcommand{\\xpdf}[3]{
    \\begin{figure}[hbt] \\begin{center}
    \\begin{minipage}{#2}
    \\includegraphics[width=#2, keepaspectratio] {#1}
    \\caption{#3}\\label{f.#1}
    \\end{minipage}\\end{center}\\end{figure}}
 
\\newcommand\\xltable[6]{\\begin{longtable}[bht]
    \\begin{minipage}{#2}\\caption{\\it #3}
    \\bigskip \\begin{tabular}{#4}
    \\hline\\\\[-.2cm]#5
    \\\\[.15cm]\\hline\\label{#1}\\end{tabular}
    {\\footnotesize\\begin{flushleft}\\vspace{-.3cm}#6\\end{flushleft}}
    \\end{minipage}\\end{longtable}}
\\newcommand{\\rdarrow}{\\hspace{.2cm}{\\tt <\\hspace{-.05cm}<\\hspace{-.05cm}-}\\hspace{.2cm}}
\\newcommand{\\xnote}{$^\\bigtriangledown$\\marginpar}

%#\\newcommand\\xtable[6]{\\begin{table}[bht]
%#    \\begin{minipage}{#2}
%#}

\n
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\n
\n"

if(rriskModel@version@status=="Draft")
{ watermark<-"
\\makeatletter
\\AddToShipoutPicture{%
            \\setlength{\\@tempdimb}{.5\\paperwidth}%
            \\setlength{\\@tempdimc}{.5\\paperheight}%
            \\setlength{\\unitlength}{1pt}%
            \\put(\\strip@pt\\@tempdimb,\\strip@pt\\@tempdimc){%
        \\makebox(0,0){\\rotatebox{45}{\\textcolor[gray]{0.80}%
        {\\fontsize{1cm}{1cm}\\selectfont{Draft. Do not distribute. Do not cite.}}}}%
            }%
}
\\makeatother  \n\n
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\n
\n
"
}

type <- 

cat(packages[1:2], file = file.name)
cat(preamble, file = file.name, append = TRUE)
if(rriskModel@version@status=="Draft") cat(watermark, file = file.name, append = TRUE)
}


################################################################################
##                     Function tolatex.frontmatter                           ##
################################################################################
#' @description This creates the LaTeX scripts producing the entire frontmatter of the documentation 
#' of 5 pages, including the coverpage, inside front cover, contents, acknowledgements 
#' as well as a brief introduction of the document. 
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.frontmatter
#' @aliases tolatex.frontmatter
#' @title LaTeX code for setting the frontmatter
#' @usage tolatex.frontmatter(rriskModel, disclaimer, file.name)
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param disclaimer is the slot disclaimer of an instance of \code{rriskClass} 
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' rriskSession<- init.rriskSession(useDemoModels = "all")
#' tolatex.frontmatter(rriskModel=rriskModel, disclaimer=rriskSession@@disclaimer, 
#' file.name="report.tex") }

tolatex.frontmatter <- function(rriskModel,disclaimer,file.name)
{
cat("Writing frontmatter of the LaTeX document...\n") 
    settings<-rriskModel@settings
    if (settings@coverheader=="concept graph")
    { if(length(rriskModel@graphs@graphs)>0)
      { coverpic<-""
        for(i in 1:length(rriskModel@graphs@graphs))
        { if(rriskModel@graphs@graphs[[i]]@cover==TRUE & !is.null(rriskModel@graphs@graphs[[i]]@graphdata))
          coverpic <- paste(gsub(x=rriskModel@name@name," ",replacement=""),"_conceptGraph",i,".png",sep="")
        }
        if(coverpic=="")
        { print("No graph data is available for the concept graph!")
          settings@coverheader<-"none"
        }
      } else
      { print("There are no model concept graphs available!")
        settings@coverheader<-"none"
      }       
    }else if (settings@coverheader == "model network")
    { if(length(rriskModel@items@items)>0)
      { coverpic <- paste(gsub(x=rriskModel@name@name," ",replacement=""),"_modelNetwork.pdf",sep="")
      } else
      { settings@coverheader<-"none"
      }
    }
    
    if (length(rriskModel@authors@authors) > 0) {
    for (i in c(1:length(rriskModel@authors@authors)))
    { if(i==1)
      { autoren<-rriskModel@authors@authors[[i]]@name
      } else
      { if (length(rriskModel@authors@authors[[i]]@name) != 0) {
        autoren <- paste(autoren,", ",rriskModel@authors@authors[[i]]@name,sep="")
        } else {autoren <- paste(autoren, rriskModel@authors@authors[[i]]@name, sep = "")}
      }
    }
    } else autoren  <- ""
    today <- format(Sys.Date(), "%Y-%m-%d")
    #contact <- rriskModel@authors@authors[[1]]@email
    modelversion<-paste(rriskModel@version@majorupdate,rriskModel@version@minorupdate,sep=".")
    
    authors_arr <- strsplit(disclaimer$AUTHORS, split="\n")[[1]]
    for (i in c(1:length(authors_arr))) {
      authors_arr[i] <- paste(authors_arr[i], "\\\\")
    }
    contact_info <- strsplit(disclaimer$CONTACT, split = "\n")[[1]]

cat("
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               Front matter                                 %%
%%                                                                            %%
%%----------------------------------------------------------------------------%%
%%                               Cover page                                   %%
%%----------------------------------------------------------------------------%%
\\pagenumbering{roman}
\\title{Documentation of the Model \\\\ \\texttt{", rriskModel@name@name, "}  \\\\ \\vspace{1cm}  
\\textbf{\\large{", rriskModel@version@subtitle,"}}}
\\date{ ",autoren," } 
\\maketitle \\pagenumbering{roman}", file = file.name, append = TRUE)

if (is.element(settings@coverheader,c("model network","concept graph")))
{ cat(paste("\\maketitle \\begin{center} \\includegraphics[height=8cm]{",coverpic,"}\\end{center}", sep = ""), file = file.name, append = TRUE)
}


cat("\\vspace{1cm}\\begin{center}Version:", rriskModel@version@status, modelversion, "\\\\ Date:", today, " \\end{center}
\\thispagestyle{fancy}
\\fancyhf{}
\\fancyhead[C]{\\large{\\textsf{Probabilistic risk scenario modelling}}} 
\\fancyfoot{\\framebox[13cm]{\\textsf{Model Definition \\& Documentation \\& Analysis using} \\texttt{rrisk}}}
\\newpage

%%----------------------------------------------------------------------------%%
%%                                 Disclaimer                                 %%
%%----------------------------------------------------------------------------%%
\\fbox{\\parbox{\\columnwidth}{\\vspace{0.4cm}\\textsf{
\\hspace{-.1cm}\\textbf{ABOUT} \\\\
The Model has been definded and analysed using rrisk. \\\\", 
disclaimer$ABOUT, " \\\\
\\\\
\\textbf{COPYRIGHT} \\\\", 
disclaimer$COPYRIGHT, " \\\\
\\\\
\\textbf{CONTACT} \\\\", 
contact_info[1], "\\\\", 
contact_info[2]
, " \\\\
\\\\
\\textbf{DEVELOPERS AND CONTRIBUTORS TO THE rrisk PROJECT} \\\\
---------------------------------------------------------------------------------------------- \\\\ \n",
authors_arr," \\\\
---------------------------------------------------------------------------------------------- \\\\ 
\\\\
\\textbf{NOTES} \\\\", 
disclaimer$NOTES," \\\\
\\\\
\\textbf{DISCLAIMER} \\\\", 
disclaimer$DISCLAIMER, " \\\\ 
}}}


\\newpage

%%----------------------------------------------------------------------------%%
%%                                 Contents                                   %%
%%----------------------------------------------------------------------------%%
\\pagenumbering{roman}
\\tableofcontents
\\newpage

%%----------------------------------------------------------------------------%%
%%                         Authors and Contributors                           %%
%%----------------------------------------------------------------------------%%
\\section*{Authors and contributors}
", file = file.name, append = TRUE)

if (length(rriskModel@authors@authors) == 0 ) {
    cat("", file = file.name, append = TRUE)
} else if(length(rriskModel@authors@authors) == 1 ) {
    cat(rriskModel@authors@authors[[1]]@name, file = file.name, append = TRUE)
    if (rriskModel@authors@authors[[1]]@email != "") {
      cat(paste("\\footnote{Email of corresponding author: ", rriskModel@authors@authors[[1]]@email, "}", ), file = file.name, append = TRUE)
      }
    cat(paste(" (", rriskModel@authors@authors[[1]]@institution, ")", sep = ""), file = file.name, append = TRUE)
} else {
    for (i in c(1:length(rriskModel@authors@authors))) {
      cat(rriskModel@authors@authors[[i]]@name, file = file.name, append = TRUE)
      if (rriskModel@authors@authors[[i]]@email != "" ) {
        cat(paste("\\footnote{Email of corresponding author: ", rriskModel@authors@authors[[i]]@email, "}", sep = ""),
        file = file.name, append = TRUE)
      }
      if (length(rriskModel@authors@authors[[i]]@institution) != 0) {
      cat(paste(" (", rriskModel@authors@authors[[i]]@institution, ")", sep=""), file = file.name, append = TRUE)
      } else {cat("", file = file.name, append = TRUE)}

      if (i == length(rriskModel@authors@authors) & length(rriskModel@authors@authors[[i]]@name != 0) & 
      length(rriskModel@authors@authors[[i]]@institution) != 0) {
      cat(".", file = file.name, append = TRUE)
      } else if (length(rriskModel@authors@authors[[i]]@name != 0) & length(rriskModel@authors@authors[[i]]@institution) != 0){
      cat(", ", file = file.name, append = TRUE)
      }
    }
}
cat("

\\newpage

%%----------------------------------------------------------------------------%%
%%                            About the document                              %%
%%----------------------------------------------------------------------------%%

\\section*{About this model documentation}
This documentation has been auto-generated by \\texttt{rrisk} based on the model specification by the authors. 
This report is designed to faciliate a detailed review of the model.\\\\
\\hspace*{5mm} Section \\ref{s.basic} contains a basic description of the model. 
Section \\ref{s.struct} contains a detailed description of all model inputs, sorted by the model parts in which 
they were defined.
Section \\ref{s.impl} provides essential technical information about the implementation
of the model.
 Section \\ref{s.results} provides the numerical and graphical results including results of 
sensitivity analysis.
Section \\ref{s.model.validation} presents the status of verification and validation of the 
model.
 Section \\ref{s.concl} contains comments and conclusions.  \n \\\\
\\hspace*{5mm}
A qualitative scoring system for uncertainty and knowledge base is described in Annex \\ref{s.uncertainties}.
This scheme is used in the same section for assessing general aspects of the model 
and later-on for assessing model input such as data and parameters. 
A glossary and list of abbreviations is given in Annex \\ref{a.glossabb}. The software used and 
the workspace, which contains all data, definitions, descriptions, results and code of this model are described 
in Annex \\ref{a.soft}. Figures and tables are Annex \\ref{a.tabfig}.\n    \\\\
\\newpage
%%                                                                            %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
",
file = file.name, append = TRUE )
}




################################################################################
##                   Function tolatex.basics                                  ##
################################################################################
#' @description This creates the LaTeX scripts producing the first part in the documentation - 
#' "General Description of the model". 
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.basics
#' @aliases tolatex.basics
#' @title LaTeX code producing the general description of the model
#' @usage tolatex.basics(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.basics(rriskModel=rriskModel, file.name="report.tex") }

tolatex.basics<-function(rriskModel,file.name)
{ 
  cat("Writing model basic descriptions...\n")
  basics<-rriskModel@basics@basics
  output<-c("
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               Body matter                                  %%
%%                                                                            %%
%%----------------------------------------------------------------------------%%
%%                      General description of the model                      %%
%%----------------------------------------------------------------------------%%  
\\pagenumbering{arabic}\n
\\section{General description of the model}\\label{s.basic} \n\n")
  for(i in 1:length(basics))
  { name<-basics[[i]]@name
    explanation<-basics[[i]]@explanation
    if(gsub(x=explanation," ",replacement="")!="")
    { if(name=="mainDoc") name<-"Main document"
      label<-paste("\\label{s.basic.",name,"}",sep="")
      temp<-paste("\\subsection{",name,"}",label,"\n",explanation,"\n\n",sep="")
      output<-paste(output,temp,sep="")
    }
  }
  cat(output,file=file.name,append=TRUE)

  figuresText<-c()
  if (length(rriskModel@graphs@graphs) > 0)
  { 
    for (i in 1:length(rriskModel@graphs@graphs))
    { if (!is.null(rriskModel@graphs@graphs[[i]]@graphdata))
      { #figuresTextTemp<-paste("Fig.~\\ref{fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_conceptGraph",i,".png}",sep="")
        #figuresText<-c(figuresText,figuresTextTemp)
        figuresText<-c(figuresText,
            paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_conceptGraph",i,".png",sep="")
            )
      }
    }
    #bfrgreiner Falleinschränkung 
  figuresText.l <- length(figuresText)
  cat(paste("The model concept is illustrated graphically (Fig.~\\ref{",figuresText[1],"}",
    ifelse(figuresText.l>1,paste("--\\ref{",figuresText[figuresText.l],"}).",sep=""),")."),sep=""),
    file=file.name, append=TRUE)
  }

  #if (length(figuresText)>0)
  #{ figuresText<-paste(figuresText[1],"--",figuresText[length(figuresText)],sep="")
  #  figuresText<-paste("\\subsection{Concept graphs} \n The model concept is illustrated graphically in figure(s) ",figuresText,".\n",sep="")
  #  cat(figuresText, file=file.name, append=TRUE)
  #}
  
} # end of fucntion  tolatex.basics()


################################################################################
##                   Function tolatex.uncert                                  ##
################################################################################
#' @description This creates the LaTeX scripts producing the second part in the documentation - 
#' "Uncertainties and knowledge base".
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.uncert
#' @aliases tolatex.uncert
#' @title LaTeX code for producing texts displaying uncertainties of the model
#' @usage tolatex.uncert(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.uncert(rriskModel=rriskModel, file.name="report.tex") }

tolatex.uncert<-function(rriskModel,file.name)
{ cat("Writing model uncertainties...\n")
  ## erzeugung scoringText
  scoring <- rriskModel@scoring@scoring
  scoringText<-""
  scoreNames<-c()
  if(length(scoring)>0)
  { for(k in 1:length(scoring))
    { temp<-scoring[[k]]
      scoringTextTemp<-paste(" ",tolower(temp@name)," (",temp@notation,")",sep="")
      if(k!=length(scoring))
      { 
      scoringTextTemp<-paste(scoringTextTemp,", ",sep="")
      }#else  scoringTextTemp<-paste(scoringTextTemp,".",sep="")
      scoringText<-paste(scoringText,scoringTextTemp,sep="")
      scoreNames<-c(scoreNames,scoring[[k]]@notation) 
    }
    scoreNames<-paste(scoreNames,collapse=" & ")
  }
  
  cat(paste("
  %\\pagenumbering{arabic}\n
  \\appendix
%%----------------------------------------------------------------------------%%
%%                      Uncertainties and knowledge base                      %%
%%----------------------------------------------------------------------------%%    
  \\newpage
  \\section{Model uncertainties and knowledge base}\\label{s.uncertainties}\n 
  \\subsection{Qualitative scoring system} \\label{s.uncertainty.scores} \n
  Uncertainties and the knowledge base of original aspects of the model have been assesd systematically. 
  Apart from verbal descriptions, a qualitive scoring system has been used (Tab.~\\ref{t.unc}, p.~\\pageref{t.unc}). 
  The criteria were",scoringText,".\n \\\\",rriskModel@scoring@explanatory,
  "\n \\\\ The same scoring scheme was also used do assess model input such as data 
  and parameters (see \\emph{scores} in tables in Section 3). \\\\
  \\subsection{Assessment of general model uncertainties}
  ", rriskModel@uncertainties@note, "\n", sep = ""), file = file.name, append = TRUE)

  if(length(rriskModel@uncertainties@uncertainties)>0)
  { uncertParts<-rriskModel@uncertainties@uncertainties[[1]]@namemain
    for(i in 1:length(rriskModel@uncertainties@uncertainties))
      { if(!is.element(rriskModel@uncertainties@uncertainties[[i]]@namemain,uncertParts))
        {  uncertParts<-c(uncertParts,rriskModel@uncertainties@uncertainties[[i]]@namemain)
        }
      }
    #------------------------------
    for (i in 1:length(uncertParts))
    { name.main <- uncertParts[i]
      
      for (j in 1:length(rriskModel@uncertainties@uncertainties))
      { if (rriskModel@uncertainties@uncertainties[[j]]@namemain == name.main)
        { score.length <- length(rriskModel@uncertainties@uncertainties[[j]]@scores)
        }
      }
    
      label1 <- gsub(x=rriskModel@name@name," ",replacement="")
      label2 <- gsub(x=name.main," ",replacement="")
      label <- paste("fig:",label1,"_",label2, sep="")
      cat(paste("\\subsection{Assessment of ",tolower(name.main), "} \n  The following aspects of ", tolower(name.main),
              " were identified and assessed. Fig.~\\ref{",label,"} (p.~\\pageref{",label,"}) and the score values below summarise the ", 
              score.length," criteria scores for each of the aspects. \n
              \\begin{description} \n", sep=""), file = file.name, append = TRUE)
    for (j in 1:length(rriskModel@uncertainties@uncertainties))
    {
      if (rriskModel@uncertainties@uncertainties[[j]]@namemain == name.main)
      {
         scores <- rriskModel@uncertainties@uncertainties[[j]]@scores
         scores<-paste(scores,collapse=" & ")
         cat(paste("         
          \\item[\\textbf{",rriskModel@uncertainties@uncertainties[[j]]@namesub,"}]", rriskModel@uncertainties@uncertainties[[j]]@explanation," 
         ", 
         sep = ""),file = file.name, append = TRUE)
         cat(paste("
         \\begin{tabular}{lllllllllll}\n ", 
                        "\\multirow{2}{*}{Scores:}& ",scoreNames,"\\\\ \n", 
                        " &", scores<-paste(scores,collapse=" & ") , "\\\\",  
                        "\n \\end{tabular}  \n", "
         ", sep = ""), file = file.name, append = TRUE)  
      }  
    }
    cat("\\end{description}", file = file.name, append = TRUE)
  }
  } # if(length(scoring)==0)  
}  
    

################################################################################
##                    Function tolatex.itemsAndParts                          ##
################################################################################
#' @description This creates the LaTeX scripts producing the third part in the documentation - 
#' "Model structure and contents". 
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.itemsAndParts
#' @aliases tolatex.itemsAndParts
#' @title LaTeX code for producing texts displaying the structure and contents 
#' of the model in form of items of every available parts
#' @usage tolatex.itemsAndParts(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.itemsAndParts(rriskModel=rriskModel, file.name="report.tex") }

tolatex.itemsAndParts <- function(rriskModel, file.name)
{
  cat("Writing model items and parts...\n")
graphFileName<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_modelNetwork",sep="")

    cat(paste("
    
%%----------------------------------------------------------------------------%%
%%                         Model structure and contents                       %%
%%----------------------------------------------------------------------------%%
\\newpage \n
\\section{Model structure and contents}\\label{s.struct}
The model consists of " ,length(rriskModel@parts@parts), " parts, in which a total of ",length(rriskModel@items@items),
" items are defined. Model \\emph{items} represent the quantitative information put into the model or derived 
from the model through calculation or simulation. A \\emph{part} of a model refers to a set of model items that
 describe one given subsystem in the real world risk scenario or one component of the assessment structure. 
 \\trrisk~supports a variety of types of model items and uses a standard template for characterisation of each 
 item (Tab.~\\ref{t.itemdef}, p.~\\pageref{t.itemdef}). The model items are also 
 listed in Tab.~\\ref{t.items} (p.~\\pageref{t.items}). The dependencies among model items are visualised 
 as graph (Fig.~\\ref{fig:",graphFileName,"}, p.~\\pageref{fig:",graphFileName,"}).  \n
", sep = "")
, file = file.name, append = TRUE)

items.checklist <- c()
items.all <- c()

if(length(rriskModel@items@items) > 0)
{ 
  scoreNames<-c()
  if(length(rriskModel@scoring@scoring)>0){
    for(i in 1:length(rriskModel@scoring@scoring)){
      scoreNamesTemp<-rriskModel@scoring@scoring[[i]]@notation
      scoreNames<-c(scoreNames,scoreNamesTemp)
    } # end for
  } # end if
  scoreNames<-paste(scoreNames,collapse=" & ")
  
  for (i in c(1:length(rriskModel@items@items))){
    items.all <- append(items.all, rriskModel@items@items[[i]]@name, after = length(items.all))
  } # end for

 if(length(rriskModel@parts@parts)>0){                                          # 1. Case: items with parts
  for (i in c(1:length(rriskModel@parts@parts))){                               # for all the parts in the model:
    cat(paste("                                                                 
    \\subsection{Part ", rriskModel@parts@parts[[i]]@name, "} \n
    ", rriskModel@parts@parts[[i]]@explanation, "
    ", sep = "")
    , file = file.name, append = TRUE)                                          # print i-th part
    
    itm <- strsplit(rriskModel@parts@parts[[i]]@items, " ")                     # for all the items in i-th part
    for (j in c(1:length(itm[[1]]))) {                                          # for the j-th item in i-th part
        for (k in c(1:length(rriskModel@items@items))) {                        # for the k-th item in the model
            if (itm[[1]][j] == rriskModel@items@items[[k]]@name) {              # if this item is in the model, then save following information: 
                items.checklist <- append(items.checklist, rriskModel@items@items[[k]]@name, after = length(items.checklist))
                this.expl <- gsub("\b%", "%", rriskModel@items@items[[k]]@explanation, fixed = TRUE)
                                this.expl <- gsub("%", "\\%", this.expl, fixed = TRUE)
                this.title <- rriskModel@items@items[[k]]@title
                this.name <- rriskModel@items@items[[k]]@name
                this.type <- rriskModel@items@items[[k]]@type
                              this.type1 <- substr(this.type, start = 0, stop = regexpr("(", this.type, fixed = T)[1]-1)
                              #this.type2 <- substr(this.type, start = regexpr("(", this.type, fixed = T)[1]+1, stop = regexpr(")", this.type, fixed = T)[1]-1)
                              this.type2<-rriskModel@items@items[[k]]@typecode
                this.def <- gsub("$", "$", rriskModel@items@items[[k]]@definition, fixed = TRUE)
                this.depitem <- rriskModel@items@items[[k]]@depitem
                this.unit <- rriskModel@items@items[[k]]@unit
                this.role <- rriskModel@items@items[[k]]@role
                this.min <- rriskModel@items@items[[k]]@plausimin
                this.max <- rriskModel@items@items[[k]]@plausimax
                this.scores <- rriskModel@items@items[[k]]@scores
                this.assump <- rriskModel@items@items[[k]]@assumptions
                this.remark <- rriskModel@items@items[[k]]@remark
                this.ref <- rriskModel@items@items[[k]]@reference
                this.data <- rriskModel@items@items[[k]]@data
                this.typecode <- rriskModel@items@items[[k]]@typecode
                this.stratum <- rriskModel@items@items[[k]]@stratum
                this.stratumevaluated <- rriskModel@items@items[[k]]@stratumevaluated
            } # end if  (itm[[1]][j] == rriskModel@items@items[[k]]@name) {
        } # end k-for loop
        
        if (this.typecode == "stra"){                                           # the special case for stra-object
          if(is.logical(this.stratumevaluated)){
            if (this.stratumevaluated) {
              this.def <- "See table below for evaluated stra-item"
            } else {
              this.def <- "See table below"
            } # end  if (this.stratumevaluated) {
          } # end   if(is.logical(this.stratumevaluated)){
        } # end if (this.typecode == "stra"){
        
        cat(paste("\\subsubsection{Item \\texttt{",itm[[1]][j],"}: ", this.title," \n}
        ", sep = ""),
        file = file.name, append = TRUE)                                        # print the information of j-th item
        cat(paste("\\begin{longtable}{|p{2.5cm}|p{10cm}|}
        \\hline
        Name & ", this.name, " \\\\ \\hline
        Explanation & ", this.expl, " \\\\  \\hline
        Type & ", this.type1, "(\\texttt{",this.type2,"})"," \\\\ \\hline
        Definition & \\begin{lstlisting} \n", this.def, "\n \\end{lstlisting}  \\\\ \\hline
        Dep. items & ", this.depitem, " \\\\ \\hline
        Unit & ", this.unit, " \\\\ \\hline
        Stratum & ", this.stratum, " \\\\ \\hline
        ", sep = ""),
        file = file.name, append = TRUE)
        
        #if (this.role != "Not defined (nd)") {
        cat(paste("Role & ", this.role, " \\\\ \\hline
          ", sep = ""),
          file = file.name, append = TRUE)
        #}
        
        if (length(this.min) != 0 & length(this.max) != 0)
        { cat(paste("Absolute range & [", this.min,", ",this.max, "] \\\\ \\hline", sep = ""), file = file.name, append = TRUE)
        }
        
        scores <- paste(this.scores,collapse=" & ")
        cat(paste("
        Scores & \\begin{tabular}{llllllllll}",
                 scoreNames,"\\\\",
                 scores, " \\\\
                 \\end{tabular} \\\\     \\hline
        ", sep = ""), file = file.name, append = TRUE)
        
        cat(paste("Assumptions &", this.assump, "\\\\ \\hline \n", sep = ""), file = file.name, append = TRUE)
        cat(paste("Remark &", this.remark, "\\\\ \\hline \n", sep = ""), file = file.name, append = TRUE)
        
        #if (this.ref != "No information given") {
        cat(paste("Reference & ", this.ref, " \\\\ \\hline \n
        ", sep = ""),
        file = file.name, append = TRUE)
        #}                                                                                
        
        cat(paste("\\end{longtable}", sep = ""), file = file.name, append = TRUE)
        
        if (this.type2 %in% c("mcrv", "fnrv", "rsrv", "bsrv", "mxrv") & (length(this.data)!=0) ) {      
          cat(paste("Statistics for evaluated item: \\\\", sep = ""), file = file.name, append = TRUE)
          cat("\\vspace{-.4cm}\\begin{table}[H]", file = file.name, append = TRUE)
        
          if (class(this.data) == "table") {
            xt <- capture.output(print.xtable(xtable(this.data),include.rownames=FALSE))
            cat(xt[-c(1:4, length(xt), length(xt)-1)], file = file.name, append = TRUE)
          } else if (class(this.data) == "data.frame"){
            xt <- capture.output(print.xtable(xtable(summary(this.data)),include.rownames=FALSE))
            cat(xt[-c(1:4, length(xt), length(xt)-1)], file = file.name, append = TRUE)
          } else {
            xt <- ""
          }  # end if (class(this.data) == "table") {
        
          cat("\\end{table}", file = file.name, append = TRUE)
        
        } else if (this.type2 == "stra" & length(this.data)!=0) { # special case of stra
          cat(paste(ifelse(this.stratumevaluated != "" && this.stratumevaluated == TRUE, "Evaluated ", ""), "Stratum: \\\\", sep = ""), file = file.name, append = TRUE)
          cat("\\vspace{-.4cm}\\begin{table}[H]", file = file.name, append = TRUE)
          stra <- capture.output(print.xtable(xtable(this.data$stratum),include.rownames=FALSE))  
          cat(stra[-c(1:4, length(stra), length(stra)-1)], file = file.name, append = TRUE)
          cat("\\end{table}", file = file.name, append = TRUE)
        } else if (this.type2 == "data" & length(this.data)!=0){
          cat("Summary statistics of data set: \\\\", file = file.name, append = TRUE)
          cat("\\vspace{-.4cm}\\begin{table}[H]", file = file.name, append = TRUE)
          xt <- capture.output(print.xtable(xtable(summary(this.data)),include.rownames=FALSE))  
          cat(xt[-c(1:4, length(xt), length(xt)-1)], file = file.name, append = TRUE)
          cat("\\end{table}", file = file.name, append = TRUE)
        } else if(this.type2 == "bdjp" & length(this.data)!=0){
          cat("Summary statistics of bdjp item: \\\\", file = file.name, append = TRUE)
          cat("\\vspace{-.4cm}\\begin{table}[H]", file = file.name, append = TRUE)
          xt <- capture.output(print.xtable(xtable(this.data),include.rownames=FALSE))  
          cat(xt[-c(1:4, length(xt), length(xt)-1)], file = file.name, append = TRUE)
          cat("\\end{table}", file = file.name, append = TRUE)
        }# end  if (this.type2 %in% c("mcrv", "fnrv", "rsrv", "bsrv") & (length(this.data)!=0) )
      
    }

  }
                                                                                # end of 1. case
} # end if(length(rriskModel@parts@parts)>0)
#cat(paste(items.checklist, sep = ""), file=file.name, append = TRUE)

        if (length(items.all) != length(items.checklist)) {                     # 2. Case: items without parts
            cat("\\subsection{Items without part}", file = file.name, append = TRUE)
            for (i in c(1:length(items.all))) {                                 # for all the items
                if (!items.all[i] %in% items.checklist) {                       # for all the items in the checklist
                    for (k in c(1:length(rriskModel@items@items))) {            # for all the items in the model
                        if (rriskModel@items@items[[k]]@name == items.all[i]) { # if the k-th item is in the checklist, then save following information: 
                            items.checklist <- append(items.checklist, rriskModel@items@items[[k]]@name, after = length(items.checklist))
                            this.expl <- gsub("\b%", "%", rriskModel@items@items[[k]]@explanation, fixed = TRUE)
                                            this.expl <- gsub("%", "\\%", this.expl, fixed = TRUE)
                            this.title <- rriskModel@items@items[[k]]@title
                            this.name <- rriskModel@items@items[[k]]@name
                            this.type <- rriskModel@items@items[[k]]@type
                                           this.type1 <- substr(this.type, start = 0, stop = regexpr("(", this.type, fixed = T)[1]-1)
                                           #this.type2 <- substr(this.type, start = regexpr("(", this.type, fixed = T)[1]+1, stop = regexpr(")", this.type, fixed = T)[1]-1)
                                           this.type2<-rriskModel@items@items[[k]]@typecode
                            this.def <- gsub("$", "$", rriskModel@items@items[[k]]@definition, fixed = TRUE)
                            this.depitem <- rriskModel@items@items[[k]]@depitem
                            this.unit <- rriskModel@items@items[[k]]@unit
                            this.role <- rriskModel@items@items[[k]]@role
                            this.min <- rriskModel@items@items[[k]]@plausimin
                            this.max <- rriskModel@items@items[[k]]@plausimax
                            this.scores <- rriskModel@items@items[[k]]@scores
                            this.assump <- rriskModel@items@items[[k]]@assumptions
                            this.remark <- rriskModel@items@items[[k]]@remark
                            this.ref <- rriskModel@items@items[[k]]@reference
                            this.typecode <- rriskModel@items@items[[k]]@typecode
                            this.data <- rriskModel@items@items[[k]]@data
                            this.stratum <- rriskModel@items@items[[k]]@stratum
                            this.stratumevaluated <- rriskModel@items@items[[k]]@stratumevaluated
                        }
                    }
                    if (this.typecode == "stra"){
                      if (this.stratumevaluated) {
                        this.def <- "See table below for evaluated stra-item"
                      } else {
                        this.def <- "See table below"
                      }
                    }
                    cat(paste("\\subsubsection{Item \\texttt{",this.name,"}: ", this.title," \n}
                    ", sep = ""),
                    file = file.name, append = TRUE)
                    cat(paste("\\begin{longtable}{|p{2.5cm}|p{10cm}|}
                    \\hline
                    Name & ", this.name, " \\\\ \\hline
                    Explanation & ", this.expl, " \\\\  \\hline
                    Type & ", this.type1, "(\\texttt{",this.type2,"})"," \\\\ \\hline
                    Definition & \\begin{lstlisting} \n", this.def, "\n \\end{lstlisting}  \\\\ \\hline
                    Dep. items & ", this.depitem, " \\\\ \\hline
                    Unit & ", this.unit, " \\\\ \\hline
                    ", sep = ""),
                    file = file.name, append = TRUE)
                    
                    #if (this.role != "Not defined (nd)") {
                    cat(paste("Role & ", this.role, " \\\\ \\hline
                    ", sep = ""),
                    file = file.name, append = TRUE)
                    #}
                    
                    if (length(this.min) != 0 & length(this.max) != 0) {
                    cat(paste("Absolute range & [", this.min,", ",this.max, "] \\\\ \\hline", sep = ""), file = file.name, append = TRUE)
                    }
                    
                    scores <- this.scores
                    cat(paste("
                    Scores & \\begin{tabular}{llllllllll}
                             U1 & U2 & U3 & U4 & U5 & U6 & K1 & K2 & K3 & K4 \\\\
                             ", scores[1], "&", scores[2], "&", scores[3], "&", scores[4], "&", scores[5], "&", scores[6], "&", scores[7], 
                             "&", scores[8], "&", scores[9], "&", scores[10], " \\\\
                             \\end{tabular} \\\\     \\hline
                    ", sep = ""), file = file.name, append = TRUE)
                    
                    cat(paste("Assumptions &", this.assump, "\\\\ \\hline \n", sep = ""), file = file.name, append = TRUE)
                    cat(paste("Remark &", this.remark, "\\\\ \\hline \n", sep = ""), file = file.name, append = TRUE)
                    
                   # if (this.ref != "No information given") {
                    cat(paste("Reference & ", this.ref, " \\\\ \\hline \n
                    ", sep = ""),
                    file = file.name, append = TRUE)
                    #}
                    
                    cat(paste("\\end{longtable}", sep = ""), file = file.name, append = TRUE)
                    
                    if (this.type2 %in% c("mcrv", "fnrv", "rsrv", "bsrv", "mxrv") & length(this.data)!=0 ) {
                    
                      cat(paste("Statistics for evaluated item: \\\\", sep = ""), file = file.name, append = TRUE)
                      cat("\\vspace{-.4cm}\\begin{table}[H]", file = file.name, append = TRUE)

                      if (class(this.data) == "table") {
                        xt <- capture.output(print.xtable(xtable(this.data),include.rownames=FALSE))
                        cat(xt[-c(1:4, length(xt), length(xt)-1)], file = file.name, append = TRUE)
                      } else if (class(this.data) == "data.frame") {
                        xt <- capture.output(print.xtable(xtable(summary(this.data)),include.rownames=FALSE))
                        cat(xt[-c(1:4, length(xt), length(xt)-1)], file = file.name, append = TRUE)
                      } else {
                        xt <- ""
                      } # end  if (class(this.data) == "table")                    
                    cat("\\end{table}", file = file.name, append = TRUE)
                    
                    } else if (this.type2 == "stra" & length(this.data)!=0) {                       # special cases of stra
                      cat(paste(ifelse(this.stratumevaluated != "" && this.stratumevaluated == TRUE, "Evaluated ", ""), "Stratum: \\\\", sep = ""), file = file.name, append = TRUE)
                      cat("\\vspace{-.4cm}\\begin{table}[H]", file = file.name, append = TRUE)
                      stra <- capture.output(print.xtable(xtable(this.data$stratum),include.rownames=FALSE))  
                      cat(stra[-c(1:4, length(stra), length(stra)-1)], file = file.name, append = TRUE)
                      cat("\\end{table}", file = file.name, append = TRUE)
                    } else if (this.type2 == "data" & length(this.data)!=0){
                      cat("Summary statistics of data set: \\\\", file = file.name, append = TRUE)
                      cat("\\vspace{-.4cm}\\begin{table}[H]", file = file.name, append = TRUE)
                      xt <- capture.output(print.xtable(xtable(summary(this.data)),include.rownames=FALSE)) 
                      cat(xt[-c(1:4, length(xt), length(xt)-1)], file = file.name, append = TRUE)
                      cat("\\end{table}", file = file.name, append = TRUE)
                    } else if(this.type2 == "bdjp" & length(this.data)!=0){
                      cat("Summary statistics of bdjp item: \\\\", file = file.name, append = TRUE)
                      cat("\\vspace{-.4cm}\\begin{table}[H]", file = file.name, append = TRUE)
                      xt <- capture.output(print.xtable(xtable(this.data),include.rownames=FALSE)) 
                      cat(xt[-c(1:4, length(xt), length(xt)-1)], file = file.name, append = TRUE)
                      cat("\\end{table}", file = file.name, append = TRUE)
                    }# end  if (this.type2 %in% c("mcrv", "fnrv", "rsrv", "bsrv") & (length(this.data)!=0) )
                }
            }
            
        }
}        
}


################################################################################
##                    Function tolatex.modelImplementation                    ##
################################################################################
#' @description This creates the LaTeX scripts producing the fourth part in the documentation - 
#' "Model Implementation". 
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.modelImplementation
#' @aliases tolatex.modelImplementation
#' @title LaTeX code for producing texts displaying the model implementations
#' @usage tolatex.modelImplementation(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.modelImplementation(rriskModel=rriskModel, file.name="report.tex") }

tolatex.modelImplementation <- function(rriskModel, file.name)
{ #-----------------------------------------------------------------------------
  # get names of all OF-items
  #-----------------------------------------------------------------------------
  OFitems<-c()
  if(length(rriskModel@items@items)>0){
    for(i in 1:length(rriskModel@items@items)){
      if(rriskModel@items@items[[i]]@rolecode=="OF"){
        OFitems<-c(OFitems,rriskModel@items@items[[i]]@name)
      } # end
    } # end for
  }
  
  conv <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_convergence_",OFitems,sep="")
  torn <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_tornado_",OFitems,sep="")
   
  
  #conv <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_convergence",rriskModel@output@OFname.2d,sep="")
  #torn <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_tornado_",rriskModel@output@OFname.2d,sep="")
  try.result<-try(items<-plotGAM(rriskModel,pdfGraph=TRUE),silent=TRUE)
  gamNames<-c()  
  for(i in 1:length(items))
          { gamNames[i]<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_npreg_",rriskModel@output@OFname.2d,items[i],sep="")
          }
  
 cat("Writing model implementation details...\n")
cat("
%%----------------------------------------------------------------------------%%
%%                              Model Implementation                          %%
%%----------------------------------------------------------------------------%%
\\section{Model implementation}\\label{s.impl}
\\paragraph{First and second order simulation.} The probabilistic modelling approach is based on Monte-Carlo random variate (\\tmcrv) items, 
which are random realisations from the probability density function (pdf) specified in the item definition. 
The notation is given in Tab.~\\ref{t.itemPDF} (p.~\\pageref{t.itemPDF}). \\trrisk~supports two-dimensional simulations for 
differentiation of variability and unvertainty. 
For 1st order (variability) and for the combined simulation of variability and uncertainty,",rriskModel@settings@N,"iterations were used. 
For 2nd order simulation (uncertainty),",rriskModel@settings@N2d,"iterations were used. The algorithm for 2d simulation is as follows.
\\begin{enumerate}
\\setlength{\\itemsep}{1pt}
  \\setlength{\\parskip}{0pt}
\\setlength{\\itemsep}{0.2pt}
\\item Draw $n_1$ realisations from \\tmcrv-distributions for input variables reflecting variability  (Role \\texttt{v}) and draw only 
one single realisation from \\tmcrv-distributions for input variables reflecting uncertainty (Role \\texttt{u} or \\texttt{uv}). 
These steps refer to the first dimension of the simulation.
\\item Evaluate the main outcome function OF using the data in step 1, resulting in $n_1$ estimates in the first dimension.
\\item Repeat $n_2$ times the steps 1 and 2 and collect the OF results in the two-dimensional ($n_1\\times n_2$) matrix.
\\end{enumerate}\n", file = file.name, append = TRUE)
#-------------------------------------------------------------------------------
# Explain the Bayes domains if they occur in the model
items<-rriskModel@items@items
bdjp.items<-c()
if(length(items)>0)
{ for(i in 1:length(items))
  { if(items[[i]]@typecode=="bdjp")
    { bdjp.items<-c(bdjp.items,items[[i]]@name)
    }
    if(i==length(items)) bdjp.items<-paste(bdjp.items,collapse=", ") 
  }
}
if(length(bdjp.items)>0)
{ cat("\\paragraph{bdjp items.}
The model includes the items",bdjp.items,", which are so-called Bayes domain joint posterior (\\tbdjp) items. These are sets
of parameters linked by a sub-model and estimated using data and posterior information about the parameters using Bayesian
methods (Markov chain Monte-Carlo as implemented in the BRugs package for R). Each \\tbdjp-item consists of a resample of the required
length from the joint posterior distribution, which is saved as data item.
",file = file.name, append = TRUE)
and.bdjp.items<-"and \\tbdjp-items"
} else and.bdjp.items<-"" 
#-------------------------------------------------------------------------------
#cat(paste("\\paragraph{Convergence.} The convergence of the model is visualised using a plot of the cumulative median value of the outcome function 
#(Fig.~\\ref{",conv,"}, p.~\\pageref{",conv,"}). The desired absolute error tolerance is added and subtracted from the final 
#median estimate and plotted as reference lines.
#", sep=""), file = file.name, append = TRUE)
#-------------------------------------------------------------------------------
if(length(conv)>1){
  conv.ref<-paste("\\ref{",conv,"}",sep="",collapse="--")
  conv.page<-paste("\\pageref{",conv,"}",sep="",collapse="--")
  cat(paste("\\paragraph{Convergence.} The convergence of the model is visualised using a plot of the cumulative median value of the outcome function(s) 
  (Fig.~",conv.ref,", p.~",conv.page,"). The desired absolute error tolerance is added and subtracted from the final 
  median estimate and plotted as reference lines.
  ", sep=""), file = file.name, append = TRUE)
} else {
  cat(paste("\\paragraph{Convergence.} The convergence of the model is visualised using a plot of the cumulative median value of the outcome function 
  (Fig.~\\ref{",conv,"}, p.~\\pageref{",conv,"}). The desired absolute error tolerance is added and subtracted from the final 
  median estimate and plotted as reference lines.
  ", sep=""), file = file.name, append = TRUE)
}
#-------------------------------------------------------------------------------
trans.txt<-""
if(rriskModel@settings@trans=="rank")
{ trans.txt<-"After rank-transformation of the"
} else if(rriskModel@settings@trans=="z-score")
{ trans.txt<-"After z-transformation (z-scores) of the"
} else if(rriskModel@settings@trans=="identity")
{ trans.txt<-"With the"
}
#------------
sens.txt<-""
sensexpl.txt<-""
if(rriskModel@settings@sens=="correlation")
{ sens.txt<-"correlation"
  sensexpl.txt<-"A sensitivity analysis based on correlation is thought to be independent of the actual model strusture."
} else if(rriskModel@settings@sens=="regression")
{ sens.txt<-"linear regression"
  sensexpl.txt<-"A sesitivity analysis based on regression may be affected ny the non-linear nature of the rrisk model."
}

#-------------------------------------------------------------------------------
if(length(conv)>1){
  torn.ref<-paste("\\ref{",torn,"}",sep="",collapse="--")
  torn.page<-paste("\\pageref{",torn,"}",sep="",collapse="--")
    
  cat(paste("\\paragraph{Sensitivity analysis using tornado charts.}
",trans.txt,"outcome function(OF) and all stochastic (\\tmcrv) items, a",sens.txt,"analysis was conducted to describe the impact
of the latter on the OF (Fig.~",torn.ref,", p.~",torn.page,"). The risk estimation model used to generate the OF is non-linear
is most cases. ",sensexpl.txt," 

Only the coefficients of the uncertain \\tmcrv-items",and.bdjp.items," are relevant for interpreting the item uncertainty sensitivity. 
The coefficients refer to the \\emph{full model}, i.e. the risk model as specified by the user for estimating the outcome. 

An alternative approach is implemented for comparative purposes. In this case, the distributional assumption for each \\tmcrv-item is relaxed
in favour of a uniform distribution spanning the absolute plausible range for the given \\tmcrv-item. 
The scale type (continuous or discrete) of the uniform distribution for each item corresponds to the type of distribution specified in the full model. 
This sensitivity analysis is therefore thought to assess the sensitivity of uncertain \\tmcrv-items in a \\emph{relaxed model}. 
", sep=""), file = file.name, append = TRUE)
} else {
  cat(paste("\\paragraph{Sensitivity analysis using tornado charts.}
",trans.txt,"outcome function(OF) and all stochastic (\\tmcrv) items, a",sens.txt,"analysis was conducted to describe the impact
of the latter on the OF (Fig.~\\ref{",torn,"}, p.~\\pageref{",torn,"}). The risk estimation model used to generate the OF is non-linear
is most cases. ",sensexpl.txt," 

Only the coefficients of the uncertain \\tmcrv-items",and.bdjp.items," are relevant for interpreting the item uncertainty sensitivity. 
The coefficients refer to the \\emph{full model}, i.e. the risk model as specified by the user for estimating the outcome. 

An alternative approach is implemented for comparative purposes. In this case, the distributional assumption for each \\tmcrv-item is relaxed
in favour of a uniform distribution spanning the absolute plausible range for the given \\tmcrv-item. 
The scale type (continuous or discrete) of the uniform distribution for each item corresponds to the type of distribution specified in the full model. 
This sensitivity analysis is therefore thought to assess the sensitivity of uncertain \\tmcrv-items in a \\emph{relaxed model}. 
", sep=""), file = file.name, append = TRUE)
}
#-------------------------------------------------------------------------------
cat(paste("\\paragraph{Summary of item uncertainty, knowledge base and sensitivity indicator using traffic light matrix.}
The squared coefficients of uncertain \\tmcrv-items are shown in a traffic light matrix and are interpreted 
as senstivity effects (Fig.~\\ref{",paste("fig:", gsub(x = rriskModel@name@name, " ", replacement = ""), "_Sensitivityeffects", sep = ""),"}, p.~\\pageref{", paste("fig:", gsub(x = rriskModel@name@name, " ", replacement = ""), "_Sensitivityeffects", sep = ""),"}, left part). 
The continuous colour scheme from green to red corresponds to values between zero and the largest 
observed squared coefficient for all \\tmcrv-items. 
Along with the sensitivity indicators, the results of the uncertainty and knowledge base scoring 
(see Section \\ref{s.uncertainty.scores}) are shown for each item. 
The traffic light colour scheme in this case is adjusted between green and red, reflecting the 
lowest (most uncritical) and highest (most critical) level of uncertainty and knowledge base, respectively."
,sep = ""), file = file.name, append = TRUE)
#-------------------------------------------------------------------------------
# Nonparametric regression 
#npreg.files <- list.files(pattern="npreg.pdf")
#if(length(npreg.files) > 0)
#{ select.statement <- ""
#  npreg.files[file.info(npreg.files)$ctime > file.info(file.name)$ctime]
#  npreg.files.l <- length(npreg.files)
#  #if(length(.expl$unc_items) > npreg.files.l) select.statement <- "The items for this analysis have been selected by the user. "
#  npreg.files <- gsub(".pdf","",list.files(pattern="npreg.pdf"))
#  if(length(gamNames) > 0){
    select.statement <- ""
#  }
  cat("
  \\paragraph{Sensitivity analysis using nonparametric regression.}
  Generalised additive models (GAM, R package {\\tt mgcv} package) are used to study the effect of uncertain \\tmcrv-items on the outcome function ", file=file.name, append = TRUE)
  cat(paste("Fig.~\\ref{fig:",gamNames[1],"}",ifelse(length(gamNames)>1,paste("--\\ref{fig:",gamNames[length(gamNames)],"}).",sep=""),"."),sep=""),
  "This graphical analysis is used to investigate the type (linear, non-linear) of impact of the uncertain variable.",select.statement," If 2d 
  simulation has been conducted, the analysis consists of two models; one using the 1st order simulation and one using the full 2d simulation. 
  In the latter case, there is a limited number (.N2) of support points for the 2nd dimension (i.e. realisations of uncertain \\tmcrv-items),
  each of which with a larger number of realisations (.N1). This may result in a waved shape of regression line."
  ,file = file.name, append = TRUE)
}
#-------------------------------------------------------------------------------
# Regression tree
#if(file.exists("regtree.pdf")) {
#    if((file.info("regtree.pdf")$ctime - file.info("OF_histo.pdf")$ctime) > 0) # i.e. the regtree is done after the histogram
#    cat("
#        \\paragraph{Sensitivity analysis using regression tree analysis.}
#        Binary recursive partitioning (R package ``{\\tt tree}'') was conducted whereby the data were successively split on the axes of explanatory variables (\\tmcrv-items) 
#        such that the split maximally distinguishes the outcome function at each step of the procedure (Fig.~\\ref{f.regtree}, p.~\\pageref{f.regtree}). The same data were used as for generating the 
#        tornado charts. The tree diagrammes present the influential \\tmcrv-items in decreasing order (top-down) and indicate the split points at which each
#        input variable was dichotomized."
#        ,file=report.tex,append=TRUE,fill=TRUE,sep=" ")
#        }
#main.result.unit <- sub("%","\\%",.model$Unit[which(.model$Name == .OF$name)],fixed=TRUE)
#main.result <- paste(.get.brief.def(.model$Explanation[which(.model$Name == .OF$name)])," ({\\tt ",.OF$name,"}) is the main outcome of the model.
#    The central value (median) of this quantity is around ",signif(.OF$q[7,2],dig=3)," ",main.result.unit," 
#    (based on 2d simulation with 95\\% credible interval of ",signif(.OF$q[7,3],dig=3),"--",signif(.OF$q[7,4],dig=3),").
#    The probabilistic modelling approach indicates that -- taking into account variability and uncertainty -- this quantity could also be higher
#    (around ",signif(.OF$q[11,4],dig=3)," as derived from the upper 95\\% credible interval of the 99th percentile). ",sep="")
#-------------------------------------------------------------------------------    
# violin plots are done if there are variability strata
#vio.txt <- ""
#if(file.exists("OF_violin.pdf")){
#    vstrata.id <- which(.model$Name %in% .expl$vstrata)
#    vio.factors <- paste("The factors are ",paste(.model$Explanation[vstrata.id],collapse=" and "),".",sep="")
#    vio.txt <- paste("
#    
#    The outcome function has also been evaluated using a stratification for factors describing sources of variability.",vio.factors,"
#    The effects are visualised using violin plots (R package {\\tt vioplot}), which are a combination of a box plot and kernel density plot (Fig.~\\ref{f.OF_violin}, p.~\\pageref{f.OF_violin}).")
#    }
    
#}


################################################################################
##                    Function tolatex.modelResults                           ##
################################################################################
#' @description This creates the LaTeX scripts producing the fifth part in the documentation - 
#' "model results ". 
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.modelResults
#' @aliases tolatex.modelResults
#' @title LaTeX code for producing texts displaying the model results
#' @usage tolatex.modelResults(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.modelResults(rriskModel=rriskModel, file.name="report.tex") }

tolatex.modelResults <- function(rriskModel, file.name)
{ cat("Writing model results...\n")

  #-----------------------------------------------------------------------------
  # get names of all OF-items
  #-----------------------------------------------------------------------------
  OFitems<-c()
  if(length(rriskModel@items@items)>0){
    for(i in 1:length(rriskModel@items@items)){
      if(rriskModel@items@items[[i]]@rolecode=="OF"){
        OFitems<-c(OFitems,rriskModel@items@items[[i]]@name)
      } # end
    } # end for
  }
  
  conv <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_convergence_",OFitems,sep="")
  torn <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_tornado_",OFitems,sep="")
  histo <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_histogram_",OFitems,sep="")
  cdf <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_cdf_",OFitems,sep="")
  tree <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_tree_",OFitems,sep="")
   
  #conv <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_convergence",rriskModel@output@OFname.2d,sep="")
  #histo <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_histogram",rriskModel@output@OFname.2d,sep="")
  #cdf <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_cdf_",rriskModel@output@OFname.2d,sep="")
  #torn <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_tornado_",rriskModel@output@OFname.2d,sep="")
  #tree <- paste("fig:",gsub(x=rriskModel@name@name," ",replacement=""),"_tree",rriskModel@output@OFname.2d,sep="")
  
  gamNames<-c()  
  try.result<-try(items<-plotGAM(rriskModel,pdfGraph=TRUE),silent=TRUE)
  for(i in 1:length(items)){
    gamNames[i]<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_npreg_",rriskModel@output@OFname.2d,items[i],sep="")
  }

  if(length(conv)>1){
    conv.ref<-paste("\\ref{",conv,"}",sep="",collapse="--")
    conv.page<-paste("\\pageref{",conv,"}",sep="",collapse="--")
    
    torn.ref<-paste("\\ref{",torn,"}",sep="",collapse="--")
    torn.page<-paste("\\pageref{",torn,"}",sep="",collapse="--") 
    
    cdf.ref<-paste("\\ref{",cdf,"}",sep="",collapse="--")
    cdf.page<-paste("\\pageref{",cdf,"}",sep="",collapse="--") 
    
    histo.ref<-paste("\\ref{",histo,"}",sep="",collapse="--")
    histo.page<-paste("\\pageref{",histo,"}",sep="",collapse="--") 
 
cat(paste("
%------------------------------------------------------------------------------%
%                                 model results                                %
%------------------------------------------------------------------------------%
\\section{Model results}\\label{s.results}

\\subsection{Risk estimation}
The convergence of the model is reflected in Fig.~",conv.ref," (p.~",conv.page,"). The numerical results of 
the model are shown in Tab.~\\ref{t.out.OF} (p.~\\pageref{t.out.OF}) and in Figs~",histo.ref," and ", cdf.ref, "
(p.~",histo.page," and ",cdf.page,"). The results are based on ",rriskModel@settings@N," and ",rriskModel@settings@N2d,"
iterations for 1st and 2nd order (uncertainty) simulation, respectively.
\\subsection{Sensitivity analysis}
The coefficients of the full and the relaxed model are shown as tornado charts (Fig.~",torn.ref,", p.~",torn.page,"). 
This chart allows comparing the size and direction of the effect of all stochastic variables on the outcome of the model. Large 
differences of the direction and size of effects between the full and the relaxed model indicate that the outcome critically 
depends on the chosen distribution models of uncertain parameters.
The senstivity effects of stochastic model inputs, along with the results of the uncertainty scoring and knowledge base are 
shown using a traffic light matrix in Fig.~\\ref{",paste("fig:", gsub(x = rriskModel@name@name, " ", replacement = ""), "_Sensitivityeffects", sep = ""),"} (p.~\\pageref{",
paste("fig:", gsub(x = rriskModel@name@name, " ", replacement = ""), "_Sensitivityeffects", sep = ""),"}). The results of nonparametric 
regression of the outcome function over the ranges of relevant uncertain model items are shown in ",sep="") 
,file = file.name, append=TRUE)
cat(paste("Fig.~\\ref{fig:",gamNames[1],"}",ifelse(length(gamNames)>1,paste("--\\ref{fig:",gamNames[length(gamNames)],"}).",sep=""),"."),sep=""), file = file.name, append=TRUE)
  } else {
    cat(paste("
%------------------------------------------------------------------------------%
%                                 model results                                %
%------------------------------------------------------------------------------%
\\section{Model results}\\label{s.results}

\\subsection{Risk estimation}
The convergence of the model is reflected in Fig.~\\ref{",conv,"} (p.~\\pageref{",conv,"}). The numerical results of 
the model are shown in Tab.~\\ref{t.out.OF} (p.~\\pageref{t.out.OF}) and in Figs~\\ref{",histo,"} and \\ref{", cdf, "}
(p.~\\pageref{",histo,"} and \\pageref{",cdf,"}). The results are based on ",rriskModel@settings@N," and ",rriskModel@settings@N2d,"
iterations for 1st and 2nd order (uncertainty) simulation, respectively.
\\subsection{Sensitivity analysis}
The coefficients of the full and the relaxed model are shown as tornado charts (Fig.~\\ref{",torn,"}, p.~\\pageref{",torn,"}). 
This chart allows comparing the size and direction of the effect of all stochastic variables on the outcome of the model. Large 
differences of the direction and size of effects between the full and the relaxed model indicate that the outcome critically 
depends on the chosen distribution models of uncertain parameters.
The senstivity effects of stochastic model inputs, along with the results of the uncertainty scoring and knowledge base are 
shown using a traffic light matrix in Fig.~\\ref{",paste("fig:", gsub(x = rriskModel@name@name, " ", replacement = ""), "_Sensitivityeffects", sep = ""),"} (p.~\\pageref{",
paste("fig:", gsub(x = rriskModel@name@name, " ", replacement = ""), "_Sensitivityeffects", sep = ""),"}). The results of nonparametric 
regression of the outcome function over the ranges of relevant uncertain model items are shown in ",sep="") 
,file = file.name, append=TRUE)
cat(paste("Fig.~\\ref{fig:",gamNames[1],"}",ifelse(length(gamNames)>1,paste("--\\ref{fig:",gamNames[length(gamNames)],"}).",sep=""),"."),sep=""), file = file.name, append=TRUE)
  }

} # end of function tolatex.modelResults()


#cat("
#\\section{Model results}\\label{s.results}
#\\subsection{Risk estimation}
#The convergence of the model is reflected in Fig.~\\ref{f.convergence} (p.~\\pageref{f.convergence}). The numerical results of the model are shown in Tab.~\\ref{t.out.OF} (p.~\\pageref{t.out.OF}) and in Figs~\\ref{f.OF_histo} and \\ref{f.OF_cdf}
#(p.~\\pageref{f.OF_histo} and \\pageref{f.OF_cdf}). The results are based on",.expl$settings$N,"and",.expl$settings$N2d,"iterations for 1st and 2nd order (uncertainty) simulation, respectively.",
#main.result,vio.txt,"
#
#\\subsection{Sensitivity analysis}
#The coefficients of the full and the relaxed model are shown as tornado charts (Fig.~\\ref{f.tornado}, p.~\\pageref{f.tornado}). This chart allows comparing the size and direction of the effect of all stochastic variables on the outcome of the model. Large differences of the direction and size of effects between the full and the relaxed model indicate that the outcome critically depends on the chosen distribution models of uncertain parameters.
#
#The senstivity effects of stochastic model inputs, along with the results of the uncertainty scoring and knowledge base are shown using a traffic light matrix in Fig.~\\ref{f.sensitivity} (p.~\\pageref{f.sensitivity})." 
#        ,file=report.tex,append=TRUE,fill=TRUE,sep=" ")
#
## Nonparametric regression 
#    if(length(npreg.files) > 0) {
#    cat("The results of nonparametric regression of the outcome function over the ranges of relevant uncertain model items are shown in"
#        ,paste("Fig.~\\ref{f.",npreg.files[1],"}",ifelse(npreg.files.l>1,paste("--\\ref{f.",npreg.files[npreg.files.l],"}).",sep=""),"."),sep=""),
#        file=report.tex,append=TRUE,fill=TRUE,sep=" ")
#        }
#        
# Regression tree
#if(file.exists("regtree.pdf")) {
#    if((file.info("regtree.pdf")$ctime - file.info("OF_histo.pdf")$ctime) > 0) # i.e. the regtree is done after the histogram
#    cat("
#        The regression tree analysis revealed a pattern of effects and interactions as shown in Fig.~\\ref{f.regtree} (p.~\\pageref{f.regtree}).
#        ", 
#        file=report.tex,append=TRUE,fill=TRUE,sep=" ")
#    }



################################################################################
##                   Function tolatex.modelValidation                         ##
################################################################################
#' @description This creates the LaTeX scripts producing the sixth part in the documentation - 
#' "Verification and Validation". 
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.modelValidation
#' @aliases tolatex.modelValidation
#' @title LaTeX code for producing texts displaying the Verification and 
#' validation of the model
#' @usage tolatex.modelValidation(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.modelValidation(rriskModel=rriskModel, file.name="report.tex") }

tolatex.modelValidation <- function(rriskModel, file.name)
{ cat("Writing model validations...\n")
cat("
%%----------------------------------------------------------------------------%%
%%                            Verification and Validation                     %%
%%----------------------------------------------------------------------------%%
\\section{Model verification and validation}\\label{s.model.validation}
", file = file.name, append = TRUE)
  if(length(rriskModel@validation@validation)>0)
  { for (i in 1:length(rriskModel@validation@validation))
    { cat(
      paste("\\paragraph{",rriskModel@validation@validation[[i]]@name, "}", rriskModel@validation@validation[[i]]@explanation, 
      sep = ""), file = file.name, append = TRUE)
    } # end for
  } # end if
}

################################################################################
##                   Function tolatex.commentsAndConclusions                  ##
################################################################################
#' @description This creates the LaTeX scripts producing the seventh part in the documentation - 
#' "Comments and conclusions". 
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.commentsAndConclusions
#' @aliases tolatex.commentsAndConclusions
#' @title LaTeX code for producing texts listing all comments and conclusions 
#' of the model
#' @usage tolatex.commentsAndConclusions(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.commentsAndConclusions(rriskModel=rriskModel, file.name="report.tex") }

tolatex.commentsAndConclusions<-function(rriskModel, file.name)
{ cat("Writing comments and conclusions...\n")
cat("
%%----------------------------------------------------------------------------%%
%%                           Comments and conclusions                         %%
%%----------------------------------------------------------------------------%%

    \\section{Comments and conclusions} \\label{s.concl}
         \\subsection{Comments}\n", file = file.name, append = TRUE)
    if(length(rriskModel@comments@comments)>0)
    { for (i in 1:length(rriskModel@comments@comments))
      { if(i==1) cat("\\begin{description}\n", file = file.name, append = TRUE)
        cat(paste("\\item[(", i,")] \\ ", rriskModel@comments@comments[[i]], "\\\\", sep = ""), file = file.name, append = TRUE)
        if(i==length(rriskModel@comments@comments)) cat("\\end{description}\n", file = file.name, append = TRUE)
      } # end for
    } # end if
    cat("\\subsection{Conclusions}\n", file = file.name, append = TRUE)
    if(length(rriskModel@conclusions@conclusions)>0)
    { for (i in 1:length(rriskModel@conclusions@conclusions))
      { if(i==1) cat("\\begin{description}\n", file = file.name, append = TRUE)
        cat(paste("\\item[(", i,")] \\ ", rriskModel@conclusions@conclusions[[i]], "\\\\", sep = ""), file = file.name, append = TRUE)
         if(i==length(rriskModel@conclusions@conclusions)) cat("\\end{description}\n", file = file.name, append = TRUE)
      } # end for
    } # end if
} # end of function tolatex.commentsAndConclusions()


################################################################################
##                    Function tolatex.references                             ##
################################################################################
#' @description This creates the LaTeX scripts producing the eighth part in the documentation - 
#' "References". 
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.references
#' @aliases tolatex.references
#' @title LaTeX code for producing refrences page concerning the model
#' @usage tolatex.references(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.references(rriskModel=rriskModel, file.name="report.tex") }

tolatex.references<-function(rriskModel,file.name)
{ cat("Creating Bibtex file...\n")
  references.check<-paste(rriskModel@references@references,collapse="")
  references.check<-gsub(x=references.check," ",replacement="")
  references.check<-gsub(x=references.check,"%",replacement="")
  if(references.check!="")
  {
  cat(" 
\\clearpage
%------------------------------------------------------------------------------%
%                             References                                       %
%------------------------------------------------------------------------------%
", file = file.name, append = TRUE)
  bibName<-gsub(x=rriskModel@name@name," ",replacement="")
  writeLines(text=rriskModel@references@references,paste(bibName,"bib",sep="."))
  cat(paste("\\addcontentsline{toc}{section}{References} \n     \\bibliography{",bibName,"}\\bibliographystyle{elsart-harv}",sep=""),
    file = file.name, append = TRUE)
 } else cat("Reference list is empty --> no Bibtex file can be created!\n")
}


################################################################################
##                    Function tolatex.glossAbbr                              ##
################################################################################
#' @description This creates the LaTeX scripts for the nineth part in the documentation - 
#' "Glossary and abbreviations", commencing the appendix of the documentation.
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.glossAbbr
#' @aliases tolatex.glossAbbr
#' @title LaTeX code for displaying the model's glossaries and abbreviations
#' @usage tolatex.glossAbbr(rriskSession,rriskModel,file.name)
#' @param rriskSession is an instance of the \code{rriskClass}
#' @param rriskModel is an instance of the \code{rriskModel}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' rriskSession<-init.rriskSession(useDemoModels = "all")
#' tolatex.glossAbbr(rriskSession=rriskSession,
#' rriskModel=rriskModel,file.name="report.tex") }

tolatex.glossAbbr <- function(rriskSession, rriskModel, file.name)
{ cat("Writing glossary and abbreviations...\n")
cat("
\\clearpage
%------------------------------------------------------------------------------%
%                         Glossary and abbreviations                           %
%------------------------------------------------------------------------------%
%\\appendix 
\\setcounter{figure}{0}
\\setcounter{table}{0}
\\section{Glossary and abbreviations} \\label{a.glossabb}
\\subsection*{Glossary}
\\begin{description}
\\item[Standard terms in \\trrisk] \\hrulefill", file = file.name, append = TRUE)
if(length(rriskSession@glossary)>0)
{ for (i in 1:length(rriskSession@glossary))
  {
    cat("\\item[", rriskSession@glossary[[i]]@name,"]", rriskSession@glossary[[i]]@explanation,"\n", file = file.name, append = TRUE)  
  }
}
cat("\\item[User-defined terms] \\hrulefill", file = file.name, append = TRUE)
if(length(rriskModel@glossary@glossary)>0)
{ for (i in 1:length(rriskModel@glossary@glossary))
  {
    cat("\\item[", rriskModel@glossary@glossary[[i]]@name,"]", rriskModel@glossary@glossary[[i]]@explanation,"\n", file = file.name, append = TRUE)  
  }
}
cat("\\end{description}", file = file.name, append = TRUE)

cat("\\subsection*{Abbreviations}
\\begin{tabular}{lp{11cm}}
\\multicolumn{2}{l}{\\bfseries Standard abbreviations in \\trrisk \\hrulefill}\\\\[.2cm]
", file = file.name, append = TRUE)
if(length(rriskSession@abbreviations)>0)
{ for (i in 1:length(rriskSession@abbreviations))
  {
    cat(rriskSession@abbreviations[[i]]@name,"&", rriskSession@abbreviations[[i]]@explanation,"\\\\", file = file.name, append = TRUE)  
  }
}
cat(" & \\\\ \\end{tabular}
\\vspace{2cm}
\\begin{tabular}{lp{11cm}}
\\multicolumn{2}{l}{\\bfseries User-defined abbreviations \\hrulefill}\\\\[.2cm]
", file = file.name, append = TRUE)
if(length(rriskModel@abbreviations@abbreviations)>0)
{ for (i in 1:length(rriskModel@abbreviations@abbreviations))
  {
    cat(rriskModel@abbreviations@abbreviations[[i]]@name,"&", rriskModel@abbreviations@abbreviations[[i]]@explanation,"\\\\", 
    file = file.name, append = TRUE)  
  }
}  
cat("\\end{tabular}", file = file.name, append = TRUE)

}

################################################################################
##                    Function tolatex.softwares                              ##
################################################################################
#' @description This creates the LaTeX scripts for the second part in the appendix - 
#' "softwares, packages and versions".
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.softwares
#' @aliases tolatex.softwares
#' @title LaTeX code for listing all softwares, R packages and the R version
#' required by the model 
#' @usage tolatex.softwares(rriskSession,file.name)
#' @param rriskSession is an instance of class \code{rriskClass}
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskSession<-init.rriskSession(useDemoModels = "all")
#' tolatex.softwares(rriskSession=rriskSession,file.name="report.tex") }

tolatex.softwares <- function(rriskSession,file.name)
{ cat("Writing software...\n")
  rpackages<-rriskSession@rpackages
  #rversion<-rriskSession@rversion
cat("
\\clearpage
%------------------------------------------------------------------------------%
%                       softwares, packages and versions                       %
%------------------------------------------------------------------------------%
\\section{Software, packages and versions} \\label{a.soft}
\\trrisk~depends on several R packages and softwares which are acknowladges below. The package citation are auto-generated and are as provided by the package developers.
\\begin{description}
", file = file.name, append = TRUE)
cat("\\item[\\textbf{",sessionInfo()[1]$R.version$version.string,"}] ",attr(citation()[[1]], "textVersion"),"\n", file = file.name, append = TRUE)
if(length(rpackages)>0)
{ for (i in 1:length(rpackages))
  { item <- rpackages[[i]]
    result <- try(citation(rpackages[i]), silent = TRUE)
    if (inherits(result, "try-error")) {
        beschr <- "No citation available."
    } else
    { beschr <- "Citation"
        if (length(attr(citation(rpackages[i]), "textVersion")) == 1) {
            beschr <- attr(citation(rpackages[i]), "textVersion")
        }   
        if (length(attr(citation(rpackages[i])[[1]], "textVersion")) == 1){
            beschr <- attr(citation(rpackages[i])[[1]], "textVersion")
        } 
        if (length(citation(rpackages[i])$textVersion) == 1) {
            beschr <- citation(rpackages[i])$textVersion
        }
        if (length(citation(rpackages[i])) > 1) {
           beschr <- citation(rpackages[i])[[i]]$textVersion
       }
    }
    cat("
        \\item[\\textbf{", item, "}]", beschr, "\n
    ", file = file.name, append = TRUE)
  } # end for
} # end if
cat("\\item[\\textbf{MikTeX}] http://www.miktex.org \n", file = file.name, append = TRUE)

cat("\\end{description}", file = file.name, append = TRUE)

}

################################################################################
##                   Function tolatex.graphics                                ##
################################################################################
#' @description This creates the LaTeX scripts for the third part in the appendix - 
#' "Graphics".
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.graphics
#' @aliases tolatex.graphics
#' @title producing LaTeX codes adding graphics into the documentation including 
#' concept graphics, network graphics and uncertainty graphics
#' required by the model 
#' @usage tolatex.graphics(rriskModel,file.name)
#' @param rriskModel is an instance of the \code{rriskModel} 
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.graphics(rriskModel=rriskModel,file.name="report.tex") }

tolatex.graphics <- function(rriskModel, file.name)
{ cat("Writing model graph(s)...\n")
cat("
\\clearpage
\\section{Figures and tables} \\label{a.figtabs}
%%----------------------------------------------------------------------------%%
%%                               Graphics                                     %%
%%----------------------------------------------------------------------------%%  
", file = file.name, append = TRUE)

#-------------------------------------------------------------------------------
# create model concept graph(s)
#-------------------------------------------------------------------------------
cat("\t Creating model concept graph(s)...")
try.result<-try(tolatexGraphs.concept(rriskModel=rriskModel,file.name=file.name),silent=TRUE)
if(inherits(try.result, "try-error")){
  cat("ERROR\n")
} else cat("OK\n")

#-------------------------------------------------------------------------------
# create model network graph
#-------------------------------------------------------------------------------
cat("\t Creating model network graph(s)...")
try.result<-try(tolatexGraphs.network(rriskModel=rriskModel,file.name=file.name),silent=TRUE)
if(inherits(try.result, "try-error")){
  cat("ERROR\n")
} else cat("OK\n")

#-------------------------------------------------------------------------------
# create model uncertainties graph(s)
#-------------------------------------------------------------------------------
cat("\t Creating model uncertainties graph(s)...")
try.result<-try(tolatexGraphs.uncertainties(rriskModel=rriskModel,LatexReport=TRUE,file.name=file.name,pdfGraph=TRUE),silent=TRUE)
if(inherits(try.result, "try-error")){
  cat("ERROR\n")
} else cat("OK\n")

}  # end of function tolatex.graphics()


################################################################################
##                   Function tolatex.outcomes                                ##
################################################################################
#' @description This displays the output in the R-console.
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.outcomes
#' @aliases tolatex.outcomes
#' @title isplaying the output in the R-console
#' required by the model 
#' @usage tolatex.outcomes(rriskModel,file.name)
#' @param rriskModel is an instance of the \code{rriskModel} 
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.outcomes(rriskModel=rriskModel,file.name="report.tex") }

tolatex.outcomes <- function(rriskModel, file.name)
{ cat("Writing model outputs...\n")
cat("
%%----------------------------------------------------------------------------%%
%%                               Outcomes                                     %%
%%----------------------------------------------------------------------------%%  
", file = file.name, append = TRUE)


if(!is.null(rriskModel@output@summaries))
{ 
  tolatexGraphs.convergence(rriskModel,file.name=file.name)
tolatexGraphs.histogram(rriskModel,file.name=file.name)
tolatexGraphs.cdf(rriskModel=rriskModel,file.name=file.name)
tolatexGraphs.tornado(rriskModel,file.name=file.name)
tolatexGraphs.trafficLights(rriskModel,file.name=file.name)
tolatexGraphs.gam(rriskModel,file.name=file.name)
tolatexGraphs.tree(rriskModel,file.name=file.name)

  items<-rriskModel@items@items
  if(length(items)>0)
  { OF.names<-c()
    OF.explanations<-c()
    for(i in 1:length(items))
    { if(items[[i]]@rolecode=="OF")
      { OF.names<-c(OF.names,items[[i]]@name)
        OF.explanations<-c(OF.explanations,items[[i]]@name)
      }
    }
    text1<-paste(rep("l",ncol(rriskModel@output@summaries)+1),collapse="")
    text2<-paste(OF.names,"=",OF.explanations,collapse="; ")
cat("
\\xtable{t.out.OF}{11cm}{Quantiles of the simulated outcome functions of the model$^a$.}{lllllllllll}
{&",file=file.name,append=TRUE,fill=TRUE,sep=" ")
suppressWarnings(write.table(rriskModel@output@summaries,file=file.name,sep="&",eol="\\\\",na="NA",append=TRUE,quote=FALSE,col.names=TRUE))
cat("
}{$^a$Model outcome: ",text2,"The estimate is based on one-dimensional simulation 
and comparative estimate based on median of two-dimensional simulation (50), lower 
limit (2.5) and upper limit (97.5) of 95\\% uncertainty interval of quantile based on 2d order simulation.
The quantiles are calculated using the empirical probabilities $p(k)=(k-1)/(n-1)$ for $n$
ordered data $(x_1,\\ldots,x_k,\\ldots,x_n)$ using the function \\enquote{quantile}. 
Quantiles having less than 5 points above or below are given with NA to represent their inaccuracy.
\\newline
The runtime of the model with", rriskModel@settings@N,"(1st order) and",rriskModel@settings@N2d,
"(2nd order) iterations was",rriskModel@output@runtime2d,"sec.}",file=file.name,append=TRUE,fill=TRUE,sep=" ")
  }
}
} # end of function tolatex.outcomes()




################################################################################
##                   Function tolatex.scoringTable                            ##
################################################################################
#' @description This creates the LaTeX scripts for the first table in the appendix - 
#' "Scoring table".
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.scoringTable
#' @aliases tolatex.scoringTable
#' @title producing LaTeX codes tabling the scoring system of the model with the
#' default criteria and qualitive scores
#' required by the model 
#' @usage tolatex.scoringTable(rriskModel,file.name)
#' @param rriskModel is an instance of the \code{rriskModel} 
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.scoringTable(rriskModel=rriskModel,file.name="report.tex")}

tolatex.scoringTable <- function(rriskModel, file.name)
{ cat("Writing scoring table...\n")
  scoring <- rriskModel@scoring
  if(length(scoring@scoring)>0)
{ cat("\\clearpage
%%----------------------------------------------------------------------------%%
%%                             Scoring table                                  %%
%%----------------------------------------------------------------------------%%  
\\newpage \n \\label{a.tabfig} \n", file = file.name, append = TRUE)

cat("\\begin{longtable}{p{2.5cm}p{10cm}}  \n ", file = file.name, append = TRUE)
cat("\\caption{",scoring@tableheader,"} 
\\label{t.unc} \\\\  \n \\hline \\parbox[0pt][2em][c]{0cm}{} \n ", file = file.name, append = TRUE)
cat("Criterion & Scores and definitions(interpretation guide)$^a$ \\\\ \n", file = file.name, append = TRUE)
cat("\\hline \n \\\\ \n", file = file.name, append = TRUE)
    for (i in 1:length(scoring@scoring))
      { output <- paste("(", scoring@scoring[[i]]@notation, ")", "\\newline ", scoring@scoring[[i]]@name ,"  \n &", scoring@scoring[[i]]@explanation,  
       "\\\\ \\\\  \n ", sep = "")
        cat (output, file = file.name, append = TRUE)
      } # end for
notapplValue<-scoring@vmeanings[which(names(scoring@vmeanings)=="notapplicable")]
cat(paste("\\hline \n \\end{longtable} \n $^a$For all criteria the level \\enquote{",notapplValue,"=not applicable} applies. \n \\\\",sep=""), file = file.name, append = TRUE)
} # end if(length(scoring@scoring)>0)
}

################################################################################
##                    Function tolatex.itemsTable                             ##
################################################################################
#' @description This function creates the LaTeX scripts for the second table in the appendix - 
#' "Item table".
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.itemsTable
#' @aliases tolatex.itemsTable
#' @title producing LaTeX codes tabling the characteristics defined for each model item
#' concept graphics, network graphics and uncertainty graphics
#' required by the model 
#' @usage tolatex.itemsTable(file.name)
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{tolatex.itemsTable(file.name="report.tex")}

tolatex.itemsTable <- function(file.name)
{ cat("Writing items table...\n")
cat("\\clearpage
%%----------------------------------------------------------------------------%%
%%                               Item table                                   %%
%%----------------------------------------------------------------------------%% 
", file = file.name, append = TRUE)
cat("\\begin{longtable}{p{2.5cm}p{10cm}} \n \\caption{Set of characteristics defined for each model item. } \\label{t.itemdef} \\\\ \n 
\\hline \n \\parbox[0pt][2em][c]{0cm}{} \n ", file = file.name, append = TRUE)
cat("Characteristics & Description \\\\ \n \\hline \n \\\\ \n ", file = file.name, append = TRUE)
cat("Part & Part of the model in which the item is defined \\\\
     Name$^a$ & Symbolic name of the item \\\\
     Explanation & Text field to define the item in words \\\\
     Type$^b$ & Type of item \\\\
     Definition$^c$ & Mathematical definition of the item \\\\
     Dep.~item$^{c,d}$ & Depending item \\\\
     Units$^e$ & Unit of measurement; unit of observation \\\\
     Stratum & Certifications \\\\
     Stratum evaluated$^f$ & Indicator for the evaluation of the weights, only relevant for \\texttt{type = stra} \\\\
     Role$^g$ & Role of the item in the model \\\\
     Absolute range & Absolute plausible limits for an uncertain model item \\\\
     Scores$^h$ & Qualitative scores for assessing uncertaintiy and knowledge base \\\\
     Assumptions & Text field for assumption made in defining the item \\\\
     Remark & Text field for additional remarks \\\\
     Reference & Text field for a bibliographic reference \\\\
     \n \\\\ \n \\hline \n
", file = file.name, append = TRUE)
cat("\\end{longtable} \n \\setlength{\\parindent}{0pt} \n
$^a$ This name is used to represent the item in mathematical expressions. \\\\
$^b$ Item types: \\tdata=data, indexing triplet of items \\tstid=stratum distribution, \\tstrv=stratum random variate for assigning 
each iteration to one of the defined strata, \\tnumv=numerical value(s), \\tmcrv=Monte-Carlo random variate, \\tfnrv=function of an 
\\tmcrv-item, \\tcorv=correlated random variate, \\tmxrv=mixture distribution item, \\trsrv=a resampling item, \\tbsrv=a bootstrap item. 
The vector length of all random variate({\\tt rv}) items equals the number or iterations. \\\\
$^c$ Automatically generated by \\trrisk~or defined by user depending on item type. \\\\
$^d$ Further model items functionally depending on the declared item. \\\\
$^e$ Unit of measurement refers to the physical quantity (e.g. \\textcelsius); unit of observation refers to the level of aggregation 
(e.g., herd, animal, meat dish) in which the simulated value could occur under natural conditions. \\\\
$^f$ value = \\texttt{FALSE}: weights are derived from existing items; value = \\texttt{TRUE}: weights are entered manually. \\\\
$^g$ Used to define whether a \\tmcrv-item expresses uncertainty (u), variability (v) or both variability and uncertainty (uv) or used 
to define an outcome function (OF). \\\\
$^h$ Qualitative score: see Section \\ref{s.uncertainty.scores} \\\\ \n
", file = file.name, append = TRUE)
} # end of function  tolatex.itemsTable()


################################################################################
##                        Function tolatex.modelTable                         ##
################################################################################
#' @description This creates the LaTeX scripts for the last table in the appendix - 
#' "Model items in the order of occurrence in the model".
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.modelTable
#' @aliases tolatex.modelTable
#' @title producing LaTeX codes listing model items in order of their occurrence 
#' in the model 
#' @usage tolatex.modelTable(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel} 
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.modelTable(rriskModel=rriskModel,file.name="report.tex")}

tolatex.modelTable <- function(rriskModel, file.name)
{ cat("Writing model table...\n")
items <- rriskModel@items@items
if(length(items)>0)
{
cat("
%%----------------------------------------------------------------------------%%
%%        Model items in the order of occurrence in the model                 %%
%%----------------------------------------------------------------------------%%
\\newpage
\\begin{landscape}
\\begin{longtable}{lllll p{10cm}}
\\caption{Model items in the order of occurrence in the model$^a$.}
\\label{t.items}
\\\\ \\hline \\\\[-.2cm]
Part & Name & Type & Role & Stratum & Definition \\\\
[.2cm]\\hline
\\\\[-.2cm]
"
, file = file.name, append = TRUE)
if(length(items)>0)
{ for (i in 1:length(items))
  {
     this.type <- items[[i]]@typecode
     this.role<- items[[i]]@rolecode
     this.stratum <- items[[i]]@stratum
     #this.type1 <- substr(this.type, start = regexpr("(", this.type, fixed = T)[1]+1, stop = regexpr(")", this.type, fixed = T)[1]-1)
     this.def <- gsub("$", "$", items[[i]]@definition, fixed = TRUE)
     cat(paste(
     items[[i]]@part, " & ", "\\texttt{", items[[i]]@name, "} & \\texttt{", this.type,"} & \\texttt{", this.role, "} & \\texttt{", this.stratum, "} & \\begin{minipage}{10cm}\\begin{lstlisting} \n", this.def, "\n \\end{lstlisting}\\end{minipage} \\\\ \n"
     ,sep = ""),
     file=file.name, append=TRUE)
  } # end fir
} # end if
cat("
\\hline \n \\end{longtable} \n \\vspace{.3cm}$^a$Choose {\\tt View model}, {\\tt Print code for full model} in \\trrisk-workspace to 
obtain the model code.  \\end{landscape}
",file=file.name,append=TRUE,fill=TRUE,sep=" ")

cat("\n
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
", file = file.name, append = TRUE)
} # end if(length(items)>0)
} # end of function tolatex.modelTable()



################################################################################
##                        Function tolatex.modelTable                         ##
################################################################################
#' @description This function creates the LaTeX scripts for the item pobability density functions (pdf).
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name tolatex.pdfTable
#' @aliases tolatex.pdfTable
#' @title producing LaTeX scripts for the item pobability density functions (pdf)
#' @usage tolatex.pdfTable(rriskModel, file.name)
#' @param rriskModel is an instance of the \code{rriskModel} 
#' @param file.name is a character value that indicates the Tex file where codes to be written into
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1()
#' tolatex.pdfTable(rriskModel=rriskModel,file.name="report.tex")}

tolatex.pdfTable <- function(rriskModel, file.name) {
cat("Writing pdf table...\n")
  rriskPDFs   <- as.data.frame(matrix(ncol=6,nrow=26,data=c(
    "bern",         "Bernoulli",                 "prob","","","",
    "beta",         "beta",                      "shape1","shape2","","",
    "pert",         "beta Pert",                 "min","mode","max","shape",
    "binom",        "binomial",                  "size","prob","","",
    "discrete",     "discrete",                  "values","probs","","",
    "cauchy",       "Cauchy",                    "location","scale","","",
    "chisq",        "chi-square",                "df","","","",
    "chisqnc",      "chi-square, non-central",   "df","ncp","","",
    "exp",          "exponential",               "rate","","","",
    "f",            "F",                         "df1","df2","","",
    "gamma",        "gamma",                     "shape","rate","","",
    "geom",         "geometric",                 "prob","","","",
    "gompertz",     "Gompertz",                  "shape","scale","","",
    "hyper",        "hypergeometric",            "m","n","k","",
    "logis",        "logistic",                  "location","scale","","",
    "lnorm",        "lognormal",                 "meanlog","slog","","",
    "multinom",     "multinomial",               "size","prob","","",
    "norm",         "Normal",                    "mean","sd","","",
    "tnorm",        "Truncated Normal",          "mean","st","lower","upper",
    "nbinom",       "negative binomial",         "size","prob","","",
    "pois",         "Poisson",                    "lambda","","","",
    "t",            "t",                         "df","","","",
    "triang",       "triangular",                "min","mode","max","",
    "unif",            "uniform",                "min","max","","",
    "udiscrete",    "uniform discrete",          "min","max","","",
    "weibull",      "Weibull",                   "shape","scale","",""),byrow=TRUE))
  
  pdf.used <- c()
  # pdf.id <- c()
  
  if(length(rriskModel@items@items)>0){
  for (i in c(1:length(rriskModel@items@items))) {
    if (rriskModel@items@items[[i]]@typecode == "mcrv") {
      pdf <- strsplit( rriskModel@items@items[[i]]@definition, split = "(", fixed = TRUE )[[1]][1]  
      pdf.id <- which( rriskPDFs[, 1] == pdf )
      if (is.element(pdf, rriskPDFs[, 1]) & !is.element(pdf, pdf.used) ) pdf.used <- c(pdf.used, pdf.id)
    }
  }
  pdf.used <- unique(pdf.used)       # Aenderung am 06.08.2012 von Yinchong
  pdf.used.m <- rriskPDFs[pdf.used, ]

  cat(" 
  \\newpage \n
  \\begin{longtable}{llllll} \n 
  \\caption{Probability density functions (pdf) used for Monte-Carlo simulation and their parameters.} \\label{t.itemPDF} \\\\
  \\hline \n
  \\parbox[0pt][2em][c]{0cm}{} 
  Abbreviation & pdf name & Para.~1 & Para.~2 & Para.~3 & Para.~4 \\\\ \n \\hline \\\\ \n 
  ", file = file.name, append = TRUE)
  for (i in c(1:nrow(pdf.used.m))) {
    for (j in c(1:5)) {
      cat( paste( pdf.used.m[i, j], " & ", sep = " " ), file = file.name, append = TRUE)
    }
    cat(paste( pdf.used.m[i, 6], "\\\\ \n ", sep = " "), file = file.name, append = TRUE)
  }
  cat("\\\\ \n \\hline \n \\end{longtable}", file = file.name, append = TRUE)
} # end if(length(rriskModel@items@items)>0) 
} # end of function tolatex.pdfTable()

               
################################################################################
##                            Function createModelReport                      ##
################################################################################
#' @description This function creates and saves a complete model report by combining all other tolatex-functions.
#' 
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name createModelReport
#' @aliases createModelReport
#' @title creates and saves a complete model report
#' @usage createModelReport(rriskModel,rriskSession,menuLevel=1)
#' @param rriskModel is an instance of the \code{rriskModel} 
#' @param rriskSession is an instance of the \code{rriskSession}
#' @param menuLevel ...
#' @keywords report
#' @export
#' @examples
#' \donttest{rriskModel<-init.Model1(demomode=TRUE)
#' rriskSession<-init.rriskSession(useDemoModels = "all", demomode=TRUE)
#' createModelReport(rriskModel=rriskModel, rriskSession=rriskSession)}

createModelReport<-function(rriskModel,rriskSession,menuLevel=1)
{
  oldDirectory<-getwd()
  on.exit(setwd(oldDirectory))
  
  oldEncoding <- options()$encoding
  enc<-options()$encoding
  if (substr(options()$pkgType, 1, 3) == "mac")
  { enc="MAC"
    options(encoding="MAC")
  }
  
  # choose directory where model graphs shoul be written
  #directory<-tclvalue(tkchooseDirectory())
  directory<-rrisk.chooseDir(default=oldDirectory)
  if(nchar(directory)>0)
  { directoryLatex<-file.path(directory,"latexCode")
    suppressWarnings(dir.create(directoryLatex))
    setwd(directoryLatex)
    
    #---------------------------------------------------------------------------
    # model updating...
    rriskModel<-changeUpdate(rriskModel,menuLevel=menuLevel)
    #---------------------------------------------------------------------------
   
    reportFile<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"report.tex",sep="_")

    tolatex.preamble(rriskModel,enc,file.name=reportFile)
    cat("\\begin{document}",file=reportFile,append=TRUE)
    tolatex.frontmatter(rriskModel,disclaimer=rriskSession@disclaimer,file.name=reportFile)
    tolatex.basics(rriskModel,file.name=reportFile)   
    tolatex.itemsAndParts(rriskModel,file.name=reportFile)
    tolatex.modelImplementation(rriskModel,file.name=reportFile)
    tolatex.modelResults(rriskModel,file.name=reportFile)
    tolatex.modelValidation(rriskModel,file.name=reportFile)
    tolatex.commentsAndConclusions(rriskModel,file.name=reportFile)
    tolatex.references(rriskModel,file.name=reportFile)
    # Hier beginnt Appendix
    tolatex.uncert(rriskModel,file.name=reportFile)
    tolatex.glossAbbr(rriskSession=rriskSession,rriskModel=rriskModel,file.name=reportFile)
    tolatex.softwares(rriskSession,file.name=reportFile)
    tolatex.graphics(rriskModel,file.name=reportFile)
    tolatex.outcomes(rriskModel,file.name=reportFile)
    tolatex.scoringTable(rriskModel,file.name=reportFile)
    tolatex.itemsTable(file.name=reportFile)
    tolatex.pdfTable(rriskModel, file.name = reportFile)
    tolatex.modelTable(rriskModel,file.name=reportFile)
    cat("\\end{document}",file=reportFile,append=TRUE) 
  
    try.result<-try(texi2dvi(reportFile,pdf=TRUE,clean=TRUE),silent=FALSE)
    if (!inherits(try.result, "try-error"))
    { reportFilePDF<-gsub(x=reportFile,".tex",replacement=".pdf")
      suppressWarnings(file.remove(file.path(directory,reportFilePDF)))
      file.copy(from=file.path(directoryLatex,reportFilePDF),to=file.path(directory,reportFilePDF))
      tkmessageBox(title="Creating model report",icon="info",type="ok",
        message=paste("Model report has been successfully created and has been saved under\n",file.path(directory,reportFilePDF),sep=""))               
      try.result<-try(rrisk.openPDF(file.path(directory,reportFilePDF)),silent=TRUE)
      if(inherits(try.result, "try-error")) cat("The pdf report file could not be opened!\n")
    } else
    { tkmessageBox(title="Creating model report",icon="error",type="ok",
        message="Cannot create model report! Please check the model definitions and syntax!")   
    }
    #---------------------------------------------------------------------------
    # return to the old working directory
    #---------------------------------------------------------------------------
    setwd(oldDirectory)
    
    #---------------------------------------------------------------------------
    # remove latex source files (temporary inactivated...)
    #---------------------------------------------------------------------------
    if(rriskModel@settings@deleteTeX)
    { cat("Removing LaTeX source files...\n")
      suppressWarnings(unlink(directoryLatex,recursive=TRUE))
    }
  }
  #-----------------------------------------------------------------------------
  # set standard options for encoding
  #-----------------------------------------------------------------------------
  options(encoding=oldEncoding)  
} # end of function createModelReport()



################################################################################
################################################################################
#' @name changeUpdate
#' @aliases changeUpdate
#' @title Non-executable auxiliary function
#' @usage changeUpdate(rriskModel,menuLevel=1)
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords report ...
#' @export

changeUpdate<-function(rriskModel,menuLevel=1)
{ on.exit(return(rriskModel))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")  
  choices<-c("No update",
              "Minor update",
              "Major update",
              "Release model")
  title.temp<-paste("The current version of the model is ", rriskModel@version@status," ",rriskModel@version@majorupdate,".",rriskModel@version@minorupdate,sep="")
  input<-99
  cat(levelTabulator,title.temp,"\n")
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Please choose version update",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(input==1)
    { # tue nix
      break()
    } else if(input==2)
    { minor<-rriskModel@version@minorupdate+1
      rriskModel@version@minorupdate<-minor
      rriskModel<-changeUpdateAuthor(rriskModel,menuLevel=menuLevel+1)
      break()
    } else if (input==3)
    { major<-rriskModel@version@majorupdate+1
      rriskModel@version@majorupdate<-major
      rriskModel@version@minorupdate<-0
      rriskModel<-changeUpdateAuthor(rriskModel,menuLevel=menuLevel+1)
      break()
    } else if(input==4)
    { rriskModel@version@status<-"Release"
      rriskModel@version@majorupdate<-1
      rriskModel@version@minorupdate<-0
      rriskModel<-changeUpdateAuthor(rriskModel,menuLevel=menuLevel+1)
      break()
    } 
    input<-99
  }
} # end of function changeUpdate()


################################################################################
################################################################################
#' @name changeUpdateAuthor
#' @aliases changeUpdateAuthor
#' @title Non-executable auxiliary function
#' @usage changeUpdateAuthor(rriskModel,menuLevel=1)
#' @param rriskModel ...
#' @param menuLevel ...
#' @keywords report
#' @export

changeUpdateAuthor<-function(rriskModel,menuLevel=1)
{ on.exit(return(rriskModel))
  levelTabulator<-paste(rep("\t",menuLevel),sep="",collapse="")
  levelTabulator<-paste("\n",levelTabulator,sep="")  
  
  authors<-rriskModel@authors@authors
  authorNames<-c()
  if(length(authors)>0)
  { for(i in 1:length(authors))
    { authorNames<-c(authorNames,authors[[i]]@name)
    }
  }
  choices<-authorNames
  input<-99
  while(!is.element(input,seq(1:length(choices))))
  { input<-mymenu(title="Please choose name of author who is responsible for last change",choices=choices,part="NA",help="No further help available",levelTabulator=levelTabulator)
    if(is.element(input,1:length(authorNames)))
    { input<-as.numeric(input)
      responsibleAuthor<-authorNames[input]
      rriskModel@version@editedby<-responsibleAuthor
      break()
    } 
    input<-99
  } # end while
} # end of function changeUpdateAuthor()

