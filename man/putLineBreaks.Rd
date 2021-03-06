% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpFunctions.R
\name{putLineBreaks}
\alias{putLineBreaks}
\title{function for wraping text by definite length}
\usage{
putLineBreaks(object,show.output=FALSE,width.output)
}
\arguments{
\item{object}{character string, text to be wraped}

\item{show.output}{logical, if \code{TRUE} the text will be wraped in lines with definite length}

\item{width.output}{numeric, single positive integer defining the length of a line}
}
\value{
Returns the wraped text in lines with user-defined length if 
\code{show.output=TRUE}. Otherwise no text will be returned.
}
\description{
This function wraps a text in lines with definite length.
}
\details{
The argument \code{object} denotes the text to be displayed. \cr
If \code{show.output=TRUE} the text will be displayed in lines with a definite 
length, otherweise blank. \cr
The argument \code{width.output} denotes the to be defined length of a line
}
\examples{
\donttest{object<-"This is a lang text to be wraped. This is a lang text 
 to be wraped. This is a lang text to be wraped. This is a lang text to be 
 wraped. This is a lang text to be wraped. This is a lang text to be wraped. 
 This is a lang text to be wraped. This is a lang text to be wraped."
object<-gsub(x=object,"\\n",replacement="")
putLineBreaks(object=object,show.output=TRUE)       }
}
\author{
Matthias Greiner \email{matthias.greiner@bfr.bund.de}(BfR), \cr
Katharina Schueller \email{schueller@stat-up.de}(\acronym{STAT-UP} Statistical Consulting), \cr
Natalia Belgorodski \email{belgorodski@stat-up.de}(\acronym{STAT-UP} statistical Consulting)
}
\keyword{misc}

