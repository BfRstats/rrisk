% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{modelAbbreviationsClass-class}
\alias{modelAbbreviationsClass}
\alias{modelAbbreviationsClass-class}
\title{S4 class for representing 'rrisk' model abbreviations list}
\description{
S4 class for representaing all the abbreviations uesed in a \code{rrisk} model
}
\examples{
\donttest{new("modelAbbreviationsClass")

new("modelAbbreviationsClass",
abbreviations=list(
new("glossaryClass",name="cfu", explanation="colony forming units"),
new("glossaryClass",name="cfu", explanation="colony forming units")))}
}

