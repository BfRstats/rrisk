% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{validationClass-class}
\alias{validationClass}
\alias{validationClass-class}
\title{S4 class for representing model validity entry}
\description{
A S4 class for representing descriptions of a validity of a \code{rrisck} model
}
\examples{
\donttest{new("validationClass")

new("validationClass",
name="Model concept",
explanation=gsub(x="The model concept is a reasonable representation of
the key processes that are to be considered to answer the risk
question. The concept has been verified from a microbiological
and modelling viewpoint.","\\n",replacement=""))}
}

