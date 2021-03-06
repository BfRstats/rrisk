% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{modelCommentsClass-class}
\alias{modelCommentsClass}
\alias{modelCommentsClass-class}
\title{S4 class for representing all 'rrisk' model comments}
\description{
S4 class for representing all the comments of a \code{rrisk} model
}
\examples{
 \donttest{new("modelCommentsClass")

new("modelCommentsClass",
comments=list(gsub(x="The structure of the model follows the scenario 
pathway. The grouping of items into parts could be re-arranged to 
be consistent with the structure of risk assessments according to 
Codex Alimentarius if necessary.","\\n",replacement=""),
gsub(x="The sensitivity analysis indicates that among the uncertain model 
inputs, the probability of illness due to exposure to a single 
E.~coli cell ($r$) has a great impact on the outcome risk estimate.
The variability of the outcome is largely dependent on the number 
of viable bacteria in the dish at time of consumption ($n$)...",
"\\n",replacement=""))) }
}

