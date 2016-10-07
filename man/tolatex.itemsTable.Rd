\name{tolatex.itemsTable}
\alias{tolatex.itemsTable}
\title{producing LaTeX codes tabling the characteristics defined for each model item
concept graphics, network graphics and uncertainty graphics
required by the model}
\usage{
  tolatex.itemsTable(file.name)
}
\arguments{
  \item{file.name}{is a character value that indicates the
  Tex file where codes to be written into}
}
\description{
  This function creates the LaTeX scripts for the second
  table in the appendix - "Item table".
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{tolatex.itemsTable(file.name="report.tex")}
}
\keyword{report}

