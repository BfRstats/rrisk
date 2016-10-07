\name{writeOutModelClass}
\alias{writeOutModelClass}
\title{Non-executable auxiliary function}
\usage{
  writeOutModelClass(object,file.name)
}
\arguments{
  \item{object}{an instance of the \code{modelClass}
  object}

  \item{file.name}{the file which saves the write-outs}
}
\description{
  Auxiliary function for the \code{writeOut} method of
  class \code{\linkS4class{modelClass}}.
}
\examples{
\donttest{rriskModel <- init.Model1()
writeOutModelClass(rriskModel,"rriskModel.txt")}
}
\keyword{model}

