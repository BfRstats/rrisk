\name{writeOutScoringsystemClass}
\alias{writeOutScoringsystemClass}
\title{Auxiliary function for the writeOut method of scoring system object}
\usage{
  writeOutScoringsystemClass(object,file.name)
}
\arguments{
  \item{object}{the slot scoringsystems of an instance of
  the \code{rriskSession} object}

  \item{file.name}{file that saves the write-outs}
}
\description{
  Auxiliary function for the \code{writeOut} method of
  scoring system object
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{rriskSession <- init.rriskSession(useDemoModels = "all")
writeOutScoringsystemClass(rriskSession@scoringsystems[[1]],"ssystem.txt")}
}
\keyword{scoring}

