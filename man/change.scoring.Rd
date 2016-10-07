\name{change.scoring}
\alias{change.scoring}
\title{Function for editing model slot 'scoring'}
\usage{
  change.scoring(scoring,availableSsystems)
}
\arguments{
  \item{scoring}{(model slot \code{scoring}) is an object
  of the class \code{modelScoringClass}}

  \item{availableSsystems}{is a character vector containing
  scoring system names that are available in the current
  rrisk session}
}
\value{
  This function returns edited model scoring system (slot
  \code{scoring}).
}
\description{
  A change function for
  \code{\linkS4class{modelScoringClass}}.
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\note{
  This function requires some functions from the package
  \code{tcltk}.
}
\examples{
\donttest{scoringSystem<-init.scoresystem1()
# It is not possible to call current scoring system by "my scoring system"
scoringSystem<-change.scoring(scoringSystem,availableSsystems=c("my scoring system"))
scoringSystem}
}
\keyword{edit}

