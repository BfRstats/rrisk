\name{mymenu1}
\alias{mymenu1}
\title{Function that produces menu element and catches the input of the user}
\usage{
  mymenu1(title="Hi",choices="y",codes=1:length(choices),part="NA",
    help="No further help available",levelTabulator)
}
\arguments{
  \item{title}{the title}

  \item{choices}{choice made my the user}

  \item{codes}{vector from 1 to the length of choices}

  \item{part}{part}

  \item{help}{whether further help is available}

  \item{levelTabulator}{numeric indicating the level of the
  menu}
}
\value{
  choice
}
\description{
  Function that produces menu element and catches the input
  of the user
}
\details{
  This function is not intended to be called directly but
  is internally called during \code{rrisk} session.
}
\examples{
\donttest{mymenu1(title="Hi",choices="y",codes=1:length("y"),part="NA",
help="No further help available",levelTabulator=1)}
}
\keyword{misc}

