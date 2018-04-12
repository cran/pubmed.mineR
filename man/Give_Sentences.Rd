\name{Give_Sentences}
\alias{Give_Sentences}
\title{
To extract sentences from the Abstracts
}
\description{
\code{Give_Sentences} will help to extract the sentence containing query term/s from the large corpus.
}
\usage{
Give_Sentences(m, abs)
}
\arguments{
  \item{m}{
\code{m} a character term.
}
  \item{abs}{
\code{abs} an S4 object of class Abstracts.
}
}
\value{
It will return a list object containing sentences 
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{Give_Sentences_PMC}}
}
\examples{
\dontrun{Give_Sentences("diabetes", Abstracts)}
}
\keyword{Function}

