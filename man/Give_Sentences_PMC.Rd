\name{Give_Sentences_PMC}
\alias{Give_Sentences_PMC}
\title{
To fetch the sentence from the PMC full text article
}
\description{
\code{Give_Sentences_PMC} is used to extract the sentences from the full text article of given PMC id/s.
}
\usage{
Give_Sentences_PMC(PMCID, term)
}
\arguments{
  \item{PMCID}{
\code{PMCID} represents the PMC Id from where we want to extract the sentence.
}
  \item{term}{
\code{term} represents the term contained in a sentence.
}
}
\value{
It will return a list object containing the sentences for query term from the given article.
}
\author{
S.Ramachandran
}
\examples{
\dontrun{Give_Sentences_PMC(PMC4039032, "atherosclerosis")}
}
\keyword{function}

