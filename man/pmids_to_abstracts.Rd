\name{pmids_to_abstracts}
\alias{pmids_to_abstracts}
\title{
To Find and match the PMIDs to the abstracts.
}
\description{
 \code{pmids_to_abstracts} is used to extract the abstract/s of query PMID/s.
}
\usage{
pmids_to_abstracts(x, abs)
}
\arguments{
  \item{x}{
\code{x} a numeric vector containing PMIDs
}
  \item{abs}{
\code{abs} an S4 object of class Abstracts.
}
}
\value{
It will return an S4 object of class abstracts containing abstracts for query PMIDs.
}
\author{
S.Ramachandran
}
\examples{
\dontrun{pmids_to_abstracts(26878666,abs)}
}
\keyword{function}
