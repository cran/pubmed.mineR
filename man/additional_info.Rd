\name{additional_info}
\alias{additional_info}
\title{
To extract sentences with nultiple keywords from Abstracts
}
\description{
\code{additional_info} will help to extract the sentences containing multiple query term(s) from a large corpus of multiple abstracts.}
\usage{
additional_info(abs, pmid, keywords)
}
\arguments{
  \item{abs}{
\code{abs} an S4 object of class Abstracts.
}
  \item{pmid}{
Vector of PMIDs from abstracts
}
  \item{keywords}{Character Vector of Terms 
}
}
\value{
It will return a matrix object containing PMID, keywords and sentences 
}

\author{
Surabhi Seth
}
\seealso{
\code{\link{Give_Sentences}}
}
\examples{
\dontrun{additional_info(abs = Abstract, pmid = "26564970"", keywords = "text-mining" )}
}
\keyword{Function}
