\name{SentenceToken}
\alias{SentenceToken}
\title{To Tokenize the sentences}
\description{\code{SentenceToken} will tokenize abstracts into individual sentences.}
\usage{
SentenceToken(x)
}
\arguments{
  \item{x}{is a character string; could be an output from paste}
}
\details{ This function is necessary for extracting sentences from abstracts,  used by contextSearch function. The tokenization principle follows the overall strategy as described in \code{contextSearch}}
\value{ A character vector of sentences }
\author{ Dr.S.Ramachandran }
\examples{
\dontrun{SentenceToken(x)}
}
\keyword{Function}

