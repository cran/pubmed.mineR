\name{BWI}
\alias{BWI}
\title{
To Get the Buzz Word Index of terms form the Abstracts.
}
\description{
This function is used to get the Buzz word index value for the terms. Buzz word became popular for a period of time.
}
\usage{
BWI(current, previous, n, N)
}
\arguments{
\item{current}{
  \code{current} an S4 object containing the Abtracts for the year we want to study. Output from currentabs_fn()
}
  \item{previous}{
\code{previous} an S4 object containing the Abstracts for years previous to our year of study. Output from previousabs_fn().
}
  \item{n}{
\code{n} is a character value for which Buzz Word Index is to be calculated.
}
  \item{N}{
\code{N} is a character value specifying the theme from the large corpus.
}
}
\value{
It returns a list containing BWI value for the given word.
}
\references{
Jensen, Lars Juhl, Jasmin Saric, and Peer Bork. "Literature mining for the biologist: from information retrieval to biological discovery." Nature reviews genetics 7.2 (2006): 119-129.
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{genes_BWI}}
}
\examples{
\dontrun{BWI(current, previous, n, N)}
## here current is an S4 object containing the output from currentabs_fn()
## previous is an S4 object containing the output from previousabs_fn().
## 'n' and 'N' are query word and theme respectively
}
\keyword{ function }

