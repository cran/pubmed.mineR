\name{BWI}
\alias{BWI}
\title{
To obtain the Buzz Word Index of terms from the Abstracts.
}
\description{
This function is used to obtain the Buzz word index value for the terms.
}
\usage{
BWI(current, previous, n, N)
}
\arguments{
\item{current}{
  \code{current} an S4 object containing the Abtracts for the current year we require the BWI an output from currentabs_fn()
}
  \item{previous}{
\code{previous} an S4 object containing the Abstracts for years previous to current year of study an output from previousabs_fn().
}
  \item{n}{
\code{n} is a character term for which Buzz Word Index is to be calculated.
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
\dontrun{result = BWI(mycurrentabs, mypreviousabs, "insulin", "inflammation")}
## BWI for the term "insulin" and the theme is inflammation. 
## Note that in the previous, years are starting one before the current year 2015; 
## current is an S4 object containing the output from currentabs_fn()
## previous is an S4 object containing the output from previousabs_fn().
## 'n' and 'N' are query terms whose BWI is sought and the theme respectively
}
\keyword{ function }

