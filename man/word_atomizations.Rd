\name{word_atomizations}
\alias{word_atomizations}
\title{Atomization of words}
\description{\code{word_atomizations} will automatically break the whole text into words nd rank them according to their frequency of occurence.}
\usage{
word_atomizations(m)
}
\arguments{
  \item{m}{An S4 object of class Abstracts}
}
\details{word_atomizations() will break down the whole text into words after removing the extra white space, punctuation marks and very common english words.}
\value{ A text file containing words with their frequencies}
\author{S.Ramachandran}
\examples{
\dontrun{word_atomizations(x)}
## here x is the object containing abstracts.
}
\keyword{function}

