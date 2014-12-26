\name{get_original_term}
\alias{get_original_term}
\title{To get the original terms from the corpus.}
\description{\code{get_original_term} is used to get the exact term as it is present in corpus.}
\usage{
get_original_term(m, n)
}
\arguments{
  \item{m}{
an S4 object of class Abstracts containing the corpus.
}
  \item{n}{
a list object output from the function cluster_words
}
}
\value{
a list object contatining the terms.
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{wordscluster}}
}
\examples{
\dontrun{test = get_original_term(abs, words)}
## here abs is an S4 object of class Abstracts
## words is the output object of cluster_words()
}
\keyword{Function}
