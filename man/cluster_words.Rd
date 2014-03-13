\name{cluster_words}
\alias{cluster_words}
\title{To Find the highest frequency of words within clusters}
\description{Function for finding the word (term) of highest frequency within clusters.}
\usage{
cluster_words(wordscluster, n)
}
\arguments{
  \item{wordscluster}{an R object containing the output of wordscluster()}
  \item{n}{a numeric vector containing cluster numbers}
}
\value{a list containing cluster and its highest frequency word}
\author{S.Ramachandran}
\seealso{\code{\link{wordscluster}}}
\examples{
\dontrun{test = cluster_words(wordscluster, 5)
## wordscluster is an R object of wordscluster
## 5 is number of cluster}
}
\keyword{Function}

