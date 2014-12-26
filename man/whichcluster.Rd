\name{whichcluster}
\alias{whichcluster}
\title{To fetch the cluster for words}
\description{\code{whichcluster} is used to get the cluster in which a given word (term) occurs.}
\usage{
whichcluster(clusterobject, y)
}
\arguments{
  \item{clusterobject}{an R object containing the clusters of words output by \code{wordscluster} function.}
  \item{y}{a character string of query term.}
}
\value{a list containing the number of cluster under which given term occurs.}
\author{S.Ramachandran}
\seealso{
\code{\link{wordscluster}}
}
\examples{
\dontrun{test<-whichcluster(x, "diabetes")
## here x is an R object output form wordscluster function.
## and "diabetes" is the term for which cluster number is to be searched. }
}
\keyword{Function}
