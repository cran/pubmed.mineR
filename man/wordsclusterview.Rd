\name{wordsclusterview}
\alias{wordsclusterview}
\title{To view the words in cluster}
\description{\code{wordsclusterview} is used to view the words comes in cluster formed by \code{wordscluster} function.}
\usage{
wordsclusterview(words_cluster, all)
}
\arguments{
  \item{words_cluster}{an R object containing output of wordscluster}
  \item{all}{ is logical and default is FALSE, if set TRUE including those with one member word.}
}
\details{The first 5 words and 5 words near the median nd 5 words at the tail end are shown for clusters with more than 15 members. In case of cluster size less than 15, all the words are written in output.}
\value{It returns a text file named word_cluster_view.txt}
\author{S.Ramachandran}
\seealso{\code{\link{wordscluster}}}
\examples{
\dontrun{test= wordsclusterview(cluster)
# here cluster is output from wordscluster}}
\keyword{Function}

