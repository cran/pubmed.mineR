\name{wordscluster}
\alias{wordscluster}
\title{To cluster the words}
\description{\code{wordscluster} is used to cluster the words, using the levenshtein distance concept, which are coming together in combination with either 'prefixes' or 'suffixes' or other compound words. The first word, usually of lowest length, could be  'stemmed' word in many cases drastically so, is considered as representative for that cluster.}
\usage{
wordscluster(lower, upper)
}
\arguments{
  \item{lower}{lower limit for characters in word. Default = 5.}
  \item{upper}{upper limit of characters in word. Default = 30}
}
\details{This function is usefull for dampening the 'explotion' of words output from word_atomizations. This step enables easy examination of the terms.}
\note{The function may run faster when the lower limits are reduced but 'risks' producing plenty of 'runaway' situations. Their frequencies are very rare. Runaway situations. Some 'words' with part identity to other smaller words will runaway with smaller words. This event creates an unfavorable situation whereby the generated 'clusters' of words become difficult to interpret. This situation can be minimized by increasing the lower limit of word length, however at the cost of lowering computational speed. An example is: the word hypercholesterolemia runsaway with the smaller word 'lester' which could be another name.In this instance increasing the lower limit will be more usefull. Words longer than 30 characters are usually names of chemical comnpunds in IUPAC system of nomenclature.}
\value{a list object of words clustered together and a text filenamed "resulttable.txt" with the columns cluster number, cluster size and representatives of clusters.}
\author{S.Ramachandran, Jyoti Rani}
\seealso{
\code{\link{whichcluster} \link{word_atomizations}}
}
\examples{
\dontrun{
test=wordscluster(5, 10)
## here it will start making cluster of words of length with minimum of 5 characters 
## and maximum of 10 characters.  
}
}
\keyword{Function}
