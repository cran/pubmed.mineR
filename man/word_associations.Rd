\name{word_associations}
\alias{word_associations}
\title{Extracts the words associated (to the left and to the right) with a given word 
}
\description{\code{word_associations}will automatically extract associated words for a given word, namely the words immediately to teh left and to the right. The given word is usually in the middle except for those cases, where the given word occurrs either at the start or the end of the sentence.
}
\usage{
word_associations(term, abs)
}
\arguments{
  \item{term}{is a single word
}
  \item{abs}{an S4 object of class Abstracts
}
}
\details{
Certain words are qualified by authors in various ways. For example, physical therapy, gene therapy etc. This functions is useful in extracting these qualified words in the form of available associated words. Useful for preparing terms to be given in co_occurrence_fn (). There could be other uses also.
}
\value{
\item{comp1 }{A list of all the word pairs in a given set of abstracts.}
}
\references{
Rani J, Shah AB, Ramachandran S. pubmed.mineR: an R package with text-mining algorithms to analyse PubMed abstracts. J Biosci. 2015 Oct;40(4):671-82. PubMed PMID: 26564970.
}
\author{
S. Ramachandran
}
\seealso{
\code{Give_Sentences}
}
\examples{
\dontrun{word_associations("therapy",myabs}
##
}
\keyword{ Function }

