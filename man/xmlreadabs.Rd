\name{xmlreadabs}
\alias{xmlreadabs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{To read the abstracts from the PubMed saved in XML format.}
\description{\code{xmlreadabs} is modified form of readabs as it reads the abstracts downloaded/saved in XML format from PubMed. This is helpful to give clean and better result after preprocessing i.e. \code{word_atomizations}, \code{wordscluster} etc.}
\usage{
xmlreadabs(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{an XML file saved from PubMed.}
}
\value{an S4 object of class Abstracts containing journals, abstracts and PMID.}
\author{S.Ramachandran}
\seealso{\code{\link{readabs}}}
\examples{
\dontrun{test_run = xmlreadabs("pubmed_result.xml")}
## here "pubmed_result.xml" is an xml format file downloaded from PubMed.}
\keyword{Function}

