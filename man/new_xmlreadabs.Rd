\name{new_xmlreadabs}
\alias{new_xmlreadabs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{To read the abstracts from the PubMed saved in XML format.}
\description{\code{new_xmlreadabs} is modified form of xmlreadabs as it reads the abstracts downloaded or saved in XML format from PubMed. This function should be used for recent XML format from PubMed.
}
\usage{
new_xmlreadabs(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
an XML file saved from PubMed.
}
}
\value{
an S4 object of class Abstracts containing journals, abstracts and PMID.
}
\author{
S.Ramachandran
}
\note{
This function is useful with recent format of XML files from PubMed. The older xmlreadabs will not work with recent format.
}

\seealso{
\code{\link{readabsnew}}
\code{\link{readabs}}
}
\examples{
\dontrun{xmlabs = new_xmlreadabs("easyPubMed_00001.txt")}
## here "easyPubMed_00001.txt" is an xml file from PubMed using package easyPubMed}
\keyword{Function}
