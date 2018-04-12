\name{xmlword_atomizations}
\alias{xmlword_atomizations}
\title{Word atomizations of abstracts from xml format.}
\description{\code{xmlword_atomizations} is used to process the abstracts from PubMed in XML format.}
\usage{
xmlword_atomizations(m)
}
\arguments{
  \item{m}{an S4 object of class Abstracts resulted from xmlreadabs.}
}
\value{a list containing words from the text with their frequencies.}
\author{S. Ramachandran}
\note{\code{xmlword_atomizations} cannot work on output of readabs.}
\seealso{\code{\link{xmlreadabs}}}
\examples{\dontrun{test = xmlword_atomizations(xmlabs)}
## here xmlabs is an S4 object i.e. output of xmlreadabs}
\keyword{Function}

