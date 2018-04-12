\name{xmlgene_atomizations}
\alias{xmlgene_atomizations}
\title{Gene atomization of xml abstracts.}
\description{\code{xmlgene_atomizations} is used to fetch the list of genes from the xml abstracts}
\usage{
xmlgene_atomizations(m)
}
\arguments{
  \item{m}{an S4 object of class Abstracts, output from xmlreadabs.}
}
\value{a list containing genes from the text with their frquency of occurence.}
\author{
S.Ramachandran, Jyoti Sharma
}
\seealso{
\code{\link{xmlreadabs}}
}
\examples{\dontrun{test = xmlgene_atomizations(xmlabs)}
## xmlabs is an S4 object of class Abstracts i.e. output of xmlreadabs}
\keyword{Function}
