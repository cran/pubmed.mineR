\name{xmlgene_atomizations_new}
\alias{xmlgene_atomizations_new}
\title{Gene atomization of xml abstracts.}
\description{\code{xmlgene_atomizations_new} is used to fetch the list of genes
 from the xml abstracts}

\usage{
xmlgene_atomizations_new(m)
}

\arguments{
  \item{m}{an S4 object of class Abstracts, output from xmlreadabs.}

}

\value{a list containing genes from the text with their frquency of 
occurrence.}

 
\author{
S.Ramachandran, Jyoti Sharma
}

\seealso{
\code{\link{new_xmlreadabs}}
}
\examples{\dontrun{test = xmlgene_atomizations(xmlabs)}
## xmlabs is an S4 object of class Abstracts i.e. output of xmlreadabs}
\keyword{Function}
