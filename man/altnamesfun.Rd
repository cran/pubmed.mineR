\name{altnamesfun}
\alias{altnamesfun}
\title{
To Get Alternative names of Genes
}
\description{
This function is used to retrieve the Alternative names of genes from UniProt using HGNC gene symbol. 
}
\usage{
altnamesfun(m)
}
\arguments{
  \item{m}{
is the HGNC gene symbol.
}
}
\value{
It returns a list of alternative names of given Genes.
}
\references{
UniProt Consortium. "The universal protein resource (UniProt)." Nucleic acids research 36.suppl 1 (2008): D190-D195.
http://www.uniprot.org/
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{uniprotfun}}, ~~~
}
\examples{
\dontrun{test = altnamesfun(x)}
## here x is the HGNC gene symbol for which alternative name is required.
}
\keyword{ function }

