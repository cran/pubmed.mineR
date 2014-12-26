\name{pubtator_function}
\alias{pubtator_function}
\title{
function for text annotation uisng online PubTator
}
\description{
\code{pubtator_function} is used to extract specific information from an abstract like Gene, chemical, and diseases etc. 
}
\usage{
pubtator_function(x)
}
\arguments{
  \item{x}{
numeric value describing 'PMID'.
}
}
\details{
\code{pubtator_function} allow users to get information about 'Gene', 'Chemical' and 'Disease' for given PMID. It uses online tool PubTator on R plateform. It also removes redundancy from the output. It takes one PMID at once, for multiple PMIDs user can use lapply() function.
}
\value{
It returns a list object containing Gene, Chemical, Disease and PMID. 
}
\references{
Wei CH et. al., PubTator: a Web-based text mining tool for assisting Biocuration, Nucleic acids research, 2013, 41 (W1): W518-W522. doi: 10.1093/nar/gkt44
 
Wei CH et. al., Accelerating literature curation with text-mining tools: a case study of using PubTator to curate genes in PubMed abstracts, Database (Oxford), bas041, 2012
 
Wei CH et. al., PubTator: A PubMed-like interactive curation system for document triage and literature curation, in Proceedings of BioCreative 2012 workshop, Washington DC, USA, 145-150, 2012
}
\author{
Dr S.Ramachandran
}
\examples{
\dontrun{test = pubtator_function(17922911)}
## here pubtator_function() will extract the information from this given pmid.
}
\keyword{function}

