\name{Pathway_Link}
\alias{Pathway_Link}
\title{To get the Links of the pathways for given genes}
\description{\code{Pathway_Link} will give the links of the pathways in which the query gene is involved}
\usage{
Pathway_Link(x)
}
\arguments{
  \item{x}{character string to search, HGNC approved gene symbol.}
}
\value{A list with the links for the pathways. By clicking on the links we can see the pathways on WikiPathways}
\author{Jyoti Sharma}
\note{Pathway_Link retrieves information from web so it need the package XML to parse. This function depends upon SSOAP package, and it retrieves information from Wikipathways.}
\seealso{\code{\link{Pathway_Info}}}
\examples{
Pathway_Link("TLR4") 
## list the links of the pathways in which the gene TLR4 is involved from Wikipathways.
}
\keyword{Function}

