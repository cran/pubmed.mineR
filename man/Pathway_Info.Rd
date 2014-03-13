\name{Pathway_Info}
\alias{Pathway_Info}
\title{To get the information of pathways for Genes}
\description{\code{Pathway_Info} will give the brief information about pathways in which the query gene is involved.}
\usage{
Pathway_Info(x)
}
\arguments{
  \item{x}{character string for HGNC approved gene symbol.}
}
\details{Pathway_Info() fetches the information from WikiPathways through SSOAP, an Bioconductor R package. It provides the name of the pathway with the name of species, but here in this package it is human specific}
\value{A list showing the information of the pathways.}
\author{Jyoti Sharma}
\note{Pathway_Info fetches information from world wide web so it needs the package XML for parsing.}
\seealso{\code{\link{Pathway_Link}}}
\examples{
Pathway_Info("TLR4") 
## will output the names of pathways of the given genes
}
\keyword{Function}

