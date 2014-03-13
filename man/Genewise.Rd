\name{Genewise}
\alias{Genewise}
\title{To Search the number of abstracts for Genes}
\description{\code{Genewise} reports the number of abstracts for given gene(s) name(s)}
\usage{
Genewise(object, gene)
}
\arguments{
  \item{object}{An S4 object of class Abstracts}
  \item{gene}{a character vector of gene names(HGNC approved symbol)}
}
\details{This function will report the number of abstracts containing the query gene term(s) [HGNC approved symbols], and the result is saved in a text file "dataout.txt". Genewise() will report numbers of abstracts only. The abstracts themselves for corresponding gene names can be obtained using searchabsL() and searchabsT.}
\value{Genewise will return an R object containing the abstracts for given gene, and a text file named "dataout.txt" containing the number of abstracts}
\author{Dr.S.Ramachandran}
\examples{
\dontrun{Genewise(x, "TLR4")}
## here 'x' contains the S4 object of Abstracts.
}
\keyword{Function}
